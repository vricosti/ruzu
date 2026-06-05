//! Diagnostic memory-SVC history for late MK8D nvmap failures.
//!
//! This is intentionally Rust-only investigation tooling. It is inactive unless
//! `RUZU_TRACE_MEM_HISTORY_TARGET=0x...` is set, and it does not affect guest
//! results or scheduler state.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

use crate::core::System;
use crate::hle::kernel::svc::svc_types::MemoryInfo;

const CAPACITY: usize = 256;

#[derive(Clone, Copy)]
pub enum MemoryHistoryKind {
    QueryMemory,
    SetHeapSize,
    MapMemory,
    UnmapMemory,
    MapPhysicalMemory,
    UnmapPhysicalMemory,
}

impl MemoryHistoryKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::QueryMemory => "QueryMemory",
            Self::SetHeapSize => "SetHeapSize",
            Self::MapMemory => "MapMemory",
            Self::UnmapMemory => "UnmapMemory",
            Self::MapPhysicalMemory => "MapPhysicalMemory",
            Self::UnmapPhysicalMemory => "UnmapPhysicalMemory",
        }
    }
}

#[derive(Clone, Copy)]
struct MemoryHistoryEvent {
    sequence: u64,
    tid: u64,
    kind: MemoryHistoryKind,
    query_or_addr: u64,
    base_or_out: u64,
    size: u64,
    state_or_result: u32,
    attr: u32,
    perm: u32,
    ipc_count: u32,
    device_count: u32,
}

static HISTORY: OnceLock<Mutex<VecDeque<MemoryHistoryEvent>>> = OnceLock::new();
static SEQUENCE: AtomicU64 = AtomicU64::new(0);

fn target() -> Option<u64> {
    static TARGET: OnceLock<Option<u64>> = OnceLock::new();
    *TARGET.get_or_init(|| {
        let raw = std::env::var("RUZU_TRACE_MEM_HISTORY_TARGET").ok()?;
        let raw = raw.trim();
        let digits = raw
            .strip_prefix("0x")
            .or_else(|| raw.strip_prefix("0X"))
            .unwrap_or(raw);
        u64::from_str_radix(digits, 16)
            .ok()
            .or_else(|| raw.parse::<u64>().ok())
    })
}

fn current_tid(system: &System) -> u64 {
    system
        .current_thread()
        .and_then(|thread| thread.lock().ok().map(|guard| guard.get_thread_id()))
        .unwrap_or(0)
}

fn range_covers(addr: u64, size: u64, point: u64) -> bool {
    let end = addr.saturating_add(size);
    addr <= point && point < end
}

fn near(addr: u64, point: u64) -> bool {
    addr.abs_diff(point) <= 0x20_0000
}

fn push(event: MemoryHistoryEvent) {
    let mut history = HISTORY
        .get_or_init(|| Mutex::new(VecDeque::with_capacity(CAPACITY)))
        .lock()
        .unwrap();
    if history.len() == CAPACITY {
        history.pop_front();
    }
    history.push_back(MemoryHistoryEvent {
        sequence: SEQUENCE.fetch_add(1, Ordering::Relaxed) + 1,
        ..event
    });
}

pub fn record_query(system: &System, query_address: u64, info: &MemoryInfo) {
    let Some(target) = target() else {
        return;
    };
    if !range_covers(info.base_address, info.size, target) && !near(query_address, target) {
        return;
    }
    push(MemoryHistoryEvent {
        sequence: 0,
        tid: current_tid(system),
        kind: MemoryHistoryKind::QueryMemory,
        query_or_addr: query_address,
        base_or_out: info.base_address,
        size: info.size,
        state_or_result: info.state,
        attr: info.attribute,
        perm: info.permission,
        ipc_count: info.ipc_count,
        device_count: info.device_count,
    });
}

pub fn record_heap(system: &System, size: u64, result: u32, out_address: u64) {
    if target().is_none() {
        return;
    }
    push(MemoryHistoryEvent {
        sequence: 0,
        tid: current_tid(system),
        kind: MemoryHistoryKind::SetHeapSize,
        query_or_addr: 0,
        base_or_out: out_address,
        size,
        state_or_result: result,
        attr: 0,
        perm: 0,
        ipc_count: 0,
        device_count: 0,
    });
}

pub fn record_physical(system: &System, kind: MemoryHistoryKind, addr: u64, size: u64, result: u32) {
    let Some(target) = target() else {
        return;
    };
    if !range_covers(addr, size, target) && !near(addr, target) {
        return;
    }
    push(MemoryHistoryEvent {
        sequence: 0,
        tid: current_tid(system),
        kind,
        query_or_addr: addr,
        base_or_out: addr,
        size,
        state_or_result: result,
        attr: 0,
        perm: 0,
        ipc_count: 0,
        device_count: 0,
    });
}

pub fn record_map_memory(
    system: &System,
    kind: MemoryHistoryKind,
    dst_addr: u64,
    src_addr: u64,
    size: u64,
    result: u32,
) {
    let Some(target) = target() else {
        return;
    };
    if !range_covers(dst_addr, size, target)
        && !range_covers(src_addr, size, target)
        && !near(dst_addr, target)
        && !near(src_addr, target)
    {
        return;
    }
    push(MemoryHistoryEvent {
        sequence: 0,
        tid: current_tid(system),
        kind,
        query_or_addr: dst_addr,
        base_or_out: src_addr,
        size,
        state_or_result: result,
        attr: 0,
        perm: 0,
        ipc_count: 0,
        device_count: 0,
    });
}

pub fn dump(reason: &str) {
    let Some(history) = HISTORY.get() else {
        return;
    };
    let history = history.lock().unwrap();
    eprintln!(
        "[MEM_HISTORY] reason={} target=0x{:X} events={}",
        reason,
        target().unwrap_or(0),
        history.len()
    );
    for event in history.iter() {
        match event.kind {
            MemoryHistoryKind::QueryMemory => {
                eprintln!(
                    "[MEM_HISTORY] #{:05} tid={} {} query=0x{:X} -> base=0x{:X} size=0x{:X} state=0x{:X} attr=0x{:X} perm=0x{:X} ipc={} dev={}",
                    event.sequence,
                    event.tid,
                    event.kind.as_str(),
                    event.query_or_addr,
                    event.base_or_out,
                    event.size,
                    event.state_or_result,
                    event.attr,
                    event.perm,
                    event.ipc_count,
                    event.device_count,
                );
            }
            MemoryHistoryKind::SetHeapSize => {
                eprintln!(
                    "[MEM_HISTORY] #{:05} tid={} {} size=0x{:X} -> result=0x{:08X} address=0x{:X}",
                    event.sequence,
                    event.tid,
                    event.kind.as_str(),
                    event.size,
                    event.state_or_result,
                    event.base_or_out,
                );
            }
            MemoryHistoryKind::MapMemory | MemoryHistoryKind::UnmapMemory => {
                eprintln!(
                    "[MEM_HISTORY] #{:05} tid={} {} dst=0x{:X} src=0x{:X} size=0x{:X} result=0x{:08X}",
                    event.sequence,
                    event.tid,
                    event.kind.as_str(),
                    event.query_or_addr,
                    event.base_or_out,
                    event.size,
                    event.state_or_result,
                );
            }
            MemoryHistoryKind::MapPhysicalMemory | MemoryHistoryKind::UnmapPhysicalMemory => {
                eprintln!(
                    "[MEM_HISTORY] #{:05} tid={} {} addr=0x{:X} size=0x{:X} result=0x{:08X}",
                    event.sequence,
                    event.tid,
                    event.kind.as_str(),
                    event.query_or_addr,
                    event.size,
                    event.state_or_result,
                );
            }
        }
    }
}
