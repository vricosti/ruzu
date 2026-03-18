// Debug/tracing helpers for main.rs
// Extract these into a separate file so main.rs stays clean and committable.
// To disable all debug tracing, comment out `pub mod main_log;` in main.rs.

use rdynarmic::frontend::a32::decoder::decode_arm;
use ruzu_core::hle::kernel::k_process::ProcessMemoryData;
use std::sync::RwLock;
use std::sync::Arc;

/// Dump module .dynamic info and search for __nnDetailInitLibc0 symbol.
pub fn dump_module(mem: &ProcessMemoryData, name: &str, base: u64) {
    let mod0_ptr = mem.read_32(base + 4) as u64;
    let mod0_addr = base + mod0_ptr;
    let magic = mem.read_32(mod0_addr);
    if magic != 0x30444F4D {
        log::warn!("{} @ {:#x}: bad MOD0 magic {:#x}", name, base, magic);
        return;
    }
    let dyn_off = mem.read_32(mod0_addr + 4) as i32;
    let dyn_addr = (mod0_addr as i64 + dyn_off as i64) as u64;
    let mut strtab = 0u64;
    let mut symtab = 0u64;
    let mut strsz = 0u64;
    let mut dt_hash = 0u64;
    let mut dt_gnu_hash = 0u64;
    for i in 0..80u64 {
        let tag = mem.read_32(dyn_addr + i * 8);
        let val = mem.read_32(dyn_addr + i * 8 + 4);
        match tag {
            4 => dt_hash = val as u64,
            5 => strtab = val as u64,
            6 => symtab = val as u64,
            10 => strsz = val as u64,
            _ => {
                if tag == 0x6ffffef5u32 { dt_gnu_hash = val as u64; }
            }
        }
        if tag == 0 { break; }
    }
    let strtab_addr = base + strtab;
    let symtab_addr = base + symtab;
    log::info!("{} @ {:#x}: DT_STRTAB={:#x}(abs={:#x}) DT_SYMTAB={:#x}(abs={:#x}) STRSZ={:#x} DT_HASH={:#x} DT_GNU_HASH={:#x}",
        name, base, strtab, strtab_addr, symtab, symtab_addr, strsz, dt_hash, dt_gnu_hash);

    // Dump first 5 symbols.
    for si in 0..5u64 {
        let sa = symtab_addr + si * 16;
        if !mem.is_valid_range(sa, 16) { break; }
        let st_name = mem.read_32(sa);
        let st_value = mem.read_32(sa + 4);
        let st_info = mem.read_8(sa + 12);
        let na = strtab_addr + st_name as u64;
        let mut sn = String::new();
        if mem.is_valid_range(na, 1) {
            for j in 0..80u64 {
                let c = mem.read_8(na + j);
                if c == 0 { break; }
                sn.push(c as char);
            }
        }
        log::info!("  sym[{}]: st_name={:#x} val={:#x} info={:#x} '{}'", si, st_name, st_value, st_info, sn);
    }

    // Search for __nnDetailInitLibc0 in this module's strtab.
    let target = b"__nnDetailInitLibc0";
    let target_len = target.len() as u64;
    if strsz > target_len && mem.is_valid_range(strtab_addr, strsz as usize) {
        let mut sym_found = false;
        for offset in 0..(strsz - target_len) {
            let mut m = true;
            for (k, &b) in target.iter().enumerate() {
                if mem.read_8(strtab_addr + offset + k as u64) != b {
                    m = false;
                    break;
                }
            }
            if m {
                let preceded_by_nul = offset == 0 || mem.read_8(strtab_addr + offset - 1) == 0;
                if preceded_by_nul {
                    log::info!("  FOUND '__nnDetailInitLibc0' at strtab+{:#x} (abs={:#x})", offset, strtab_addr + offset);
                    let sym_entry_size = 16u64;
                    let n_syms = (strtab_addr - symtab_addr) / sym_entry_size;
                    for si in 0..n_syms {
                        let sa = symtab_addr + si * sym_entry_size;
                        if !mem.is_valid_range(sa, sym_entry_size as usize) { break; }
                        let st_name_val = mem.read_32(sa);
                        if st_name_val as u64 == offset {
                            let st_value = mem.read_32(sa + 4);
                            let st_size = mem.read_32(sa + 8);
                            let st_info = mem.read_8(sa + 12);
                            let st_other = mem.read_8(sa + 13);
                            let st_shndx = mem.read_16(sa + 14);
                            let bind = st_info >> 4;
                            let stype = st_info & 0xf;
                            log::info!("    sym[{}]: val={:#x} sz={:#x} bind={} type={} other={} shndx={}",
                                si, st_value, st_size, bind, stype, st_other, st_shndx);
                        }
                    }
                    sym_found = true;
                    break;
                }
            }
        }
        if !sym_found {
            log::info!("  '__nnDetailInitLibc0' NOT in strtab");
        }
    }
}

/// Dump all known modules' .dynamic info.
pub fn dump_all_modules(shared_memory: &Arc<RwLock<ProcessMemoryData>>, code_base: u64) {
    let mem = shared_memory.read().unwrap();
    dump_module(&mem, "rtld", code_base);
    dump_module(&mem, "main", 0x206000);
    dump_module(&mem, "subsdk0", 0x1512000);
    dump_module(&mem, "subsdk1", 0x16AB000);
    dump_module(&mem, "subsdk2", 0x16D3000);
    dump_module(&mem, "subsdk3", 0x16E6000);
    dump_module(&mem, "subsdk4", 0x1723000);
    dump_module(&mem, "sdk", 0x1C9C000);

    log::info!("rtld data segment (0x205000) PRE-execution:");
    for i in 0..8u64 {
        let addr = 0x205000 + i * 4;
        let val = mem.read_32(addr);
        log::info!("  [{:#010x}] = {:#010x}", addr, val);
    }

    if mem.is_valid_range(code_base, 0x6000) {
        let _ = std::fs::write("/tmp/rtld.bin", mem.read_block(code_base, 0x6000));
    }
}

/// Dump __nnDetailInitLibc0 function code and TLS area.
pub fn dump_initlibc0_and_tls(shared_memory: &Arc<RwLock<ProcessMemoryData>>, tls_base: u64) {
    let mem = shared_memory.read().unwrap();
    let real_func_addr: u64 = 0xc5f740;
    log::error!("=== __nnDetailInitLibc0 real function at {:#x} ===", real_func_addr);
    for i in 0..64u64 {
        let addr = real_func_addr + i * 4;
        let insn = mem.read_32(addr);
        let decoded = if (insn & 0x0F000000) == 0x0A000000 || (insn & 0x0F000000) == 0x0B000000 {
            let offset = ((insn & 0x00FFFFFF) as i32) << 8 >> 6;
            let target = (addr as i64 + 8 + offset as i64) as u64;
            let kind = if (insn & 0x0F000000) == 0x0B000000 { "BL" } else { "B" };
            format!("  ; {} {:#x}", kind, target)
        } else if insn == 0xee1d0f70 {
            "  ; MRC p15, 0, R0, c13, c0, 3 (TPIDRURO)".to_string()
        } else if (insn & 0x0FFF0FFF) == 0x0E1D0F70 {
            let rd = (insn >> 12) & 0xF;
            format!("  ; MRC p15, 0, R{}, c13, c0, 3 (TPIDRURO)", rd)
        } else {
            String::new()
        };
        log::error!("  {:#010x}: {:#010x}{}", addr, insn, decoded);
    }

    let init_libc0_addr: u64 = 0x20629c;
    log::error!("=== __nnDetailInitLibc0 PLT stub at {:#x} ===", init_libc0_addr);
    for i in 0..16u64 {
        let addr = init_libc0_addr + i * 4;
        let insn = mem.read_32(addr);
        let decoded = if (insn & 0x0F000000) == 0x0A000000 || (insn & 0x0F000000) == 0x0B000000 {
            let offset = ((insn & 0x00FFFFFF) as i32) << 8 >> 6;
            let target = (addr as i64 + 8 + offset as i64) as u64;
            let kind = if (insn & 0x0F000000) == 0x0B000000 { "BL" } else { "B" };
            format!("  ; {} {:#x}", kind, target)
        } else {
            String::new()
        };
        log::error!("  {:#010x}: {:#010x}{}", addr, insn, decoded);
    }

    log::error!("=== TLS area at {:#x} ===", tls_base);
    for i in 0..16u64 {
        log::error!("  [{:#x}+{:#x}] = {:#010x}", tls_base, i * 4, mem.read_32(tls_base + i * 4));
    }
}

/// Dump abort context at SVC #135 (SetThreadPriority — abort handler entry).
pub fn dump_abort_context(
    shared_memory: &Arc<RwLock<ProcessMemoryData>>,
    ctx: &ruzu_core::arm::arm_interface::ThreadContext,
) {
    let mem = shared_memory.read().unwrap();
    for reg_name in ["r4", "r8"] {
        let reg_val = match reg_name {
            "r4" => ctx.r[4],
            "r8" => ctx.r[8],
            _ => 0,
        };
        if reg_val > 0x200000 && mem.is_valid_range(reg_val, 64) {
            log::error!("=== ABORT CONTEXT at {}={:#x} ===", reg_name, reg_val);
            for j in 0..16u64 {
                let val = mem.read_32(reg_val + j * 4);
                log::error!("  [+{:#x}] = {:#010x}", j * 4, val);
                if val >= 0x200000 && val < 0x80000000 && mem.is_valid_range(val as u64, 64) {
                    let mut s = Vec::new();
                    for k in 0..64u64 {
                        let b = mem.read_8(val as u64 + k);
                        if b == 0 { break; }
                        s.push(b);
                    }
                    if !s.is_empty() {
                        log::error!("    -> string: \"{}\"", String::from_utf8_lossy(&s));
                    }
                }
            }
        }
    }
}

/// Dump stack backtrace and abort info at SVC #136.
pub fn dump_abort_backtrace(
    shared_memory: &Arc<RwLock<ProcessMemoryData>>,
    ctx: &ruzu_core::arm::arm_interface::ThreadContext,
) {
    let mem = shared_memory.read().unwrap();
    let sp = ctx.sp;
    log::error!("=== STACK BACKTRACE at SVC #136 (abort entry) ===");
    log::error!("  SP={:#x} LR={:#x}", sp, ctx.lr);
    for i in 0..16u64 {
        let addr = sp + i * 4;
        if mem.is_valid_range(addr, 4) {
            let val = mem.read_32(addr);
            if val >= 0x200000 && val < 0x2400000 {
                log::error!("  [SP+{:#x}] = {:#010x} <- possible return address", i * 4, val);
            } else {
                log::error!("  [SP+{:#x}] = {:#010x}", i * 4, val);
            }
        }
    }

    let abort_info = ctx.r[4];
    if abort_info > 0x200000 && mem.is_valid_range(abort_info, 64) {
        log::error!("=== ABORT INFO at r4={:#x} ===", abort_info);
        for j in 0..16u64 {
            let val = mem.read_32(abort_info + j * 4);
            let mut str_found = String::new();
            if val >= 0x200000 && val < 0x80000000 && mem.is_valid_range(val as u64, 4) {
                let mut s = Vec::new();
                for k in 0..256u64 {
                    if !mem.is_valid_range(val as u64 + k, 1) { break; }
                    let b = mem.read_8(val as u64 + k);
                    if b == 0 { break; }
                    if b < 0x20 || b > 0x7e { break; }
                    s.push(b);
                }
                if s.len() >= 2 {
                    str_found = format!(" => \"{}\"", String::from_utf8_lossy(&s));
                }
            }
            log::error!("  [+{:#x}] = {:#010x}{}", j * 4, val, str_found);
        }
    }
}

/// Dump module objects after rtld relocation (after QueryMemory calls).
pub fn dump_module_objects(shared_memory: &Arc<RwLock<ProcessMemoryData>>, query_memory_count: u32, svc_num: u32) {
    let mem = shared_memory.read().unwrap();
    let dump_module_object = |name: &str, base: u64| {
        if !mem.is_valid_range(base + 4, 4) {
            log::info!("{} @ {:#x}: module header unavailable", name, base);
            return;
        }
        let mod_offset = mem.read_32(base + 4) as u64;
        let mod_addr = base + mod_offset;
        if !mem.is_valid_range(mod_addr, 0x1c) {
            log::info!("{} @ {:#x}: MOD0 out of range (offset={:#x})", name, base, mod_offset);
            return;
        }
        let magic = mem.read_32(mod_addr);
        let dynamic_offset = mem.read_32(mod_addr + 4) as u64;
        let bss_start_offset = mem.read_32(mod_addr + 8) as u64;
        let bss_end_offset = mem.read_32(mod_addr + 12) as u64;
        let module_offset = mem.read_32(mod_addr + 24) as u64;
        let module_object = base + module_offset;
        log::info!(
            "{} @ {:#x}: MOD0={:#x} mod={:#x} dyn={:#x} bss=[{:#x}..{:#x}) module_obj={:#x}",
            name, base, magic, mod_addr, mod_addr + dynamic_offset,
            base + bss_start_offset, base + bss_end_offset, module_object
        );
        if !mem.is_valid_range(module_object, 0x40) {
            log::info!("  {} module object @ {:#x} is out of range", name, module_object);
            return;
        }
        for i in 0..16u64 {
            let addr = module_object + i * 4;
            let val = mem.read_32(addr);
            log::info!("  [{} + {:#04x}] = {:#010x}", name, i * 4, val);
        }
    };

    log::info!(
        "=== RTLD MODULE OBJECT DUMP after {} QueryMemory calls, first non-QM SVC=0x{:02X} ===",
        query_memory_count, svc_num
    );
    dump_module_object("rtld", 0x200000);
    dump_module_object("main", 0x206000);
    dump_module_object("subsdk0", 0x1512000);
    dump_module_object("subsdk1", 0x16AB000);
    dump_module_object("subsdk2", 0x16D3000);
    dump_module_object("subsdk3", 0x16E6000);
    dump_module_object("subsdk4", 0x1723000);
    dump_module_object("sdk", 0x1C9C000);
    log::info!("=== RTLD LIST REGION DUMP 0x2051d0..0x205240 ===");
    for addr in (0x2051d0u64..0x205240u64).step_by(4) {
        log::info!("  [{:#010x}] = {:#010x}", addr, mem.read_32(addr));
    }
}

/// Dump instruction window around an address.
pub fn dump_instruction_window(
    shared_memory: &Arc<RwLock<ProcessMemoryData>>,
    label: &str,
    center: u64,
    mark_pc: Option<u64>,
) {
    let mem = shared_memory.read().unwrap();
    let start = center.saturating_sub(0x10);
    log::error!("{} around {:#x}", label, center);
    for addr in (start..=center.saturating_add(0x10)).step_by(4) {
        if !mem.is_valid_range(addr, 4) {
            log::error!("  [{:#010x}] <unmapped>", addr);
            continue;
        }
        let insn = mem.read_32(addr);
        let decoded = decode_arm(insn);
        let marker = if Some(addr) == mark_pc {
            " <PC>"
        } else if addr == center {
            " <EXC>"
        } else {
            ""
        };
        log::error!("  [{:#010x}] {:#010x} {:?}{}", addr, insn, decoded.id, marker);
    }
}

/// Dump ArbitrateLock debug info.
pub fn dump_arbitrate_lock(
    shared_memory: &Arc<RwLock<ProcessMemoryData>>,
    ctx: &ruzu_core::arm::arm_interface::ThreadContext,
    svc_args: &[u64; 8],
) {
    let mem = shared_memory.read().unwrap();
    let addr = svc_args[1] as u64;
    let handle = svc_args[0] as u32;
    let tag = svc_args[2] as u32;

    log::info!("=== FIRST ArbitrateLock DUMP ===");
    log::info!("  handle={:#x}, addr={:#x}, tag={:#x}, addr%4={}", handle, addr, tag, addr % 4);
    if mem.is_valid_range(addr, 4) {
        let mutex_val = mem.read_32(addr);
        log::info!(
            "  mutex word @ {:#x} = {:#010x} (owner_handle={:#x}, has_waiters={})",
            addr, mutex_val, mutex_val & !0x40000000u32, mutex_val & 0x40000000 != 0
        );
    } else {
        log::info!("  mutex word @ {:#x} = <unmapped>", addr);
    }

    for (label, base) in [("SVC site (PC)", ctx.pc as u64), ("Caller (LR)", ctx.lr as u64)] {
        log::info!("  {} = {:#x}:", label, base);
        let start = base.saturating_sub(0x20);
        for a in (start..=base.saturating_add(0x20)).step_by(4) {
            if !mem.is_valid_range(a, 4) { continue; }
            let insn = mem.read_32(a);
            let decoded = decode_arm(insn);
            let marker = if a == base { " <--" } else { "" };
            log::info!("    [{:#010x}] {:#010x} {:?}{}", a, insn, decoded.id, marker);
        }
    }

    log::info!("  Memory around mutex addr {:#x}:", addr);
    let mstart = addr.saturating_sub(0x10);
    for a in (mstart..addr.saturating_add(0x20)).step_by(4) {
        if mem.is_valid_range(a, 4) {
            log::info!("    [{:#010x}] = {:#010x}", a, mem.read_32(a));
        }
    }
    log::info!("=== END ArbitrateLock DUMP ===");
}
