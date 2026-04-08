• Je retire d’abord les probes les plus clairement temporaires sur le chemin de debug MK8D: RUZU_TRACE_*, dumps TLS/IPC, WriteBuffer* probe, HID logs
  ciblés, et sampling scheduler. Je laisse les vrais logs normaux et les fonctionnalités de debug générales qui ne sont pas des probes de campagne.

• Explored
  └ Search maybe_log_thread_pc_sample|RUZU_TRACE_ENABLE_SCHED|multi_core_run_guest_thread:|RUZU_TRACE_SVC_RET|RUZU_TRACE_PREEMPT in cpu_manager.rs
    Read cpu_manager.rs

• Edited core/src/cpu_manager.rs (+1 -83)
     12  use common::thread::Barrier;
     13 -use std::collections::HashMap;
     13  use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
     15 -use std::sync::{Arc, Mutex, OnceLock};
     14 +use std::sync::{Arc, Mutex};
     15
        ⋮
     67  impl CpuManager {
     69 -    fn maybe_log_thread_pc_sample(
     70 -        thread_arc: &Arc<Mutex<super::hle::kernel::k_thread::KThread>>,
     71 -        jit_ref: &dyn crate::arm::arm_interface::ArmInterface,
     72 -    ) {
     73 -        static THREAD_SET: OnceLock<Option<Vec<u64>>> = OnceLock::new();
     74 -        static INTERVAL_MS: OnceLock<u128> = OnceLock::new();
     75 -        static LAST_LOG_MS: OnceLock<Mutex<HashMap<u64, u128>>> = OnceLock::new();
     76 -
     77 -        let Some(thread_ids) = THREAD_SET
     78 -            .get_or_init(|| {
     79 -                std::env::var("RUZU_SAMPLE_THREAD_IDS")
     80 -                    .ok()
     81 -                    .and_then(|value| {
     82 -                        let ids = value
     83 -                            .split(',')
     84 -                            .filter_map(|part| part.trim().parse::<u64>().ok())
     85 -                            .collect::<Vec<_>>();
     86 -                        if ids.is_empty() {
     87 -                            None
     88 -                        } else {
     89 -                            Some(ids)
     90 -                        }
     91 -                    })
     92 -            })
     93 -            .as_ref()
     94 -        else {
     95 -            return;
     96 -        };
     97 -
     98 -        let interval_ms = *INTERVAL_MS.get_or_init(|| {
     99 -            std::env::var("RUZU_SAMPLE_INTERVAL_MS")
    100 -                .ok()
    101 -                .and_then(|s| s.trim().parse::<u128>().ok())
    102 -                .unwrap_or(250)
    103 -        });
    104 -
    105 -        let (thread_id, state, disable_dispatch, active_core, current_core) = {
    106 -            let thread = thread_arc.lock().unwrap();
    107 -            (
    108 -                thread.get_thread_id(),
    109 -                thread.get_raw_state(),
    110 -                thread.get_disable_dispatch_count(),
    111 -                thread.get_active_core(),
    112 -                thread.get_current_core(),
    113 -            )
    114 -        };
    115 -
    116 -        if !thread_ids.contains(&thread_id) {
    117 -            return;
    118 -        }
    119 -
    120 -        let now_ms = std::time::SystemTime::now()
    121 -            .duration_since(std::time::UNIX_EPOCH)
    122 -            .map(|d| d.as_millis())
    123 -            .unwrap_or(0);
    124 -        let last_log = LAST_LOG_MS.get_or_init(|| Mutex::new(HashMap::new()));
    125 -        {
    126 -            let mut last = last_log.lock().unwrap();
    127 -            let entry = last.entry(thread_id).or_insert(0);
    128 -            if now_ms.saturating_sub(*entry) < interval_ms {
    129 -                return;
    130 -            }
    131 -            *entry = now_ms;
    132 -        }
    133 -
    134 -        let mut ctx = crate::arm::arm_interface::ThreadContext::default();
    135 -        jit_ref.get_context(&mut ctx);
    136 -        log::info!(
    137 -            "thread_pc_sample: tid={} pc=0x{:08X} sp=0x{:08X} lr=0x{:08X} r0=0x{:08X} state={:?} disable_dispatch={} active_core={} current_
         core={}",
    138 -            thread_id,
    139 -            ctx.pc as u32,
    140 -            ctx.sp as u32,
    141 -            ctx.lr as u32,
    142 -            ctx.r[0] as u32,
    143 -            state,
    144 -            disable_dispatch,
    145 -            active_core,
    146 -            current_core,
    147 -        );
    148 -    }
    149 -
     68      /// Creates a new CpuManager.

• Edited core/src/cpu_manager.rs (+0 -11)
    234                  let guard = scheduler_arc.lock().unwrap();
    235 -                if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    236 -                    log::info!(
    237 -                        "CpuManager::reschedule_current_core_raw core={} current={:?} highest={:?} needs_scheduling={}",
    238 -                        guard.core_id,
    239 -                        guard.current_thread_id,
    240 -                        guard.state.highest_priority_thread_id,
    241 -                        guard.state.needs_scheduling.load(Ordering::SeqCst),
    242 -                    );
    243 -                }
    235                  &*guard as *const super::hle::kernel::k_scheduler::KScheduler
        ⋮
    241              }
    251 -        } else if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    252 -            log::info!("CpuManager::reschedule_current_core_raw no current scheduler");
    242          }

• Edited core/src/cpu_manager.rs (+0 -18)
    366      pub fn multi_core_run_guest_thread(kernel: &KernelCore) {
    367 -        log::info!(
    368 -            "multi_core_run_guest_thread: ENTERED on core {}",
    369 -            kernel.current_physical_core_index()
    370 -        );
    371 -
    372 -        let cur_thread = super::hle::kernel::kernel::get_current_thread_pointer();
    373 -        log::info!(
    374 -            "multi_core_run_guest_thread: current_thread={}, has_parent={}",
    375 -            cur_thread
    376 -                .as_ref()
    377 -                .map(|t| t.lock().unwrap().get_thread_id())
    378 -                .unwrap_or(u64::MAX),
    379 -            cur_thread
    380 -                .as_ref()
    381 -                .map(|t| t.lock().unwrap().parent.is_some())
    382 -                .unwrap_or(false),
    383 -        );
    384 -
    367          // Upstream: auto* thread = Kernel::GetCurrentThreadPointer(kernel);

• Edited core/src/cpu_manager.rs (+0 -25)
    491
    492 -            Self::maybe_log_thread_pc_sample(&thread_arc, jit_ref);
    493 -
    492              match event {
        ⋮
    497                      let current_thread_id = thread_arc.lock().unwrap().get_thread_id();
    500 -                    if std::env::var_os("RUZU_TRACE_SVC_RET").is_some() {
    501 -                        let thread = thread_arc.lock().unwrap();
    502 -                        let tid = thread.get_thread_id();
    503 -                        if tid >= 18 {
         X},0x{:X},0x{:X},0x{:X},0x{:X}]",
    508 -                                tid,
    509 -                                kernel.current_physical_core_index(),
    510 -                                svc_num,
    511 -                                ctx.pc,
    512 -                                svc_args[0],
    513 -                                svc_args[1],
    514 -                                svc_args[2],
    515 -                                svc_args[3],
    516 -                                svc_args[4],
    517 -                                svc_args[5],
    518 -                                svc_args[6],
    519 -                                svc_args[7],
    520 -                            );
    521 -                        }
    522 -                    }
    498                      let system_ref = kernel.system();

• Explored
  └ Search RUZU_TRACE_PREEMPT|HALT #|CpuManager::multi_core_run_guest_thread HALT in cpu_manager.rs
    Read cpu_manager.rs

• Edited core/src/cpu_manager.rs (+0 -22)
    533
    534 -                    if std::env::var_os("RUZU_TRACE_PREEMPT").is_some() {
    535 -                        use std::sync::atomic::{AtomicU32, Ordering};
    536 -                        static HALT_LOGS: AtomicU32 = AtomicU32::new(0);
    537 -                        let thread = thread_arc.lock().unwrap();
    538 -                        let tid = thread.get_thread_id();
    539 -                        let n = HALT_LOGS.fetch_add(1, Ordering::Relaxed);
    540 -                        if tid >= 18 && n < 256 && !halt_reason.is_empty() {
    541 -                            let mut ctx = crate::arm::arm_interface::ThreadContext::default();
    542 -                            jit_ref.get_context(&mut ctx);
    543 -                            log::info!(
    544 -                                "CpuManager::multi_core_run_guest_thread HALT #{} tid={} core={} pc=0x{:08X} interrupt={} data_abort={} pref
         etch_abort={} breakpoint={}",
    545 -                                n,
    546 -                                tid,
    547 -                                kernel.current_physical_core_index(),
    548 -                                ctx.pc,
    549 -                                interrupt,
    550 -                                data_abort,
    551 -                                prefetch_abort,
    552 -                                breakpoint
    553 -                            );
    554 -                        }
    555 -                    }
    534                  }

• Edited core/src/hle/kernel/svc/svc_ipc.rs (+2 -143)
     27      };
     28 -    let (tls_address, thread_id, saved_tpidr) = {
     28 +    let tls_address = {
     29          let thread = current_thread.lock().unwrap();
     30 -        (
     31 -            thread.get_tls_address().get(),
     32 -            thread.thread_id,
     33 -            thread.thread_context.tpidr,
     34 -        )
     30 +        thread.get_tls_address().get()
     31      };
        ⋮
     62      // where memory = client_thread->GetOwnerProcess()->GetMemory()
     67 -    let debug_memory = shared_memory.clone();
     63      let mut context =
        ⋮
     94      );
    100 -
    101 -    if context.get_command_type() == ipc::CommandType::Invalid {
    102 -        let runtime_tpidr = {
    103 -            let core_id = system.cpu_manager.current_core();
    104 -            let mut process = system.current_process_arc().lock().unwrap();
    105 -            if let Some(jit) = process.get_arm_interface_mut(core_id) {
    106 -                let mut ctx = crate::arm::arm_interface::ThreadContext::default();
    107 -                jit.get_context(&mut ctx);
    108 -                ctx.tpidr
    109 -            } else {
    110 -                0
    111 -            }
    112 -        };
    113 -        if let Some(memory) = system.get_svc_memory() {
    114 -            let m = memory.lock().unwrap();
    115 -            log::error!(
    116 -                "  SendSyncRequest INVALID CMD: thread_id={} handle={:#x} tls={:#x} saved_tpidr={:#x} runtime_tpidr={:#x} service={} words=[
         {:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    117 -                thread_id,
    118 -                session_handle,
    119 -                tls_address,
    120 -                saved_tpidr,
    121 -                runtime_tpidr,
    122 -                session_handler_name,
    123 -                m.read_32(tls_address),
    124 -                m.read_32(tls_address + 4),
    125 -                m.read_32(tls_address + 8),
    126 -                m.read_32(tls_address + 12),
    127 -                m.read_32(tls_address + 16),
    128 -                m.read_32(tls_address + 20),
    129 -                m.read_32(tls_address + 24),
    130 -                m.read_32(tls_address + 28),
    131 -            );
    132 -            if tls_address >= 0x200 {
    133 -                let prev_tls = tls_address - 0x200;
    134 -                log::error!(
    135 -                    "  SendSyncRequest INVALID CMD neighbor-0x200: addr={:#x} words=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    136 -                    prev_tls,
    137 -                    m.read_32(prev_tls),
    138 -                    m.read_32(prev_tls + 4),
    139 -                    m.read_32(prev_tls + 8),
    140 -                    m.read_32(prev_tls + 12),
    141 -                    m.read_32(prev_tls + 16),
    142 -                    m.read_32(prev_tls + 20),
    143 -                    m.read_32(prev_tls + 24),
    144 -                    m.read_32(prev_tls + 28),
    145 -                );
    146 -            }
    147 -            if tls_address >= 0x400 {
    148 -                let prev_prev_tls = tls_address - 0x400;
    149 -                log::error!(
    150 -                    "  SendSyncRequest INVALID CMD neighbor-0x400: addr={:#x} words=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    151 -                    prev_prev_tls,
    152 -                    m.read_32(prev_prev_tls),
    153 -                    m.read_32(prev_prev_tls + 4),
    154 -                    m.read_32(prev_prev_tls + 8),
    155 -                    m.read_32(prev_prev_tls + 12),
    156 -                    m.read_32(prev_prev_tls + 16),
    157 -                    m.read_32(prev_prev_tls + 20),
    158 -                    m.read_32(prev_prev_tls + 24),
    159 -                    m.read_32(prev_prev_tls + 28),
    160 -                );
    161 -            }
    162 -        } else {
    163 -            let mem = debug_memory.read().unwrap();
    164 -            log::error!(
    165 -                "  SendSyncRequest INVALID CMD: thread_id={} handle={:#x} tls={:#x} saved_tpidr={:#x} runtime_tpidr={:#x} service={} words=[
         {:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    166 -                thread_id,
    167 -                session_handle,
    168 -                tls_address,
    169 -                saved_tpidr,
    170 -                runtime_tpidr,
    171 -                session_handler_name,
    172 -                mem.read_32(tls_address),
    173 -                mem.read_32(tls_address + 4),
    174 -                mem.read_32(tls_address + 8),
    175 -                mem.read_32(tls_address + 12),
    176 -                mem.read_32(tls_address + 16),
    177 -                mem.read_32(tls_address + 20),
    178 -                mem.read_32(tls_address + 24),
    179 -                mem.read_32(tls_address + 28),
    180 -            );
    181 -            if tls_address >= 0x200 {
    182 -                let prev_tls = tls_address - 0x200;
    183 -                log::error!(
    184 -                    "  SendSyncRequest INVALID CMD neighbor-0x200: addr={:#x} words=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    185 -                    prev_tls,
    186 -                    mem.read_32(prev_tls),
    187 -                    mem.read_32(prev_tls + 4),
    188 -                    mem.read_32(prev_tls + 8),
    189 -                    mem.read_32(prev_tls + 12),
    190 -                    mem.read_32(prev_tls + 16),
    191 -                    mem.read_32(prev_tls + 20),
    192 -                    mem.read_32(prev_tls + 24),
    193 -                    mem.read_32(prev_tls + 28),
    194 -                );
    195 -            }
    196 -            if tls_address >= 0x400 {
    197 -                let prev_prev_tls = tls_address - 0x400;
    198 -                log::error!(
    199 -                    "  SendSyncRequest INVALID CMD neighbor-0x400: addr={:#x} words=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    200 -                    prev_prev_tls,
    201 -                    mem.read_32(prev_prev_tls),
    202 -                    mem.read_32(prev_prev_tls + 4),
    203 -                    mem.read_32(prev_prev_tls + 8),
    204 -                    mem.read_32(prev_prev_tls + 12),
    205 -                    mem.read_32(prev_prev_tls + 16),
    206 -                    mem.read_32(prev_prev_tls + 20),
    207 -                    mem.read_32(prev_prev_tls + 24),
    208 -                    mem.read_32(prev_prev_tls + 28),
    209 -                );
    210 -            }
    211 -        }
    212 -    }
    213 -
    214 -    // Debug: if handle is 0, the game didn't receive the previous response correctly
    215 -    if session_handle == 0 {
    216 -        if let Some(memory) = system.get_svc_memory() {
    217 -            let m = memory.lock().unwrap();
    218 -            log::error!(
    219 -                "  SendSyncRequest with handle=0! TLS at send time: [{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    220 -                m.read_32(tls_address), m.read_32(tls_address + 4),
    221 -                m.read_32(tls_address + 8), m.read_32(tls_address + 12),
    222 -                m.read_32(tls_address + 16), m.read_32(tls_address + 20),
    223 -                m.read_32(tls_address + 24), m.read_32(tls_address + 28),
    224 -            );
    225 -        } else {
    226 -            let mem = debug_memory.read().unwrap();
    227 -            log::error!(
    228 -                "  SendSyncRequest with handle=0! TLS at send time: [{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    229 -                mem.read_32(tls_address), mem.read_32(tls_address + 4),
    230 -                mem.read_32(tls_address + 8), mem.read_32(tls_address + 12),
    231 -                mem.read_32(tls_address + 16), mem.read_32(tls_address + 20),
    232 -                mem.read_32(tls_address + 24), mem.read_32(tls_address + 28),
    233 -            );
    234 -        }
    235 -    }
     95

• Edited core/src/hle/service/hle_ipc.rs (+0 -28)
    925          self.write_guest_memory(address, &data[..size]);
    926 -        if (size == 32 && (data[0] == 0x04 || data[0] == 0x08))
    927 -            || (size == 24 && data.len() >= 24 && data[4] == 0x00 && data[5] == 0x00)
    928 -            || (size == 48 && data.len() >= 48 && data[24] == 0x00 && data[28] == 0x00)
    929 -            || (size == 64 && data.len() >= 64 && data[8] == 0x30 && data[12] == 0x00)
    930 -        {
    931 -            let verify = self.read_guest_memory(address, size.min(64));
    932 -            log::debug!(
    933 -                "WriteBufferB probe addr=0x{:X} size=0x{:X} data_prefix={:02X?} verify_prefix={:02X?}",
    934 -                address,
    935 -                size,
    936 -                &data[..size.min(64)],
        ⋮
    939          self.write_guest_memory(address, &data[..size]);
    954 -        if (size == 32 && (data[0] == 0x04 || data[0] == 0x08))
    955 -            || (size == 24 && data.len() >= 24 && data[4] == 0x00 && data[5] == 0x00)
    956 -            || (size == 48 && data.len() >= 48 && data[24] == 0x00 && data[28] == 0x00)
    957 -            || (size == 64 && data.len() >= 64 && data[8] == 0x30 && data[12] == 0x00)
    958 -        {
    959 -            let verify = self.read_guest_memory(address, size.min(64));
    960 -            log::debug!(
    965 -                &verify[..verify.len().min(64)]
    966 -            );
    967 -        }
    940          size

• Explored
  └ Search CreateAppletResource called in hid_server.rs
    Read hid_server.rs

• Edited core/src/hle/service/hid/hid_server.rs (+0 -5)
    656          let result = server.resource_manager.lock().create_applet_resource(aruid);
    657 -        log::debug!(
    658 -            "IHidServer::CreateAppletResource called, aruid={}, result=0x{:X}",
    659 -            aruid,
    660 -            result.raw()
    661 -        );
    657

• Explored
  └ Search GetSharedMemoryHandle called in applet_resource.rs
    Read applet_resource.rs

• Edited core/src/hle/service/hid/applet_resource.rs (+0 -6)
    108
    109 -        log::debug!(
    110 -            "IAppletResource::GetSharedMemoryHandle called, aruid={}, handle={:#x}",
    111 -            service.aruid,
    112 -            handle.unwrap_or(0)
    113 -        );
    114 -
    109          let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);

• Edited core/src/hle/kernel/global_scheduler_context.rs (+0 -28)
    104
    105 -        let trace_sched = std::env::var_os("RUZU_TRACE_SCHED_UPDATE").is_some() && thread_id >= 17;
    106 -        if trace_sched {
    107 -            log::info!(
    108 -                "GSC::on_thread_state_changed tid={} old={:?} new={:?} prio={} core={} aff=0x{:X}",
    109 -                thread_id,
    110 -                old_state,
    111 -                new_state,
    112 -                priority,
    113 -                active_core,
    114 -                affinity,
    115 -            );
    145
    159 -        if trace_sched {
    160 -            let front = if active_core >= 0 {
    161 -                self.m_priority_queue.get_scheduled_front(active_core)
    162 -            } else {
    163 -                None
    164 -            };
    165 -            log::info!(
    166 -                "GSC::on_thread_state_changed post tid={} front_core{}={:?} update_needed={}",
    167 -                thread_id,
    168 -                active_core,
    169 -                front,
    170 -                self.m_scheduler_update_needed
    171 -                    .load(std::sync::atomic::Ordering::Relaxed),
    172 -            );
    173 -        }
    146      }

    Explored
  └ Search trace_sleep_timer\(|RUZU_TRACE_SLEEP_TIMER in k_hardware_timer.rs
    Read k_hardware_timer.rs

• Edited core/src/hle/kernel/k_hardware_timer.rs (+1 -115)
     15
     16 -fn trace_sleep_timer() -> bool {
     17 -    std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some()
     18 -}
     19 -
     16  /// The kernel hardware timer.
        ⋮
     93          guard.core_timing = Some(core_timing);
     98 -        if trace_sleep_timer() {
     99 -            log::info!("KHardwareTimer::wire_callback");
    100 -        }
     94      }
        ⋮
    134          };
    142 -        if trace_sleep_timer() {
    143 -            log::info!(
    144 -                "KHardwareTimer::register_absolute_task tid={} task_time={} current_tick={} wakeup_before={}",
    145 -                thread_id,
    146 -                task_time,
    147 -                self.get_tick(),
    148 -                self.m_wakeup_time
    149 -            );
    150 -        }
    151 -
    135          let thread_ptr = {
        ⋮
    149              }
    167 -        }
    168 -        if trace_sleep_timer() {
    169 -            log::info!(
    170 -                "KHardwareTimer::register_absolute_task done tid={} wakeup_after={}",
    171 -                thread_id,
    172 -                self.m_wakeup_time
    173 -            );
    150          }
    175 -
    151          if had_current {
        ⋮
    163      ) {
    189 -        if trace_sleep_timer() {
    190 -            log::info!(
    191 -                "KHardwareTimer::register_absolute_task_by_id tid={} task_time={} current_tick={} wakeup_before={}",
    192 -                thread_id,
    193 -                task_time,
    194 -                self.get_tick(),
    195 -                self.m_wakeup_time
    196 -            );
    197 -        }
    198 -
    164          self.thread_ptrs.insert(thread_id, thread_ptr);
        ⋮
    176
    212 -        if trace_sleep_timer() {
    213 -            log::info!(
    214 -                "KHardwareTimer::register_absolute_task_by_id done tid={} wakeup_after={}",
    215 -                thread_id,
    216 -                self.m_wakeup_time
    217 -            );
    218 -        }
    219 -
    177          if had_current {
        ⋮
    193          };
    237 -        if trace_sleep_timer() {
    238 -            log::info!(
    239 -                "KHardwareTimer::cancel_task tid={} task_time={} wakeup_before={}",
    240 -                thread_id,
    241 -                task_time,
    242 -                self.m_wakeup_time
    243 -            );
    244 -        }
    245 -
    194          let had_current = super::kernel::with_current_thread_fast_mut(|t| {
        ⋮
    203          self.thread_ptrs.remove(&thread_id);
    256 -        if trace_sleep_timer() {
    257 -            log::info!(
    258 -                "KHardwareTimer::cancel_task done tid={} wakeup_after={}",
    259 -                thread_id,
    260 -                self.m_wakeup_time
    261 -            );
    262 -        }
    263 -
    204          if had_current {
        ⋮
    211      pub fn cancel_task_by_id(&mut self, thread_id: u64, task_time: i64) {
    272 -        if trace_sleep_timer() {
    273 -            log::info!(
    274 -                "KHardwareTimer::cancel_task_by_id tid={} task_time={} wakeup_before={}",
    275 -                thread_id,
    276 -                task_time,
    277 -                self.m_wakeup_time
    278 -            );
    279 -        }
    280 -
    212          let had_current = super::kernel::with_current_thread_fast_mut(|t| {
        ⋮
    221
    291 -        if trace_sleep_timer() {
    292 -            log::info!(
    293 -                "KHardwareTimer::cancel_task_by_id done tid={} wakeup_after={}",
    294 -                thread_id,
    295 -                self.m_wakeup_time
    296 -            );
    297 -        }
    298 -
    222          if had_current {
        ⋮
    233          self.m_wakeup_time = wakeup_time;
    311 -        if trace_sleep_timer() {
    312 -            log::info!(
    313 -                "KHardwareTimer::enable_interrupt wakeup_time={}",
    314 -                wakeup_time
    315 -            );
    316 -        }
    234          if let (Some(ref core_timing), Some(ref event_type)) =
        ⋮
    278      fn do_task(&mut self) {
    362 -        if trace_sleep_timer() {
    363 -            log::info!(
    364 -                "KHardwareTimer::do_task enter wakeup_time={} current_tick={}",
    365 -                self.m_wakeup_time,
    366 -                self.get_tick()
    367 -            );
    368 -        }
    279          if !self.get_interrupt_enabled() {
        ⋮
    292          for task_id in fired_task_ids {
    383 -            if trace_sleep_timer() {
    384 -                log::info!(
    385 -                    "KHardwareTimer::do_task firing tid={} cur_tick={}",
    386 -                    task_id,
    387 -                    cur_tick
    388 -                );
    389 -            }
    390 -
    293              let target = if let Some(gsc) = gsc.as_ref() {
        ⋮
    319                  }
    418 -                None => {
    422 -                            task_id,
    423 -                            gsc.is_some(),
    424 -                            self.thread_ptrs.contains_key(&task_id),
    425 -                        );
    426 -                    }
    427 -                }
    327              self.enable_interrupt(next_time);
    435 -        }
    436 -        if trace_sleep_timer() {
    437 -            log::info!(
    438 -                "KHardwareTimer::do_task exit next_time={} wakeup_after={}",
    439 -                next_time,
    440 -                self.m_wakeup_time
    441 -            );
    328          }

• Explored
  └ Read svc_thread.rs

• Edited core/src/hle/kernel/svc/svc_thread.rs (+1 -1)
    59  ) -> ResultCode {
    60 -    log::info!(
    60 +    log::debug!(
    61          "svc::CreateThread called entry=0x{:08X}, arg=0x{:08X}, stack=0x{:08X}, prio={}, core={}",

• Explored
  └ Read svc_shared_memory.rs

─ Worked for 5m 35s ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

• Context compacted

  │ schedule_impl_fiber:|multi_core_run_guest_thread:|QueryEvent fd=|SubmitGPFIFOImpl|dispatch_calls complete|WaitProcessWideKeyAtomic enter|before
  │ … +2 lines
  └ 842
    /home/vricosti/Dev/emulators/ruzu/video_core/src/vulkan_common/vulkan_device.rs:275:    /// This is the main constructor that probes device
    … +170 lines
    /home/vricosti/Dev/emulators/ruzu/core/src/hle/service/nvdrv/devices/nvhost_ctrl.rs:406:                if
    std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {

• Ran git -C /home/vricosti/Dev/emulators/ruzu diff -w --name-only | wc -l && git -C /home/vricosti/Dev/emulators/ruzu diff --name-only | wc -l
  └ 842
    842

─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

• Il reste encore des probes explicites RUZU_TRACE_* et des dumps scheduler/GPU temporaires. Je lis uniquement les sections concernées pour les
  retirer sans toucher à la logique.

• Explored
  └ Read physical_core.rs, k_scheduler_lock.rs, k_scheduler.rs, k_scoped_scheduler_lock_and_sleep.rs, k_thread_queue.rs, k_thread.rs, kernel.rs,
         svc_condition_variable.rs, k_condition_variable.rs, nvdrv_interface.rs, nvhost_gpu.rs, nvhost_ctrl.rs, puller.rs, syncpoint_manager.rs,
         dma_pusher.rs

─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

• Je retire maintenant les RUZU_TRACE_* restants et les info! de diagnostic encore visibles dans le scheduler, le sleep path et le GPU submit path.
  Je ne touche pas à la logique, seulement au bruit d’investigation.

• Edited 15 files (+5 -479)
  └ core/src/hle/kernel/k_condition_variable.rs (+2 -90)
    299
    300 -        if std::env::var_os("RUZU_TRACE_ARB").is_some() {
    301 -            let next_owner_id = next_owner_thread
    302 -                .as_ref()
    303 -                .map(|thread| thread.lock().unwrap().get_thread_id());
    304 -            log::info!(
    305 -                "KConditionVariable::signal_to_address: owner_tid={} addr=0x{:X} next_owner={:?} rc=0x{:X}",
    306 -                current_thread_id,
    307 -                addr,
    308 -                next_owner_id,
    309 -                result.get_inner_value(),
    310 -            );
    311 -        }
    312 -
    300          result
        ⋮
    386          if test_tag != (handle | HANDLE_WAIT_MASK) {
    400 -            if std::env::var_os("RUZU_TRACE_ARB").is_some() {
    401 -                log::info!(
    402 -                    "KConditionVariable::wait_for_address fast-path: tid={} handle=0x{:08X} addr=0x{:X} tag=0x{:08X} test_tag=0x{:08X}",
    403 -                    current_thread_id,
    404 -                    handle,
    405 -                    addr,
    406 -                    value,
    407 -                    test_tag,
    408 -                );
    409 -            }
    387              return RESULT_SUCCESS;
        ⋮
    425
    449 -        let result = ResultCode::new(current_thread.lock().unwrap().get_wait_result());
    450 -        if std::env::var_os("RUZU_TRACE_ARB").is_some() {
    451 -            log::info!(
    452 -                "KConditionVariable::wait_for_address waited: tid={} owner_tid={} handle=0x{:08X} addr=0x{:X} tag=0x{:08X} rc=0x{:X}",
    453 -                current_thread_id,
    454 -                owner_thread_id,
    455 -                handle,
    456 -                addr,
    457 -                value,
    458 -                result.get_inner_value(),
    459 -            );
    460 -        }
    461 -        result
    426 +        ResultCode::new(current_thread.lock().unwrap().get_wait_result())
    427      }
        ⋮
    497      ) -> ResultCode {
    533 -        let trace = std::env::var_os("RUZU_TRACE_CV").is_some();
    534 -        let tid = if trace {
    535 -            Some(current_thread.lock().unwrap().get_thread_id())
    536 -        } else {
    537 -            None
    538 -        };
    498          let mut process_guard = process.lock().unwrap();
        ⋮
    509          if result == RESULT_SUCCESS {
    551 -            if let Some(tid) = tid {
    552 -                let state = current_thread.lock().unwrap().get_state();
    553 -                log::info!(
    554 -                    "KConditionVariable::wait enter-spin tid={} addr=0x{:X} key=0x{:X} tag=0x{:08X} timeout={} state_after_wait_locked={:?}"
         ,
    555 -                    tid,
    556 -                    addr,
    557 -                    key,
    558 -                    value,
    559 -                    timeout,
    560 -                    state,
    561 -                );
    562 -            }
    510              Self::wait_for_current_thread(process, current_thread);
    564 -            let wait_result = ResultCode::new(current_thread.lock().unwrap().get_wait_result());
    565 -            if let Some(tid) = tid {
    566 -                log::info!(
    567 -                    "KConditionVariable::wait complete tid={} addr=0x{:X} key=0x{:X} tag=0x{:08X} timeout={} rc=0x{:X}",
    568 -                    tid,
    569 -                    addr,
    570 -                    key,
    571 -                    value,
    572 -                    timeout,
    573 -                    wait_result.get_inner_value(),
    574 -                );
    575 -            }
    576 -            wait_result
    511 +            ResultCode::new(current_thread.lock().unwrap().get_wait_result())
    512          } else {
    578 -            if let Some(tid) = tid {
    579 -                log::info!(
    580 -                    "KConditionVariable::wait immediate tid={} addr=0x{:X} key=0x{:X} tag=0x{:08X} timeout={} rc=0x{:X}",
    581 -                    tid,
    582 -                    addr,
    583 -                    key,
    584 -                    value,
    585 -                    timeout,
    586 -                    result.get_inner_value(),
    587 -                );
    588 -            }
    513              result
        ⋮
    608          Self::begin_wait_condition_variable(current_thread, addr, key, value, timeout);
    685 -        if std::env::var_os("RUZU_TRACE_CV").is_some() && current_thread_id >= 18 {
    686 -            let guard = current_thread.lock().unwrap();
    687 -            log::info!(
    688 -                "KConditionVariable::wait_locked begin_wait tid={} addr=0x{:X} key=0x{:X} tag=0x{:08X} timeout={} state={:?}",
    689 -                current_thread_id,
    690 -                addr,
    691 -                key,
    692 -                value,
    693 -                timeout,
    694 -                guard.get_state(),
    695 -            );
    696 -        }
    609          let thread_key = current_thread.lock().unwrap().condition_variable_tree_key();

  └ core/src/hle/kernel/k_scheduler.rs (+0 -76)
     367      pub unsafe fn schedule_raw_if_needed(sched: *mut KScheduler) {
     368 -        if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
     369 -            log::info!(
     370 -                "KScheduler::schedule_raw_if_needed enter core={} current={:?} highest={:?} needs_scheduling={}",
     371 -                (*sched).core_id,
     372 -                (*sched).current_thread_id,
     373 -                (*sched).state.highest_priority_thread_id,
     374 -                (*sched).state.needs_scheduling.load(Ordering::SeqCst),
     375 -            );
     376 -        }
     368          // Trigger UpdateHighestPriorityThreads to sync PQ → highest_priority_thread_id.
         ⋮
     382
     392 -        if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
     393 -            log::info!(
     394 -                "KScheduler::schedule_raw_if_needed post-update core={} current={:?} highest={:?} needs_scheduling={}",
     395 -                (*sched).core_id,
     396 -                (*sched).current_thread_id,
     397 -                (*sched).state.highest_priority_thread_id,
     398 -                (*sched).state.needs_scheduling.load(Ordering::SeqCst),
     399 -            );
     400 -        }
     383          if (*sched).state.needs_scheduling.load(Ordering::SeqCst) {
         ⋮
     390              }
     409 -        } else if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
     410 -            log::info!(
     411 -                "KScheduler::schedule_raw_if_needed skipping schedule core={} current={:?} highest={:?}",
     412 -                (*sched).core_id,
     413 -                (*sched).current_thread_id,
     414 -                (*sched).state.highest_priority_thread_id,
     415 -            );
     391          }
         ⋮
    1306
    1332 -        log::info!(
    1333 -            "schedule_impl_fiber: core={} cur_thread={} highest={:?} has_gsc={} has_switch_fiber={}",
    1334 -            self.core_id,
    1335 -            cur_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
    1336 -            highest,
    1337 -            self.global_scheduler_context.is_some(),
    1338 -            self.switch_fiber.is_some(),
    1339 -        );
    1340 -
    1307          let target = if self.state.interrupt_task_runnable {
         ⋮
    1314          };
    1349 -
    1350 -        log::info!(
    1351 -            "schedule_impl_fiber: target={}",
    1352 -            target
    1353 -                .as_ref()
    1354 -                .map(|t| t.lock().unwrap().get_thread_id())
    1355 -                .unwrap_or(u64::MAX),
    1356 -        );
    1357 -
    1358 -        if target.is_none() && std::env::var("RUZU_LOG_RUNNABLE_DUMP").as_deref() == Ok("1") {
    1359 -            if let Some(ref gsc) = self.global_scheduler_context {
    1360 -                let threads = gsc.lock().unwrap().get_thread_list();
    1361 -                for thread in threads {
    1362 -                    let thread = thread.lock().unwrap();
    1363 -                    log::info!(
    1364 -                        "schedule_impl_fiber: no target dump tid={} prio={} state={:?} wait_reason={:?} active_core={} current_core={} tls=
          0x{:X}",
    1365 -                        thread.get_thread_id(),
    1366 -                        thread.get_priority(),
    1367 -                        thread.get_raw_state(),
    1368 -                        thread.get_wait_reason_for_debugging(),
    1369 -                        thread.get_active_core(),
    1370 -                        thread.get_current_core(),
    1371 -                        thread.get_tls_address().get(),
    1372 -                    );
    1373 -                }
    1374 -            }
    1375 -        }
    1315
         ⋮
    1318              if Arc::ptr_eq(cur, tgt) {
    1380 -                log::info!("schedule_impl_fiber: target == current, returning");
    1319                  std::sync::atomic::fence(Ordering::SeqCst);
         ⋮
    1331              let cur_lock = cur.lock().unwrap();
    1394 -            let has_ctx = cur_lock.host_context.is_some();
    1395 -            log::info!(
    1396 -                "schedule_impl_fiber: cur thread {} has host_context={}",
    1397 -                cur_lock.get_thread_id(),
    1398 -                has_ctx
    1399 -            );
    1332              if let Some(ref host_ctx) = cur_lock.host_context {
         ⋮
    1335                      drop(cur_lock); // release thread lock before yield
    1404 -                    log::info!("schedule_impl_fiber: yielding to switch_fiber NOW");
    1336                      Fiber::yield_to(host_weak, switch_fiber);
    1406 -                    log::info!("schedule_impl_fiber: returned from switch_fiber");
    1407 -                } else {
    1408 -                    log::warn!("schedule_impl_fiber: no switch_fiber");
    1337                  }
    1410 -            } else {
    1411 -                log::warn!("schedule_impl_fiber: current thread has no host_context");
    1338              }
    1413 -        } else {
    1414 -            log::warn!("schedule_impl_fiber: no current thread");
    1339          }

  └ core/src/hle/kernel/k_scheduler_lock.rs (+0 -31)
    113      pub fn set_callbacks(&mut self, callbacks: &'static SchedulerCallbacks) {
    114 -        if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    115 -            log::info!(
    116 -                "KAbstractSchedulerLock::set_callbacks lock_ptr=0x{:X} enable_cb=0x{:X} update_cb=0x{:X}",
    117 -                self as *const _ as usize,
    118 -                callbacks.enable_scheduling as usize,
    119 -                callbacks.update_highest_priority_threads as usize,
    120 -            );
    121 -        }
    114          self.callbacks = callbacks;
        ⋮
    169          let new_count = self.m_lock_count.get() - 1;
    178 -        if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    179 -            log::info!(
    180 -                "KAbstractSchedulerLock::unlock owner={} old_count={} new_count={}",
    181 -                self.m_owner_thread.load(Ordering::Relaxed),
    182 -                self.m_lock_count.get(),
    183 -                new_count
    184 -            );
    185 -        }
    170          self.m_lock_count.set(new_count);
        ⋮
    175              let cores_needing_scheduling = (self.callbacks.update_highest_priority_threads)();
    192 -            if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    193 -                log::info!(
    194 -                    "KAbstractSchedulerLock::unlock callbacks cores_needing_scheduling=0x{:X}",
    195 -                    cores_needing_scheduling
    196 -                );
    197 -            }
    176
        ⋮
    178              self.m_spin_lock.unlock();
    201 -
    202 -            if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    203 -                log::info!(
    204 -                    "KAbstractSchedulerLock::unlock invoking enable_scheduling lock_ptr=0x{:X} enable_cb=0x{:X} update_cb=0x{:X}",
    205 -                    self as *const _ as usize,
    206 -                    self.callbacks.enable_scheduling as usize,
    207 -                    self.callbacks.update_highest_priority_threads as usize,
    208 -                );
    209 -            }
    179              (self.callbacks.enable_scheduling)(cores_needing_scheduling);

  └ core/src/hle/kernel/k_scoped_scheduler_lock_and_sleep.rs (+0 -18)
    13
    14 -fn trace_sleep_timer() -> bool {
    15 -    std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some()
    16 -}
    17 -
    14  /// KScopedSchedulerLockAndSleep — acquires the scheduler lock on construction,
       ⋮
    51          let out_timer = timer.clone();
    56 -        if trace_sleep_timer() {
    57 -            log::info!(
    58 -                "KScopedSchedulerLockAndSleep::new timeout_tick={} has_timer={}",
    59 -                timeout_tick,
    60 -                timer.is_some()
    61 -            );
    62 -        }
    52          (
       ⋮
    77      fn drop(&mut self) {
    89 -        if trace_sleep_timer() {
    90 -            log::info!(
    91 -                "KScopedSchedulerLockAndSleep::drop timeout_tick={} has_timer={}",
    92 -                self.m_timeout_tick,
    93 -                self.m_timer.is_some()
    94 -            );
    95 -        }
    78          // Register the sleep timer if the timeout is still positive.

  └ core/src/hle/kernel/k_thread.rs (+0 -53)
    2750          self.sleep_deadline = deadline_from_timeout_tick(timeout, current_tick);
    2751 -        if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() && self.thread_id == 17 {
    2752 -            log::info!(
    2753 -                "KThread::sleep tid={} timeout={} current_tick={:?} deadline={:?}",
    2754 -                self.thread_id,
    2755 -                timeout,
    2756 -                current_tick,
    2757 -                self.sleep_deadline
    2758 -            );
    2759 -        }
    2751          let hardware_timer = super::kernel::get_hardware_timer_arc();
         ⋮
    2753          if self.scheduler_lock_ptr == 0 {
    2763 -            if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() {
    2764 -                log::info!(
    2765 -                    "KThread::sleep tid={} missing scheduler_lock_ptr",
    2766 -                    self.thread_id
    2767 -                );
    2768 -            }
    2754              return RESULT_INVALID_STATE.get_inner_value();
         ⋮
    2760              let scheduler_lock = unsafe { &*scheduler_lock_ptr };
    2776 -            if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() {
    2777 -                let current_tid = super::kernel::get_current_thread_id_fast();
    2778 -                log::info!(
    2779 -                    "KThread::sleep tid={} before scoped_sleep_lock",
    2780 -                    self.thread_id
    2781 -                );
    2782 -                log::info!(
    2783 -                    "KThread::sleep tid={} scheduler_lock count={} locked_by_current={} current_tid={:?}",
    2784 -                    self.thread_id,
    2785 -                    scheduler_lock.get_lock_count(),
    2786 -                    scheduler_lock.is_locked_by_current_thread(),
    2787 -                    current_tid
    2788 -                );
    2789 -            }
    2761              let (mut sleep_guard, timer) = KScopedSchedulerLockAndSleep::new(
         ⋮
    2767              );
    2797 -            if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() {
    2798 -                log::info!(
    2799 -                    "KThread::sleep tid={} after scoped_sleep_lock has_timer={}",
    2800 -                    self.thread_id,
    2801 -                    timer.is_some()
    2802 -                );
    2803 -            }
    2768
         ⋮
    2909      pub fn on_timer(&mut self) {
    2946 -        if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() && self.thread_id == 17 {
    2947 -            log::info!(
    2948 -                "KThread::on_timer tid={} state={:?} has_wait_queue={} wait_reason={:?}",
    2949 -                self.thread_id,
    2950 -                self.get_state(),
    2951 -                self.wait_queue.is_some(),
    2952 -                self.wait_reason_for_debugging,
    2953 -            );
    2954 -        }
    2910          if self.get_state() == ThreadState::WAITING {
         ⋮
    2913              if let Some(wait_queue) = self.wait_queue.clone() {
    2959 -                if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() && self.thread_id == 17 {
    2960 -                    log::info!(
    2961 -                        "KThread::on_timer cancel_wait tid={} end_wait_allowed={} has_timer={}",
    2962 -                        self.thread_id,
    2963 -                        wait_queue.end_wait_allowed,
    2964 -                        wait_queue.hardware_timer.is_some(),
    2965 -                    );
    2966 -                }
    2914                  wait_queue.cancel_wait(self, RESULT_TIMED_OUT.get_inner_value(), false);

  └ core/src/hle/kernel/k_thread_queue.rs (+0 -16)
    123      pub fn cancel_wait(&self, thread: &mut KThread, wait_result: u32, cancel_timer_task: bool) {
    124 -        if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() && thread.get_thread_id() == 17 {
    125 -            log::info!(
    126 -                "KThreadQueue::cancel_wait tid={} wait_result=0x{:X} cancel_timer={} state_before={:?}",
    127 -                thread.get_thread_id(),
    128 -                wait_result,
    129 -                cancel_timer_task,
    130 -                thread.get_state(),
    131 -            );
    132 -        }
    124          if let Some(cancel_impl) = self.cancel_wait_impl {
        ⋮
    129          thread.set_state(super::k_thread::ThreadState::RUNNABLE);
    139 -        if std::env::var_os("RUZU_TRACE_SLEEP_TIMER").is_some() && thread.get_thread_id() == 17 {
    140 -            log::info!(
    141 -                "KThreadQueue::cancel_wait tid={} state_after_set={:?}",
    142 -                thread.get_thread_id(),
    143 -                thread.get_state(),
    144 -            );
    145 -        }
    130          thread.clear_wait_queue();

  └ core/src/hle/kernel/kernel.rs (+0 -27)
     74  fn real_enable_scheduling(cores_needing_scheduling: u64) {
     75 -    if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
     76 -        log::info!(
     77 -            "real_enable_scheduling: enter core_mask=0x{:X} kernel_ptr_null={}",
     78 -            cores_needing_scheduling,
     79 -            KERNEL_PTR.load(Ordering::Acquire).is_null()
     80 -        );
     81 -    }
     75      let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
        ⋮
     86      if current_thread.is_none() {
     94 -        if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
     95 -            log::info!(
     96 -                "real_enable_scheduling: no current_thread cores_mask=0x{:X}",
     97 -                cores_needing_scheduling
     98 -            );
     99 -        }
     87          KScheduler::reschedule_cores(cores_needing_scheduling);
        ⋮
     90      let current_scheduler = kernel.current_scheduler();
    104 -    if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    105 -        let current_tid = get_current_thread_id_fast();
    106 -        let (state, disable_dispatch) =
    107 -            with_current_thread_fast_mut(|t| (t.get_state(), t.get_disable_dispatch_count()))
    108 -                .unwrap_or((super::k_thread::ThreadState::INITIALIZED, -1));
    109 -        log::info!(
    110 -            "real_enable_scheduling: tid={} state={:?} disable_dispatch={} core_mask=0x{:X} has_scheduler={}",
    111 -            current_tid.unwrap_or(0),
    112 -            state,
    113 -            disable_dispatch,
    114 -            cores_needing_scheduling,
    115 -            current_scheduler.is_some()
    116 -        );
    117 -    }
     91

  └ core/src/hle/kernel/physical_core.rs (+0 -40)
    492      ) {
    493 -        if std::env::var_os("RUZU_TRACE_PREEMPT").is_some() {
    494 -            let tid = unsafe { (&*thread).get_thread_id() };
    495 -            if tid >= 18 {
    496 -                log::info!(
    497 -                    "PhysicalCore::set_running core={} tid={}",
    498 -                    self.m_core_index,
    499 -                    tid
    500 -                );
    501 -            }
    502 -        }
    493          let mut state = self.m_guard.lock().unwrap();
        ⋮
    500      pub fn clear_running(&self) {
    511 -        if std::env::var_os("RUZU_TRACE_PREEMPT").is_some() {
    512 -            let state = self.m_guard.lock().unwrap();
    513 -            if let Some(thread) = state.m_current_thread {
    514 -                let tid = unsafe { (&*thread).get_thread_id() };
    515 -                if tid >= 18 {
    516 -                    log::info!(
    517 -                        "PhysicalCore::clear_running core={} tid={}",
    518 -                        self.m_core_index,
    519 -                        tid
    520 -                    );
    521 -                }
    522 -            }
    523 -            drop(state);
    524 -        }
    501          let mut state = self.m_guard.lock().unwrap();
        ⋮
    516          state.m_is_interrupted = true;
    541 -
    542 -        if std::env::var_os("RUZU_TRACE_PREEMPT").is_some() {
    543 -            use std::sync::atomic::{AtomicU32, Ordering};
    544 -            static INTERRUPT_LOGS: AtomicU32 = AtomicU32::new(0);
    545 -            let tid = current_thread.map(|ptr| unsafe { (&*ptr).get_thread_id() });
    546 -            let n = INTERRUPT_LOGS.fetch_add(1, Ordering::Relaxed);
    547 -            if matches!(tid, Some(t) if t >= 18) && n < 256 {
    548 -                log::info!(
    549 -                    "PhysicalCore::interrupt #{} core={} has_thread={} tid={:?}",
    550 -                    n,
    551 -                    self.m_core_index,
    552 -                    current_thread.is_some(),
    553 -                    tid
    554 -                );
    555 -            }
    556 -        }
    517

  └ core/src/hle/kernel/svc/svc_condition_variable.rs (+0 -41)
     61      let aligned_cv_key = cv_key & !3u64;
     62 -    if std::env::var_os("RUZU_TRACE_CV").is_some() {
     63 -        let thread = current_thread.lock().unwrap();
     64 -        let tid = thread.get_thread_id();
     65 -        if tid >= 18 {
     66 -            log::info!(
     67 -                "svc::WaitProcessWideKeyAtomic enter tid={} address=0x{:X} cv_key=0x{:X} tag=0x{:08X} timeout_ns={} timeout_tick={} state_be
         fore={:?}",
     68 -                tid,
     69 -                address,
     70 -                aligned_cv_key,
     71 -                tag,
     72 -                timeout_ns,
     73 -                timeout,
     74 -                thread.get_state(),
     75 -            );
     76 -        }
     77 -    }
     62      let result = crate::hle::kernel::k_process::KProcess::wait_condition_variable(
        ⋮
     70
     87 -    if std::env::var_os("RUZU_TRACE_CV").is_some() {
     88 -        let tid = current_thread.lock().unwrap().get_thread_id();
     89 -        log::info!(
     90 -            "svc::WaitProcessWideKeyAtomic result tid={} address=0x{:X} cv_key=0x{:X} tag=0x{:08X} timeout_ns={} timeout_tick={} rc=0x{:X}",
     91 -            tid,
     92 -            address,
     93 -            aligned_cv_key,
     94 -            tag,
     95 -            timeout_ns,
     96 -            timeout,
     97 -            result,
     98 -        );
     99 -    }
    100 -
     71      ResultCode::new(result)
        ⋮
     83      let aligned_cv_key = cv_key & !3u64;
    114 -    if std::env::var_os("RUZU_TRACE_CV").is_some() {
    115 -        let tid = system
    116 -            .current_thread()
    117 -            .map(|thread| thread.lock().unwrap().get_thread_id());
    118 -        log::info!(
    119 -            "svc::SignalProcessWideKey tid={:?} cv_key=0x{:X} count={}",
    120 -            tid,
    121 -            aligned_cv_key,
    122 -            count
    123 -        );
    124 -    }
     84      system

  └ core/src/hle/service/nvdrv/devices/nvhost_ctrl.rs (+0 -24)
    362          if self.syncpoint_manager().is_fence_signalled(&params.fence) {
    363 -            if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    364 -                log::info!(
    365 -                    "nvhost_ctrl::IocCtrlEventWait signalled-immediate syncpt_id={} threshold={} min={}",
    366 -                    fence_id,
    367 -                    params.fence.value,
    368 -                    self.syncpoint_manager().read_syncpoint_min_value(fence_id)
    369 -                );
    370 -            }
    363              params.value.raw = self.syncpoint_manager().read_syncpoint_min_value(fence_id);
        ⋮
    368          if self.syncpoint_manager().is_fence_signalled(&params.fence) {
    377 -            if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    378 -                log::info!(
    379 -                    "nvhost_ctrl::IocCtrlEventWait signalled-after-update syncpt_id={} threshold={} min={}",
    380 -                    fence_id,
    381 -                    params.fence.value,
    382 -                    new_value
    383 -                );
    384 -            }
    369              params.value.raw = new_value;
        ⋮
    389              if params.timeout == 0 {
    406 -                if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    407 -                    log::info!(
    408 -                        "nvhost_ctrl::IocCtrlEventWait pending timeout=0 syncpt_id={} threshold={} min={}",
    409 -                        fence_id,
    410 -                        target_value,
    411 -                        self.syncpoint_manager().read_syncpoint_min_value(fence_id)
    412 -                    );
    413 -                }
    390                  if events[slot as usize].fails > 2 {

  └ core/src/hle/service/nvdrv/devices/nvhost_gpu.rs (+0 -17)
    372              .expect("GPU core must remain available while nvhost_gpu is alive");
    373 -        log::info!(
    374 -            "nvhost_gpu::SubmitGPFIFOImpl bind_id={} syncpoint={} fence_in={{id:{}, value:{}}} flags=0x{:X} entries={} prefetch={}",
    375 -            bind_id,
    376 -            channel_syncpoint,
    377 -            params.fence.id,
    378 -            params.fence.value,
    379 -            params.flags,
    380 -            entries.command_lists.len(),
    381 -            entries.prefetch_command_list.len()
    382 -        );
    383 -
    373          if params.fence_wait() {
        ⋮
    402
    414 -        log::info!(
    415 -            "nvhost_gpu::SubmitGPFIFOImpl result fence_out={{id:{}, value:{}}} flags=0x{:X}",
    416 -            params.fence.id,
    417 -            params.fence.value,
    418 -            params.flags
    419 -        );
    403          params.flags = 0;

  └ core/src/hle/service/nvdrv/nvdrv_interface.rs (+1 -9)
    283
    284 -        let result = self.nvdrv.query_event(fd, event_id);
    285 -        log::info!(
    286 -            "NVDRV::QueryEvent fd={} event_id={} -> nv_result={:?} has_event={}",
    287 -            fd,
    288 -            event_id,
    289 -            result.0,
    290 -            result.1.is_some()
    291 -        );
    292 -        result
    284 +        self.nvdrv.query_event(fd, event_id)
    285      }

  └ video_core/src/dma_pusher.rs (+0 -5)
    293      pub fn dispatch_calls(&mut self) {
    294 -        log::info!(
    295 -            "DmaPusher::dispatch_calls queued_lists={}",
    296 -            self.dma_pushbuffer.len()
    297 -        );
    294          self.dma_pushbuffer_subindex = 0;
        ⋮
    298          while self.step() {}
    303 -        log::info!("DmaPusher::dispatch_calls complete");
    299

  └ video_core/src/engines/puller.rs (+0 -14)
    707              FenceOperation::Acquire => {
    708 -                if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    709 -                    log::info!(
    710 -                        "Puller::process_fence_action_method Acquire syncpoint={} fence_value={}",
    711 -                        action.syncpoint_id(),
    712 -                        self.regs.fence_value()
    713 -                    );
    714 -                }
    708                  self.with_rasterizer_mut(|rasterizer| rasterizer.release_fences(false));
        ⋮
    711                  let syncpoint_id = action.syncpoint_id();
    719 -                if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    720 -                    log::info!(
    721 -                        "Puller::process_fence_action_method Increment syncpoint={} fence_value={}",
    722 -                        syncpoint_id,
    723 -                        self.regs.fence_value()
    724 -                    );
    725 -                }
    712                  self.with_rasterizer_mut(|rasterizer| rasterizer.signal_sync_point(syncpoint_id));

  └ video_core/src/host1x/syncpoint_manager.rs (+2 -18)
    214      pub fn increment_guest(&self, syncpoint_id: u32) {
    215 -        if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    216 -            let before = self.syncpoints_guest[syncpoint_id as usize].load(Ordering::Acquire);
    217 -            log::info!(
    218 -                "Host1x::SyncpointManager::increment_guest id={} before={}",
    219 -                syncpoint_id,
    220 -                before
    221 -            );
    222 -        }
    221 +
    222      pub fn increment_host(&self, syncpoint_id: u32) {
    231 -        if std::env::var_os("RUZU_TRACE_SYNCPT").is_some() {
    232 -            let before = self.syncpoints_host[syncpoint_id as usize].load(Ordering::Acquire);
    233 -            log::info!(
    234 -                "Host1x::SyncpointManager::increment_host id={} before={}",
    235 -                syncpoint_id,
    236 -                before
    237 -            );
    238 -        }
    223          self.increment(
• Je refais un balayage ciblé. S’il reste des probes explicites, je retire les dernières avant le commit.

• Explored
  └ Search RUZU_TRACE|INVALID CMD|WriteBuffer[BC] probe|CreateAppletResource called|GetSharedMemoryHandle called|schedule_impl_fiber:|
           multi_core_run_guest_thread:|dispatch_calls complete|before scoped_sleep_lock|real_enable_scheduling:|QueryEvent fd=|SubmitGPFIFOImpl
• Ran git -C /home/vricosti/Dev/emulators/ruzu diff --stat | sed -n '1,220p'
  └  .../src/adsp/apps/audio_renderer/audio_renderer.rs |   21 +-
     .../apps/audio_renderer/command_list_processor.rs  |   57 +-
    … +216 lines
     core/src/hle/service/acc/acc_su.rs                 |  143 +-
     core/src/hle/service/acc/acc_u0.rs                 |  138 +-

Il restait du contexte mélangé entre kernel.rs et k_scheduler.rs. Je corrige ça par petits patches exacts pour éviter de heurter le reste du
  worktree.

• Edited 4 files (+0 -65)
  └ core/src/arm/dynarmic/arm_dynarmic_32.rs (+0 -22)
    1394      fn signal_interrupt(&mut self, _thread: &mut KThread) {
    1395 -        if std::env::var_os("RUZU_TRACE_PREEMPT").is_some() {
    1396 -            use std::sync::atomic::{AtomicU32, Ordering};
    1397 -            static SIGNAL_INTERRUPT_LOGS: AtomicU32 = AtomicU32::new(0);
    1398 -            let thread_tid = unsafe {
    1399 -                (&*(_thread as *mut KThread as *mut crate::hle::kernel::k_thread::KThread))
    1400 -                    .get_thread_id()
    1401 -            };
    1402 -            let n = SIGNAL_INTERRUPT_LOGS.fetch_add(1, Ordering::Relaxed);
    1403 -            if thread_tid >= 18 && n < 256 {
    1404 -                let pc = self
    1405 -                    .jit
    1406 -                    .as_ref()
    1407 -                    .map(|jit| jit.get_register(15))
    1408 -                    .unwrap_or(0);
    1409 -                log::info!(
    1410 -                    "ArmDynarmic32::signal_interrupt #{} tid={} pc=0x{:08X}",
    1411 -                    n,
    1412 -                    thread_tid,
    1413 -                    pc
    1414 -                );
    1415 -            }
    1416 -        }
    1395          if let Some(jit) = self.jit.as_ref() {

  └ core/src/hle/kernel/k_thread.rs (+0 -13)
      52
      53 -fn trace_waitstate_for_thread(thread_id: u64) -> bool {
      54 -    std::env::var_os("RUZU_TRACE_WAITSTATE").is_some() && (17..=19).contains(&thread_id)
      55 -}
      56 -
      53  // ---------------------------------------------------------------------------
         ⋮
    2682      pub fn cancel_wait(&mut self, _wait_result: u32, _cancel_timer_task: bool) {
    2687 -        if trace_waitstate_for_thread(self.thread_id) {
    2688 -            log::info!(
    2689 -                "KThread::cancel_wait tid={} result=0x{:X} cancel_timer={} wait_reason={:?}",
    2690 -                self.thread_id,
    2691 -                _wait_result,
    2692 -                _cancel_timer_task,
    2693 -                self.wait_reason_for_debugging
    2694 -            );
    2695 -        }
    2683          self.sleep_deadline = None;

  └ core/src/hle/kernel/kernel.rs (+0 -8)
    469              Box::new(move |_time, _ns| {
    470 -                if std::env::var_os("RUZU_TRACE_PREEMPT").is_some() {
    471 -                    use std::sync::atomic::{AtomicU32, Ordering};
    472 -                    static PREEMPT_LOGS: AtomicU32 = AtomicU32::new(0);
    473 -                    let n = PREEMPT_LOGS.fetch_add(1, Ordering::Relaxed);
    474 -                    if n < 64 {
    475 -                        log::info!("KernelCore::PreemptionCallback #{}", n);
    476 -                    }
    477 -                }
    470                  let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);

  └ core/src/hle/kernel/svc/svc_lock.rs (+0 -22)
    47
    48 -    if std::env::var_os("RUZU_TRACE_ARB").is_some() {
    49 -        let tid = current_thread.lock().unwrap().get_thread_id();
    50 -        log::info!(
    51 -            "svc::ArbitrateLock result: tid={} handle=0x{:08X} addr=0x{:X} tag=0x{:08X} rc=0x{:X}",
    52 -            tid,
    56 -            result.get_inner_value(),
    57 -        );
    58 -    }
    59 -
    48      result
       ⋮
    71      );
    84 -
    85 -    if std::env::var_os("RUZU_TRACE_ARB").is_some() {
    86 -        let tid = current_thread.lock().unwrap().get_thread_id();
    87 -        log::info!(
    88 -            "svc::ArbitrateUnlock result: tid={} addr=0x{:X} rc=0x{:X}",
    89 -            tid,
    90 -            address,
    91 -            result.get_inner_value(),
    92 -        );
    93 -    }
    72

• Explored
  └ Read k_scheduler.rs

• Edited core/src/hle/kernel/k_scheduler.rs (+0 -88)
    555              let mut t = cur_thread.lock().unwrap();
    556 -            if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    557 -                log::info!(
    558 -                    "KScheduler::reschedule_current_hle_thread tid={} state={:?} disable_dispatch={} dummy={}",
    559 -                    t.get_thread_id(),
    560 -                    t.get_state(),
    561 -                    t.get_disable_dispatch_count(),
    562 -                    t.is_dummy_thread(),
    563 -                );
    564 -            }
    556              debug_assert!(t.get_disable_dispatch_count() == 1);
        ⋮
    642          if scheduler.is_none() || is_phantom_mode {
    652 -            if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    653 -                let (tid, state, disable_dispatch) =
    654 -                    super::kernel::with_current_thread_fast_mut(|t| {
    655 -                        (
    656 -                            t.get_thread_id(),
    657 -                            t.get_state(),
    658 -                            t.get_disable_dispatch_count(),
    659 -                        )
    660 -                    })
    661 -                    .unwrap_or((0, ThreadState::INITIALIZED, -1));
    662 -                log::info!(
    663 -                    "KScheduler::enable_scheduling_with_scheduler hle-fallback tid={} state={:?} disable_dispatch={} has_scheduler={} phanto
         m={}",
    664 -                    tid,
    665 -                    state,
    666 -                    disable_dispatch,
    667 -                    scheduler.is_some(),
    668 -                    is_phantom_mode,
    669 -                );
    670 -            }
    643              // Upstream: KScheduler::RescheduleCores(kernel, cores_needing_scheduling);
        ⋮
    656          if initial_disable_dispatch > 1 {
    685 -            if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    686 -                let (tid, state, disable_dispatch) =
    687 -                    super::kernel::with_current_thread_fast_mut(|t| {
    688 -                        (
    689 -                            t.get_thread_id(),
    690 -                            t.get_state(),
    691 -                            t.get_disable_dispatch_count(),
    692 -                        )
    693 -                    })
    694 -                    .unwrap_or((0, ThreadState::INITIALIZED, -1));
    695 -                log::info!(
    696 -                    "KScheduler::enable_scheduling_with_scheduler nested tid={} state={:?} disable_dispatch={}",
    697 -                    tid,
    698 -                    state,
    699 -                    disable_dispatch
    700 -                );
    701 -            }
    657              super::kernel::with_current_thread_fast_mut(|t| {
        ⋮
    672              if current_state != ThreadState::RUNNABLE {
    718 -                if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    719 -                    let (tid, state, disable_dispatch) =
    720 -                        super::kernel::with_current_thread_fast_mut(|t| {
    721 -                            (
    722 -                                t.get_thread_id(),
    723 -                                t.get_state(),
    724 -                                t.get_disable_dispatch_count(),
    725 -                            )
    726 -                        })
    727 -                        .unwrap_or((0, ThreadState::INITIALIZED, -1));
    728 -                    log::info!(
    729 -                        "KScheduler::enable_scheduling_with_scheduler forcing reschedule tid={} state={:?} disable_dispatch={}",
    730 -                        tid,
    731 -                        state,
    732 -                        disable_dispatch
    733 -                    );
    734 -                }
    673                  scheduler.lock().unwrap().reschedule_current_core();
        ⋮
    677                  .unwrap_or(ThreadState::INITIALIZED);
    740 -            if std::env::var_os("RUZU_TRACE_ENABLE_SCHED").is_some() {
    741 -                let (tid, state, disable_dispatch) =
    742 -                    super::kernel::with_current_thread_fast_mut(|t| {
    743 -                        (
    744 -                            t.get_thread_id(),
    745 -                            t.get_state(),
    746 -                            t.get_disable_dispatch_count(),
    747 -                        )
    748 -                    })
    749 -                    .unwrap_or((0, ThreadState::INITIALIZED, -1));
    750 -                log::info!(
    751 -                    "KScheduler::enable_scheduling_with_scheduler direct-resched tid={} state={:?} disable_dispatch={}",
    752 -                    tid,
    753 -                    state,
    833 -            if std::env::var_os("RUZU_TRACE_SCHED_UPDATE").is_some() {
    834 -                log::info!(
    835 -                    "KScheduler::update_highest_priority_threads_impl core={} top={:?} prev={:?}",
    836 -                    core_id,
    837 -                    top_thread_id,
    838 -                    schedulers[core_id].state.highest_priority_thread_id,
    839 -                );
    840 -            }
    841 -
    754              // Upstream: check pinned thread for the process.

    • Edited core/src/hle/kernel/svc/svc_ipc.rs (+0 -37)
    104
    105 -    if (session_handler_name == "sm:" && context.get_command() == 1)
    106 -        || (session_handler_name == "lm" && context.get_command() == 0)
    107 -        || (session_handler_name == "nvdrv" && context.get_command() == 1)
    108 -    {
    109 -        if let Some(memory) = system.get_svc_memory() {
    110 -            let m = memory.lock().unwrap();
    111 -            log::info!(
    112 -                "  SendSyncRequest response: service={} cmd={} tls=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    113 -                session_handler_name,
    114 -                context.get_command(),
    115 -                m.read_32(tls_address),
    116 -                m.read_32(tls_address + 4),
    117 -                m.read_32(tls_address + 8),
    118 -                m.read_32(tls_address + 12),
    119 -                m.read_32(tls_address + 16),
    120 -                m.read_32(tls_address + 20),
    121 -                m.read_32(tls_address + 24),
    122 -                m.read_32(tls_address + 28),
    123 -            );
    124 -        } else {
    125 -            let mem = debug_memory.read().unwrap();
    126 -            log::info!(
    127 -                "  SendSyncRequest response: service={} cmd={} tls=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
    128 -                session_handler_name,
    129 -                context.get_command(),
    130 -                mem.read_32(tls_address),
    131 -                mem.read_32(tls_address + 4),
    132 -                mem.read_32(tls_address + 8),
    133 -                mem.read_32(tls_address + 12),
    134 -                mem.read_32(tls_address + 16),
    135 -                mem.read_32(tls_address + 20),
    136 -                mem.read_32(tls_address + 24),
    137 -                mem.read_32(tls_address + 28),
    138 -            );
    139 -        }
    140 -    }
    141 -
    105      // Upstream: SendSyncRequest always returns ResultSuccess to the guest.

• Edited core/src/hle/kernel/k_thread.rs (+0 -34)
    1337      pub fn begin_wait_synchronization(&mut self, wait_set: SynchronizationWaitSet, timeout: i64) {
    1338 -        if trace_waitstate_for_thread(self.thread_id) {
    1339 -            log::info!(
    1340 -                "KThread::begin_wait_synchronization tid={} timeout={} active_before={}",
    1341 -                self.thread_id,
    1342 -                timeout,
    1343 -                wait_set.is_active()
    1344 -            );
    1345 -        }
    1338          self.synchronization_wait = wait_set;
         ⋮
    2421          let old_state = self.get_raw_state();
    2430 -        if trace_waitstate_for_thread(self.thread_id) {
    2431 -            log::info!(
    2432 -                "KThread::set_state tid={} old={:?} new_base={:?} wait_reason_before={:?}",
    2433 -                self.thread_id,
    2434 -                old_state,
    2435 -                state,
    2436 -                self.wait_reason_for_debugging
    2437 -            );
    2438 -        }
    2422          self.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::None);
         ⋮
    2595      pub fn begin_wait_with_queue(&mut self, wait_queue: KThreadQueue) {
    2613 -        if trace_waitstate_for_thread(self.thread_id) {
    2614 -            log::info!(
    2615 -                "KThread::begin_wait_with_queue tid={} end_wait_allowed={} has_notify_impl={} has_cancel_impl={}",
    2616 -                self.thread_id,
    2617 -                wait_queue.end_wait_allowed,
    2619 -                wait_queue.cancel_wait_impl.is_some()
    2620 -            );
    2621 -        }
    2596          self.set_state(ThreadState::WAITING);
         ⋮
    2632      pub fn end_wait(&mut self, _wait_result: u32) {
    2659 -        if trace_waitstate_for_thread(self.thread_id) {
    2660 -            log::info!(
    2661 -                "KThread::end_wait tid={} result=0x{:X} wait_reason={:?}",
    2662 -                self.thread_id,
    2663 -                _wait_result,
    2664 -                self.wait_reason_for_debugging
    2665 -            );
    2666 -        }
    2633          self.sleep_deadline = None;
