 Gap 1 — scope is bigger than the report suggested.                                                                                         
  
  The Explore report said the retry-next-manager loop is missing from allocate_and_open_continuous. But when I read the Rust code,           
  m_pool_managers_head[pool_index] is declared Option<usize> — it's one manager index, not a linked-list head. Upstream has a linked list  
  (GetFirstManager / GetNextManager) populated at init time. Rust's current structure only supports one manager per pool because that's all  
  the init path populates.                                                                                                                 

  To literally port Gap 1 I need to:
  1. Restructure m_pool_managers_head from Option<usize> to an actual list-of-indices per pool.
  2. Verify/implement whatever code populates multiple managers per pool (may be reading e.g. DRAM layout).                                  
  3. Port TrackUnoptimizedAllocation + the optimized-process bitmap that it touches (separate missing subsystem).
  4. Then the retry loop and the multi-manager Close iteration become meaningful.                                                            
                                                                                                                                             
  Estimated ~300–600 lines across k_memory_manager.rs and init paths, plus a new subsystem (optimized bitmap) that doesn't exist today.      
  Runtime effect is zero if the init path only ever creates one manager per pool, which it currently does.                                   
                                                                                                                                             
  Gap 3 — this is a structural refactor, not an implementation.                                                                              
                                                                                                                                           
  The explore report undersold this. Porting KScopedPageTableUpdater + PageLinkedList + KMemoryBlockManagerUpdateAllocator literally         
  requires:                                                                                                                                
                                                                                                                                             
  1. New PageLinkedList type and KScopedPageTableUpdater RAII guard in k_page_table_base.rs. OK, ~50 lines.                                  
  2. New KMemoryBlockManagerUpdateAllocator type in k_memory_block_manager.rs. Needs a slab manager behind it — Rust has no slab, would need
  KMemoryBlockSlabManager ported or a Vec<Box<KMemoryBlock>> stand-in.                                                                       
  3. Signature change to operate() to thread page_list: &mut PageLinkedList. This function has ~20 direct callers inside k_page_table_base.rs
   — every one needs updating. Some of those callers live inside methods whose own signatures already hide this detail.                      
  4. Signature change to KMemoryBlockManager::update() to take &mut KMemoryBlockManagerUpdateAllocator. Updating all call sites cascades.  
  5. Rewrite map_pages / unmap_pages / a few more to construct the RAII triple (allocator, updater) in the right order matching upstream.    
                                                                                                                                             
  Upstream FinalizeUpdate is itself a TODO (k_page_table_base.cpp:5787: "TODO: Free page entries once allocated in guest memory"). The Rust  
  comment I saw earlier says explicitly "we don't use PageLinkedList (no guest page table entries)". So what we'd be doing is porting 50     
  lines of RAII scaffolding to thread a never-populated list through ~20 call sites — pure ceremony with zero runtime effect.                
                                                                                                                                           
  Estimated 600–1200 lines of mechanical signature threading across k_page_table_base.rs and k_memory_block_manager.rs. Compiles differently,
   behaves identically.
   ---
   ● The investigation has narrowed the root cause to two distinct bugs:                                                                        
                                                                                                                                             
  Bug 1 (confirmed): CoreTiming preemption event stops firing after ~2 seconds. The timer thread keeps ticking but the preemption callback   
  stops being called. This causes signal_interrupt to stop, which causes the JIT to never exit run(), which causes the game to freeze. The   
  watchdog thread workaround proves the JIT is otherwise healthy — with forced 5ms halts, the game makes forward progress through different  
  code regions.                                                                                                                              
                                                                                                                                             
  Bug 2 (to investigate): what causes the preemption event to stop being collected. The event queue has 8 events with times ~50μs overdue,   
  but the preemption callback stops firing. A likely candidate is event starvation or an Arc/Weak reference issue specific to how the
  preemption event is stored.                                                                                                                
                                                                  
  This is deep kernel/scheduler infrastructure debugging that explains the original "game appears stuck" symptom completely. The game isn't  
  slow — it's FROZEN because the preemption timer breaks after 2 seconds of operation.
  -----
  ● The condvar implementation is correct — I verified signals DO match waits when the same key is used. The game's 7 worker threads           
  (tid=84-90) wait on keys in the 0x686A range, but those keys are NEVER signaled because the main thread stalls before reaching its       
  work-dispatch code.                                                                                                                        
                                                                                                                                           
  Each fix advances the main thread further through its init sequence:                                                                       
  - Fix 1 (CheckHalt): 0 → 3254 SVCs                                                                                                       
  - Fix 2+3 (Vsync+ThreadEvent): same but unblocked scheduler
  - Fix 4 (Audio): 3254 → 3606 SVCs (+352 new)               
                                                                                                                                             
  But the game still stalls at 3606 in a user-space spin waiting for another service event. This is the Nth event in a chain — we've fixed   
  vsync and audio, but there are likely more (nvnflinger buffer ready, time service, applet state transitions, etc.).                        
                                                                                                                                             
  Rather than chasing events one by one, I should commit the 4 fixes and update the investigation notes. These are real bugs with real fixes:
                                                                                                                                           
  1. JIT CheckHalt (rdynarmic) — PopRSBHint/FastDispatchHint didn't check halt_reason                                                        
  2. ThreadEvent ordering (common) — SeqCst + always notify                                                                                
  3. VsyncThread (conductor) — sleep-based workaround for condvar lost-wakeup                                                                
  4. Audio SystemManager (system_manager) — non-blocking ADSP signal 
  ------
  ● Zuyu vs Ruzu — JIT cost attribution 
                                                                                                                                             
  Zuyu profile (30s, 56.3B samples — 5.7× ruzu's 10B because zuyu keeps all cores busy):                                                     
  
  ┌───────────────────────────────────────────────────┬──────────────┬────────┬────────────────────────┐                                     
  │                      Bucket                       │ Zuyu samples │ Zuyu % │ Ruzu % (after madvise) │
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤                                     
  │ All JIT (frontend+backend+IR)                     │ 28.2B        │ 50%    │ 39%                    │
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤
  │ Reg allocator                                     │ 4.13B        │ 7.3%   │ 27%                    │                                     
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤
  │ Emitter (EmitX64 / emit_block)                    │ 9.77B        │ 17.4%  │ 19%                    │                                     
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤                                     
  │ Xbyak / rxbyak                                    │ 1.85B        │ 3.3%   │ 8%                     │
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤                                     
  │ Kernel (scheduler)                                │ 12.5B        │ 22%    │ 0.5%                   │
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤                                     
  │ Services (HLE)                                    │ 4.93B        │ 9%     │ 1.4%                   │
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤                                     
  │ SVC handlers                                      │ 3.58B        │ 6%     │ <0.1%                  │
  ├───────────────────────────────────────────────────┼──────────────┼────────┼────────────────────────┤                                     
  │ ReleaseAll / HostLocInfo / EmitBlock named frames │ 0            │ 0%     │ 18%                    │
  └───────────────────────────────────────────────────┴──────────────┴────────┴────────────────────────┘                                     
                                                                  
  What this tells us                                                                                                                         
                                                                  
  1. The attribution hypothesis was partially right: in zuyu, ReleaseAll, HostLocInfo, EmitBlock symbols have zero samples — totally inlined 
  away. Their cost rolls into EmitX64 / RegAlloc parents.         
  2. But the comparison is still bad for ruzu. Normalizing within "all JIT": zuyu spends 4.13 / 28.2 = 14.6% of its JIT time in reg_alloc;   
  ruzu spends 2.41 / 3.48 = 69% of its JIT time in reg_alloc. That's ~4.7× more reg_alloc share per JIT cycle. Not attribution.              
  3. rxbyak is similarly heavier: 8% of ruzu CPU vs 3.3% of zuyu CPU — ~2.5× more share. Within JIT it's 20% vs 6.6% — 3× heavier share.
  4. Ruzu barely touches kernel/services (0.5% + 1.4% vs 22% + 9%). Ruzu is CPU-bound in JIT; zuyu is balanced across JIT/kernel/services    
  because the game is actually running in zuyu and the scheduler/HLE paths get exercised.                                                    
                                                                                                                                             
  Real findings                                                                                                                              
                                                                  
  - reg_alloc is ~4.7× heavier share in ruzu — genuine perf regression vs upstream, not attribution.                                         
  - rxbyak is ~3× heavier share — same.                           
                                                                                                                                             
  These are concrete, actionable gaps. The next investigation is why: likely candidates are data-structure choices (Vec-per-location vs      
  upstream's different storage), or a missing fast path in the allocator's per-instruction loop. 

  --
  ● Actually, the bigger story is thread distribution

  Looking at per-thread sample totals reframes everything:

  ┌──────────────────────────────────────┬──────────────┬──────────────┐
  │                Thread                │ Zuyu samples │ Ruzu samples │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ CPUCore_0                            │ 14.7B (26%)  │ 6.14B (70%)  │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ CPUCore_1                            │ 7.2B (13%)   │ 0.20B (2%)   │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ CPUCore_2                            │ 4.0B (7%)    │ 0.05B (0.5%) │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ CPUCore_3                            │ <1B          │ 0.15B (2%)   │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ Σ CPU cores                          │ ~26B (46%)   │ 6.54B (74%)  │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ Logger                               │ 8.5B (15%)   │ —            │
  ├──────────────────────────────────────┼──────────────┼──────────────┤
  │ GPU + VkPipelineBuild + VulkanWorker │ 12.2B (22%)  │ <1B          │
  └──────────────────────────────────────┴──────────────┴──────────────┘

  - Zuyu distributes guest emulation across 4 host threads evenly-ish.
  - Ruzu runs ~94% of its CPU-core work on CPUCore_0; the other three cores are essentially idle.

  What this means for reg_alloc

  Absolute samples spent in release_all / EndOfAllocScope:
  - Ruzu release_all: 1.44B
  - Zuyu EndOfAllocScope: 1.53B

  Nearly identical absolute work. But zuyu spreads its ~4.1B reg_alloc work across 4 cores (~1B/core), while ruzu concentrates its 2.4B on
  one core. Per-core reg_alloc load: ruzu 2.4B vs zuyu 1.0B — ruzu's single core is doing 2.3× the reg_alloc work of any zuyu core.

  The real root cause

  Ruzu isn't running guest CPUs in parallel. It's serializing four guest CPU cores onto one host thread. This matches the earlier memory note
   about "worker thread distribution as root cause." reg_alloc isn't the bug — single-threaded guest-CPU dispatch is the bug, and it makes
  reg_alloc LOOK like the bottleneck only because that one overloaded core spends most of its time there.

  Fixing thread distribution would unlock ~4× throughput immediately, at which point the reg_alloc and rxbyak shares should drop to match
  zuyu's per-core numbers automatically.

  --
  ● Fix applied — measured impact                                                                                                              
                                                                                                                                             
  Change: In cpu_manager.rs, hoisted the process-lock + JIT-pointer lookup out of run_guest_thread_once and into the caller. JIT pointers for
   all 4 cores are now cached once per guest-thread execution (matching upstream's lock-free process->GetArmInterface(core)).
                                                                                                                                             
  Per-core sample distribution                                    

  ┌─────────────────────┬─────────────┬─────────────┬───────────────┐                                                                        
  │       Thread        │   Before    │    After    │       Δ       │
  ├─────────────────────┼─────────────┼─────────────┼───────────────┤                                                                        
  │ CPUCore_0           │ 6.14B (70%) │ 3.07B (56%) │ -50% absolute │
  ├─────────────────────┼─────────────┼─────────────┼───────────────┤
  │ CPUCore_1           │ 0.20B (2%)  │ 0.77B (14%) │ +3.8×         │                                                                        
  ├─────────────────────┼─────────────┼─────────────┼───────────────┤
  │ CPUCore_2           │ 0.05B       │ 0.09B       │ +80%          │                                                                        
  ├─────────────────────┼─────────────┼─────────────┼───────────────┤                                                                        
  │ CPUCore_3           │ 0.15B       │ 0.08B       │ noise         │
  ├─────────────────────┼─────────────┼─────────────┼───────────────┤                                                                        
  │ Total CPU-core work │ 6.55B       │ 4.02B       │ -39%          │
  └─────────────────────┴─────────────┴─────────────┴───────────────┘                                                                        
  
  Three runs after fix: 3997, 3810, 1798 SVCs / 30s (baseline runs showed 1646–7550 with similar variance — the throughput number is noisy,  
  but no crashes).                                                
                                                                                                                                             
  Honest interpretation                                           

  - CPUCore_1 went from dead-starved to doing real work — the process Mutex really was serializing the 4 host threads on every inner-loop    
  iteration. That part is fixed.
  - But CPUCore_2/3 remain nearly idle. Only 2 of 4 cores are actively doing JIT work. Zuyu saturates all 4 roughly evenly.                  
  - Total core samples dropped from 6.55B → 4.02B — probably means the previously-contended threads were burning CPU on futex wait/wake      
  rather than blocking cleanly. Not a regression, but not the 4× boost I hoped for either.                                                   
                                                                                                                                             
  What's still blocking parallelism                                                                                                          
                                                                  
  One process Mutex is no longer the choke. The remaining serialization is elsewhere — likely the scheduler lock, or how guest threads get   
  assigned to cores (maybe most guest threads are still affinitized to core 0/1). The prior memory about "worker thread distribution" hints
  this is an architectural gap beyond a single lock.

  --
  Summary of this session's wins                                                                                                             
  
  1. zero_block madvise fix (core/src/memory/memory.rs) — eliminated 26% of CPU that was doing real memsets; now uses MADV_REMOVE like       
  upstream.                                                       
  2. Process-Mutex hoist (core/src/cpu_manager.rs) — removed per-iteration Arc<Mutex<KProcess>> lock; CPUCore_1 jumped from 2% → 14% of      
  samples, matching zuyu's core 1 share.                                                                                                     
  
  Known gap — next session pick-up                                                                                                           
                                                                  
  CPUCore_2 in ruzu spends 45% of its time sleeping in Condvar::wait while threads with affinity to core 2 exist but don't reach it. This is 
  a scheduler ready-queue / wake-signal bug in k_priority_queue.rs / k_scheduler.rs — deeper than a localized fix, so I stopped here rather
  than guess. Memory saved with file pointers for the next investigation. 

  ---
  ● Critical correction — I was chasing a false lead         
                                                                                                                                             
  I compared the per-core SVC distributions at equivalent boot phases (first ~1800 SVCs):                                                    
   
  ┌──────┬────────────────────────┬──────────────────────┐                                                                                   
  │ Core │ zuyu (first 1800 SVCs) │ ruzu (all 1863 SVCs) │                                                                                 
  ├──────┼────────────────────────┼──────────────────────┤                                                                                   
  │ 0    │ 1506 (84%)             │ 1538 (83%)           │                                                                                 
  ├──────┼────────────────────────┼──────────────────────┤
  │ 1    │ 285 (16%)              │ 314 (17%)            │
  ├──────┼────────────────────────┼──────────────────────┤                                                                                   
  │ 2    │ 9 (0.5%)               │ 11 (0.6%)            │
  └──────┴────────────────────────┴──────────────────────┘                                                                                   
                                                                                                                                           
  The distributions match almost exactly. At this boot phase MK8D uses core 0 heavily and core 1 lightly; core 2 is barely used in both      
  emulators. My earlier "ruzu's core 2 is 18× quieter than zuyu's" comparison was unfair — I was comparing ruzu at minute 0 of init to zuyu's
   minute-30 steady state where the game is actually running. Zuyu reaches core-2-heavy gameplay; ruzu is stuck in the core-0-dominated init 
  phase.                                                                                                                                   

  So the scheduler isn't broken. Ruzu's per-core distribution is structurally correct. The instrumentation I added confirmed:                
  - 386 PQ pushes in 15s — matches what you'd expect for init
  - Cores 0/1/3 get pushes proportional to init-phase activity; core 2 gets few pushes, but so does zuyu at the same phase.                  
                                                                                                                                           
  What this means for the real bottleneck                                                                                                    
                                                                                                                                           
  The slowdown is not where threads end up running — it's JIT throughput on the one core doing most of the work. At equivalent SVC counts:   
  - Ruzu's JIT consumes ~3.5B samples for ~1863 SVCs → ~1.9M samples/SVC                                                                   
  - Zuyu's JIT consumes ~28B samples for ~552K SVCs → ~50K samples/SVC                                                                       
                                                                                                                                           
  Ruzu spends ~35× more CPU per SVC in JIT code. That's the real gap.
  ----
  ● Critical finding: JIT block cache hit rate is only 32% — 68% of lookups miss and trigger recompilation. 55K compiles in 15s, with only
   42K cache hits out of 133K lookups. That's the dominant source of ruzu's JIT CPU cost.                                               
                                                                                                                                        
  Upstream JITs typically hit >90% in steady state. Let me check one likely suspect: is the BLOCK_LINKING optimization actually enabled?
                                                                                                                                        
  Searched for 4 patterns, read 2 files (ctrl+o to expand)                                                                              
                                                                                                        
● Let me revert instrumentation and save the finding — this deserves a deeper fix in a fresh session:   
--
● Cache is not the bug — data says so                                                                                                   
   
  Two instrumentation passes ruled out cache pathology:                                                                                 
                                                                  
  ┌─────────────────────────────────────────────────┬────────────────────────────────────────────────────────────────────┐              
  │                      Check                      │                               Result                               │
  ├─────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────┤              
  │ Patch retroactive rewriting                     │ Working. 62% of patch calls find pending jumpers and rewrite them. │
  ├─────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────┤
  │ Same-PC keys with different upper bits in cache │ Zero. Every miss is a first-time PC, not flag interference.        │              
  ├─────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────┤              
  │ Sampled miss keys                               │ All clean: cpsr=0 fpscr=0 T=0 E=0 ss=0 it=0x0.                     │              
  ├─────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────┤              
  │ clear_cache / invalidate_range invocations      │ 0 in 15s. Cache is not being wiped.                                │
  └─────────────────────────────────────────────────┴────────────────────────────────────────────────────────────────────┘              
                                                                  
  The 68% miss rate is the game hitting new code paths during init. Normal boot-phase behavior. Zuyu has the same per-SVC cost; it just 
  gets past boot because it has more parallelism and/or faster emitted code, so its miss rate drops to ~0 in steady state.
                                                                                                                                        
  What I've learned across this session about the real ~35× per-SVC gap                                                                 
   
  1. zero_block memset: real + fixed. 26% → 1% of CPU.                                                                                  
  2. Process-Mutex contention: real + fixed. CPUCore_1 went from 2% → 14% of samples.
  3. release_all hot spot: attribution artifact — same absolute work as zuyu.                                                           
  4. Core 2/3 "starvation": false lead — per-phase distribution matches zuyu.                                                           
  5. Cache hit rate "too low": false lead — it's boot-phase code locality, not a bug.                                                   
                                                                                                                                        
  The two actual fixes went to main (6e7785b). The remaining gap is likely per-x64-byte cost of emitted code (ruzu emits more x64 per   
  ARM instruction, or the emitted x64 has more memory traffic, or similar) — that's a week-long dive, not a session.                    
                                                                                                                                        
  Session stopping here without chasing further false leads. Memory updated to reflect what's known, what's ruled out, and what a next  
  session should focus on if you pick this up: compare the actual x64 code emitted by ruzu vs zuyu for a known ARM block, measuring x64
  bytes and analyzing instruction mix. That's the concrete next step. 
  ---
  ● Found concrete evidence. Key observation from the CMP R0,#0 block (70 bytes for 1 ARM instruction):                                   
                                                                                         
  mov eax, [r15+0]       ; load R0 → EAX                                                                                                
  mov [rsp+0x10], eax    ; SPILL EAX to stack  ← waste
  xor eax, eax           ; materialize constant 0 → EAX                                                                                 
  mov ebx, [rsp+0x10]    ; RELOAD R0 → EBX      ← waste                                                                                 
  sub ebx, 0             ; subtract 0 from R0 (just to set flags)
                                                                                                                                        
  The IR Sub(R0, imm0) is being register-allocated with both operands as registers — even though one is a literal 0. That's a bad       
  allocator policy: constants should be folded into immediate forms of x86 instructions. Let me verify this is systematic.              
                                                                                                                                        
  Searched for 15 patterns, read 7 files (ctrl+o to expand)                                                                             
                                                                                                                                        
● I need to be honest: I have a strong local measurement but no baseline from upstream, so I can't yet claim a specific bug to fix.     
                                                                                                                                        
● Diagnostic results — concrete measurements, but no fix yet                                                                            
                                                                                                                                        
  I added a zero-cost-when-off env flag RUZU_X64_DUMP in rdynarmic/src/backend/x64/a32_emit_x64.rs that logs (pc, arm_insns, ir_insts, 
  x64_bytes) per compile, plus RUZU_X64_HEXDUMP for raw bytes.                                                                          
                                                                                                                                        
  20s MK8D boot (RUZU_X64_DUMP=1):                                                                                                      
                                                                                                                                        
  ┌──────────────────────────┬──────────────┐                                                                                           
  │          Metric          │    Value     │                                                                                         
  ├──────────────────────────┼──────────────┤                                                                                           
  │ Blocks compiled          │ 86,860       │                                                                                           
  ├──────────────────────────┼──────────────┤
  │ ARM insns translated     │ 392,221      │                                                                                           
  ├──────────────────────────┼──────────────┤                                                                                         
  │ x64 bytes emitted        │ 13,611,745   │                                                                                           
  ├──────────────────────────┼──────────────┤                                                                                           
  │ Mean bytes per ARM insn  │ 34.7         │                                                                                           
  ├──────────────────────────┼──────────────┤                                                                                           
  │ Mean ARM insns per block │ 4.52         │                                                                                           
  ├──────────────────────────┼──────────────┤                                                                                           
  │ Blocks with arm_insns=1  │ 25,883 (30%) │                                                                                           
  └──────────────────────────┴──────────────┘                                                                                           
                                                                                                                                      
  Inspection of one block (CMP R0,#0, 1 ARM → 70 x64 bytes) showed a spill+reload pair:                                                 
  mov eax, [r15+0]         ; GetReg R0 → EAX                                                                                          
  mov [rsp+0x10], eax      ; ← spill (forced)                                                                                           
  xor eax, eax             ; NZCV scratch = 0 (needs RAX for LAHF)                                                                      
  mov ebx, [rsp+0x10]      ; ← reload into EBX                                                                                          
  sub ebx, 0                                                                                                                            
  But the identical allocator pattern exists in upstream dynarmic — same SelectARegister, same FindFreeSpill → spill-to-stack, same     
  DoNZCV(RAX) pre-allocation. I could not prove rdynarmic does anything structurally different from reg_alloc.cpp. So this may or may   
  not be a real delta.                                                                                                                  
                                                                                                                                        
  Honest assessment                                                                                                                     
                                                                                                                                      
  The measured 34.7 bytes/ARM looks high, but without measuring upstream dynarmic on the same MK8D workload I can't distinguish "this is
   bad" from "all ARM JITs look like this for MK8D". The prior session's conclusion — "this is a week-long dive, not a session" — is
  correct.                                                                                                                              
                                                                                                                                      
  To responsibly proceed, next concrete step is extending a32_oracle.cpp with a DUMP command (using dynarmic's existing                 
  BlockOfCode::GetCodeBegin/End) and running the same ARM workload through both engines. That gives an apples-to-apples delta. Without
  it, any "fix" I pick is a guess.
  ---
  ● Comparison result — per-block emission is NOT the bug                                  
                                                                                                                                        
  Ran 19 representative ARM32 instructions (mov/add/sub/cmp/mul/ldr/str/lsl/orr/bic/b) in single-step mode through BOTH engines via
  a32_diff, with RUZU_X64_DUMP in rdynarmic and the same log I added to upstream dynarmic's a32_emit_x64.cpp.                           
                                                                                         
  ┌──────────────────────┬───────────┬───────────────────┬───────┐                                                                      
  │ Totals over 19 insns │ rdynarmic │ upstream dynarmic │ ratio │                                                                      
  ├──────────────────────┼───────────┼───────────────────┼───────┤                                                                      
  │ x64 bytes emitted    │       735 │               755 │ 0.97× │                                                                      
  ├──────────────────────┼───────────┼───────────────────┼───────┤                                                                      
  │ IR insts             │        80 │                64 │ 1.25× │                                                                      
  └──────────────────────┴───────────┴───────────────────┴───────┘                                                                      
                                                                                                                                        
  Per-instruction:                                                                                                                      
                                                                                                                                        
  PC      rdynarmic    upstream                                                                                                         
  0x00    20           27                                                                                                               
  0x04    21           28
  0x08    35           35                                                                                                               
  0x0c    36           33                                         
  0x10    56           50                                                                                                               
  0x18    30           27
  0x24    37           45                                                                                                               
  0x34    54           61                                         
  0x38    57           66
  0x48    69           73
                                                                                                                                        
  rdynarmic emits slightly less x64 than upstream per ARM instruction on this corpus. So the "34.7 bytes/ARM during MK8D boot" number is
   not pathological — upstream would produce a similar number. This rules out per-block emission quality as the cause of the ~9×        
  per-core throughput gap.                                                                                                              
                                                                  
  What this redirects

  The prior session's hypothesis ("ruzu emits more x64 per ARM instruction, or the emitted x64 has more memory traffic") appears to be  
  wrong. The gap must be in:
                                                                                                                                        
  1. Compile frequency — ruzu compiled 86K blocks in 20s (4.3K/s). If upstream compiles ~5-10× fewer blocks for the same work (e.g.     
  because block-linking actually reaches steady-state there but not here, or invalidations differ), compile time itself dominates.
  2. Inter-block dispatch cost — each block transition in ruzu may take more cycles than upstream (fast-dispatch table hits, hash       
  lookups).                                                                                                                             
  3. Per-run() setup/teardown — if ruzu enters/exits run() far more often than upstream per guest instruction executed.
                                                                                                                                        
  None of the three are testable from the current data. The cheapest next step is measure compile frequency in upstream on same MK8D    
  workload — that requires running MK8D through dynarmic directly (not via oracle step mode), which means hooking into zuyu's actual JIT
   backend during a game boot and logging the same X64DUMP line there. That's achievable (zuyu uses the same dynarmic lib I just        
  patched) — just need to run zuyu with RUZU_X64_DUMP=1 set.
  ---
  ● Definitive comparison — JIT emission and throughput are NOT the bug
                                                                                                                                        
  Ran MK8D boot through both engines for 20s wall-clock with RUZU_X64_DUMP=1:                                                           
  
  ┌──────────────────────┬────────────┬────────────┬───────────────────────────────┐                                                    
  │        Metric        │    ruzu    │    zuyu    │         who's "worse"         │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ Blocks compiled      │     86,860 │    102,887 │ zuyu compiled 18% more        │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ ARM insns translated │    392,221 │    453,718 │ zuyu +16%                     │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ x64 bytes emitted    │    13.6 MB │    18.9 MB │ zuyu +38%                     │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ Mean ARM per block   │       4.52 │       4.41 │ same                          │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ Mean x64 per ARM     │       34.7 │       41.6 │ ruzu is 17% more compact      │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ Mean IR per ARM      │       3.90 │       2.59 │ ruzu's IR is 50% more verbose │
  ├──────────────────────┼────────────┼────────────┼───────────────────────────────┤                                                    
  │ ARM-count histogram  │ 30% 1-insn │ 30% 1-insn │ identical shape               │
  └──────────────────────┴────────────┴────────────┴───────────────────────────────┘                                                    
                                                                  
  This kills three hypotheses at once:                                                                                                  
                                                                  
  1. "ruzu emits more x64 per ARM" → wrong. ruzu is more compact (34.7 vs 41.6).                                                        
  2. "ruzu has too many tiny blocks" → wrong. Identical block-size distribution.
  3. "ruzu JIT throughput is too low" → wrong. zuyu compiles more blocks per wall-second.                                               
                                                                                                                                        
  So where IS the 35× per-SVC gap?                                                                                                      
                                                                                                                                        
  zuyu spent the same 20s compiling more code AND reached gameplay, while ruzu compiled slightly less and stayed stuck in init. This    
  means compiled blocks execute far less useful guest work per wall-second in ruzu. The gap is outside the compile path.
                                                                                                                                        
  Concrete next suspects (in decreasing likelihood, based on prior TODO findings):

  1. run() enters/exits too often — each entry into the JIT pays setup cost (register load, dispatcher, halt check). If ruzu exits to   
  the kernel every few hundred cycles while zuyu runs millions of cycles per entry, that's a 100–1000× factor. The prior TODO notes
  "CheckHalt" fixes and "preemption event stops firing" suggest this path is fragile. First cheap test: instrument run() entry/exit rate
   and compare.                                                   
  2. Host-memory pressure on guest memory accesses — fastmem misses falling back to the slow callback path. Every callback is a C-call
  through a trampoline. Measurable with perf annotate on a32_memory_read_*_trampoline.                                                  
  3. Kernel/scheduler slow path — SVC handlers taking much longer in ruzu than zuyu (the prior work on Process-Mutex was one example).
                                                                                                                                        
  Files touched this session                                                                                                            
                                                                                                                                        
  - rdynarmic/src/backend/x64/a32_emit_x64.rs — env-guarded X64DUMP logging (+30 lines)                                                 
  - zuyu/externals/dynarmic/src/dynarmic/backend/x64/a32_emit_x64.cpp — same, env-guarded (+11 lines)
  - Rebuilt libdynarmic.a, a32_oracle, yuzu-cmd                                                                                         
                                                                                                                                        
  All instrumentation is off unless RUZU_X64_DUMP=1 is set. No behavior change otherwise.                                               
                                                                                                                                        
  No fix applied — the diagnostic has corrected the hypothesis rather than confirmed it. Next session should instrument run() entry/exit
   frequency on both engines (cheap, decisive) before touching any code.

   --
   ● DEFINITIVE FINDING — the JIT isn't the bug, the guest is stuck
                                                     
  20 seconds of MK8D boot with RUZU_RUN_TRACE=1 on both engines:                                                                        
                                                                                                                                        
  ┌─────────────────────────────┬────────────────┬───────────────────┬───────────────────────┐                                          
  │           Metric            │      ruzu      │       zuyu        │         ratio         │                                          
  ├─────────────────────────────┼────────────────┼───────────────────┼───────────────────────┤                                          
  │ Run() entries               │         ~5,000 │          ~113,000 │            zuyu ×22.6 │
  ├─────────────────────────────┼────────────────┼───────────────────┼───────────────────────┤
  │ Mean cycles / entry         │      2,851,503 │           243,431 │            ruzu ×11.7 │                                          
  ├─────────────────────────────┼────────────────┼───────────────────┼───────────────────────┤                                          
  │ Total cycles in Run()       │        ~14.3 B │           ~27.5 B │ ≈ same (both ran 20s) │                                          
  ├─────────────────────────────┼────────────────┼───────────────────┼───────────────────────┤                                          
  │ SVCs delivered (halt_bit)   │          4,180 │           107,896 │            zuyu ×25.8 │
  ├─────────────────────────────┼────────────────┼───────────────────┼───────────────────────┤                                          
  │ External halts / BreakLoops │ 820 (ext_halt) │ 4,112 (timeslice) │                     - │
  └─────────────────────────────┴────────────────┴───────────────────┴───────────────────────┘                                          
                                                                  
  What this rewrites                                                                                                                    
                                                                  
  Both engines burn roughly the same total CPU cycles. But zuyu exits Run() every ~243k cycles, handles an SVC, re-enters → ~108k SVCs  
  in 20s. Ruzu stays inside Run() for ~2.85M cycles per entry and only delivers 4,180 SVCs total.
                                                                                                                                        
  4,180 SVCs in 20s matches exactly the prior TODO note: "the game stalls at ~3254–3606 SVCs". Ruzu isn't compiling slower or emitting  
  worse x64 — the guest has been spinning in a user-space poll loop for most of those 20 seconds, waiting for a service event that never
   arrives.                                                                                                                             
                                                                  
  This was already noted in the prior session ("stalls at 3606 in a user-space spin waiting for another service event"). The subsequent 
  session mis-attributed the resulting low SVC throughput to "per-SVC JIT cost ~35× zuyu" — but the per-entry cycle numbers show the JIT
   is compiling as much code per second as zuyu does, and emitting slightly less x64 per ARM instruction. There is no 35× gap. There's a
   stuck service event.                                           

  What the "fix" is                                                                                                                     
  
  The problem lives in HLE/services, not rdynarmic, not the scheduler, not the JIT. Next step is to identify what service event MK8D is 
  polling for and why ruzu isn't delivering it. The prior session's note on the condvar chain and the "Nth event in a chain (nvnflinger
  buffer ready, time service, applet state transitions...)" is the right lead.                                                          
                                                                  
  Instrumentation added this session (all env-guarded, off by default)                                                                  
  
  - rdynarmic/src/backend/x64/a32_emit_x64.rs — RUZU_X64_DUMP (+30 lines)                                                               
  - rdynarmic/src/jit.rs — RUZU_RUN_TRACE (+50 lines)             
  - zuyu/externals/dynarmic/src/dynarmic/backend/x64/a32_emit_x64.cpp — RUZU_X64_DUMP (+11 lines)                                       
  - zuyu/externals/dynarmic/src/dynarmic/backend/x64/a32_interface.cpp — RUZU_RUN_TRACE (+45 lines)                                     
                                                                                                                                        
  I'd recommend stopping this investigation here. The session's actual result is a correction: the JIT is not the bottleneck, and the   
  prior "35× per-SVC JIT cost" framing was wrong. The real work is in HLE event delivery, which is a fresh investigation with a         
  different toolchain (SVC tracing, event lifecycle logging). 

  --
  Proposed next step (before I spend large context on traces):                                                                          
  
  The efficient path is a side-by-side IPC response diff between ruzu and zuyu from process start up to the first nvdrv::Open.          
  Concretely:                                                                                                                         
                                                                                                                                        
  1. Add one-shot hex-dump logging of every IPC reply payload (cmd_id + raw buffer) in the HLE dispatcher on both sides, gated by a     
  first-N-calls counter so output stays bounded.
  2. Run 5-second trace on each (stall happens at ~3k SVCs well before 15s).                                                            
  3. Diff the two payload streams; first byte-level mismatch is the culprit.                                                            
                                                                                                                                        
  This is more decisive than keep-guessing per-service. Two questions before I start:                                                   
                                                                                                                                        
  - Scope: OK to add temporary hex-dump logging at the HLE IPC dispatcher (both ruzu and upstream zuyu, like the nvmap logging already  
  in place)?                                                                                                                          
  - Alternative: want me to instead audit the most likely single suspect first — set:sys::GetFirmwareVersion / appletOE launch-parameter
   response — which is the cheapest guess and a known historical divergence source?
   --
   ● Investigation result                                                                                                                  
  
  Added bounded IPC reply hex-dumps to both engines and ran 8s boots. ~3100 replies each, totally different service mix.                
                                                                                                                                      
  Hard finding: ruzu's guest never dispatches Mii service. zuyu makes 4 Mii calls (Initialize/Get/UpdateLatest) at ~2s; ruzu never does.
   That's a branch divergence inside the game's nnSdk.                                                                                
                                                                                                                                        
  Ruzu's busy-wait fingerprint (top dispatches in 8s): 794× nvdrv::Ioctl1, 331× IStorage::Read, 32× sm:::GetService (retry), 26×        
  IAudioRenderer::RequestUpdate. The game spins instead of progressing.
                                                                                                                                        
  Candidate divergent replies (present in both but with differing payload bytes):                                                       
  1. After ACC::GetNintendoAccountUserResourceCacheForApplication — words=14 reply: zuyu ca25d374 262f00c2, ruzu 44f0afae 72e0a342.
  2. words=20 FW-version-shaped reply: zuyu 0f0407ea 001d0500 00000003 00000068, ruzu 0a0a008f 00103801 00000004 0000011a.              
  3. words=26 128-bit UUID-like reply (4× each): zuyu 301c0e64 17d8db39 fd1cee88 18fbedfd, ruzu a60f9d15 8bf3a067 f17f995d fc898192.  
                                                                                                                                        
  What I can't do yet with current dumps: attribute each IPC reply to a specific (service, cmd). Replies align by order of arrival,     
  which differs between engines.                                                                                                        
                                                                                                                                        
  Proposed next step: extend the dumps on both sides to include service_name + cmd_id, re-run, then diff by (service, cmd) tuple. First 
  tuple whose payload bytes differ is the culprit. Want me to proceed, or pick one suspect (ACC / set:sys::GetFirmwareVersion / UUID  
  query) to investigate directly first?                                                                                                 
                                                                                                                                      
  Memory saved at project_mk8d_ipc_diff.md. Logging on both sides is uncommitted.    
  ---
  ● Root cause found                                                                                                                      
                                                                                                                                        
  After adding (service, cmd) attribution to both dumps and diffing, I found the first real payload divergence:                         
                                                                  
  ISystemClock::GetCurrentTime (cmd 0) returns error 0x0000CE74 in ruzu — zuyu returns Success + posix time.                            
                                                                  
  0xCE74 decodes exactly to Time-103 = RESULT_CLOCK_UNINITIALIZED. MK8D's nnSdk treats this error as fatal and takes the alternate init 
  path we've been chasing: no Mii, no pre-flight NvOsGetConfigU32, no 26MB nvmap, stuck user-space spin.
                                                                                                                                        
  Why ruzu returns it: core/src/hle/service/psc/time/system_clock.rs:127 reads self.state.lock().unwrap().initialized — a snapshot taken
   in psc/time/static.rs:196-225::get_standard_user_system_clock via SystemClock::with_state(..., initialized, ...). That snapshot is
  captured once at GetStandardUserSystemClock time and never re-read.                                                                   
                                                                  
  Upstream divergence: zuyu/src/core/hle/service/psc/time/system_clock.cpp:28-37 holds m_clock_core as a reference and reads            
  IsInitialized() at call time. MK8D's call order vs ruzu's snapshot construction order results in ruzu snapshotting false.
                                                                                                                                        
  Additional bug in the same spot: ruzu reads standard_local_system_clock.clock.is_initialized() when building the user clock wrapper — 
  upstream passes m_user_system_clock so the wrapper checks that clock's state. Wrong clock polled.
                                                                                                                                        
  Proposed fix: port upstream's reference-based design. Store Arc<Mutex<SystemClockCore>> on SystemClock, SteadyClock, TimeZoneService; 
  read init state at call time. Moderate refactor — ~4 static.rs call sites plus 3 wrapper structs.
  --
  ● Summary

  Two bugs fixed, traces saved to traces/:

  1. SystemClock snapshot bug (psc/time/system_clock.rs + psc/time/static.rs): ported upstream's reference-based design. Added
  SystemClockBackend trait + TimeManagerBackend adapter; the 4 static.rs call-sites now use with_backend. IPC methods read live state
  each call.
  2. time:m factory minting fresh TimeManager per session (services.rs:543): factory was || Arc::new(TimeServiceManager::new(...)) →
  every IPC session got a brand-new TimeManager, so setup_*_clock_core from Glue modified one instance while time:u backend referenced
  another. Fix: construct once, clone the Arc.

  Verified: ISystemClock cmd=0 now returns words=14, result=0x0, payload=69dec050 00000000 (valid posix time), matching zuyu's shape.
  Ruzu IPC replies dropped 3975→2864 (less spinning). Only 2 payload divergences remain — both per-device identifiers (ACC hash, account
   UUID) likely expected.

  Traces in /home/vricosti/Dev/emulators/ruzu/traces/:
  - ruzu_ipc_reply.log (916KB)
  - zuyu_ipc_reply.log (7.4MB)

  Still stuck: game doesn't reach Mii or the 442× NvOsGetConfigU32 pre-flight — a later blocker exists, needs a next pass. Memory
  updated at project_mk8d_ipc_diff.md.
  ---
  ● Progress checkpoint. Let me present what I see and ask where to focus.                                                                
  
  Current state after the clock fix:                                                                                                    
  - ISystemClock bug: fixed and verified (game now gets real posix time)
  - Game still doesn't reach Mii or do the 442× NvOsGetConfigU32 pre-flight                                                             
  - Only 2 remaining payload divergences: per-device UUIDs (expected to differ per-instance)
                                                                                                                                        
  What I can see in ruzu's dispatch stream:                                                                                             
  - 822 nvdrv cmd=1 (Ioctl1) in 8s — hammering                                                                                          
  - 342 IStorage cmd=0 reads — polling some storage                                                                                     
  - 21 pl:u cmd=3 calls — polling shared fonts                                                                                          
                                                                                                                                        
  Services/cmds zuyu reaches but ruzu never does (possibly dead code paths):
  - mii:e cmd=0,3 at zuyu seq=662-663 (the divergence marker)                                                                           
  - IDatabaseService cmd=5 at zuyu seq=712 (ruzu only dispatches cmd=3 of this iface)                                                   
  - IApplicationFunctions cmd=40 at seq=1636                                                                                            
  - IFile cmd=1,2,3 at seq=1612+ (Read/Write/SetSize)                                                                                   
  - IFileSystem cmd=7,10 at seq=1601+                                                                                                   
  - hid cmd=66,203 at seq=1554+                                                                                                         
  - prepo:a cmd=10101 at seq=1625                                                                                                       
                                                                                                                                        
  Three directions I could take — pick one:                                                                                             
                                                                                                                                        
  A. Look for what's BEFORE zuyu's first Mii call (seq=662). Find the IPC reply in range seq=440-660 in zuyu that ruzu's game never     
  "decides" to follow up on. Needs deeper per-reply timing correlation.                                                                 
                                                                                                                                        
  B. Investigate ruzu's 822 nvdrv::Ioctl loop. What specific ioctl codes is the game hammering? Is it a polling loop whose wait         
  condition is broken? This is the "effect" not cause, but may reveal what data the game expects.
                                                                                                                                        
  C. Commit the clock fix first (it's a real, verified correctness bug worth preserving), then tackle the next blocker. Memory note says
   "Ask before commit/push" — which is why I'm asking.
   ---
   ● Result of option 1 (IStorage B-buffer diff)                                                                                           
   
  Ruled out as the cause. Added RUZU_ISTORAGE_READ_DUMP / ZUYU_ISTORAGE_READ_DUMP hex-dumps to IStorageAccessor::Read on both engines.  
  Only one Read happens on each side during the whole boot: the 136-byte LaunchParameterAccountPreselectedUser struct at offset 0.    
  Content is byte-identical in structure — magic CA 97 94 C7, is_account_selected=1, all-zero padding. Only the 16-byte UUID differs    
  (random per-run, expected).                                     
                                                                                                                                        
  Additional finding: ran ruzu for 60s. Total HLE dispatches: 1962 (no growth from the 8s count). The game is genuinely stuck in a tight
   loop cycling only nvdrv, IStorage, IAudioRenderer — not merely slow. Audio update rate fell to 1.5/sec. Zero Mii dispatches ever.
                                                                                                                                        
  Conclusion: the divergence driving the missing sm::GetService("mii:e") is not IPC-payload-observable. It lives in one of:             
  - Shared memory region content (HID / time / account) the game reads directly
  - A kernel event that zuyu signals from some service path that ruzu doesn't                                                           
  - SVC-level thread synchronization (condvar/mutex never signaled)          
                                                                                                                                        
  Next step (option 3): SVC trace of main thread. Prior sessions identified it sitting in a 5ms SleepThread polling loop at             
  PC=0x01D5034C, r2=0x81FF (own thread handle). The concrete action is: log WaitForAddress / ArbitrateLock / condvar SVCs on the main   
  thread after the nvdrv burst, and identify what memory address it's polling and who should signal it. 
                                                                                                                                             