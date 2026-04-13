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
                                                                                                                                             