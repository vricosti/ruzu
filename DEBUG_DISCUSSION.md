Summary of what was found:                                                                                                                                                            
  - All 7 SVC implementations verified correct against upstream (QueryMemory, GetInfo, GetThreadPriority, SignalProcessWideKey, ConnectToNamedPort, SendSyncRequest + all IPC handlers)
  - Root cause found: CoreTiming timer thread deadlocked by lock contention with CPU thread's advance() call in the inner JIT loop — this was a pre-timer-era workaround that conflicted
   with the multicore timer thread                                
  - Fix: Guard advance()/reset_ticks() with if !is_multicore(), matching upstream which never calls CoreTiming from the JIT loop
  - Preemption timer now fires correctly (~150 callbacks/5s = 30Hz, consistent with 10ms interval)
  - Game still loops after 40 SVCs — the next investigation is what the game's ARM code does in its tight loop (not a kernel/timer/IPC bug)

  Here's the status:                                                                                                                               
                                                                                                                                                     
  Fixed in this session:
  1. CoreTiming timer thread was starting paused — fixed by forcing paused=false at thread start                                                     
  2. event.wait_for() was returning immediately due to spuriously-set event flag — fixed by using thread::sleep instead                              
                                                                                                                                                     
  Current blocker (zero SVCs hit):                                                                                                                   
  The game thread enters multi_core_run_guest_thread on core 0 but never reaches run_guest_thread_once. The outer loop's while !is_interrupted() is  
  always true, or reschedule_current_core_raw fiber-switches away and the game thread never gets control back to the JIT loop.                       
                                                                                                                                                     
  The likely root cause is that the game thread (thread_id=9) was dispatched by core 3's switch fiber, but multi_core_run_guest_thread runs          
  reschedule_current_core_raw which uses core 0's scheduler. The fiber switch might be going to core 0's switch fiber which doesn't know about the 
  game thread, creating a dispatch loop where control bounces between fibers without ever entering the JIT.                                          
                                                                                                                                                   
  This is a cross-core fiber dispatch issue that needs further investigation in a separate session.  