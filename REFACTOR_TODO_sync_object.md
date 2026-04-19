● Full architectural refactor: KProcess/KThread → raw access under scheduler spin-lock                                                                          
                                               
  Goal                                                                                                                                                          
                                                                                                                                                                
  Match upstream's synchronization model exactly:                                                                                                               
  - One serialization primitive: KScopedSchedulerLock (reentrant spin-lock).                                                                                    
  - KProcess and KThread accessed as raw references/pointers, not through sleeping mutexes.                                                                     
  - Held-across-yield becomes structurally impossible: the spin-lock forbids sleeping, so no fiber can yield while holding it.                                  
                                                                                                                                                                
  New types                                                                                                                                                     
                                                                                                                                                                
  Replace the current wrapping:                                                                                                                                 
                                                                  
  // OLD                                                                                                                                                        
  pub type ProcessLock = TrackedMutex<KProcess>;             // sleeping
  type ThreadArc = Arc<std::sync::Mutex<KThread>>;           // sleeping                                                                                        
                                                                                                                                                                
  // NEW                                                                                                                                                        
  pub struct KProcessCell {                                                                                                                                     
      inner: UnsafeCell<KProcess>,                                
  }                                                                                                                                                             
   
  pub struct KThreadCell {                                                                                                                                      
      inner: UnsafeCell<KThread>,                                 
  }

  // SAFETY: access always gated on scheduler-lock-held assertion.                                                                                              
  unsafe impl Sync for KProcessCell {}
  unsafe impl Sync for KThreadCell {}                                                                                                                           
                                                                  
  Access API — the only way to get &mut T:                                                                                                                      
                                                                  
  impl KProcessCell {                                                                                                                                           
      /// SAFETY: caller holds the scheduler spin-lock.           
      pub unsafe fn get_mut_unchecked(&self) -> &mut KProcess {                                                                                                 
          &mut *self.inner.get()                                                                                                                                
      }                                                                                                                                                         
                                                                                                                                                                
      /// Safe wrapper. `guard` is evidence the scheduler lock is held.                                                                                         
      pub fn with(&self, _guard: &KScopedSchedulerLock, f: impl FnOnce(&mut KProcess)) {
          // SAFETY: guard's existence proves scheduler lock is held, which                                                                                     
          // serializes all access across cores.                                                                                                                
          unsafe { f(self.get_mut_unchecked()) }                                                                                                                
      }                                                                                                                                                         
  }                                                                                                                                                             
                                                                  
  Call sites look like:                                                                                                                                         
   
  // OLD                                                                                                                                                        
  let guard = process.lock().unwrap();                            
  guard.do_thing();                                                                                                                                             
   
  // NEW (scheduler lock acquired at entry to the SVC path)                                                                                                     
  let sl = KScopedSchedulerLock::new(&kernel.scheduler_lock);     
  process.with(&sl, |p| p.do_thing());                                                                                                                          
                                                                                                                                                                
  Scheduler-lock reentrancy requirement                                                                                                                         
                                                                                                                                                                
  Upstream's KScopedSchedulerLock is reentrant with a depth counter. Ruzu's KAbstractSchedulerLock needs to either be (or become) reentrant for this to work —  
  many call paths acquire it transitively. First task of the refactor is auditing / confirming reentrancy of the existing spin-lock impl.
                                                                                                                                                                
  Access-site classification (the hard part)                                                                                                                    
   
  ~628 call sites across 76 files. Before migrating, each must be classified:                                                                                   
                                                                  
  ┌────────────────────────────┬────────────────────────────────────────────────────────────────────┬───────────────────────────────────────────────────────┐   
  │           Class            │                          Current pattern                           │                        Action                         │
  ├────────────────────────────┼────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────┤   
  │ A — already under          │ KScopedSchedulerLock sl; process.lock().unwrap().x                 │ Drop .lock(), use with (or raw in release).           │
  │ scheduler lock             │                                                                    │                                                       │
  ├────────────────────────────┼────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────┤   
  │ B — SVC path, missing      │ process.lock().unwrap().x inside SVC body                          │ Add KScopedSchedulerLock wrapping the whole SVC body  │   
  │ scheduler lock             │                                                                    │ (matches upstream).                                   │   
  ├────────────────────────────┼────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────┤   
  │ C — short-lived read-only  │ thread.lock().unwrap().get_thread_id() as a temporary              │ Still needs scheduler lock since other cores may      │
  │                            │                                                                    │ write. Wrap.                                          │   
  ├────────────────────────────┼────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────┤
  │ D — HLE / service-thread   │ Background Rust thread manipulates KThread/KProcess (IPC reply,    │ Must acquire scheduler lock at the boundary. This is  │   
  │ access                     │ event signal, etc.)                                                │ new code most places.                                 │   
  ├────────────────────────────┼────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────┤
  │ E — construction/init      │ Arc::new(TrackedMutex::from_value(KProcess::new())) in kernel      │ Mechanical: KProcessCell::new(KProcess::new()).       │   
  │                            │ bootstrap                                                          │                                                       │   
  ├────────────────────────────┼────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────┤
  │ F — tests                  │ Unit tests construct bare objects, often without a scheduler       │ Add a test-only with_unchecked helper or a dummy      │   
  │                            │                                                                    │ scheduler lock.                                       │   
  └────────────────────────────┴────────────────────────────────────────────────────────────────────┴───────────────────────────────────────────────────────┘
                                                                                                                                                                
  The classification is the expensive part — probably 1–2 sessions of pure reading through 628 sites.                                                           
   
  Phased migration                                                                                                                                              
                                                                  
  Phase 0: Audit & scaffolding (1 session)                                                                                                                      
   
  - Audit every .lock() site for the 6 categories above. Produce a spreadsheet (file, line, category, notes).                                                   
  - Confirm / fix reentrancy on KAbstractSchedulerLock.           
  - Land KProcessCell / KThreadCell type definitions in a new file alongside the existing TrackedMutex, unused.                                                 
  - Commit boundary: pure additive, no behavior change.                                                                                                         
                                                                                                                                                                
  Phase 1: Extend scheduler-lock coverage (2–3 sessions)                                                                                                        
                                                                                                                                                                
  Before removing the Mutex, make sure every category-B and category-D site                                                                                     
  acquires the scheduler lock. This step is UPSTREAM-FAITHFUL and 
  independently useful — it converges the port's synchronization model to                                                                                       
  upstream's, even before the Mutex removal.                                                                                                                    
                                                                                                                                                                
  For each SVC entry path:                                                                                                                                      
  // BEFORE                                                                                                                                                     
  pub fn svc_foo(system: &System, ...) -> ResultCode {            
      let mut process = system.current_process_arc().lock().unwrap();
      // ...                                                                                                                                                    
  }                                                                                                                                                             
                                                                                                                                                                
  // AFTER                                                                                                                                                      
  pub fn svc_foo(system: &System, ...) -> ResultCode {            
      let sl = KScopedSchedulerLock::new(system.scheduler_lock());
      let mut process = system.current_process_arc().lock().unwrap();
      // same body                                                                                                                                              
  }               
                                                                                                                                                                
  MK8D boot progression is the end-to-end validator. The Mutex is still
  there, so existing races still exist — but they should now be rarer                                                                                           
  because scheduler-lock coverage narrows the concurrency.                                                                                                      
                                                                                                                                                                
  Commit boundary: each SVC family can be a separate commit (svc_lock,                                                                                          
  svc_condition_variable, svc_address_arbiter, etc.).                                                                                                           
                                                                                                                                                                
  Phase 2: Migrate HLE / service sites (1–2 sessions)                                                                                                           
   
  HLE background threads (audio renderer, VI conductor, service handlers)                                                                                       
  manipulate KThread/KProcess without holding the scheduler lock. This is
  the most subtle category.                                                                                                                                     
                                                                  
  Each such site needs to:                                                                                                                                      
  1. Acquire scheduler lock at the manipulation point.            
  2. OR change the HLE to post work into the CPU-core fiber queue (upstream                                                                                     
  approach — upstream kernel only runs on CPU cores, HLE services run      
  guest-side).                                                                                                                                                  
                                                                  
  Option 2 is more upstream-faithful but significantly more code. Option 1                                                                                      
  is simpler but adds spin-lock contention from host threads.                                                                                                   
                                                                                                                                                                
  Commit boundary: one subsystem at a time.                                                                                                                     
                                                                  
  Phase 3: Replace Mutex with Cell — ATOMIC COMMIT (1 session)                                                                                                  
                                                                  
  This is the big one. In a single commit:                                                                                                                      
                                                                  
  1. type ProcessLock = TrackedMutex<KProcess> → type ProcessCell = KProcessCell.                                                                               
  2. Arc<Mutex<KThread>> → Arc<KThreadCell>.                      
  3. Every .lock().unwrap() on these types → .with(&sl, |x| ...) or                                                                                             
  unsafe { x.get_mut_unchecked() } (in release hot paths).                                                                                                      
  4. Remove TrackedMutex, ProcessLockTracker machinery (keep access_unlocked concept as the base of KProcessCell::with).                                        
                                                                                                                                                                
  Has to land as one commit because any partial state regresses (as                                                                                             
  Phase 2 pilot proved).                                                                                                                                        
                                                                                                                                                                
  Tooling for the migration:                                                                                                                                    
  - A sed-based pass converts the mechanical changes.
  - Rust compiler catches the cases where .lock() was used for its MutexGuard::deref side-effects but not for actual mutation.                                  
                                                                                                                              
  After this lands, the entire class of deadlock is gone. The game                                                                                              
  either boots further, or hits a different class of bug (real logic                                                                                            
  error, not a sync issue).                                                                                                                                     
                                                                                                                                                                
  Phase 4: Simplify & remove workarounds (1 session)                                                                                                            
                                                                                                                                                                
  - Remove ProcessLockTracker now-dead sites.                                                                                                                   
  - Remove TrackedMutex::access_unlocked (subsumed by KProcessCell::with).
  - Remove any preempt/scheduler workarounds that existed only to paper over AB-BA.                                                                             
  - Tighten KScopedSchedulerLock assertions: debug-panic if access happens without it held.                                                                     
                                                                                                                                                                
  Risk register                                                                                                                                                 
                                                                                                                                                                
  ┌─────────────────────────────────────────────────────────────┬───────────────────────────────────────────────────────────────────────────────────────────┐
  │                            Risk                             │                                        Mitigation                                         │
  ├─────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────┤
  │ Reentrancy: if a site transitively re-enters scheduler lock │ Audit + confirm reentrancy in Phase 0. Add a test that exercises nested                   │
  │  and the lock isn't reentrant, deadlock.                    │ KScopedSchedulerLock.                                                                     │
  ├─────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────┤   
  │ HLE threads can't hold scheduler spin-lock across blocking  │ Don't hold it across I/O — acquire, touch state, release, do I/O. Match upstream idiom.   │
  │ I/O.                                                        │                                                                                           │   
  ├─────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────┤
  │ Perf regression: scheduler lock contention replacing        │ Scheduler lock is a spin-lock; short critical sections are fine. Long ones were already a │   
  │ parking_lot contention.                                     │  bug. If perf regresses, narrow critical sections per-site.                               │   
  ├─────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────┤
  │ Tests that construct bare KProcess without a scheduler.     │ Add KProcessCell::test_access(&mut self, f) that does raw access unconditionally. Gated   │   
  │                                                             │ behind #[cfg(test)].                                                                      │   
  ├─────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────┤
  │ Partial-conversion accident during Phase 3 (one site        │ The Rust compiler enforces the type change — any missed site fails to compile. Use the    │   
  │ missed).                                                    │ type system as the atomic-conversion oracle.                                              │   
  ├─────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────┤
  │ Sites that legitimately need to yield while accessing       │ Upstream handles these via continuation-style: save state under scheduler lock, release,  │   
  │ KProcess (e.g., waiting on I/O completion).                 │ perform wait, re-acquire. Port would do the same.                                         │   
  └─────────────────────────────────────────────────────────────┴───────────────────────────────────────────────────────────────────────────────────────────┘
                                                                                                                                                                
  Estimated effort                                                

  ┌───────────────────────────┬──────────────────────┬────────────────────────────────────────────────────────────┐
  │           Phase           │       Sessions       │                            Risk                            │
  ├───────────────────────────┼──────────────────────┼────────────────────────────────────────────────────────────┤                                             
  │ 0 audit + scaffolding     │ 1                    │ Low                                                        │
  ├───────────────────────────┼──────────────────────┼────────────────────────────────────────────────────────────┤                                             
  │ 1 scheduler-lock coverage │ 2–3                  │ Medium (can regress boot if partially applied)             │
  ├───────────────────────────┼──────────────────────┼────────────────────────────────────────────────────────────┤                                             
  │ 2 HLE/service sites       │ 1–2                  │ High (semantics vary per service)                          │
  ├───────────────────────────┼──────────────────────┼────────────────────────────────────────────────────────────┤                                             
  │ 3 atomic Mutex→Cell swap  │ 1 long               │ Medium (compile-time enforced; runtime surprises possible) │
  ├───────────────────────────┼──────────────────────┼────────────────────────────────────────────────────────────┤                                             
  │ 4 cleanup                 │ 1                    │ Low                                                        │
  ├───────────────────────────┼──────────────────────┼────────────────────────────────────────────────────────────┤                                             
  │ Total                     │ 6–8 focused sessions │                                                            │
  └───────────────────────────┴──────────────────────┴────────────────────────────────────────────────────────────┘                                             
                                                                  
  Buffer for testing/regression on each phase: realistic total is 10–12 sessions. Each session must end in a committable state (either fully landed phase or    
  cleanly revertable).
                                                                                                                                                                
  Fallback option: parking_lot::ReentrantMutex with fiber-aware ownership                                                                                       
   
  If the full refactor turns out infeasible, the alternative is keeping the Mutex structure but making it fiber-aware:                                          
                                                                  
  - Replace std::sync::Mutex<KThread> with a custom FiberAwareMutex<KThread> that keys ownership on (fiber_id, host_thread_id) instead of just host_thread_id.  
  - Add hooks to Fiber::yield_to that transfer ownership when a fiber migrates.
  - Same 628 sites but the change per site is smaller (just the type, not the access pattern).                                                                  
                                                                                                                                                                
  Less upstream-faithful (still wraps kernel objects in a mutex where upstream has raw refs), but probably 3–4 sessions instead of 6–8. Violates CLAUDE.md's    
  "fidelity beats elegance" rule, but it's pragmatic.                                                                                                           
                                                                                                                                                                
  Recommendation                                                                                                                                                
   
  Do Phase 0 (audit + scaffolding) as a single focused session to produce the classification spreadsheet. That's the cheap step that lets us commit to either   
  path (full refactor or fallback) with real numbers. After Phase 0, decide based on the actual distribution of the 628 sites across categories A–F.