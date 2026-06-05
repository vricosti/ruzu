//! Port of zuyu/src/common/fiber.h and zuyu/src/common/fiber.cpp
//! Status: IN_PROGRESS
//! Derniere synchro: 2026-04-04
//!
//! This backend uses the `context` crate, which wraps Boost.Context and maps
//! much more directly to upstream `make_fcontext` / `jump_fcontext` than the
//! previous coroutine-based backend.

// Use `ProtectedFixedSizeStack` so that fiber stack overflows fault on a
// guard page instead of silently corrupting adjacent memory. Upstream zuyu
// uses `VirtualBuffer<u8>` which is paired with explicit guard handling;
// ruzu's prior `FixedSizeStack` had no protection — a 512 KB fiber stack
// overflow could land anywhere in the host VA space, including the
// fastmem arena's backing pages, causing the kind of silent dlmalloc
// state corruption observed in the STK multi-core wedge.
use context::stack::ProtectedFixedSizeStack as FixedSizeStack;
use context::{Context, Transfer};
use parking_lot::Mutex;
use std::sync::{Arc, Weak};

// Upstream zuyu uses 512 KB but with `VirtualBuffer<u8>` which is paired
// with a guard page protection. Ruzu's debug-mode Rust frames are
// significantly larger than equivalent C++ frames (no inlining, more
// scaffolding for safety checks). Confirmed via guard-page SIGSEGV that
// the 512 KB stack overflows almost immediately at STK boot. Use 4 MB
// to give Rust frames headroom; release-mode builds could shrink this.
const DEFAULT_STACK_SIZE: usize = 4 * 1024 * 1024;
type RawContext = usize;

fn context_to_raw(context: Context) -> RawContext {
    unsafe { std::mem::transmute::<Context, RawContext>(context) }
}

fn raw_to_context(raw: RawContext) -> Context {
    unsafe { std::mem::transmute::<RawContext, Context>(raw) }
}

struct FiberImpl {
    stack: Option<FixedSizeStack>,
    rewind_stack: Option<FixedSizeStack>,
    guard: Mutex<()>,
    entry_point: Option<Box<dyn FnOnce() + Send>>,
    rewind_point: Option<Box<dyn FnOnce() + Send>>,
    previous_fiber: Option<Arc<Fiber>>,
    is_thread_fiber: bool,
    released: bool,
    context: Option<RawContext>,
    rewind_context: Option<RawContext>,
}

impl FiberImpl {
    fn new_empty() -> Self {
        Self {
            stack: None,
            rewind_stack: None,
            guard: Mutex::new(()),
            entry_point: None,
            rewind_point: None,
            previous_fiber: None,
            is_thread_fiber: false,
            released: false,
            context: None,
            rewind_context: None,
        }
    }
}

/// Fiber class - a userspace thread with its own context.
/// They can be used to implement coroutines, emulated threading systems
/// and certain asynchronous patterns.
///
/// This fiber class is 'threadsafe' - only one fiber can be running at a time
/// and threads will be locked while trying to yield to a running fiber until
/// it yields. WARNING: exchanging two running fibers between threads will cause
/// a deadlock. Each thread should have an intermediary fiber to prevent this.
pub struct Fiber {
    imp: std::cell::UnsafeCell<FiberImpl>,
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}

impl Fiber {
    extern "C" fn fiber_start_func(transfer: Transfer) -> ! {
        // Optional alignment trace. SysV x86-64 ABI requires RSP at function
        // entry to satisfy `RSP mod 16 == 8` (after a `call` pushes the
        // return address from a 16-aligned RSP). The context crate's
        // trampoline ends with `push %rbp; jmp *%rbx` which preserves that
        // invariant, so fiber-entry alignment is correct here.
        //
        // Verified 2026-05-16: the MK8D ~60s SIGSEGV in `hash_one` came from
        // a downstream misalignment, NOT from this entry point. The fiber
        // stack itself is correctly aligned. Trace kept as a sanity check
        // for future fiber-related investigations.
        #[cfg(target_arch = "x86_64")]
        if std::env::var_os("RUZU_TRACE_FIBER_ENTRY").is_some() {
            let rsp: u64;
            unsafe {
                core::arch::asm!("mov {}, rsp", out(reg) rsp, options(nomem, nostack));
            }
            eprintln!(
                "[FIBER_ENTRY] post-prologue rsp=0x{:016x} rsp_mod_16={:#x} (expected 0x0 if ABI entry RSP mod 16 == 8 and prologue subtracts 0x88)",
                rsp,
                rsp & 0xF
            );
        }
        let fiber = unsafe { &*(transfer.data as *const Fiber) };
        fiber.start(transfer)
    }

    extern "C" fn rewind_start_func(transfer: Transfer) -> ! {
        let fiber = unsafe { &*(transfer.data as *const Fiber) };
        fiber.on_rewind(transfer)
    }

    fn start(&self, transfer: Transfer) -> ! {
        // Inside start() the new fiber's guard is locked by the YieldTo
        // caller, so this fiber's FiberImpl is exclusively accessible. We
        // can safely create &mut here — but the PREVIOUS fiber's guard
        // hand-off requires care: it's locked by us (via this fiber's
        // resume path) and we'll unlock it. While the unlock window is
        // open, another thread on a different core could begin its own
        // YieldTo targeting the previous fiber. We must therefore drop
        // any `&mut FiberImpl` to the previous fiber BEFORE force_unlock.
        let entry_point = {
            let imp = unsafe { &mut *self.imp.get() };
            let previous_fiber = imp
                .previous_fiber
                .take()
                .expect("Fiber::start missing previous_fiber");
            let entry = imp.entry_point.take();
            // Drop `imp` (this fiber's &mut) before touching previous's
            // FiberImpl so we don't briefly hold &mut to two FiberImpls
            // and risk aliasing if the second &mut narrowly overlaps with
            // another core's YieldTo on the previous fiber.
            drop(imp);
            // Access previous fiber's FiberImpl via raw pointer so we
            // never create a `&mut FiberImpl` that could alias with a
            // concurrent YieldTo on the previous fiber after we unlock.
            let prev_imp_ptr = previous_fiber.imp.get();
            unsafe {
                std::ptr::addr_of_mut!((*prev_imp_ptr).context)
                    .write(Some(context_to_raw(transfer.context)));
                // Release-store visible to whichever thread will pick up
                // the previous fiber next via lock().
                (*prev_imp_ptr).guard.force_unlock();
            }
            entry.expect("Fiber::start missing entry point")
        };

        entry_point();
        panic!("Fiber entry point returned");
    }

    fn on_rewind(&self, _transfer: Transfer) -> ! {
        // on_rewind runs on this fiber while its guard is held by the
        // caller of Rewind. Safe to use `&mut` for fields of self.
        let rewind_point = {
            let imp = unsafe { &mut *self.imp.get() };
            assert!(imp.context.is_some(), "Fiber::on_rewind missing context");
            imp.context = imp.rewind_context.take();
            std::mem::swap(&mut imp.stack, &mut imp.rewind_stack);
            imp.rewind_point
                .take()
                .expect("Fiber::on_rewind missing rewind point")
        };

        rewind_point();
        panic!("Fiber rewind point returned");
    }

    /// Create a new fiber with the given entry point function.
    /// Matches upstream `Fiber::Fiber(std::function<void()>&& entry_point_func)`.
    pub fn new(entry_point_func: Box<dyn FnOnce() + Send>) -> Arc<Self> {
        let stack =
            FixedSizeStack::new(DEFAULT_STACK_SIZE).expect("failed to allocate fiber stack");
        let rewind_stack =
            FixedSizeStack::new(DEFAULT_STACK_SIZE).expect("failed to allocate rewind stack");

        let mut imp = FiberImpl::new_empty();
        imp.context = Some(context_to_raw(unsafe {
            Context::new(
                &stack,
                Self::fiber_start_func as extern "C" fn(Transfer) -> !,
            )
        }));
        imp.stack = Some(stack);
        imp.rewind_stack = Some(rewind_stack);
        imp.entry_point = Some(entry_point_func);

        Arc::new(Self {
            imp: std::cell::UnsafeCell::new(imp),
        })
    }

    /// Convert the current thread to a fiber (creates a fiber that represents the current thread).
    /// Matches upstream `Fiber::ThreadToFiber()`.
    pub fn thread_to_fiber() -> Arc<Self> {
        let imp = FiberImpl {
            is_thread_fiber: true,
            ..FiberImpl::new_empty()
        };

        let fiber = Arc::new(Self {
            imp: std::cell::UnsafeCell::new(imp),
        });

        // Lock the guard via &Mutex (raw-pointer-dereferenced field) — does
        // NOT create a `&mut FiberImpl`. We're the sole owner at this
        // point (Arc was just constructed) so even &mut would be sound,
        // but matching the no-&mut convention used in yield_to keeps the
        // pattern uniform.
        let guard = unsafe { (*fiber.imp.get()).guard.lock() };
        std::mem::forget(guard);

        fiber
    }

    /// Yields control from Fiber 'from' to Fiber 'to'.
    /// Fiber 'from' must be the currently running fiber.
    /// Matches upstream `Fiber::YieldTo(weak_ptr<Fiber>, Fiber&)`.
    pub fn yield_to(weak_from: Weak<Fiber>, to: &Arc<Fiber>) {
        // CRITICAL: do NOT create `&mut FiberImpl` for `to` BEFORE acquiring
        // the guard. Two host threads (different physical cores) can each
        // call yield_to on the same `to` fiber concurrently. If both
        // construct `&mut FiberImpl` before either acquires the lock, the
        // two `&mut` would briefly co-exist — UB under Rust's `noalias`.
        // The C++ upstream uses raw pointers (no aliasing claim) and is
        // safe; the original Rust port translated `to.impl->guard.lock()`
        // as `(&mut *to.imp.get()).guard.lock()` which silently introduced
        // the UB. Acquire the lock through the raw pointer first, then
        // construct &mut (the lock now guarantees exclusivity).
        let guard = unsafe { (*to.imp.get()).guard.lock() };
        std::mem::forget(guard);
        let to_imp = unsafe { &mut *to.imp.get() };
        to_imp.previous_fiber = weak_from.upgrade();

        let context = to_imp.context.expect("Target fiber missing context");
        // Drop the &mut before the context-switch so no `&mut FiberImpl`
        // lives across the resume. After resume returns, another thread
        // might have already unlocked our guard and started its own
        // yield_to with this fiber as the target; that would create a new
        // `&mut FiberImpl` and the OLD `to_imp` would be an aliased &mut.
        drop(to_imp);
        let transfer = unsafe { raw_to_context(context).resume(Arc::as_ptr(to) as usize) };

        if let Some(from) = weak_from.upgrade() {
            // Same pattern: acquire access without creating &mut, then
            // narrow scope of any &mut we do construct.
            let from_imp = unsafe { &mut *from.imp.get() };
            let previous_fiber = from_imp
                .previous_fiber
                .take()
                .expect("previous_fiber is nullptr!");
            drop(from_imp);
            // Use raw pointer for the previous fiber — after the
            // force_unlock below, another thread can immediately take its
            // guard. Holding `&mut` past force_unlock would be the
            // mirror-image of the bug at the entry of yield_to.
            let prev_imp_ptr = previous_fiber.imp.get();
            unsafe {
                std::ptr::addr_of_mut!((*prev_imp_ptr).context)
                    .write(Some(context_to_raw(transfer.context)));
                (*prev_imp_ptr).guard.force_unlock();
            }
        }
    }

    /// Set a rewind function for this fiber.
    /// Matches upstream `Fiber::SetRewindPoint`.
    pub fn set_rewind_point(&self, rewind_func: Box<dyn FnOnce() + Send>) {
        let imp = unsafe { &mut *self.imp.get() };
        imp.rewind_point = Some(rewind_func);
    }

    /// Rewind this fiber — sets up rewind context and jumps to rewind entry,
    /// which calls OnRewind logic (run rewind_point).
    /// Matches upstream `Fiber::Rewind`.
    pub fn rewind(&self) {
        let imp = unsafe { &mut *self.imp.get() };
        assert!(imp.rewind_point.is_some(), "No rewind point set");
        assert!(imp.rewind_context.is_none(), "Already has rewind context");
        let rewind_stack = imp
            .rewind_stack
            .as_ref()
            .expect("Fiber missing rewind stack");
        imp.rewind_context = Some(context_to_raw(unsafe {
            Context::new(
                rewind_stack,
                Self::rewind_start_func as extern "C" fn(Transfer) -> !,
            )
        }));

        let _ = unsafe {
            raw_to_context(imp.rewind_context.expect("rewind context missing"))
                .resume(self as *const Fiber as usize)
        };
        unreachable!("Fiber::rewind returned");
    }

    /// Only call from main thread's fiber.
    /// Matches upstream `Fiber::Exit`.
    pub fn exit(&self) {
        let imp = unsafe { &mut *self.imp.get() };
        assert!(imp.is_thread_fiber, "Exiting non main thread fiber");
        if !imp.is_thread_fiber {
            return;
        }
        unsafe { imp.guard.force_unlock() };
        imp.released = true;
    }
}

impl Drop for Fiber {
    fn drop(&mut self) {
        let imp = self.imp.get_mut();
        if imp.released {
            return;
        }
        let locked = imp.guard.try_lock();
        if locked.is_none() {
            log::error!("Destroying a fiber that's still running");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::thread;

    /// Thread-to-ID registry, matching upstream ThreadIds helper.
    struct ThreadIds {
        mutex: std::sync::Mutex<HashMap<thread::ThreadId, u32>>,
    }

    impl ThreadIds {
        fn new() -> Self {
            Self {
                mutex: std::sync::Mutex::new(HashMap::new()),
            }
        }

        fn register(&self, id: u32) {
            let mut map = self.mutex.lock().unwrap();
            let tid = thread::current().id();
            assert!(!map.contains_key(&tid), "Registering the same thread twice");
            map.insert(tid, id);
        }

        fn get(&self) -> u32 {
            let map = self.mutex.lock().unwrap();
            *map.get(&thread::current().id()).unwrap()
        }
    }

    /// Port of upstream TestControl1 — basic fiber setup test
    #[test]
    fn test_fiber_setup() {
        use std::sync::Mutex as StdMutex;

        const NUM_THREADS: usize = 7;

        struct TestControl {
            thread_ids: ThreadIds,
            thread_fibers: Vec<StdMutex<Option<Arc<Fiber>>>>,
            work_fibers: Vec<StdMutex<Option<Arc<Fiber>>>>,
            items: Vec<StdMutex<u32>>,
            results: Vec<StdMutex<u32>>,
        }

        let test_control = Arc::new(TestControl {
            thread_ids: ThreadIds::new(),
            thread_fibers: (0..NUM_THREADS).map(|_| StdMutex::new(None)).collect(),
            work_fibers: (0..NUM_THREADS).map(|_| StdMutex::new(None)).collect(),
            items: (0..NUM_THREADS).map(|_| StdMutex::new(0)).collect(),
            results: (0..NUM_THREADS).map(|_| StdMutex::new(0)).collect(),
        });

        let threads: Vec<_> = (0..NUM_THREADS)
            .map(|i| {
                let tc = Arc::clone(&test_control);
                thread::spawn(move || {
                    let id = i as u32;
                    tc.thread_ids.register(id);
                    let thread_fiber = Fiber::thread_to_fiber();
                    *tc.thread_fibers[i].lock().unwrap() = Some(Arc::clone(&thread_fiber));

                    let tc2 = Arc::clone(&tc);
                    let work_fiber = Fiber::new(Box::new(move || {
                        let id = tc2.thread_ids.get();
                        let idx = id as usize;
                        let value = *tc2.items[idx].lock().unwrap();
                        let mut result = value;
                        for j in 0..id {
                            result += 1;
                            let _ = j;
                        }
                        *tc2.results[idx].lock().unwrap() = result;
                        let wf = tc2.work_fibers[idx]
                            .lock()
                            .unwrap()
                            .as_ref()
                            .unwrap()
                            .clone();
                        let tf = tc2.thread_fibers[idx]
                            .lock()
                            .unwrap()
                            .as_ref()
                            .unwrap()
                            .clone();
                        Fiber::yield_to(Arc::downgrade(&wf), &tf);
                    }));
                    *tc.work_fibers[i].lock().unwrap() = Some(Arc::clone(&work_fiber));

                    *tc.items[i].lock().unwrap() = (rand_u32() % 256) as u32;
                    Fiber::yield_to(Arc::downgrade(&thread_fiber), &work_fiber);
                    thread_fiber.exit();
                })
            })
            .collect();

        for t in threads {
            t.join().unwrap();
        }

        for i in 0..NUM_THREADS {
            let item = *test_control.items[i].lock().unwrap();
            let result = *test_control.results[i].lock().unwrap();
            assert_eq!(item + i as u32, result, "Fiber {i} result mismatch");
        }
    }

    /// Port of upstream TestControl2 — fiber inter-thread exchange test
    #[test]
    fn test_fiber_inter_exchange() {
        use std::sync::Mutex as StdMutex;

        struct TestControl {
            assert1: AtomicBool,
            assert2: AtomicBool,
            assert3: AtomicBool,
            value1: StdMutex<u32>,
            value2: StdMutex<u32>,
            trap: AtomicBool,
            trap2: AtomicBool,
            thread_ids: ThreadIds,
            thread_fibers: Vec<StdMutex<Option<Arc<Fiber>>>>,
            fiber1: StdMutex<Option<Arc<Fiber>>>,
            fiber2: StdMutex<Option<Arc<Fiber>>>,
            fiber3: StdMutex<Option<Arc<Fiber>>>,
        }

        let tc = Arc::new(TestControl {
            assert1: AtomicBool::new(false),
            assert2: AtomicBool::new(false),
            assert3: AtomicBool::new(true),
            value1: StdMutex::new(0),
            value2: StdMutex::new(0),
            trap: AtomicBool::new(true),
            trap2: AtomicBool::new(true),
            thread_ids: ThreadIds::new(),
            thread_fibers: vec![StdMutex::new(None), StdMutex::new(None)],
            fiber1: StdMutex::new(None),
            fiber2: StdMutex::new(None),
            fiber3: StdMutex::new(None),
        });

        // DoWork1
        let tc1 = Arc::clone(&tc);
        let f1 = Fiber::new(Box::new(move || {
            tc1.trap2.store(false, Ordering::SeqCst);
            while tc1.trap.load(Ordering::SeqCst) {
                std::hint::spin_loop();
            }
            {
                let mut v = tc1.value1.lock().unwrap();
                for i in 0u32..12000 {
                    *v += i;
                }
            }
            let f1_ref = tc1.fiber1.lock().unwrap().as_ref().unwrap().clone();
            let f3_ref = tc1.fiber3.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&f1_ref), &f3_ref);
            let id = tc1.thread_ids.get();
            tc1.assert1.store(id == 1, Ordering::SeqCst);
            *tc1.value2.lock().unwrap() += 5000;
            let f1_ref = tc1.fiber1.lock().unwrap().as_ref().unwrap().clone();
            let tf = tc1.thread_fibers[id as usize]
                .lock()
                .unwrap()
                .as_ref()
                .unwrap()
                .clone();
            Fiber::yield_to(Arc::downgrade(&f1_ref), &tf);
        }));
        *tc.fiber1.lock().unwrap() = Some(Arc::clone(&f1));

        // DoWork2
        let tc2 = Arc::clone(&tc);
        let f2 = Fiber::new(Box::new(move || {
            while tc2.trap2.load(Ordering::SeqCst) {
                std::hint::spin_loop();
            }
            *tc2.value2.lock().unwrap() = 2000;
            tc2.trap.store(false, Ordering::SeqCst);
            let f2_ref = tc2.fiber2.lock().unwrap().as_ref().unwrap().clone();
            let f1_ref = tc2.fiber1.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&f2_ref), &f1_ref);
            tc2.assert3.store(false, Ordering::SeqCst);
        }));
        *tc.fiber2.lock().unwrap() = Some(Arc::clone(&f2));

        // DoWork3
        let tc3 = Arc::clone(&tc);
        let f3 = Fiber::new(Box::new(move || {
            let id = tc3.thread_ids.get();
            tc3.assert2.store(id == 0, Ordering::SeqCst);
            *tc3.value1.lock().unwrap() += 1000;
            let f3_ref = tc3.fiber3.lock().unwrap().as_ref().unwrap().clone();
            let tf = tc3.thread_fibers[id as usize]
                .lock()
                .unwrap()
                .as_ref()
                .unwrap()
                .clone();
            Fiber::yield_to(Arc::downgrade(&f3_ref), &tf);
        }));
        *tc.fiber3.lock().unwrap() = Some(Arc::clone(&f3));

        let tc_t1 = Arc::clone(&tc);
        let thread1 = thread::spawn(move || {
            tc_t1.thread_ids.register(0);
            let thread_fiber = Fiber::thread_to_fiber();
            *tc_t1.thread_fibers[0].lock().unwrap() = Some(Arc::clone(&thread_fiber));
            // CallFiber1
            let f1_ref = tc_t1.fiber1.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&thread_fiber), &f1_ref);
            thread_fiber.exit();
        });

        let tc_t2 = Arc::clone(&tc);
        let thread2 = thread::spawn(move || {
            tc_t2.thread_ids.register(1);
            let thread_fiber = Fiber::thread_to_fiber();
            *tc_t2.thread_fibers[1].lock().unwrap() = Some(Arc::clone(&thread_fiber));
            // CallFiber2
            let f2_ref = tc_t2.fiber2.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&thread_fiber), &f2_ref);
            thread_fiber.exit();
        });

        thread1.join().unwrap();
        thread2.join().unwrap();

        assert!(tc.assert1.load(Ordering::SeqCst), "assert1 failed");
        assert!(tc.assert2.load(Ordering::SeqCst), "assert2 failed");
        assert!(tc.assert3.load(Ordering::SeqCst), "assert3 failed");
        assert_eq!(*tc.value2.lock().unwrap(), 7000);
        let mut cal_value: u32 = 0;
        for i in 0u32..12000 {
            cal_value += i;
        }
        cal_value += 1000;
        assert_eq!(*tc.value1.lock().unwrap(), cal_value);
    }

    /// Port of upstream TestControl3 — fiber start race test
    #[test]
    fn test_fiber_start_race() {
        use std::sync::Mutex as StdMutex;

        struct TestControl {
            value1: StdMutex<u32>,
            value2: StdMutex<u32>,
            value3: StdMutex<u32>,
            thread_ids: ThreadIds,
            thread_fibers: Vec<StdMutex<Option<Arc<Fiber>>>>,
            fiber1: StdMutex<Option<Arc<Fiber>>>,
            fiber2: StdMutex<Option<Arc<Fiber>>>,
        }

        let tc = Arc::new(TestControl {
            value1: StdMutex::new(0),
            value2: StdMutex::new(0),
            value3: StdMutex::new(0),
            thread_ids: ThreadIds::new(),
            thread_fibers: vec![StdMutex::new(None), StdMutex::new(None)],
            fiber1: StdMutex::new(None),
            fiber2: StdMutex::new(None),
        });

        // DoWork1
        let tc1 = Arc::clone(&tc);
        let f1 = Fiber::new(Box::new(move || {
            *tc1.value1.lock().unwrap() += 1;
            let f1_ref = tc1.fiber1.lock().unwrap().as_ref().unwrap().clone();
            let f2_ref = tc1.fiber2.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&f1_ref), &f2_ref);
            let id = tc1.thread_ids.get();
            *tc1.value3.lock().unwrap() += 1;
            let f1_ref = tc1.fiber1.lock().unwrap().as_ref().unwrap().clone();
            let tf = tc1.thread_fibers[id as usize]
                .lock()
                .unwrap()
                .as_ref()
                .unwrap()
                .clone();
            Fiber::yield_to(Arc::downgrade(&f1_ref), &tf);
        }));
        *tc.fiber1.lock().unwrap() = Some(Arc::clone(&f1));

        // DoWork2
        let tc2 = Arc::clone(&tc);
        let f2 = Fiber::new(Box::new(move || {
            *tc2.value2.lock().unwrap() += 1;
            let id = tc2.thread_ids.get();
            let f2_ref = tc2.fiber2.lock().unwrap().as_ref().unwrap().clone();
            let tf = tc2.thread_fibers[id as usize]
                .lock()
                .unwrap()
                .as_ref()
                .unwrap()
                .clone();
            Fiber::yield_to(Arc::downgrade(&f2_ref), &tf);
        }));
        *tc.fiber2.lock().unwrap() = Some(Arc::clone(&f2));

        let race_function = |tc: Arc<TestControl>, id: u32| {
            tc.thread_ids.register(id);
            let thread_fiber = Fiber::thread_to_fiber();
            *tc.thread_fibers[id as usize].lock().unwrap() = Some(Arc::clone(&thread_fiber));
            // CallFiber1
            let f1_ref = tc.fiber1.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&thread_fiber), &f1_ref);
            thread_fiber.exit();
        };

        let tc_t1 = Arc::clone(&tc);
        let tc_t2 = Arc::clone(&tc);
        let thread1 = thread::spawn(move || race_function(tc_t1, 0));
        let thread2 = thread::spawn(move || race_function(tc_t2, 1));
        thread1.join().unwrap();
        thread2.join().unwrap();

        assert_eq!(*tc.value1.lock().unwrap(), 1);
        assert_eq!(*tc.value2.lock().unwrap(), 1);
        assert_eq!(*tc.value3.lock().unwrap(), 1);
    }

    struct RewindTestControl {
        fiber1: std::sync::Mutex<Option<Arc<Fiber>>>,
        thread_fiber: std::sync::Mutex<Option<Arc<Fiber>>>,
        goal_reached: AtomicBool,
        rewinded: AtomicBool,
    }

    fn do_rewind_work(tc: &Arc<RewindTestControl>) {
        let f1 = tc.fiber1.lock().unwrap().as_ref().unwrap().clone();

        // SetRewindPoint — set rewind to call DoWork again
        let tc_rewind = Arc::clone(tc);
        f1.set_rewind_point(Box::new(move || {
            do_rewind_work(&tc_rewind);
        }));

        if tc.rewinded.load(Ordering::SeqCst) {
            tc.goal_reached.store(true, Ordering::SeqCst);
            let tf = tc.thread_fiber.lock().unwrap().as_ref().unwrap().clone();
            Fiber::yield_to(Arc::downgrade(&f1), &tf);
        }
        tc.rewinded.store(true, Ordering::SeqCst);
        f1.rewind();
    }

    /// Port of upstream TestControl4 — fiber rewind test
    #[test]
    fn test_fiber_rewind() {
        let tc = Arc::new(RewindTestControl {
            fiber1: std::sync::Mutex::new(None),
            thread_fiber: std::sync::Mutex::new(None),
            goal_reached: AtomicBool::new(false),
            rewinded: AtomicBool::new(false),
        });

        // DoWork — this is called twice (once normally, once after rewind)
        let tc1 = Arc::clone(&tc);
        let f1 = Fiber::new(Box::new(move || {
            do_rewind_work(&tc1);
        }));
        *tc.fiber1.lock().unwrap() = Some(Arc::clone(&f1));

        // Execute
        let thread_fiber = Fiber::thread_to_fiber();
        *tc.thread_fiber.lock().unwrap() = Some(Arc::clone(&thread_fiber));
        Fiber::yield_to(Arc::downgrade(&thread_fiber), &f1);
        thread_fiber.exit();

        assert!(tc.goal_reached.load(Ordering::SeqCst), "goal not reached");
        assert!(tc.rewinded.load(Ordering::SeqCst), "not rewinded");
    }

    #[test]
    fn test_thread_fiber_can_roundtrip_same_worker_multiple_times() {
        let thread_fiber = Fiber::thread_to_fiber();
        let counter = Arc::new(std::sync::Mutex::new(0u32));
        let worker_slot: Arc<std::sync::Mutex<Option<Arc<Fiber>>>> =
            Arc::new(std::sync::Mutex::new(None));

        let worker_slot_clone = Arc::clone(&worker_slot);
        let thread_fiber_clone = Arc::clone(&thread_fiber);
        let counter_clone = Arc::clone(&counter);

        let worker = Fiber::new(Box::new(move || {
            for _ in 0..2 {
                *counter_clone.lock().unwrap() += 1;
                let self_fiber = worker_slot_clone.lock().unwrap().as_ref().unwrap().clone();
                Fiber::yield_to(Arc::downgrade(&self_fiber), &thread_fiber_clone);
            }
        }));
        *worker_slot.lock().unwrap() = Some(Arc::clone(&worker));

        Fiber::yield_to(Arc::downgrade(&thread_fiber), &worker);
        assert_eq!(*counter.lock().unwrap(), 1);

        Fiber::yield_to(Arc::downgrade(&thread_fiber), &worker);
        assert_eq!(*counter.lock().unwrap(), 2);

        thread_fiber.exit();
    }

    /// Simple random u32 for test (avoids external dependency)
    fn rand_u32() -> u32 {
        use std::time::SystemTime;
        let d = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap();
        // Mix time-based entropy
        let mut x = d.subsec_nanos() ^ (d.as_secs() as u32);
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        x
    }
}
