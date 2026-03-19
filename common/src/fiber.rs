//! Port of zuyu/src/common/fiber.h and zuyu/src/common/fiber.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-19
//!
//! Fiber/coroutine implementation using platform-specific context switching.
//! On Linux, this uses libc's ucontext API (makecontext/swapcontext).
//! The C++ version uses boost::context::detail::fcontext; we use ucontext as a portable
//! alternative that is available on all Linux systems.

use crate::virtual_buffer::VirtualBuffer;
use parking_lot::Mutex;
use std::sync::{Arc, Weak};

const DEFAULT_STACK_SIZE: usize = 512 * 1024;

/// Opaque fiber context. On Linux we use ucontext_t.
#[cfg(unix)]
type FiberContext = libc::ucontext_t;

struct FiberImpl {
    stack: VirtualBuffer<u8>,
    rewind_stack: VirtualBuffer<u8>,

    guard: Mutex<()>,
    entry_point: Option<Box<dyn FnOnce() + Send>>,
    rewind_point: Option<Box<dyn FnOnce() + Send>>,
    previous_fiber: Option<Arc<Fiber>>,
    is_thread_fiber: bool,
    released: bool,

    #[cfg(unix)]
    context: FiberContext,
    #[cfg(unix)]
    rewind_context: Option<FiberContext>,
}

impl FiberImpl {
    #[cfg(unix)]
    fn new_empty() -> Self {
        Self {
            stack: VirtualBuffer::with_count(DEFAULT_STACK_SIZE),
            rewind_stack: VirtualBuffer::with_count(DEFAULT_STACK_SIZE),
            guard: Mutex::new(()),
            entry_point: None,
            rewind_point: None,
            previous_fiber: None,
            is_thread_fiber: false,
            released: false,
            context: unsafe { std::mem::zeroed() },
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

// Safety: Fiber is designed to be shared between threads via Arc,
// but only one fiber runs at a time (enforced by the mutex guard).
unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}

/// Helper to reconstruct a Fiber pointer from two i32 halves passed via makecontext.
#[cfg(unix)]
unsafe fn fiber_from_halves(low: i32, high: i32) -> &'static Fiber {
    let ptr = ((high as u64) << 32) | (low as u64 & 0xFFFFFFFF);
    &*(ptr as *const Fiber)
}

/// Corresponds to upstream Fiber::FiberStartFunc + Fiber::Start
#[cfg(unix)]
extern "C" fn fiber_start_func(fiber_ptr: i32, fiber_ptr_high: i32) {
    let fiber = unsafe { fiber_from_halves(fiber_ptr, fiber_ptr_high) };
    let imp = unsafe { &mut *fiber.imp.get() };

    // Upstream Start(): unlock previous fiber's guard and reset
    assert!(imp.previous_fiber.is_some());
    if let Some(prev) = imp.previous_fiber.take() {
        let prev_imp = unsafe { &mut *prev.imp.get() };
        // Safety: the previous fiber locked its guard before the context switch;
        // we unlock it here from a different context, matching upstream behavior.
        unsafe { prev_imp.guard.force_unlock() };
    }

    // Run the entry point
    if let Some(entry) = imp.entry_point.take() {
        entry();
    }

    unreachable!("Fiber entry point returned!");
}

/// Corresponds to upstream Fiber::RewindStartFunc + Fiber::OnRewind
#[cfg(unix)]
extern "C" fn rewind_start_func(fiber_ptr: i32, fiber_ptr_high: i32) {
    let fiber = unsafe { fiber_from_halves(fiber_ptr, fiber_ptr_high) };
    let imp = unsafe { &mut *fiber.imp.get() };

    // OnRewind: swap context <- rewind_context, clear rewind_context, swap stacks
    assert!(imp.rewind_context.is_some());
    imp.context = imp.rewind_context.take().unwrap();
    std::mem::swap(&mut imp.stack, &mut imp.rewind_stack);

    // Execute the rewind function
    if let Some(rewind) = imp.rewind_point.take() {
        rewind();
    }

    unreachable!("Fiber rewind point returned!");
}

impl Fiber {
    /// Create a new fiber with the given entry point function.
    pub fn new(entry_point_func: Box<dyn FnOnce() + Send>) -> Arc<Self> {
        let mut imp = FiberImpl::new_empty();
        imp.entry_point = Some(entry_point_func);

        let fiber = Arc::new(Self {
            imp: std::cell::UnsafeCell::new(imp),
        });

        // Set up the context
        #[cfg(unix)]
        {
            let fiber_imp = unsafe { &mut *fiber.imp.get() };
            unsafe {
                libc::getcontext(&mut fiber_imp.context);
                fiber_imp.context.uc_stack.ss_sp = fiber_imp.stack.data_mut() as *mut libc::c_void;
                fiber_imp.context.uc_stack.ss_size = DEFAULT_STACK_SIZE;
                fiber_imp.context.uc_link = std::ptr::null_mut();

                // Pass the pointer as two int arguments (makecontext only takes int args)
                let ptr = &*fiber as *const Fiber as u64;
                let low = ptr as i32;
                let high = (ptr >> 32) as i32;

                libc::makecontext(
                    &mut fiber_imp.context,
                    std::mem::transmute::<extern "C" fn(i32, i32), extern "C" fn()>(
                        fiber_start_func,
                    ),
                    2,
                    low,
                    high,
                );
            }
        }

        fiber
    }

    /// Convert the current thread to a fiber (creates a fiber that represents the current thread).
    pub fn thread_to_fiber() -> Arc<Self> {
        let mut imp = FiberImpl::new_empty();
        imp.is_thread_fiber = true;

        let fiber = Arc::new(Self {
            imp: std::cell::UnsafeCell::new(imp),
        });

        // Lock the guard to indicate this fiber is "running"
        let fiber_imp = unsafe { &mut *fiber.imp.get() };
        let guard = fiber_imp.guard.lock();
        // We intentionally leak the guard here - it will be unlocked by Exit()
        std::mem::forget(guard);

        // Get current context
        #[cfg(unix)]
        unsafe {
            libc::getcontext(&mut fiber_imp.context);
        }

        fiber
    }

    /// Yields control from Fiber 'from' to Fiber 'to'.
    /// Fiber 'from' must be the currently running fiber.
    pub fn yield_to(weak_from: Weak<Fiber>, to: &Arc<Fiber>) {
        let to_imp = unsafe { &mut *to.imp.get() };

        // Lock the target fiber
        let guard = to_imp.guard.lock();
        std::mem::forget(guard); // Will be unlocked by the target fiber

        // Set previous fiber
        to_imp.previous_fiber = weak_from.upgrade();

        #[cfg(unix)]
        {
            // Save current context and switch to target
            if let Some(from_fiber) = weak_from.upgrade() {
                let from_imp = unsafe { &mut *from_fiber.imp.get() };
                unsafe {
                    libc::swapcontext(&mut from_imp.context, &to_imp.context);
                }

                // After returning here (another fiber yielded back to us),
                // release previous fiber — matches upstream YieldTo post-jump logic
                let from_imp2 = unsafe { &mut *from_fiber.imp.get() };
                if let Some(prev) = from_imp2.previous_fiber.take() {
                    let prev_imp = unsafe { &mut *prev.imp.get() };
                    // Note: with fcontext, upstream also saves transfer.fctx here.
                    // With ucontext, swapcontext already saved it in-place.
                    unsafe { prev_imp.guard.force_unlock() };
                }
            }
        }
    }

    /// Set a rewind function for this fiber.
    pub fn set_rewind_point(&self, rewind_func: Box<dyn FnOnce() + Send>) {
        let imp = unsafe { &mut *self.imp.get() };
        imp.rewind_point = Some(rewind_func);
    }

    /// Rewind this fiber — sets up rewind context and jumps to rewind_start_func,
    /// which calls OnRewind logic (swap contexts/stacks, run rewind_point).
    pub fn rewind(&self) {
        let imp = unsafe { &mut *self.imp.get() };
        assert!(imp.rewind_point.is_some(), "No rewind point set");
        assert!(imp.rewind_context.is_none(), "Already has rewind context");

        #[cfg(unix)]
        unsafe {
            // Create a new context on the rewind stack, pointing to rewind_start_func
            let mut rewind_ctx: FiberContext = std::mem::zeroed();
            libc::getcontext(&mut rewind_ctx);
            rewind_ctx.uc_stack.ss_sp = imp.rewind_stack.data_mut() as *mut libc::c_void;
            rewind_ctx.uc_stack.ss_size = DEFAULT_STACK_SIZE;
            rewind_ctx.uc_link = std::ptr::null_mut();

            let ptr = self as *const Fiber as u64;
            let low = ptr as i32;
            let high = (ptr >> 32) as i32;

            libc::makecontext(
                &mut rewind_ctx,
                std::mem::transmute::<extern "C" fn(i32, i32), extern "C" fn()>(
                    rewind_start_func,
                ),
                2,
                low,
                high,
            );

            // Store the rewind context, then jump to it.
            // Upstream does: jump_fcontext(impl->rewind_context, this)
            // With ucontext: swapcontext saves current context into imp.context,
            // then rewind_start_func's OnRewind swaps context <-> rewind_context.
            imp.rewind_context = Some(rewind_ctx);
            let rewind_ctx_ptr = imp.rewind_context.as_mut().unwrap() as *mut FiberContext;
            libc::swapcontext(&mut imp.context, rewind_ctx_ptr);
        }
    }

    /// Only call from main thread's fiber.
    pub fn exit(&self) {
        let imp = unsafe { &mut *self.imp.get() };
        assert!(imp.is_thread_fiber, "Exiting non main thread fiber");
        if !imp.is_thread_fiber {
            return;
        }
        // Unlock the guard
        unsafe {
            imp.guard.force_unlock();
        }
        imp.released = true;
    }
}

impl Drop for Fiber {
    fn drop(&mut self) {
        let imp = self.imp.get_mut();
        if imp.released {
            return;
        }
        // Make sure the Fiber is not being used
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
                        let wf = tc2.work_fibers[idx].lock().unwrap().as_ref().unwrap().clone();
                        let tf = tc2.thread_fibers[idx].lock().unwrap().as_ref().unwrap().clone();
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
