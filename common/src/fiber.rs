//! Port of zuyu/src/common/fiber.h and zuyu/src/common/fiber.cpp
//! Status: IN_PROGRESS
//! Derniere synchro: 2026-04-04
//!
//! The macOS/AArch64 backend uses `corosensei` as an implementation detail.
//! The public model intentionally remains upstream's low-level `Fiber`
//! transfer API rather than exposing coroutine semantics outside this file.

use corosensei::stack::{DefaultStack, Stack};
use corosensei::{Coroutine, CoroutineResult, Yielder};
use parking_lot::Mutex;
use std::cell::Cell;
use std::sync::{Arc, Weak};

// Upstream zuyu uses 512 KB but with `VirtualBuffer<u8>` which is paired
// with a guard page protection. Ruzu's debug-mode Rust frames are
// significantly larger than equivalent C++ frames (no inlining, more
// scaffolding for safety checks). Confirmed via guard-page SIGSEGV that
// the 512 KB stack overflows almost immediately at STK boot. Use 4 MB
// to give Rust frames headroom; release-mode builds could shrink this.
const DEFAULT_STACK_SIZE: usize = 4 * 1024 * 1024;

type FiberCoroutine = Coroutine<ResumeCommand, SwitchRequest, (), DefaultStack>;

#[derive(Clone)]
enum ResumeCommand {
    Continue,
}

#[derive(Clone)]
enum SwitchRequest {
    YieldTo(Arc<Fiber>),
    Rewind(Arc<Fiber>),
}

thread_local! {
    static CURRENT_YIELDER: Cell<*const Yielder<ResumeCommand, SwitchRequest>> =
        const { Cell::new(std::ptr::null()) };
}

struct FiberImpl {
    coroutine: Option<FiberCoroutine>,
    guard: Mutex<()>,
    entry_point: Option<Box<dyn FnOnce() + Send>>,
    rewind_point: Option<Box<dyn FnOnce() + Send>>,
    previous_fiber: Option<Arc<Fiber>>,
    self_weak: Weak<Fiber>,
    is_thread_fiber: bool,
    released: bool,
    stack_limit: usize,
    rewind_stack_limit: usize,
    in_rewind: bool,
}

impl FiberImpl {
    fn new_empty() -> Self {
        Self {
            coroutine: None,
            guard: Mutex::new(()),
            entry_point: None,
            rewind_point: None,
            previous_fiber: None,
            self_weak: Weak::new(),
            is_thread_fiber: false,
            released: false,
            stack_limit: 0,
            rewind_stack_limit: 0,
            in_rewind: false,
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
    fn make_coroutine(fiber: Weak<Fiber>, rewind: bool) -> (FiberCoroutine, usize) {
        let stack = DefaultStack::new(DEFAULT_STACK_SIZE).expect("failed to allocate fiber stack");
        let stack_limit = stack.limit().get();
        let coroutine = Coroutine::with_stack(stack, move |yielder, _| {
            CURRENT_YIELDER.with(|slot| slot.set(yielder as *const _));
            let fiber = fiber.upgrade().expect("Fiber coroutine outlived Fiber");
            if rewind {
                fiber.on_rewind();
            } else {
                fiber.start();
            }
        });
        (coroutine, stack_limit)
    }

    fn finish_resume(&self) {
        let previous_fiber = {
            let imp = unsafe { &mut *self.imp.get() };
            imp.previous_fiber.take()
        };
        if let Some(previous_fiber) = previous_fiber {
            unsafe { (*previous_fiber.imp.get()).guard.force_unlock() };
        }
    }

    fn start(&self) -> ! {
        let entry_point = {
            self.finish_resume();
            let imp = unsafe { &mut *self.imp.get() };
            imp.entry_point
                .take()
                .expect("Fiber::start missing entry point")
        };

        entry_point();
        unreachable!("Fiber entry point returned");
    }

    fn on_rewind(&self) -> ! {
        let rewind_point = {
            self.finish_resume();
            let imp = unsafe { &mut *self.imp.get() };
            imp.rewind_point
                .take()
                .expect("Fiber::on_rewind missing rewind point")
        };

        rewind_point();
        unreachable!("Fiber rewind point returned");
    }

    /// Create a new fiber with the given entry point function.
    /// Matches upstream `Fiber::Fiber(std::function<void()>&& entry_point_func)`.
    pub fn new(entry_point_func: Box<dyn FnOnce() + Send>) -> Arc<Self> {
        Arc::new_cyclic(|weak| {
            let (coroutine, stack_limit) = Self::make_coroutine(weak.clone(), false);
            let mut imp = FiberImpl::new_empty();
            imp.self_weak = weak.clone();
            imp.stack_limit = stack_limit;
            imp.rewind_stack_limit = stack_limit;
            imp.coroutine = Some(coroutine);
            imp.entry_point = Some(entry_point_func);
            Self {
                imp: std::cell::UnsafeCell::new(imp),
            }
        })
    }

    /// Convert the current thread to a fiber (creates a fiber that represents the current thread).
    /// Matches upstream `Fiber::ThreadToFiber()`.
    pub fn thread_to_fiber() -> Arc<Self> {
        let fiber = Arc::new_cyclic(|weak| {
            let mut imp = FiberImpl {
                is_thread_fiber: true,
                ..FiberImpl::new_empty()
            };
            imp.self_weak = weak.clone();
            Self {
                imp: std::cell::UnsafeCell::new(imp),
            }
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
        let _ = to_imp;

        let request = SwitchRequest::YieldTo(Arc::clone(to));
        if let Some(from) = weak_from.upgrade() {
            let is_thread_fiber = unsafe { (*from.imp.get()).is_thread_fiber };
            if !is_thread_fiber {
                let yielder = CURRENT_YIELDER.with(|slot| slot.get());
                assert!(
                    !yielder.is_null(),
                    "Fiber::yield_to missing current yielder"
                );
                let resumed = unsafe { (&*yielder).suspend(request) };
                // A guest fiber may resume on a different host thread. Do not
                // carry a reference to the old thread's TLS cell across the
                // switch; reacquire TLS after returning on the new host thread.
                CURRENT_YIELDER.with(|slot| slot.set(yielder));
                from.finish_resume();
                match resumed {
                    ResumeCommand::Continue => return,
                }
            }
        }

        Self::dispatch_until_thread(weak_from, request);
    }

    fn dispatch_until_thread(root: Weak<Fiber>, mut request: SwitchRequest) {
        loop {
            let target = match request {
                SwitchRequest::YieldTo(target) => target,
                SwitchRequest::Rewind(target) => {
                    Self::replace_with_rewind_coroutine(&target);
                    target
                }
            };

            if unsafe { (*target.imp.get()).is_thread_fiber } {
                target.finish_resume();
                return;
            }

            let result = {
                let imp = unsafe { &mut *target.imp.get() };
                let coroutine = imp
                    .coroutine
                    .as_mut()
                    .expect("Target fiber missing coroutine");
                coroutine.resume(ResumeCommand::Continue)
            };

            request = match result {
                CoroutineResult::Yield(next) => next,
                CoroutineResult::Return(()) => unreachable!("Fiber coroutine returned"),
            };

            if let (Some(root), SwitchRequest::YieldTo(next)) = (root.upgrade(), &request) {
                if Arc::ptr_eq(&root, next) {
                    root.finish_resume();
                    return;
                }
            }
        }
    }

    fn replace_with_rewind_coroutine(fiber: &Arc<Fiber>) {
        let (new_coroutine, stack_limit) = {
            let weak = unsafe { (*fiber.imp.get()).self_weak.clone() };
            Self::make_coroutine(weak, true)
        };
        let old_coroutine = {
            let imp = unsafe { &mut *fiber.imp.get() };
            assert!(imp.rewind_point.is_some(), "No rewind point set");
            assert!(!imp.in_rewind, "Already has rewind context");
            imp.in_rewind = true;
            let old_coroutine = std::mem::replace(&mut imp.coroutine, Some(new_coroutine));
            imp.rewind_stack_limit = stack_limit;
            imp.in_rewind = false;
            old_coroutine
        };
        if let Some(mut old_coroutine) = old_coroutine {
            unsafe { old_coroutine.force_reset() };
        }
        let imp = unsafe { &mut *fiber.imp.get() };
        if imp.coroutine.is_none() {
            let (coroutine, _) = Self::make_coroutine(imp.self_weak.clone(), true);
            imp.coroutine = Some(coroutine);
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
        let fiber = unsafe { (*self.imp.get()).self_weak.upgrade() }
            .expect("Fiber::rewind missing self weak");
        CURRENT_YIELDER.with(|slot| {
            let yielder = slot.get();
            assert!(!yielder.is_null(), "Fiber::rewind missing current yielder");
            unsafe { (&*yielder).suspend(SwitchRequest::Rewind(fiber)) };
        });
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
