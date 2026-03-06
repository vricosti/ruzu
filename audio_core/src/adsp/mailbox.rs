use parking_lot::{Condvar, Mutex};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AppMailboxId {
    Invalid = 0,
    AudioRenderer = 50,
    AudioRendererMemoryMapUnmap = 51,
}

impl Default for AppMailboxId {
    fn default() -> Self {
        Self::Invalid
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Host,
    Dsp,
}

#[derive(Clone, Default)]
pub struct Mailbox {
    inner: Arc<MailboxInner>,
}

#[derive(Default)]
struct MailboxInner {
    id: Mutex<AppMailboxId>,
    host_queue: Mutex<VecDeque<u32>>,
    dsp_queue: Mutex<VecDeque<u32>>,
    host_cv: Condvar,
    dsp_cv: Condvar,
}

impl Mailbox {
    pub fn initialize(&self, app_mailbox_id: AppMailboxId) {
        self.reset();
        *self.inner.id.lock() = app_mailbox_id;
    }

    pub fn id(&self) -> AppMailboxId {
        *self.inner.id.lock()
    }

    pub fn send(&self, direction: Direction, message: u32) {
        let (queue, cv) = match direction {
            Direction::Host => (&self.inner.host_queue, &self.inner.host_cv),
            Direction::Dsp => (&self.inner.dsp_queue, &self.inner.dsp_cv),
        };
        queue.lock().push_back(message);
        cv.notify_all();
    }

    pub fn receive(&self, direction: Direction) -> u32 {
        let (queue, cv) = match direction {
            Direction::Host => (&self.inner.host_queue, &self.inner.host_cv),
            Direction::Dsp => (&self.inner.dsp_queue, &self.inner.dsp_cv),
        };
        let mut queue = queue.lock();
        loop {
            if let Some(message) = queue.pop_front() {
                return message;
            }
            cv.wait(&mut queue);
        }
    }

    pub fn receive_with_stop(
        &self,
        direction: Direction,
        stop_requested: &AtomicBool,
    ) -> Option<u32> {
        let (queue, cv) = match direction {
            Direction::Host => (&self.inner.host_queue, &self.inner.host_cv),
            Direction::Dsp => (&self.inner.dsp_queue, &self.inner.dsp_cv),
        };
        let mut queue = queue.lock();
        loop {
            if let Some(message) = queue.pop_front() {
                return Some(message);
            }
            if stop_requested.load(Ordering::SeqCst) {
                return None;
            }
            cv.wait_for(&mut queue, Duration::from_millis(1));
        }
    }

    pub fn try_receive(&self, direction: Direction) -> Option<u32> {
        let queue = match direction {
            Direction::Host => &self.inner.host_queue,
            Direction::Dsp => &self.inner.dsp_queue,
        };
        queue.lock().pop_front()
    }

    pub fn reset(&self) {
        *self.inner.id.lock() = AppMailboxId::Invalid;
        self.inner.host_queue.lock().clear();
        self.inner.dsp_queue.lock().clear();
        self.inner.host_cv.notify_all();
        self.inner.dsp_cv.notify_all();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicBool;

    #[test]
    fn initialize_sets_mailbox_id() {
        let mailbox = Mailbox::default();
        mailbox.initialize(AppMailboxId::AudioRenderer);
        assert_eq!(mailbox.id(), AppMailboxId::AudioRenderer);
    }

    #[test]
    fn reset_clears_id_and_pending_messages() {
        let mailbox = Mailbox::default();
        mailbox.initialize(AppMailboxId::AudioRendererMemoryMapUnmap);
        mailbox.send(Direction::Host, 1);
        mailbox.send(Direction::Dsp, 2);

        mailbox.reset();

        assert_eq!(mailbox.id(), AppMailboxId::Invalid);
        assert_eq!(mailbox.try_receive(Direction::Host), None);
        assert_eq!(mailbox.try_receive(Direction::Dsp), None);
    }

    #[test]
    fn receive_with_stop_returns_none_when_stop_requested() {
        let mailbox = Mailbox::default();
        let stop_requested = AtomicBool::new(true);

        assert_eq!(
            mailbox.receive_with_stop(Direction::Host, &stop_requested),
            None
        );
    }
}
