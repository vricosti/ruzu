//! Port of Eden's core/hle/kernel/k_scoped_resource_reservation.h.
//!
//! KScopedResourceReservation: RAII guard that reserves a resource from a
//! KResourceLimit and releases it on drop unless committed.

use super::k_resource_limit::{KResourceLimit, LimitableResource};
use std::sync::Arc;

/// RAII guard for resource limit reservations.
///
/// On creation, reserves `value` units of `resource` from the resource limit.
/// On drop, releases the reservation unless `commit()` was called.
///
/// Upstream uses a raw pointer (`KResourceLimit*`) and nullifies it in `Commit()`.
/// Rust retains the object with an `Arc` and takes it in `commit()`.
pub struct KScopedResourceReservation {
    m_limit: Option<Arc<KResourceLimit>>,
    m_value: i64,
    m_resource: LimitableResource,
    m_succeeded: bool,
}

impl KScopedResourceReservation {
    /// Upstream: `KScopedResourceReservation(KResourceLimit* l, LimitableResource r, s64 v)`
    pub fn new(
        limit: Option<Arc<KResourceLimit>>,
        resource: LimitableResource,
        value: i64,
    ) -> Self {
        let succeeded = if let Some(ref lim) = limit {
            if value != 0 {
                lim.reserve(resource, value)
            } else {
                true
            }
        } else {
            true
        };
        Self {
            m_limit: limit,
            m_value: value,
            m_resource: resource,
            m_succeeded: succeeded,
        }
    }

    /// Upstream: `KScopedResourceReservation(KResourceLimit* l, LimitableResource r, s64 v, s64 timeout)`
    pub fn new_with_timeout(
        limit: Option<Arc<KResourceLimit>>,
        resource: LimitableResource,
        value: i64,
        timeout: i64,
    ) -> Self {
        let succeeded = if let Some(ref lim) = limit {
            if value != 0 {
                lim.reserve_with_timeout(resource, value, timeout)
            } else {
                true
            }
        } else {
            true
        };
        Self {
            m_limit: limit,
            m_value: value,
            m_resource: resource,
            m_succeeded: succeeded,
        }
    }

    /// Commit the reservation. The resource will not be released on drop.
    /// Upstream: sets `m_limit = nullptr`.
    pub fn commit(&mut self) {
        self.m_limit = None;
    }

    /// Whether the reservation succeeded.
    pub fn succeeded(&self) -> bool {
        self.m_succeeded
    }
}

impl Drop for KScopedResourceReservation {
    fn drop(&mut self) {
        // Upstream: if (m_limit && m_value && m_succeeded) { m_limit->Release(m_resource, m_value); }
        if let Some(ref lim) = self.m_limit {
            if self.m_value != 0 && self.m_succeeded {
                lim.release(self.m_resource, self.m_value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uncommitted_reservation_is_released_on_drop() {
        let limit = Arc::new(KResourceLimit::new());
        limit
            .set_limit_value(LimitableResource::EventCountMax, 1)
            .unwrap();

        {
            let reservation = KScopedResourceReservation::new(
                Some(Arc::clone(&limit)),
                LimitableResource::EventCountMax,
                1,
            );
            assert!(reservation.succeeded());
            assert_eq!(limit.get_current_value(LimitableResource::EventCountMax), 1);
        }

        assert_eq!(limit.get_current_value(LimitableResource::EventCountMax), 0);
    }

    #[test]
    fn committed_reservation_remains_charged() {
        let limit = Arc::new(KResourceLimit::new());
        limit
            .set_limit_value(LimitableResource::SessionCountMax, 1)
            .unwrap();

        let mut reservation = KScopedResourceReservation::new(
            Some(Arc::clone(&limit)),
            LimitableResource::SessionCountMax,
            1,
        );
        assert!(reservation.succeeded());
        reservation.commit();
        drop(reservation);

        assert_eq!(
            limit.get_current_value(LimitableResource::SessionCountMax),
            1
        );
        limit.release(LimitableResource::SessionCountMax, 1);
    }

    #[test]
    fn explicit_timeout_is_forwarded_to_resource_limit() {
        let limit = Arc::new(KResourceLimit::new());
        limit
            .set_limit_value(LimitableResource::ThreadCountMax, 2)
            .unwrap();
        assert!(limit.reserve(LimitableResource::ThreadCountMax, 2));
        limit.release_with_hint(LimitableResource::ThreadCountMax, 0, 1);

        let reservation = KScopedResourceReservation::new_with_timeout(
            Some(Arc::clone(&limit)),
            LimitableResource::ThreadCountMax,
            1,
            0,
        );

        assert!(!reservation.succeeded());
        limit.release_with_hint(LimitableResource::ThreadCountMax, 2, 1);
    }
}
