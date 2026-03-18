//! Port of zuyu/src/core/hle/kernel/k_scoped_resource_reservation.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-18
//!
//! KScopedResourceReservation: RAII guard that reserves a resource from a
//! KResourceLimit and releases it on drop unless committed.

use std::sync::{Arc, Mutex};
use super::k_resource_limit::{KResourceLimit, LimitableResource};

/// RAII guard for resource limit reservations.
///
/// On creation, reserves `value` units of `resource` from the resource limit.
/// On drop, releases the reservation unless `commit()` was called.
///
/// Upstream uses a raw pointer (`KResourceLimit*`) and nullifies it in `Commit()`.
/// Rust port uses `Option<Arc<Mutex<KResourceLimit>>>` and takes it in `commit()`.
pub struct KScopedResourceReservation {
    m_limit: Option<Arc<Mutex<KResourceLimit>>>,
    m_value: i64,
    m_resource: LimitableResource,
    m_succeeded: bool,
}

impl KScopedResourceReservation {
    /// Upstream: `KScopedResourceReservation(KResourceLimit* l, LimitableResource r, s64 v)`
    pub fn new(
        limit: Option<Arc<Mutex<KResourceLimit>>>,
        resource: LimitableResource,
        value: i64,
    ) -> Self {
        let succeeded = if let Some(ref lim) = limit {
            if value != 0 {
                lim.lock().unwrap().reserve(resource, value)
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
        limit: Option<Arc<Mutex<KResourceLimit>>>,
        resource: LimitableResource,
        value: i64,
        _timeout: i64,
    ) -> Self {
        // TODO: pass timeout to KResourceLimit::reserve when timeout support is ported
        let succeeded = if let Some(ref lim) = limit {
            if value != 0 {
                lim.lock().unwrap().reserve(resource, value)
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
                lim.lock().unwrap().release(self.m_resource, self.m_value);
            }
        }
    }
}
