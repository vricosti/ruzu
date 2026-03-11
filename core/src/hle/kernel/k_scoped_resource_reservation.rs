//! Port of zuyu/src/core/hle/kernel/k_scoped_resource_reservation.h
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KScopedResourceReservation: RAII guard that reserves a resource from a
//! KResourceLimit and releases it on drop unless committed.

/// Limitable resource types.
/// Maps to upstream LimitableResource.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LimitableResource {
    PhysicalMemoryMax = 0,
    ThreadCountMax = 1,
    EventCountMax = 2,
    TransferMemoryCountMax = 3,
    SessionCountMax = 4,
}

/// RAII guard for resource limit reservations.
///
/// On creation, reserves `value` units of `resource` from the resource limit.
/// On drop, releases the reservation unless `commit()` was called.
///
/// Full implementation requires KResourceLimit, which is not yet ported.
pub struct KScopedResourceReservation {
    // m_limit: Option<&KResourceLimit>,
    m_value: i64,
    m_resource: LimitableResource,
    m_succeeded: bool,
    m_committed: bool,
}

impl KScopedResourceReservation {
    /// Create a reservation. If `value` is 0 or the limit is None, always succeeds.
    pub fn new(resource: LimitableResource, value: i64) -> Self {
        // TODO: Actually call KResourceLimit::Reserve.
        Self {
            m_value: value,
            m_resource: resource,
            m_succeeded: true,
            m_committed: false,
        }
    }

    /// Create a reservation with a timeout.
    pub fn new_with_timeout(resource: LimitableResource, value: i64, _timeout: i64) -> Self {
        // TODO: Actually call KResourceLimit::Reserve with timeout.
        Self {
            m_value: value,
            m_resource: resource,
            m_succeeded: true,
            m_committed: false,
        }
    }

    /// Commit the reservation. The resource will not be released on drop.
    pub fn commit(&mut self) {
        self.m_committed = true;
    }

    /// Whether the reservation succeeded.
    pub fn succeeded(&self) -> bool {
        self.m_succeeded
    }

    /// Get the reserved resource type.
    pub fn resource(&self) -> LimitableResource {
        self.m_resource
    }

    /// Get the reserved value.
    pub fn value(&self) -> i64 {
        self.m_value
    }
}

impl Drop for KScopedResourceReservation {
    fn drop(&mut self) {
        if self.m_value != 0 && self.m_succeeded && !self.m_committed {
            // TODO: m_limit.Release(m_resource, m_value);
        }
    }
}
