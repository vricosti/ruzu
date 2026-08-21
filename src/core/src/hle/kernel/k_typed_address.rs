//! Port of zuyu/src/core/hle/kernel/k_typed_address.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! Re-exports of typed address types from common::typed_address into the Kernel
//! namespace, matching the upstream C++ `using` declarations.

pub use common::typed_address::PhysicalAddress as KPhysicalAddress;
pub use common::typed_address::ProcessAddress as KProcessAddress;
pub use common::typed_address::VirtualAddress as KVirtualAddress;
