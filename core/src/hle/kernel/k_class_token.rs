//! Port of zuyu/src/core/hle/kernel/k_class_token.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KClassTokenGenerator with ObjectType enum and compile-time token generation.
//! The C++ version uses template metaprogramming to generate class tokens at
//! compile time based on the class hierarchy. In Rust, we use const functions
//! and an enum-based approach.

/// The underlying type for class tokens.
pub type TokenBaseType = u16;

/// Alias matching upstream `ClassTokenType`.
pub type ClassTokenType = TokenBaseType;

/// Number of bits reserved for base class identification.
pub const BASE_CLASS_BITS: usize = 8;

/// Number of bits reserved for final class identification.
pub const FINAL_CLASS_BITS: usize = (core::mem::size_of::<TokenBaseType>() * 8) - BASE_CLASS_BITS;

/// One bit per base class.
pub const NUM_BASE_CLASSES: usize = BASE_CLASS_BITS;

/// Final classes are permutations of three bits from FINAL_CLASS_BITS bits.
/// This is the number of 3-combinations from FINAL_CLASS_BITS bits.
pub const NUM_FINAL_CLASSES: usize = {
    let mut index: usize = 0;
    let mut i: usize = 0;
    while i < FINAL_CLASS_BITS {
        let mut j = i + 1;
        while j < FINAL_CLASS_BITS {
            let mut k = j + 1;
            while k < FINAL_CLASS_BITS {
                index += 1;
                k += 1;
            }
            j += 1;
        }
        i += 1;
    }
    index
};

/// Object type enum matching the upstream KClassTokenGenerator::ObjectType.
/// Used to identify kernel object types in the class token hierarchy.
#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObjectType {
    KAutoObject = 0,

    // Base classes start
    KSynchronizationObject = 1, // BaseClassesStart
    KReadableEvent = 2,
    // BaseClassesEnd = 3

    // Final classes start (= BaseClassesEnd = 3)
    KInterruptEvent = 3, // FinalClassesStart
    KDebug = 4,
    KThread = 5,
    KServerPort = 6,
    KServerSession = 7,
    KClientPort = 8,
    KClientSession = 9,
    KProcess = 10,
    KResourceLimit = 11,
    KLightSession = 12,
    KPort = 13,
    KSession = 14,
    KSharedMemory = 15,
    KEvent = 16,
    KLightClientSession = 17,
    KLightServerSession = 18,
    KTransferMemory = 19,
    KDeviceAddressSpace = 20,
    KSessionRequest = 21,
    KCodeMemory = 22,

    KSystemResource = 23,

    // NOTE: True order for these has not been determined yet.
    KAlpha = 24,
    KBeta = 25,
    // FinalClassesEnd = FinalClassesStart + NUM_FINAL_CLASSES
}

impl ObjectType {
    pub const BASE_CLASSES_START: u16 = ObjectType::KSynchronizationObject as u16;
    pub const BASE_CLASSES_END: u16 = 3; // One past KReadableEvent
    pub const FINAL_CLASSES_START: u16 = Self::BASE_CLASSES_END;
    pub const FINAL_CLASSES_END: u16 = Self::FINAL_CLASSES_START + NUM_FINAL_CLASSES as u16;
}

/// Compute the base class token for a given base class index.
/// Each base class gets a single bit in the lower BASE_CLASS_BITS.
pub const fn base_class_token(index: TokenBaseType) -> TokenBaseType {
    1u16 << index
}

/// Compute the final class token for a given final class index.
/// Final classes are 3-bit combinations shifted into the upper FINAL_CLASS_BITS.
pub const fn final_class_token(index: TokenBaseType) -> TokenBaseType {
    let mut count: TokenBaseType = 0;
    let mut i: usize = 0;
    while i < FINAL_CLASS_BITS {
        let mut j = i + 1;
        while j < FINAL_CLASS_BITS {
            let mut k = j + 1;
            while k < FINAL_CLASS_BITS {
                if count == index {
                    return (((1u16 << i) | (1u16 << j) | (1u16 << k)) << BASE_CLASS_BITS as u16)
                        as TokenBaseType;
                }
                count += 1;
                k += 1;
            }
            j += 1;
        }
        i += 1;
    }
    panic!("final_class_token: index out of range");
}

/// Describes a kernel object's position in the class hierarchy for token computation.
/// This replaces the C++ template-based class token generation.
///
/// Each kernel object type declares its ObjectType and its base class's ObjectType.
/// The token is computed by OR-ing together the chain of tokens from the object
/// up to KAutoObject.
pub struct ClassTokenInfo {
    pub object_type: ObjectType,
    /// The base class's ObjectType. KAutoObject has no base (uses KAutoObject itself).
    pub base_object_type: ObjectType,
    /// Whether this is a final class (not further subclassed in the token hierarchy).
    pub is_final: bool,
}

/// Compute the class token for a given object type, given the full hierarchy table.
/// This mirrors the C++ recursive GetClassToken<T>() template.
pub const fn compute_class_token(
    object_type: ObjectType,
    hierarchy: &[ClassTokenInfo],
) -> TokenBaseType {
    let info = find_info(object_type, hierarchy);
    let ot = info.object_type as u16;

    if ot == ObjectType::KAutoObject as u16 {
        // KAutoObject has token 0
        return 0;
    }

    let base_token = compute_class_token(info.base_object_type, hierarchy);

    if !info.is_final && ot != ObjectType::KSystemResource as u16 {
        // Base class: single bit
        let class_index = ot - ObjectType::BASE_CLASSES_START;
        base_token | base_class_token(class_index)
    } else {
        // Final class (or KSystemResource which is treated as final)
        let class_index = ot - ObjectType::FINAL_CLASSES_START;
        base_token | final_class_token(class_index)
    }
}

const fn find_info(object_type: ObjectType, hierarchy: &[ClassTokenInfo]) -> &ClassTokenInfo {
    let mut i = 0;
    while i < hierarchy.len() {
        if hierarchy[i].object_type as u16 == object_type as u16 {
            return &hierarchy[i];
        }
        i += 1;
    }
    panic!("find_info: ObjectType not found in hierarchy");
}

/// The full kernel class hierarchy, matching upstream.
/// Each entry describes: (ObjectType, BaseObjectType, is_final).
///
/// Base classes (non-final, subclassable):
///   KAutoObject -> (root)
///   KSynchronizationObject -> KAutoObject
///   KReadableEvent -> KAutoObject
///
/// Final classes:
///   Most final classes inherit from KAutoObject directly.
///   KThread, KProcess, etc. inherit from KSynchronizationObject.
///   KInterruptEvent inherits from KReadableEvent.
pub const CLASS_HIERARCHY: &[ClassTokenInfo] = &[
    ClassTokenInfo {
        object_type: ObjectType::KAutoObject,
        base_object_type: ObjectType::KAutoObject,
        is_final: false,
    },
    // Base classes
    ClassTokenInfo {
        object_type: ObjectType::KSynchronizationObject,
        base_object_type: ObjectType::KAutoObject,
        is_final: false,
    },
    ClassTokenInfo {
        object_type: ObjectType::KReadableEvent,
        base_object_type: ObjectType::KAutoObject,
        is_final: false,
    },
    // Final classes
    ClassTokenInfo {
        object_type: ObjectType::KInterruptEvent,
        base_object_type: ObjectType::KReadableEvent,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KDebug,
        base_object_type: ObjectType::KSynchronizationObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KThread,
        base_object_type: ObjectType::KSynchronizationObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KServerPort,
        base_object_type: ObjectType::KSynchronizationObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KServerSession,
        base_object_type: ObjectType::KSynchronizationObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KClientPort,
        base_object_type: ObjectType::KSynchronizationObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KClientSession,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KProcess,
        base_object_type: ObjectType::KSynchronizationObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KResourceLimit,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KLightSession,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KPort,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KSession,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KSharedMemory,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KEvent,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KLightClientSession,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KLightServerSession,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KTransferMemory,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KDeviceAddressSpace,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KSessionRequest,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KCodeMemory,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KSystemResource,
        base_object_type: ObjectType::KAutoObject,
        is_final: true, // Treated as final for token purposes (same_as<KSystemResource> check)
    },
    ClassTokenInfo {
        object_type: ObjectType::KAlpha,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
    ClassTokenInfo {
        object_type: ObjectType::KBeta,
        base_object_type: ObjectType::KAutoObject,
        is_final: true,
    },
];

/// Pre-computed class tokens for each object type.
/// Use `class_token(ObjectType::KThread)` to get the token for KThread.
pub const fn class_token(object_type: ObjectType) -> ClassTokenType {
    compute_class_token(object_type, CLASS_HIERARCHY)
}

// Pre-computed token constants for common types, usable in const contexts.
pub const TOKEN_K_AUTO_OBJECT: ClassTokenType = class_token(ObjectType::KAutoObject);
pub const TOKEN_K_SYNCHRONIZATION_OBJECT: ClassTokenType =
    class_token(ObjectType::KSynchronizationObject);
pub const TOKEN_K_READABLE_EVENT: ClassTokenType = class_token(ObjectType::KReadableEvent);
pub const TOKEN_K_THREAD: ClassTokenType = class_token(ObjectType::KThread);
pub const TOKEN_K_PROCESS: ClassTokenType = class_token(ObjectType::KProcess);
pub const TOKEN_K_SERVER_PORT: ClassTokenType = class_token(ObjectType::KServerPort);
pub const TOKEN_K_SERVER_SESSION: ClassTokenType = class_token(ObjectType::KServerSession);
pub const TOKEN_K_CLIENT_PORT: ClassTokenType = class_token(ObjectType::KClientPort);
pub const TOKEN_K_CLIENT_SESSION: ClassTokenType = class_token(ObjectType::KClientSession);
pub const TOKEN_K_RESOURCE_LIMIT: ClassTokenType = class_token(ObjectType::KResourceLimit);
pub const TOKEN_K_EVENT: ClassTokenType = class_token(ObjectType::KEvent);
pub const TOKEN_K_SHARED_MEMORY: ClassTokenType = class_token(ObjectType::KSharedMemory);
pub const TOKEN_K_TRANSFER_MEMORY: ClassTokenType = class_token(ObjectType::KTransferMemory);
pub const TOKEN_K_CODE_MEMORY: ClassTokenType = class_token(ObjectType::KCodeMemory);
pub const TOKEN_K_PORT: ClassTokenType = class_token(ObjectType::KPort);
pub const TOKEN_K_SESSION: ClassTokenType = class_token(ObjectType::KSession);
pub const TOKEN_K_LIGHT_SESSION: ClassTokenType = class_token(ObjectType::KLightSession);
pub const TOKEN_K_LIGHT_CLIENT_SESSION: ClassTokenType =
    class_token(ObjectType::KLightClientSession);
pub const TOKEN_K_LIGHT_SERVER_SESSION: ClassTokenType =
    class_token(ObjectType::KLightServerSession);
pub const TOKEN_K_DEVICE_ADDRESS_SPACE: ClassTokenType =
    class_token(ObjectType::KDeviceAddressSpace);
pub const TOKEN_K_SESSION_REQUEST: ClassTokenType = class_token(ObjectType::KSessionRequest);
pub const TOKEN_K_INTERRUPT_EVENT: ClassTokenType = class_token(ObjectType::KInterruptEvent);
pub const TOKEN_K_DEBUG: ClassTokenType = class_token(ObjectType::KDebug);
pub const TOKEN_K_SYSTEM_RESOURCE: ClassTokenType = class_token(ObjectType::KSystemResource);

/// Check whether a token `derived` is derived from `base` in the token hierarchy.
/// A derived class token includes all the bits of its base class tokens.
pub const fn is_derived_from(derived: ClassTokenType, base: ClassTokenType) -> bool {
    (derived & base) == base
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_num_final_classes() {
        // C(8,3) = 56 combinations of 3 bits from 8 bits
        assert_eq!(NUM_FINAL_CLASSES, 56);
    }

    #[test]
    fn test_k_auto_object_token_is_zero() {
        assert_eq!(TOKEN_K_AUTO_OBJECT, 0);
    }

    #[test]
    fn test_base_class_tokens_have_single_bit() {
        // KSynchronizationObject should have bit 0 set (index 0)
        assert_eq!(TOKEN_K_SYNCHRONIZATION_OBJECT, 1);
        // KReadableEvent should have bit 1 set (index 1)
        assert_eq!(TOKEN_K_READABLE_EVENT, 2);
    }

    #[test]
    fn test_derived_classes_include_base_bits() {
        // KThread derives from KSynchronizationObject -> KAutoObject
        // So it should include the KSynchronizationObject bit
        assert!(is_derived_from(TOKEN_K_THREAD, TOKEN_K_SYNCHRONIZATION_OBJECT));
        assert!(is_derived_from(TOKEN_K_THREAD, TOKEN_K_AUTO_OBJECT));

        // KProcess also derives from KSynchronizationObject
        assert!(is_derived_from(TOKEN_K_PROCESS, TOKEN_K_SYNCHRONIZATION_OBJECT));

        // KInterruptEvent derives from KReadableEvent -> KAutoObject
        assert!(is_derived_from(TOKEN_K_INTERRUPT_EVENT, TOKEN_K_READABLE_EVENT));
        assert!(is_derived_from(TOKEN_K_INTERRUPT_EVENT, TOKEN_K_AUTO_OBJECT));
    }

    #[test]
    fn test_unrelated_classes_not_derived() {
        // KThread should not be derived from KReadableEvent
        assert!(!is_derived_from(TOKEN_K_THREAD, TOKEN_K_READABLE_EVENT));
        // KInterruptEvent should not be derived from KSynchronizationObject
        assert!(!is_derived_from(
            TOKEN_K_INTERRUPT_EVENT,
            TOKEN_K_SYNCHRONIZATION_OBJECT
        ));
    }

    #[test]
    fn test_final_class_tokens_are_nonzero() {
        assert_ne!(TOKEN_K_THREAD, 0);
        assert_ne!(TOKEN_K_PROCESS, 0);
        assert_ne!(TOKEN_K_EVENT, 0);
        assert_ne!(TOKEN_K_SHARED_MEMORY, 0);
    }

    #[test]
    fn test_distinct_final_tokens() {
        // Different final classes should have different tokens
        assert_ne!(TOKEN_K_THREAD, TOKEN_K_PROCESS);
        assert_ne!(TOKEN_K_EVENT, TOKEN_K_SHARED_MEMORY);
        assert_ne!(TOKEN_K_CLIENT_SESSION, TOKEN_K_SERVER_SESSION);
    }
}
