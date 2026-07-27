// SPDX-FileCopyrightText: 2026 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Cached lookups for environment-gated diagnostics.
//!
//! These gates have no upstream counterpart and some are queried from hot
//! paths. Release builds read each variable once per call site. Test builds
//! retain live environment reads because tests legitimately change gates at
//! runtime.

/// Cached `std::env::var_os(name).is_some()`.
#[macro_export]
macro_rules! env_flag {
    ($name:literal) => {{
        #[cfg(test)]
        {
            ::std::env::var_os($name).is_some()
        }
        #[cfg(not(test))]
        {
            static FLAG: ::std::sync::OnceLock<bool> = ::std::sync::OnceLock::new();
            *FLAG.get_or_init(|| ::std::env::var_os($name).is_some())
        }
    }};
}

/// Cached `std::env::var(name).ok()`, yielding `Option<&'static str>`.
#[macro_export]
macro_rules! env_value {
    ($name:literal) => {{
        #[cfg(test)]
        {
            ::std::env::var($name)
                .ok()
                .map(|value| &*::std::boxed::Box::leak(value.into_boxed_str()))
        }
        #[cfg(not(test))]
        {
            static VALUE: ::std::sync::OnceLock<::std::option::Option<::std::string::String>> =
                ::std::sync::OnceLock::new();
            VALUE.get_or_init(|| ::std::env::var($name).ok()).as_deref()
        }
    }};
}

#[cfg(test)]
mod tests {
    #[test]
    fn absent_flag_is_false() {
        assert!(!crate::env_flag!("RUZU_ENV_FLAG_TEST_DEFINITELY_ABSENT"));
    }

    #[test]
    fn test_build_reads_environment_live() {
        fn gate() -> bool {
            crate::env_flag!("RUZU_ENV_FLAG_TEST_LIVE")
        }

        // SAFETY: this test owns this uniquely named environment variable.
        unsafe { std::env::remove_var("RUZU_ENV_FLAG_TEST_LIVE") };
        assert!(!gate());
        unsafe { std::env::set_var("RUZU_ENV_FLAG_TEST_LIVE", "1") };
        assert!(gate());
        unsafe { std::env::remove_var("RUZU_ENV_FLAG_TEST_LIVE") };
        assert!(!gate());
    }

    #[test]
    fn value_is_returned() {
        // SAFETY: this test owns this uniquely named environment variable.
        unsafe { std::env::set_var("RUZU_ENV_VALUE_TEST", "hello") };
        assert_eq!(crate::env_value!("RUZU_ENV_VALUE_TEST"), Some("hello"));
        unsafe { std::env::remove_var("RUZU_ENV_VALUE_TEST") };
    }
}
