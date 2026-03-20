// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/patch_manager.h / .cpp

use super::common_funcs::get_base_title_id;
use super::content_archive::NCA;
use super::control_metadata::{LANGUAGE_NAMES, NACP};
use super::nca_metadata::ContentRecordType;
use super::registered_cache::{get_update_title_id, ContentProvider, ContentProviderEntry};
use super::romfs::extract_romfs;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};

// ============================================================================
// Constants
// ============================================================================

const SINGLE_BYTE_MODULUS: u32 = 0x100;

/// The standard set of ExeFS file names.
/// Corresponds to upstream `EXEFS_FILE_NAMES`.
const EXEFS_FILE_NAMES: &[&str] = &[
    "main",
    "main.npdm",
    "rtld",
    "sdk",
    "subsdk0",
    "subsdk1",
    "subsdk2",
    "subsdk3",
    "subsdk4",
    "subsdk5",
    "subsdk6",
    "subsdk7",
    "subsdk8",
    "subsdk9",
];

// ============================================================================
// Title version formatting
// ============================================================================

/// Title version display format.
/// Corresponds to upstream `TitleVersionFormat`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TitleVersionFormat {
    ThreeElements,
    FourElements,
}

/// Format a title version number into a human-readable string.
/// Corresponds to upstream `FormatTitleVersion`.
fn format_title_version(mut version: u32, format: TitleVersionFormat) -> String {
    let mut bytes = [0u8; 4];
    bytes[0] = (version % SINGLE_BYTE_MODULUS) as u8;
    for i in 1..4 {
        version /= SINGLE_BYTE_MODULUS;
        bytes[i] = (version % SINGLE_BYTE_MODULUS) as u8;
    }
    match format {
        TitleVersionFormat::FourElements => {
            format!("v{}.{}.{}.{}", bytes[3], bytes[2], bytes[1], bytes[0])
        }
        TitleVersionFormat::ThreeElements => {
            format!("v{}.{}.{}", bytes[3], bytes[2], bytes[1])
        }
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Find a subdirectory case-insensitively.
/// Corresponds to upstream `FindSubdirectoryCaseless`.
fn find_subdirectory_caseless(dir: &VirtualDir, name: &str) -> Option<VirtualDir> {
    let name_lower = name.to_lowercase();
    for subdir in dir.get_subdirectories() {
        if subdir.get_name().to_lowercase() == name_lower {
            return Some(subdir);
        }
    }
    None
}

/// Append a string with comma separation if not empty.
/// Corresponds to upstream `AppendCommaIfNotEmpty`.
fn append_comma_if_not_empty(to: &mut String, with: &str) {
    if to.is_empty() {
        to.push_str(with);
    } else {
        to.push_str(", ");
        to.push_str(with);
    }
}

/// Check if a directory is valid and non-empty.
/// Corresponds to upstream `IsDirValidAndNonEmpty`.
fn is_dir_valid_and_non_empty(dir: &Option<VirtualDir>) -> bool {
    match dir {
        Some(d) => !d.get_files().is_empty() || !d.get_subdirectories().is_empty(),
        None => false,
    }
}

// ============================================================================
// PatchType, Patch
// ============================================================================

/// Patch type.
/// Corresponds to upstream `PatchType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatchType {
    Update,
    DLC,
    Mod,
}

/// A single patch entry.
/// Corresponds to upstream `Patch`.
#[derive(Debug, Clone)]
pub struct Patch {
    pub enabled: bool,
    pub name: String,
    pub version: String,
    pub patch_type: PatchType,
    pub program_id: u64,
    pub title_id: u64,
}

/// Build ID — 0x20 bytes.
pub type BuildId = [u8; 0x20];

// ============================================================================
// PatchManager
// ============================================================================

/// Centralized manager for patches to games.
/// Corresponds to upstream `PatchManager`.
///
/// NOTE: Full implementation requires FileSystemController and ContentProvider references.
/// Methods that depend on these will use the provided content_provider trait object
/// or return reasonable defaults when the fs_controller is not available.
pub struct PatchManager {
    title_id: u64,
    // In the full implementation, these would be references to the filesystem
    // controller and content provider. For now, we store an optional content
    // provider reference.
}

impl PatchManager {
    /// Create a new PatchManager.
    /// Corresponds to upstream `PatchManager::PatchManager`.
    pub fn new(title_id: u64) -> Self {
        Self { title_id }
    }

    /// Get the title ID.
    /// Corresponds to upstream `PatchManager::GetTitleID`.
    pub fn get_title_id(&self) -> u64 {
        self.title_id
    }

    /// Patch ExeFS with updates and LayeredExeFS.
    /// Corresponds to upstream `PatchManager::PatchExeFS`.
    ///
    /// Without FileSystemController, returns the input unchanged.
    pub fn patch_exefs(&self, exefs: VirtualDir) -> VirtualDir {
        log::info!("Patching ExeFS for title_id={:016X}", self.title_id);
        // TODO: Full implementation requires:
        // 1. Check disabled addons from Settings
        // 2. Apply game updates from ContentProvider
        // 3. Apply LayeredExeFS from mod directories
        // 4. Dump ExeFS if settings require it
        exefs
    }

    /// Collect IPS/IPSwitch patches from patch directories matching a build ID.
    /// Corresponds to upstream `PatchManager::CollectPatches`.
    fn collect_patches(
        &self,
        patch_dirs: &[VirtualDir],
        build_id: &str,
    ) -> Vec<VirtualFile> {
        let nso_build_id = format!("{:0<64}", build_id);
        let mut out = Vec::new();

        for subdir in patch_dirs {
            // TODO: Check disabled addons from Settings
            if let Some(exefs_dir) = find_subdirectory_caseless(subdir, "exefs") {
                for file in exefs_dir.get_files() {
                    let ext = file.get_extension();
                    if ext == "ips" {
                        let name = file.get_name();
                        let dot_pos = name.find('.').unwrap_or(name.len());
                        let this_build_id = format!("{:0<64}", &name[..dot_pos]);
                        if nso_build_id == this_build_id {
                            out.push(file);
                        }
                    } else if ext == "pchtxt" {
                        let compiler = super::ips_layer::IpSwitchCompiler::new(file.clone());
                        if !compiler.is_valid() {
                            continue;
                        }
                        let this_build_id: String = compiler
                            .get_build_id()
                            .iter()
                            .map(|b| format!("{:02x}", b))
                            .collect();
                        let this_build_id = format!("{:0<64}", this_build_id);
                        if nso_build_id == this_build_id {
                            out.push(file);
                        }
                    }
                }
            }
        }

        out
    }

    /// Patch NSO data with IPS and IPSwitch patches.
    /// Corresponds to upstream `PatchManager::PatchNSO`.
    pub fn patch_nso(&self, nso: Vec<u8>, name: &str) -> Vec<u8> {
        use super::super::loader::nso::NsoHeader;

        if nso.len() < std::mem::size_of::<NsoHeader>() {
            return nso;
        }

        let mut header = NsoHeader::default();
        let header_size = std::mem::size_of::<NsoHeader>();
        unsafe {
            std::ptr::copy_nonoverlapping(
                nso.as_ptr(),
                &mut header as *mut NsoHeader as *mut u8,
                header_size,
            );
        }

        use super::super::loader::loader::make_magic;
        if header.magic != make_magic(b'N', b'S', b'O', b'0') {
            return nso;
        }

        let build_id_raw: String = header
            .build_id
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect();
        let build_id = build_id_raw
            .trim_end_matches('0')
            .to_string();

        log::info!(
            "Patching NSO for name={}, build_id={}",
            name,
            build_id
        );

        // TODO: Full implementation requires:
        // 1. Get load_dir from FileSystemController
        // 2. Sort patch directories
        // 3. CollectPatches for the build_id
        // 4. Apply IPS patches using PatchIPS
        // 5. Apply IPSwitch patches using IPSwitchCompiler
        // 6. Dump NSO if settings require it

        nso
    }

    /// Check if PatchNSO would have any effect given the NSO's build ID.
    /// Corresponds to upstream `PatchManager::HasNSOPatch`.
    pub fn has_nso_patch(&self, _build_id: &BuildId, _name: &str) -> bool {
        // TODO: Full implementation requires FileSystemController for mod directories
        false
    }

    /// Get the list of patches available for this title.
    /// Corresponds to upstream `PatchManager::GetPatches`.
    pub fn get_patches(&self, _update_raw: Option<VirtualFile>) -> Vec<Patch> {
        if self.title_id == 0 {
            return Vec::new();
        }

        // TODO: Full implementation requires:
        // 1. ContentProvider to check for updates
        // 2. FileSystemController for mod directories
        // 3. Settings for disabled addons
        Vec::new()
    }

    /// Get the game version from update meta NCA, falling back to base.
    /// Corresponds to upstream `PatchManager::GetGameVersion`.
    pub fn get_game_version(&self) -> Option<u32> {
        // TODO: requires ContentProvider
        None
    }

    /// Parse a control NCA to extract NACP metadata and icon file.
    /// Corresponds to upstream `PatchManager::ParseControlNCA`.
    ///
    /// Returns `(Option<NACP>, Option<VirtualFile>)` — the NACP and icon file.
    pub fn parse_control_nca(&self, nca: &NCA) -> (Option<NACP>, Option<VirtualFile>) {
        let base_romfs = match nca.get_romfs() {
            Some(romfs) => romfs,
            None => return (None, None),
        };

        let romfs = self.patch_romfs(base_romfs, ContentRecordType::Control, None, true);

        let extracted = match extract_romfs(Some(romfs)) {
            Some(dir) => dir,
            None => return (None, None),
        };

        let nacp_file = extracted
            .get_file("control.nacp")
            .or_else(|| extracted.get_file("Control.nacp"));

        let nacp = nacp_file.map(|f| NACP::from_file(&f));

        // Determine icon language priority from settings.
        // Upstream reads Settings::values.language_index and converts through
        // Set::GetLanguageCodeFromIndex -> NS::ConvertToApplicationLanguage ->
        // NS::GetApplicationLanguagePriorityList to build a priority-ordered
        // language name list for icon lookup.
        use crate::hle::service::ns::language::{
            convert_to_application_language, get_application_language_priority_list,
            ApplicationLanguage, LanguageCode as NsLanguageCode,
        };

        // Map settings language_index to NS LanguageCode.
        // Upstream uses Settings::values.language_index.GetValue() which is a global.
        // PatchManager currently does not hold a reference to Settings, so we use
        // the default (index 1 = AmericanEnglish) matching upstream default behavior.
        let language_code = NsLanguageCode::EN_US;
        let application_language = convert_to_application_language(language_code)
            .unwrap_or(ApplicationLanguage::AmericanEnglish);

        let mut priority_language_names: Vec<&str> = LANGUAGE_NAMES.to_vec();
        if let Some(priority_list) = get_application_language_priority_list(application_language) {
            for i in 0..priority_language_names.len().min(priority_list.len()) {
                let language_index = priority_list[i] as u8 as usize;
                if language_index < LANGUAGE_NAMES.len() {
                    priority_language_names[i] = LANGUAGE_NAMES[language_index];
                } else {
                    log::warn!("Invalid language index {}", language_index);
                }
            }
        }

        let mut icon_file: Option<VirtualFile> = None;
        for language in &priority_language_names {
            let filename = format!("icon_{}.dat", language);
            if let Some(file) = extracted.get_file(&filename) {
                icon_file = Some(file);
                break;
            }
        }

        (nacp, icon_file)
    }

    /// Patch RomFS with updates and LayeredFS.
    /// Corresponds to upstream `PatchManager::PatchRomFS`.
    pub fn patch_romfs(
        &self,
        base_romfs: VirtualFile,
        record_type: ContentRecordType,
        _packed_update_raw: Option<VirtualFile>,
        _apply_layeredfs: bool,
    ) -> VirtualFile {
        let log_string = format!(
            "Patching RomFS for title_id={:016X}, type={:02X}",
            self.title_id, record_type as u8
        );
        if record_type == ContentRecordType::Program || record_type == ContentRecordType::Data {
            log::info!("{}", log_string);
        } else {
            log::debug!("{}", log_string);
        }

        // TODO: Full implementation requires:
        // 1. ContentProvider for game updates
        // 2. NCA parsing for update application
        // 3. LayeredFS support via mod directories
        // 4. RomFS extraction and reconstruction
        base_romfs
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_title_version_three() {
        // Version 0x00010002 = bytes [2, 0, 1, 0] -> "v0.1.0"
        assert_eq!(
            format_title_version(0x00010002, TitleVersionFormat::ThreeElements),
            "v0.1.0"
        );
    }

    #[test]
    fn test_format_title_version_four() {
        assert_eq!(
            format_title_version(0x01020304, TitleVersionFormat::FourElements),
            "v1.2.3.4"
        );
    }

    #[test]
    fn test_format_title_version_zero() {
        assert_eq!(
            format_title_version(0, TitleVersionFormat::ThreeElements),
            "v0.0.0"
        );
    }

    #[test]
    fn test_append_comma_if_not_empty() {
        let mut s = String::new();
        append_comma_if_not_empty(&mut s, "IPS");
        assert_eq!(s, "IPS");
        append_comma_if_not_empty(&mut s, "LayeredFS");
        assert_eq!(s, "IPS, LayeredFS");
    }

    #[test]
    fn test_patch_manager_get_title_id() {
        let pm = PatchManager::new(0x0100000000001000);
        assert_eq!(pm.get_title_id(), 0x0100000000001000);
    }

    #[test]
    fn test_get_patches_zero_title() {
        let pm = PatchManager::new(0);
        assert!(pm.get_patches(None).is_empty());
    }
}
