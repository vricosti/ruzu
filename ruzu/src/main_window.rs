// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of the upstream `GMainWindow` class defined in
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/main.cpp` + `main.h`, whose
// widget tree is described declaratively in `main.ui`.
//
// The upstream window layout is:
//   * a `QMenuBar` (menubar) with six top-level menus: File, Emulation, View,
//     Tools, Multiplayer, Help;
//   * a central widget hosting the game list;
//   * a `QStatusBar` with a message label plus permanent status widgets.
//
// This module reproduces that structure with GTK4. The menu *actions* are
// registered as stubs (they log when triggered) so the menus are visible and
// selectable but not yet wired to real behaviour — matching the current
// milestone: "build the main window with un-wired menus".

use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};

use gtk::prelude::*;
use gtk::{gio, glib, Application, ApplicationWindow};

use common::settings_enums::ConfirmStop;
use input_common::drivers::mouse::MouseButton;
use ruzu_core::frontend::framebuffer_layout::{default_frame_layout, FramebufferLayout};

use crate::boot::{EmulationSession, LoadingEvent};
use crate::loading_screen::{LoadStage, LoadingScreen};
use crate::status_bar::StatusBar;

/// Names of the pages held by the central [`gtk::Stack`]. Upstream swaps the
/// central widget between the game list, the loading screen, and the render
/// window; the stack reproduces that.
const PAGE_GAME_LIST: &str = "game_list";
const PAGE_LOADING: &str = "loading";
/// Black backdrop shown behind the native render window while a game runs, so a
/// window resize briefly exposes black (matching the render area) instead of the
/// light loading-screen page.
const PAGE_RENDER: &str = "render";

/// Default window geometry, mirroring `main.ui` (`1280 x 720`).
const DEFAULT_WIDTH: i32 = 1280;
const DEFAULT_HEIGHT: i32 = 720;

/// Window title. Upstream uses "yuzu"; adapted to the ruzu app name.
const WINDOW_TITLE: &str = "ruzu";

/// Upstream `default_input_update_timeout`, the interval of `update_input_timer`.
const INPUT_UPDATE_TIMEOUT_MS: u64 = 1;
/// Upstream `status_bar_update_timer` interval.
const STATUS_BAR_UPDATE_TIMEOUT_MS: u64 = 500;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FullscreenHotkey {
    Toggle,
    Exit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StopConfirmation {
    None,
    ChangeGame,
    ForceLockedExit,
}

fn stop_confirmation(setting: ConfirmStop, exit_locked: bool) -> StopConfirmation {
    match (setting, exit_locked) {
        (ConfirmStop::AskAlways, false) => StopConfirmation::ChangeGame,
        (ConfirmStop::AskAlways | ConfirmStop::AskBasedOnGame, true) => {
            StopConfirmation::ForceLockedExit
        }
        (ConfirmStop::AskBasedOnGame | ConfirmStop::AskNever, false)
        | (ConfirmStop::AskNever, true) => StopConfirmation::None,
    }
}

fn fullscreen_hotkey(keyval: gtk::gdk::Key) -> Option<FullscreenHotkey> {
    match keyval {
        gtk::gdk::Key::F11 => Some(FullscreenHotkey::Toggle),
        gtk::gdk::Key::Escape => Some(FullscreenHotkey::Exit),
        _ => None,
    }
}

/// The main launcher window.
///
/// Upstream `GMainWindow` derives from `QMainWindow`; here we wrap a
/// `gtk::ApplicationWindow`. Kept as a thin newtype so future state (game list
/// model, status labels, emulation handles) can hang off it the way the
/// upstream class members do.
pub struct GMainWindow {
    window: ApplicationWindow,
    /// In-window menu bar on non-macOS platforms. Upstream hides the menu bar
    /// while the single-window render surface is fullscreen.
    menu_bar: Option<gtk::PopoverMenuBar>,
    /// Central stack swapping between the game list, loading screen, and render
    /// view (upstream swaps `centralwidget`).
    stack: gtk::Stack,
    /// Shader-loading progress UI (upstream `LoadingScreen`).
    loading_screen: Rc<LoadingScreen>,
    /// The active emulation session, if a game is running (upstream keeps the
    /// `System` + emu thread on `GMainWindow`).
    session: RefCell<Option<EmulationSession>>,
    /// GTK close requests are asynchronous when confirmation is required.
    /// These flags prevent duplicate dialogs and let the accepted request pass
    /// through the close handler exactly once.
    close_confirmation_pending: Cell<bool>,
    close_confirmed: Cell<bool>,
    /// Prevent duplicate asynchronous `ConfirmShutdownGame` dialogs.
    stop_confirmation_pending: Cell<bool>,
    /// Invalidates the previous session's GTK event poller when another title
    /// is booted before that poller receives a terminal event.
    session_generation: Cell<u64>,
    /// Bottom status bar (renderer / accuracy / dock / filter / AA / volume).
    status_bar: Rc<StatusBar>,
    /// Native render-window handles for the running game, so it can be resized
    /// when the GTK window resizes.
    render: RefCell<Option<RenderHandles>>,
    /// Last observed central-stack size, to detect resizes.
    render_size: Cell<(i32, i32)>,
    /// The open configuration dialog, kept alive while it is on screen
    /// (upstream holds `ConfigureDialog` on the stack across `exec()`).
    configure_dialog: RefCell<Option<Rc<crate::configuration::ConfigureDialog>>>,
    /// Handle to the game list, so it can be rescanned when the configured
    /// directories change.
    game_list: RefCell<Option<crate::game_list::GameListHandle>>,
    /// Input drivers. Upstream `GMainWindow` owns the `InputSubsystem` and
    /// passes it to `GRenderWindow`, which forwards events into it.
    ///
    /// It must stay alive for the whole session: `Initialize` registers each
    /// engine in the process-wide factory registry that the emulated HID
    /// resolves bindings through, and `Shutdown` unregisters them.
    input_subsystem: Rc<RefCell<input_common::InputSubsystem>>,
    /// The single HID core shared by configuration and the emulation System.
    ///
    /// Upstream owner: `Core::System::Impl::hid_core`.
    hid_core: Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
}

/// Handles needed to resize the embedded render surface on window resize.
struct RenderHandles {
    /// macOS: child `NSWindow*`; Linux: X11 `Window`; Windows: child `HWND`.
    child_window: usize,
    /// macOS: the `CAMetalLayer*`.
    #[cfg(target_os = "macos")]
    metal_layer: usize,
    /// Linux: the X11 `Display*`.
    #[cfg(target_os = "linux")]
    display: usize,
    /// Shared frame layout the renderer reads; updated so the frame is rendered
    /// at the new native resolution on resize (upstream `OnFramebufferSizeChanged`).
    framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct RenderPointerPosition {
    mouse_x: i32,
    mouse_y: i32,
    center_x: i32,
    center_y: i32,
    touch_x: f32,
    touch_y: f32,
}

/// Convert render-widget coordinates to the mouse and touchscreen coordinate
/// spaces used by upstream `GRenderWindow`.
fn map_render_pointer(
    local_x: f64,
    local_y: f64,
    logical_width: f64,
    logical_height: f64,
    layout: &FramebufferLayout,
) -> Option<RenderPointerPosition> {
    if logical_width <= 0.0
        || logical_height <= 0.0
        || layout.width == 0
        || layout.height == 0
        || layout.screen.right <= layout.screen.left
        || layout.screen.bottom <= layout.screen.top
    {
        return None;
    }

    // Upstream keeps mouse coordinates in widget pixels, but ScaleTouch first
    // applies the device-pixel ratio before MapToTouchScreen. Deriving the
    // scale from the live framebuffer layout also handles fractional GTK
    // allocations without drifting from the drawable size.
    let mouse_x = local_x.round().max(0.0) as i32;
    let mouse_y = local_y.round().max(0.0) as i32;
    let framebuffer_x = (local_x * f64::from(layout.width) / logical_width)
        .round()
        .max(0.0) as u32;
    let framebuffer_y = (local_y * f64::from(layout.height) / logical_height)
        .round()
        .max(0.0) as u32;

    let clipped_x = framebuffer_x.clamp(layout.screen.left, layout.screen.right - 1);
    let clipped_y = framebuffer_y.clamp(layout.screen.top, layout.screen.bottom - 1);
    let touch_x =
        (clipped_x - layout.screen.left) as f32 / (layout.screen.right - layout.screen.left) as f32;
    let touch_y =
        (clipped_y - layout.screen.top) as f32 / (layout.screen.bottom - layout.screen.top) as f32;

    Some(RenderPointerPosition {
        mouse_x,
        mouse_y,
        center_x: (logical_width / 2.0) as i32,
        center_y: (logical_height / 2.0) as i32,
        touch_x,
        touch_y,
    })
}

#[derive(Default)]
struct LoadingEventMailbox {
    events: VecDeque<LoadingEvent>,
}

impl LoadingEventMailbox {
    fn push(&mut self, event: LoadingEvent) {
        match event {
            LoadingEvent::Progress {
                stage: LoadStage::Build,
                value,
                total,
            } => {
                if let Some(LoadingEvent::Progress {
                    stage: LoadStage::Build,
                    value: queued_value,
                    total: queued_total,
                }) = self.events.back_mut()
                {
                    *queued_value = value;
                    *queued_total = total;
                } else {
                    self.events.push_back(LoadingEvent::Progress {
                        stage: LoadStage::Build,
                        value,
                        total,
                    });
                }
            }
            LoadingEvent::Assets(assets) => {
                if let Some(LoadingEvent::Assets(queued_assets)) = self.events.back_mut() {
                    *queued_assets = assets;
                } else {
                    self.events.push_back(LoadingEvent::Assets(assets));
                }
            }
            LoadingEvent::FirstFrame => {
                if !self
                    .events
                    .iter()
                    .any(|event| matches!(event, LoadingEvent::FirstFrame))
                {
                    self.events.push_back(LoadingEvent::FirstFrame);
                }
            }
            event => self.events.push_back(event),
        }
    }

    fn pop(&mut self) -> Option<LoadingEvent> {
        self.events.pop_front()
    }
}

#[cfg(test)]
mod loading_event_mailbox_tests {
    use super::*;

    #[test]
    fn build_updates_are_coalesced_without_losing_stage_transitions() {
        let mut mailbox = LoadingEventMailbox::default();
        mailbox.push(LoadingEvent::Progress {
            stage: LoadStage::Prepare,
            value: 0,
            total: 0,
        });
        for value in 0..=929 {
            mailbox.push(LoadingEvent::Progress {
                stage: LoadStage::Build,
                value,
                total: 929,
            });
        }
        mailbox.push(LoadingEvent::Progress {
            stage: LoadStage::Complete,
            value: 0,
            total: 0,
        });
        mailbox.push(LoadingEvent::FirstFrame);
        mailbox.push(LoadingEvent::FirstFrame);

        assert!(matches!(
            mailbox.pop(),
            Some(LoadingEvent::Progress {
                stage: LoadStage::Prepare,
                ..
            })
        ));
        assert!(matches!(
            mailbox.pop(),
            Some(LoadingEvent::Progress {
                stage: LoadStage::Build,
                value: 929,
                total: 929,
            })
        ));
        assert!(matches!(
            mailbox.pop(),
            Some(LoadingEvent::Progress {
                stage: LoadStage::Complete,
                ..
            })
        ));
        assert!(matches!(mailbox.pop(), Some(LoadingEvent::FirstFrame)));
        assert!(mailbox.pop().is_none());
    }

    #[test]
    fn guest_exit_is_preserved_after_the_first_frame() {
        let mut mailbox = LoadingEventMailbox::default();
        mailbox.push(LoadingEvent::FirstFrame);
        mailbox.push(LoadingEvent::Stopped {
            before_first_frame: false,
        });

        assert!(matches!(mailbox.pop(), Some(LoadingEvent::FirstFrame)));
        assert!(matches!(
            mailbox.pop(),
            Some(LoadingEvent::Stopped {
                before_first_frame: false
            })
        ));
    }

    #[test]
    fn requested_stop_completion_is_preserved() {
        let mut mailbox = LoadingEventMailbox::default();
        mailbox.push(LoadingEvent::StopComplete);

        assert!(matches!(mailbox.pop(), Some(LoadingEvent::StopComplete)));
    }
}

#[cfg(test)]
mod fullscreen_hotkey_tests {
    use super::*;

    #[test]
    fn classifies_upstream_fullscreen_shortcuts() {
        assert_eq!(
            fullscreen_hotkey(gtk::gdk::Key::F11),
            Some(FullscreenHotkey::Toggle)
        );
        assert_eq!(
            fullscreen_hotkey(gtk::gdk::Key::Escape),
            Some(FullscreenHotkey::Exit)
        );
        assert_eq!(fullscreen_hotkey(gtk::gdk::Key::F10), None);
    }
}

#[cfg(test)]
mod stop_confirmation_tests {
    use super::*;

    #[test]
    fn follows_upstream_confirm_shutdown_policy() {
        assert_eq!(
            stop_confirmation(ConfirmStop::AskAlways, false),
            StopConfirmation::ChangeGame
        );
        assert_eq!(
            stop_confirmation(ConfirmStop::AskAlways, true),
            StopConfirmation::ForceLockedExit
        );
        assert_eq!(
            stop_confirmation(ConfirmStop::AskBasedOnGame, false),
            StopConfirmation::None
        );
        assert_eq!(
            stop_confirmation(ConfirmStop::AskBasedOnGame, true),
            StopConfirmation::ForceLockedExit
        );
        assert_eq!(
            stop_confirmation(ConfirmStop::AskNever, false),
            StopConfirmation::None
        );
        assert_eq!(
            stop_confirmation(ConfirmStop::AskNever, true),
            StopConfirmation::None
        );
    }
}

#[cfg(test)]
mod render_pointer_tests {
    use super::*;
    use ruzu_core::frontend::framebuffer_layout::Rectangle;

    #[test]
    fn maps_center_of_render_area_to_center_of_touchscreen() {
        let layout = default_frame_layout(1280, 720);
        let position = map_render_pointer(640.0, 360.0, 1280.0, 720.0, &layout).unwrap();

        assert_eq!((position.mouse_x, position.mouse_y), (640, 360));
        assert_eq!((position.center_x, position.center_y), (640, 360));
        assert!((position.touch_x - 0.5).abs() < f32::EPSILON);
        assert!((position.touch_y - 0.5).abs() < f32::EPSILON);
    }

    #[test]
    fn clips_letterbox_coordinates_like_upstream_map_to_touch_screen() {
        let layout = FramebufferLayout {
            width: 1024,
            height: 768,
            screen: Rectangle::new(0, 96, 1024, 672),
            is_srgb: false,
        };

        let top = map_render_pointer(512.0, 0.0, 1024.0, 768.0, &layout).unwrap();
        let bottom = map_render_pointer(512.0, 767.0, 1024.0, 768.0, &layout).unwrap();

        assert_eq!(top.touch_y, 0.0);
        assert_eq!(bottom.touch_y, 575.0 / 576.0);
    }

    #[test]
    fn scales_touch_coordinates_to_physical_framebuffer() {
        let layout = default_frame_layout(2560, 1440);
        let position = map_render_pointer(320.0, 180.0, 1280.0, 720.0, &layout).unwrap();

        assert_eq!((position.mouse_x, position.mouse_y), (320, 180));
        assert_eq!(position.touch_x, 0.25);
        assert_eq!(position.touch_y, 0.25);
    }

    #[test]
    fn controller_bindings_use_upstream_qt_key_codes() {
        use gtk::gdk::Key;

        assert_eq!(gdk_key_to_qt_key(Key::Left), 16_777_234);
        assert_eq!(gdk_key_to_qt_key(Key::Up), 16_777_235);
        assert_eq!(gdk_key_to_qt_key(Key::Right), 16_777_236);
        assert_eq!(gdk_key_to_qt_key(Key::Down), 16_777_237);
        assert_eq!(gdk_key_to_qt_key(Key::space), 32);
        assert_eq!(gdk_key_to_qt_key(Key::w), i32::from(b'W'));
        assert_eq!(gdk_key_to_qt_key(Key::C), i32::from(b'C'));
    }
}

impl GMainWindow {
    /// Construct and lay out the main window. Mirrors the body of the upstream
    /// `GMainWindow::GMainWindow` constructor (widget creation + `Initialize*`
    /// calls), minus the not-yet-ported subsystems.
    pub fn new(app: &Application) -> Rc<Self> {
        Self::new_with_config_import_offer(app, true)
    }

    /// Construct a window for a game supplied directly on the command line.
    ///
    /// The first-run configuration-import question must not cover a game that
    /// is already booting in the background. A later launcher-only start can
    /// still offer that one-time import.
    pub fn new_for_direct_game(app: &Application) -> Rc<Self> {
        Self::new_with_config_import_offer(app, false)
    }

    fn new_with_config_import_offer(app: &Application, offer_config_import: bool) -> Rc<Self> {
        let window = ApplicationWindow::builder()
            .application(app)
            .title(WINDOW_TITLE)
            .default_width(DEFAULT_WIDTH)
            .default_height(DEFAULT_HEIGHT)
            .build();

        // Root vertical layout. On macOS the menu bar lives in the native
        // global menu bar (installed once via `init_app_menu` on the
        // application's `startup`), so the window itself only holds the central
        // stack and the status bar. Every other platform has no global menu
        // bar, so the same `GMenuModel` is rendered in-window as a
        // `PopoverMenuBar` — the position upstream's `QMenuBar` occupies.
        let root = gtk::Box::new(gtk::Orientation::Vertical, 0);

        #[cfg(not(target_os = "macos"))]
        let menu_bar = {
            let menubar = gtk::PopoverMenuBar::from_model(Some(&build_menu_model()));
            menubar.set_halign(gtk::Align::Start);
            root.append(&menubar);
            Some(menubar)
        };
        #[cfg(target_os = "macos")]
        let menu_bar = None;

        // --- Central stack (upstream `centralwidget`) ------------------------
        // Pages: game list, loading screen, (later) render view.
        let stack = gtk::Stack::new();
        stack.set_hexpand(true);
        stack.set_vexpand(true);

        let loading_screen = Rc::new(LoadingScreen::new());
        stack.add_named(loading_screen.widget(), Some(PAGE_LOADING));

        // Black backdrop page (behind the native render window during a game).
        install_render_bg_css();
        let render_bg = gtk::Box::new(gtk::Orientation::Vertical, 0);
        render_bg.add_css_class("ruzu-render-bg");
        render_bg.set_hexpand(true);
        render_bg.set_vexpand(true);
        stack.add_named(&render_bg, Some(PAGE_RENDER));

        // The game list page (PAGE_GAME_LIST) is added after `this` exists so its
        // row-activate handler can boot via the window; see below.
        root.append(&stack);

        // --- Status bar (upstream `QStatusBar`) -------------------------------
        let status_bar = StatusBar::new();
        root.append(status_bar.widget());

        window.set_child(Some(&root));

        // Proof path for the embedded render surface: when RUZU_EMBED_METAL=1,
        // attach a CAMetalLayer to the window's NSView once realized (see
        // `render_window`). It is opt-in because the Metal child view covers the
        // GTK UI; the boot layer will attach it only when presentation begins.
        #[cfg(target_os = "macos")]
        if std::env::var_os("RUZU_EMBED_METAL").is_some() {
            window.connect_map(|window| {
                match crate::render_window::attach_metal_layer(
                    window.upcast_ref::<gtk::Window>(),
                    None,
                ) {
                    Some(layer) => {
                        // Reveal immediately so the (red) child window is visible
                        // for the embed test.
                        crate::render_window::set_render_view_hidden(layer.child_window, false);
                        log::info!("Embedded CAMetalLayer at {:p}", layer.metal_layer);
                    }
                    None => log::warn!("Failed to embed CAMetalLayer into the window"),
                }
            });
        }

        let this = Rc::new(Self {
            window,
            menu_bar,
            stack,
            loading_screen,
            session: RefCell::new(None),
            close_confirmation_pending: Cell::new(false),
            close_confirmed: Cell::new(false),
            stop_confirmation_pending: Cell::new(false),
            session_generation: Cell::new(0),
            status_bar,
            render: RefCell::new(None),
            render_size: Cell::new((0, 0)),
            configure_dialog: RefCell::new(None),
            game_list: RefCell::new(None),
            input_subsystem: Rc::new(RefCell::new(input_common::InputSubsystem::new())),
            hid_core: Arc::new(parking_lot::Mutex::new(hid_core::hid_core::HIDCore::new())),
        });

        // Upstream calls `input_subsystem->Initialize()` from the
        // `GRenderWindow` constructor. Do it once here, before any boot, so the
        // engines are registered when the guest starts reading its controllers.
        this.input_subsystem.borrow_mut().initialize();
        this.hid_core.lock().reload_input_devices();
        this.install_input_handlers();
        this.start_input_driver_updates();
        this.start_status_bar_updates();

        // Game list page: activating a row boots that game in-process.
        let (game_list, game_list_handle) = crate::game_list::build(glib::clone!(
            #[weak(rename_to = w)]
            this,
            #[upgrade_or_default]
            move |path: String| w.boot_game(path)
        ));
        this.stack.add_named(&game_list, Some(PAGE_GAME_LIST));
        this.stack.set_visible_child_name(PAGE_GAME_LIST);
        *this.game_list.borrow_mut() = Some(game_list_handle);

        // First run with an importable yuzu configuration: ask before copying
        // anything. Deferred to an idle callback so the window is on screen
        // behind the dialog rather than appearing after it.
        if offer_config_import {
            let this = Rc::clone(&this);
            glib::idle_add_local_once(move || this.maybe_offer_yuzu_import());
        }

        // Keep the embedded render surface sized to the central stack as the
        // window is resized. GTK4 has no widget `size-allocate` signal, so poll
        // the stack size on the frame clock and act only on change.
        #[cfg(any(target_os = "macos", target_os = "linux", target_os = "windows"))]
        this.stack.add_tick_callback(glib::clone!(
            #[weak(rename_to = w)]
            this,
            #[upgrade_or]
            glib::ControlFlow::Break,
            move |_, _| {
                w.maybe_resize_render();
                glib::ControlFlow::Continue
            }
        ));

        // Wire the window-dependent File actions to in-process boot, overriding
        // the startup stubs (`g_action_map_add_action` replaces by name). These
        // need the window (render surface, loading screen, stack) so they live
        // here rather than in the app-startup registration.
        this.register_boot_actions(app);
        this.register_fullscreen_actions(app);

        // Keep the checkable menu action and the window chrome synchronized
        // when the compositor exits fullscreen independently.
        this.window.connect_fullscreened_notify(glib::clone!(
            #[weak(rename_to = this)]
            this,
            #[weak]
            app,
            move |window| {
                let fullscreen = window.is_fullscreen();
                this.set_fullscreen_action_state(&app, fullscreen);
                crate::uisettings::with_mut(|values| values.fullscreen.set_value(fullscreen));
                this.update_fullscreen_chrome(fullscreen);
            }
        ));

        // Confirm while a title is active, then stop emulation before GTK tears
        // down the native surface. This mirrors upstream `ConfirmClose()` and
        // `closeEvent()`.
        this.window.connect_close_request(glib::clone!(
            #[weak(rename_to = w)]
            this,
            #[upgrade_or]
            glib::Propagation::Proceed,
            move |_| {
                log::debug!(
                    "GTK close request: session_active={} confirmed={} pending={}",
                    w.session.borrow().is_some(),
                    w.close_confirmed.get(),
                    w.close_confirmation_pending.get()
                );
                if w.session.borrow().is_none() {
                    return glib::Propagation::Proceed;
                }

                if w.close_confirmed.replace(false) {
                    if let Some(mut session) = w.session.borrow_mut().take() {
                        session.stop();
                    }
                    return glib::Propagation::Proceed;
                }

                if w.close_confirmation_pending.replace(true) {
                    return glib::Propagation::Stop;
                }

                crate::gtk_compat::ask_question(
                    Some(&w.window),
                    "ruzu",
                    "Are you sure you want to close ruzu?",
                    "Cancel",
                    "Close ruzu",
                    glib::clone!(
                        #[weak(rename_to = w)]
                        w,
                        move |accepted| {
                            log::debug!("GTK close confirmation answered: {accepted}");
                            w.close_confirmation_pending.set(false);
                            if accepted {
                                w.close_confirmed.set(true);
                                w.window.close();
                            }
                        }
                    ),
                );

                glib::Propagation::Stop
            }
        ));

        // Optional demo of the loading screen UI: RUZU_DEMO_LOADING=1 shows the
        // loading page and animates fake shader-build progress so the widget can
        // be exercised without a running game.
        if std::env::var_os("RUZU_DEMO_LOADING").is_some() {
            this.start_loading_demo();
        }

        this
    }

    /// Register `load_file` / `load_folder` as window-aware actions that boot a
    /// game in-process. Mirrors upstream `connect_menu(action_Load_File,
    /// OnMenuLoadFile)` etc., but the handler lives on the window.
    fn register_boot_actions(self: &Rc<Self>, app: &Application) {
        let load_file = gio::SimpleAction::new("load_file", None);
        load_file.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, _| this.on_menu_load_file()
        ));
        app.add_action(&load_file);

        let load_folder = gio::SimpleAction::new("load_folder", None);
        load_folder.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, _| this.on_menu_load_folder()
        ));
        app.add_action(&load_folder);

        // Upstream `connect_menu(action_Stop, OnStopGame)` and the default
        // "Stop Emulation" hotkey.
        let stop = gio::SimpleAction::new("stop", None);
        stop.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, _| this.on_stop_game()
        ));
        app.add_action(&stop);
        app.set_accels_for_action("app.stop", &["F5"]);

        // Upstream `connect_menu(action_Configure, OnConfigure)`. Overrides the
        // startup stub, since the dialog is parented to this window.
        let configure = gio::SimpleAction::new("configure", None);
        configure.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, _| this.on_configure()
        ));
        app.add_action(&configure);

        // The macOS App menu's "Preferences" opens the same dialog upstream.
        let preferences = gio::SimpleAction::new("preferences", None);
        preferences.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, _| this.on_configure()
        ));
        app.add_action(&preferences);

        // Tools menu — upstream `connect_menu(action_Install_Keys, …)` etc.
        macro_rules! window_action {
            ($name:literal, $handler:ident) => {{
                let action = gio::SimpleAction::new($name, None);
                action.connect_activate(glib::clone!(
                    #[weak(rename_to = this)]
                    self,
                    move |_, _| this.$handler()
                ));
                app.add_action(&action);
            }};
        }
        window_action!("install_keys", on_install_decryption_keys);
        window_action!("install_firmware", on_install_firmware);
        window_action!("verify_installed_contents", on_verify_installed_contents);

        // The blocks above replace the startup stubs by name, which resets
        // their enabled state; re-apply it (upstream re-runs `UpdateMenuState`
        // after `ConnectMenuEvents`).
        update_menu_state(app, self.session.borrow().is_some(), true);
    }

    /// Register upstream's checkable `View > Fullscreen` action and its two
    /// hotkeys: `F11` toggles it, while `Esc` only exits fullscreen.
    fn register_fullscreen_actions(self: &Rc<Self>, app: &Application) {
        let initially_checked = crate::uisettings::with(|values| *values.fullscreen.get_value());
        let fullscreen =
            gio::SimpleAction::new_stateful("fullscreen", None, &initially_checked.to_variant());
        fullscreen.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            #[weak]
            app,
            move |_, _| this.toggle_fullscreen(&app)
        ));
        app.add_action(&fullscreen);
        app.set_accels_for_action("app.fullscreen", &["F11"]);

        let exit_fullscreen = gio::SimpleAction::new("exit_fullscreen", None);
        exit_fullscreen.connect_activate(glib::clone!(
            #[weak(rename_to = this)]
            self,
            #[weak]
            app,
            move |_, _| {
                this.exit_fullscreen(&app);
            }
        ));
        app.add_action(&exit_fullscreen);
        app.set_accels_for_action("app.exit_fullscreen", &["Escape"]);
    }

    fn toggle_fullscreen(&self, app: &Application) {
        let checked = !app
            .lookup_action("fullscreen")
            .and_then(|action| action.state())
            .and_then(|state| state.get::<bool>())
            .unwrap_or(false);
        self.set_fullscreen_action_state(app, checked);
        crate::uisettings::with_mut(|values| values.fullscreen.set_value(checked));

        // Upstream leaves the action checked when no game is running, but
        // ToggleFullscreen returns without changing the launcher window.
        if self.session.borrow().is_some() {
            self.set_fullscreen(checked);
        }
    }

    fn exit_fullscreen(&self, app: &Application) {
        if self.session.borrow().is_some() && self.window.is_fullscreen() {
            self.set_fullscreen_action_state(app, false);
            crate::uisettings::with_mut(|values| values.fullscreen.set_value(false));
            self.set_fullscreen(false);
        }
    }

    /// Upstream `GMainWindow::ToggleFullscreen` / `ShowFullscreen` /
    /// `HideFullscreen`, adapted to ruzu's always-single-window GTK frontend.
    fn set_fullscreen(&self, fullscreen: bool) {
        self.update_fullscreen_chrome(fullscreen);
        self.window.set_fullscreened(fullscreen);
    }

    fn update_fullscreen_chrome(&self, fullscreen: bool) {
        if let Some(menu_bar) = self.menu_bar.as_ref() {
            menu_bar.set_visible(!fullscreen);
        }
        let show_status_bar = crate::uisettings::with(|values| *values.show_status_bar.get_value());
        self.status_bar
            .widget()
            .set_visible(!fullscreen && show_status_bar);
    }

    fn set_fullscreen_action_state(&self, app: &Application, fullscreen: bool) {
        if let Some(action) = app
            .lookup_action("fullscreen")
            .and_downcast::<gio::SimpleAction>()
        {
            action.set_state(&fullscreen.to_variant());
        }
    }

    /// Forward keyboard and mouse events into the input subsystem.
    ///
    /// Transposition of `GRenderWindow::keyPressEvent` / `keyReleaseEvent` /
    /// `mousePressEvent` etc. (`zuyu/src/yuzu/bootmanager.cpp`). Upstream does
    /// three things per key:
    ///
    /// ```cpp
    /// input_subsystem->GetKeyboard()->SetKeyboardModifiers(modifier);
    /// input_subsystem->GetKeyboard()->PressKeyboardKey(key);   // emulated USB keyboard
    /// input_subsystem->GetKeyboard()->PressKey(event->key());  // controller bindings
    /// ```
    ///
    /// The last call is the one that drives gamepad buttons, resolved through
    /// the `engine:keyboard` bindings in `Settings::values.players`.
    ///
    /// GTK keyvals are converted to Qt's key space before reaching the
    /// controller-binding path. Settings store those Qt values upstream, so
    /// preserving them keeps imported bindings and newly captured bindings in
    /// the same key space.
    fn install_input_handlers(self: &Rc<Self>) {
        let keys = gtk::EventControllerKey::new();
        // Capture, so a key bound to a game control is not first swallowed by a
        // focused widget in the launcher chrome.
        keys.set_propagation_phase(gtk::PropagationPhase::Capture);

        keys.connect_key_pressed(glib::clone!(
            #[weak(rename_to = this)]
            self,
            #[upgrade_or]
            glib::Propagation::Proceed,
            move |_, keyval, _keycode, state| {
                if this.session.borrow().is_some() {
                    if keyval == gtk::gdk::Key::F5 {
                        this.on_stop_game();
                        return glib::Propagation::Stop;
                    }
                    if let Some(hotkey) = fullscreen_hotkey(keyval) {
                        if let Some(app) = this.window.application() {
                            match hotkey {
                                FullscreenHotkey::Toggle => this.toggle_fullscreen(&app),
                                FullscreenHotkey::Exit => this.exit_fullscreen(&app),
                            }
                        }
                        return glib::Propagation::Stop;
                    }
                }

                this.on_key_event(keyval, state, true);
                // Upstream delivers gameplay keys to the focused
                // GRenderWindow, whose keyPressEvent does not forward them to
                // the launcher chrome. Capture at the GTK toplevel because the
                // native render child is not a GTK widget, then preserve that
                // ownership by stopping propagation while emulation is active.
                if this.session.borrow().is_some() {
                    glib::Propagation::Stop
                } else {
                    glib::Propagation::Proceed
                }
            }
        ));
        keys.connect_key_released(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, keyval, _keycode, state| {
                if this.session.borrow().is_some()
                    && (keyval == gtk::gdk::Key::F5 || fullscreen_hotkey(keyval).is_some())
                {
                    return;
                }
                this.on_key_event(keyval, state, false);
            }
        ));
        self.window.add_controller(keys);

        let clicks = gtk::GestureClick::new();
        // Zero asks GTK to report every mouse button. This mirrors upstream's
        // QtButtonToMouseButton dispatch instead of recognizing only primary
        // clicks.
        clicks.set_button(0);
        clicks.set_propagation_phase(gtk::PropagationPhase::Capture);
        clicks.connect_pressed(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |gesture, _press_count, x, y| {
                this.on_mouse_button_pressed(gesture.current_button(), x, y);
            }
        ));
        clicks.connect_released(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |gesture, _press_count, _x, _y| {
                this.on_mouse_button_released(gesture.current_button());
            }
        ));
        self.window.add_controller(clicks);

        let motion = gtk::EventControllerMotion::new();
        motion.set_propagation_phase(gtk::PropagationPhase::Capture);
        motion.connect_motion(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_, x, y| {
                this.on_mouse_motion(x, y);
            }
        ));
        self.window.add_controller(motion);

        let scroll = gtk::EventControllerScroll::new(
            gtk::EventControllerScrollFlags::BOTH_AXES | gtk::EventControllerScrollFlags::DISCRETE,
        );
        scroll.set_propagation_phase(gtk::PropagationPhase::Capture);
        scroll.connect_scroll(glib::clone!(
            #[weak(rename_to = this)]
            self,
            #[upgrade_or]
            glib::Propagation::Proceed,
            move |_, dx, dy| {
                this.on_mouse_wheel(dx, dy);
                glib::Propagation::Proceed
            }
        ));
        self.window.add_controller(scroll);
    }

    /// One key press or release — upstream's `keyPressEvent` / `keyReleaseEvent`.
    fn on_key_event(&self, keyval: gtk::gdk::Key, state: gtk::gdk::ModifierType, pressed: bool) {
        let subsystem = self.input_subsystem.borrow();
        let Some(keyboard) = subsystem.get_keyboard() else {
            return;
        };

        keyboard.set_keyboard_modifiers(switch_modifiers(state));

        // The emulated USB keyboard, for games that read one directly.
        let switch_key = gdk_key_to_switch_key(keyval);
        if pressed {
            keyboard.press_keyboard_key(switch_key);
        } else {
            keyboard.release_keyboard_key(switch_key);
        }

        // The controller-binding path uses Qt key codes in upstream settings.
        let code = gdk_key_to_qt_key(keyval);
        if pressed {
            keyboard.press_key(code);
        } else {
            keyboard.release_key(code);
        }
    }

    /// Resolve a toplevel GTK pointer position into the embedded render
    /// surface. Upstream receives these coordinates directly because
    /// `GRenderWindow` is a QWidget; GTK's native X11 child is not a widget, so
    /// the toplevel handler performs the equivalent bounds translation.
    fn render_pointer_position(
        &self,
        window_x: f64,
        window_y: f64,
    ) -> Option<RenderPointerPosition> {
        if self.stack.visible_child_name().as_deref() != Some(PAGE_RENDER) {
            return None;
        }
        let rect = self.stack.compute_bounds(&self.window)?;
        let local_x = window_x - f64::from(rect.x());
        let local_y = window_y - f64::from(rect.y());
        let width = f64::from(rect.width());
        let height = f64::from(rect.height());
        if local_x < 0.0 || local_y < 0.0 || local_x >= width || local_y >= height {
            return None;
        }

        let render = self.render.borrow();
        let layout = render.as_ref()?.framebuffer_layout.read().ok()?;
        map_render_pointer(local_x, local_y, width, height, &layout)
    }

    /// Upstream `GRenderWindow::mousePressEvent`.
    fn on_mouse_button_pressed(&self, button: u32, x: f64, y: f64) {
        let Some(position) = self.render_pointer_position(x, y) else {
            return;
        };
        let button = gdk_button_to_mouse_button(button);
        let mut subsystem = self.input_subsystem.borrow_mut();
        let Some(mouse) = subsystem.get_mouse_mut() else {
            return;
        };

        mouse.press_mouse_button(button);
        mouse.press_button(position.mouse_x, position.mouse_y, button);
        mouse.press_touch_button(position.touch_x, position.touch_y, button);
    }

    /// Upstream `GRenderWindow::mouseMoveEvent`.
    fn on_mouse_motion(&self, x: f64, y: f64) {
        let Some(position) = self.render_pointer_position(x, y) else {
            return;
        };
        let mut subsystem = self.input_subsystem.borrow_mut();
        let Some(mouse) = subsystem.get_mouse_mut() else {
            return;
        };

        mouse.mouse_move(position.touch_x, position.touch_y);
        mouse.touch_move(position.touch_x, position.touch_y);
        mouse.move_cursor(
            position.mouse_x,
            position.mouse_y,
            position.center_x,
            position.center_y,
        );
    }

    /// Upstream `GRenderWindow::mouseReleaseEvent`.
    fn on_mouse_button_released(&self, button: u32) {
        if self.render.borrow().is_none() {
            return;
        }
        let mut subsystem = self.input_subsystem.borrow_mut();
        if let Some(mouse) = subsystem.get_mouse_mut() {
            mouse.release_button(gdk_button_to_mouse_button(button));
        }
    }

    /// Upstream `GRenderWindow::wheelEvent`.
    fn on_mouse_wheel(&self, delta_x: f64, delta_y: f64) {
        if self.render.borrow().is_none() {
            return;
        }
        let mut subsystem = self.input_subsystem.borrow_mut();
        if let Some(mouse) = subsystem.get_mouse_mut() {
            // GTK scrolls down with positive Y; Qt's angleDelta is positive
            // upward. One discrete GTK step corresponds to Qt's conventional
            // 120 angle units.
            mouse.mouse_wheel_change(
                (delta_x * 120.0).round() as i32,
                (-delta_y * 120.0).round() as i32,
            );
        }
    }

    /// On a first run with an existing yuzu installation, offer to import its
    /// configuration.
    ///
    /// ruzu has no upstream counterpart for this — yuzu has nothing to migrate
    /// *from*. The rule is that nothing is copied without the user saying so,
    /// and yuzu's directory is only ever read.
    ///
    /// The offer is made once: whichever way it is answered, a marker is
    /// written into ruzu's config directory so the next launch starts silently.
    fn maybe_offer_yuzu_import(self: &Rc<Self>) {
        let Some(import) = crate::config_import::available_import() else {
            return;
        };

        crate::gtk_compat::ask_question(
            Some(&self.window),
            "Import your yuzu configuration?",
            &format!(
                "A yuzu configuration was found at:\n{}\n\n\
                 ruzu can copy its settings — including your game directories — \
                 so you can carry on where you left off. \
                 Your yuzu configuration is only read, never modified.",
                import.yuzu_dir.display()
            ),
            "Start Fresh",
            "Import Settings",
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |accepted| {
                    if accepted {
                        import.accept();
                        this.on_yuzu_config_imported();
                    } else {
                        import.decline();
                    }
                }
            ),
        );
    }

    /// Re-read everything that came from the freshly imported configuration.
    fn on_yuzu_config_imported(self: &Rc<Self>) {
        let game_dirs = crate::configuration::qt_config::load_game_dirs();
        log::info!(
            "Imported configuration provides {} game directory(ies)",
            game_dirs.len()
        );
        crate::uisettings::with_mut(|v| v.game_dirs = game_dirs);

        // The imported file also carries the widget theme and the emulator
        // settings the status bar reflects.
        update_ui_theme();
        self.status_bar.refresh();
        if let Some(game_list) = self.game_list.borrow().as_ref() {
            game_list.reload();
        }
    }

    /// Upstream `GMainWindow::OnInstallDecryptionKeys`.
    ///
    /// Asks for a `prod.keys`, copies it (plus `title.keys` / `key_retail.bin`
    /// when they sit beside it) into ruzu's keys directory, reloads the key
    /// manager, and rescans the game list so titles that were undecryptable
    /// appear.
    fn on_install_decryption_keys(self: &Rc<Self>) {
        // Upstream refuses while emulation is running.
        if self.session.borrow().is_some() {
            log::info!("Install Decryption Keys ignored: emulation is running");
            return;
        }

        let filter = gtk::FileFilter::new();
        filter.set_name(Some("prod.keys"));
        filter.add_pattern("prod.keys");

        log::info!("Install Decryption Keys: opening file chooser");
        crate::gtk_compat::open_file(
            Some(&self.window),
            "Select Dumped Keys Location",
            std::slice::from_ref(&filter),
            Some(&filter),
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |result| {
                    let file = match result {
                        Some(file) => file,
                        None => {
                            log::info!("Install Decryption Keys cancelled");
                            return;
                        }
                    };
                    let Some(prod_keys) = file.path() else { return };
                    this.install_decryption_keys_from(&prod_keys);
                }
            ),
        );
    }

    /// Copy the key files sitting beside `prod_keys` into the keys directory.
    fn install_decryption_keys_from(self: &Rc<Self>, prod_keys: &std::path::Path) {
        log::info!("Installing key files from {}", prod_keys.display());
        let Some(source_dir) = prod_keys.parent() else {
            return;
        };

        // There must be at least prod.keys; the other two are optional.
        if !prod_keys.is_file() {
            self.alert(
                "Decryption Keys install failed",
                "prod.keys is a required decryption key file.",
            );
            return;
        }
        let mut sources = vec![prod_keys.to_path_buf()];
        for optional in ["title.keys", "key_retail.bin"] {
            let candidate = source_dir.join(optional);
            if candidate.is_file() {
                sources.push(candidate);
            }
        }

        let keys_dir =
            common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::KeysDir);
        if let Err(e) = std::fs::create_dir_all(&keys_dir) {
            log::error!("Could not create keys dir {}: {e}", keys_dir.display());
            self.alert(
                "Decryption Keys install failed",
                "Could not create the keys directory.",
            );
            return;
        }

        for source in &sources {
            let Some(name) = source.file_name() else {
                continue;
            };
            let destination = keys_dir.join(name);
            // Selecting the keys that are *already* installed would make source
            // and destination the same file, and `fs::copy` onto itself
            // truncates it — destroying the user's keys. Nothing to do anyway.
            if same_file(source, &destination) {
                log::info!("{} is already installed; skipping", source.display());
                continue;
            }
            if let Err(e) = std::fs::copy(source, &destination) {
                log::error!(
                    "Failed to copy file {} to {}: {e}",
                    source.display(),
                    destination.display()
                );
                self.alert(
                    "Decryption Keys install failed",
                    "One or more keys failed to copy.",
                );
                return;
            }
        }

        // Reinitialize the key manager and re-populate the game list, so titles
        // that could not be decrypted before are picked up.
        ruzu_core::crypto::key_manager::KeyManager::instance()
            .lock()
            .unwrap()
            .reload_keys();
        if let Some(game_list) = self.game_list.borrow().as_ref() {
            game_list.reload();
        }

        if frontend_common::content_manager::are_keys_present() {
            self.alert(
                "Decryption Keys install succeeded",
                "Decryption Keys were successfully installed",
            );
        } else {
            self.alert(
                "Decryption Keys install failed",
                "Decryption Keys failed to initialize. Check that your dumping tools are \
                 up to date and re-dump keys.",
            );
        }
    }

    /// Upstream `GMainWindow::OnInstallFirmware`.
    ///
    /// Clears `nand/system/Contents/registered` and copies the dumped firmware
    /// NCAs into it.
    fn on_install_firmware(self: &Rc<Self>) {
        if self.session.borrow().is_some() {
            log::info!("Install Firmware ignored: emulation is running");
            return;
        }

        // Upstream checks for keys first: firmware NCAs cannot be read without
        // them, so installing would produce an unusable NAND.
        if !frontend_common::content_manager::are_keys_present() {
            self.alert(
                "Keys not installed",
                "Install decryption keys and restart ruzu before attempting to install firmware.",
            );
            return;
        }

        crate::gtk_compat::select_folder(
            Some(&self.window),
            "Select Dumped Firmware Source Location",
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |result| {
                    let Some(folder) = result else { return };
                    let Some(path) = folder.path() else { return };
                    this.install_firmware_from(&path);
                }
            ),
        );
    }

    /// Replace the installed firmware with the NCAs found in `source`.
    fn install_firmware_from(self: &Rc<Self>, source: &std::path::Path) {
        log::info!("Installing firmware from {}", source.display());

        // Check for a reasonable number of .nca files — upstream does not
        // hardcode names, it just looks for some.
        let mut ncas: Vec<std::path::PathBuf> = match std::fs::read_dir(source) {
            Ok(entries) => entries
                .filter_map(Result::ok)
                .map(|e| e.path())
                .filter(|p| p.extension().is_some_and(|ext| ext == "nca"))
                .collect(),
            Err(e) => {
                log::error!("Could not read {}: {e}", source.display());
                return;
            }
        };
        ncas.sort();

        if ncas.is_empty() {
            self.alert(
                "Firmware install failed",
                "Unable to locate potential firmware NCA files",
            );
            return;
        }

        // Locate and erase the content of nand/system/Contents/registered.
        let registered =
            common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::NANDDir)
                .join("system/Contents/registered");

        if registered.exists() {
            if let Err(e) = std::fs::remove_dir_all(&registered) {
                log::error!("Failed to clean {}: {e}", registered.display());
                self.alert(
                    "Firmware install failed",
                    "Failed to delete one or more firmware file.",
                );
                return;
            }
        }
        if let Err(e) = std::fs::create_dir_all(&registered) {
            log::error!("Failed to create {}: {e}", registered.display());
            self.alert(
                "Firmware install failed",
                "Failed to create the firmware directory.",
            );
            return;
        }
        log::info!(
            "Cleaned {} in preparation for new firmware",
            registered.display()
        );

        let progress = ProgressWindow::new(&self.window, "Installing Firmware...");
        for (index, nca) in ncas.iter().enumerate() {
            let Some(name) = nca.file_name() else {
                continue;
            };
            if let Err(e) = std::fs::copy(nca, registered.join(name)) {
                log::error!(
                    "Failed to copy firmware file {} into the registered folder: {e}",
                    nca.display()
                );
                progress.close();
                self.alert(
                    "Firmware install failed",
                    "One or more firmware files failed to copy into NAND.",
                );
                return;
            }
            progress.set_fraction((index + 1) as f64 / ncas.len() as f64);
        }
        progress.close();

        log::info!("Installed {} firmware NCA(s)", ncas.len());
        // Upstream then verifies the freshly installed firmware; that runs here
        // as the separate Tools ▸ Verify Installed Contents action rather than
        // automatically, so a slow scan does not block the install dialog.
        self.alert(
            "Firmware install succeeded",
            &format!(
                "Installed {} firmware file(s).\n\n\
                 Run Tools ▸ Verify Installed Contents to check their integrity.",
                ncas.len()
            ),
        );
    }

    /// Upstream `GMainWindow::OnVerifyInstalledContents`.
    fn on_verify_installed_contents(self: &Rc<Self>) {
        log::info!("Verifying installed contents");
        let progress = ProgressWindow::new(&self.window, "Verifying integrity...");

        // Upstream verifies through `system.GetFileSystemController()`. The
        // launcher has no booted `System`, so build the same controller over the
        // real filesystem — the registries it opens are the on-disk NAND ones
        // either way.
        let vfs = ruzu_core::file_sys::vfs::vfs_real::RealVfsFilesystem::new();
        let mut filesystem =
            ruzu_core::hle::service::filesystem::filesystem::FileSystemController::new();
        filesystem.create_factories(vfs, false);

        let failed = frontend_common::content_manager::verify_installed_contents(
            &filesystem,
            &|total, processed| {
                if total > 0 {
                    progress.set_fraction(processed as f64 / total as f64);
                }
                // Returning true cancels; nothing cancels this yet.
                false
            },
            false,
        );

        progress.close();

        if failed.is_empty() {
            self.alert(
                "Integrity verification succeeded!",
                "The operation completed successfully.",
            );
        } else {
            self.alert(
                "Integrity verification failed!",
                &format!(
                    "Verification failed for the following files:\n\n{}",
                    failed.join("\n")
                ),
            );
        }
    }

    /// Show a modal message — the `QMessageBox` calls peppered through the
    /// upstream handlers.
    fn alert(&self, message: &str, detail: &str) {
        crate::gtk_compat::show_message(Some(&self.window), message, detail);
    }

    /// Upstream `GMainWindow::OnConfigure`: build and show the configuration
    /// dialog, parented to the main window.
    ///
    /// Upstream keeps the dialog on the stack and inspects its exec() result to
    /// decide whether to re-read settings. GTK dialogs are modeless objects
    /// whose OK handler applies the settings itself, so the `Rc` is held alive
    /// by the window until the next invocation replaces it.
    fn on_configure(self: &Rc<Self>) {
        let dialog = crate::configuration::ConfigureDialog::new(
            Some(&self.window),
            Rc::clone(&self.input_subsystem),
            Arc::clone(&self.hid_core),
        );
        dialog.connect_closed(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move || {
                this.configure_dialog.borrow_mut().take();
            }
        ));
        dialog.present();
        *self.configure_dialog.borrow_mut() = Some(dialog);
    }

    /// Upstream `OnMenuLoadFile`: choose a Switch executable, then boot it.
    fn on_menu_load_file(self: &Rc<Self>) {
        let filter = gtk::FileFilter::new();
        filter.set_name(Some("Switch Executable"));
        for ext in &["nso", "nro", "nca", "xci", "nsp", "kip"] {
            filter.add_pattern(&format!("*.{ext}"));
        }
        filter.add_pattern("main");
        let all_files = gtk::FileFilter::new();
        all_files.set_name(Some("All Files (*.*)"));
        all_files.add_pattern("*");
        crate::gtk_compat::open_file(
            Some(&self.window),
            "Load File",
            &[filter.clone(), all_files],
            Some(&filter),
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |result| match result {
                    Some(file) => {
                        if let Some(path) = file.path() {
                            this.boot_game(path.to_string_lossy().into_owned());
                        }
                    }
                    None => log::debug!("Load File cancelled"),
                }
            ),
        );
    }

    /// Upstream `OnMenuLoadFolder`: choose an extracted-ROM directory and boot
    /// its `main` file.
    fn on_menu_load_folder(self: &Rc<Self>) {
        crate::gtk_compat::select_folder(
            Some(&self.window),
            "Open Extracted ROM Directory",
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |result| {
                    let Some(dir) = result else { return };
                    let Some(dir_path) = dir.path() else { return };
                    let main = dir_path.join("main");
                    if main.is_file() {
                        this.boot_game(main.to_string_lossy().into_owned());
                    } else {
                        crate::gtk_compat::show_message(
                            Some(&this.window),
                            "Invalid Directory Selected",
                            "The directory you have selected does not contain a 'main' file.",
                        );
                    }
                }
            ),
        );
    }

    /// Boot `filepath` into the embedded render surface. Stand-in for upstream
    /// `GMainWindow::BootGame`: attach the Metal layer, show the loading screen,
    /// start the boot thread, and reveal the render view when loading completes.
    #[cfg(target_os = "macos")]
    pub fn boot_game(self: &Rc<Self>, filepath: String) {
        use crate::emu_window::GtkEmuWindow;

        // The render surface only exists once the window is realized, and the
        // central stack only has an allocation after the first layout pass. If a
        // boot is requested before that (e.g. launched with a game argument),
        // retry on a short timer until both are ready — otherwise the render
        // area would be 0×0.
        let ready =
            self.window.surface().is_some() && self.stack.width() > 0 && self.stack.height() > 0;
        if !ready {
            let this = Rc::clone(self);
            glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
                if this.window.surface().is_some()
                    && this.stack.width() > 0
                    && this.stack.height() > 0
                {
                    this.boot_game(filepath.clone());
                    glib::ControlFlow::Break
                } else {
                    glib::ControlFlow::Continue
                }
            });
            return;
        }

        // Stop any existing session first (upstream stops before re-booting).
        if let Some(mut session) = self.session.borrow_mut().take() {
            session.stop();
        }
        let session_generation = self.session_generation.get().wrapping_add(1);
        self.session_generation.set(session_generation);

        // Reflect current settings in the status bar (upstream refreshes the
        // status buttons around boot).
        self.status_bar.refresh();

        // Render area = the central stack's bounds in window coordinates, so
        // the child render window leaves the bottom status bar visible.
        let render_rect = self.stack.compute_bounds(&self.window).map(|r| {
            (
                r.x() as f64,
                r.y() as f64,
                r.width() as f64,
                r.height() as f64,
            )
        });

        let Some(layer) = crate::render_window::attach_metal_layer(
            self.window.upcast_ref::<gtk::Window>(),
            render_rect,
        ) else {
            log::error!("Cannot boot: failed to embed render surface");
            return;
        };
        // Keep the render window hidden so the GTK loading screen shows during
        // load; revealed on completion.
        crate::render_window::set_render_view_hidden(layer.child_window, true);
        let child_window = layer.child_window as usize;

        let emu = GtkEmuWindow::from_metal_layer(layer);
        let window_info = emu.window_info().clone();
        let drawable_size = emu.drawable_size();
        let shown_state = emu.shown_state();
        let framebuffer_layout = emu.framebuffer_layout();

        // Remember the render handles so the surface can be resized with the
        // window.
        *self.render.borrow_mut() = Some(RenderHandles {
            child_window: layer.child_window as usize,
            metal_layer: layer.metal_layer as usize,
            framebuffer_layout: Arc::clone(&framebuffer_layout),
        });
        self.render_size
            .set((self.stack.width(), self.stack.height()));

        self.show_loading_screen();

        // Loading events are produced on the boot and Vulkan worker threads,
        // then consumed on GTK's main thread. Adjacent Build events are
        // coalesced while stage transitions remain queued, so a hot cache still
        // visibly passes through `Loading Shaders` before `Launching`.
        let mailbox = Arc::new(Mutex::new(LoadingEventMailbox::default()));
        let producer = Arc::clone(&mailbox);
        let loading_event: crate::boot::LoadingEventFn = Arc::new(move |event| {
            producer.lock().unwrap().push(event);
        });

        let loading = Rc::clone(&self.loading_screen);
        let stack = self.stack.clone();
        let this = Rc::clone(self);
        glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
            if this.session_generation.get() != session_generation {
                return glib::ControlFlow::Break;
            }
            match mailbox.lock().unwrap().pop() {
                Some(LoadingEvent::Assets(assets)) => {
                    loading.set_assets(assets.logo.as_deref(), assets.banner.as_deref());
                }
                Some(LoadingEvent::Progress {
                    stage,
                    value,
                    total,
                }) => loading.on_load_progress(stage, value, total),
                Some(LoadingEvent::FirstFrame) => {
                    let stack = stack.clone();
                    loading.on_load_complete(move || {
                        // Upstream reveals the render window only after the
                        // loading screen has faded out following its first
                        // framebuffer.
                        stack.set_visible_child_name(PAGE_RENDER);
                        crate::render_window::set_render_view_hidden(child_window as *mut _, false);
                    });
                }
                Some(LoadingEvent::Failed { message, detail }) => {
                    this.on_emulation_stopped(Some((message, detail)));
                    return glib::ControlFlow::Break;
                }
                Some(LoadingEvent::Stopped { before_first_frame }) => {
                    let failure = before_first_frame.then(|| {
                        (
                            "The game stopped unexpectedly".to_owned(),
                            "The application stopped before displaying a frame. \
                             Verify that its required files are available on the emulated SD card \
                             and check the log for details."
                                .to_owned(),
                        )
                    });
                    this.on_emulation_stopped(failure);
                    return glib::ControlFlow::Break;
                }
                Some(LoadingEvent::StopComplete) => {
                    this.on_emulation_stopped(None);
                    return glib::ControlFlow::Break;
                }
                None => {}
            }
            glib::ControlFlow::Continue
        });

        let session = crate::boot::boot_game(
            window_info,
            drawable_size,
            shown_state,
            framebuffer_layout,
            Arc::clone(&self.hid_core),
            filepath,
            loading_event,
        );
        *self.session.borrow_mut() = Some(session);
        if let Some(app) = self.window.application() {
            update_menu_state(&app, true, false);
        }
        if crate::uisettings::with(|values| *values.fullscreen.get_value()) {
            self.set_fullscreen(true);
        }
    }

    /// Boot `filepath` into an X11 child window embedded in the GTK window.
    ///
    /// Same shape as the macOS path above; only the native surface differs —
    /// an X11 child `Window` instead of a `CAMetalLayer` sub-view, matching
    /// upstream's per-platform `GetWindowSystemInfo`.
    #[cfg(target_os = "linux")]
    pub fn boot_game(self: &Rc<Self>, filepath: String) {
        use crate::emu_window::GtkEmuWindow;
        use crate::render_window_x11 as render;
        use ruzu_core::frontend::emu_window::{WindowSystemInfo, WindowSystemType};

        // The render surface only exists once the window is realized, and the
        // central stack only has an allocation after the first layout pass.
        let ready =
            self.window.surface().is_some() && self.stack.width() > 0 && self.stack.height() > 0;
        if !ready {
            let this = Rc::clone(self);
            glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
                if this.window.surface().is_some()
                    && this.stack.width() > 0
                    && this.stack.height() > 0
                {
                    this.boot_game(filepath.clone());
                    glib::ControlFlow::Break
                } else {
                    glib::ControlFlow::Continue
                }
            });
            return;
        }

        // Stop any existing session first (upstream stops before re-booting).
        if let Some(mut session) = self.session.borrow_mut().take() {
            session.stop();
        }
        let session_generation = self.session_generation.get().wrapping_add(1);
        self.session_generation.set(session_generation);
        self.status_bar.refresh();

        // Render area = the central stack's bounds, so the child window leaves
        // the menu bar and status bar visible.
        let render_rect = self.stack.compute_bounds(&self.window).map(|r| {
            (
                r.x() as f64,
                r.y() as f64,
                r.width() as f64,
                r.height() as f64,
            )
        });

        let Some(embedded) =
            render::attach_render_window(self.window.upcast_ref::<gtk::Window>(), render_rect)
        else {
            log::error!(
                "Cannot boot: failed to embed an X11 render surface. \
                 Native Wayland is not supported yet — relaunch with GDK_BACKEND=x11."
            );
            self.alert(
                "Unable to start the game",
                "ruzu could not create its embedded X11 render surface. \
                 Ensure XWayland is available and restart the application.",
            );
            return;
        };

        // Keep it hidden so the loading screen shows during load.
        render::set_render_window_hidden(embedded.display, embedded.window, true);

        let window_info = WindowSystemInfo {
            type_: WindowSystemType::X11,
            display_connection: embedded.display as usize,
            render_surface: embedded.window as usize,
            render_surface_scale: embedded.scale,
        };
        let emu = GtkEmuWindow::from_window_info(window_info.clone(), embedded.drawable_size);
        let drawable_size = emu.drawable_size();
        let shown_state = emu.shown_state();
        let framebuffer_layout = emu.framebuffer_layout();

        *self.render.borrow_mut() = Some(RenderHandles {
            display: embedded.display as usize,
            child_window: embedded.window as usize,
            framebuffer_layout: Arc::clone(&framebuffer_layout),
        });
        self.render_size
            .set((self.stack.width(), self.stack.height()));

        self.show_loading_screen();

        let mailbox = Arc::new(Mutex::new(LoadingEventMailbox::default()));
        let producer = Arc::clone(&mailbox);
        let loading_event: crate::boot::LoadingEventFn = Arc::new(move |event| {
            producer.lock().unwrap().push(event);
        });

        let loading = Rc::clone(&self.loading_screen);
        let stack = self.stack.clone();
        let display = embedded.display as usize;
        let child = embedded.window;
        let this = Rc::clone(self);
        glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
            if this.session_generation.get() != session_generation {
                return glib::ControlFlow::Break;
            }
            match mailbox.lock().unwrap().pop() {
                Some(LoadingEvent::Assets(assets)) => {
                    loading.set_assets(assets.logo.as_deref(), assets.banner.as_deref());
                }
                Some(LoadingEvent::Progress {
                    stage,
                    value,
                    total,
                }) => loading.on_load_progress(stage, value, total),
                Some(LoadingEvent::FirstFrame) => {
                    let stack = stack.clone();
                    loading.on_load_complete(move || {
                        stack.set_visible_child_name(PAGE_RENDER);
                        render::set_render_window_hidden(display as *mut _, child, false);
                    });
                }
                Some(LoadingEvent::Failed { message, detail }) => {
                    this.on_emulation_stopped(Some((message, detail)));
                    return glib::ControlFlow::Break;
                }
                Some(LoadingEvent::Stopped { before_first_frame }) => {
                    let failure = before_first_frame.then(|| {
                        (
                            "The game stopped unexpectedly".to_owned(),
                            "The application stopped before displaying a frame. \
                             Verify that its required files are available on the emulated SD card \
                             and check the log for details."
                                .to_owned(),
                        )
                    });
                    this.on_emulation_stopped(failure);
                    return glib::ControlFlow::Break;
                }
                Some(LoadingEvent::StopComplete) => {
                    this.on_emulation_stopped(None);
                    return glib::ControlFlow::Break;
                }
                None => {}
            }
            glib::ControlFlow::Continue
        });

        let session = crate::boot::boot_game(
            window_info,
            drawable_size,
            shown_state,
            framebuffer_layout,
            Arc::clone(&self.hid_core),
            filepath,
            loading_event,
        );
        *self.session.borrow_mut() = Some(session);
        if let Some(app) = self.window.application() {
            update_menu_state(&app, true, false);
        }
        if crate::uisettings::with(|values| *values.fullscreen.get_value()) {
            self.set_fullscreen(true);
        }
    }

    /// Boot `filepath` into a Win32 child window embedded in the GTK window.
    ///
    /// Upstream's Windows `RenderWidget` owns a native child `HWND` and passes
    /// `windowHandle()->winId()` to Vulkan. GTK has no native surface per
    /// widget, so `render_window_windows` creates the equivalent child directly.
    #[cfg(target_os = "windows")]
    pub fn boot_game(self: &Rc<Self>, filepath: String) {
        use crate::emu_window::GtkEmuWindow;
        use crate::render_window_windows as render;
        use ruzu_core::frontend::emu_window::{WindowSystemInfo, WindowSystemType};

        let ready =
            self.window.surface().is_some() && self.stack.width() > 0 && self.stack.height() > 0;
        if !ready {
            let this = Rc::clone(self);
            glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
                if this.window.surface().is_some()
                    && this.stack.width() > 0
                    && this.stack.height() > 0
                {
                    this.boot_game(filepath.clone());
                    glib::ControlFlow::Break
                } else {
                    glib::ControlFlow::Continue
                }
            });
            return;
        }

        // Upstream releases the previous render target before initializing the
        // next one. Stop the session first so Vulkan no longer owns its HWND.
        if let Some(mut session) = self.session.borrow_mut().take() {
            session.stop();
        }
        if let Some(handles) = self.render.borrow_mut().take() {
            render::destroy_render_window(handles.child_window as _);
        }
        let session_generation = self.session_generation.get().wrapping_add(1);
        self.session_generation.set(session_generation);
        self.status_bar.refresh();

        let render_rect = self.stack.compute_bounds(&self.window).map(|rect| {
            (
                rect.x() as f64,
                rect.y() as f64,
                rect.width() as f64,
                rect.height() as f64,
            )
        });
        let Some(embedded) =
            render::attach_render_window(self.window.upcast_ref::<gtk::Window>(), render_rect)
        else {
            log::error!("Cannot boot: failed to embed a Win32 render surface");
            self.alert(
                "Unable to start the game",
                "ruzu could not create its embedded Windows render surface.",
            );
            return;
        };
        render::set_render_window_hidden(embedded.window, true);

        let window_info = WindowSystemInfo {
            type_: WindowSystemType::Windows,
            display_connection: 0,
            render_surface: embedded.window as usize,
            render_surface_scale: embedded.scale,
        };
        let emu = GtkEmuWindow::from_window_info(window_info.clone(), embedded.drawable_size);
        let drawable_size = emu.drawable_size();
        let shown_state = emu.shown_state();
        let framebuffer_layout = emu.framebuffer_layout();

        *self.render.borrow_mut() = Some(RenderHandles {
            child_window: embedded.window as usize,
            framebuffer_layout: Arc::clone(&framebuffer_layout),
        });
        self.render_size
            .set((self.stack.width(), self.stack.height()));
        self.show_loading_screen();

        let mailbox = Arc::new(Mutex::new(LoadingEventMailbox::default()));
        let producer = Arc::clone(&mailbox);
        let loading_event: crate::boot::LoadingEventFn = Arc::new(move |event| {
            producer.lock().unwrap().push(event);
        });

        let loading = Rc::clone(&self.loading_screen);
        let stack = self.stack.clone();
        let child = embedded.window as usize;
        let this = Rc::clone(self);
        glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
            if this.session_generation.get() != session_generation {
                return glib::ControlFlow::Break;
            }
            match mailbox.lock().unwrap().pop() {
                Some(LoadingEvent::Assets(assets)) => {
                    loading.set_assets(assets.logo.as_deref(), assets.banner.as_deref());
                }
                Some(LoadingEvent::Progress {
                    stage,
                    value,
                    total,
                }) => loading.on_load_progress(stage, value, total),
                Some(LoadingEvent::FirstFrame) => {
                    let stack = stack.clone();
                    loading.on_load_complete(move || {
                        stack.set_visible_child_name(PAGE_RENDER);
                        render::set_render_window_hidden(child as _, false);
                    });
                }
                Some(LoadingEvent::Failed { message, detail }) => {
                    this.on_emulation_stopped(Some((message, detail)));
                    return glib::ControlFlow::Break;
                }
                Some(LoadingEvent::Stopped { before_first_frame }) => {
                    let failure = before_first_frame.then(|| {
                        (
                            "The game stopped unexpectedly".to_owned(),
                            "The application stopped before displaying a frame. \
                             Verify that its required files are available on the emulated SD card \
                             and check the log for details."
                                .to_owned(),
                        )
                    });
                    this.on_emulation_stopped(failure);
                    return glib::ControlFlow::Break;
                }
                Some(LoadingEvent::StopComplete) => {
                    this.on_emulation_stopped(None);
                    return glib::ControlFlow::Break;
                }
                None => {}
            }
            glib::ControlFlow::Continue
        });

        let session = crate::boot::boot_game(
            window_info,
            drawable_size,
            shown_state,
            framebuffer_layout,
            Arc::clone(&self.hid_core),
            filepath,
            loading_event,
        );
        *self.session.borrow_mut() = Some(session);
        if let Some(app) = self.window.application() {
            update_menu_state(&app, true, false);
        }
        if crate::uisettings::with(|values| *values.fullscreen.get_value()) {
            self.set_fullscreen(true);
        }
    }

    /// In-process boot needs a platform-specific native render surface.
    #[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
    pub fn boot_game(self: &Rc<Self>, _filepath: String) {
        log::error!("In-process boot is not implemented on this platform yet");
    }

    /// If a game is running and the central stack changed size, resize the
    /// embedded render window and update the framebuffer layout so the renderer
    /// recreates its swapchain at the new size (fills the window instead of
    /// staying fixed at the boot size).
    #[cfg(target_os = "macos")]
    fn maybe_resize_render(&self) {
        let (w, h) = (self.stack.width(), self.stack.height());
        if w <= 0 || h <= 0 || self.render_size.get() == (w, h) {
            return;
        }
        let render = self.render.borrow();
        let Some(handles) = render.as_ref() else {
            self.render_size.set((w, h));
            return;
        };
        let Some(rect) = self.stack.compute_bounds(&self.window) else {
            return;
        };
        self.render_size.set((w, h));
        let gr = (
            rect.x() as f64,
            rect.y() as f64,
            rect.width() as f64,
            rect.height() as f64,
        );
        // Resize the render surface to the new native size and update the shared
        // frame layout so the renderer recreates its swapchain and renders at the
        // new resolution (upstream-equivalent, crisp — see `resize_child_window`
        // and the `device_wait_idle` in the swapchain recreation).
        if let Some((dw, dh)) = crate::render_window::resize_child_window(
            self.window.upcast_ref::<gtk::Window>(),
            handles.child_window as *mut _,
            handles.metal_layer as *mut _,
            gr,
        ) {
            *handles.framebuffer_layout.write().unwrap() = default_frame_layout(dw, dh);
        }
    }

    /// Linux counterpart of `maybe_resize_render`: move/resize the X11 child
    /// window to the stack's new bounds and rebuild the frame layout.
    #[cfg(target_os = "linux")]
    fn maybe_resize_render(&self) {
        let (w, h) = (self.stack.width(), self.stack.height());
        if w <= 0 || h <= 0 || self.render_size.get() == (w, h) {
            return;
        }
        let render = self.render.borrow();
        let Some(handles) = render.as_ref() else {
            self.render_size.set((w, h));
            return;
        };
        let Some(rect) = self.stack.compute_bounds(&self.window) else {
            return;
        };
        self.render_size.set((w, h));
        let gr = (
            rect.x() as f64,
            rect.y() as f64,
            rect.width() as f64,
            rect.height() as f64,
        );
        if let Some((dw, dh)) = crate::render_window_x11::resize_render_window(
            self.window.upcast_ref::<gtk::Window>(),
            handles.display as *mut _,
            handles.child_window as u64,
            gr,
        ) {
            *handles.framebuffer_layout.write().unwrap() = default_frame_layout(dw, dh);
        }
    }

    /// Windows counterpart of `maybe_resize_render`: resize the child `HWND`
    /// and publish the new framebuffer layout after the native resize.
    #[cfg(target_os = "windows")]
    fn maybe_resize_render(&self) {
        let (width, height) = (self.stack.width(), self.stack.height());
        if width <= 0 || height <= 0 || self.render_size.get() == (width, height) {
            return;
        }
        let render = self.render.borrow();
        let Some(handles) = render.as_ref() else {
            self.render_size.set((width, height));
            return;
        };
        let Some(rect) = self.stack.compute_bounds(&self.window) else {
            return;
        };
        self.render_size.set((width, height));
        let gtk_rect = (
            rect.x() as f64,
            rect.y() as f64,
            rect.width() as f64,
            rect.height() as f64,
        );
        if let Some((drawable_width, drawable_height)) =
            crate::render_window_windows::resize_render_window(
                self.window.upcast_ref::<gtk::Window>(),
                handles.child_window as _,
                gtk_rect,
            )
        {
            *handles.framebuffer_layout.write().unwrap() =
                default_frame_layout(drawable_width, drawable_height);
        }
    }

    /// Switch the central stack to the loading screen and reset its state.
    /// Mirrors the point where upstream shows `LoadingScreen` before booting.
    pub fn show_loading_screen(&self) {
        self.loading_screen.prepare();
        self.stack.set_visible_child_name(PAGE_LOADING);
    }

    /// Switch the central stack back to the game list.
    pub fn show_game_list(&self) {
        self.stack.set_visible_child_name(PAGE_GAME_LIST);
    }

    /// Upstream `GMainWindow::OnStopGame` and `ConfirmShutdownGame`.
    fn on_stop_game(self: &Rc<Self>) {
        let Some(exit_locked) = self
            .session
            .borrow()
            .as_ref()
            .map(EmulationSession::exit_locked)
        else {
            return;
        };
        let setting = crate::uisettings::with(|values| *values.confirm_before_stopping.get_value());
        let confirmation = stop_confirmation(setting, exit_locked);
        if confirmation == StopConfirmation::None {
            self.begin_stop_game();
            return;
        }
        if self.stop_confirmation_pending.replace(true) {
            return;
        }

        let detail = match confirmation {
            StopConfirmation::ChangeGame => {
                "Are you sure you want to stop the emulation? Any unsaved progress will be lost."
            }
            StopConfirmation::ForceLockedExit => {
                "The currently running application has requested ruzu to not exit.\n\n\
                 Would you like to bypass this and exit anyway?"
            }
            StopConfirmation::None => unreachable!(),
        };
        crate::gtk_compat::ask_question(
            Some(&self.window),
            "ruzu",
            detail,
            "No",
            "Yes",
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |accepted| {
                    this.stop_confirmation_pending.set(false);
                    if accepted {
                        this.begin_stop_game();
                    }
                }
            ),
        );
    }

    /// Begin the non-blocking half of upstream `OnShutdownBegin`. The boot
    /// thread requests guest exit, applies the upstream timeout, and reports
    /// `StopComplete` after forced teardown if necessary.
    fn begin_stop_game(self: &Rc<Self>) {
        let requested = self
            .session
            .borrow_mut()
            .as_mut()
            .map(EmulationSession::request_stop)
            .unwrap_or(false);
        if !requested {
            return;
        }

        if let Some(app) = self.window.application() {
            if self.window.is_fullscreen() {
                self.set_fullscreen_action_state(&app, false);
                crate::uisettings::with_mut(|values| values.fullscreen.set_value(false));
                self.set_fullscreen(false);
            }
            for name in ["pause", "restart", "stop"] {
                if let Some(action) = app
                    .lookup_action(name)
                    .and_then(|action| action.downcast::<gio::SimpleAction>().ok())
                {
                    action.set_enabled(false);
                }
            }
        }
        if let Some(game_list) = self.game_list.borrow().as_ref() {
            game_list.reload();
        }
    }

    /// Finish a session after a load failure or guest-requested exit.
    ///
    /// This follows upstream `OnEmulationStopped`: stop and join emulation
    /// before releasing the native render target, clear the loading assets,
    /// restore the game list, and then report an error when applicable.
    fn on_emulation_stopped(self: &Rc<Self>, failure: Option<(String, String)>) {
        self.stop_confirmation_pending.set(false);
        if let Some(mut session) = self.session.borrow_mut().take() {
            session.stop();
        }

        if let Some(handles) = self.render.borrow_mut().take() {
            #[cfg(target_os = "linux")]
            crate::render_window_x11::destroy_render_window(
                handles.display as *mut _,
                handles.child_window as u64,
            );
            #[cfg(target_os = "macos")]
            crate::render_window::set_render_view_hidden(handles.child_window as *mut _, true);
            #[cfg(target_os = "windows")]
            crate::render_window_windows::destroy_render_window(handles.child_window as _);
        }

        self.loading_screen.clear();
        self.show_game_list();
        self.status_bar.update_performance(None);
        if let Some(app) = self.window.application() {
            update_menu_state(&app, false, true);
        }

        if let Some((message, detail)) = failure {
            self.alert(&message, &detail);
        }
    }

    /// Animate fake shader-build progress to exercise the loading-screen UI.
    fn start_loading_demo(&self) {
        self.show_loading_screen();
        let loading = Rc::clone(&self.loading_screen);
        let total = 1628usize;
        let value = Rc::new(std::cell::Cell::new(0usize));
        glib::timeout_add_local(std::time::Duration::from_millis(40), move || {
            let v = value.get() + 11;
            value.set(v);
            if v >= total {
                loading.on_load_progress(crate::loading_screen::LoadStage::Complete, total, total);
                glib::ControlFlow::Break
            } else {
                loading.on_load_progress(crate::loading_screen::LoadStage::Build, v, total);
                glib::ControlFlow::Continue
            }
        });
    }

    /// Upstream `GMainWindow::UpdateInputDrivers`, driven by `update_input_timer`.
    ///
    /// Nothing else pumps the input engines: the SDL driver installs an event
    /// watch but SDL only dispatches to it while somebody pumps its queue, so
    /// without this timer no button press or stick movement ever reaches the
    /// engines. Upstream's interval is `default_input_update_timeout = 1` ms.
    fn start_input_driver_updates(self: &Rc<Self>) {
        let input_subsystem = Rc::clone(&self.input_subsystem);
        glib::timeout_add_local(
            std::time::Duration::from_millis(INPUT_UPDATE_TIMEOUT_MS),
            move || {
                input_subsystem.borrow_mut().pump_events();
                glib::ControlFlow::Continue
            },
        );
    }

    /// Refresh the performance section of the status bar at upstream's 500 ms
    /// cadence. The boot thread owns `System` and publishes each reset sample
    /// through [`EmulationSession`], so GTK only reads a small copied snapshot.
    fn start_status_bar_updates(self: &Rc<Self>) {
        glib::timeout_add_local(
            std::time::Duration::from_millis(STATUS_BAR_UPDATE_TIMEOUT_MS),
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                #[upgrade_or]
                glib::ControlFlow::Break,
                move || {
                    let results = this
                        .session
                        .borrow()
                        .as_ref()
                        .and_then(EmulationSession::perf_stats);
                    this.status_bar.update_performance(results);
                    glib::ControlFlow::Continue
                }
            ),
        );
    }

    /// Show the window. Mirrors upstream `main_window.show()`.
    pub fn present(&self) {
        self.window.present();
    }
}

/// Install the application-scoped menu bar and its actions. Called once from
/// the application's `startup` signal (see `main.rs`).
///
/// On the macOS (quartz) GDK backend, [`gtk::prelude::GtkApplicationExt::set_menubar`]
/// bridges this `GMenuModel` into the native global menu bar at the top of the
/// screen. GTK additionally provides the standard macOS *App* menu (with
/// `app.preferences` / `app.quit`) and *Edit* menu automatically, so those two
/// actions are registered here.
pub fn init_app_menu(app: &Application) {
    // Wire the implemented File-menu actions (load_file, load_folder,
    // open_ruzu_folder, exit) first; `register_menu_actions` then skips any
    // name already registered, leaving the rest as logging stubs.
    crate::file_menu::register(app);
    register_menu_actions(app);

    // macOS App-menu items GTK adds automatically. "Quit" is wired to actually
    // terminate; "Preferences" is a stub until the configuration dialog exists.
    if app.lookup_action("quit").is_none() {
        let quit = gio::SimpleAction::new("quit", None);
        quit.connect_activate(glib::clone!(
            #[weak]
            app,
            move |_, _| app.quit()
        ));
        app.add_action(&quit);
        app.set_accels_for_action("app.quit", &["<Meta>q"]);
    }
    if app.lookup_action("preferences").is_none() {
        let preferences = gio::SimpleAction::new("preferences", None);
        preferences.connect_activate(|_, _| {
            log::info!("menu action 'preferences' triggered (not yet wired)");
        });
        app.add_action(&preferences);
        app.set_accels_for_action("app.preferences", &["<Meta>comma"]);
    }

    app.set_menubar(Some(&build_menu_model()));

    // Upstream calls `UpdateMenuState()` once the menu is built, which greys
    // out the run-time entries because no game is running yet.
    update_menu_state(app, false, true);
}

/// GTK counterpart of upstream `GRenderWindow::QtButtonToMouseButton`.
fn gdk_button_to_mouse_button(button: u32) -> MouseButton {
    match button {
        1 => MouseButton::Left,
        2 => MouseButton::Wheel,
        3 => MouseButton::Right,
        8 => MouseButton::Backward,
        9 => MouseButton::Forward,
        10 => MouseButton::Task,
        _ => MouseButton::Extra,
    }
}

/// GDK modifier state to the Switch's HID modifier bitmask.
///
/// Port of `GRenderWindow::QtModifierToSwitchModifier`. Like Qt, GDK does not
/// distinguish left from right for a held modifier, so upstream's commented-out
/// right-hand cases stay unreachable here too.
fn switch_modifiers(state: gtk::gdk::ModifierType) -> i32 {
    use common::settings_input::native_keyboard::Modifiers;
    use gtk::gdk::ModifierType;

    let mut modifier = 0;
    if state.contains(ModifierType::SHIFT_MASK) {
        modifier |= 1 << Modifiers::LeftShift as i32;
    }
    if state.contains(ModifierType::CONTROL_MASK) {
        modifier |= 1 << Modifiers::LeftControl as i32;
    }
    if state.contains(ModifierType::ALT_MASK) {
        modifier |= 1 << Modifiers::LeftAlt as i32;
    }
    if state.contains(ModifierType::SUPER_MASK) {
        modifier |= 1 << Modifiers::LeftMeta as i32;
    }
    // Unlike Qt, GDK does report these lock states, so they are worth mapping.
    if state.contains(ModifierType::LOCK_MASK) {
        modifier |= 1 << Modifiers::CapsLock as i32;
    }
    modifier
}

/// Convert a GDK keyval to the `Qt::Key` value upstream stores in controller
/// bindings and passes to `Keyboard::PressKey`.
///
/// Printable Qt keys use their uppercase Unicode value. Non-printable keys
/// occupy Qt's `0x01000000` range.
fn gdk_key_to_qt_key(keyval: gtk::gdk::Key) -> i32 {
    use gtk::gdk::Key;

    const QT_KEY_ESCAPE: i32 = 0x0100_0000;
    const QT_KEY_TAB: i32 = 0x0100_0001;
    const QT_KEY_BACKTAB: i32 = 0x0100_0002;
    const QT_KEY_BACKSPACE: i32 = 0x0100_0003;
    const QT_KEY_RETURN: i32 = 0x0100_0004;
    const QT_KEY_ENTER: i32 = 0x0100_0005;
    const QT_KEY_INSERT: i32 = 0x0100_0006;
    const QT_KEY_DELETE: i32 = 0x0100_0007;
    const QT_KEY_PAUSE: i32 = 0x0100_0008;
    const QT_KEY_PRINT: i32 = 0x0100_0009;
    const QT_KEY_HOME: i32 = 0x0100_0010;
    const QT_KEY_END: i32 = 0x0100_0011;
    const QT_KEY_LEFT: i32 = 0x0100_0012;
    const QT_KEY_UP: i32 = 0x0100_0013;
    const QT_KEY_RIGHT: i32 = 0x0100_0014;
    const QT_KEY_DOWN: i32 = 0x0100_0015;
    const QT_KEY_PAGE_UP: i32 = 0x0100_0016;
    const QT_KEY_PAGE_DOWN: i32 = 0x0100_0017;
    const QT_KEY_F1: i32 = 0x0100_0030;

    match keyval {
        Key::Escape => QT_KEY_ESCAPE,
        Key::Tab => QT_KEY_TAB,
        Key::ISO_Left_Tab => QT_KEY_BACKTAB,
        Key::BackSpace => QT_KEY_BACKSPACE,
        Key::Return => QT_KEY_RETURN,
        Key::KP_Enter => QT_KEY_ENTER,
        Key::Insert => QT_KEY_INSERT,
        Key::Delete => QT_KEY_DELETE,
        Key::Pause => QT_KEY_PAUSE,
        Key::Print => QT_KEY_PRINT,
        Key::Home | Key::KP_Home => QT_KEY_HOME,
        Key::End | Key::KP_End => QT_KEY_END,
        Key::Left | Key::KP_Left => QT_KEY_LEFT,
        Key::Up | Key::KP_Up => QT_KEY_UP,
        Key::Right | Key::KP_Right => QT_KEY_RIGHT,
        Key::Down | Key::KP_Down => QT_KEY_DOWN,
        Key::Page_Up | Key::KP_Page_Up => QT_KEY_PAGE_UP,
        Key::Page_Down | Key::KP_Page_Down => QT_KEY_PAGE_DOWN,
        Key::F1 => QT_KEY_F1,
        Key::F2 => QT_KEY_F1 + 1,
        Key::F3 => QT_KEY_F1 + 2,
        Key::F4 => QT_KEY_F1 + 3,
        Key::F5 => QT_KEY_F1 + 4,
        Key::F6 => QT_KEY_F1 + 5,
        Key::F7 => QT_KEY_F1 + 6,
        Key::F8 => QT_KEY_F1 + 7,
        Key::F9 => QT_KEY_F1 + 8,
        Key::F10 => QT_KEY_F1 + 9,
        Key::F11 => QT_KEY_F1 + 10,
        Key::F12 => QT_KEY_F1 + 11,
        _ => keyval
            .to_unicode()
            .map(|character| {
                if character.is_ascii_lowercase() {
                    character.to_ascii_uppercase() as i32
                } else {
                    character as i32
                }
            })
            .unwrap_or(0),
    }
}

/// GDK keyval to the Switch's HID key code.
///
/// Port of `GRenderWindow::QtKeyToSwitchKey`, which maps the frontend toolkit's
/// key constants onto `Settings::NativeKeyboard::Keys` (the USB HID usage IDs
/// the console expects). Anything unmapped is `None`, which
/// `PressKeyboardKey` ignores.
fn gdk_key_to_switch_key(keyval: gtk::gdk::Key) -> i32 {
    use common::settings_input::native_keyboard::Keys;
    use gtk::gdk::Key;

    let key = match keyval {
        Key::a | Key::A => Keys::A,
        Key::b | Key::B => Keys::B,
        Key::c | Key::C => Keys::C,
        Key::d | Key::D => Keys::D,
        Key::e | Key::E => Keys::E,
        Key::f | Key::F => Keys::F,
        Key::g | Key::G => Keys::G,
        Key::h | Key::H => Keys::H,
        Key::i | Key::I => Keys::I,
        Key::j | Key::J => Keys::J,
        Key::k | Key::K => Keys::K,
        Key::l | Key::L => Keys::L,
        Key::m | Key::M => Keys::M,
        Key::n | Key::N => Keys::N,
        Key::o | Key::O => Keys::O,
        Key::p | Key::P => Keys::P,
        Key::q | Key::Q => Keys::Q,
        Key::r | Key::R => Keys::R,
        Key::s | Key::S => Keys::S,
        Key::t | Key::T => Keys::T,
        Key::u | Key::U => Keys::U,
        Key::v | Key::V => Keys::V,
        Key::w | Key::W => Keys::W,
        Key::x | Key::X => Keys::X,
        Key::y | Key::Y => Keys::Y,
        Key::z | Key::Z => Keys::Z,
        Key::_1 => Keys::N1,
        Key::_2 => Keys::N2,
        Key::_3 => Keys::N3,
        Key::_4 => Keys::N4,
        Key::_5 => Keys::N5,
        Key::_6 => Keys::N6,
        Key::_7 => Keys::N7,
        Key::_8 => Keys::N8,
        Key::_9 => Keys::N9,
        Key::_0 => Keys::N0,
        Key::Return => Keys::Return,
        Key::Escape => Keys::Escape,
        Key::BackSpace => Keys::Backspace,
        Key::Tab => Keys::Tab,
        Key::space => Keys::Space,
        Key::Left => Keys::Left,
        Key::Right => Keys::Right,
        Key::Up => Keys::Up,
        Key::Down => Keys::Down,
        _ => Keys::None,
    };
    key as i32
}

/// Whether two paths refer to the same file on disk, resolving symlinks.
///
/// Used to keep `fs::copy` from being handed identical source and destination,
/// which truncates the file rather than being a no-op.
fn same_file(a: &std::path::Path, b: &std::path::Path) -> bool {
    match (std::fs::canonicalize(a), std::fs::canonicalize(b)) {
        (Ok(a), Ok(b)) => a == b,
        // A destination that does not exist yet cannot be the source.
        _ => false,
    }
}

/// A modal progress window — upstream's `QProgressDialog`.
///
/// The operations it covers (firmware copy, integrity verification) run
/// synchronously on the main thread, as upstream's do. Qt pumps its event loop
/// from inside `QProgressDialog::setValue`; the GTK equivalent is to iterate the
/// main context explicitly, which is what [`Self::set_fraction`] does — without
/// it the window would never paint.
struct ProgressWindow {
    window: gtk::Window,
    bar: gtk::ProgressBar,
}

impl ProgressWindow {
    fn new(parent: &gtk::ApplicationWindow, message: &str) -> Self {
        let bar = gtk::ProgressBar::new();
        bar.set_show_text(true);
        bar.set_hexpand(true);

        let content = gtk::Box::new(gtk::Orientation::Vertical, 12);
        content.set_margin_top(18);
        content.set_margin_bottom(18);
        content.set_margin_start(18);
        content.set_margin_end(18);
        let label = gtk::Label::new(Some(message));
        label.set_xalign(0.0);
        content.append(&label);
        content.append(&bar);

        let window = gtk::Window::builder()
            .title(message)
            .modal(true)
            .transient_for(parent)
            .resizable(false)
            .default_width(360)
            .child(&content)
            .build();
        window.present();

        let this = Self { window, bar };
        this.pump();
        this
    }

    fn set_fraction(&self, fraction: f64) {
        self.bar.set_fraction(fraction.clamp(0.0, 1.0));
        self.pump();
    }

    /// Let GTK lay out and paint while the caller keeps the main thread busy.
    fn pump(&self) {
        let context = glib::MainContext::default();
        // Bounded so a storm of pending events cannot stall the operation.
        for _ in 0..32 {
            if !context.iteration(false) {
                break;
            }
        }
    }

    fn close(&self) {
        self.window.close();
        self.pump();
    }
}

/// Whether the desktop is currently in dark mode — upstream
/// `GMainWindow::CheckDarkMode` (`zuyu/src/yuzu/main.cpp`).
///
/// Upstream's Unix implementation compares the Qt palette's active Text and
/// Window colours and calls it dark when the text is brighter than the
/// background:
///
/// ```cpp
/// const QColor text_color = test_palette.color(QPalette::Active, QPalette::Text);
/// const QColor window_color = test_palette.color(QPalette::Active, QPalette::Window);
/// return (text_color.value() > window_color.value());
/// ```
///
/// GTK has no equivalent palette object to sample, but it does expose the
/// desktop's stated preference directly, which is the thing the Qt heuristic is
/// inferring. Two sources are consulted, most authoritative first:
///
///  1. the XDG desktop portal's `org.freedesktop.appearance` / `color-scheme`
///     key (`0` = no preference, `1` = prefer dark, `2` = prefer light), which
///     both GNOME and KDE publish;
///  2. the GTK theme name, treated as dark when it ends in `-dark`.
///
/// Returning `false` (light) when neither source answers matches upstream,
/// whose default theme on non-Windows is the light `DefaultColorful`.
pub fn check_dark_mode() -> bool {
    if let Some(prefers_dark) = portal_color_scheme() {
        return prefers_dark;
    }

    gtk::Settings::default()
        .and_then(|settings| settings.gtk_theme_name())
        .map(|name| name.to_lowercase().ends_with("-dark"))
        .unwrap_or(false)
}

/// Read `org.freedesktop.appearance` / `color-scheme` from the XDG desktop
/// portal. `None` when no portal is reachable (no session bus, no portal
/// service, or the key is absent).
fn portal_color_scheme() -> Option<bool> {
    let connection = gio::bus_get_sync(gio::BusType::Session, gio::Cancellable::NONE).ok()?;
    let reply = connection
        .call_sync(
            Some("org.freedesktop.portal.Desktop"),
            "/org/freedesktop/portal/desktop",
            "org.freedesktop.portal.Settings",
            "ReadOne",
            Some(&("org.freedesktop.appearance", "color-scheme").to_variant()),
            None,
            gio::DBusCallFlags::NONE,
            // The portal is local; a short timeout keeps startup from stalling
            // when the service is present but wedged.
            500,
            gio::Cancellable::NONE,
        )
        .ok()?;

    // `ReadOne` returns `(v)` wrapping a `u` colour-scheme code.
    let scheme: u32 = reply.child_value(0).as_variant()?.get()?;
    match scheme {
        1 => Some(true),  // prefer dark
        2 => Some(false), // prefer light
        _ => None,        // no preference — fall through to the theme name
    }
}

/// Apply the configured widget theme — upstream `GMainWindow::UpdateUITheme`.
///
/// Upstream loads a `.qss` stylesheet per theme, and for the two
/// system-following themes (`default` / `colorful`) additionally swaps to the
/// dark variant when [`check_dark_mode`] reports dark. GTK has no stylesheet
/// equivalent to load, so the ruzu port carries over the part that is
/// observable: whether the window renders light or dark.
///
/// Themes whose name marks them as dark force dark; the system-following ones
/// defer to the desktop, which is why ruzu (like yuzu) shows light on a light
/// Linux desktop and dark on a dark macOS one instead of hard-coding either.
pub fn update_ui_theme() {
    let Some(settings) = gtk::Settings::default() else {
        return;
    };

    let theme = crate::uisettings::with(|v| v.theme.get_value().clone());
    let internal = crate::uisettings::THEMES
        .iter()
        .find(|(name, internal)| *name == theme || *internal == theme)
        .map(|(_, internal)| *internal)
        // Upstream falls back to `default_theme`, which is `DefaultColorful`
        // on every non-Windows target.
        .unwrap_or("colorful");

    let dark = match internal {
        // The system-following themes: ask the desktop, as upstream's
        // `if (CheckDarkMode()) current_theme = "default_dark"` does.
        "default" | "colorful" => check_dark_mode(),
        // Every other theme upstream ships is a dark stylesheet.
        _ => true,
    };

    settings.set_gtk_application_prefer_dark_theme(dark);
    log::debug!(
        "UI theme '{internal}' resolved to {} mode",
        if dark { "dark" } else { "light" }
    );
}

/// Install the black backdrop CSS for the render page once.
fn install_render_bg_css() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let Some(display) = gtk::gdk::Display::default() else {
            return;
        };
        let provider = gtk::CssProvider::new();
        provider.load_from_data(".ruzu-render-bg { background-color: black; }");
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });
}

/// Build the GMenu model that mirrors upstream `main.ui`'s menu bar.
///
/// Qt `&` mnemonics become GTK `_` mnemonics. Qt separators (`<addaction
/// name="separator"/>`) map to GMenu `<section>` boundaries. Dynamically
/// populated menus upstream (Recent Files, Debugging) are declared but left
/// empty here.
fn build_menu_model() -> gio::MenuModel {
    let builder = gtk::Builder::from_string(MENU_UI);
    builder
        .object::<gio::MenuModel>("menubar")
        .expect("menubar object present in menu UI definition")
}

/// Every action name referenced by [`MENU_UI`], without the `app.` prefix.
/// Each is registered as a stub that logs its invocation.
const MENU_ACTION_NAMES: &[&str] = &[
    // File
    "install_file_nand",
    "load_file",
    "load_folder",
    "load_amiibo",
    "open_ruzu_folder",
    "exit",
    // Emulation
    "pause",
    "stop",
    "restart",
    "configure",
    "configure_current_game",
    // View
    "fullscreen",
    "single_window_mode",
    "display_dock_widget_headers",
    "show_filter_bar",
    "show_status_bar",
    "reset_window_size_720",
    "reset_window_size_900",
    "reset_window_size_1080",
    // Tools
    "install_keys",
    "install_firmware",
    "verify_installed_contents",
    "load_cabinet_nickname_owner",
    "load_cabinet_eraser",
    "load_cabinet_restorer",
    "load_cabinet_formatter",
    "load_album",
    "load_mii_edit",
    "open_controller_menu",
    "capture_screenshot",
    "tas_start",
    "tas_record",
    "tas_reset",
    "configure_tas",
    // Multiplayer
    "view_lobby",
    "start_room",
    "connect_to_room",
    "show_room",
    "leave_room",
    // Help
    "report_compatibility",
    "open_mods_page",
    "open_quickstart_guide",
    "open_faq",
    "about",
];

/// Register the menu actions as stubs on the application. Wiring them to real
/// behaviour is the next milestone (per-menu, on request).
fn register_menu_actions(app: &Application) {
    for &name in MENU_ACTION_NAMES {
        // Skip if already registered (both `activate` and `open` construct a
        // window, so this may run more than once per process).
        if app.lookup_action(name).is_some() {
            continue;
        }
        let action = gio::SimpleAction::new(name, None);
        let action_name = name.to_string();
        action.connect_activate(move |_, _| {
            log::info!("menu action '{action_name}' triggered (not yet wired)");
        });
        app.add_action(&action);
    }
}

/// Menu actions that require a running game — upstream `UpdateMenuState`'s
/// `running_actions` array.
const RUNNING_ACTIONS: &[&str] = &[
    "stop",
    "restart",
    "configure_current_game",
    "report_compatibility",
    "load_amiibo",
    "pause",
];

/// Menu actions that open a system applet, which upstream enables only when
/// firmware is installed *and* no game is running — `applet_actions`.
const APPLET_ACTIONS: &[&str] = &[
    "load_album",
    "load_cabinet_nickname_owner",
    "load_cabinet_eraser",
    "load_cabinet_restorer",
    "load_cabinet_formatter",
    "load_mii_edit",
    "open_controller_menu",
];

/// Enable/disable menu entries for the current emulation state — upstream
/// `GMainWindow::UpdateMenuState`.
///
/// `is_paused` mirrors upstream's `emu_thread == nullptr || !emu_thread->IsRunning()`,
/// which is why a *stopped* emulator counts as paused there too.
pub fn update_menu_state(app: &Application, emulation_running: bool, is_paused: bool) {
    let set_enabled = |name: &str, enabled: bool| {
        if let Some(action) = app.lookup_action(name) {
            if let Some(action) = action.downcast_ref::<gio::SimpleAction>() {
                action.set_enabled(enabled);
            }
        }
    };

    for &name in RUNNING_ACTIONS {
        set_enabled(name, emulation_running);
    }

    set_enabled("install_firmware", !emulation_running);
    set_enabled("install_keys", !emulation_running);

    let firmware_available = check_firmware_presence();
    for &name in APPLET_ACTIONS {
        set_enabled(name, firmware_available && !emulation_running);
    }

    set_enabled("capture_screenshot", emulation_running && !is_paused);
}

/// Whether system firmware is installed — upstream
/// `GMainWindow::CheckFirmwarePresence`, which asks the content provider for
/// the Mii Edit applet's program NCA.
///
/// The content provider is not reachable from the launcher yet, so this probes
/// the registered-contents directory instead: it is the same NAND location
/// upstream's provider indexes, and an empty one means no firmware either way.
fn check_firmware_presence() -> bool {
    let registered = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::NANDDir)
        .join("system/Contents/registered");
    std::fs::read_dir(registered)
        .map(|mut entries| entries.next().is_some())
        .unwrap_or(false)
}

/// GMenu UI definition mirroring upstream `main.ui`'s menu bar structure and
/// labels. App-name strings ("yuzu") are adapted to "ruzu".
const MENU_UI: &str = r##"<?xml version="1.0" encoding="UTF-8"?>
<interface>
  <menu id="menubar">
    <submenu>
      <attribute name="label" translatable="yes">_File</attribute>
      <section>
        <item>
          <attribute name="label" translatable="yes">_Install Files to NAND...</attribute>
          <attribute name="action">app.install_file_nand</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">L_oad File...</attribute>
          <attribute name="action">app.load_file</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Load _Folder...</attribute>
          <attribute name="action">app.load_folder</attribute>
        </item>
      </section>
      <section>
        <submenu>
          <attribute name="label" translatable="yes">_Recent Files</attribute>
        </submenu>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">Load/Remove _Amiibo...</attribute>
          <attribute name="action">app.load_amiibo</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">Open _ruzu Folder</attribute>
          <attribute name="action">app.open_ruzu_folder</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">E_xit</attribute>
          <attribute name="action">app.exit</attribute>
        </item>
      </section>
    </submenu>

    <submenu>
      <attribute name="label" translatable="yes">_Emulation</attribute>
      <section>
        <item>
          <attribute name="label" translatable="yes">_Pause</attribute>
          <attribute name="action">app.pause</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_Stop</attribute>
          <attribute name="action">app.stop</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_Restart</attribute>
          <attribute name="action">app.restart</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">Con_figure...</attribute>
          <attribute name="action">app.configure</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Configure C_urrent Game...</attribute>
          <attribute name="action">app.configure_current_game</attribute>
        </item>
      </section>
    </submenu>

    <submenu>
      <attribute name="label" translatable="yes">_View</attribute>
      <section>
        <item>
          <attribute name="label" translatable="yes">F_ullscreen</attribute>
          <attribute name="action">app.fullscreen</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Single _Window Mode</attribute>
          <attribute name="action">app.single_window_mode</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Display D_ock Widget Headers</attribute>
          <attribute name="action">app.display_dock_widget_headers</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Show _Filter Bar</attribute>
          <attribute name="action">app.show_filter_bar</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Show _Status Bar</attribute>
          <attribute name="action">app.show_status_bar</attribute>
        </item>
      </section>
      <section>
        <submenu>
          <attribute name="label" translatable="yes">_Reset Window Size</attribute>
          <section>
            <item>
              <attribute name="label" translatable="yes">Reset Window Size to _720p</attribute>
              <attribute name="action">app.reset_window_size_720</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">Reset Window Size to _900p</attribute>
              <attribute name="action">app.reset_window_size_900</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">Reset Window Size to _1080p</attribute>
              <attribute name="action">app.reset_window_size_1080</attribute>
            </item>
          </section>
        </submenu>
        <submenu>
          <attribute name="label" translatable="yes">_Debugging</attribute>
        </submenu>
      </section>
    </submenu>

    <submenu>
      <attribute name="label" translatable="yes">_Tools</attribute>
      <section>
        <item>
          <attribute name="label" translatable="yes">Install Decryption Keys</attribute>
          <attribute name="action">app.install_keys</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Install Firmware</attribute>
          <attribute name="action">app.install_firmware</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_Verify Installed Contents</attribute>
          <attribute name="action">app.verify_installed_contents</attribute>
        </item>
      </section>
      <section>
        <submenu>
          <attribute name="label" translatable="yes">_Amiibo</attribute>
          <section>
            <item>
              <attribute name="label" translatable="yes">_Set Nickname and Owner</attribute>
              <attribute name="action">app.load_cabinet_nickname_owner</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">_Delete Game Data</attribute>
              <attribute name="action">app.load_cabinet_eraser</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">_Restore Amiibo</attribute>
              <attribute name="action">app.load_cabinet_restorer</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">_Format Amiibo</attribute>
              <attribute name="action">app.load_cabinet_formatter</attribute>
            </item>
          </section>
        </submenu>
        <item>
          <attribute name="label" translatable="yes">Open _Album</attribute>
          <attribute name="action">app.load_album</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Open _Mii Editor</attribute>
          <attribute name="action">app.load_mii_edit</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Open _Controller Menu</attribute>
          <attribute name="action">app.open_controller_menu</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">_Capture Screenshot</attribute>
          <attribute name="action">app.capture_screenshot</attribute>
        </item>
        <submenu>
          <attribute name="label" translatable="yes">_TAS</attribute>
          <section>
            <item>
              <attribute name="label" translatable="yes">_Start</attribute>
              <attribute name="action">app.tas_start</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">R_ecord</attribute>
              <attribute name="action">app.tas_record</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">_Reset</attribute>
              <attribute name="action">app.tas_reset</attribute>
            </item>
          </section>
          <section>
            <item>
              <attribute name="label" translatable="yes">_Configure TAS...</attribute>
              <attribute name="action">app.configure_tas</attribute>
            </item>
          </section>
        </submenu>
      </section>
    </submenu>

    <submenu>
      <attribute name="label" translatable="yes">_Multiplayer</attribute>
      <section>
        <item>
          <attribute name="label" translatable="yes">_Browse Public Game Lobby</attribute>
          <attribute name="action">app.view_lobby</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_Create Room</attribute>
          <attribute name="action">app.start_room</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_Direct Connect to Room</attribute>
          <attribute name="action">app.connect_to_room</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">_Show Current Room</attribute>
          <attribute name="action">app.show_room</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_Leave Room</attribute>
          <attribute name="action">app.leave_room</attribute>
        </item>
      </section>
    </submenu>

    <submenu>
      <attribute name="label" translatable="yes">_Help</attribute>
      <section>
        <item>
          <attribute name="label" translatable="yes">_Report Compatibility</attribute>
          <attribute name="action">app.report_compatibility</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Open _Mods Page</attribute>
          <attribute name="action">app.open_mods_page</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">Open _Quickstart Guide</attribute>
          <attribute name="action">app.open_quickstart_guide</attribute>
        </item>
        <item>
          <attribute name="label" translatable="yes">_FAQ</attribute>
          <attribute name="action">app.open_faq</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name="label" translatable="yes">_About ruzu</attribute>
          <attribute name="action">app.about</attribute>
        </item>
      </section>
    </submenu>
  </menu>
</interface>
"##;
