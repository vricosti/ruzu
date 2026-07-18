// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of the upstream `GMainWindow` class defined in
// `/Users/vricosti/Dev/emulators/zuyu/src/yuzu/main.cpp` + `main.h`, whose
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
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};

use gtk::prelude::*;
use gtk::{gio, glib, Application, ApplicationWindow};

use ruzu_core::frontend::framebuffer_layout::{default_frame_layout, FramebufferLayout};

use crate::boot::EmulationSession;
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

/// The main launcher window.
///
/// Upstream `GMainWindow` derives from `QMainWindow`; here we wrap a
/// `gtk::ApplicationWindow`. Kept as a thin newtype so future state (game list
/// model, status labels, emulation handles) can hang off it the way the
/// upstream class members do.
pub struct GMainWindow {
    window: ApplicationWindow,
    /// Central stack swapping between the game list, loading screen, and render
    /// view (upstream swaps `centralwidget`).
    stack: gtk::Stack,
    /// Shader-loading progress UI (upstream `LoadingScreen`).
    loading_screen: Rc<LoadingScreen>,
    /// The active emulation session, if a game is running (upstream keeps the
    /// `System` + emu thread on `GMainWindow`).
    session: RefCell<Option<EmulationSession>>,
    /// Bottom status bar (renderer / accuracy / dock / filter / AA / volume).
    status_bar: StatusBar,
    /// Native render-window handles for the running game, so it can be resized
    /// when the GTK window resizes.
    render: RefCell<Option<RenderHandles>>,
    /// Last observed central-stack size, to detect resizes.
    render_size: Cell<(i32, i32)>,
}

/// Handles needed to resize the embedded render surface on window resize.
struct RenderHandles {
    child_window: usize,
    metal_layer: usize,
    /// Shared frame layout the renderer reads; updated so the frame is rendered
    /// at the new native resolution on resize (upstream `OnFramebufferSizeChanged`).
    framebuffer_layout: Arc<RwLock<FramebufferLayout>>,
}

impl GMainWindow {
    /// Construct and lay out the main window. Mirrors the body of the upstream
    /// `GMainWindow::GMainWindow` constructor (widget creation + `Initialize*`
    /// calls), minus the not-yet-ported subsystems.
    pub fn new(app: &Application) -> Rc<Self> {
        let window = ApplicationWindow::builder()
            .application(app)
            .title(WINDOW_TITLE)
            .default_width(DEFAULT_WIDTH)
            .default_height(DEFAULT_HEIGHT)
            .build();

        // Root vertical layout. On macOS the menu bar lives in the native
        // global menu bar (installed once via `init_app_menu` on the
        // application's `startup`), so the window itself only holds the central
        // stack and the status bar — no in-window `PopoverMenuBar`.
        let root = gtk::Box::new(gtk::Orientation::Vertical, 0);

        // --- Central stack (upstream `centralwidget`) ------------------------
        // Pages: game list, loading screen, (later) render view.
        let stack = gtk::Stack::new();
        stack.set_hexpand(true);
        stack.set_vexpand(true);

        let game_list_placeholder = gtk::Label::builder()
            .label("Game list — not yet implemented")
            .hexpand(true)
            .vexpand(true)
            .build();
        game_list_placeholder.add_css_class("dim-label");
        stack.add_named(&game_list_placeholder, Some(PAGE_GAME_LIST));

        let loading_screen = Rc::new(LoadingScreen::new());
        stack.add_named(loading_screen.widget(), Some(PAGE_LOADING));

        // Black backdrop page (behind the native render window during a game).
        install_render_bg_css();
        let render_bg = gtk::Box::new(gtk::Orientation::Vertical, 0);
        render_bg.add_css_class("ruzu-render-bg");
        render_bg.set_hexpand(true);
        render_bg.set_vexpand(true);
        stack.add_named(&render_bg, Some(PAGE_RENDER));

        stack.set_visible_child_name(PAGE_GAME_LIST);
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
            stack,
            loading_screen,
            session: RefCell::new(None),
            status_bar,
            render: RefCell::new(None),
            render_size: Cell::new((0, 0)),
        });

        // Keep the embedded render surface sized to the central stack as the
        // window is resized. GTK4 has no widget `size-allocate` signal, so poll
        // the stack size on the frame clock and act only on change.
        #[cfg(target_os = "macos")]
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

        // Stop the emulation session when the window is closing, *before* GTK
        // tears down the native surface — otherwise the GPU thread keeps
        // presenting into a destroyed Metal layer and crashes. If a game is
        // running, `stop()` lets the boot thread shut down and `process::exit`,
        // so the close handler doesn't return (the process is already gone).
        this.window.connect_close_request(glib::clone!(
            #[weak(rename_to = w)]
            this,
            #[upgrade_or]
            glib::Propagation::Proceed,
            move |_| {
                if let Some(mut session) = w.session.borrow_mut().take() {
                    session.stop();
                }
                glib::Propagation::Proceed
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
        let filters = gio::ListStore::new::<gtk::FileFilter>();
        filters.append(&filter);
        filters.append(&all_files);

        let dialog = gtk::FileDialog::builder()
            .title("Load File")
            .filters(&filters)
            .default_filter(&filter)
            .modal(true)
            .build();

        dialog.open(
            Some(&self.window),
            gio::Cancellable::NONE,
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |result| match result {
                    Ok(file) => {
                        if let Some(path) = file.path() {
                            this.boot_game(path.to_string_lossy().into_owned());
                        }
                    }
                    Err(err) => log::debug!("Load File cancelled: {err}"),
                }
            ),
        );
    }

    /// Upstream `OnMenuLoadFolder`: choose an extracted-ROM directory and boot
    /// its `main` file.
    fn on_menu_load_folder(self: &Rc<Self>) {
        let dialog = gtk::FileDialog::builder()
            .title("Open Extracted ROM Directory")
            .modal(true)
            .build();

        dialog.select_folder(
            Some(&self.window),
            gio::Cancellable::NONE,
            glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |result| {
                    let Ok(dir) = result else { return };
                    let Some(dir_path) = dir.path() else { return };
                    let main = dir_path.join("main");
                    if main.is_file() {
                        this.boot_game(main.to_string_lossy().into_owned());
                    } else {
                        let alert = gtk::AlertDialog::builder()
                            .modal(true)
                            .message("Invalid Directory Selected")
                            .detail(
                                "The directory you have selected does not contain a 'main' file.",
                            )
                            .build();
                        alert.show(Some(&this.window));
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

        // Progress is produced on the boot thread and consumed on the GTK main
        // thread via a shared slot polled by a timeout (cross-thread marshaling,
        // the same shape as yuzu's queued LoadProgress signal).
        let slot: Arc<Mutex<Option<(LoadStage, usize, usize)>>> = Arc::new(Mutex::new(None));
        let slot_producer = Arc::clone(&slot);
        let progress: crate::boot::ProgressFn = Box::new(move |stage, value, total| {
            *slot_producer.lock().unwrap() = Some((stage, value, total));
        });

        let loading = Rc::clone(&self.loading_screen);
        let stack = self.stack.clone();
        glib::timeout_add_local(std::time::Duration::from_millis(30), move || {
            let update = slot.lock().unwrap().take();
            if let Some((stage, value, total)) = update {
                loading.on_load_progress(stage, value, total);
                if stage == LoadStage::Complete {
                    // Switch the backdrop to the black render page so a resize
                    // exposes black (not the light loading screen), then reveal
                    // the native render window over it.
                    stack.set_visible_child_name(PAGE_RENDER);
                    crate::render_window::set_render_view_hidden(child_window as *mut _, false);
                    return glib::ControlFlow::Break;
                }
            }
            glib::ControlFlow::Continue
        });

        let session = crate::boot::boot_game(
            window_info,
            drawable_size,
            shown_state,
            framebuffer_layout,
            filepath,
            progress,
        );
        *self.session.borrow_mut() = Some(session);
    }

    /// In-process boot currently requires the macOS CAMetalLayer bridge.
    #[cfg(not(target_os = "macos"))]
    pub fn boot_game(self: &Rc<Self>, _filepath: String) {
        log::error!("In-process boot is only implemented on macOS for now");
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
