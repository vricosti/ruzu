// SPDX-License-Identifier: GPL-3.0-or-later
//
// Linux/X11 render surface — the counterpart of upstream's `RenderWidget`
// (`/home/vricosti/Dev/emulators/zuyu/src/yuzu/bootmanager.cpp`) and of the
// `render_window.rs` CAMetalLayer bridge used on macOS.
//
// Upstream embeds a *native child window* inside `GRenderWindow`'s layout:
//
// ```cpp
// class RenderWidget : public QWidget {
//     explicit RenderWidget(GRenderWindow* parent) : QWidget(parent) {
//         setAttribute(Qt::WA_NativeWindow);
//         setAttribute(Qt::WA_PaintOnScreen);
//         ...
// ```
//
// and then hands that child's native handle to the renderer — per platform, in
// `qt_common.cpp`'s `GetWindowSystemInfo`:
//
// ```cpp
// wsi.display_connection = pni->nativeResourceForWindow("display", window);
// wsi.render_surface     = reinterpret_cast<void*>(window->winId());   // X11
// ```
//
// Qt creates that native child for you. GTK4 has no equivalent — its widgets
// are all drawn into the toplevel's single surface — so the child `Window` is
// created directly with Xlib here, parented to the GTK toplevel's XID and moved
// to cover the render area. That is the same shape as the macOS path, which
// likewise creates its own child window/layer rather than reusing the
// toplevel's.
//
// Only X11 is handled. Under Wayland the equivalent needs a `wl_subsurface`,
// which GTK4 does not expose; running with `GDK_BACKEND=x11` (XWayland) works
// on a Wayland session in the meantime.

use std::ffi::c_void;

use gtk::prelude::*;

use x11::xlib;

/// A native X11 child window covering the render area.
///
/// Mirrors `EmbeddedMetalLayer` on macOS so `main_window` can treat both the
/// same way.
pub struct EmbeddedX11Window {
    /// `Display*` — pass as `WindowSystemInfo::display_connection`.
    pub display: *mut c_void,
    /// The child `Window` XID — pass as `WindowSystemInfo::render_surface`.
    pub window: u64,
    /// Size in physical pixels.
    pub drawable_size: (u32, u32),
    /// Scale factor, for `render_surface_scale`.
    pub scale: f32,
}

/// Create the child render window inside `window`, covering `gtk_render_rect`
/// (GTK coordinates within the toplevel; `None` covers the whole window).
///
/// Returns `None` when the GTK window is not realized or is not an X11 surface.
pub fn attach_render_window(
    window: &gtk::Window,
    gtk_render_rect: Option<(f64, f64, f64, f64)>,
) -> Option<EmbeddedX11Window> {
    let surface = window.surface()?;

    // GTK must be on the X11 backend; under a native Wayland surface there is
    // no XID to parent to.
    let x11_surface = surface.downcast_ref::<gdk4_x11::X11Surface>()?;
    let parent_xid = x11_surface.xid();

    let display = surface.display();
    let x11_display = display.downcast_ref::<gdk4_x11::X11Display>()?;
    // SAFETY: the display is owned by GDK and outlives the child window we
    // create under it; we only use the pointer while `window` is alive.
    let xdisplay = unsafe { x11_display.xdisplay() };
    if xdisplay.is_null() || parent_xid == 0 {
        return None;
    }

    let scale = surface.scale_factor().max(1) as f32;
    let (x, y, width, height) =
        gtk_render_rect.unwrap_or((0.0, 0.0, window.width() as f64, window.height() as f64));
    // Zero-sized windows are invalid in X and would fail the swapchain later.
    let width = (width.max(1.0) * scale as f64) as u32;
    let height = (height.max(1.0) * scale as f64) as u32;

    let child = unsafe {
        let child = xlib::XCreateSimpleWindow(
            xdisplay,
            parent_xid,
            (x * scale as f64) as i32,
            (y * scale as f64) as i32,
            width,
            height,
            0,
            0,
            // Black background, so an unpainted frame matches the render page's
            // backdrop rather than flashing white.
            xlib::XBlackPixel(xdisplay, xlib::XDefaultScreen(xdisplay)),
        );
        if child == 0 {
            return None;
        }
        // The renderer paints this window; X must not send it Expose events we
        // would have to service, and the child starts hidden so the loading
        // screen shows through until `set_render_window_hidden(.., false)`.
        xlib::XSelectInput(xdisplay, child, 0);
        xlib::XFlush(xdisplay);
        child
    };

    log::info!(
        "Embedded X11 render window 0x{child:x} in parent 0x{parent_xid:x} \
         ({width}x{height} @ {scale}x)"
    );

    Some(EmbeddedX11Window {
        display: xdisplay as *mut c_void,
        window: child,
        drawable_size: (width, height),
        scale,
    })
}

/// Show or hide the child render window.
///
/// The macOS path fades its child `NSWindow` via `setAlphaValue`; X11 has no
/// per-window alpha here, so the window is mapped/unmapped instead — the same
/// observable effect.
pub fn set_render_window_hidden(display: *mut c_void, window: u64, hidden: bool) {
    if display.is_null() || window == 0 {
        return;
    }
    let xdisplay = display as *mut xlib::Display;
    unsafe {
        if hidden {
            xlib::XUnmapWindow(xdisplay, window);
        } else {
            xlib::XMapWindow(xdisplay, window);
        }
        xlib::XFlush(xdisplay);
    }
}

/// Move and resize the child window to cover `gtk_render_rect`, returning the
/// new drawable size in physical pixels so the caller can rebuild the frame
/// layout (upstream `OnFramebufferSizeChanged`).
pub fn resize_render_window(
    window: &gtk::Window,
    display: *mut c_void,
    child: u64,
    gtk_render_rect: (f64, f64, f64, f64),
) -> Option<(u32, u32)> {
    if display.is_null() || child == 0 {
        return None;
    }
    let scale = window
        .surface()
        .map(|s| s.scale_factor())
        .unwrap_or(1)
        .max(1) as f64;
    let (x, y, width, height) = gtk_render_rect;
    let width = (width.max(1.0) * scale) as u32;
    let height = (height.max(1.0) * scale) as u32;

    let xdisplay = display as *mut xlib::Display;
    unsafe {
        xlib::XMoveResizeWindow(
            xdisplay,
            child,
            (x * scale) as i32,
            (y * scale) as i32,
            width,
            height,
        );
        xlib::XFlush(xdisplay);
    }
    Some((width, height))
}

/// Destroy the child window. Called when emulation stops, so a subsequent boot
/// starts from a fresh surface rather than reusing one whose swapchain is gone.
pub fn destroy_render_window(display: *mut c_void, window: u64) {
    if display.is_null() || window == 0 {
        return;
    }
    let xdisplay = display as *mut xlib::Display;
    unsafe {
        xlib::XDestroyWindow(xdisplay, window);
        xlib::XFlush(xdisplay);
    }
}
