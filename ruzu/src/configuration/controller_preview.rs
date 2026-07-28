// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/Cairo counterpart of upstream's `PlayerControlPreview`
// (`/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_input_player_widget.cpp`),
// the widget that draws the controller picture in the middle of the Controls
// page.
//
// Upstream renders each controller with `QPainter` from flat vertex arrays
// (shell, triggers, glyphs). Those arrays are reproduced verbatim in
// [`super::controller_outlines`]; this module is the painter half — the
// `Draw*Controller` / `Draw*Body` entry points and the primitive helpers they
// build on (`DrawPolygon`, `DrawCircleButton`, `DrawArrowButton`, …).
//
// Upstream also animates the drawing: pressed buttons fill with a highlight
// colour and the sticks follow live input, refreshed from a 40 ms timer. That
// needs the polling loop hooked to the widget; the shapes and layout come
// first, so buttons are drawn in their released colour for now.

use gtk::cairo;
use gtk::prelude::*;

use common::settings_input::ControllerType;

use super::controller_outlines as art;

/// Size reserved for the preview.
///
/// Upstream's `controllerFrame` has an Expanding size policy and no minimum, so
/// it simply takes whatever the dialog's centre column leaves it. The vertex
/// data is in absolute pixels though, and the tallest drawing — a single
/// Joy-Con, whose ZL arc reaches y = -204 and whose stick readout reaches
/// y = +135 — needs about 340px around the centre. Anything smaller clips it,
/// so the request below is sized from the artwork rather than left to chance.
const PREVIEW_WIDTH: i32 = 560;
const PREVIEW_HEIGHT: i32 = 460;

/// Upstream's palette, from `PlayerControlPreview::UpdateColors`, light theme.
///
/// The dark-theme branch swaps `primary`/`button`/`outline`; both are provided
/// so the preview follows the launcher theme the way upstream follows the Qt
/// icon theme.
struct Colors {
    outline: (f64, f64, f64),
    primary: (f64, f64, f64),
    left: (f64, f64, f64),
    right: (f64, f64, f64),
    button: (f64, f64, f64),
    button2: (f64, f64, f64),
    font: (f64, f64, f64),
    font2: (f64, f64, f64),
    highlight: (f64, f64, f64),
    highlight2: (f64, f64, f64),
    indicator: (f64, f64, f64),
    indicator2: (f64, f64, f64),
    led_on: (f64, f64, f64),
    led_off: (f64, f64, f64),
    slider: (f64, f64, f64),
    slider_button: (f64, f64, f64),
    slider_arrow: (f64, f64, f64),
    deadzone: (f64, f64, f64),
    charging: (f64, f64, f64),
}

impl Colors {
    /// Upstream's `else` branch — the light theme.
    fn light() -> Self {
        let primary = rgb(225, 225, 225);
        Self {
            outline: rgb(0, 0, 0),
            primary,
            // Upstream sets `left`/`right` from the emulated controller body
            // colour and falls back to `primary` when both are zero, which is
            // the case for the default (unconfigured) player.
            left: primary,
            right: primary,
            button: rgb(109, 111, 114),
            button2: rgb(77, 80, 84),
            font: rgb(255, 255, 255),
            font2: rgb(0, 0, 0),
            highlight: rgb(170, 0, 0),
            highlight2: rgb(119, 0, 0),
            indicator: rgb(0, 0, 200),
            indicator2: rgb(59, 165, 93),
            led_on: rgb(255, 255, 0),
            led_off: rgb(170, 238, 255),
            slider: rgb(103, 106, 110),
            slider_button: rgb(153, 149, 149),
            slider_arrow: rgb(65, 68, 73),
            deadzone: rgb(170, 0, 0),
            charging: rgb(250, 168, 26),
        }
    }

    /// Upstream's `themeName().contains("dark")` branch.
    fn dark() -> Self {
        let primary = rgb(204, 204, 204);
        let button = rgb(35, 38, 41);
        Self {
            outline: rgb(160, 160, 160),
            primary,
            left: primary,
            right: primary,
            button,
            button2: rgb(26, 27, 30),
            font: rgb(255, 255, 255),
            font2: rgb(255, 255, 255),
            highlight: rgb(170, 0, 0),
            highlight2: rgb(119, 0, 0),
            indicator: rgb(170, 238, 255),
            indicator2: rgb(59, 165, 93),
            led_on: rgb(255, 255, 0),
            led_off: rgb(170, 238, 255),
            slider: rgb(103, 106, 110),
            // Upstream: `colors.slider_button = colors.button;`
            slider_button: button,
            slider_arrow: rgb(14, 15, 18),
            deadzone: rgb(204, 136, 136),
            charging: rgb(250, 168, 26),
        }
    }
}

fn rgb(r: u8, g: u8, b: u8) -> (f64, f64, f64) {
    (r as f64 / 255.0, g as f64 / 255.0, b as f64 / 255.0)
}

/// Direction of an arrow or trigger, upstream's `Direction` enum.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
    /// Upstream's `Direction::None` — used by `DrawRoundButton` callers that
    /// want no press displacement.
    None,
}

/// Render `controller_type` onto `cr`, centred at `center`.
///
/// Split out of the draw callback so the drawing can be exercised without a
/// widget — see the tests, which render each type to an image surface.
pub fn draw(cr: &cairo::Context, center: (f64, f64), controller_type: ControllerType, dark: bool) {
    let colors = if dark { Colors::dark() } else { Colors::light() };
    cr.set_line_width(1.0);
    match controller_type {
        ControllerType::ProController => draw_pro_controller(cr, center, &colors),
        ControllerType::DualJoyconDetached => draw_dual_controller(cr, center, &colors),
        ControllerType::LeftJoycon => draw_left_controller(cr, center, &colors),
        ControllerType::RightJoycon => draw_right_controller(cr, center, &colors),
        ControllerType::Handheld => draw_handheld_controller(cr, center, &colors),
        ControllerType::GameCube => draw_gc_controller(cr, center, &colors),
        // Upstream falls back to the Pro drawing for the remaining types.
        _ => draw_pro_controller(cr, center, &colors),
    }
}

/// Build the preview widget for `controller_type`.
///
/// Upstream keeps one `PlayerControlPreview` and switches what it draws from
/// `SetConnectedStatus` / the controller-type combo; here the caller rebuilds
/// the drawing area, which has the same effect and keeps the state in one place.
pub fn build(controller_type: ControllerType) -> gtk::DrawingArea {
    let area = gtk::DrawingArea::new();
    area.set_content_width(PREVIEW_WIDTH);
    area.set_content_height(PREVIEW_HEIGHT);
    area.set_hexpand(true);
    area.set_valign(gtk::Align::Center);

    area.set_draw_func(move |widget, cr, width, height| {
        // Upstream draws around the widget centre, in the same coordinate
        // space as the vertex data.
        let center = (width as f64 / 2.0, height as f64 / 2.0);
        draw(cr, center, controller_type, is_dark(widget));
    });

    area
}

/// Whether the widget renders on a dark theme, mirroring upstream's
/// `QIcon::themeName().contains("dark")` test.
fn is_dark(widget: &gtk::DrawingArea) -> bool {
    widget
        .settings()
        .is_gtk_application_prefer_dark_theme()
}

// ---------------------------------------------------------------------------
// Primitives — upstream's Draw* helpers
// ---------------------------------------------------------------------------

/// Upstream `DrawPolygon`: the vertex list offset by `center`, closed.
fn polygon(cr: &cairo::Context, center: (f64, f64), points: art::Outline, mirror_x: bool) {
    if points.len() < 4 {
        return;
    }
    let sign = if mirror_x { -1.0 } else { 1.0 };
    for (index, pair) in points.chunks_exact(2).enumerate() {
        let x = center.0 + sign * pair[0] as f64;
        let y = center.1 + pair[1] as f64;
        if index == 0 {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
}

/// A symmetric shell outline: the data is only the left half, and the right
/// half is its mirror walked back the other way.
///
/// Upstream fills one array from both ends to the same effect:
///
/// ```cpp
/// qbody[point]                       = center + QPointF( body_x, body_y);
/// qbody[pro_body.size() - 1 - point] = center + QPointF(-body_x, body_y);
/// ```
///
/// Drawing only the listed points would leave the shell open down the middle
/// and give the controller one straight edge.
fn mirrored_polygon(cr: &cairo::Context, center: (f64, f64), points: art::Outline) {
    let pairs: Vec<(f64, f64)> = points
        .chunks_exact(2)
        .map(|p| (p[0] as f64, p[1] as f64))
        .collect();
    if pairs.len() < 2 {
        return;
    }
    for (index, (x, y)) in pairs.iter().enumerate() {
        let (px, py) = (center.0 + x, center.1 + y);
        if index == 0 {
            cr.move_to(px, py);
        } else {
            cr.line_to(px, py);
        }
    }
    for (x, y) in pairs.iter().rev() {
        cr.line_to(center.0 - x, center.1 + y);
    }
    cr.close_path();
}

/// A shell outline scaled and offset the way upstream's `Draw*Body` does.
///
/// The vertex arrays are stored in their own units and each body applies its
/// own `size` / `offset` before drawing — e.g. `DrawLeftBody` uses
/// `size = 1.78, offset = 312.39` for the shell and `size2 = 1.1115,
/// offset2 = 335` for the side view, and `DrawDualBody` uses
/// `size = 1.61, offset = 209.3` mirrored for the two halves. Drawing the raw
/// data at scale 1 leaves the shell detached from its buttons.
fn placed_polygon(
    cr: &cairo::Context,
    center: (f64, f64),
    points: art::Outline,
    scale: f64,
    offset: (f64, f64),
    mirror_x: bool,
) {
    if points.len() < 4 {
        return;
    }
    let sign = if mirror_x { -1.0 } else { 1.0 };
    for (index, pair) in points.chunks_exact(2).enumerate() {
        let x = center.0 + sign * (pair[0] as f64 * scale + offset.0);
        let y = center.1 + pair[1] as f64 * scale + offset.1;
        if index == 0 {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
}

/// Fill then stroke, the effect of Qt's brush + pen pair.
fn fill_stroke(cr: &cairo::Context, fill: (f64, f64, f64), outline: (f64, f64, f64)) {
    cr.set_source_rgb(fill.0, fill.1, fill.2);
    let _ = cr.fill_preserve();
    cr.set_source_rgb(outline.0, outline.1, outline.2);
    let _ = cr.stroke();
}

/// Upstream `DrawCircleButton`.
fn circle_button(
    cr: &cairo::Context,
    center: (f64, f64),
    radius: f64,
    fill: (f64, f64, f64),
    outline: (f64, f64, f64),
) {
    cr.arc(center.0, center.1, radius, 0.0, std::f64::consts::TAU);
    fill_stroke(cr, fill, outline);
}

/// Upstream `DrawCircle` — outline only.
fn circle(cr: &cairo::Context, center: (f64, f64), radius: f64, color: (f64, f64, f64)) {
    cr.arc(center.0, center.1, radius, 0.0, std::f64::consts::TAU);
    cr.set_source_rgb(color.0, color.1, color.2);
    let _ = cr.stroke();
}

/// Upstream `DrawRectangle`, centred on `center`.
///
/// `p.drawRect` paints with the *current pen and brush*, so this fills and
/// strokes — a fill-only version loses the outline upstream draws around the
/// minus/plus bars and the LED cells.
fn rectangle(
    cr: &cairo::Context,
    center: (f64, f64),
    width: f64,
    height: f64,
    fill: (f64, f64, f64),
    outline: (f64, f64, f64),
) {
    cr.rectangle(
        center.0 - width / 2.0,
        center.1 - height / 2.0,
        width,
        height,
    );
    fill_stroke(cr, fill, outline);
}

/// Upstream `DrawArrowButton`: the `up_arrow_button` outline rotated into
/// `direction`. Upstream rotates by swapping and negating the coordinates
/// rather than using a transform, and the mapping is reproduced exactly.
fn arrow_button(
    cr: &cairo::Context,
    center: (f64, f64),
    direction: Direction,
    size: f64,
    fill: (f64, f64, f64),
    outline: (f64, f64, f64),
) {
    for (index, pair) in art::UP_ARROW_BUTTON.chunks_exact(2).enumerate() {
        let ax = pair[0] as f64 * size;
        let ay = pair[1] as f64 * size;
        let (dx, dy) = match direction {
            Direction::Up => (ax, ay),
            Direction::Right => (-ay, ax),
            Direction::Down => (ax, -ay),
            Direction::Left => (ay, ax),
            Direction::None => (ax, ay),
        };
        let (x, y) = (center.0 + dx, center.1 + dy);
        if index == 0 {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
    fill_stroke(cr, fill, outline);
}

/// Upstream `DrawSymbol`: a glyph outline scaled about `center`.
fn symbol(
    cr: &cairo::Context,
    center: (f64, f64),
    glyph: art::Outline,
    size: f64,
    color: (f64, f64, f64),
) {
    for (index, pair) in glyph.chunks_exact(2).enumerate() {
        let x = center.0 + pair[0] as f64 * size;
        let y = center.1 + pair[1] as f64 * size;
        if index == 0 {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
    cr.set_source_rgb(color.0, color.1, color.2);
    let _ = cr.fill();
}

/// `DrawPolygon` over a slice of the vertex list.
///
/// `DrawHandheldBody` builds several arrays from the same source data with
/// different bounds — the filled body uses every vertex while its outline stops
/// six points short, so the seam where the shell meets the Joy-Cons is filled
/// but not stroked.
fn polygon_range(
    cr: &cairo::Context,
    center: (f64, f64),
    points: art::Outline,
    start: usize,
    end: usize,
) {
    let end = end.min(points.len() / 2);
    if end <= start {
        return;
    }
    for index in start..end {
        let x = center.0 + points[index * 2] as f64;
        let y = center.1 + points[index * 2 + 1] as f64;
        if index == start {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
}

/// `DrawSymbol` with the painter rotated half a turn.
///
/// Upstream spells this out as `p.rotate(-180); DrawSymbol(p, -center + ...);
/// p.rotate(180);` around the right Joy-Con's SL/SR labels, which reads them
/// the right way up when the rail is held the other way round.
fn symbol_rotated_180(
    cr: &cairo::Context,
    center: (f64, f64),
    glyph: art::Outline,
    size: f64,
    color: (f64, f64, f64),
) {
    for (index, pair) in glyph.chunks_exact(2).enumerate() {
        let x = center.0 - pair[0] as f64 * size;
        let y = center.1 - pair[1] as f64 * size;
        if index == 0 {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
    cr.set_source_rgb(color.0, color.1, color.2);
    let _ = cr.fill();
}

/// Upstream `DrawTriggerButton`: the `trigger_button` outline, mirrored for the
/// right-hand side.
fn trigger_button(
    cr: &cairo::Context,
    center: (f64, f64),
    direction: Direction,
    fill: (f64, f64, f64),
    outline: (f64, f64, f64),
) {
    let mirror = direction == Direction::Right;
    polygon(cr, center, art::TRIGGER_BUTTON, mirror);
    fill_stroke(cr, fill, outline);
}

/// Upstream `DrawProJoystick`: the stick well plus its cap.
fn pro_joystick(
    cr: &cairo::Context,
    center: (f64, f64),
    radius: f64,
    colors: &Colors,
) {
    // Outer ring, upstream's `radius1 = 32` circle drawn by `DrawProBody`.
    circle(cr, center, 32.0, colors.outline);
    // Stick cap.
    circle_button(cr, center, radius + 13.0, colors.button, colors.outline);
    circle_button(cr, center, radius, colors.slider_button, colors.outline);
}

/// Upstream `DrawRawJoystick`: the dotted range indicators below the pad.
/// Upstream `DrawRawJoystick`: the stick range readout under the pad.
///
/// Upstream takes two `QPointF` and skips a side whose centre is exactly
/// `QPointF(0, 0)` — the single Joy-Cons pass that literal for the side they do
/// not have. `Option` says the same thing without the sentinel.
fn raw_joystick(
    cr: &cairo::Context,
    left: Option<(f64, f64)>,
    right: Option<(f64, f64)>,
    colors: &Colors,
) {
    // Upstream draws the right-hand side first.
    for center in [right, left].into_iter().flatten() {
        joystick_properties(cr, center, colors);

        // `DrawJoystickDot` twice: the raw value in `indicator`, then the
        // calibrated one in `indicator2`. Both rest at the centre with no
        // device attached.
        circle(cr, center, 2.0, colors.indicator);
        circle(cr, center, 2.0, colors.indicator2);
    }
}

/// Upstream `DrawJoystickProperties`: a dotted range circle and, inside it, the
/// deadzone circle.
///
/// The radii scale with the analog properties of the bound stick; the defaults
/// are `range = 1.0` and `deadzone = 0.0`, so the deadzone circle collapses to
/// a point until a stick is actually mapped.
fn joystick_properties(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    const SIZE: f64 = 45.0;
    const RANGE: f64 = 1.0;
    const DEADZONE: f64 = 0.0;

    cr.save().ok();
    cr.set_dash(&[1.0, 3.0], 0.0);
    circle(cr, center, SIZE * RANGE, colors.outline);
    circle(cr, center, SIZE * DEADZONE, colors.deadzone);
    cr.restore().ok();
}

// ---------------------------------------------------------------------------
// Per-controller drawings
// ---------------------------------------------------------------------------

/// Upstream `DrawProController` + `DrawProBody`.
fn draw_pro_controller(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    let at = |dx: f64, dy: f64| (center.0 + dx, center.1 + dy);

    // Triggers sit behind the shell.
    polygon(cr, center, art::PRO_LEFT_TRIGGER, false);
    fill_stroke(cr, colors.button, colors.outline);
    polygon(cr, center, art::PRO_LEFT_TRIGGER, true);
    fill_stroke(cr, colors.button, colors.outline);

    // Body: the handles are mirrored halves of one outline, then the shell.
    polygon(cr, center, art::PRO_LEFT_HANDLE, false);
    fill_stroke(cr, colors.primary, colors.outline);
    polygon(cr, center, art::PRO_LEFT_HANDLE, true);
    fill_stroke(cr, colors.primary, colors.outline);
    mirrored_polygon(cr, center, art::PRO_BODY);
    fill_stroke(cr, colors.primary, colors.outline);

    // Sticks — upstream's offsets.
    pro_joystick(cr, at(-111.0, -55.0), 11.0, colors);
    pro_joystick(cr, at(51.0, 0.0), 11.0, colors);
    raw_joystick(cr, Some(at(-50.0, 105.0)), Some(at(50.0, 105.0)), colors);

    // Face buttons, upstream's `face_center`, distance 31, radius 15.
    let face = at(105.0, -56.0);
    let face_at = |dx: f64, dy: f64| (face.0 + dx, face.1 + dy);
    for (offset, glyph) in [
        ((31.0, 0.0), art::SYMBOL_A),
        ((0.0, 31.0), art::SYMBOL_B),
        ((0.0, -31.0), art::SYMBOL_X),
        ((-31.0, 0.0), art::SYMBOL_Y),
    ] {
        let position = face_at(offset.0, offset.1);
        circle_button(cr, position, 15.0, colors.button, colors.outline);
        symbol(cr, position, glyph, 1.5, (1.0, 1.0, 1.0));
    }

    // D-pad.
    let dpad = at(-61.0, 0.0);
    for direction in [
        Direction::Up,
        Direction::Left,
        Direction::Right,
        Direction::Down,
    ] {
        arrow_button(cr, dpad, direction, 1.0, colors.button, colors.outline);
    }

    // ZL / ZR, drawn detached above the shoulders as upstream does.
    for (offset, direction, glyph) in [
        ((-210.0, -120.0), Direction::Left, art::SYMBOL_ZL),
        ((210.0, -120.0), Direction::Right, art::SYMBOL_ZR),
    ] {
        let position = at(offset.0, offset.1);
        trigger_button(cr, position, direction, colors.button, colors.outline);
        symbol(cr, position, glyph, 1.5, (1.0, 1.0, 1.0));
    }

    // Minus and Plus.
    circle_button(cr, at(-50.0, -86.0), 9.0, colors.button, colors.outline);
    circle_button(cr, at(50.0, -86.0), 9.0, colors.button, colors.outline);
    rectangle(cr, at(-50.0, -86.0), 9.0, 1.5, colors.font2, colors.font2);
    rectangle(cr, at(50.0, -86.0), 9.0, 1.5, colors.font2, colors.font2);
    rectangle(cr, at(50.0, -86.0), 1.5, 9.0, colors.font2, colors.font2);

    // Screenshot button — upstream uses `DrawRoundButton(.., 7, 7)`, whose
    // default corner radius is 2, so this is a rounded square and not a circle.
    round_button(cr, at(-29.0, -56.0), 7.0, 7.0, 2.0, colors.button, colors.outline);
    circle_button(cr, at(-29.0, -56.0), 4.5, colors.font2, colors.font2);

    // Home button: upstream draws a wide ring then the button on top.
    circle_button(cr, at(29.0, -56.0), 10.0, colors.slider_button, colors.outline);
    circle_button(cr, at(29.0, -56.0), 7.1, colors.button, colors.outline);
    symbol(cr, at(29.0, -56.0), art::HOUSE, 3.9, colors.font2);
}


/// Trace a rounded rectangle whose *top-left* corner is `(x, y)`.
///
/// Qt's `drawRoundedRect` clamps the corner radius to half the shorter side;
/// Cairo has no such clamp, so it is applied here or the corner arcs overlap
/// and the path self-intersects.
fn rounded_rect_path(cr: &cairo::Context, x: f64, y: f64, w: f64, h: f64, radius: f64) {
    let radius = radius.min(w / 2.0).min(h / 2.0).max(0.0);
    const HALF_PI: f64 = std::f64::consts::FRAC_PI_2;
    const PI: f64 = std::f64::consts::PI;
    cr.new_sub_path();
    cr.arc(x + w - radius, y + radius, radius, -HALF_PI, 0.0);
    cr.arc(x + w - radius, y + h - radius, radius, 0.0, HALF_PI);
    cr.arc(x + radius, y + h - radius, radius, HALF_PI, PI);
    cr.arc(x + radius, y + radius, radius, PI, 1.5 * PI);
    cr.close_path();
}

/// Upstream `DrawRoundButton`.
///
/// Note the upstream rectangle is `{cx - width, cy - height, width * 2, height
/// * 2}` — `width`/`height` are *half* extents, unlike `DrawRoundRectangle`
/// just below, where they are the full size. The asymmetry is upstream's.
fn round_button(
    cr: &cairo::Context,
    center: (f64, f64),
    width: f64,
    height: f64,
    radius: f64,
    fill: (f64, f64, f64),
    outline: (f64, f64, f64),
) {
    rounded_rect_path(
        cr,
        center.0 - width,
        center.1 - height,
        width * 2.0,
        height * 2.0,
        radius,
    );
    fill_stroke(cr, fill, outline);
}

/// Upstream `DrawRoundRectangle`: centred, `width`/`height` are the full size.
fn round_rectangle(
    cr: &cairo::Context,
    center: (f64, f64),
    width: f64,
    height: f64,
    radius: f64,
    fill: (f64, f64, f64),
    outline: (f64, f64, f64),
) {
    rounded_rect_path(
        cr,
        center.0 - width / 2.0,
        center.1 - height / 2.0,
        width,
        height,
        radius,
    );
    fill_stroke(cr, fill, outline);
}

/// Upstream `DrawJoystickSideview`: the stick seen edge-on, rotated by
/// `18 * angle` degrees, with two bracing lines across the shaft.
fn joystick_sideview(
    cr: &cairo::Context,
    center: (f64, f64),
    angle: f64,
    size: f64,
    colors: &Colors,
) {
    let points: Vec<(f64, f64)> = art::LEFT_JOYSTICK_SIDEVIEW
        .chunks_exact(2)
        .map(|pair| (pair[0] as f64 * size, pair[1] as f64 * size - 1.0))
        .collect();
    if points.is_empty() {
        return;
    }

    // Upstream rotates with a QTransform; the same rotation applied by hand.
    let radians = (18.0 * angle).to_radians();
    let (sin, cos) = radians.sin_cos();
    let mapped: Vec<(f64, f64)> = points
        .iter()
        .map(|&(x, y)| (center.0 + x * cos - y * sin, center.1 + x * sin + y * cos))
        .collect();

    for (index, &(x, y)) in mapped.iter().enumerate() {
        if index == 0 {
            cr.move_to(x, y);
        } else {
            cr.line_to(x, y);
        }
    }
    cr.close_path();
    fill_stroke(cr, colors.button, colors.outline);

    // `p.drawLine(p2.at(1), p2.at(30)); p.drawLine(p2.at(32), p2.at(71));`
    cr.set_source_rgb(colors.outline.0, colors.outline.1, colors.outline.2);
    for (from, to) in [(1usize, 30usize), (32, 71)] {
        if let (Some(&a), Some(&b)) = (mapped.get(from), mapped.get(to)) {
            cr.move_to(a.0, a.1);
            cr.line_to(b.0, b.1);
            let _ = cr.stroke();
        }
    }
}

/// Upstream `DrawArrow`: the small filled glyph inside a d-pad button.
fn arrow(
    cr: &cairo::Context,
    center: (f64, f64),
    direction: Direction,
    size: f64,
    color: (f64, f64, f64),
) {
    for (index, pair) in art::UP_ARROW_SYMBOL.chunks_exact(2).enumerate() {
        let ax = pair[0] as f64 * size;
        let ay = pair[1] as f64 * size;
        let (dx, dy) = match direction {
            Direction::Up => (ax, ay),
            Direction::Right => (-ay, ax),
            Direction::Down => (ax, -ay),
            Direction::Left => (ay, ax),
            Direction::None => (ax, ay),
        };
        let (x, y) = (center.0 + dx, center.1 + dy);
        if index == 0 { cr.move_to(x, y); } else { cr.line_to(x, y); }
    }
    cr.close_path();
    cr.set_source_rgb(color.0, color.1, color.2);
    let _ = cr.fill();
}

/// Upstream `DrawJoystick`: outer circle of radius `13 * size` with a cross
/// through it, then an inner `9 * size` cap.
fn joystick(cr: &cairo::Context, center: (f64, f64), size: f64, colors: &Colors) {
    let radius1 = 13.0 * size;
    let radius2 = 9.0 * size;

    circle_button(cr, center, radius1, colors.button, colors.outline);

    // Cross.
    cr.set_source_rgb(colors.outline.0, colors.outline.1, colors.outline.2);
    cr.move_to(center.0 - radius1, center.1);
    cr.line_to(center.0 + radius1, center.1);
    let _ = cr.stroke();
    cr.move_to(center.0, center.1 - radius1);
    cr.line_to(center.0, center.1 + radius1);
    let _ = cr.stroke();

    circle_button(cr, center, radius2, colors.button2, colors.outline);
}

/// Upstream `DrawMinusButton`: a single bar, `button_size` wide and a third as
/// tall. There is no surrounding circle upstream.
fn minus_button(cr: &cairo::Context, center: (f64, f64), size: f64, colors: &Colors) {
    rectangle(cr, center, size, size / 3.0, colors.button, colors.outline);
}

/// Upstream `DrawPlusButton`: the same bar crossed by its transpose.
///
/// Upstream then repaints both bars at 88% with a transparent pen, which hides
/// the outline segments that would otherwise run through the middle of the
/// cross. `button_size` is an `int` upstream, so the rescale truncates.
fn plus_button(cr: &cairo::Context, center: (f64, f64), size: f64, colors: &Colors) {
    rectangle(cr, center, size, size / 3.0, colors.button, colors.outline);
    rectangle(cr, center, size / 3.0, size, colors.button, colors.outline);

    let inner = (size * 0.88).trunc();
    for (w, h) in [(inner, inner / 3.0), (inner / 3.0, inner)] {
        cr.rectangle(center.0 - w / 2.0, center.1 - h / 2.0, w, h);
        cr.set_source_rgb(colors.button.0, colors.button.1, colors.button.2);
        let _ = cr.fill();
    }
}

/// Upstream `DrawGCController` + `DrawGCBody`.
///
/// Offsets are upstream's: A at (111,-44) r=21, B at (70,-23) r=13, the X/Y
/// paddles from their own outlines, the control stick at (-111,-44) and the
/// C-stick at (61,37).
fn draw_gc_controller(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    let at = |dx: f64, dy: f64| (center.0 + dx, center.1 + dy);

    polygon(cr, center, art::LEFT_GC_TRIGGER, false);
    fill_stroke(cr, colors.button, colors.outline);
    polygon(cr, center, art::LEFT_GC_TRIGGER, true);
    fill_stroke(cr, colors.button, colors.outline);

    polygon(cr, center, art::GC_LEFT_BODY, false);
    fill_stroke(cr, colors.primary, colors.outline);
    polygon(cr, center, art::GC_LEFT_BODY, true);
    fill_stroke(cr, colors.primary, colors.outline);
    mirrored_polygon(cr, center, art::GC_BODY);
    fill_stroke(cr, colors.primary, colors.outline);

    gc_joystick(cr, at(-111.0, -44.0), colors);
    // The C-stick is a plain yellow-ish button with a C on it upstream.
    circle_button(cr, at(61.0, 37.0), 15.0, colors.button, colors.outline);
    symbol(cr, at(61.0, 37.0), art::SYMBOL_C, 1.0, colors.font2);
    raw_joystick(cr, Some(at(-198.0, -125.0)), Some(at(198.0, -125.0)), colors);

    // A is the large centre button; B sits below-left; X and Y are paddles.
    circle_button(cr, at(111.0, -44.0), 21.0, colors.button, colors.outline);
    symbol(cr, at(111.0, -44.0), art::SYMBOL_A, 1.5, (1.0, 1.0, 1.0));
    circle_button(cr, at(70.0, -23.0), 13.0, colors.button, colors.outline);
    symbol(cr, at(70.0, -23.0), art::SYMBOL_B, 1.0, (1.0, 1.0, 1.0));
    polygon(cr, center, art::GC_BUTTON_X, false);
    fill_stroke(cr, colors.button, colors.outline);
    symbol(cr, at(151.0, -53.0), art::SYMBOL_X, 1.0, (1.0, 1.0, 1.0));
    polygon(cr, center, art::GC_BUTTON_Y, false);
    fill_stroke(cr, colors.button, colors.outline);
    symbol(cr, at(100.0, -83.0), art::SYMBOL_Y, 1.0, (1.0, 1.0, 1.0));
    // Z rides above the right trigger.
    polygon(cr, center, art::GC_BUTTON_Z, false);
    fill_stroke(cr, colors.button, colors.outline);

    // D-pad, upstream draws it at 0.8 scale.
    let dpad = at(-61.0, 20.0);
    for direction in [Direction::Up, Direction::Left, Direction::Right, Direction::Down] {
        arrow_button(cr, dpad, direction, 0.8, colors.button, colors.outline);
    }

    // Start / Pause.
    circle_button(cr, at(0.0, -44.0), 8.0, colors.button, colors.outline);
}

/// A GameCube stick: upstream draws a deeper well than the Switch one.
fn gc_joystick(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    circle_button(cr, center, 26.0, colors.button, colors.outline);
    circle_button(cr, center, 20.0, colors.slider_button, colors.outline);
    circle(cr, center, 13.0, colors.outline);
}

/// Upstream `DrawLeftController`.
///
/// The drawing shows the left Joy-Con three times: its top view up and to the
/// left, the front face in the middle, and the side view on the right. Buttons
/// that appear in more than one view are drawn once per view.
///
/// Every literal below is upstream's.
fn draw_left_controller(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    let at = |dx: f64, dy: f64| (center.0 + dx, center.1 + dy);

    // Sideview left joystick.
    joystick_sideview(cr, at(142.0, -69.0), 0.0, 1.15, colors);

    // Topview D-pad buttons.
    round_button(cr, at(-163.0, -21.0), 11.0, 5.0, 2.0, colors.button, colors.outline);
    round_button(cr, at(-117.0, -21.0), 11.0, 5.0, 2.0, colors.button, colors.outline);

    // Topview left joystick — upstream biases the resting angle by +15.
    joystick_sideview(cr, at(-140.5, -28.0), 15.0, 1.15, colors);

    // Topview minus button.
    round_button(cr, at(-111.0, -22.0), 8.0, 4.0, 1.0, colors.button, colors.outline);

    // Left trigger, its top face button, and ZL.
    draw_left_triggers(cr, center, colors, false);
    round_button(cr, at(151.0, -146.0), 8.0, 4.0, 2.0, colors.button, colors.outline);
    draw_left_z_triggers(cr, center, colors, false);

    // Sideview D-pad buttons.
    for dy in [14.0, 36.0, -10.0, 14.0] {
        round_button(cr, at(135.0, dy), 5.0, 11.0, 2.0, colors.button, colors.outline);
    }
    round_button(cr, at(135.0, 71.0), 3.0, 8.0, 1.0, colors.button, colors.outline);

    // Sideview minus button.
    round_button(cr, at(135.0, -118.0), 4.0, 2.66, 1.0, colors.button, colors.outline);

    // Sideview SL and SR buttons.
    round_button(cr, at(59.0, 52.0), 5.0, 12.0, 2.0, colors.slider_button, colors.outline);
    round_button(cr, at(59.0, -69.0), 5.0, 12.0, 2.0, colors.slider_button, colors.outline);

    draw_left_body(cr, center, colors);

    // Left trigger top view, drawn over the body.
    polygon(cr, center, art::LEFT_JOYSTICK_L_TOPVIEW, false);
    fill_stroke(cr, colors.button, colors.outline);
    symbol(cr, at(-143.0, -36.0), art::SYMBOL_L, 1.0, colors.font2);
    polygon(cr, center, art::LEFT_JOYSTICK_ZL_TOPVIEW, false);
    fill_stroke(cr, colors.button, colors.outline);
    symbol(cr, at(-140.0, -68.0), art::SYMBOL_ZL, 1.0, colors.font2);

    // Sticks.
    joystick(cr, at(9.0, -69.0), 1.8, colors);
    raw_joystick(cr, Some(at(-140.0, 90.0)), None, colors);

    // D-pad.
    let dpad_center = at(9.0, 14.0);
    const DPAD_DISTANCE: f64 = 23.0;
    const DPAD_RADIUS: f64 = 11.0;
    const DPAD_ARROW_SIZE: f64 = 1.2;
    for ((dx, dy), direction) in [
        ((DPAD_DISTANCE, 0.0), Direction::Right),
        ((0.0, DPAD_DISTANCE), Direction::Down),
        ((0.0, -DPAD_DISTANCE), Direction::Up),
        ((-DPAD_DISTANCE, 0.0), Direction::Left),
    ] {
        let position = (dpad_center.0 + dx, dpad_center.1 + dy);
        circle_button(cr, position, DPAD_RADIUS, colors.button, colors.outline);
        arrow(cr, position, direction, DPAD_ARROW_SIZE, colors.font2);
    }

    // SR and SL buttons, with their labels.
    for (dy, glyph) in [(52.0, art::SYMBOL_SR), (-69.0, art::SYMBOL_SL)] {
        let position = at(155.0, dy);
        round_button(cr, position, 5.2, 12.0, 4.0, colors.slider_button, colors.outline);
        symbol(cr, position, glyph, 1.0, colors.font2);
    }

    // Minus button.
    minus_button(cr, at(39.0, -118.0), 16.0, colors);

    // Screenshot button.
    round_button(cr, at(26.0, 71.0), 8.0, 8.0, 2.0, colors.button, colors.outline);
    circle_button(cr, at(26.0, 71.0), 5.0, colors.font2, colors.font2);
}

/// Upstream `DrawRightController`.
///
/// Not a mirror of the left one: the stick sits below the buttons rather than
/// above them, the top view carries four face buttons instead of two D-pad
/// halves, the face letters keep their unmirrored positions, and the SL/SR
/// labels are drawn upside down (upstream rotates the painter by 180°).
fn draw_right_controller(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    let at = |dx: f64, dy: f64| (center.0 + dx, center.1 + dy);

    // Sideview right joystick — upstream writes the x as `173 - 315`.
    joystick_sideview(cr, at(173.0 - 315.0, 11.0), 10.0, 1.15, colors);

    // Topview right joystick.
    joystick_sideview(cr, at(140.0, -28.0), 15.0, 1.15, colors);

    // Topview face buttons. B and X land on the same spot upstream.
    for dx in [163.0, 140.0, 140.0, 117.0] {
        round_button(cr, at(dx, -21.0), 11.0, 5.0, 2.0, colors.button, colors.outline);
    }

    // Topview plus button — a wide bar crossed by a narrow one.
    round_button(cr, at(111.0, -22.0), 8.0, 4.0, 1.0, colors.button, colors.outline);
    round_button(cr, at(111.0, -22.0), 2.66, 4.0, 1.0, colors.button, colors.outline);

    // Right trigger, its top face button, and ZR.
    draw_left_triggers(cr, center, colors, true);
    round_button(cr, at(-151.0, -146.0), 8.0, 4.0, 2.0, colors.button, colors.outline);
    draw_left_z_triggers(cr, center, colors, true);

    // Sideview face buttons; A and Y share a position upstream.
    for dy in [-73.0, -50.0, -95.0, -73.0] {
        round_button(cr, at(-135.0, dy), 5.0, 11.0, 2.0, colors.button, colors.outline);
    }

    // Sideview home and plus buttons.
    round_button(cr, at(-135.0, 66.0), 3.0, 12.0, 2.0, colors.button, colors.outline);
    round_button(cr, at(-135.0, -118.0), 4.0, 8.0, 1.0, colors.button, colors.outline);
    round_button(cr, at(-135.0, -118.0), 4.0, 2.66, 1.0, colors.button, colors.outline);

    // Sideview SL and SR buttons — height 11 here, unlike the left Joy-Con's 12.
    round_button(cr, at(-59.0, 52.0), 5.0, 11.0, 2.0, colors.slider_button, colors.outline);
    round_button(cr, at(-59.0, -69.0), 5.0, 11.0, 2.0, colors.slider_button, colors.outline);

    draw_right_body(cr, center, colors);

    // Right trigger top view.
    polygon(cr, center, art::LEFT_JOYSTICK_L_TOPVIEW, true);
    fill_stroke(cr, colors.button, colors.outline);
    symbol(cr, at(143.0, -36.0), art::SYMBOL_R, 1.0, colors.font2);
    polygon(cr, center, art::LEFT_JOYSTICK_ZL_TOPVIEW, true);
    fill_stroke(cr, colors.button, colors.outline);
    symbol(cr, at(140.0, -68.0), art::SYMBOL_ZR, 1.0, colors.font2);

    // Sticks.
    joystick(cr, at(-9.0, 11.0), 1.8, colors);
    raw_joystick(cr, None, Some(at(140.0, 90.0)), colors);

    // Face buttons, at their unmirrored offsets from a mirrored centre.
    let face_center = at(-9.0, -73.0);
    const FACE_DISTANCE: f64 = 23.0;
    const FACE_RADIUS: f64 = 11.0;
    const TEXT_SIZE: f64 = 1.1;
    for ((dx, dy), glyph, text_dy) in [
        ((FACE_DISTANCE, 0.0), art::SYMBOL_A, 0.0),
        ((0.0, FACE_DISTANCE), art::SYMBOL_B, 0.0),
        ((0.0, -FACE_DISTANCE), art::SYMBOL_X, 0.0),
        // Upstream nudges the Y label one pixel down.
        ((-FACE_DISTANCE, 0.0), art::SYMBOL_Y, 1.0),
    ] {
        let position = (face_center.0 + dx, face_center.1 + dy);
        circle_button(cr, position, FACE_RADIUS, colors.button, colors.outline);
        symbol(cr, (position.0, position.1 + text_dy), glyph, TEXT_SIZE, colors.font);
    }

    // SR and SL buttons; the labels read upside down on this side.
    for (dy, glyph) in [(52.0, art::SYMBOL_SL), (-69.0, art::SYMBOL_SR)] {
        let position = at(-155.0, dy);
        round_button(cr, position, 5.0, 12.0, 4.0, colors.slider_button, colors.outline);
        symbol_rotated_180(cr, position, glyph, 1.0, colors.font2);
    }

    // Plus button.
    plus_button(cr, at(-40.0, -118.0), 16.0, colors);

    // Home button: ring, face, house.
    let home = at(-26.0, 66.0);
    circle_button(cr, home, 12.0, colors.slider_button, colors.outline);
    circle_button(cr, home, 9.0, colors.button, colors.outline);
    symbol(cr, home, art::HOUSE, 5.0, colors.font2);
}

/// Upstream `DrawLeftTriggers` / `DrawRightTriggers`: the shoulder seen from
/// the front, at its own offset (311.5, slightly tighter than the body's).
fn draw_left_triggers(cr: &cairo::Context, center: (f64, f64), colors: &Colors, mirror: bool) {
    const SIZE: f64 = 1.78;
    const OFFSET: f64 = 311.5;
    placed_polygon(cr, center, art::LEFT_JOYCON_TRIGGER, SIZE, (OFFSET, -1.0), mirror);
    fill_stroke(cr, colors.button, colors.outline);
}

/// Upstream `DrawLeftZTriggers` / `DrawRightZTriggers`: the ZL/ZR block on the
/// side view, plus the arc that suggests its curve.
fn draw_left_z_triggers(cr: &cairo::Context, center: (f64, f64), colors: &Colors, mirror: bool) {
    const SIZE: f64 = 1.1115;
    const OFFSET2: f64 = 335.0;
    placed_polygon(cr, center, art::LEFT_JOYCON_SIDEVIEW_ZL, SIZE, (OFFSET2, 1.0), mirror);
    fill_stroke(cr, colors.button, colors.outline);

    // Qt angles are in 1/16th degrees, counter-clockwise from 3 o'clock:
    //   left:  drawArc(cx + 158, cy - 204, 77, 77, 225 * 16, 44 * 16)
    //   right: drawArc(cx - 236, cy - 204, 77, 77, 271 * 16, 44 * 16)
    // The right-hand rectangle is the left one reflected (and nudged a pixel),
    // but its sweep is not the mirror of 225°..269°, so both are spelled out.
    let (arc_x, arc_start) = if mirror { (-236.0, 271.0) } else { (158.0, 225.0) };
    arc_rect(
        cr,
        (center.0 + arc_x, center.1 - 204.0),
        77.0,
        77.0,
        arc_start,
        44.0,
        colors.outline,
    );
}

/// Upstream `DrawLeftBody`.
fn draw_left_body(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    draw_joycon_body(cr, center, colors, false);
}

/// Upstream `DrawRightBody` — the same shapes with every x negated.
fn draw_right_body(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    draw_joycon_body(cr, center, colors, true);
}

/// The shared body drawing.
///
/// `DrawLeftBody` and `DrawRightBody` really are line-for-line mirrors of one
/// another upstream — same vertex arrays, same constants, every x negated — so
/// unlike the controller functions above they collapse without losing anything.
fn draw_joycon_body(cr: &cairo::Context, center: (f64, f64), colors: &Colors, mirror: bool) {
    let flip = if mirror { -1.0 } else { 1.0 };
    let at = |dx: f64, dy: f64| (center.0 + flip * dx, center.1 + dy);

    const BODY_SCALE: f64 = 1.78;
    const BODY_OFFSET: f64 = 312.39;
    const SIDE_SCALE: f64 = 1.1115;
    const SIDE_OFFSET: f64 = 335.0;

    let shell = if mirror { colors.right } else { colors.left };

    // Joy-Con body and the shoulder block. Both take the shell colour upstream
    // — the body trigger is part of the shell, not a button.
    placed_polygon(cr, center, art::LEFT_JOYCON_BODY, BODY_SCALE, (BODY_OFFSET, -1.0), mirror);
    fill_stroke(cr, shell, colors.outline);
    placed_polygon(
        cr,
        center,
        art::LEFT_JOYCON_BODY_TRIGGER,
        SIDE_SCALE,
        (SIDE_OFFSET, 2.0),
        mirror,
    );
    fill_stroke(cr, shell, colors.outline);

    // Slider release button, top view.
    round_rectangle(cr, at(-107.0, -62.0), 14.0, 12.0, 2.0, colors.button, colors.outline);

    // Rail, top view. The top views use the raw vertices — no scale, no offset.
    polygon(cr, center, art::LEFT_JOYCON_SLIDER_TOPVIEW, mirror);
    fill_stroke(cr, colors.slider, colors.outline);
    cr.set_source_rgb(colors.outline.0, colors.outline.1, colors.outline.2);
    cr.move_to(center.0 + flip * -91.1, center.1 - 51.7);
    cr.line_to(center.0 + flip * -91.1, center.1 - 26.5);
    let _ = cr.stroke();

    // Body, top view.
    polygon(cr, center, art::LEFT_JOYCON_TOPVIEW, mirror);
    fill_stroke(cr, shell, colors.outline);

    // Slider release button, side view.
    round_rectangle(cr, at(175.0, -110.0), 12.0, 14.0, 2.0, colors.button, colors.outline);

    // Side view body and its rail.
    placed_polygon(cr, center, art::LEFT_JOYCON_SIDEVIEW, SIDE_SCALE, (SIDE_OFFSET, 2.0), mirror);
    fill_stroke(cr, shell, colors.outline);
    placed_polygon(cr, center, art::LEFT_JOYCON_SLIDER, SIDE_SCALE, (81.0, 0.0), mirror);
    fill_stroke(cr, colors.slider, colors.outline);

    // Rail detail, centred on the side view.
    let sideview = at(155.0, 0.0);
    let rail = |dx: f64, dy: f64| (sideview.0 + flip * dx, sideview.1 + dy);
    round_rectangle(cr, rail(0.0, -5.0), 28.0, 253.0, 3.0, colors.slider, colors.outline);
    round_rectangle(cr, rail(0.0, 97.0), 22.44, 44.66, 3.0, colors.button2, colors.outline);

    for dy in [83.0, 96.0, 109.0] {
        arrow_button(
            cr,
            rail(0.0, dy),
            Direction::Down,
            2.2,
            colors.slider_arrow,
            colors.outline,
        );
    }
    circle(cr, rail(0.0, 19.0), 4.44, colors.outline);

    // Player LED indicators. Upstream lights them from `led_pattern`, which is
    // all-off until a controller is connected to the emulated console.
    const LED_SIZE: f64 = 5.0;
    for index in 0..4 {
        rectangle(
            cr,
            rail(0.0, -36.0 + 12.0 * index as f64),
            LED_SIZE,
            LED_SIZE,
            colors.led_off,
            colors.outline,
        );
    }
}

/// Qt's `QPainter::drawArc(rect, start, span)`, with the angles in degrees and
/// the rectangle given by its top-left corner.
///
/// Qt measures angles counter-clockwise from 3 o'clock; Cairo measures them
/// clockwise, so both angles are negated and the sweep runs backwards.
fn arc_rect(
    cr: &cairo::Context,
    top_left: (f64, f64),
    width: f64,
    height: f64,
    start_degrees: f64,
    span_degrees: f64,
    color: (f64, f64, f64),
) {
    let (cx, cy) = (top_left.0 + width / 2.0, top_left.1 + height / 2.0);
    cr.save().ok();
    cr.translate(cx, cy);
    cr.scale(width / 2.0, height / 2.0);
    cr.arc_negative(
        0.0,
        0.0,
        1.0,
        -start_degrees.to_radians(),
        -(start_degrees + span_degrees).to_radians(),
    );
    // Restoring before stroking keeps the pen width at 1px, the way Qt's
    // cosmetic pen is unaffected by the shape's geometry.
    cr.restore().ok();
    cr.set_source_rgb(color.0, color.1, color.2);
    let _ = cr.stroke();
}

/// Upstream `DrawDualController` — both Joy-Cons detached.
///
/// Offsets are upstream's: face buttons at (65,-65) distance 20 radius 10,
/// d-pad at (-65,12) distance 20 radius 10, sticks at (-65,-65) and (65,12),
/// minus/plus at (∓39,-106), screenshot at (-52,63), home at (50,60).
fn draw_dual_controller(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    let at = |dx: f64, dy: f64| (center.0 + dx, center.1 + dy);

    // Upstream `DrawDualBody`: both shells at size 1.61, offset +/-209.3,
    // shifted 1px up; the top views ride above at size2 0.9.
    const BODY_SCALE: f64 = 1.61;
    const BODY_OFFSET: f64 = 209.3;
    const TOP_SCALE: f64 = 0.9;

    // Shoulder pieces, drawn as top views on either side.
    for mirror in [false, true] {
        placed_polygon(cr, center, art::LEFT_JOYCON_TOPVIEW, TOP_SCALE, (-155.0, -100.0), mirror);
        fill_stroke(cr, colors.primary, colors.outline);
        placed_polygon(
            cr, center, art::LEFT_JOYSTICK_ZL_TOPVIEW, TOP_SCALE, (-155.0, -100.0), mirror,
        );
        fill_stroke(cr, colors.button, colors.outline);
    }

    // The two bodies.
    for mirror in [false, true] {
        placed_polygon(cr, center, art::LEFT_JOYCON_BODY, BODY_SCALE, (BODY_OFFSET, -1.0), mirror);
        fill_stroke(cr, colors.primary, colors.outline);
    }

    // Left half.
    joystick(cr, at(-65.0, -65.0), 1.62, colors);
    let dpad = at(-65.0, 12.0);
    for (offset, direction) in [
        ((20.0, 0.0), Direction::Right),
        ((0.0, 20.0), Direction::Down),
        ((0.0, -20.0), Direction::Up),
        ((-20.0, 0.0), Direction::Left),
    ] {
        let position = (dpad.0 + offset.0, dpad.1 + offset.1);
        circle_button(cr, position, 10.0, colors.button, colors.outline);
        arrow(cr, position, direction, 1.1, colors.font2);
    }

    // Right half.
    let face = at(65.0, -65.0);
    for (offset, glyph) in [
        ((20.0, 0.0), art::SYMBOL_A),
        ((0.0, 20.0), art::SYMBOL_B),
        ((0.0, -20.0), art::SYMBOL_X),
        ((-20.0, 0.0), art::SYMBOL_Y),
    ] {
        let position = (face.0 + offset.0, face.1 + offset.1);
        circle_button(cr, position, 10.0, colors.button, colors.outline);
        symbol(cr, position, glyph, 1.0, (1.0, 1.0, 1.0));
    }
    joystick(cr, at(65.0, 12.0), 1.62, colors);

    minus_button(cr, at(-39.0, -106.0), 14.0, colors);
    plus_button(cr, at(39.0, -106.0), 14.0, colors);

    circle_button(cr, at(-52.0, 63.0), 8.0, colors.button, colors.outline);
    circle(cr, at(-52.0, 63.0), 5.0, colors.font2);
    circle_button(cr, at(50.0, 60.0), 11.0, colors.slider_button, colors.outline);
    circle_button(cr, at(50.0, 60.0), 8.5, colors.button, colors.outline);
    symbol(cr, at(50.0, 60.0), art::HOUSE, 4.2, colors.font2);

    raw_joystick(cr, Some(at(-180.0, 90.0)), Some(at(180.0, 90.0)), colors);
}

/// Upstream `DrawHandheldController` — the console with both Joy-Cons on.
///
/// Offsets are upstream's: sticks at (-171,-41) and (171,8), face buttons at
/// (171,-41) and d-pad at (-171,8), both distance 12.8 radius 6.4.
fn draw_handheld_controller(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    let at = |dx: f64, dy: f64| (center.0 + dx, center.1 + dy);

    // `DrawHandheldTriggers`: the two shoulders, at the raw vertex scale.
    polygon(cr, center, art::LEFT_JOYCON_TRIGGER, false);
    fill_stroke(cr, colors.button, colors.outline);
    polygon(cr, center, art::LEFT_JOYCON_TRIGGER, true);
    fill_stroke(cr, colors.button, colors.outline);

    draw_handheld_body(cr, center, colors);

    // Sticks. The handheld drawing is the small one — scale 1.0, not 1.8.
    joystick(cr, at(-171.0, -41.0), 1.0, colors);
    joystick(cr, at(171.0, 8.0), 1.0, colors);
    raw_joystick(cr, Some(at(-50.0, 0.0)), Some(at(50.0, 0.0)), colors);

    // Face buttons.
    let face_center = at(171.0, -41.0);
    const FACE_DISTANCE: f64 = 12.8;
    const FACE_RADIUS: f64 = 6.4;
    const TEXT_SIZE: f64 = 0.6;
    for ((dx, dy), glyph, text_dy) in [
        ((FACE_DISTANCE, 0.0), art::SYMBOL_A, 0.0),
        ((0.0, FACE_DISTANCE), art::SYMBOL_B, 0.0),
        ((0.0, -FACE_DISTANCE), art::SYMBOL_X, 0.0),
        ((-FACE_DISTANCE, 0.0), art::SYMBOL_Y, 1.0),
    ] {
        let position = (face_center.0 + dx, face_center.1 + dy);
        circle_button(cr, position, FACE_RADIUS, colors.button, colors.outline);
        symbol(cr, (position.0, position.1 + text_dy), glyph, TEXT_SIZE, colors.font);
    }

    // D-pad.
    let dpad_center = at(-171.0, 8.0);
    const DPAD_ARROW_SIZE: f64 = 0.68;
    for ((dx, dy), direction) in [
        ((FACE_DISTANCE, 0.0), Direction::Right),
        ((0.0, FACE_DISTANCE), Direction::Down),
        ((0.0, -FACE_DISTANCE), Direction::Up),
        ((-FACE_DISTANCE, 0.0), Direction::Left),
    ] {
        let position = (dpad_center.0 + dx, dpad_center.1 + dy);
        circle_button(cr, position, FACE_RADIUS, colors.button, colors.outline);
        arrow(cr, position, direction, DPAD_ARROW_SIZE, colors.font2);
    }

    // ZL and ZR.
    for (dx, direction, glyph) in [
        (-210.0, Direction::Left, art::SYMBOL_ZL),
        (210.0, Direction::Right, art::SYMBOL_ZR),
    ] {
        let position = at(dx, -120.0);
        trigger_button(cr, position, direction, colors.button, colors.outline);
        symbol(cr, position, glyph, 1.5, colors.font);
    }

    // Minus and plus.
    minus_button(cr, at(-155.0, -67.0), 8.0, colors);
    plus_button(cr, at(155.0, -67.0), 8.0, colors);

    // Screenshot button.
    round_button(cr, at(-162.0, 39.0), 5.0, 5.0, 2.0, colors.button, colors.outline);
    circle_button(cr, at(-162.0, 39.0), 3.0, colors.font2, colors.font2);

    // Home button.
    let home = at(161.0, 37.0);
    circle_button(cr, home, 7.0, colors.slider_button, colors.outline);
    circle_button(cr, home, 5.0, colors.button, colors.outline);
    symbol(cr, home, art::HOUSE, 2.75, colors.font2);
}

/// Upstream `DrawHandheldBody`.
///
/// The console shell, its bezel and the two Joy-Con bodies, all at the raw
/// vertex scale. Upstream draws the fills with a transparent pen and the
/// outlines with a transparent brush, using *shorter* vertex ranges for the
/// outlines so the edges hidden behind the Joy-Cons are not stroked.
fn draw_handheld_body(cr: &cairo::Context, center: (f64, f64), colors: &Colors) {
    // `handheld_body.size() / 2 - 6` and `handheld_bezel.size() / 2 - 6`.
    let body_outline_end = art::HANDHELD_BODY.len() / 2 - 6;
    let bezel_outline_end = art::HANDHELD_BEZEL.len() / 2 - 6;
    const BEZEL_INLINE_START: usize = 35;
    const BEZEL_INLINE_SIZE: usize = 4;

    // Left and right Joy-Con bodies, unscaled.
    polygon(cr, center, art::LEFT_JOYCON_BODY, false);
    fill_stroke(cr, colors.left, colors.outline);
    polygon(cr, center, art::LEFT_JOYCON_BODY, true);
    fill_stroke(cr, colors.right, colors.outline);

    // The rail buttons between them.
    polygon(cr, center, art::HANDHELD_BUTTONS, false);
    fill_stroke(cr, colors.button, colors.outline);

    // Console body: filled whole, outlined short.
    polygon(cr, center, art::HANDHELD_BODY, false);
    cr.set_source_rgb(colors.primary.0, colors.primary.1, colors.primary.2);
    let _ = cr.fill();
    polygon_range(cr, center, art::HANDHELD_BODY, 0, body_outline_end);
    cr.set_source_rgb(colors.outline.0, colors.outline.1, colors.outline.2);
    let _ = cr.stroke();

    // Screen bezel, the same way, plus the inline seam across its top.
    polygon(cr, center, art::HANDHELD_BEZEL, false);
    cr.set_source_rgb(colors.button.0, colors.button.1, colors.button.2);
    let _ = cr.fill();
    cr.set_source_rgb(colors.outline.0, colors.outline.1, colors.outline.2);
    polygon_range(cr, center, art::HANDHELD_BEZEL, 0, bezel_outline_end);
    let _ = cr.stroke();
    polygon_range(
        cr,
        center,
        art::HANDHELD_BEZEL,
        BEZEL_INLINE_START,
        BEZEL_INLINE_START + BEZEL_INLINE_SIZE,
    );
    let _ = cr.stroke();
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Render one type and return the number of pixels that differ from the
    /// white background.
    fn render(controller_type: ControllerType) -> (u32, cairo::ImageSurface) {
        let surface =
            cairo::ImageSurface::create(cairo::Format::Rgb24, PREVIEW_WIDTH, PREVIEW_HEIGHT)
                .expect("image surface");
        {
            let cr = cairo::Context::new(&surface).expect("cairo context");
            cr.set_source_rgb(1.0, 1.0, 1.0);
            cr.paint().unwrap();
            draw(
                &cr,
                (PREVIEW_WIDTH as f64 / 2.0, PREVIEW_HEIGHT as f64 / 2.0),
                controller_type,
                false,
            );
        }
        let mut surface = surface;
        let painted = {
            let data = surface.data().expect("surface data");
            data.chunks_exact(4)
                .filter(|px| px[0] != 0xff || px[1] != 0xff || px[2] != 0xff)
                .count() as u32
        };
        (painted, surface)
    }

    const ALL_TYPES: &[(ControllerType, &str)] = &[
        (ControllerType::ProController, "pro"),
        (ControllerType::DualJoyconDetached, "dual"),
        (ControllerType::LeftJoycon, "left"),
        (ControllerType::RightJoycon, "right"),
        (ControllerType::Handheld, "handheld"),
        (ControllerType::GameCube, "gamecube"),
    ];

    /// Write each drawing to `$RUZU_PREVIEW_DUMP` for visual inspection.
    #[test]
    fn dump_previews_when_asked() {
        let Ok(dir) = std::env::var("RUZU_PREVIEW_DUMP") else {
            return;
        };
        std::fs::create_dir_all(&dir).unwrap();
        for (controller, name) in ALL_TYPES {
            let (_, surface) = render(*controller);
            let mut file = std::fs::File::create(format!("{dir}/{name}.png")).unwrap();
            let mut surface = surface;
            surface.write_to_png(&mut file).unwrap();
        }
    }

    #[test]
    fn every_controller_type_draws_something() {
        // A type whose outlines were mis-wired would render an empty widget;
        // the threshold is well below any real drawing.
        for (controller, name) in ALL_TYPES {
            let (painted, _) = render(*controller);
            assert!(
                painted > 2_000,
                "{name} painted only {painted} pixels — the drawing is missing or empty"
            );
        }
    }

    #[test]
    fn controller_types_do_not_render_identically() {
        // Guards the bug where several types fell through to the Pro drawing:
        // each must produce a distinct image.
        let mut seen: Vec<(String, Vec<u8>)> = Vec::new();
        for (controller, name) in ALL_TYPES {
            let (_, mut surface) = render(*controller);
            let bytes = surface.data().expect("surface data").to_vec();
            for (other, other_bytes) in &seen {
                assert_ne!(
                    &bytes, other_bytes,
                    "{name} renders identically to {other}"
                );
            }
            seen.push((name.to_string(), bytes));
        }
    }

    #[test]
    fn drawings_stay_inside_the_widget() {
        // Upstream's coordinates assume a 500x350 area; an outline that ran off
        // the edge would be silently clipped rather than reported.
        for (controller, name) in ALL_TYPES {
            let (_, mut surface) = render(*controller);
            let stride = surface.stride() as usize;
            let data = surface.data().expect("surface data");
            let painted_at = |x: usize, y: usize| {
                let i = y * stride + x * 4;
                data[i] != 0xff || data[i + 1] != 0xff || data[i + 2] != 0xff
            };
            for x in 0..PREVIEW_WIDTH as usize {
                assert!(!painted_at(x, 0), "{name} paints the top edge");
                assert!(
                    !painted_at(x, PREVIEW_HEIGHT as usize - 1),
                    "{name} paints the bottom edge"
                );
            }
        }
    }

    #[test]
    fn outlines_are_coordinate_pairs() {
        // `polygon` walks the data in twos; an odd length would drop a point.
        for outline in [
            art::PRO_BODY,
            art::PRO_LEFT_HANDLE,
            art::GC_BODY,
            art::LEFT_JOYCON_BODY,
            art::HANDHELD_BODY,
            art::UP_ARROW_BUTTON,
            art::TRIGGER_BUTTON,
        ] {
            assert_eq!(outline.len() % 2, 0);
            assert!(outline.len() >= 4);
        }
    }

    #[test]
    fn vertex_counts_match_upstream() {
        // Upstream declares these array sizes; a bad extraction would change
        // them and silently distort the shells.
        assert_eq!(art::PRO_BODY.len(), 245 * 2);
        assert_eq!(art::PRO_LEFT_HANDLE.len(), 145 * 2);
        assert_eq!(art::GC_BODY.len(), 199 * 2);
        assert_eq!(art::LEFT_JOYCON_BODY.len(), 84 * 2);
        assert_eq!(art::HANDHELD_BODY.len(), 70 * 2);
    }
}
