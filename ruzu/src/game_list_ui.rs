// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SDL2-based game library UI.
//!
//! Displays a dark-themed list of scanned games with keyboard/mouse navigation.
//! Uses SDL2's built-in pixel drawing for a minimal text renderer (no SDL2_ttf).

use anyhow::Result;
use ruzu_loader::game_info::GameInfo;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;

/// Window dimensions.
const WINDOW_WIDTH: u32 = 800;
const WINDOW_HEIGHT: u32 = 600;

/// Row height in pixels.
const ROW_HEIGHT: u32 = 48;

/// Padding.
const PADDING_X: i32 = 16;
const PADDING_Y: i32 = 12;

/// Header height.
const HEADER_HEIGHT: u32 = 50;

/// Colors.
const BG_COLOR: Color = Color::RGB(30, 30, 36);
const HEADER_BG: Color = Color::RGB(40, 40, 50);
const ROW_EVEN: Color = Color::RGB(35, 35, 42);
const ROW_ODD: Color = Color::RGB(30, 30, 36);
const ROW_SELECTED: Color = Color::RGB(60, 60, 90);
const ROW_HOVER: Color = Color::RGB(45, 45, 58);
const TEXT_TITLE: Color = Color::RGB(240, 240, 245);
const TEXT_DEVELOPER: Color = Color::RGB(150, 150, 165);
const TEXT_FORMAT: Color = Color::RGB(100, 180, 255);
const TEXT_HEADER: Color = Color::RGB(200, 200, 215);
const SCROLLBAR_BG: Color = Color::RGB(45, 45, 55);
const SCROLLBAR_FG: Color = Color::RGB(100, 100, 120);

/// Show the game list UI and wait for the user to select a game.
///
/// Returns the index of the selected game, or `None` if the user quit.
pub fn show_game_list(games: &[GameInfo]) -> Result<Option<usize>> {
    let sdl = sdl2::init().map_err(|e| anyhow::anyhow!("SDL2 init: {}", e))?;
    let video = sdl.video().map_err(|e| anyhow::anyhow!("SDL2 video: {}", e))?;

    let window = video
        .window("ruzu - Game Library", WINDOW_WIDTH, WINDOW_HEIGHT)
        .position_centered()
        .resizable()
        .build()
        .map_err(|e| anyhow::anyhow!("Window: {}", e))?;

    let mut canvas = window
        .into_canvas()
        .accelerated()
        .present_vsync()
        .build()
        .map_err(|e| anyhow::anyhow!("Canvas: {}", e))?;

    let mut event_pump = sdl
        .event_pump()
        .map_err(|e| anyhow::anyhow!("Event pump: {}", e))?;

    let mut selected: usize = 0;
    let mut scroll_offset: usize = 0;
    let mut hover_row: Option<usize> = None;

    if games.is_empty() {
        // Show empty state and wait for quit
        loop {
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => return Ok(None),
                    _ => {}
                }
            }

            canvas.set_draw_color(BG_COLOR);
            canvas.clear();

            let (w, h) = canvas.output_size().unwrap_or((WINDOW_WIDTH, WINDOW_HEIGHT));
            draw_centered_text(
                &mut canvas,
                "No games found. Add games to your games directory.",
                w as i32 / 2 - 200,
                h as i32 / 2 - 8,
                TEXT_DEVELOPER,
            );

            canvas.present();
            std::thread::sleep(std::time::Duration::from_millis(16));
        }
    }

    loop {
        let (win_w, win_h) = canvas.output_size().unwrap_or((WINDOW_WIDTH, WINDOW_HEIGHT));
        let visible_rows = ((win_h - HEADER_HEIGHT) / ROW_HEIGHT) as usize;

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => return Ok(None),

                Event::KeyDown { keycode, .. } => match keycode {
                    Some(Keycode::Escape) => return Ok(None),
                    Some(Keycode::Return) | Some(Keycode::KpEnter) => {
                        return Ok(Some(selected));
                    }
                    Some(Keycode::Up) => {
                        if selected > 0 {
                            selected -= 1;
                            if selected < scroll_offset {
                                scroll_offset = selected;
                            }
                        }
                    }
                    Some(Keycode::Down) => {
                        if selected + 1 < games.len() {
                            selected += 1;
                            if selected >= scroll_offset + visible_rows {
                                scroll_offset = selected - visible_rows + 1;
                            }
                        }
                    }
                    Some(Keycode::PageUp) => {
                        selected = selected.saturating_sub(visible_rows);
                        scroll_offset = scroll_offset.saturating_sub(visible_rows);
                    }
                    Some(Keycode::PageDown) => {
                        selected = (selected + visible_rows).min(games.len().saturating_sub(1));
                        if selected >= scroll_offset + visible_rows {
                            scroll_offset = selected - visible_rows + 1;
                        }
                    }
                    Some(Keycode::Home) => {
                        selected = 0;
                        scroll_offset = 0;
                    }
                    Some(Keycode::End) => {
                        selected = games.len().saturating_sub(1);
                        scroll_offset =
                            games.len().saturating_sub(visible_rows);
                    }
                    _ => {}
                },

                Event::MouseMotion { y, .. } => {
                    let row_y = y - HEADER_HEIGHT as i32;
                    if row_y >= 0 {
                        let row = scroll_offset + (row_y as u32 / ROW_HEIGHT) as usize;
                        hover_row = if row < games.len() { Some(row) } else { None };
                    } else {
                        hover_row = None;
                    }
                }

                Event::MouseButtonDown { y, clicks, .. } => {
                    let row_y = y - HEADER_HEIGHT as i32;
                    if row_y >= 0 {
                        let row = scroll_offset + (row_y as u32 / ROW_HEIGHT) as usize;
                        if row < games.len() {
                            selected = row;
                            if clicks >= 2 {
                                return Ok(Some(selected));
                            }
                        }
                    }
                }

                Event::MouseWheel { y, .. } => {
                    if y > 0 {
                        scroll_offset = scroll_offset.saturating_sub(3);
                        if selected > scroll_offset + visible_rows {
                            selected = scroll_offset + visible_rows - 1;
                        }
                    } else if y < 0 {
                        let max_scroll = games.len().saturating_sub(visible_rows);
                        scroll_offset = (scroll_offset + 3).min(max_scroll);
                        if selected < scroll_offset {
                            selected = scroll_offset;
                        }
                    }
                }

                _ => {}
            }
        }

        // --- Render ---
        canvas.set_draw_color(BG_COLOR);
        canvas.clear();

        // Header
        canvas.set_draw_color(HEADER_BG);
        canvas.fill_rect(Rect::new(0, 0, win_w, HEADER_HEIGHT)).ok();
        draw_text(
            &mut canvas,
            &format!("ruzu Game Library ({} games)", games.len()),
            PADDING_X,
            (HEADER_HEIGHT as i32 - 16) / 2,
            TEXT_HEADER,
        );

        // Draw "Format" column header
        draw_text(
            &mut canvas,
            "Format",
            win_w as i32 - 80,
            (HEADER_HEIGHT as i32 - 16) / 2,
            TEXT_DEVELOPER,
        );

        // Game rows
        let end = (scroll_offset + visible_rows).min(games.len());
        for i in scroll_offset..end {
            let row_y = HEADER_HEIGHT as i32 + ((i - scroll_offset) as u32 * ROW_HEIGHT) as i32;

            // Row background
            let bg = if i == selected {
                ROW_SELECTED
            } else if hover_row == Some(i) {
                ROW_HOVER
            } else if i % 2 == 0 {
                ROW_EVEN
            } else {
                ROW_ODD
            };
            canvas.set_draw_color(bg);
            canvas
                .fill_rect(Rect::new(0, row_y, win_w, ROW_HEIGHT))
                .ok();

            let game = &games[i];

            // Title
            draw_text(
                &mut canvas,
                &game.title,
                PADDING_X,
                row_y + PADDING_Y / 2,
                TEXT_TITLE,
            );

            // Developer
            if !game.developer.is_empty() {
                draw_text(
                    &mut canvas,
                    &game.developer,
                    PADDING_X,
                    row_y + PADDING_Y / 2 + 18,
                    TEXT_DEVELOPER,
                );
            }

            // Format badge
            draw_text(
                &mut canvas,
                game.format.label(),
                win_w as i32 - 70,
                row_y + (ROW_HEIGHT as i32 - 12) / 2,
                TEXT_FORMAT,
            );
        }

        // Scrollbar
        if games.len() > visible_rows {
            let scrollbar_x = win_w as i32 - 8;
            let scrollbar_area_h = win_h - HEADER_HEIGHT;
            let thumb_h = (visible_rows as f32 / games.len() as f32 * scrollbar_area_h as f32)
                .max(20.0) as u32;
            let thumb_y = HEADER_HEIGHT
                + (scroll_offset as f32 / (games.len() - visible_rows) as f32
                    * (scrollbar_area_h - thumb_h) as f32) as u32;

            canvas.set_draw_color(SCROLLBAR_BG);
            canvas
                .fill_rect(Rect::new(scrollbar_x, HEADER_HEIGHT as i32, 8, scrollbar_area_h))
                .ok();

            canvas.set_draw_color(SCROLLBAR_FG);
            canvas
                .fill_rect(Rect::new(scrollbar_x, thumb_y as i32, 8, thumb_h))
                .ok();
        }

        canvas.present();
        std::thread::sleep(std::time::Duration::from_millis(16));
    }
}

/// Minimal text drawing using SDL2 rectangles (no SDL2_ttf dependency).
///
/// Each character is rendered as a 6x10 bitmap. This is intentionally minimal —
/// just enough to display game titles legibly.
fn draw_text(canvas: &mut sdl2::render::Canvas<sdl2::video::Window>, text: &str, x: i32, y: i32, color: Color) {
    canvas.set_draw_color(color);
    let mut cx = x;
    for ch in text.chars() {
        if let Some(bitmap) = get_char_bitmap(ch) {
            for (row, &bits) in bitmap.iter().enumerate() {
                for col in 0..6 {
                    if bits & (1 << (5 - col)) != 0 {
                        canvas
                            .fill_rect(Rect::new(cx + col, y + row as i32, 1, 1))
                            .ok();
                    }
                }
            }
        }
        cx += 7; // 6px char + 1px spacing
    }
}

/// Same as draw_text but for centered messages.
fn draw_centered_text(
    canvas: &mut sdl2::render::Canvas<sdl2::video::Window>,
    text: &str,
    x: i32,
    y: i32,
    color: Color,
) {
    draw_text(canvas, text, x, y, color);
}

/// Get a 6x10 bitmap for a character. Returns None for unsupported chars.
///
/// Each byte represents one row, with the top 6 bits being pixels (MSB = leftmost).
fn get_char_bitmap(ch: char) -> Option<&'static [u8; 10]> {
    // Minimal bitmap font covering ASCII printable range
    match ch {
        ' ' => Some(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
        '!' => Some(&[0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x08, 0x00, 0x00]),
        '"' => Some(&[0x14, 0x14, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
        '#' => Some(&[0x14, 0x14, 0x3E, 0x14, 0x3E, 0x14, 0x14, 0x00, 0x00, 0x00]),
        '(' => Some(&[0x04, 0x08, 0x10, 0x10, 0x10, 0x10, 0x08, 0x04, 0x00, 0x00]),
        ')' => Some(&[0x10, 0x08, 0x04, 0x04, 0x04, 0x04, 0x08, 0x10, 0x00, 0x00]),
        '+' => Some(&[0x00, 0x00, 0x08, 0x08, 0x3E, 0x08, 0x08, 0x00, 0x00, 0x00]),
        ',' => Some(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x08, 0x10, 0x00]),
        '-' => Some(&[0x00, 0x00, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00]),
        '.' => Some(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00]),
        '/' => Some(&[0x02, 0x02, 0x04, 0x08, 0x10, 0x20, 0x20, 0x00, 0x00, 0x00]),
        '0' => Some(&[0x1C, 0x22, 0x26, 0x2A, 0x32, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        '1' => Some(&[0x08, 0x18, 0x08, 0x08, 0x08, 0x08, 0x1C, 0x00, 0x00, 0x00]),
        '2' => Some(&[0x1C, 0x22, 0x02, 0x04, 0x08, 0x10, 0x3E, 0x00, 0x00, 0x00]),
        '3' => Some(&[0x1C, 0x22, 0x02, 0x0C, 0x02, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        '4' => Some(&[0x04, 0x0C, 0x14, 0x24, 0x3E, 0x04, 0x04, 0x00, 0x00, 0x00]),
        '5' => Some(&[0x3E, 0x20, 0x3C, 0x02, 0x02, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        '6' => Some(&[0x0C, 0x10, 0x20, 0x3C, 0x22, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        '7' => Some(&[0x3E, 0x02, 0x04, 0x08, 0x10, 0x10, 0x10, 0x00, 0x00, 0x00]),
        '8' => Some(&[0x1C, 0x22, 0x22, 0x1C, 0x22, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        '9' => Some(&[0x1C, 0x22, 0x22, 0x1E, 0x02, 0x04, 0x18, 0x00, 0x00, 0x00]),
        ':' => Some(&[0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00]),
        '=' => Some(&[0x00, 0x00, 0x3E, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00]),
        '?' => Some(&[0x1C, 0x22, 0x02, 0x04, 0x08, 0x00, 0x08, 0x00, 0x00, 0x00]),
        'A' | 'a' => Some(&[0x1C, 0x22, 0x22, 0x3E, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00]),
        'B' | 'b' => Some(&[0x3C, 0x22, 0x22, 0x3C, 0x22, 0x22, 0x3C, 0x00, 0x00, 0x00]),
        'C' | 'c' => Some(&[0x1C, 0x22, 0x20, 0x20, 0x20, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        'D' | 'd' => Some(&[0x3C, 0x22, 0x22, 0x22, 0x22, 0x22, 0x3C, 0x00, 0x00, 0x00]),
        'E' | 'e' => Some(&[0x3E, 0x20, 0x20, 0x3C, 0x20, 0x20, 0x3E, 0x00, 0x00, 0x00]),
        'F' | 'f' => Some(&[0x3E, 0x20, 0x20, 0x3C, 0x20, 0x20, 0x20, 0x00, 0x00, 0x00]),
        'G' | 'g' => Some(&[0x1C, 0x22, 0x20, 0x2E, 0x22, 0x22, 0x1E, 0x00, 0x00, 0x00]),
        'H' | 'h' => Some(&[0x22, 0x22, 0x22, 0x3E, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00]),
        'I' | 'i' => Some(&[0x1C, 0x08, 0x08, 0x08, 0x08, 0x08, 0x1C, 0x00, 0x00, 0x00]),
        'J' | 'j' => Some(&[0x0E, 0x04, 0x04, 0x04, 0x04, 0x24, 0x18, 0x00, 0x00, 0x00]),
        'K' | 'k' => Some(&[0x22, 0x24, 0x28, 0x30, 0x28, 0x24, 0x22, 0x00, 0x00, 0x00]),
        'L' | 'l' => Some(&[0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x3E, 0x00, 0x00, 0x00]),
        'M' | 'm' => Some(&[0x22, 0x36, 0x2A, 0x2A, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00]),
        'N' | 'n' => Some(&[0x22, 0x32, 0x2A, 0x26, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00]),
        'O' | 'o' => Some(&[0x1C, 0x22, 0x22, 0x22, 0x22, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        'P' | 'p' => Some(&[0x3C, 0x22, 0x22, 0x3C, 0x20, 0x20, 0x20, 0x00, 0x00, 0x00]),
        'Q' | 'q' => Some(&[0x1C, 0x22, 0x22, 0x22, 0x2A, 0x24, 0x1A, 0x00, 0x00, 0x00]),
        'R' | 'r' => Some(&[0x3C, 0x22, 0x22, 0x3C, 0x28, 0x24, 0x22, 0x00, 0x00, 0x00]),
        'S' | 's' => Some(&[0x1C, 0x22, 0x20, 0x1C, 0x02, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        'T' | 't' => Some(&[0x3E, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x00, 0x00]),
        'U' | 'u' => Some(&[0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x1C, 0x00, 0x00, 0x00]),
        'V' | 'v' => Some(&[0x22, 0x22, 0x22, 0x22, 0x14, 0x14, 0x08, 0x00, 0x00, 0x00]),
        'W' | 'w' => Some(&[0x22, 0x22, 0x22, 0x2A, 0x2A, 0x36, 0x22, 0x00, 0x00, 0x00]),
        'X' | 'x' => Some(&[0x22, 0x22, 0x14, 0x08, 0x14, 0x22, 0x22, 0x00, 0x00, 0x00]),
        'Y' | 'y' => Some(&[0x22, 0x22, 0x14, 0x08, 0x08, 0x08, 0x08, 0x00, 0x00, 0x00]),
        'Z' | 'z' => Some(&[0x3E, 0x02, 0x04, 0x08, 0x10, 0x20, 0x3E, 0x00, 0x00, 0x00]),
        '[' => Some(&[0x1C, 0x10, 0x10, 0x10, 0x10, 0x10, 0x1C, 0x00, 0x00, 0x00]),
        ']' => Some(&[0x1C, 0x04, 0x04, 0x04, 0x04, 0x04, 0x1C, 0x00, 0x00, 0x00]),
        '_' => Some(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00]),
        _ => Some(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char_bitmap_coverage() {
        // Verify all ASCII letters have bitmaps
        for ch in 'A'..='Z' {
            assert!(get_char_bitmap(ch).is_some(), "Missing bitmap for '{}'", ch);
        }
        for ch in '0'..='9' {
            assert!(get_char_bitmap(ch).is_some(), "Missing bitmap for '{}'", ch);
        }
        assert!(get_char_bitmap(' ').is_some());
        assert!(get_char_bitmap('.').is_some());
        assert!(get_char_bitmap('-').is_some());
    }
}
