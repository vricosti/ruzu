// Scratch check: enumerate input devices and the default mapping the Controls
// page would install for each. Not part of the port.
use common::settings_input::{native_analog, native_button, native_motion};

fn main() {
    let mut subsystem = input_common::InputSubsystem::new();
    subsystem.initialize();
    std::thread::sleep(std::time::Duration::from_millis(500));

    for (index, device) in subsystem.get_input_devices().iter().enumerate() {
        println!(
            "[{index}] display={:?} engine={:?}",
            device.get_str("display", "?"),
            device.get_str("engine", "?")
        );
        let buttons = subsystem.get_button_mapping_for_device(device);
        let analogs = subsystem.get_analog_mapping_for_device(device);
        let motions = subsystem.get_motion_mapping_for_device(device);
        if buttons.is_empty() && analogs.is_empty() && motions.is_empty() {
            println!("      (no default mapping)");
            continue;
        }
        for (slot, name) in native_button::MAPPING.iter().enumerate() {
            if let Some(p) = buttons.get(&(slot as i32)) {
                println!("      {name:<20} {}", p.serialize());
            }
        }
        for (slot, name) in native_analog::MAPPING.iter().enumerate() {
            if let Some(p) = analogs.get(&(slot as i32)) {
                println!("      {name:<20} {}", p.serialize());
            }
        }
        for (slot, name) in native_motion::MAPPING.iter().enumerate() {
            if let Some(p) = motions.get(&(slot as i32)) {
                println!("      {name:<20} {}", p.serialize());
            }
        }
    }
    subsystem.shutdown();
}
