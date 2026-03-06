use crate::common::common::align_audio;

use super::command_list_header::{CommandListHeader, COMMAND_LIST_HEADER_SIZE};
use super::command_processing_time_estimator::CommandProcessingTimeEstimator;
use super::commands::{ClearMixBufferCommand, Command};
use super::icommand::CommandHeader;

#[derive(Debug, Clone)]
pub struct CommandEntry {
    pub command: Command,
    pub estimated_process_time: u32,
    pub node_id: u32,
    pub enabled: bool,
}

#[derive(Debug, Clone)]
pub struct CommandBuffer {
    commands: Vec<CommandEntry>,
    size: usize,
    count: u32,
    estimated_process_time: u32,
    capacity: usize,
    estimator: CommandProcessingTimeEstimator,
}

impl CommandBuffer {
    pub fn new(capacity: usize, estimator: CommandProcessingTimeEstimator) -> Self {
        Self {
            commands: Vec::new(),
            size: 0,
            count: 0,
            estimated_process_time: 0,
            capacity,
            estimator,
        }
    }

    pub fn generate_clear_mix_command(&mut self, node_id: u32, buffer_count: u32) -> bool {
        self.push(
            Command::ClearMixBuffer(ClearMixBufferCommand { buffer_count }),
            node_id,
        )
    }

    pub fn push(&mut self, command: Command, node_id: u32) -> bool {
        self.push_with_enabled(command, node_id, true)
    }

    pub fn push_with_enabled(&mut self, command: Command, node_id: u32, enabled: bool) -> bool {
        let serialized_size = command.serialized_size();
        if self.size + serialized_size > self.capacity {
            return false;
        }

        let estimated_process_time = self.estimator.estimate(&command);
        self.commands.push(CommandEntry {
            command,
            estimated_process_time,
            node_id,
            enabled,
        });
        self.size += serialized_size;
        self.count += 1;
        if enabled {
            self.estimated_process_time = self
                .estimated_process_time
                .saturating_add(estimated_process_time);
        }
        true
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn count(&self) -> u32 {
        self.count
    }

    pub fn estimated_process_time(&self) -> u32 {
        self.estimated_process_time
    }

    pub fn entries(&self) -> &[CommandEntry] {
        &self.commands
    }

    pub fn entries_mut(&mut self) -> &mut [CommandEntry] {
        &mut self.commands
    }

    pub fn disable(&mut self, index: usize) -> bool {
        let Some(entry) = self.commands.get_mut(index) else {
            return false;
        };
        if !entry.enabled {
            return false;
        }
        entry.enabled = false;
        self.estimated_process_time = self
            .estimated_process_time
            .saturating_sub(entry.estimated_process_time);
        true
    }

    pub fn serialize_into(&self, header: &CommandListHeader, output: &mut [u8]) -> usize {
        let mut offset = 0usize;
        if output.len() < COMMAND_LIST_HEADER_SIZE {
            return 0;
        }

        offset += write_header(header, output);

        for entry in &self.commands {
            let cmd_header = CommandHeader::new(
                entry.command.id(),
                entry.command.serialized_size(),
                entry.estimated_process_time,
                entry.node_id,
            );
            let Some(target) =
                output.get_mut(offset..offset + std::mem::size_of::<CommandHeader>())
            else {
                break;
            };
            write_command_header(&cmd_header, entry.enabled, target);
            offset += std::mem::size_of::<CommandHeader>();

            let payload_size = entry.command.payload_size();
            if payload_size > 0 {
                let Some(target) = output.get_mut(offset..offset + payload_size) else {
                    break;
                };
                let written = entry.command.write_payload(target);
                if written != payload_size {
                    break;
                }
                offset += written;
            }
        }

        align_audio(offset as u64, 0x40) as usize
    }
}

fn write_header(header: &CommandListHeader, output: &mut [u8]) -> usize {
    let start = 0;
    output[start..start + COMMAND_LIST_HEADER_SIZE].fill(0);
    output[start..start + 8].copy_from_slice(&header.buffer_size.to_le_bytes());
    output[start + 8..start + 12].copy_from_slice(&header.command_count.to_le_bytes());
    output[start + 12..start + 20].copy_from_slice(&(header.samples_buffer as u64).to_le_bytes());
    output[start + 20..start + 22].copy_from_slice(&header.buffer_count.to_le_bytes());
    output[start + 24..start + 28].copy_from_slice(&header.sample_count.to_le_bytes());
    output[start + 28..start + 32].copy_from_slice(&header.sample_rate.to_le_bytes());
    32
}

fn write_command_header(header: &CommandHeader, enabled: bool, output: &mut [u8]) {
    output[0..4].copy_from_slice(&header.magic.to_le_bytes());
    output[4] = u8::from(enabled);
    output[5] = header.type_ as u8;
    output[6..8].copy_from_slice(&header.size.to_le_bytes());
    output[8..12].copy_from_slice(&header.estimated_process_time.to_le_bytes());
    output[12..16].copy_from_slice(&header.node_id.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::renderer::behavior::BehaviorInfo;

    #[test]
    fn serialize_zeroes_command_list_header_padding() {
        let behavior = BehaviorInfo::new();
        let estimator = CommandProcessingTimeEstimator::new(&behavior, 160, 6);
        let mut command_buffer = CommandBuffer::new(256, estimator);
        let header = CommandListHeader {
            buffer_size: 256,
            command_count: 1,
            samples_buffer: 0x1234,
            buffer_count: 6,
            sample_count: 160,
            sample_rate: 48_000,
        };
        let mut output = [0xAAu8; 256];

        assert!(command_buffer.generate_clear_mix_command(u32::MAX, 6));
        let written = command_buffer.serialize_into(&header, &mut output);

        assert!(written >= COMMAND_LIST_HEADER_SIZE);
        assert_eq!(&output[22..24], &[0, 0]);
    }
}
