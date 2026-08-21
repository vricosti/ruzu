use crate::ir::block::Block;
use crate::ir::terminal::Terminal;

/// A64 Merge Interpret Blocks pass.
///
/// If the block ends with a Terminal::Interpret, this pass checks if the
/// following instruction(s) would also fall through to Interpret.
/// If so, it merges them by incrementing the interpret terminal's
/// `num_instructions` count and adjusting the cycle count.
///
/// This is a simplified version — the full C++ version re-decodes following
/// instructions to check. Here we just handle the existing terminal.
pub fn a64_merge_interpret_blocks(block: &mut Block) {
    // If the terminal is Interpret with num_instructions == 1,
    // there's nothing to merge without a memory callback.
    // The full implementation would need a callback to read the next instruction.
    // For now, this is a no-op placeholder that will be filled in when
    // we have the UserCallbacks interface available.
    if let Terminal::Interpret {
        num_instructions, ..
    } = &block.terminal
    {
        // Already merged or nothing to merge without code read callback
        let _ = num_instructions;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::block::Block;
    use crate::ir::location::LocationDescriptor;

    #[test]
    fn test_noop_on_non_interpret() {
        let mut block = Block::new(LocationDescriptor(0));
        block.set_terminal(Terminal::ReturnToDispatch);
        a64_merge_interpret_blocks(&mut block);
        assert!(matches!(block.terminal, Terminal::ReturnToDispatch));
    }
}
