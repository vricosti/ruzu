use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::ir::terminal::Terminal;

impl<'a> TranslatorVisitor<'a> {
    /// SVC - Supervisor call
    pub fn svc(&mut self, inst: &DecodedInst) -> bool {
        let imm16 = inst.imm16();
        let loc = self.ir.current_location.unwrap();

        // Push RSB for the return
        let return_loc = loc.advance_pc(4).to_location();
        self.ir.ir().push_rsb(return_loc);

        // Set PC to next instruction
        let next_pc = self.ir.ir().imm64(loc.pc() + 4);
        self.ir.set_pc(next_pc);

        // Call supervisor
        self.ir.call_supervisor(imm16);

        // Check for halt after supervisor call
        self.ir.set_term(Terminal::check_halt(Terminal::PopRSBHint));
        false
    }

    /// BRK - Breakpoint
    pub fn brk(&mut self, _inst: &DecodedInst) -> bool {
        self.raise_exception(crate::frontend::a64::types::Exception::Breakpoint)
    }
}
