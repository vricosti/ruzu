// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/abstract_syntax_list.h`
//!
//! Abstract syntax tree nodes for structured control flow.
//! The existing `SyntaxNode` in `program.rs` is a simplified version;
//! this module provides a closer match to the upstream representation
//! using block indices.

/// Type tag for abstract syntax nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbstractSyntaxNodeType {
    Block,
    If,
    EndIf,
    Loop,
    Repeat,
    Break,
    Return,
    Unreachable,
}

/// Abstract syntax node for structured control flow.
///
/// Uses block indices rather than pointers. The `data` field interpretation
/// depends on `node_type`.
#[derive(Debug, Clone)]
pub struct AbstractSyntaxNode {
    pub node_type: AbstractSyntaxNodeType,
    pub data: AbstractSyntaxData,
}

/// Data payload for each AST node type.
#[derive(Debug, Clone)]
pub enum AbstractSyntaxData {
    /// Block node: contains a block index.
    Block { block: u32 },
    /// If node: condition (as block index producing the condition), body block, merge block.
    If {
        cond_block: u32,
        body: u32,
        merge: u32,
    },
    /// EndIf node: merge block.
    EndIf { merge: u32 },
    /// Loop node: body block, continue block, merge block.
    Loop {
        body: u32,
        continue_block: u32,
        merge: u32,
    },
    /// Repeat node: condition block, loop header, merge block.
    Repeat {
        cond_block: u32,
        loop_header: u32,
        merge: u32,
    },
    /// Break node: condition block, merge block, skip block.
    Break {
        cond_block: u32,
        merge: u32,
        skip: u32,
    },
    /// Return node: no data.
    Return,
    /// Unreachable node: no data.
    Unreachable,
}

/// A list of abstract syntax nodes.
pub type AbstractSyntaxList = Vec<AbstractSyntaxNode>;
