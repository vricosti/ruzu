// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Structured control flow conversion.
//!
//! Ref: zuyu `frontend/maxwell/structured_control_flow.cpp`

use super::control_flow::{CfgBlock, Condition, EndClass};
use crate::ir::program::SyntaxNode;
use crate::ir::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StatementType {
    Code,
    Goto,
    Label,
    If,
    Loop,
    Break,
    Return,
    Kill,
    Unreachable,
    Function,
    Identity,
    Not,
    Or,
    SetVariable,
    SetIndirectBranchVariable,
    Variable,
    IndirectBranchCond,
}

fn has_children(statement_type: StatementType) -> bool {
    matches!(
        statement_type,
        StatementType::If | StatementType::Loop | StatementType::Function
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Expr {
    Identity(Condition),
    Not(Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Variable(u32),
    IndirectBranchCond(u32),
}

impl Expr {
    fn true_value() -> Self {
        Self::Identity(Condition::always())
    }

    fn false_value() -> Self {
        Self::Not(Box::new(Self::true_value()))
    }

    fn syntax_placeholder(&self) -> Value {
        Value::ImmU1(true)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum StructuredAction {
    SetVariable {
        block: u32,
        id: u32,
        expr: Expr,
    },
    SetIndirectBranchVariable {
        block: u32,
        branch_reg: u32,
        branch_offset: i32,
    },
    Condition {
        syntax_index: usize,
        block: u32,
        expr: Expr,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct StructuredSyntax {
    pub syntax: Vec<SyntaxNode>,
    pub actions: Vec<StructuredAction>,
    pub block_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Statement {
    Code {
        block: usize,
    },
    Goto {
        cond: Expr,
        label_id: u32,
    },
    Label {
        id: u32,
    },
    If {
        cond: Expr,
        children: Vec<Statement>,
    },
    Loop {
        cond: Expr,
        children: Vec<Statement>,
    },
    Break {
        cond: Expr,
    },
    Return,
    Kill,
    Unreachable,
    Function {
        children: Vec<Statement>,
    },
    SetVariable {
        id: u32,
        op: Expr,
    },
    SetIndirectBranchVariable {
        branch_reg: u32,
        branch_offset: i32,
    },
}

impl Statement {
    fn statement_type(&self) -> StatementType {
        match self {
            Self::Code { .. } => StatementType::Code,
            Self::Goto { .. } => StatementType::Goto,
            Self::Label { .. } => StatementType::Label,
            Self::If { .. } => StatementType::If,
            Self::Loop { .. } => StatementType::Loop,
            Self::Break { .. } => StatementType::Break,
            Self::Return => StatementType::Return,
            Self::Kill => StatementType::Kill,
            Self::Unreachable => StatementType::Unreachable,
            Self::Function { .. } => StatementType::Function,
            Self::SetVariable { .. } => StatementType::SetVariable,
            Self::SetIndirectBranchVariable { .. } => StatementType::SetIndirectBranchVariable,
        }
    }
}

/// Convert a CFG into a structured abstract syntax list.
pub fn structure_cfg(cfg_blocks: &[CfgBlock]) -> Vec<SyntaxNode> {
    structure_cfg_detailed(cfg_blocks).syntax
}

/// Convert a CFG into structured ASL plus the IR actions that upstream
/// `TranslatePass::Visit` emits into each header/merge block.
pub(crate) fn structure_cfg_detailed(cfg_blocks: &[CfgBlock]) -> StructuredSyntax {
    if cfg_blocks.is_empty() {
        return StructuredSyntax {
            syntax: Vec::new(),
            actions: Vec::new(),
            block_count: 0,
        };
    }

    let mut pass = GotoPass::new(cfg_blocks);
    pass.run();

    let mut translator = TranslatePass::new(cfg_blocks.len());
    translator.visit(pass.root_children(), None, None);
    translator.finish()
}

struct GotoPass {
    root: Vec<Statement>,
    goto_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StatementPath {
    parent: Vec<usize>,
    index: usize,
}

impl GotoPass {
    fn new(cfg_blocks: &[CfgBlock]) -> Self {
        let mut pass = Self {
            root: Vec::new(),
            goto_count: 0,
        };
        pass.build_tree(cfg_blocks);
        pass
    }

    fn run(&mut self) {
        while self.goto_count > 0 {
            let Some(goto_path) = self.find_last_goto_path() else {
                break;
            };
            self.remove_goto(goto_path);
            self.goto_count -= 1;
        }
    }

    fn root_children(&self) -> &[Statement] {
        &self.root
    }

    fn build_tree(&mut self, cfg_blocks: &[CfgBlock]) {
        for block in 0..cfg_blocks.len() {
            self.root.push(Statement::Label { id: block as u32 });
        }

        for (block_index, block) in cfg_blocks.iter().enumerate() {
            let Some(label_pos) = self.find_label_position(block_index as u32) else {
                continue;
            };
            let mut ip = label_pos + 1;

            self.root.insert(
                0,
                Statement::SetVariable {
                    id: block_index as u32,
                    op: Expr::false_value(),
                },
            );
            ip += 1;

            self.root.insert(
                ip,
                Statement::SetVariable {
                    id: block_index as u32,
                    op: Expr::false_value(),
                },
            );
            ip += 1;

            self.root.insert(ip, Statement::Code { block: block_index });
            ip += 1;

            match block.end_class {
                EndClass::Branch => self.build_branch(block, &mut ip),
                EndClass::IndirectBranch => {
                    self.root.insert(
                        ip,
                        Statement::SetIndirectBranchVariable {
                            branch_reg: 0,
                            branch_offset: 0,
                        },
                    );
                    self.root.insert(ip + 1, Statement::Unreachable);
                }
                EndClass::Call => {}
                EndClass::Exit if !block.cond.is_always() => {
                    self.root.insert(
                        ip,
                        Statement::If {
                            cond: Expr::Identity(block.cond),
                            children: vec![Statement::Return],
                        },
                    );
                }
                EndClass::Exit | EndClass::Return => self.root.insert(ip, Statement::Return),
                EndClass::Kill => self.root.insert(ip, Statement::Kill),
            }
        }
    }

    fn build_branch(&mut self, block: &CfgBlock, ip: &mut usize) {
        if block.cond.is_always() {
            if let Some(true_target) = block.branch_true {
                self.insert_goto(*ip, Expr::true_value(), true_target as u32);
            }
            return;
        }

        if let Some(true_target) = block.branch_true {
            self.insert_goto(*ip, Expr::Identity(block.cond), true_target as u32);
            *ip += 1;
        }
        if let Some(false_target) = block.branch_false {
            self.insert_goto(*ip, Expr::true_value(), false_target as u32);
        }
    }

    fn insert_goto(&mut self, index: usize, cond: Expr, label_id: u32) {
        self.root.insert(index, Statement::Goto { cond, label_id });
        self.goto_count += 1;
    }

    fn remove_goto(&mut self, goto_path: StatementPath) {
        let Some(Statement::Goto { label_id, .. }) = self.statement_at(&goto_path).cloned() else {
            return;
        };
        let Some(mut label_path) = self.find_label_path(label_id) else {
            return;
        };
        let mut goto_path = goto_path;

        if self.is_indirectly_related(&goto_path, &label_path) {
            while !self.is_directly_related(&goto_path, &label_path) {
                let Some(new_path) = self.move_outward(goto_path) else {
                    return;
                };
                goto_path = new_path;
                label_path = self.find_label_path(label_id).unwrap_or(label_path);
            }
        }

        if self.is_directly_related(&goto_path, &label_path) {
            let label_level = Self::level(&label_path);
            let mut goto_level = Self::level(&goto_path);
            if goto_level > label_level {
                while goto_level > label_level {
                    let Some(new_path) = self.move_outward(goto_path) else {
                        return;
                    };
                    goto_path = new_path;
                    label_path = self.find_label_path(label_id).unwrap_or(label_path);
                    goto_level -= 1;
                }
            } else if goto_level < label_level {
                if self.needs_lift(&goto_path, &label_path) {
                    let Some(new_path) = self.lift(goto_path, &label_path) else {
                        return;
                    };
                    goto_path = new_path;
                    label_path = self.find_label_path(label_id).unwrap_or(label_path);
                }
                while goto_level < label_level {
                    let Some(new_path) = self.move_inward(goto_path, &label_path) else {
                        return;
                    };
                    goto_path = new_path;
                    label_path = self.find_label_path(label_id).unwrap_or(label_path);
                    goto_level += 1;
                }
            }
        }

        if goto_path.parent != label_path.parent {
            return;
        }

        let goto_pos = goto_path.index;
        let label_pos = label_path.index;

        if goto_pos + 1 == label_pos {
            self.tree_mut(&goto_path.parent).remove(goto_pos);
        } else if goto_pos < label_pos {
            self.eliminate_as_conditional(&goto_path.parent, goto_pos, label_pos);
        } else {
            self.eliminate_as_loop(&goto_path.parent, goto_pos, label_pos);
        }
    }

    fn eliminate_as_conditional(
        &mut self,
        parent_path: &[usize],
        goto_pos: usize,
        label_pos: usize,
    ) {
        let tree = self.tree_mut(parent_path);
        let Statement::Goto { cond, .. } = tree[goto_pos].clone() else {
            return;
        };
        let children: Vec<_> = tree.drain(goto_pos + 1..label_pos).collect();
        tree.insert(
            goto_pos,
            Statement::If {
                cond: Expr::Not(Box::new(cond)),
                children,
            },
        );
        tree.remove(goto_pos + 1);
    }

    fn eliminate_as_loop(&mut self, parent_path: &[usize], goto_pos: usize, label_pos: usize) {
        let tree = self.tree_mut(parent_path);
        let Statement::Goto { cond, .. } = tree[goto_pos].clone() else {
            return;
        };
        let children: Vec<_> = tree.drain(label_pos..goto_pos).collect();
        let insert_pos = label_pos;
        tree.insert(insert_pos, Statement::Loop { cond, children });
        tree.remove(insert_pos + 1);
    }

    fn find_label_position(&self, label_id: u32) -> Option<usize> {
        self.root
            .iter()
            .position(|statement| matches!(statement, Statement::Label { id } if *id == label_id))
    }

    fn find_last_goto_path(&self) -> Option<StatementPath> {
        find_last_goto_in_tree(&self.root, Vec::new())
    }

    fn level(path: &StatementPath) -> usize {
        path.parent.len() + 1
    }

    fn is_directly_related(&self, goto_path: &StatementPath, label_path: &StatementPath) -> bool {
        let goto_level = Self::level(goto_path);
        let label_level = Self::level(label_path);
        let (min_path, max_path, min_level, max_level) = if label_level < goto_level {
            (label_path, goto_path, label_level, goto_level)
        } else {
            (goto_path, label_path, goto_level, label_level)
        };
        let mut max_parent = max_path.parent.clone();
        for _ in min_level..max_level {
            let Some(parent_index) = max_parent.pop() else {
                return false;
            };
            let _ = parent_index;
        }
        min_path.parent == max_parent
    }

    fn is_indirectly_related(&self, goto_path: &StatementPath, label_path: &StatementPath) -> bool {
        goto_path.parent != label_path.parent && !self.is_directly_related(goto_path, label_path)
    }

    fn sibling_from_nephew(
        &self,
        uncle_path: &StatementPath,
        nephew_path: &StatementPath,
    ) -> Option<usize> {
        if nephew_path.parent.len() < uncle_path.parent.len() {
            return None;
        }
        nephew_path.parent.get(uncle_path.parent.len()).copied()
    }

    fn are_ordered(&self, parent_path: &[usize], left: usize, right: usize) -> bool {
        let tree = self.tree(parent_path);
        right < tree.len() && left <= right
    }

    fn needs_lift(&self, goto_path: &StatementPath, label_path: &StatementPath) -> bool {
        let Some(sibling) = self.sibling_from_nephew(goto_path, label_path) else {
            return false;
        };
        self.are_ordered(&goto_path.parent, sibling, goto_path.index)
    }

    fn move_outward(&mut self, goto_path: StatementPath) -> Option<StatementPath> {
        let (&parent_index, parent_parent_path) = goto_path.parent.split_last()?;
        let parent_parent_path = parent_parent_path.to_vec();
        match self
            .tree(&parent_parent_path)
            .get(parent_index)?
            .statement_type()
        {
            StatementType::If => self.move_outward_if(goto_path, &parent_parent_path, parent_index),
            StatementType::Loop => {
                self.move_outward_loop(goto_path, &parent_parent_path, parent_index)
            }
            _ => None,
        }
    }

    fn move_outward_if(
        &mut self,
        goto_path: StatementPath,
        parent_parent_path: &[usize],
        if_index: usize,
    ) -> Option<StatementPath> {
        let Statement::Goto { cond, label_id } = self.statement_at(&goto_path)?.clone() else {
            return None;
        };
        let if_tree = self.tree_mut(&goto_path.parent);
        if_tree.insert(
            goto_path.index,
            Statement::SetVariable {
                id: label_id,
                op: cond,
            },
        );
        let mut if_body: Vec<_> = if_tree.drain(goto_path.index + 2..).collect();
        if !if_body.is_empty() {
            if_tree.insert(
                goto_path.index + 1,
                Statement::If {
                    cond: Expr::Not(Box::new(Expr::Variable(label_id))),
                    children: std::mem::take(&mut if_body),
                },
            );
            if_tree.remove(goto_path.index + 2);
        } else {
            if_tree.remove(goto_path.index + 1);
        }

        let parent_tree = self.tree_mut(parent_parent_path);
        let insert_pos = if_index + 1;
        parent_tree.insert(
            insert_pos,
            Statement::Goto {
                cond: Expr::Variable(label_id),
                label_id,
            },
        );
        Some(StatementPath {
            parent: parent_parent_path.to_vec(),
            index: insert_pos,
        })
    }

    fn move_outward_loop(
        &mut self,
        goto_path: StatementPath,
        parent_parent_path: &[usize],
        loop_index: usize,
    ) -> Option<StatementPath> {
        let Statement::Goto { cond, label_id } = self.statement_at(&goto_path)?.clone() else {
            return None;
        };
        let loop_tree = self.tree_mut(&goto_path.parent);
        loop_tree.insert(
            goto_path.index,
            Statement::SetVariable {
                id: label_id,
                op: cond,
            },
        );
        loop_tree.insert(
            goto_path.index + 1,
            Statement::Break {
                cond: Expr::Variable(label_id),
            },
        );
        loop_tree.remove(goto_path.index + 2);

        let parent_tree = self.tree_mut(parent_parent_path);
        let insert_pos = loop_index + 1;
        parent_tree.insert(
            insert_pos,
            Statement::Goto {
                cond: Expr::Variable(label_id),
                label_id,
            },
        );
        Some(StatementPath {
            parent: parent_parent_path.to_vec(),
            index: insert_pos,
        })
    }

    fn move_inward(
        &mut self,
        goto_path: StatementPath,
        label_path: &StatementPath,
    ) -> Option<StatementPath> {
        let Statement::Goto { cond, label_id } = self.statement_at(&goto_path)?.clone() else {
            return None;
        };
        let label_nested_index = self.sibling_from_nephew(&goto_path, label_path)?;
        let tree = self.tree_mut(&goto_path.parent);
        tree.insert(
            goto_path.index,
            Statement::SetVariable {
                id: label_id,
                op: cond,
            },
        );
        let adjusted_label_nested_index = if label_nested_index > goto_path.index {
            label_nested_index + 1
        } else {
            label_nested_index
        };
        let if_body: Vec<_> = tree
            .drain(goto_path.index + 2..adjusted_label_nested_index)
            .collect();
        let if_body_len = if_body.len();
        let nested_index_after_drain = adjusted_label_nested_index - if_body_len;
        let nested_index = if !if_body.is_empty() {
            tree.insert(
                goto_path.index + 1,
                Statement::If {
                    cond: Expr::Not(Box::new(Expr::Variable(label_id))),
                    children: if_body,
                },
            );
            tree.remove(goto_path.index + 2);
            nested_index_after_drain
        } else {
            tree.remove(goto_path.index + 1);
            nested_index_after_drain - 1
        };
        match &mut tree[nested_index] {
            Statement::If {
                cond: nested_cond,
                children,
            } => {
                *nested_cond = Expr::Or(
                    Box::new(Expr::Variable(label_id)),
                    Box::new(nested_cond.clone()),
                );
                children.insert(
                    0,
                    Statement::Goto {
                        cond: Expr::Variable(label_id),
                        label_id,
                    },
                );
            }
            Statement::Loop { children, .. } => {
                children.insert(
                    0,
                    Statement::Goto {
                        cond: Expr::Variable(label_id),
                        label_id,
                    },
                );
            }
            _ => return None,
        }
        let mut parent = goto_path.parent;
        parent.push(nested_index);
        Some(StatementPath { parent, index: 0 })
    }

    fn lift(
        &mut self,
        goto_path: StatementPath,
        label_path: &StatementPath,
    ) -> Option<StatementPath> {
        let Statement::Goto { cond, label_id } = self.statement_at(&goto_path)?.clone() else {
            return None;
        };
        let label_nested_index = self.sibling_from_nephew(&goto_path, label_path)?;
        let tree = self.tree_mut(&goto_path.parent);
        if tree[label_nested_index..goto_path.index]
            .iter()
            .any(contains_break)
        {
            return None;
        }
        let loop_body: Vec<_> = tree.drain(label_nested_index..goto_path.index).collect();
        let insert_pos = label_nested_index;
        tree.insert(
            insert_pos,
            Statement::Loop {
                cond: Expr::Variable(label_id),
                children: loop_body,
            },
        );
        tree.remove(insert_pos + 1);
        if let Statement::Loop { children, .. } = &mut tree[insert_pos] {
            children.insert(
                0,
                Statement::Goto {
                    cond: Expr::Variable(label_id),
                    label_id,
                },
            );
            children.push(Statement::SetVariable {
                id: label_id,
                op: cond,
            });
        }
        let mut parent = goto_path.parent;
        parent.push(insert_pos);
        Some(StatementPath { parent, index: 0 })
    }

    fn find_label_path(&self, label_id: u32) -> Option<StatementPath> {
        find_label_in_tree(&self.root, Vec::new(), label_id)
    }

    fn statement_at(&self, path: &StatementPath) -> Option<&Statement> {
        self.tree(&path.parent).get(path.index)
    }

    fn tree(&self, path: &[usize]) -> &[Statement] {
        let mut tree = self.root.as_slice();
        for &index in path {
            tree = match tree.get(index) {
                Some(Statement::If { children, .. })
                | Some(Statement::Loop { children, .. })
                | Some(Statement::Function { children }) => children.as_slice(),
                _ => &[],
            };
        }
        tree
    }

    fn tree_mut(&mut self, path: &[usize]) -> &mut Vec<Statement> {
        tree_mut_at(&mut self.root, path)
    }
}

fn tree_mut_at<'a>(tree: &'a mut Vec<Statement>, path: &[usize]) -> &'a mut Vec<Statement> {
    if path.is_empty() {
        return tree;
    }
    let (head, tail) = path.split_first().expect("non-empty path");
    match &mut tree[*head] {
        Statement::If { children, .. }
        | Statement::Loop { children, .. }
        | Statement::Function { children } => tree_mut_at(children, tail),
        _ => panic!("path does not point to a statement with children"),
    }
}

fn find_last_goto_in_tree(tree: &[Statement], parent: Vec<usize>) -> Option<StatementPath> {
    let mut result = None;
    for (index, statement) in tree.iter().enumerate() {
        let mut child_parent = parent.clone();
        child_parent.push(index);
        match statement {
            Statement::Goto { .. } => {
                result = Some(StatementPath {
                    parent: parent.clone(),
                    index,
                });
            }
            Statement::If { children, .. }
            | Statement::Loop { children, .. }
            | Statement::Function { children } => {
                if let Some(path) = find_last_goto_in_tree(children, child_parent) {
                    result = Some(path);
                }
            }
            _ => {}
        }
    }
    result
}

fn find_label_in_tree(
    tree: &[Statement],
    parent: Vec<usize>,
    label_id: u32,
) -> Option<StatementPath> {
    for (index, statement) in tree.iter().enumerate() {
        if matches!(statement, Statement::Label { id } if *id == label_id) {
            return Some(StatementPath { parent, index });
        }
        let mut child_parent = parent.clone();
        child_parent.push(index);
        match statement {
            Statement::If { children, .. }
            | Statement::Loop { children, .. }
            | Statement::Function { children } => {
                if let Some(path) = find_label_in_tree(children, child_parent, label_id) {
                    return Some(path);
                }
            }
            _ => {}
        }
    }
    None
}

fn contains_break(statement: &Statement) -> bool {
    match statement {
        Statement::Break { .. } => true,
        Statement::If { children, .. }
        | Statement::Loop { children, .. }
        | Statement::Function { children } => children.iter().any(contains_break),
        _ => false,
    }
}

struct TranslatePass {
    syntax: Vec<SyntaxNode>,
    actions: Vec<StructuredAction>,
    next_block: u32,
    current_block: Option<u32>,
}

impl TranslatePass {
    fn new(original_block_count: usize) -> Self {
        Self {
            syntax: Vec::new(),
            actions: Vec::new(),
            next_block: original_block_count as u32,
            current_block: None,
        }
    }

    fn finish(self) -> StructuredSyntax {
        StructuredSyntax {
            syntax: self.syntax,
            actions: self.actions,
            block_count: self.next_block as usize,
        }
    }

    fn create_block(&mut self) -> u32 {
        let block = self.next_block;
        self.next_block += 1;
        block
    }

    fn ensure_block(&mut self) -> u32 {
        if let Some(block) = self.current_block {
            return block;
        }
        let block = self.create_block();
        self.syntax.push(SyntaxNode::Block(block));
        self.current_block = Some(block);
        block
    }

    fn merge_block(&mut self) -> u32 {
        // Upstream returns a fresh IR block even when it can attach it to an
        // existing forward statement. Keeping merge blocks explicit preserves
        // SPIR-V dominance and avoids detached selection/loop merge nodes.
        self.create_block()
    }

    fn visit(
        &mut self,
        tree: &[Statement],
        break_block: Option<u32>,
        fallthrough_block: Option<u32>,
    ) {
        for statement in tree {
            match statement {
                Statement::Label { .. } => {}
                Statement::Code { block } => {
                    self.syntax.push(SyntaxNode::Block(*block as u32));
                    self.current_block = Some(*block as u32);
                }
                Statement::SetVariable { id, op } => {
                    let block = self.ensure_block();
                    self.actions.push(StructuredAction::SetVariable {
                        block,
                        id: *id,
                        expr: op.clone(),
                    });
                }
                Statement::SetIndirectBranchVariable {
                    branch_reg,
                    branch_offset,
                } => {
                    let block = self.ensure_block();
                    self.actions
                        .push(StructuredAction::SetIndirectBranchVariable {
                            block,
                            branch_reg: *branch_reg,
                            branch_offset: *branch_offset,
                        });
                }
                Statement::If { cond, children } => {
                    let header_block = self.ensure_block();
                    let merge = self.merge_block();
                    let if_index = self.syntax.len();
                    self.syntax.push(SyntaxNode::If {
                        cond: cond.syntax_placeholder(),
                        body: merge,
                        merge,
                    });
                    self.actions.push(StructuredAction::Condition {
                        syntax_index: if_index,
                        block: header_block,
                        expr: cond.clone(),
                    });

                    self.current_block = None;
                    let body_index = self.syntax.len();
                    self.visit(children, break_block, Some(merge));
                    let body = first_block_from_syntax(&self.syntax[body_index..]).unwrap_or(merge);
                    if let SyntaxNode::If { body: slot, .. } = &mut self.syntax[if_index] {
                        *slot = body;
                    }

                    self.syntax.push(SyntaxNode::EndIf { merge });
                    self.syntax.push(SyntaxNode::Block(merge));
                    self.current_block = Some(merge);
                }
                Statement::Loop { cond, children } => {
                    let loop_header = self.create_block();
                    self.syntax.push(SyntaxNode::Block(loop_header));
                    self.current_block = Some(loop_header);

                    let continue_block = self.create_block();
                    let merge = self.merge_block();
                    let loop_index = self.syntax.len();
                    self.syntax.push(SyntaxNode::Loop {
                        body: merge,
                        continue_block,
                        merge,
                    });

                    self.current_block = None;
                    let body_index = self.syntax.len();
                    self.visit(children, Some(merge), Some(continue_block));
                    let body = first_block_from_syntax(&self.syntax[body_index..]).unwrap_or(merge);
                    if let SyntaxNode::Loop { body: slot, .. } = &mut self.syntax[loop_index] {
                        *slot = body;
                    }

                    self.syntax.push(SyntaxNode::Block(continue_block));
                    self.current_block = Some(continue_block);
                    let repeat_index = self.syntax.len();
                    self.syntax.push(SyntaxNode::Repeat {
                        cond: cond.syntax_placeholder(),
                        loop_header,
                        merge,
                    });
                    self.actions.push(StructuredAction::Condition {
                        syntax_index: repeat_index,
                        block: continue_block,
                        expr: cond.clone(),
                    });

                    self.syntax.push(SyntaxNode::Block(merge));
                    self.current_block = Some(merge);
                }
                Statement::Break { cond } => {
                    let header_block = self.ensure_block();
                    let skip = self.merge_block();
                    let merge = break_block.unwrap_or(skip);
                    let break_index = self.syntax.len();
                    self.syntax.push(SyntaxNode::Break {
                        cond: cond.syntax_placeholder(),
                        merge,
                        skip,
                    });
                    self.actions.push(StructuredAction::Condition {
                        syntax_index: break_index,
                        block: header_block,
                        expr: cond.clone(),
                    });
                    self.syntax.push(SyntaxNode::Block(skip));
                    self.current_block = Some(skip);
                }
                Statement::Return => {
                    self.ensure_block();
                    let return_block = self.create_block();
                    self.syntax.push(SyntaxNode::Block(return_block));
                    self.current_block = None;
                    self.syntax.push(SyntaxNode::Return);
                }
                Statement::Kill => {
                    self.ensure_block();
                    let demote_block = self.merge_block();
                    self.syntax.push(SyntaxNode::Block(demote_block));
                    self.current_block = Some(demote_block);
                }
                Statement::Unreachable | Statement::Goto { .. } => {
                    self.ensure_block();
                    self.current_block = None;
                    self.syntax.push(SyntaxNode::Unreachable);
                }
                Statement::Function { children } => {
                    self.visit(children, break_block, fallthrough_block)
                }
            }
        }

        if let (Some(_current), Some(fallthrough)) = (self.current_block, fallthrough_block) {
            if !matches!(self.syntax.last(), Some(SyntaxNode::Block(block)) if *block == fallthrough)
            {
                self.syntax.push(SyntaxNode::Block(fallthrough));
            }
            self.current_block = Some(fallthrough);
        } else if self.current_block.is_some() && fallthrough_block.is_none() {
            self.current_block = None;
            self.syntax.push(SyntaxNode::Unreachable);
        }
    }
}

fn first_block_from_syntax(nodes: &[SyntaxNode]) -> Option<u32> {
    nodes.iter().find_map(|node| match node {
        SyntaxNode::Block(block) => Some(*block),
        _ => None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block(end_class: EndClass, cond: Condition) -> CfgBlock {
        CfgBlock {
            begin: 0,
            end: 1,
            end_class,
            branch_true: None,
            branch_false: None,
            cond,
            stack_depth: 0,
        }
    }

    #[test]
    fn conditional_exit_does_not_emit_flat_return() {
        let mut cfg_block = block(
            EndClass::Exit,
            Condition {
                pred: 0,
                negated: false,
            },
        );
        cfg_block.branch_false = Some(1);
        let syntax = structure_cfg(&[cfg_block, block(EndClass::Return, Condition::always())]);

        assert!(matches!(
            syntax.as_slice(),
            [
                SyntaxNode::Block(0),
                SyntaxNode::If { .. },
                SyntaxNode::Return,
                SyntaxNode::EndIf { merge: 1 },
                SyntaxNode::Block(1),
                SyntaxNode::Return
            ]
        ));
    }

    #[test]
    fn unconditional_exit_still_returns() {
        let syntax = structure_cfg(&[block(EndClass::Exit, Condition::always())]);

        assert!(matches!(
            syntax.as_slice(),
            [SyntaxNode::Block(0), SyntaxNode::Return]
        ));
    }

    #[test]
    fn conditional_forward_branch_bodies_use_inverted_condition() {
        let mut branch = block(
            EndClass::Branch,
            Condition {
                pred: 0,
                negated: false,
            },
        );
        branch.branch_true = Some(2);
        branch.branch_false = Some(1);

        let syntax = structure_cfg(&[
            branch,
            block(EndClass::Branch, Condition::always()),
            block(EndClass::Return, Condition::always()),
        ]);

        assert!(matches!(
            syntax.as_slice(),
            [
                SyntaxNode::Block(0),
                SyntaxNode::If {
                    cond: Value::ImmU1(false),
                    body: 1,
                    merge: 2,
                },
                SyntaxNode::Block(1),
                SyntaxNode::EndIf { merge: 2 },
                SyntaxNode::Block(2),
                SyntaxNode::Return,
            ]
        ));
    }

    #[test]
    fn nested_goto_is_moved_outward_from_if_before_elimination() {
        let mut pass = GotoPass {
            root: vec![
                Statement::If {
                    cond: Expr::true_value(),
                    children: vec![
                        Statement::Code { block: 0 },
                        Statement::Goto {
                            cond: Expr::true_value(),
                            label_id: 1,
                        },
                        Statement::Code { block: 1 },
                    ],
                },
                Statement::Label { id: 1 },
                Statement::Code { block: 2 },
            ],
            goto_count: 1,
        };

        pass.run();

        assert_eq!(count_gotos(pass.root_children()), 0);
        assert!(matches!(
            pass.root_children(),
            [
                Statement::If { children, .. },
                Statement::Label { id: 1 },
                Statement::Code { block: 2 },
            ] if matches!(
                children.as_slice(),
                [
                    Statement::Code { block: 0 },
                    Statement::SetVariable { id: 1, .. },
                    Statement::If { children, .. },
                ] if matches!(children.as_slice(), [Statement::Code { block: 1 }])
            )
        ));
    }

    #[test]
    fn goto_moves_inward_to_nested_label() {
        let mut pass = GotoPass {
            root: vec![
                Statement::Goto {
                    cond: Expr::true_value(),
                    label_id: 1,
                },
                Statement::If {
                    cond: Expr::true_value(),
                    children: vec![Statement::Label { id: 1 }, Statement::Code { block: 0 }],
                },
                Statement::Code { block: 1 },
            ],
            goto_count: 1,
        };

        pass.run();

        assert_eq!(count_gotos(pass.root_children()), 0);
        assert!(matches!(
            pass.root_children(),
            [
                Statement::SetVariable { id: 1, .. },
                Statement::If {
                    cond: Expr::Or(_, _),
                    children,
                },
                Statement::Code { block: 1 },
            ] if matches!(
                children.as_slice(),
                [Statement::Label { id: 1 }, Statement::Code { block: 0 }]
            )
        ));
    }

    #[test]
    fn goto_lifts_when_nested_label_sibling_precedes_goto() {
        let mut pass = GotoPass {
            root: vec![
                Statement::If {
                    cond: Expr::true_value(),
                    children: vec![Statement::Label { id: 1 }, Statement::Code { block: 0 }],
                },
                Statement::Code { block: 1 },
                Statement::Goto {
                    cond: Expr::true_value(),
                    label_id: 1,
                },
                Statement::Code { block: 2 },
            ],
            goto_count: 1,
        };

        pass.run();

        assert_eq!(count_gotos(pass.root_children()), 0);
        assert!(matches!(
            pass.root_children(),
            [
                Statement::Loop { children, .. },
                Statement::Code { block: 2 },
            ] if children.iter().any(|stmt| matches!(
                stmt,
                Statement::SetVariable { id: 1, .. }
            ))
        ));
    }

    fn count_gotos(tree: &[Statement]) -> usize {
        tree.iter()
            .map(|statement| match statement {
                Statement::Goto { .. } => 1,
                Statement::If { children, .. }
                | Statement::Loop { children, .. }
                | Statement::Function { children } => count_gotos(children),
                _ => 0,
            })
            .sum()
    }

    #[test]
    fn statement_type_child_classification_matches_upstream() {
        assert!(has_children(StatementType::If));
        assert!(has_children(StatementType::Loop));
        assert!(has_children(StatementType::Function));
        assert!(!has_children(StatementType::Code));
        assert!(!has_children(StatementType::Goto));
    }
}
