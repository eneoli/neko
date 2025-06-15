use std::collections::HashMap;

use chumsky::container::Seq;

use crate::compile::{
    ast::{core::Stmt, desugared::Expr, Ast, Core, FunctionDecl},
    ir::graph::{
        export::y_comp::YCompExport, node::{binary::BinaryNodeOp, constant::ConstantNode, Node}, optimization::Optimization, BlockId, IrGraph, NodeId
    },
};

pub fn construct_graph_from_core(ast: Ast<Core>) -> IrGraph {
    SsaTranslation::new().translate(ast)
}

struct SsaTranslation {
    graph: IrGraph,
    current_block: BlockId,
    current_def: HashMap<String, HashMap<NodeId, NodeId>>,
    sealed_blocks: Vec<BlockId>,
    incomplete_phis: HashMap<NodeId, HashMap<String, NodeId>>,
    loops: Vec<(BlockId, BlockId)>, // (cond_block, next_block)
    side_effect: HashMap<BlockId, NodeId>,
    optimization: Optimization,
}

impl SsaTranslation {
    pub fn new() -> Self {
        let graph = IrGraph::new();
        let start = graph.start;

        let mut translation = Self {
            graph,
            current_block: start,
            current_def: HashMap::new(),
            sealed_blocks: vec![],
            incomplete_phis: HashMap::new(),
            loops: vec![],
            side_effect: HashMap::new(),
            optimization: Optimization::new(),
        };

        translation.seal_block(translation.graph.start);

        translation
    }

    pub fn translate(mut self, ast: Ast<Core>) -> IrGraph {
        let Ast {
            main: FunctionDecl { body, .. },
            ..
        } = ast;

        self.translate_stmts(body);

        // we are done
        self.seal_block(self.current_block);

        YCompExport::new().export_to_file(&self.graph, "ir.vcg");

        self.graph
    }

    fn translate_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            let mut last_inst = false;
            if let Stmt::Return(_, _) = stmt {
                last_inst = true;
            }

            if let Stmt::Break(_) = stmt {
                last_inst = true;
            }

            if let Stmt::Continue(_) = stmt {
                last_inst = true;
            }

            self.translate_stmt(stmt);

            if last_inst {
                break;
            }
        }
    }

    fn translate_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Decl(_, _, _) => {}
            Stmt::Assign(name, expr, _) => {
                let expr = self.translate_expr(expr);
                self.write_variable(name, self.current_block, expr);
            }
            Stmt::Return(expr, _) => {
                let expr = self.translate_expr(expr);
                let side_effect = self.current_side_effect();
                Self::new_return(&mut self.graph, self.current_block, expr, side_effect);
                let after_return_block = self.new_block();
                self.set_block(after_return_block);
            }
            Stmt::Block(stmts) => self.translate_stmts(stmts),
            Stmt::Break(_) => {
                let (_, after_loop_block) = self.peek_loop();
                let current_side_effect = self.current_side_effect();
                Self::new_jmp(
                    &mut self.graph,
                    self.current_block,
                    after_loop_block,
                    current_side_effect,
                );
                self.seal_block(self.current_block);
                let next_block = self.new_block();
                self.set_block(next_block);
            }
            Stmt::Continue(_) => {
                let (cond_block, _) = self.peek_loop();
                let current_side_effect = self.current_side_effect();
                Self::new_jmp(
                    &mut self.graph,
                    self.current_block,
                    cond_block,
                    current_side_effect,
                );
                self.seal_block(self.current_block);
                let next_block = self.new_block();
                self.set_block(next_block);
            }
            Stmt::If(cond, then, otherwise) => {
                // Parent Block ends here and cannot be visited again
                let parent_block = self.current_block();
                self.seal_block(parent_block);

                // And then otherwise and next blocks
                let then_block = self.new_block();
                let next_block = self.new_block();
                let otherwise_block = if otherwise.is_some() {
                    self.new_block()
                } else {
                    next_block
                };

                // Translate cond in parent block and add conditional jump
                let cond = self.translate_expr(cond);
                let parent_side_effect = self.side_effect(parent_block);
                Self::new_cond_jmp(
                    &mut self.graph,
                    parent_block,
                    cond,
                    then_block,
                    otherwise_block,
                    parent_side_effect,
                );

                self.seal_block(then_block);

                // Translate then block
                // Add Jump from then to next
                // Seal block after that, as there is no other way to get in it.
                self.set_block(then_block);
                self.translate_stmt(*then);
                // then might create new blocks, so use current block
                let current_side_effect = self.current_side_effect();
                Self::new_jmp(
                    &mut self.graph,
                    self.current_block,
                    next_block,
                    current_side_effect,
                );
                // self.seal_block(then_block);

                // Translate otherwise block
                // Add Jump from otherwise to next
                // Seal block after that, as there is no other way to get in it.
                if let Some(otherwise) = otherwise {
                    self.set_block(otherwise_block);
                    self.translate_stmt(*otherwise);
                    // then might create new blocks, so use current block
                    let current_side_effect = self.current_side_effect();
                    Self::new_jmp(
                        &mut self.graph,
                        self.current_block,
                        next_block,
                        current_side_effect,
                    );
                    self.seal_block(otherwise_block);
                }

                // Continue in block after if/else
                self.seal_block(next_block);
                self.set_block(next_block);
            }
            Stmt::While(expr, body) => {
                // Parent Block ends here and cannot be visited again (no nested break/continue)
                let parent_block = self.current_block();
                self.seal_block(parent_block);

                // Jump from parent into condition check
                let condition_block = self.new_block();
                let parent_side_effect = self.side_effect(parent_block);
                Self::new_jmp(
                    &mut self.graph,
                    parent_block,
                    condition_block,
                    parent_side_effect,
                );

                // Add Loop Body and afer loop blocks
                let body_block = self.new_block();
                self.seal_block(body_block);
                let next_block = self.new_block();
                self.push_loop(condition_block, next_block);

                // Place condition expr in condition block
                self.set_block(condition_block);
                let expr = self.translate_expr(expr);

                let condition_side_effect = self.side_effect(condition_block);
                Self::new_cond_jmp(
                    &mut self.graph,
                    condition_block,
                    expr,
                    body_block,
                    next_block,
                    condition_side_effect,
                );

                // Populate body block
                self.set_block(body_block);
                self.translate_stmt(*body);
                // Body might create new blocks, so use current block
                let new_body_block = self.current_block;
                let new_body_side_effect = self.side_effect(new_body_block);
                Self::new_jmp(
                    &mut self.graph,
                    new_body_block,
                    condition_block,
                    new_body_side_effect,
                );
                self.seal_block(condition_block);

                // // Place condition expr in condition block
                // self.set_block(condition_block);
                // let expr = self.translate_expr(expr);

                // Add Conditional jump into body or next
                // There is no other way to get into body, so seal body block
                // Self::new_cond_jmp(
                //     &mut self.graph,
                //     condition_block,
                //     expr,
                //     body_block,
                //     next_block,
                // );
                self.seal_block(new_body_block);

                self.pop_loop();

                // Continue in block after loop
                self.seal_block(next_block);
                self.set_block(next_block);
            }
        }
    }

    fn translate_expr(&mut self, expr: Expr) -> NodeId {
        let effectful = expr.has_side_effect();

        match expr {
            Expr::Int(value, _) => Self::new_constant(&mut self.graph, ConstantNode::Int(value)),
            Expr::Bool(value, _) => Self::new_constant(&mut self.graph, ConstantNode::Bool(value)),
            Expr::Ident(name, _) => self.read_variable(name, self.current_block),
            Expr::Binary(op, lhs, rhs) => {
                let effectful = op.has_side_effect();
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                let mut id = Self::new_binary(&mut self.graph, self.current_block, op.into(), lhs, rhs);

                id = self.optimization.local(id, &mut self.graph);

                // TODO after optimization really effectful?
                if effectful {
                    let current_side_effect = self.current_side_effect();
                    Self::append_side_effect(&mut self.graph, id, current_side_effect);
                    self.side_effect.insert(self.current_block(), id);
                }

                id
            }
            Expr::Ternary(cond, then, otherwise) => {
                // TODO optimization
                let old_side_effect = self.current_side_effect();
                let cond = self.translate_expr(*cond);
                let then = self.translate_expr(*then);
                let otherwise = self.translate_expr(*otherwise);
                let id =
                    Self::new_ternary(&mut self.graph, self.current_block, cond, then, otherwise);

                if effectful {
                    self.side_effect.insert(self.current_block(), id);
                    Self::append_side_effect(&mut self.graph, id, old_side_effect);
                }

                id

            }
        }
    }

    fn current_block(&self) -> NodeId {
        self.current_block
    }

    fn set_block(&mut self, block: NodeId) {
        // TODO assert block is block
        self.current_block = block;
    }

    fn new_block(&mut self) -> NodeId {
        let id = self.graph.add_block();

        id
    }

    fn current_side_effect(&self) -> Option<NodeId> {
        self.side_effect.get(&self.current_block).copied()
    }

    fn side_effect(&self, block: BlockId) -> Option<NodeId> {
        self.side_effect.get(&block).copied()
    }

    fn push_loop(&mut self, cond: NodeId, next: NodeId) {
        self.loops.push((cond, next));
    }

    fn peek_loop(&mut self) -> (NodeId, NodeId) {
        debug_assert!(self.loops.len() > 0, "No loop on the stack");

        self.loops[self.loops.len() - 1]
    }

    fn pop_loop(&mut self) {
        debug_assert!(self.loops.len() > 0, "No loop on the stack");

        self.loops.pop();
    }

    fn write_variable(&mut self, variable: String, block: NodeId, value: NodeId) {
        if !self.current_def.contains_key(&variable) {
            self.current_def.insert(variable.clone(), HashMap::new());
        }

        self.current_def
            .get_mut(&variable)
            .unwrap()
            .insert(block, value);
    }

    fn read_variable(&mut self, variable: String, block: NodeId) -> NodeId {
        if self.current_def[&variable].contains_key(&block) {
            return self.current_def[&variable][&block];
        }

        self.read_variable_recusrive(variable, block)
    }

    fn read_variable_recusrive(&mut self, variable: String, block: NodeId) -> NodeId {
        let mut val;
        if !self.sealed_blocks.contains(&block) {
            let phi = Node::Phi;
            val = self.graph.add_node(block, phi);

            if !self.incomplete_phis.contains_key(&block) {
                self.incomplete_phis.insert(block, HashMap::new());
            }

            self.incomplete_phis
                .get_mut(&block)
                .unwrap()
                .insert(variable.clone(), val);
        } else if self.graph.predecessors(block).len() == 1 {
            let pred = self.graph.predecessors(block)[0];
            val = self.read_variable(variable.clone(), self.graph.block(pred));
        } else {
            let phi = Node::Phi;
            val = self.graph.add_node(block, phi);
            self.write_variable(variable.clone(), block, val);
            val = self.add_phi_operands(variable.clone(), val);
        }

        self.write_variable(variable, block, val);
        val
    }

    fn add_phi_operands(&mut self, variable: String, phi: NodeId) -> NodeId {
        let block = self.graph.block(phi);
        for pred in self.graph.predecessors(block) {
            let operand = self.read_variable(variable.clone(), self.graph.block(pred));

            let node = self.graph.node(operand).unwrap();
            if let Node::Undef = node {
                continue; // Dead
            }

            if !self.graph.predecessors(phi).contains(&operand) && operand != phi {
                self.graph.add_predecessor(phi, operand);
            }
        }

        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: NodeId) -> NodeId {
        let node = self.graph.node(phi).unwrap();

        if !matches!(node, Node::Phi) {
            return phi;
        }

        let preds = self.graph.predecessors(phi);

        if preds.len() >= 2 {
            return phi;
        }

        if preds.len() == 0 {
            self.graph.change_node(phi, Node::Undef);
            return phi; // Dead
        }

        // Only one operand
        let operand = preds[0];
        let phi_users = self.graph.successors(phi);
        self.reroute(phi, operand);
        self.graph.remove_node(phi);

        // Remove recursive
        for user in phi_users {
            let node = self.graph.node(user).unwrap();
            if let Node::Phi = node {
                self.try_remove_trivial_phi(user);
            }
        }

        operand
    }

    fn reroute(&mut self, old: NodeId, new: NodeId) {
        self.graph.reroute(old, new);

        for inner in self.current_def.values_mut() {
            for value in inner.values_mut() {
                if *value == old {
                    *value = new;
                }
            }
        }
    }

    fn seal_block(&mut self, block: NodeId) {
        if self.sealed_blocks.contains(&block) {
            return;
        }

        let map = self
            .incomplete_phis
            .get(&block)
            .cloned()
            .unwrap_or(HashMap::new());

        for (key, value) in map.iter() {
            self.add_phi_operands(key.clone(), *value);
        }

        // TODO SideEffect Phis

        self.sealed_blocks.push(block);
    }

    fn new_constant(graph: &mut IrGraph, constant: ConstantNode) -> NodeId {
        graph.add_to_start(Node::Constant(constant))
    }

    fn append_side_effect(graph: &mut IrGraph, id: NodeId, effect: Option<NodeId>) {
        if let Some(effect) = effect {
            graph.add_predecessor(id, effect);
        }
    }

    fn new_return(
        graph: &mut IrGraph,
        block: NodeId,
        expr: NodeId,
        side_effect: Option<NodeId>,
    ) -> NodeId {
        let id = graph.add_node(block, Node::Return);
        graph.add_predecessor(id, expr);
        graph.add_predecessor(graph.end, id);

        if let Some(side_effect) = side_effect {
            graph.add_predecessor(id, side_effect);
        }

        graph.set_block_exit(block, id);

        id
    }

    fn new_binary(
        graph: &mut IrGraph,
        block: NodeId,
        op: BinaryNodeOp,
        lhs: NodeId,
        rhs: NodeId,
    ) -> NodeId {
        let id = graph.add_node(block, Node::Binary(op));
        graph.add_predecessor(id, lhs);
        graph.add_predecessor(id, rhs);

        id
    }

    fn new_ternary(
        graph: &mut IrGraph,
        block: NodeId,
        cond: NodeId,
        then: NodeId,
        otherwise: NodeId,
    ) -> NodeId {
        let id = graph.add_node(block, Node::Ternary);
        graph.add_predecessor(id, cond);
        graph.add_predecessor(id, then);
        graph.add_predecessor(id, otherwise);

        id
    }

    fn new_jmp(
        graph: &mut IrGraph,
        block: NodeId,
        target: NodeId,
        side_effect: Option<NodeId>,
    ) -> NodeId {
        let id = graph.add_node(block, Node::Jump);
        graph.add_successor(id, target);

        if let Some(side_effect) = side_effect {
            graph.add_predecessor(id, side_effect);
        }

        graph.set_block_exit(block, id);

        id
    }

    fn new_cond_jmp(
        graph: &mut IrGraph,
        block: NodeId,
        cond: NodeId,
        then: NodeId,
        otherwise: NodeId,
        side_effect: Option<NodeId>,
    ) -> NodeId {
        let id = graph.add_node(block, Node::CondJump);
        graph.add_predecessor(id, cond);
        graph.add_successor(id, then);
        graph.add_successor(id, otherwise);

        if let Some(side_effect) = side_effect {
            graph.add_predecessor(id, side_effect);
        }

        graph.set_block_exit(block, id);

        id
    }
}
