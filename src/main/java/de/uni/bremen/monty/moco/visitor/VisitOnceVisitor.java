/*
 * moco, the Monty Compiler
 * Copyright (c) 2013-2014, Monty's Coconut, All rights reserved.
 *
 * This file is part of moco, the Monty Compiler.
 *
 * moco is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * moco is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * Linking this program and/or its accompanying libraries statically or
 * dynamically with other modules is making a combined work based on this
 * program. Thus, the terms and conditions of the GNU General Public License
 * cover the whole combination.
 *
 * As a special exception, the copyright holders of moco give
 * you permission to link this programm and/or its accompanying libraries
 * with independent modules to produce an executable, regardless of the
 * license terms of these independent modules, and to copy and distribute the
 * resulting executable under terms of your choice, provided that you also meet,
 * for each linked independent module, the terms and conditions of the
 * license of that module.
 *
 * An independent module is a module which is not
 * derived from or based on this program and/or its accompanying libraries.
 * If you modify this library, you may extend this exception to your version of
 * the program or library, but you are not obliged to do so. If you do not wish
 * to do so, delete this exception statement from your version.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library.
 */

package de.uni.bremen.monty.moco.visitor;

import de.uni.bremen.monty.moco.ast.ASTNode;
import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.Import;
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.*;

import java.util.HashMap;
import java.util.Map;

public class VisitOnceVisitor extends BaseVisitor {

	private Map<ASTNode, Boolean> visited = new HashMap<>();

	@Override
	public void visit(ModuleDeclaration node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ArrayLiteral node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(Package node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ClassDeclaration node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(FunctionDeclaration node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ProcedureDeclaration node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(VariableDeclaration node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ConditionalExpression node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(SelfExpression node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(CastExpression node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(IsExpression node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	public void visit(ParentExpression node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(FunctionCall node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(MemberAccess node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(VariableAccess node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(BooleanLiteral node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(FloatLiteral node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(IntegerLiteral node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(StringLiteral node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(Assignment node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(BreakStatement node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(SkipStatement node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ConditionalStatement node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ContinueStatement node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(ReturnStatement node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(WhileLoop node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(Block node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	@Override
	public void visit(Import node) {
		if (shouldVisit(node)) {
			return;
		}
		super.visit(node);
		this.visited.put(node, true);
	}

	private boolean shouldVisit(ASTNode node) {
		Boolean visited1 = this.visited.get(node);
		if (visited1 != null) {
			if (!visited1) {
				throw new RuntimeException("Cyclic dependency detected.");
			}
			return true;
		}
		this.visited.put(node, false);
		return false;
	}

}
