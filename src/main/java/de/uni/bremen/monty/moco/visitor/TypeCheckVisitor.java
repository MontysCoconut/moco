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
import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.ArrayLiteral;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ConditionalStatement;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.exception.InvalidExpressionException;
import de.uni.bremen.monty.moco.exception.InvalidPlaceToDeclareException;
import de.uni.bremen.monty.moco.exception.TypeMismatchException;

import java.util.List;

/** This visitor must traverse the entire AST and perform type-safety checks.
 *
 * This visitor does neither resolve nor set a type. It just checks. */
public class TypeCheckVisitor extends BaseVisitor {

	/** {@inheritDoc} */
	@Override
	public void visit(ClassDeclaration node) {
		// go through all direct parent classes (indirect classes are not considered, though)
		for (TypeDeclaration type : node.getSuperClassDeclarations()) {
			if (!(type instanceof ClassDeclaration)) {
				throw new TypeMismatchException(node, String.format("Declaration of superclass is not a class."));
			}
		}

		if (!node.isAbstract()) {
			// go through all methods and ensure that there remains no abstract one
			// (i.e. all abstract ones are overridden)
			for (FunctionDeclaration procDecl : node.getVirtualMethodTable()) {
				if (procDecl.isAbstract()) {
					ASTNode parent = procDecl;
					while (!(parent instanceof ClassDeclaration)) {
						parent = parent.getParentNode();
					}
					// if the method is defined directly in the class
					if (parent == node) {
						throw new InvalidPlaceToDeclareException(node, "The non-abstract class '"
						        + node.getIdentifier().toString() + "' contains an abstract method '"
						        + procDecl.getIdentifier().toString() + "'!");
					}
					// if the method is inherited from a super class
					else {
						throw new InvalidPlaceToDeclareException(node, "The non-abstract class '"
						        + node.getIdentifier().toString() + "' inherits an abstract method '"
						        + procDecl.getIdentifier().toString() + "', which has not been implemented!");
					}
				}
			}
		}
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableAccess node) {
		super.visit(node);
		Declaration declaration = node.getDeclaration();
		if (!(declaration instanceof VariableDeclaration) && !(declaration instanceof TypeDeclaration)) {
			throw new TypeMismatchException(node, String.format("%s does not resolve to a type.", node.getIdentifier()));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(SelfExpression node) {
		super.visit(node);
		if (node.getType() == CoreClasses.voidType()) {
			throw new TypeMismatchException(node, "No enclosing class found.");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ParentExpression node) {
		super.visit(node);
		if (node.getSelfType() == CoreClasses.voidType()) {
			throw new TypeMismatchException(node, "No enclosing class found.");
		}
		if (!node.getSelfType().getSuperClassDeclarations().contains(node.getType())) {
			throw new TypeMismatchException(node, "Class not a direct parent class");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CastExpression node) {
		super.visit(node);
		TypeDeclaration expressionType = node.getExpression().getType();
		if (!(expressionType instanceof ClassDeclaration)) {
			throw new TypeMismatchException(node, "It is not possible to cast something different than a class.");
		}
		if (!(node.getType().matchesType(expressionType) || expressionType.matchesType(node.getType()))) {
			throw new TypeMismatchException(node, "Impossible cast");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IsExpression node) {
		super.visit(node);
		if (!(node.getExpression().getType() instanceof ClassDeclaration)) {
			throw new TypeMismatchException(node,
			        "It is not possible to check something different than an instance of a class.");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(MemberAccess node) {
		super.visit(node);
		if (!(node.getLeft().getType() instanceof ClassDeclaration)) {
			throw new TypeMismatchException(node, "Left part is not an instance of a class declaration.");
		} else if (node.getLeft() instanceof VariableAccess) {
			VariableAccess varAcc = (VariableAccess) node.getLeft();
			if (varAcc.getDeclaration() instanceof ClassDeclaration) {
				throw new TypeMismatchException(node, String.format(
				        "Invalid left part %s on member access",
				        node.getLeft().getClass().getSimpleName()));
			}
		} else if (!(node.getRight() instanceof FunctionCall) && !(node.getRight() instanceof VariableAccess)
		        && !(node.getRight() instanceof WrappedFunctionCall) && !(node.getRight() instanceof MemberAccess)) {
			throw new TypeMismatchException(node, String.format(
			        "Invalid right part %s on member access.",
			        node.getLeft().getClass().getSimpleName()));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(Assignment node) {
		super.visit(node);
		if (!node.getRight().getType().matchesType(node.getLeft().getType())) {
			throw new TypeMismatchException(node, String.format(
			        "%s does not match %s",
			        node.getRight().getType().getIdentifier().getSymbol(),
			        node.getLeft().getType().getIdentifier().getSymbol()));
		}
		if (node.getLeft() instanceof VariableAccess) {
			((VariableAccess) node.getLeft()).setLValue();
		} else if (node.getLeft() instanceof MemberAccess) {
			MemberAccess ma = (MemberAccess) node.getLeft();
			if (ma.getRight() instanceof VariableAccess) {
				((VariableAccess) ma.getRight()).setLValue();
			}
		} else {
			throw new InvalidExpressionException(node, "Left side is no variable");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ConditionalExpression node) {
		super.visit(node);
		if (!node.getThenExpression().getType().matchesType(node.getElseExpression().getType())) {
			throw new TypeMismatchException(node, String.format(
			        "%s does not match %s",
			        node.getThenExpression().getType().getIdentifier().getSymbol(),
			        node.getElseExpression().getType().getIdentifier().getSymbol()));
		} else if (!node.getCondition().getType().matchesType(CoreClasses.boolType())) {
			throw new TypeMismatchException(node, String.format(
			        "%s is not a bool type.",
			        node.getThenExpression().getType().getIdentifier().getSymbol()));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ArrayLiteral node) {
		super.visit(node);
		for (Expression entry : node.getEntries()) {
			if (!entry.getType().matchesType(CoreClasses.objectType())) {
				throw new TypeMismatchException(node, "Array entries must be Objects");
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ConditionalStatement node) {
		super.visit(node);
		if (!node.getCondition().getType().matchesType(CoreClasses.boolType())) {
			throw new TypeMismatchException(node,
			        String.format("%s is not a bool type.", node.getCondition().getType()));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionDeclaration node) {
		super.visit(node);
		if (node.isFunction()) {
			if (!(node.getReturnType() instanceof ClassDeclaration || node.getReturnType() instanceof AbstractGenericType)) {
				throw new TypeMismatchException(node, "Must return a class type.");
			}
		}
		if (node.isAbstract()) {
			ASTNode parent = node.getParentNode();
			while (!(parent instanceof ClassDeclaration)) {
				parent = parent.getParentNode();
			}
			if (!((ClassDeclaration) parent).isAbstract()) {
				throw new InvalidPlaceToDeclareException(node, "Abstract method '" + node.getIdentifier()
				        + "' declared in non-abstract class '" + ((ClassDeclaration) parent).getIdentifier() + "'!");
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionCall node) {
		super.visit(node);
		FunctionDeclaration function = node.getDeclaration();

		if (function.isFunction()) {
			if (function.isInitializer()) {
				throw new TypeMismatchException(node, "Initializer may not have a return type.");
			}
			if (!node.getType().matchesType(function.getReturnType())) {
				throw new TypeMismatchException(node, "Returntype of function call does not match declaration.");
			}
		} else {
			if (!function.isInitializer() && node.getType() != CoreClasses.voidType()) {
				throw new TypeMismatchException(node, "Procedures must not return anything.");
			}
		}

		boolean callMatchesDeclaration = callMatchesDeclaration(node, function);

		if (!callMatchesDeclaration) {
			throw new TypeMismatchException(node, "Arguments of function call do not match declaration.");
		}
	}

	private boolean callMatchesDeclaration(FunctionCall node, FunctionDeclaration function) {
		boolean callMatchesDeclaration = true;

		List<Expression> callParams = node.getArguments();
		List<VariableDeclaration> declParams = function.getParameters();
		if (callParams.size() == declParams.size()) {
			for (int i = 0; i < callParams.size(); i++) {
				Expression callParam = callParams.get(i);
				VariableDeclaration declParam = declParams.get(i);

				TypeDeclaration declType = declParam.getType();
				TypeDeclaration callParamType = callParam.getType();

				if (!callParamType.matchesType(declType)) {
					callMatchesDeclaration = false;
					break;
				}
			}
		} else {
			callMatchesDeclaration = false;
		}
		return callMatchesDeclaration;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ReturnStatement node) {
		super.visit(node);
		FunctionDeclaration proc = ((FunctionDeclaration) node.getParentNodeByType(FunctionDeclaration.class));
		if (proc.isFunction()) {
			if (!(node.getParameter().getType().matchesType(proc.getReturnType()))) {
				throw new TypeMismatchException(node, String.format(
				        "Expected to return %s:",
				        proc.getReturnType().getIdentifier()));
			}
		} else if (node.getParameter() != null) {
			throw new TypeMismatchException(node, "Expected to return void.");
		}
	}
}
