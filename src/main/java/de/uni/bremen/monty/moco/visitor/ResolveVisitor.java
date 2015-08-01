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

import de.uni.bremen.monty.moco.ast.*;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.exception.InvalidExpressionException;
import de.uni.bremen.monty.moco.exception.UnknownIdentifierException;
import de.uni.bremen.monty.moco.exception.UnknownTypeException;

import java.util.ArrayList;
import java.util.List;

/** This visitor must traverse the entire AST and resolve variables and types. */
public class ResolveVisitor extends VisitOnceVisitor {

	@Override
	protected int bit() {
		return 1;
	}

	/** Constructor. */
	public ResolveVisitor() {
		super();
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ClassDeclaration node) {
		ClassScope scope = (ClassScope) node.getScope();
		List<TypeDeclaration> superClasses = node.getSuperClassDeclarations();
		for (ResolvableIdentifier identifier : node.getSuperClassIdentifiers()) {
			TypeDeclaration type = scope.resolveType(identifier);
			superClasses.add(type);
			if (type instanceof ClassDeclaration) {
				scope.addParentClassScope((ClassScope) type.getScope());
			}
		}
		super.visit(node);

		setVMT(node, superClasses);
	}

	private void setVMT(ClassDeclaration node, List<TypeDeclaration> superClasses) {
		int attributeIndex = 1;
		List<ProcedureDeclaration> virtualMethodTable = node.getVirtualMethodTable();
		// This can only deal with single inheritance!
		if (!superClasses.isEmpty()) {
			TypeDeclaration type = superClasses.get(0);
			if (type instanceof ClassDeclaration) {
				ClassDeclaration clazz = (ClassDeclaration) type;
				attributeIndex = clazz.getLastAttributeIndex();
				virtualMethodTable.addAll(clazz.getVirtualMethodTable());
			}
		}

		// Make room for the ctable pointer
		int vmtIndex = virtualMethodTable.size() + 1;

		for (Declaration decl : node.getBlock().getDeclarations()) {
			if (decl instanceof VariableDeclaration) {
				VariableDeclaration varDecl = (VariableDeclaration) decl;
				varDecl.setAttributeIndex(attributeIndex++);
			} else if (decl instanceof ProcedureDeclaration) {
				ProcedureDeclaration procDecl = (ProcedureDeclaration) decl;
				if (!procDecl.isInitializer()) {
					boolean foundEntry = false;
					for (int i = 0; !foundEntry && i < virtualMethodTable.size(); i++) {
						ProcedureDeclaration vmtEntry = virtualMethodTable.get(i);
						if (procDecl.matchesType(vmtEntry)) {
							virtualMethodTable.set(i, procDecl);
							procDecl.setVMTIndex(vmtEntry.getVMTIndex());
							foundEntry = true;
						}
					}
					if (!foundEntry) {
						virtualMethodTable.add(procDecl);
						procDecl.setVMTIndex(vmtIndex++);
					}
				}
			}
		}
		node.setLastAttributeIndex(attributeIndex);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableDeclaration node) {
		super.visit(node);
		Scope scope = node.getScope();
		TypeDeclaration type = scope.resolveType(node.getTypeIdentifier());
		node.setType(type);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableAccess node) {
		super.visit(node);

		Scope scope = node.getScope();
		Declaration declaration = scope.resolve(node.getIdentifier());

		if (declaration instanceof VariableDeclaration) {
			VariableDeclaration variable = (VariableDeclaration) declaration;
			node.setDeclaration(variable);
			visitDoubleDispatched(variable);
			node.setType(variable.getType());
			if (!(scope instanceof ClassScope) && findEnclosingClass(node) == CoreClasses.voidType()) {
				if (node.getDeclaration() == null
				        || node.getDeclaration().getPosition().getLineNumber() > node.getPosition().getLineNumber()) {
					throw new UnknownIdentifierException(node.getIdentifier());
				}
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(SelfExpression node) {
		super.visit(node);
		node.setType(findEnclosingClass(node));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ParentExpression node) {
		super.visit(node);
		node.setType(node.getScope().resolveType(node.getParentIdentifier()));
		node.setSelfType(findEnclosingClass(node));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CastExpression node) {
		super.visit(node);
		node.setType(node.getScope().resolveType(node.getCastIdentifier()));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IsExpression node) {
		super.visit(node);
		node.setToType(node.getScope().resolveType(node.getIsIdentifier()));
		node.setType(CoreClasses.boolType());
	}

	/** {@inheritDoc} */
	@Override
	public void visit(MemberAccess node) {
		visitDoubleDispatched(node.getLeft());
		TypeDeclaration leftType = node.getLeft().getType();

		if (leftType instanceof ClassDeclaration) {
			node.getRight().setScope(leftType.getScope());
		}
		visitDoubleDispatched(node.getRight());

		TypeDeclaration type = node.getRight().getType();
		node.setType(type);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ConditionalExpression node) {
		super.visit(node);
		node.setType(node.getThenExpression().getType());
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ZeroExpression node) {
		Scope scope = node.getScope();
		node.setType(scope.resolveType(node.getIdentifier()));
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IntegerLiteral node) {
		node.setType(CoreClasses.intType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FloatLiteral node) {
		node.setType(CoreClasses.floatType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CharacterLiteral node) {
		node.setType(CoreClasses.charType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(StringLiteral node) {
		node.setType(CoreClasses.stringType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BooleanLiteral node) {
		node.setType(CoreClasses.boolType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ArrayLiteral node) {
		node.setType(CoreClasses.arrayType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ProcedureDeclaration node) {
		if (node.isFunction()) {
			Scope scope = node.getScope();
			TypeDeclaration returnType = scope.resolveType(node.getReturnTypeIdentifier());
			node.setReturnType(returnType);
		} else {
			if (node.isMethod() && node.getIdentifier().getSymbol().equals("initializer")) {
				node.setDeclarationType(ProcedureDeclaration.DeclarationType.INITIALIZER);
			}
		}
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionCall node) {
		super.visit(node);

		if (node instanceof TupleLiteral) {
			((TupleLiteral) node).setTypeAutomatically();
		}

		Scope scope = node.getScope();
		Declaration declaration = null;

		declaration = scope.tryToResolveType(node.getIdentifier());

		if (declaration instanceof ClassDeclaration) {
			ClassDeclaration classDecl = (ClassDeclaration) declaration;
			ResolvableIdentifier identifier = node.getIdentifier();
			if (classDecl.isAbstract()) {
				throw new InvalidExpressionException(node, "The abstract class '" + identifier.toString()
				        + "' may not be instantiated");
			}
			node.setType(classDecl);
			ProcedureDeclaration initializer = findMatchingInitializer(node, classDecl);
			initializer = (initializer != null) ? initializer : classDecl.getDefaultInitializer();
			node.setDeclaration(initializer);
		} else {
			ProcedureDeclaration procedure = findMatchingProcedure(node, scope.resolveProcedure(node.getIdentifier()));
			node.setDeclaration(procedure);
			if (procedure.isFunction()) {
				visitDoubleDispatched(procedure);

				node.setType(procedure.getReturnType());
			} else {
				node.setType(CoreClasses.voidType());
			}
		}
	}

	/** Find an enclosing class of this node.
	 *
	 * If the search is not sucessfull this method returns CoreClasses.voidType(). */
	private ClassDeclaration findEnclosingClass(ASTNode node) {
		ASTNode parent = node.getParentNode();
		while (parent != null) {
			if (parent instanceof ClassDeclaration) {
				return (ClassDeclaration) parent;
			}
			parent = parent.getParentNode();
		}
		return CoreClasses.voidType();
	}

	/** Searches the given class declaration in order to find a initializer declaration that matches the signature of the
	 * given initializer node.
	 *
	 * @param node
	 *            a function call node representing a initializer
	 * @param classDeclaration
	 *            the class declaration searched for a matching initializer.
	 * @return the matching initializer if one is found for the given function call or null otherwise */
	private ProcedureDeclaration findMatchingInitializer(FunctionCall node, ClassDeclaration classDeclaration) {

		List<ProcedureDeclaration> procedures = new ArrayList<>();

		// iterate through the declarations of the given class
		for (Declaration declaration : classDeclaration.getBlock().getDeclarations()) {
			// find a matching declaration
			if ("initializer".equals(declaration.getIdentifier().getSymbol())) {
				// and verify that it is a procedure...
				if (declaration instanceof ProcedureDeclaration) {
					// and not a function
					if (!(((ProcedureDeclaration) declaration).isFunction())) {
						procedures.add((ProcedureDeclaration) declaration);
					}
				}
			}
		}

		if (procedures.isEmpty()) {
			return null;
		} else {
			return findMatchingProcedure(node, procedures);
		}
	}

	/** Searches the given list of procedures in order to find one that matches the signature of the given function call.
	 *
	 * @param node
	 *            a function call node representing the function call
	 * @param procedures
	 *            the possible procedure declarations for this call
	 * @return the matching declaration if one is found for the given function call or the first in the list otherwise */
	private ProcedureDeclaration findMatchingProcedure(FunctionCall node, List<ProcedureDeclaration> procedures) {

		List<Expression> callParams = node.getArguments();
		for (ProcedureDeclaration procedure : procedures) {
			List<VariableDeclaration> procParams = procedure.getParameter();

			if (callParams.size() == procParams.size()) {
				boolean allParamsMatch = true;
				for (int i = 0; i < callParams.size(); i++) {
					Expression callParam = callParams.get(i);
					VariableDeclaration procParam = procParams.get(i);
					visit(procParam);
					if (!callParam.getType().matchesType(procParam.getType())) {
						allParamsMatch = false;
						break;
					}
				}
				if (allParamsMatch) {
					return procedure;
				}
			}
		}
		return procedures.get(0);
	}
}
