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
package de.uni.bremen.monty.moco.ast;

import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.exception.*;
import java.util.List;
import java.util.ArrayList;

/** A scope in which identifier are associated with declarations.
 * <p>
 * To nest scopes or build a stack the parent scope is passed as an argument to the construtor. So you use it like this
 * <p>
 * 
 * <pre>
 * {@code
 * // create a new scope and nest the old one
 * currentScope = new ClassScope(currentScope);
 * // do something
 * // destroy this scope and use the old (nested) one
 * currentScope = currentScope.getParentScope();
 * }
 * </pre>
 * <p>
 * This special scope searches its associations, the parent classes in inheritance hierachy and only then the parent
 * scope.
 * <p>
 * Note: only single inheritance so far. */
public class ClassScope extends Scope {

	/** The parent class in inheritance hierachy. */
	private List<ClassScope> parentClassesScopes;

	/** Constructor.
	 * 
	 * @param parent
	 *            the parent scope in nesting hierachy */
	public ClassScope(Scope parent) {
		super(parent);
		this.parentClassesScopes = new ArrayList<>();
	}

	public void addParentClassScope(ClassScope scope) {
		parentClassesScopes.add(scope);
	}

	/** Resolve an identifier in inherited scopes.
	 * 
	 * @param identifier
	 *            the identifier
	 * @return the declaration or null if nothing is found */
	protected Declaration resolveMember(ResolvableIdentifier identifier) {
		Declaration declaration = members.get(identifier);

		if (declaration != null) {
			return declaration;
		}
		for (ClassScope scope : parentClassesScopes) {
			declaration = scope.resolveMember(identifier);
			if (declaration != null) {
				return declaration;
			}
		}
		return null;
	}

	/** Resolve an identifier for list of overloaded procedures or functions in inherited scope.
	 * 
	 * @param identifier
	 *            the identifier to resolve
	 * @return the list of procedure declarations */
	protected List<ProcedureDeclaration> resolveProcedureMember(ResolvableIdentifier identifier) {
		List<ProcedureDeclaration> result = new ArrayList<ProcedureDeclaration>();

		if (procedures.containsKey(identifier)) {
			result.addAll(procedures.get(identifier));
		}
		for (ClassScope scope : parentClassesScopes) {
			result.addAll(scope.resolveProcedureMember(identifier));
		}
		return result;
	}

	/** Resolve an identifier for a declaration
	 * <p>
	 * It first searches its associations, the parent classes in inheritance hierachy and only then the parent scope.
	 * 
	 * @param identifier
	 *            the identifier to resolve
	 * @return the declaration or null if nothing is found */
	@Override
	public Declaration resolve(ResolvableIdentifier identifier) {
		Declaration declaration = resolveMember(identifier);

		if (declaration != null) {
			return declaration;
		}
		return super.resolve(identifier);
	}

	/** Resolve an identifier for list of overloaded procedures or functions.
	 * <p>
	 * It first searches its associations, the parent classes in inheritance hierachy and only then the parent scope.
	 * 
	 * @param identifier
	 *            the identifier to resolve
	 * @return the list of procedure declarations */
	@Override
	public List<ProcedureDeclaration> resolveProcedure(ResolvableIdentifier identifier) {
		List<ProcedureDeclaration> result = new ArrayList<ProcedureDeclaration>();
		result.addAll(resolveProcedureMember(identifier));
		if (parent != null) {
			try {
				result.addAll(parent.resolveProcedure(identifier));
			} catch (UnknownIdentifierException e) {
			}
		}
		if (result.isEmpty()) {
			throw new UnknownIdentifierException(identifier);
		}
		return result;
	}
}
