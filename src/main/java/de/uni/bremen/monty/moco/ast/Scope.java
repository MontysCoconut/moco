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

import java.util.*;

import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.exception.*;

/** A scope in which an identifier is associated with a declaration.
 * <p>
 * To nest scopes or build a stack the parent scope is passed as an argument to the constructor. So you use it like
 * this:
 * <p>
 * 
 * <pre>
 * {@code
 * // create a new scope and nest the old one
 * currentScope = new Scope(currentScope);
 * // do something
 * // destroy this scope and use the old (nested) one
 * currentScope = currentScope.getParentScope();
 * }
 * </pre> */
public class Scope {

	/** The parent scope in nesting hierarchy. */
	protected Scope parent;

	/** The map to store the associations to procedure declarations. */
	protected Map<Identifier, List<ProcedureDeclaration>> procedures;

	/** The map to store the remaining associations. */
	protected Map<Identifier, Declaration> members;

	/** Constructor.
	 * 
	 * @param parent
	 *            the parent scope in nesting hierarchy */
	public Scope(Scope parent) {
		this.parent = parent;
		procedures = new HashMap<Identifier, List<ProcedureDeclaration>>();
		members = new HashMap<Identifier, Declaration>();
	}

	/** Get the parent scope in nesting hierarchy.
	 * <p>
	 * This method acts as the 'pop()'-operation in the scope-stack analogy.
	 * 
	 * @return the parent scope */
	public Scope getParentScope() {
		return parent;
	}

	/** Resolve an identifier for a declaration.
	 * <p>
	 * First the declarations of this scope are searched. If the not successful the search continues recursively in the
	 * parent scope.
	 * 
	 * @param identifier
	 *            the identifier to resolve
	 * @return the declaration */
	public Declaration resolve(ResolvableIdentifier identifier) {
		Declaration declaration = members.get(identifier);

		if (declaration != null) {
			return declaration;
		}
		if (parent != null) {
			return parent.resolve(identifier);
		}
		throw new UnknownIdentifierException(identifier);
	}

	/** Resolve an identifier for a type declaration.
	 * 
	 * @param identifier
	 *            the identifier to resolve
	 * @return the declaration */
	public TypeDeclaration resolveType(ResolvableIdentifier identifier) {
		try {
			Declaration declaration = resolve(identifier);
			if (declaration instanceof TypeDeclaration) {
				return (TypeDeclaration) declaration;
			}
			throw new UnknownTypeException(identifier);
		} catch (UnknownIdentifierException e) {
			throw new UnknownTypeException(identifier);
		}
	}

	/** Resolve an identifier for list of overloaded procedures or functions.
	 * 
	 * @param identifier
	 *            the identifier to resolve
	 * @return the list of procedure declarations */
	public List<ProcedureDeclaration> resolveProcedure(ResolvableIdentifier identifier) {
		List<ProcedureDeclaration> result = new ArrayList<ProcedureDeclaration>();

		if (procedures.containsKey(identifier)) {
			result.addAll(procedures.get(identifier));
		}
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

	/** Associate an identifier with a declaration.
	 * 
	 * This method uses define(Identifier, ProcedureDeclaration) if the given declaration is a procedure or function
	 * declaration.
	 * 
	 * @param identifier
	 *            the identifier
	 * @param declaration
	 *            the declaration
	 * @throws RedeclarationException
	 *             if the identifier is already defined or this is invalid overloading */
	public void define(Identifier identifier, Declaration declaration) throws RedeclarationException {
		if (declaration instanceof ProcedureDeclaration) {
			define(identifier, (ProcedureDeclaration) declaration);
		} else if (members.get(identifier) != null) {
			throw new RedeclarationException(declaration, identifier.getSymbol());
		} else {
			members.put(identifier, declaration);
		}
	}

	/** Associate an identifier with a declaration.
	 * <p>
	 * This differs from define(Identifier, Declaration) as this method uses the declaration's Identifier-attribute to
	 * call define(Identifier, Declaration)
	 * 
	 * @param declaration
	 *            the declaration
	 * @throws RedeclarationException
	 *             if the identifier is already defined or this is invalid overloading */
	public void define(Declaration declaration) throws RedeclarationException {
		define(declaration.getIdentifier(), declaration);
	}

	/** Associate an identifier with a procedure or function declaration.
	 * 
	 * This takes overloading into account and throws a RedeclarationException if the declaration is an instance of
	 * invalid overloading.
	 * 
	 * @param identifier
	 *            the identifier
	 * @param declaration
	 *            the declaration
	 * @throws RedeclarationException
	 *             if this is invalid overloading */
	public void define(Identifier identifier, ProcedureDeclaration declaration) throws RedeclarationException {
		if (!procedures.containsKey(identifier)) {
			procedures.put(identifier, new ArrayList<ProcedureDeclaration>());
		}
		procedures.get(identifier).add(declaration);
	}
}
