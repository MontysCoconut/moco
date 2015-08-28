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

import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.Declaration;
import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.TypeDeclaration;
import de.uni.bremen.monty.moco.exception.RedeclarationException;
import de.uni.bremen.monty.moco.exception.UnknownIdentifierException;
import de.uni.bremen.monty.moco.exception.UnknownTypeException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

	/** The map to store the associations to function declarations. */
	protected Map<Identifier, List<FunctionDeclaration>> functions;

	/** The map to store the remaining associations. */
	protected Map<Identifier, Declaration> members;

	/** Constructor.
	 *
	 * @param parent
	 *            the parent scope in nesting hierarchy */
	public Scope(Scope parent) {
		this.parent = parent;
		functions = new HashMap<Identifier, List<FunctionDeclaration>>();
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
		// FIXME: das hier berücksichtigt noch nicht die richtige Reihenfolge...
		if (functions.containsKey(identifier)) {
			List<FunctionDeclaration> functionDeclarations = functions.get(identifier);
			if (functionDeclarations.size() == 1) {
				return functionDeclarations.get(0).getWrapperFunctionObjectDeclaration();
			}
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
				return resolveGenericClass((TypeDeclaration) declaration, identifier);
			}
			throw new UnknownTypeException(identifier);
		} catch (UnknownIdentifierException e) {
			throw new UnknownTypeException(identifier);
		}
	}

	/** Tries to resolve an identifier for a type declaration.
	 *
	 * @param identifier
	 *            the identifier to resolve
	 * @return the declaration or null */
	public TypeDeclaration tryToResolveType(ResolvableIdentifier identifier) {
		try {
			return resolveType(identifier);
		} catch (UnknownTypeException e) {
			return null;
		}
	}

	/** Resolve an identifier for list of overloaded functions.
	 *
	 * @param identifier
	 *            the identifier to resolve
	 * @return the list of function declarations */
	public List<FunctionDeclaration> resolveFunction(ResolvableIdentifier identifier) {
		List<FunctionDeclaration> result = new ArrayList<FunctionDeclaration>();

		if (functions.containsKey(identifier)) {
			result.addAll(functions.get(identifier));
		}
		if (parent != null) {
			try {
				result.addAll(parent.resolveFunction(identifier));
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
	 * This method uses define(Identifier, FunctionDeclaration) if the given declaration is a function declaration.
	 *
	 * @param identifier
	 *            the identifier
	 * @param declaration
	 *            the declaration
	 * @throws RedeclarationException
	 *             if the identifier is already defined or this is invalid overloading */
	public void define(Identifier identifier, Declaration declaration) throws RedeclarationException {
		if (declaration instanceof FunctionDeclaration) {
			define(identifier, (FunctionDeclaration) declaration);
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

	/** Associate an identifier with a function declaration.
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
	public void define(Identifier identifier, FunctionDeclaration declaration) throws RedeclarationException {
		if (!functions.containsKey(identifier)) {
			functions.put(identifier, new ArrayList<FunctionDeclaration>());
		}
		functions.get(identifier).add(declaration);
	}

	private TypeDeclaration resolveGenericClass(TypeDeclaration originalType, ResolvableIdentifier genericIdentifier) {
		List<ResolvableIdentifier> genericTypes = genericIdentifier.getGenericTypes();
		if (!genericTypes.isEmpty() && originalType instanceof ClassDeclaration) {
			ClassDeclaration originalClass = (ClassDeclaration) originalType;
			ArrayList<ClassDeclaration> concreteGenerics = new ArrayList<>();
			for (ResolvableIdentifier genericType : genericTypes) {
				TypeDeclaration decl = resolveType(genericType);
				decl = resolveGenericClass(decl, genericType);
				concreteGenerics.add((ClassDeclaration) decl);
			}
			return originalClass.getVariation(genericIdentifier, concreteGenerics);
		}
		return originalType;
	}
}
