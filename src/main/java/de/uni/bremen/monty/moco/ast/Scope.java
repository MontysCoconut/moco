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
import de.uni.bremen.monty.moco.ast.types.*;
import de.uni.bremen.monty.moco.exception.InvalidExpressionException;
import de.uni.bremen.monty.moco.exception.RedeclarationException;
import de.uni.bremen.monty.moco.exception.UnknownIdentifierException;
import de.uni.bremen.monty.moco.exception.UnknownTypeException;
import de.uni.bremen.monty.moco.visitor.VisitOnceVisitor;

import java.util.*;
import java.util.stream.Collectors;

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
		return _resolve(identifier).orElseThrow(() -> new UnknownIdentifierException(identifier));
	}

	protected Optional<Declaration> _resolve(ResolvableIdentifier identifier) {
		Declaration declaration = members.get(identifier);

		if (declaration != null) {
			return Optional.of(declaration);
		}
		if (functions.containsKey(identifier)) {
			List<FunctionDeclaration> functionDeclarations = functions.get(identifier);
			if (functionDeclarations.size() == 1) {
				return Optional.ofNullable(functionDeclarations.get(0).getWrapperFunctionObjectDeclaration());
			} else if (functionDeclarations.size() > 1) {
				throw new InvalidExpressionException(null, "Accessing the identifier '" + identifier.getSymbol()
				        + "' without a cast is ambiguous, since there are " + functionDeclarations.size()
				        + " overloaded versions of this function.");
			}
		}
		if (parent != null) {
			return parent._resolve(identifier);
		}

		return Optional.empty();
	}

	/** Resolve an identifier for a type declaration.
	 *
	 * @param identifier
	 *            the identifier to resolve
	 * @return the declaration */
	public Type resolveType(ResolvableIdentifier identifier, VisitOnceVisitor visitor) {
		Optional<Type> maybeType = tryToResolveType(identifier, visitor);
		return maybeType.map(t -> t.extend(getContext())).orElseThrow(() -> new UnknownTypeException(identifier));
	}

	/** Tries to resolve an identifier for a type declaration.
	 *
	 * @param identifier
	 *            the identifier to resolve
	 * @return the declaration or null */
	public Optional<Type> tryToResolveType(ResolvableIdentifier identifier, VisitOnceVisitor visitor) {
		try {
			return _resolve(identifier).flatMap((declaration) -> {
                if (declaration instanceof TypeDeclaration) {
                    List<Type> resolvedGenericTypes = resolveGenericIdentifier(identifier, visitor);
                    visitor.visitDoubleDispatched(declaration);
                    if (declaration instanceof ClassDeclaration) {
                        TypeContext context = TypeContext.from((ClassDeclaration) declaration, resolvedGenericTypes);
                        return Optional.of(TypeFactory.from((ClassDeclaration) declaration, context));
                    }
					return Optional.of(new TypeVariable((TypeParameterDeclaration) declaration));
                }
                return Optional.empty();
            });
		} catch (InvalidExpressionException e) {
			return Optional.empty();
		}
	}

	/** Resolve an identifier for list of overloaded functions.
	 *
	 * @param identifier
	 *            the identifier to resolve
	 * @return the list of function declarations */
	public List<FunctionType> resolveFunction(ResolvableIdentifier identifier, VisitOnceVisitor visitor) {
		return _resolveFunction(identifier, visitor).orElseThrow(() -> new UnknownIdentifierException(identifier));
	}

	public Optional<List<FunctionType>> _resolveFunction(ResolvableIdentifier identifier, VisitOnceVisitor visitor) {
		List<FunctionType> result = new ArrayList<>();
		if (functions.containsKey(identifier)) {
			List<FunctionType> functionDeclarations = getResolvedFunctions(identifier, visitor);
			result.addAll(functionDeclarations);
		}
		if (members.containsKey(identifier)) {
			Declaration declaration = members.get(identifier);
			if(isFunctionVariable(declaration)) {
				result.add(TypeFactory.createFunction((VariableDeclaration) declaration));
			}
		}
		if (parent != null) {
			result.addAll(parent._resolveFunction(identifier, visitor).orElse(Collections.emptyList()));
		}
		if (result.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(result);
	}

	public boolean isFunctionVariable(Declaration declaration) {
		return declaration instanceof VariableDeclaration && ((VariableDeclaration) declaration).getType().isAssignableFrom(Types.functionType());
	}

	protected List<FunctionType> getResolvedFunctions(ResolvableIdentifier identifier, VisitOnceVisitor visitor) {
		return functions.get(identifier).stream().map((declaration) -> {
			visitor.visitDoubleDispatched(declaration);
			return TypeFactory.from(declaration, TypeContext.EMPTY);
		}).collect(Collectors.toList());
	}

	private List<Type> resolveGenericIdentifier(ResolvableIdentifier identifier, VisitOnceVisitor visitor) {
		ArrayList<Type> genericIdentifier = new ArrayList<>();
		for (ResolvableIdentifier resolvableIdentifier : identifier.getGenericTypes()) {
			Type typeInfo = resolveType(resolvableIdentifier,visitor);
			genericIdentifier.add(typeInfo);
		}
		return genericIdentifier;
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
		} else if ((members.get(identifier) != null)
		        || ((functions.get(identifier) != null) && (!functions.get(identifier).isEmpty()))) {
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
	private void define(Identifier identifier, FunctionDeclaration declaration) throws RedeclarationException {
		if (!functions.containsKey(identifier)) {
			functions.put(identifier, new ArrayList<FunctionDeclaration>());
		}
		functions.get(identifier).add(declaration);
	}

	public TypeContext getContext() {
		return TypeContext.EMPTY;
	}
}
