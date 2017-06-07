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
package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.ast.*;
import de.uni.bremen.monty.moco.ast.types.PartialAppliedTypeInfo;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/** A ClassDeclaration represents the declaration of a class in the AST.
 * <p>
 * A ClassDeclaration has a list of superclasses and a list of nested declarations. It can be used as a type. */
public class ClassDeclaration extends TypeDeclaration {

	private final List<TypeParameterDeclaration> typeParameterDeclarations;

	/** Identifier of superclasses. */
	private final List<ResolvableIdentifier> superClassIdentifiers;

	/** Superclasses. */
	private final List<PartialAppliedTypeInfo> superClassDeclarations = new ArrayList<>();

	private final boolean dummy;

	/** The generated default initializer to be called from every user defined initializer. */
	private FunctionDeclaration defaultInitializer;

	/** Block with assignments **/
	private Block block;

	/** The virtal method table for this class */
	private List<FunctionDeclaration> virtualMethodTable = new ArrayList<>();

	/** this attribute is set if the class is a function wrapper */
	private FunctionDeclaration wrappedFunction = null;

	/** The last index for the attributes of this class. This counter starts at `1` as index 0 is reserved for a pointer
	 * to the vmt. */
	private int lastAttributeIndex = -1;

	private boolean abstractClass = false;

	private boolean _isGenerator = false;

	/** Constructor.
	 *
	 * @param position
	 *            Position of this node
	 * @param identifier
	 *            the identifier
	 * @param superClasses
	 *            a list of direct super-classes
	 * @param block
	 *            the block */
	public ClassDeclaration(Position position, Identifier identifier, List<ResolvableIdentifier> superClasses,
	        Block block) {
		super(position, identifier);
		this.block = block;
		this.superClassIdentifiers = superClasses;
		this.typeParameterDeclarations = Collections.emptyList();
		this.dummy = false;
	}

	public ClassDeclaration(Position position, Identifier identifier, List<ResolvableIdentifier> superClasses,
							Block block, boolean dummy) {
		super(position, identifier);
		this.block = block;
		this.superClassIdentifiers = superClasses;
		this.typeParameterDeclarations = Collections.emptyList();
		this.dummy = dummy;
	}

	public ClassDeclaration(Position position, Identifier identifier, List<ResolvableIdentifier> superClassIdentifiers,
	        Block block, boolean isAbstract, List<TypeParameterDeclaration> typeParameterDeclarations) {
		super(position, identifier);
		this.superClassIdentifiers = superClassIdentifiers;
		this.block = block;
		this.typeParameterDeclarations = typeParameterDeclarations;
		this.abstractClass = isAbstract;
		this.dummy = false;
	}

	/** Get the list of declarations and assignments.
	 *
	 *
	 * @return the block with declarations and assignments */
	public Block getBlock() {
		return block;
	}

	/** @return true if the class is abstract */
	public boolean isAbstract() {
		return this.abstractClass;
	}

	/** Get the list of identifiers of direct superclasses
	 *
	 * @return the identifier of superclasses */
	public List<ResolvableIdentifier> getSuperClassIdentifiers() {
		return superClassIdentifiers;
	}

	/** Get the list of direct superclasses this class inherits from.
	 *
	 * @return the superclasses */
	public List<PartialAppliedTypeInfo> getSuperClassDeclarations() {
		return superClassDeclarations;
	}

	/** set the last attribute index.
	 *
	 * @param lastAttributeIndex
	 *            the last attribute index */
	public void setLastAttributeIndex(int lastAttributeIndex) {
		this.lastAttributeIndex = lastAttributeIndex;
	}

	/** get the last attribute index
	 *
	 * @return the last attribute index */
	public int getLastAttributeIndex() {
		return lastAttributeIndex;
	}

	/** Get the VMT.
	 *
	 * @return the VMT */
	public List<FunctionDeclaration> getVirtualMethodTable() {
		return virtualMethodTable;
	}

	/** Get the default initializer.
	 *
	 * @return the default initializer */
	public FunctionDeclaration getDefaultInitializer() {
		return this.defaultInitializer;
	}

	/** Set the default initializer.
	 *
	 * @param defaultInitializer
	 *            the new default initializer */
	public void setDefaultInitializer(FunctionDeclaration defaultInitializer) {
		this.defaultInitializer = defaultInitializer;
	}

	public List<TypeParameterDeclaration> getTypeParameterDeclarations() {
		return typeParameterDeclarations;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** {@inheritDoc} */
	@Override
	public void visitChildren(BaseVisitor visitor) {
		typeParameterDeclarations.forEach(visitor::visitDoubleDispatched);
		visitor.visitDoubleDispatched(block);
	}

	public List<FunctionDeclaration> getMethods() {
		ArrayList<FunctionDeclaration> methods = new ArrayList<>();
		for (Declaration decl : getBlock().getDeclarations()) {
			if (decl instanceof FunctionDeclaration) {
				methods.add((FunctionDeclaration) decl);
			}
		}
		return methods;
	}

	public FunctionDeclaration getWrappedFunction() {
		return wrappedFunction;
	}

	public boolean isFunctionWrapper() {
		return wrappedFunction != null;
	}

	public void setWrappedFunction(FunctionDeclaration functionWrapper) {
		this.wrappedFunction = functionWrapper;
	}

	public boolean isGenerator() {
		return _isGenerator;
	}

	public void setGenerator(boolean gen) {
		_isGenerator = gen;
	}

	public ClassScope getScope() {
		return (ClassScope) super.getScope();
	}

	public String toString(){
		Object abstractGenericTypes = getTypeParameterDeclarations().isEmpty() ? "" : getTypeParameterDeclarations();
		String symbol;
		if(isFunctionWrapper()) {
			symbol = getWrappedFunction().getIdentifier().toString();
		} else {
			symbol = getIdentifier().getSymbol();
		}
		return symbol + abstractGenericTypes;
	}

	public boolean isDummy() {
		return dummy;
	}
}
