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
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

import java.util.List;

/** A ProcedureDeclaration represents the declaration of a procedure in the AST.
 * <p>
 * It can be used as a type. */
public class ProcedureDeclaration extends TypeDeclaration {
	public enum DeclarationType {
		INITIALIZER, DEFAULT_INITIALIZER, METHOD, UNBOUND
	}

	/** The declarations and statements within this declaration. */
	private final Block body;

	/** The parameters of this declaration. */
	private final List<VariableDeclaration> parameter;

	private DeclarationType declarationType;

	/** Index of the procedure in the vmt if it is a procedure in the class struct */
	private int vmtIndex;

	/** The return returnType. */
	private ResolvableIdentifier returnTypeIdentifier;
	private TypeDeclaration returnType;

	/** Constructor.
	 *
	 * @param position
	 *            Position of this node
	 * @param identifier
	 *            the identifier
	 * @param body
	 *            the body of this procedure
	 * @param parameter
	 *            the parameter of this procedure */
	public ProcedureDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, DeclarationType declarationType,
	        ResolvableIdentifier returnTypeIdentifier) {
		super(position, identifier);
		this.body = body;
		this.parameter = parameter;
		this.declarationType = declarationType;
		this.vmtIndex = -1;
		this.returnTypeIdentifier = returnTypeIdentifier;
	}

	public ProcedureDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, ResolvableIdentifier returnTypeIdentifier) {
		this(position, identifier, body, parameter, DeclarationType.UNBOUND, returnTypeIdentifier);
	}

	/** Constructor
	 *
	 * @param position
	 *            * Position of this node
	 * @param identifier
	 *            the identifier
	 * @param body
	 *            the body of this function
	 * @param parameter
	 *            the parameter of this function
	 * @param returnType
	 *            the return returnType */
	public ProcedureDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, ClassDeclaration returnType) {
		this(position, identifier, body, parameter,
		        returnType != null ? ResolvableIdentifier.convert(returnType.getIdentifier()) : null);
		this.returnType = returnType;
	}

	public ProcedureDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, DeclarationType declarationType, TypeDeclaration returnType) {
		this(position, identifier, body, parameter, declarationType,
		        returnType != null ? ResolvableIdentifier.convert(returnType.getIdentifier()) : null);
		this.returnType = returnType;
	}

	/** get the return returnType.
	 *
	 * @return the return returnType */
	public ResolvableIdentifier getReturnTypeIdentifier() {
		return returnTypeIdentifier;
	}

	/** get the returnType.
	 *
	 * @return the returnType */
	public TypeDeclaration getReturnType() {
		if (returnType == null) {
			return CoreClasses.voidType();
		}
		return returnType;
	}

	/** set the returnType
	 *
	 * @param returnType */
	public void setReturnType(TypeDeclaration returnType) {
		this.returnType = returnType;
	}

	/** Get the body block.
	 *
	 * @return the body */
	public Block getBody() {
		return body;
	}

	/** Get the list of parameter.
	 *
	 * @return the paramter */
	public List<VariableDeclaration> getParameter() {
		return parameter;
	}

	/** set the declaration type */
	public void setDeclarationType(DeclarationType type) {
		this.declarationType = type;
	}

	/** get the declaration type
	 *
	 * @return the declaration type */
	public DeclarationType getDeclarationType() {
		return declarationType;
	}

	public boolean isInitializer() {
		return declarationType == DeclarationType.INITIALIZER || declarationType == DeclarationType.DEFAULT_INITIALIZER;
	}

	public boolean isDefaultInitializer() {
		return declarationType == DeclarationType.DEFAULT_INITIALIZER;
	}

	public boolean isMethod() {
		return declarationType == DeclarationType.METHOD;
	}

	public boolean isUnbound() {
		return declarationType == DeclarationType.UNBOUND;
	}

	public ClassDeclaration getDefiningClass() {
		if (isMethod() || isInitializer()) {
			ASTNode parentNode = getParentNode().getParentNode();

			if (parentNode.getParentNode() instanceof ClassDeclarationVariation) {
				parentNode = parentNode.getParentNode();
			}

			return (ClassDeclaration) parentNode;
		}
		return null;
	}

	/** Get the vmtIndex. */
	public int getVMTIndex() {
		return vmtIndex;
	}

	/** Set the vmtIndex. */
	public void setVMTIndex(int vmtIndex) {
		this.vmtIndex = vmtIndex;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** {@inheritDoc} */
	@Override
	public void visitChildren(BaseVisitor visitor) {
		for (VariableDeclaration variableDeclaration : parameter) {
			visitor.visitDoubleDispatched(variableDeclaration);
		}
		visitor.visitDoubleDispatched(body);
	}

	/** @return true if the procedure has no return type */
	public boolean isProcedure() {
		return returnTypeIdentifier == null;
	}

	/** @return true if the function has a return type */
	public boolean isFunction() {
		return returnTypeIdentifier != null;
	}

	/** Check equality of two types taking into account the AST object hierachy.
	 * <p>
	 *
	 * @param other
	 *            the other TypeDeclaration to check against
	 * @return if equal */
	@Override
	public boolean matchesType(TypeDeclaration other) {
		if (!super.matchesType(other)) {
			return false;
		}
		if (!(other instanceof ProcedureDeclaration)) {
			return false;
		}
		List<VariableDeclaration> otherParameter = ((ProcedureDeclaration) other).getParameter();
		if (parameter.size() != otherParameter.size()) {
			return false;
		}
		for (int i = 0; i < parameter.size(); i++) {
			if (!parameter.get(i).getType().matchesType(otherParameter.get(i).getType())) {
				return false;
			}
		}
		ProcedureDeclaration proc = ((ProcedureDeclaration) other);

		return !((proc.getReturnType() != null) && (proc.getReturnType() != CoreClasses.voidType()))
		        || returnType.matchesType(proc.getReturnType());
	}
}
