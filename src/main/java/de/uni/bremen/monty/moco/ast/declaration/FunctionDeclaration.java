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
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.expression.WrappedFunctionCall;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

import java.util.ArrayList;
import java.util.List;

/** A FunctionDeclaration represents the declaration of a function in the AST.
 * <p>
 * It can be used as a type. */
public class FunctionDeclaration extends TypeDeclaration {
	public enum DeclarationType {
		INITIALIZER, DEFAULT_INITIALIZER, METHOD, UNBOUND
	}

	ClassDeclaration wrapperClass = null;
	VariableDeclaration wrapperFunctionObjectDeclaration = null;
	Assignment wrapperFunctionAssignment = null;

	/** The declarations and statements within this declaration. */
	private final Block body;

	/** The parameters of this declaration. */
	private final List<VariableDeclaration> parameters;

	private DeclarationType declarationType;

	/** Index of the function in the vmt if it is a function in the class struct */
	private int vmtIndex;

	/** The return returnType. */
	private ResolvableIdentifier returnTypeIdentifier;
	private TypeDeclaration returnType;

	private final boolean abstractMethod;

	private boolean returnTypeMustBeInferred = false;

	/** Constructor.
	 *
	 * @param position
	 *            Position of this node
	 * @param identifier
	 *            the identifier
	 * @param body
	 *            the body of this function
	 * @param parameters
	 *            the parameters of this function */
	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, DeclarationType declarationType,
	        ResolvableIdentifier returnTypeIdentifier) {
		super(position, identifier);
		this.body = body;
		this.parameters = parameters;
		this.declarationType = declarationType;
		this.vmtIndex = -1;
		this.returnTypeIdentifier = returnTypeIdentifier;
		this.abstractMethod = false;
	}

	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, DeclarationType declarationType,
	        ResolvableIdentifier returnTypeIdentifier, boolean isAbstract) {
		super(position, identifier);
		this.body = body;
		this.parameters = parameters;
		this.declarationType = declarationType;
		this.vmtIndex = -1;
		this.returnTypeIdentifier = returnTypeIdentifier;
		this.abstractMethod = isAbstract;
	}

	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters) {
		this(position, identifier, body, parameters, DeclarationType.UNBOUND, (ResolvableIdentifier) null);
		returnTypeMustBeInferred = true;
	}

	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, ResolvableIdentifier returnTypeIdentifier) {
		this(position, identifier, body, parameters, DeclarationType.UNBOUND, returnTypeIdentifier);
	}

	/** Constructor
	 *
	 * @param position
	 *            * Position of this node
	 * @param identifier
	 *            the identifier
	 * @param body
	 *            the body of this function
	 * @param parameters
	 *            the parameters of this function
	 * @param returnType
	 *            the return returnType */
	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, ClassDeclaration returnType) {
		this(position, identifier, body, parameters,
		        returnType != null ? ResolvableIdentifier.convert(returnType.getIdentifier()) : null);
		this.returnType = returnType;
	}

	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, DeclarationType declarationType, TypeDeclaration returnType,
	        boolean isAbstract) {
		this(position, identifier, body, parameters, declarationType,
		        returnType != null ? ResolvableIdentifier.convert(returnType.getIdentifier()) : null, isAbstract);
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
		if (this.returnType != null) return;
		this.returnType = returnType;
	}

	protected void setReturnTypeIdentifier(ResolvableIdentifier identifier) {
		this.returnTypeIdentifier = identifier;
	}

	/** Get the body block.
	 *
	 * @return the body */
	public Block getBody() {
		return body;
	}

	/** Get the list of parameters.
	 *
	 * @return the paramter */
	public List<VariableDeclaration> getParameters() {
		return parameters;
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
		for (VariableDeclaration variableDeclaration : parameters) {
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
		if (!(other instanceof FunctionDeclaration)) {
			return false;
		}
		List<VariableDeclaration> otherParameter = ((FunctionDeclaration) other).getParameters();
		if (parameters.size() != otherParameter.size()) {
			return false;
		}
		for (int i = 0; i < parameters.size(); i++) {
			if (!parameters.get(i).getType().matchesType(otherParameter.get(i).getType())) {
				return false;
			}
		}
		FunctionDeclaration proc = ((FunctionDeclaration) other);

		return !((proc.getReturnType() != null) && (proc.getReturnType() != CoreClasses.voidType()))
		        || returnType.matchesType(proc.getReturnType());
	}

	public boolean isAbstract() {
		return abstractMethod;
	}

	public ClassDeclaration getWrapperClass() {
		return wrapperClass;
	}

	public void setWrapperClass(ClassDeclaration wrapperClass) {
		this.wrapperClass = wrapperClass;
	}

	public VariableDeclaration getWrapperFunctionObjectDeclaration() {
		return wrapperFunctionObjectDeclaration;
	}

	public void setWrapperFunctionObjectDeclaration(VariableDeclaration wrapperFunctionObjectDeclaration) {
		this.wrapperFunctionObjectDeclaration = wrapperFunctionObjectDeclaration;
	}

	public Assignment getWrapperFunctionAssignment() {
		return wrapperFunctionAssignment;
	}

	public void setWrapperFunctionAssignment(Assignment wrapperFunctionAssignment) {
		this.wrapperFunctionAssignment = wrapperFunctionAssignment;
	}

	public boolean isReturnTypeToBeInferred() {
		return returnTypeMustBeInferred;
	}

	public void setInferredReturnType(TypeDeclaration type) {
		returnType = type;
		returnTypeIdentifier = ResolvableIdentifier.convert(type.getIdentifier());

		ResolvableIdentifier retIdent = returnTypeIdentifier;
		FunctionDeclaration applyMethod = wrapperClass.getMethods().get(0);

		if (CoreClasses.voidType().equals(type)) {
			returnTypeIdentifier = null;
			retIdent = new ResolvableIdentifier("Tuple0");
			applyMethod.setReturnTypeIdentifier(retIdent);
			applyMethod.setReturnType(getScope().resolveType(retIdent));

			splitReturnStatement(false);
			applyMethod.splitReturnStatement(true);

		} else {
			applyMethod.setReturnTypeIdentifier(returnTypeIdentifier);
			applyMethod.setReturnType(type);
		}

		// exchange the return type of the wrapper class
		ResolvableIdentifier genericWrapper = wrapperClass.getSuperClassIdentifiers().get(0);
		genericWrapper.getGenericTypes().remove(1);
		genericWrapper.getGenericTypes().add(retIdent);
	}

	protected void splitReturnStatement(boolean tupleInsteadOfVoid) {
		ReturnStatement oldRet = (ReturnStatement) getBody().getStatements().get(getBody().getStatements().size() - 1);

		// create a new return statement
		ReturnStatement newRet;
		if (tupleInsteadOfVoid) {
			FunctionCall emptyTuple =
			        new FunctionCall(oldRet.getPosition(), new ResolvableIdentifier("Tuple0"),
			                new ArrayList<Expression>());
			newRet = new ReturnStatement(oldRet.getPosition(), emptyTuple);
			newRet.setScope(oldRet.getScope());
			newRet.setParentNode(oldRet.getParentNode());
			emptyTuple.setScope(newRet.getScope());
			emptyTuple.setParentNode(newRet.getParentNode());
		} else {
			newRet = new ReturnStatement(oldRet.getPosition(), null);
			newRet.setScope(oldRet.getScope());
			newRet.setParentNode(oldRet.getParentNode());
		}

		// remove the old one
		getBody().getStatements().remove(getBody().getStatements().size() - 1);
		// add the new ones
		Expression oldCall = oldRet.getParameter();
		oldCall.setParentNode(getBody());
		if (oldCall instanceof FunctionCall) {
			getBody().addStatement((FunctionCall) oldCall);
		} else if (oldCall instanceof WrappedFunctionCall) {
			getBody().addStatement((WrappedFunctionCall) oldCall);
		} else {
			throw new RuntimeException("invalid AST!");
		}
		getBody().addStatement(newRet);
	}
}
