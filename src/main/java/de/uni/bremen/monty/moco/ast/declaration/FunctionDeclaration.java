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
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.ast.statement.Statement;
import de.uni.bremen.monty.moco.ast.types.*;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;
import de.uni.bremen.monty.moco.visitor.VisitOnceVisitor;

import java.util.*;

/** A FunctionDeclaration represents the declaration of a function in the AST.
 * <p>
 * It can be used as a type. */
public class FunctionDeclaration extends TypeDeclaration {
	public enum DeclarationType {
		INITIALIZER, DEFAULT_INITIALIZER, METHOD, UNBOUND
	}

	private ClassDeclaration wrapperClass = null;
	private VariableDeclaration wrapperFunctionObjectDeclaration = null;
	private Assignment wrapperFunctionAssignment = null;

	/** The declarations and statements within this declaration. */
	private final Block body;

	/** The parameters of this declaration. */
	private final List<VariableDeclaration> parameters;

	private DeclarationType declarationType;

	/** Index of the function in the vmt if it is a function in the class struct */
	private int vmtIndex;

	/** The return returnType. */
	private ResolvableIdentifier returnTypeIdentifier;
	private Type returnType;

	private final boolean abstractMethod;

	private boolean returnTypeMustBeInferred = false;

	private Map<VariableDeclaration, VariableDeclaration> closureVars = new HashMap<>();

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
		this(position, identifier, body, parameters, DeclarationType.UNBOUND, null);
		returnTypeMustBeInferred = true;
	}

	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, DeclarationType declarationType, Type returnType,
	        boolean isAbstract) {
		this(position, identifier, body, parameters, declarationType,
		        returnType != null ? ResolvableIdentifier.convert(returnType.getIdentifier()) : null, isAbstract);
		this.returnType = returnType;
	}

	/** get the return returnType.
	 *
	 * @return the return returnType */
	public ResolvableIdentifier getReturnTypeIdentifier() {
		if (returnTypeIdentifier == null && !returnTypeMustBeInferred) {
			return ResolvableIdentifier.convert(Types.voidType().getIdentifier());
		}
		return returnTypeIdentifier;
	}

	/** get the returnType.
	 *
	 * @return the returnType */
	public Type getReturnType() {
		if (returnType == null && !returnTypeMustBeInferred) {
			return Types.voidType();
		}
		return returnType;
	}

	/** set the returnType
	 *
	 * @param returnType */
	public void setReturnType(Type returnType) {
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
			return getParentNodeByType(ClassDeclaration.class);
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
//		if(wrapperFunctionObjectDeclaration != null){
//			visitor.visitDoubleDispatched(wrapperFunctionAssignment);
//		}
		visitor.visitDoubleDispatched(body);
	}

	/** @return true if the procedure has no return type */
	public boolean isProcedure() {
		return returnTypeIdentifier == null
		        || new ResolvableIdentifier(CoreClasses.VOID_SYMBOL).equals(returnTypeIdentifier);
	}

	/** @return true if the function has a return type */
	public boolean isFunction() {
		return !isProcedure();
	}

	public boolean overridesFunction(FunctionType other) {
//		ClassDeclaration thisClass = this.getParentNodeByType(ClassDeclaration.class); // List
//		ClassDeclaration otherClass = other.getParentNodeByType(ClassDeclaration.class); // Collection
//		if(thisClass.isAssignableFrom(otherClass)){
//			throw new RuntimeException("this Function must be invoked from the super class");
//		}
//		if(thisClass.getSuperClassDeclarations())
		if (!other.getIdentifier().getSymbol().equals(getIdentifier().getSymbol())) {
			return false;
		}
		List<? extends Type> otherParameter = other.getParameterTypes();
		if (parameters.size() != otherParameter.size()) {
			return false;
		}
		for (int i = 0; i < parameters.size(); i++) {
			Type type = otherParameter.get(i);
			if (!parameters.get(i).getType().matchesTypeExactly(type)) {
				return false;
			}
		}

		return !((other.getReturnType() != null) && (!other.getReturnType().isVoid()))
		        || returnType.isAssignableFrom(other.getReturnType());
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

	public void setInferredReturnType(Type type, VisitOnceVisitor visitor) {
		returnType = type;
		returnTypeIdentifier = ResolvableIdentifier.convert(type.getIdentifier());

		ResolvableIdentifier retIdent = returnTypeIdentifier;
		FunctionDeclaration applyMethod = wrapperClass.getMethods().get(0);

		if (type.isVoid()) {
			returnTypeIdentifier = null;
			retIdent = new ResolvableIdentifier("Tuple0");
			applyMethod.setReturnTypeIdentifier(retIdent);
			Type tuple0Type = getScope().resolveType(retIdent, visitor);
			applyMethod.setReturnType(tuple0Type);

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

	protected ReturnStatement splitReturnStatement(boolean tupleInsteadOfVoid) {
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

		if (oldCall instanceof Statement) {
			getBody().addStatement((Statement) oldCall);
		} else {
			throw new RuntimeException("invalid AST!");
		}
		getBody().addStatement(newRet);
		return newRet;
	}

	public String toString() {
		String params = "";
		for (VariableDeclaration param : parameters) {
			params += param.getTypeIdentifier().toString();
			params += " " + param.getIdentifier().toString() + ", ";
		}
		return String.format("%s(%s)", getIdentifier().toString(), params);
	}

	private VariableDeclaration checkSelfVariable(VariableDeclaration var) {
		if (var.getIdentifier().getSymbol().equals("self")) {
			for (VariableDeclaration key : closureVars.keySet()) {
				if (key.getIdentifier().getSymbol().equals("self")) {
					return key;
				}
			}
		}
		return var;
	}

	public VariableDeclaration addClosureVariable(VariableDeclaration var) {
		var = checkSelfVariable(var);
		if (!closureVars.containsKey(var)) {
			VariableDeclaration attr =
			        new VariableDeclaration(var.getPosition(), var.getIdentifier(), var.getTypeIdentifier(),
			                VariableDeclaration.DeclarationType.ATTRIBUTE);
			attr.setParentNode(var.getParentNode());
			attr.setScope(var.getScope());
			attr.setType(var.getType());
			closureVars.put(var, attr);
			return attr;
		}
		return closureVars.get(var);
	}

	public boolean isClosure() {
		return closureVars.size() > 0;
	}

	public VariableDeclaration getClosureVariable(VariableDeclaration var) {
		var = checkSelfVariable(var);
		return closureVars.get(var);
	}

	public Collection<VariableDeclaration> getClosureVariables() {
		return closureVars.values();
	}

	public Collection<VariableDeclaration> getClosureVariableOriginalDeclarations() {
		return closureVars.keySet();
	}
}
