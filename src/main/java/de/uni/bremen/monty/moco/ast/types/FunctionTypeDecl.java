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
package de.uni.bremen.monty.moco.ast.types;

import de.uni.bremen.monty.moco.ast.ASTNode;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration;

import java.util.List;
import java.util.stream.Collectors;

public class FunctionTypeDecl implements FunctionType {
    protected final FunctionDeclaration declaration;
    protected final TypeContext context;
    protected final Type returnType;
    protected final PartialAppliedTypeInfo definingClass;
    protected final List<? extends VariableType> parameter;
    protected final PartialAppliedTypeInfo wrapperClass;
    protected final VariableType wrapperFunctionObjects;
    protected final List<? extends VariableType> closureVariables;

    FunctionTypeDecl(FunctionDeclaration declaration, TypeContext context, List<? extends VariableType> parameter, Type returnType, PartialAppliedTypeInfo definingClass, PartialAppliedTypeInfo wrapperClass, VariableType wrapperFunctionObjects, List<? extends VariableType> closureVariables) {
        this.declaration = declaration;
        this.context = context;
        this.parameter = parameter;
        this.returnType = returnType;
        this.definingClass = definingClass;
        this.wrapperClass = wrapperClass;
        this.wrapperFunctionObjects = wrapperFunctionObjects;
        this.closureVariables = closureVariables;
    }

    @Override
    public int getVMTIndex() {
        return declaration.getVMTIndex();
    }

    @Override
    public Identifier getIdentifier() {
        return declaration.getIdentifier();
    }

    @Override
    public PartialAppliedTypeInfo getDefiningClass() {
        return definingClass;
    }

    @Override
    public Type getReturnType() {
        if(declaration.isReturnTypeToBeInferred()){
           return declaration.getReturnType().extend(context);
        }
        return returnType;
    }

    @Override
    public boolean isFunction() {
        return declaration.isFunction();
    }

    public List<Type> getParameterTypes() {
        return parameter.stream().map(VariableType::getType).collect(Collectors.toList());
    }

    public List<? extends VariableType> getParameter() {
         return parameter;
    }

    @Override
    public ASTNode getASTNode() {
        return declaration;
    }

    @Override
    public TypeContext getContext() {
        return context;
    }

    @Override
    public boolean isMethod() {
        return declaration.isMethod();
    }

    @Override
    public boolean isInitializer() {
        return declaration.isInitializer();
    }

    @Override
    public boolean isClosure() {
        return declaration.isClosure();
    }

    @Override
    public PartialAppliedTypeInfo getWrapperClass() {
        return wrapperClass;
    }

    @Override
    public boolean isDefaultInitializer() {
        return declaration.isDefaultInitializer();
    }

    @Override
    public VariableType getWrapperFunctionObjectDeclaration() {
        return wrapperFunctionObjects;
    }

    @Override
    public List<? extends VariableType> getClosureVariables() {
        return closureVariables;
    }

    @Override
    public FunctionDeclaration.DeclarationType getDeclarationType() {
        return declaration.getDeclarationType();
    }

    @Override
    public FunctionType extend(TypeContext context) {
        return TypeFactory.from(declaration, this.context.extend(context));
    }

    @Override
    public String toString() {
        return declaration.toString();
    }
}
