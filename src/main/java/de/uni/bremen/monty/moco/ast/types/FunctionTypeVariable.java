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
import de.uni.bremen.monty.moco.ast.declaration.VariableDeclaration;

import java.util.Collections;
import java.util.List;

public class FunctionTypeVariable implements ConcreteFunctionType {
    private final List<ConcreteType> paramTypes;
    private final ConcreteType returnType;
    private VariableDeclaration declaration;

    FunctionTypeVariable(VariableDeclaration declaration) {
        this.declaration = declaration;
        if(!declaration.getType().isFunction()){
            throw new RuntimeException("Function Variable has to be a Function");
        }
        List<ConcreteType> functionTypes = ((ConcreteType) declaration.getType()).getConcreteGenericTypes();

        ConcreteType paramType = functionTypes.get(0);
        if (paramType.isTuple()) {
            paramTypes = paramType.getConcreteGenericTypes();
        } else {
            paramTypes =  Collections.singletonList(paramType);
        }

        returnType = functionTypes.get(1);

        if(!(returnType instanceof ConcreteType && paramTypes.stream().allMatch( s -> s instanceof ConcreteType) )) {
            throw new RuntimeException("Only concrete Function Variables are allowed");
        }
    }

    @Override
    public int getVMTIndex() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Identifier getIdentifier() {
        return declaration.getIdentifier();
    }

    @Override
    public ConcreteType getDefiningClass() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ConcreteType getReturnType() {
        return returnType;
    }

    @Override
    public List<ConcreteVariableType> getParameter() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isFunction() {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<? extends Type> getParameterTypes() {
        return paramTypes;
    }

    @Override
    public ASTNode getASTNode() {
        throw new UnsupportedOperationException();
    }

    @Override
    public TypeContext getContext() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isMethod() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isInitializer() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isClosure() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ConcreteType getWrapperClass() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isDefaultInitializer() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ConcreteVariableType getWrapperFunctionObjectDeclaration() {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<ConcreteVariableType> getClosureVariables() {
        throw new UnsupportedOperationException();
    }

    @Override
    public FunctionDeclaration.DeclarationType getDeclarationType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public FunctionType extend(TypeContext context) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString() {
        throw new UnsupportedOperationException();
    }
}
