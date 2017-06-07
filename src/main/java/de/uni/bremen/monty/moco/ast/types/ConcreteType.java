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
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.util.AllIterator;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.*;

public class ConcreteType extends PartialAppliedTypeInfo {

    private static Set<ConcreteType> allTypes = new HashSet<>();
    private final List<ConcreteType> concreteTypeParamters;

    ConcreteType(ClassDeclaration declaration, List<ConcreteType> concreteTypeParamters, TypeContext context) {
        super(declaration,concreteTypeParamters, context);
        this.concreteTypeParamters = concreteTypeParamters;
        if(declaration.getTypeParameterDeclarations().size() != concreteTypeParamters.size()){
            throw new RuntimeException("abstract and concrete Types does not match");
        }
        if(!declaration.isDummy()) {
            allTypes.add(this);
        }
    }

    public List<ConcreteType> getSuperClassDeclarationsRecursive() {
        List<ConcreteType> allSuperClassDeclarations = new ArrayList<>();
        for (PartialAppliedTypeInfo superClass : declaration.getSuperClassDeclarations()) {
            ConcreteType concreteType = (ConcreteType) superClass.extend(context);
            Collection<ConcreteType> superClassDeclarationsRecursive = concreteType.getSuperClassDeclarationsRecursive();
            allSuperClassDeclarations.addAll(superClassDeclarationsRecursive);
        }
        allSuperClassDeclarations.add(this);
        return allSuperClassDeclarations;
    }

    @Override
    public Identifier getIdentifier() {
        return declaration.getIdentifier();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        ConcreteType that = (ConcreteType) o;

        return new EqualsBuilder()
                .append(declaration, that.declaration)
                .append(concreteTypeParamters, that.concreteTypeParamters)
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .append(declaration.getIdentifier())
                .append(1)
                .append(concreteTypeParamters)
                .toHashCode();
    }

    public List<ConcreteFunctionType> getVirtualMethods() {
        List<ConcreteFunctionType> functions = new ArrayList<>();
        for (FunctionDeclaration functionDeclaration : declaration.getVirtualMethodTable()) {
            functions.add(TypeFactory.makeConcrete(functionDeclaration, context));
        }
        return functions;
    }

    public ConcreteFunctionType getWrappedFunction() {
        return TypeFactory.makeConcrete(declaration.getWrappedFunction(), context);
    }

    public List<ConcreteVariableType> getVariables() {
        ArrayList<ConcreteVariableType> result = new ArrayList<>();
        for (Declaration decl : declaration.getBlock().getDeclarations()) {
            if (decl instanceof VariableDeclaration) {
                result.add(TypeFactory.makeConcrete((VariableDeclaration) decl,context));
            }
        }
        return result;
    }

    public boolean isGenerator() {
        return declaration.isGenerator();
    }

    public ModuleDeclaration getModuleDeclaration() {
        return declaration.getParentNodeByType(ModuleDeclaration.class);
    }

    public List<ConcreteType> getConcreteGenericTypes() {
        return concreteTypeParamters;
    }

    public List<ConcreteFunctionType> getMethods() {
        List<ConcreteFunctionType> functions = new ArrayList<>();

        for (FunctionDeclaration declaration : declaration.getMethods()) {
            functions.add(TypeFactory.makeConcrete(declaration, context));
        }
        return functions;
    }

    public static Iterable<ConcreteType> getAllTypes() {
        return () -> new AllIterator(allTypes);
    }

    public ASTNode getBlock() {
        return declaration.getBlock();
    }

    public boolean hasWrappedFunction() {
        return declaration.getWrappedFunction() != null;
    }

    public static void reset() {
        allTypes.clear();
    }

    public boolean isFunctionWrapper() {
        return declaration.isFunctionWrapper();
    }

    public ConcreteFunctionType getDefaultInitializer() {
        return (ConcreteFunctionType) super.getDefaultInitializer();
    }
}
