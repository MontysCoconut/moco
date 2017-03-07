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

import de.uni.bremen.monty.moco.ast.ClassScope;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration;
import de.uni.bremen.monty.moco.visitor.VisitOnceVisitor;

import java.util.ArrayList;
import java.util.List;

public class PartialAppliedTypeInfo extends Type {

    protected final ClassDeclaration declaration;
    private final List<? extends Type> partialAppliedTypes;
    protected final TypeContext context;

    PartialAppliedTypeInfo(ClassDeclaration declaration, List<? extends Type> partialAppliedTypes, TypeContext context) {
        this.declaration = declaration;
        this.partialAppliedTypes = partialAppliedTypes;
        this.context = TypeContext.from(declaration, partialAppliedTypes).extend(context);
    }

    @Override
    public boolean matchesTypeExactly(Type type) {
        if(type instanceof PartialAppliedTypeInfo){
            PartialAppliedTypeInfo other = (PartialAppliedTypeInfo) type;
            if (other.declaration.equals(this.declaration)) {
                for (int i = 0; i < partialAppliedTypes.size(); i++) {
                    Type partialAppliedType = partialAppliedTypes.get(i);
                    Type typeVariable = other.partialAppliedTypes.get(i);
                    if(typeVariable instanceof TypeVariable){
                        continue;
                    }
                    if(!partialAppliedType.matchesTypeExactly(typeVariable)){
                        return false;
                    }
                }
                return true;
            } else if (matchesVoidAndTuple0(this, other) || matchesVoidAndTuple0(other, this)) {
                return true;
            }
        }
//        else {
////            context.makeConcrete()
////            TypeVariable typeVariable = (TypeVariable) type;
////            typeVariable.turnConcrete(this)
////            turnConcrete(typeVariable)
//            return false;
//        }
        return false;
    }

    private static boolean matchesVoidAndTuple0(PartialAppliedTypeInfo type, PartialAppliedTypeInfo other) {
        return type.isVoid() && other.isTuple(0);
    }

    @Override
    public Identifier getIdentifier() {
        return declaration.getIdentifier();
    }

    public ClassScope getScope() {
        return declaration.getScope().extend(context);
    }

    public int getLastAttributeIndex() {
        return declaration.getLastAttributeIndex();
    }

    public List<FunctionDeclaration> getVirtualMethodTable() {
        return declaration.getVirtualMethodTable();
    }

    public List<PartialAppliedTypeInfo> getSuperClassDeclarations() {
        return declaration.getSuperClassDeclarations();
    }

    @Override
    public boolean isAssignableFrom(Type other) {
        if (matchesTypeExactly(other)) {
            return true;
        }
        if ((other instanceof PartialAppliedTypeInfo)) {
            PartialAppliedTypeInfo otherType = (PartialAppliedTypeInfo) other;
            boolean superMatch = declaration.getSuperClassDeclarations().stream().anyMatch(e -> e.declaration == otherType.declaration);
            return superMatch;
        }
        return false;
    }

    @Override
    protected boolean matchesDeclaration(ClassDeclaration declaration) {
        return this.declaration.equals(declaration);
    }

    public TypeContext getContext() {
        return context;
    }

//    ConcreteTypeInfo turnConcrete(TypeContext context) {
//        ArrayList<ConcreteTypeInfo> concreteTypeParamters = new ArrayList<>();
//        for (Type partialAppliedType : partialAppliedTypes) {
//            concreteTypeParamters.add(context.makeConcrete(partialAppliedType));
//        }
//        return new ConcreteTypeInfo(declaration, concreteTypeParamters);
//    }

    public Type extend(TypeContext context) {
        return TypeFactory.from(declaration, this.context.extend(context));
    }

    public String toString() {
        Object genericTypes = partialAppliedTypes.isEmpty() ? "" : partialAppliedTypes;
        return getIdentifier().getSymbol() + genericTypes;
    }

    public boolean isTuple(int n) {
        return isTuple() && partialAppliedTypes.size() == n;
    }

    public boolean isTuple() {
        String strIdent = getIdentifier().getSymbol();
        if (strIdent.startsWith("Tuple")) {
            int n;
            try {
                n = Integer.parseInt(strIdent.substring(5));
            } catch (Exception e) {
                return false;
            }
            if (partialAppliedTypes.size() == n) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isFunction() {
        return isAssignableFrom(Types.functionType());
    }

    public List<? extends Type> getPartialAppliedTypes() {
        return partialAppliedTypes;
    }

    public boolean isAbstract() {
        return declaration.isAbstract();
    }

    public FunctionType getDefaultInitializer() {
        return TypeFactory.from(declaration.getDefaultInitializer(), context);
    }

    public List<FunctionType> getInitializers(VisitOnceVisitor visitor) {
        List<FunctionType> functions = new ArrayList<>();

        // iterate through the declarations of the given class
        for (FunctionDeclaration declaration : declaration.getMethods()) {
            // find a matching declaration
            if ("initializer".equals(declaration.getIdentifier().getSymbol())) {
                // and verify that it is a function...
                // without any return type
                if (!(declaration.isFunction())) {
                    visitor.visitDoubleDispatched(declaration);
                    functions.add(TypeFactory.from(declaration, context));
                }
            }
        }
        return functions;
    }

    public void visit(VisitOnceVisitor visitor) {
        visitor.visitDoubleDispatched(declaration);
    }
}
