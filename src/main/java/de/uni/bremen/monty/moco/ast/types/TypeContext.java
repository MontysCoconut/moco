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

import de.uni.bremen.monty.moco.ast.declaration.TypeParameterDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;
import de.uni.bremen.monty.moco.exception.InvalidTypeException;

import java.util.List;

public abstract class TypeContext {
    public static TypeContext EMPTY = new TypeContext() {
        @Override
        public TypeContext extend(TypeContext context) {
            return context;
        }

        @Override
        public Type resolve(Type type) {
            return type;
        }

        @Override
        public Type resolve(TypeParameterDeclaration typeVariable) {
            return new TypeVariable(typeVariable);
        }

        @Override
        protected Type tryToResolve(TypeParameterDeclaration typeVariable) {
            return null;
        }
    };

    public abstract TypeContext extend(TypeContext context);
    public abstract Type resolve(Type type);
    public abstract Type resolve(TypeParameterDeclaration typeVariable);
    protected abstract Type tryToResolve(TypeParameterDeclaration typeVariable);

    public ConcreteType makeConcrete(Type type) {
        return (ConcreteType) resolve(type);
    }

    public static TypeContext from(ClassDeclaration declaration, List<? extends Type> partialAppliedTypes) {
        int declsize = declaration.getTypeParameterDeclarations().size();
        if(declsize != partialAppliedTypes.size()){
            throw new InvalidTypeException(String.format("The class %s has %d Typeparameters but only %d were provided", declaration.getIdentifier(), declsize, partialAppliedTypes.size()));
        }
        return new TypeContextImpl(declaration, partialAppliedTypes);
    }
}
