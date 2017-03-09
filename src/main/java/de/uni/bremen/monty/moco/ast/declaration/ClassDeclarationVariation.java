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

import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.ClassScope;
import de.uni.bremen.monty.moco.ast.ResolvableIdentifier;
import de.uni.bremen.monty.moco.ast.statement.Statement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ClassDeclarationVariation extends ClassDeclaration {

	private List<ClassDeclaration> concreteGenericTypes;

	private final ClassDeclaration baseClass;

	public ClassDeclarationVariation(ClassDeclaration classDecl, ResolvableIdentifier identifier,
	        List<ClassDeclaration> concreteGenericTypes) {
		super(classDecl.getPosition(), identifier, classDecl.getSuperClassIdentifiers(), new Block(
		        classDecl.getBlock().getPosition()), classDecl.isAbstract(), classDecl.getAbstractGenericTypes());
		this.baseClass = classDecl;
		this.concreteGenericTypes = concreteGenericTypes;
		setParentNode(classDecl.getParentNode());
		ClassScope classScope = new ClassScope(classDecl.getScope().getParentScope());
		setScope(classScope);
		classDecl.addVariation(this);

		addSuperClassDeclarations(classDecl);

		Collection<FunctionDeclaration> functionDeclarations = mapFunctions(classDecl.getVirtualMethodTable());
		getVirtualMethodTable().addAll(functionDeclarations);
		mapBlock(classDecl.getBlock());
		getBlock().setParentNode(this);
		FunctionDeclaration defaultInitializer = mapFunction(classDecl.getDefaultInitializer());
		setDefaultInitializer(defaultInitializer);
		for (Declaration functionDeclaration : getBlock().getDeclarations()) {
			if (!(functionDeclaration instanceof FunctionDeclaration)
			        || !((FunctionDeclaration) functionDeclaration).isDefaultInitializer()) {
				classScope.define(functionDeclaration);
			}
		}
		classScope.define(defaultInitializer);
	}

	/** The parent (super) class declarations should also be referenced in the ClassDeclarationVariation and not only in
	 * the ClassDeclaration, since they can be used interchangeably
	 *
	 * @param classDecl */
	private void addSuperClassDeclarations(ClassDeclaration classDecl) {
		List<TypeDeclaration> ownParents = getSuperClassDeclarations();
		for (TypeDeclaration parent : classDecl.getSuperClassDeclarations()) {
			// if the parent class is a concrete generic class, we can add it
			if (parent instanceof ClassDeclarationVariation) {
				ownParents.add(parent);
			} else if (parent instanceof ClassDeclaration) {
				// if the parent class has no unresolved abstract generic types, we can add it as well
				if (((ClassDeclaration) parent).getAbstractGenericTypes().isEmpty()) {
					ownParents.add(parent);
					((ClassScope) getScope()).addParentClassScope((ClassScope) parent.getScope());
				}
				// if there are unresolved type parameters, we have to resolve them using our own
				else {
					TypeDeclaration newParent = mapAbstractGenericSuperClass((ClassDeclaration) parent);
					ownParents.add(newParent);
					((ClassScope) getScope()).addParentClassScope((ClassScope) newParent.getScope());
				}
			}
			// if the parent is neither one of the above, something is terribly going wrong...
			else {
				throw new RuntimeException("Was ist hier los?!?");
			}
		}
	}

	/** converts a parent ClassDeclaration with abstractGenericTypes into a ClassDeclarationVariation with
	 * concreteGenericTypes
	 *
	 * @return */
	private TypeDeclaration mapAbstractGenericSuperClass(ClassDeclaration parent) {
		List<AbstractGenericType> placeholders = parent.getAbstractGenericTypes();
		List<AbstractGenericType> ownPlaceholders = baseClass.getAbstractGenericTypes();
		ArrayList<ClassDeclaration> concreteTypes = new ArrayList<>(placeholders.size());
		ArrayList<ResolvableIdentifier> concreteIdentifiers = new ArrayList<>(placeholders.size());

		for (AbstractGenericType placeholder : placeholders) {
			int index = ownPlaceholders.indexOf(placeholder);
			concreteTypes.add(concreteGenericTypes.get(index));
			concreteIdentifiers.add(ResolvableIdentifier.convert(concreteGenericTypes.get(index).getIdentifier()));
		}

		return parent.getVariation(
		        new ResolvableIdentifier(parent.getIdentifier().getSymbol(), concreteIdentifiers),
		        concreteTypes);
	}

	private void mapBlock(Block block) {
		for (Statement statement : block.getStatements()) {
			getBlock().addStatement(statement);
		}
		for (Declaration declaration : block.getDeclarations()) {

			if (declaration instanceof FunctionDeclaration) {
				declaration = mapFunction((FunctionDeclaration) declaration);
			} else if (declaration instanceof VariableDeclaration) {
				declaration = mapDeclaration((VariableDeclaration) declaration);
			}
			getBlock().addDeclaration(declaration);
		}
	}

	private Collection<FunctionDeclaration> mapFunctions(List<FunctionDeclaration> originalVirtualMethods) {
		ArrayList<FunctionDeclaration> functionDeclarations = new ArrayList<>();
		for (FunctionDeclaration functionDeclaration : originalVirtualMethods) {
			functionDeclarations.add(mapFunction(functionDeclaration));
		}
		return functionDeclarations;
	}

	private FunctionDeclaration mapFunction(FunctionDeclaration functionDeclaration) {
		FunctionDeclaration funDecl;
		// important for generic inheritance

		if (!functionDeclaration.getDefiningClass().getAbstractGenericTypes().isEmpty()) {
			ClassDeclarationVariation parent = findCorrectSuperClass(functionDeclaration);
			parent = parent != null ? parent : this;
			if (functionDeclaration.isFunction()) {
				TypeDeclaration returnType = mapGenericType((functionDeclaration).getReturnType());
				funDecl = new ConcreteProcDecl(parent, functionDeclaration, returnType);
			} else {
				funDecl = new ConcreteProcDecl(parent, functionDeclaration);
			}
			funDecl.getParameters().addAll(mapParameter(functionDeclaration.getParameters(), funDecl));
			funDecl.setParentNode(this);
			funDecl.setScope(functionDeclaration.getScope());
			return funDecl;
		}
		return functionDeclaration;
	}

	/** inherited methods in the VMT should get the correct ClassDeclarationVariation reference. If this does not happen,
	 * inheritance does not work for ``Gen2<T> ----|> Gen1<T>`` but only for ``NotGen ----|> Gen<Int>``
	 *
	 * @param funDecl
	 * @return */
	private ClassDeclarationVariation findCorrectSuperClass(FunctionDeclaration funDecl) {
		ClassDeclaration classDecl = funDecl.getDefiningClass();
		for (TypeDeclaration parent : getSuperClassDeclarations()) {
			if (parent instanceof ClassDeclarationVariation) {
				int index = classDecl.getVariations().indexOf(parent);
				if (index >= 0) {
					return classDecl.getVariations().get(index);
				}
			}
		}
		return null;
	}

	private TypeDeclaration mapGenericType(TypeDeclaration type) {
		if (type instanceof AbstractGenericType) {
			return mapAbstractToConcrete((AbstractGenericType) type);
		} else if ((type instanceof ClassDeclaration)
		        && (!((ClassDeclaration) type).getAbstractGenericTypes().isEmpty())) {
			return mapAbstractGenericToConcrete((ClassDeclaration) type);
		} else {
			return type;
		}
	}

	public TypeDeclaration mapAbstractGenericToConcrete(ClassDeclaration type) {
		ArrayList<ClassDeclaration> concreteTypes = new ArrayList<>(type.getAbstractGenericTypes().size());
		ArrayList<ResolvableIdentifier> concreteIdentifiers = new ArrayList<>(type.getAbstractGenericTypes().size());
		for (AbstractGenericType abs : type.getAbstractGenericTypes()) {
			TypeDeclaration con = mapGenericType(abs);
			concreteTypes.add((ClassDeclaration) con);
			concreteIdentifiers.add(ResolvableIdentifier.convert(con.getIdentifier()));
		}
		TypeDeclaration result =
		        type.getVariation(
		                new ResolvableIdentifier(type.getIdentifier().getSymbol(), concreteIdentifiers),
		                concreteTypes);
		return result;
	}

	public ClassDeclaration mapAbstractToConcrete(AbstractGenericType type) {
		int index = getAbstractGenericTypes().indexOf(type);
		if (index >= 0) {
			return concreteGenericTypes.get(index);
		} else {
			throw new RuntimeException("is this really an Error?");
		}
	}

	public List<ClassDeclaration> getConcreteGenericTypes() {
		return concreteGenericTypes;
	}

	private List<VariableDeclaration> mapParameter(List<VariableDeclaration> parameter, FunctionDeclaration decl) {
		ArrayList<VariableDeclaration> params = new ArrayList<>();
		for (VariableDeclaration variableDeclaration : parameter) {
			VariableDeclaration var;
			TypeDeclaration abstractType = variableDeclaration.getType();
			if (abstractType instanceof AbstractGenericType) {
				ClassDeclaration type = mapAbstractToConcrete((AbstractGenericType) abstractType);
				var =
				        new VariableDeclaration(variableDeclaration.getPosition(), variableDeclaration.getIdentifier(),
				                type, variableDeclaration.getDeclarationType());
				var.setParentNode(decl);
				var.setScope(getScope());
			} else {
				var = variableDeclaration;
			}
			params.add(var);
		}
		return params;
	}

	private Declaration mapDeclaration(VariableDeclaration declaration) {
		VariableDeclaration variableDeclaration =
		        new VariableDeclaration(declaration.getPosition(), declaration.getIdentifier(),
		                mapGenericType(declaration.getType()), declaration.getDeclarationType());
		variableDeclaration.setParentNode(this);
		variableDeclaration.setScope(getScope());
		variableDeclaration.setAttributeIndex(declaration.getAttributeIndex());
		return variableDeclaration;
	}

	@Override
	public List<ClassDeclarationVariation> getVariations() {
		return baseClass.getVariations();
	}

	@Override
	public TypeDeclaration getVariation(ResolvableIdentifier genericIdentifier,
	        ArrayList<ClassDeclaration> concreteGenerics) {
		return baseClass.getVariation(genericIdentifier, concreteGenerics);
	}

	@Override
	public void addVariation(ClassDeclarationVariation variation) {
		baseClass.addVariation(variation);
	}

	/** this method checks whether the type parameters match exactly
	 *
	 * @param other
	 * @return true if all params have the same type */
	protected boolean doTypeParamsMatch(TypeDeclaration other) {
		if (other instanceof ClassDeclarationVariation) {
			List<ClassDeclaration> otherGenerics = ((ClassDeclarationVariation) other).getConcreteGenericTypes();
			List<ClassDeclaration> ownGenerics = getConcreteGenericTypes();
			if (otherGenerics.size() == ownGenerics.size()) {
				for (int i = 0; i < ownGenerics.size(); i++) {
					// invariant behavior for generic classes
					if (!ownGenerics.get(i).matchesTypeExactly(otherGenerics.get(i))) {
						return false;
					}
				}
				return true;
			}
		}
		return false;
	}

	@Override
	public int getTypeDist(TypeDeclaration other, int dist) {
		if (doTypeParamsMatch(other)) { // currently we only support invariance
			return super.getTypeDist(other, dist);
		} else { // but if the other one does not have any type parameters, super types of it could do so...
			if (other instanceof ClassDeclaration) {
				List<TypeDeclaration> superTypes = ((ClassDeclaration) other).getSuperClassDeclarations();
				int minScore = Integer.MAX_VALUE;
				for (TypeDeclaration superType : superTypes) {
					int score = getTypeDist(superType);
					if (score < minScore) {
						minScore = score;
					}
				}
				if ((dist < Integer.MAX_VALUE) && (minScore < Integer.MAX_VALUE)) {
					return dist + minScore;
				}
			}
		}
		return Integer.MAX_VALUE;
	}

	public ClassDeclaration getBaseClass() {
		return baseClass;
	}

}
