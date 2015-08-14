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
		classScope.addParentClassScope((ClassScope) classDecl.getScope());
		setScope(classScope);
		classDecl.addVariation(this);
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
		if (functionDeclaration.isFunction()) {
			TypeDeclaration returnType = mapGenericType((functionDeclaration).getReturnType());
			funDecl = new ConcreteProcDecl(this, functionDeclaration, returnType);
		} else {
			funDecl = new ConcreteProcDecl(this, functionDeclaration);
		}
		funDecl.getParameters().addAll(mapParameter(functionDeclaration.getParameters(), funDecl));
		funDecl.setParentNode(this);
		funDecl.setScope(functionDeclaration.getScope());
		return funDecl;
	}

	private TypeDeclaration mapGenericType(TypeDeclaration type) {
		if (type instanceof AbstractGenericType) {
			return mapAbstractToConcrete((AbstractGenericType) type);
		} else {
			return type;
		}
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
}
