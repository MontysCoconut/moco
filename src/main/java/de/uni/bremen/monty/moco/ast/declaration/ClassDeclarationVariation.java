package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.ClassScope;
import de.uni.bremen.monty.moco.ast.ResolvableIdentifier;
import de.uni.bremen.monty.moco.ast.Scope;
import de.uni.bremen.monty.moco.ast.statement.Statement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ClassDeclarationVariation extends ClassDeclaration {

	private List<ClassDeclaration> concreteGenericTypes;

	public ClassDeclarationVariation(ClassDeclaration classDecl, ResolvableIdentifier identifier,
	        List<ClassDeclaration> concreteGenericTypes) {
		super(classDecl.getPosition(), identifier, classDecl.getSuperClassIdentifiers(), new Block(
		        classDecl.getBlock().getPosition()), classDecl.isAbstract(), classDecl.getAbstractGenericTypes());
		this.concreteGenericTypes = concreteGenericTypes;
		setParentNode(classDecl.getParentNode());
		ClassScope classScope = new ClassScope(classDecl.getScope().getParentScope());
		classScope.addParentClassScope((ClassScope) classDecl.getScope());
		setScope(classScope);
		classDecl.addVariation(this);
		Collection<ProcedureDeclaration> procedureDeclarations = mapFunctions(classDecl.getVirtualMethodTable());
		getVirtualMethodTable().addAll(procedureDeclarations);
		mapBlock(classDecl.getBlock());
		getBlock().setParentNode(this);
		ProcedureDeclaration defaultInitializer = mapFunction(classDecl.getDefaultInitializer());
		setDefaultInitializer(defaultInitializer);
		for (Declaration procedureDeclaration : getBlock().getDeclarations()) {
			if (!(procedureDeclaration instanceof ProcedureDeclaration)
			        || !((ProcedureDeclaration) procedureDeclaration).isDefaultInitializer()) {
				classScope.define(procedureDeclaration);
			}
		}
		classScope.define(defaultInitializer);
	}

	private void mapBlock(Block block) {
		for (Statement statement : block.getStatements()) {
			getBlock().addStatement(statement);
		}
		for (Declaration declaration : block.getDeclarations()) {

			if (declaration instanceof ProcedureDeclaration) {
				declaration = mapFunction((ProcedureDeclaration) declaration);
			} else if (declaration instanceof VariableDeclaration) {
				declaration = mapDeclaration((VariableDeclaration) declaration);
			}
			getBlock().addDeclaration(declaration);
		}
	}

	private Collection<ProcedureDeclaration> mapFunctions(List<ProcedureDeclaration> originalVirtualMethods) {
		ArrayList<ProcedureDeclaration> procedureDeclarations = new ArrayList<>();
		for (ProcedureDeclaration procedureDeclaration : originalVirtualMethods) {
			procedureDeclarations.add(mapFunction(procedureDeclaration));
		}
		return procedureDeclarations;
	}

	private ProcedureDeclaration mapFunction(ProcedureDeclaration procedureDeclaration) {
		ProcedureDeclaration funDecl;
		if (procedureDeclaration.isFunction()) {
			TypeDeclaration returnType = mapGenericType((procedureDeclaration).getReturnType());
			funDecl = new ConcreteProcDecl(this, procedureDeclaration, returnType);
		} else {
			funDecl = new ConcreteProcDecl(this, procedureDeclaration);
		}
		funDecl.getParameter().addAll(mapParameter(procedureDeclaration.getParameter(), funDecl));
		funDecl.setParentNode(this);
		funDecl.setScope(procedureDeclaration.getScope());
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

	private List<VariableDeclaration> mapParameter(List<VariableDeclaration> parameter, ProcedureDeclaration decl) {
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
}
