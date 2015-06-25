package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.ResolvableIdentifier;
import de.uni.bremen.monty.moco.ast.statement.Statement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ClassDeclarationVariation extends ClassDeclaration {

	private List<ConcreteGenericType> concreteGenericTypes;

	public ClassDeclarationVariation(ClassDeclaration classDecl, ResolvableIdentifier identifier,
	        List<ConcreteGenericType> concreteGenericTypes) {
		super(classDecl.getPosition(), identifier, classDecl.getSuperClassIdentifiers(), new Block(
		        classDecl.getBlock().getPosition()), classDecl.getAbstractGenericTypes());
		this.concreteGenericTypes = concreteGenericTypes;
		setParentNode(classDecl.getParentNode());
		setScope(classDecl.getScope());
		classDecl.addVariation(this);
		getVirtualMethodTable().addAll(mapFunctions(classDecl.getVirtualMethodTable()));
		mapBlock(classDecl.getBlock());
		getBlock().setParentNode(this);
		setDefaultInitializer(mapFunction(classDecl.getDefaultInitializer()));
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

	public ProcedureDeclaration mapFunction(ProcedureDeclaration procedureDeclaration) {
		ProcedureDeclaration funDecl;
		if (procedureDeclaration instanceof FunctionDeclaration) {
			TypeDeclaration returnType = mapGenericType(((FunctionDeclaration) procedureDeclaration).getReturnType());
			funDecl = new ConcreteFunDecl(this, (FunctionDeclaration) procedureDeclaration, returnType);
		} else {
			funDecl = new ConcreteProcDecl(this, procedureDeclaration);
		}
		funDecl.getParameter().addAll(mapParameter(procedureDeclaration.getParameter(), funDecl));
		funDecl.setParentNode(this);
		return funDecl;
	}

	public TypeDeclaration mapGenericType(TypeDeclaration type) {
		if (type instanceof AbstractGenericType) {
			return mapAbstractToConcrete((AbstractGenericType) type).getDecl();
		} else {
			return type;
		}
	}

	public ConcreteGenericType mapAbstractToConcrete(AbstractGenericType type) {
		int index = getAbstractGenericTypes().indexOf(type);
		if (index >= 0) {
			return concreteGenericTypes.get(index);
		} else {
			throw new RuntimeException("is this really an Error?");
		}
	}

	public List<ConcreteGenericType> getConcreteGenericTypes() {
		return concreteGenericTypes;
	}

	private List<VariableDeclaration> mapParameter(List<VariableDeclaration> parameter, ProcedureDeclaration decl) {
		ArrayList<VariableDeclaration> params = new ArrayList<>();
		for (VariableDeclaration variableDeclaration : parameter) {
			VariableDeclaration var;
			TypeDeclaration abstractType = variableDeclaration.getType();
			if (abstractType instanceof AbstractGenericType) {
				ClassDeclaration type = mapAbstractToConcrete((AbstractGenericType) abstractType).getDecl();
				var =
				        new VariableDeclaration(variableDeclaration.getPosition(), variableDeclaration.getIdentifier(),
				                type, variableDeclaration.getDeclarationType());
				var.setParentNode(decl);
			} else {
				var = variableDeclaration;
			}
			params.add(var);
		}
		return params;
	}

	public Declaration mapDeclaration(VariableDeclaration declaration) {
		VariableDeclaration variableDeclaration =
		        new VariableDeclaration(declaration.getPosition(), declaration.getIdentifier(),
		                mapGenericType(declaration.getType()), declaration.getDeclarationType());
		variableDeclaration.setParentNode(this);
		return variableDeclaration;
	}
}
