package de.uni.bremen.monty.moco.ast.declaration;

import java.util.ArrayList;

public class ConcreteProcDecl extends ProcedureDeclaration {
	private final ClassDeclarationVariation variation;

	public ConcreteProcDecl(ClassDeclarationVariation variation, ProcedureDeclaration abstractDecl) {
		super(abstractDecl.getPosition(), abstractDecl.getIdentifier(), abstractDecl.getBody(),
		        new ArrayList<VariableDeclaration>(), abstractDecl.getDeclarationType(), (TypeDeclaration) null);
		this.variation = variation;
		setParentNode(variation);
		setVMTIndex(abstractDecl.getVMTIndex());
	}

	public ConcreteProcDecl(ClassDeclarationVariation variation, ProcedureDeclaration abstractDecl,
	        TypeDeclaration returnType) {
		super(abstractDecl.getPosition(), abstractDecl.getIdentifier(), abstractDecl.getBody(),
		        new ArrayList<VariableDeclaration>(), abstractDecl.getDeclarationType(), returnType);
		this.variation = variation;
		setParentNode(variation);
		setVMTIndex(abstractDecl.getVMTIndex());
	}

	@Override
	public ClassDeclarationVariation getDefiningClass() {
		return variation;
	}

}
