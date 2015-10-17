package de.uni.bremen.monty.moco.ast.declaration;

import java.util.ArrayList;

public class ConcreteProcDecl extends FunctionDeclaration {
	private final ClassDeclarationVariation variation;

	public ConcreteProcDecl(ClassDeclarationVariation variation, FunctionDeclaration abstractDecl) {
		super(abstractDecl.getPosition(), abstractDecl.getIdentifier(), abstractDecl.getBody(),
		        new ArrayList<VariableDeclaration>(), abstractDecl.getDeclarationType(),
		        abstractDecl.getReturnTypeIdentifier(), abstractDecl.isAbstract());
		this.variation = variation;
		setParentNode(variation);
		setVMTIndex(abstractDecl.getVMTIndex());
	}

	public ConcreteProcDecl(ClassDeclarationVariation variation, FunctionDeclaration abstractDecl,
	        TypeDeclaration returnType) {
		super(abstractDecl.getPosition(), abstractDecl.getIdentifier(), abstractDecl.getBody(),
		        new ArrayList<VariableDeclaration>(), abstractDecl.getDeclarationType(), returnType,
		        abstractDecl.isAbstract());
		this.variation = variation;
		setParentNode(variation);
		setVMTIndex(abstractDecl.getVMTIndex());
		setScope(abstractDecl.getScope());
	}

	@Override
	public ClassDeclarationVariation getDefiningClass() {
		return variation;
	}

}
