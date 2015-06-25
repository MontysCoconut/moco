package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.visitor.BaseVisitor;

public class ConcreteGenericType extends TypeDeclaration {

	private ClassDeclaration decl;

	public ConcreteGenericType(ClassDeclaration decl) {
		super(decl.getPosition(), decl.getIdentifier());
		this.decl = decl;
		setScope(decl.getScope());
		setParentNode(decl.getParentNode());
	}

	@Override
	public void visit(BaseVisitor visitor) {
		// TODO
	}

	@Override
	public void visitChildren(BaseVisitor visitor) {

	}

	public ClassDeclaration getDecl() {
		return decl;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof ConcreteGenericType)) return false;

		ConcreteGenericType that = (ConcreteGenericType) o;

		if (!decl.equals(that.decl)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return decl.hashCode();
	}
}
