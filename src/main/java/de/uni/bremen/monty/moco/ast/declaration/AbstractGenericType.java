package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.ast.ASTNode;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.Position;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

public class AbstractGenericType extends TypeDeclaration {

	private ClassDeclaration definedIn;

	/** Constructor.
	 *
	 * @param definedIn
	 * @param position
	 *            Position of this node
	 * @param identifier
	 *            the identifier */
	public AbstractGenericType(ClassDeclaration definedIn, Position position, Identifier identifier) {
		super(position, identifier);
		this.definedIn = definedIn;
	}

	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	@Override
	public void visitChildren(BaseVisitor visitor) {

	}

	public ClassDeclaration getDefinedIn() {
		return definedIn;
	}

	public boolean equals(Object other) {
		if (other instanceof AbstractGenericType) {
			return ((AbstractGenericType) other).getIdentifier().equals(getIdentifier());
		}
		return false;
	}
}
