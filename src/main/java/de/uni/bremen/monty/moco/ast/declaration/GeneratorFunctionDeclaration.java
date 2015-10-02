package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.Position;
import de.uni.bremen.monty.moco.ast.ResolvableIdentifier;
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.ast.statement.YieldStatement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GeneratorFunctionDeclaration extends FunctionDeclaration {
	private List<YieldStatement> yieldStatements = new ArrayList<>();
	private List<VariableDeclaration> varDecls = new ArrayList<>();
	private int attributeIndex = 0;

	public GeneratorFunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, ResolvableIdentifier returnTypeIdentifier) {
		super(position, identifier, body, parameters, DeclarationType.METHOD, new ResolvableIdentifier("Maybe",
		        Arrays.asList(returnTypeIdentifier)));
		// the last statement inside a generator is always "return Nothing<Type>()"
		body.addStatement(new ReturnStatement(position, new FunctionCall(position, new ResolvableIdentifier("Nothing",
		        Arrays.asList(returnTypeIdentifier)), new ArrayList<Expression>())));
	}

	public List<YieldStatement> getYieldStatements() {
		return yieldStatements;
	}

	public void addYieldStatement(YieldStatement yield) {
		if (!yieldStatements.contains(yield)) {
			yieldStatements.add(yield);
		}
	}

	public List<VariableDeclaration> getVariableDeclarations() {
		return varDecls;
	}

	public void addVariableDeclaration(VariableDeclaration varDecl) {
		if (!varDecls.contains(varDecl)) {
			varDecls.add(varDecl);
			attributeIndex++;
			varDecl.setAttributeIndex(attributeIndex);
		}
	}
}
