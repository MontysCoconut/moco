grammar Monty;

import lex;

compilationUnit
  : EndOfLine* moduleDeclaration EOF
  ;

moduleDeclaration
  : importLine* ((classDeclaration | statement) EndOfLine*)*
  ;

importLine
  : 'import' Identifier EndOfLine+
  ;

independentDeclaration
  : variableDeclaration (':=' expression)? EndOfLine
  | constantDeclaration (':=' expression)? EndOfLine
  | functionDeclaration
  | procedureDeclaration
  ;

classDeclaration
  : 'abstract'? 'class' ClassIdentifier ('inherits' typeList)?
    ':' EndOfLine
    Indent
        (memberDeclaration+ | 'pass' EndOfLine)
    Dedent
  ;

memberDeclaration
  : accessModifier independentDeclaration
  ;

accessModifier
  : modifier='#'
  | modifier='+'
  | modifier='-'
  | modifier='~'
  ;

variableDeclaration
  : type Identifier
  ;

constantDeclaration
  : type ConstantIdentifier
  ;

type
  : ClassIdentifier ('<' typeList '>')?
  ;

typeList
  : type (',' type)*
  ;

functionDeclaration
  : type
    (Identifier | 'operator' binaryOperation)
    Lparenthesis parameterList? Rparenthesis ':' EndOfLine
    statementBlock
  ;

procedureDeclaration
  : Identifier Lparenthesis parameterList? Rparenthesis ':' EndOfLine
    statementBlock
  ;

defaultParameter
  : variableDeclaration ':=' expression
  ;

parameterList
  : defaultParameter (',' defaultParameter)*
  | variableDeclaration (',' variableDeclaration)*  (',' defaultParameter)*
  ;

statementBlock
  : Indent
      (statement+ | 'pass' EndOfLine)
    Dedent
  ;

statement
  : whileStatement                                                  #whileStm
  | ifStatement                                                     #ifStm
  | tryStatement                                                    #tryStm
  | assignment                                                      #assignStm
  | compoundAssignment                                              #compoundAssign
  | independentDeclaration                                          #independentDeclStm
  | command='return' expression? EndOfLine                          #returnStm
  | command='raise' expression? EndOfLine                           #raiseStm
  | command='skip' EndOfLine                                        #skipStm
  | command='break' EndOfLine                                       #breakStm
  | functionCall EndOfLine                                          #funcCallStm
  | left=expression operator='.' right=functionCall EndOfLine       #MemberAccessStmt
  ;

/* while loop: The expression must be a condition (i.e. Boolean expression). */
whileStatement
  : 'while' expression ':' EndOfLine statementBlock
  ;

ifStatement
  : 'if' ifCondition=expression ':' EndOfLine thenBlock=statementBlock
    elif*
    ('else' ':' EndOfLine elseBlock=statementBlock)?
  ;

elif
  : 'elif' elifCondition=expression ':' EndOfLine elifBlock=statementBlock
  ;

tryStatement
  : 'try:' EndOfLine tryBlock=statementBlock
    ('handle' variableDeclaration? ':' EndOfLine handleBlock=statementBlock)+
  ;

assignment
  : left=expression ':=' right=expression EndOfLine
  ;

compoundAssignment
  : left=expression compoundSymbol right=expression EndOfLine
  ;

compoundSymbol
  : operator=('+=' | '-=' | '*=' | '/=' | '%=' | '^=')
  ;

functionCall
  : (ClassIdentifier | Identifier) '(' expressionList? ')'
  ;

expressionList
  : expression (',' expression)*
  ;

expression
  : primary
  | ifExprThen=expression 'if' ifExpCondition=expression 'else' ifExprElse=expression
  | functionCall
  | array=expression Lbracket index=expression Rbracket
  | left=expression accessOperator right=expression
  | <assoc=right> (plusMinusOperator | notOperator) singleExpression=expression
  | <assoc=right> left=expression powerOperator right=expression
  | left=expression dotOperator right=expression
  | left=expression plusMinusOperator right=expression
  | left=expression compareOperator right=expression
  | left=expression eqOperator right=expression
  | left=expression inOperator right=expression
  | left=expression andOperator right=expression
  | left=expression orOperator right=expression
  | expr=expression asOperator ClassIdentifier
  | expr=expression isOperator ClassIdentifier
  ;

primary
  : Lparenthesis singleExpression=expression Rparenthesis
  | literal
  | Identifier
  | ConstantIdentifier
  | 'self'
  | 'parent(' parent=ClassIdentifier ')'
  ;

inOperator
  : operator='in'
  ;

andOperator
  : operator='and'
  ;

orOperator
  : operator=('or' | 'xor')
  ;

asOperator
  : operator='as'
  ;

isOperator
  : operator='is'
  ;

eqOperator
  : operator=('=' | '!=')
  ;

compareOperator
  : operator=('<' | '>' | '>=' | '<=')
  ;

powerOperator
  : operator='^'
  ;

plusMinusOperator
  : operator=('+' | '-')
  ;

notOperator
  : operator='not'
  ;

accessOperator
  : operator=('.' | '->')
  ;

dotOperator
  : operator=('*' | '/' | '%')
  ;

literal
  : IntegerLiteral
  | RealLiteral
  | CharacterLiteral
  | StringLiteral
  | BooleanLiteral
  | arrayLiteral
  ;

arrayLiteral
  : Lbracket (expression (',' expression)*)? Rbracket
  ;
