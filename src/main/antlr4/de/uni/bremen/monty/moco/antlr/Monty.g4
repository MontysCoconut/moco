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

declaration
  : independentDeclaration
  | classDeclaration
  | generatorDeclaration
  ;

independentDeclaration
  : variableDeclaration (':=' expression)? EndOfLine
  | constantDeclaration (':=' expression)? EndOfLine
  | functionDeclaration
  ;

classDeclaration
  : AbstractKeyword? 'class' type ('inherits' typeList)?
    ':' EndOfLine
    Indent
        (memberDeclaration+ | 'pass' EndOfLine)
    Dedent
  ;

memberDeclaration
  : accessModifier? independentDeclaration
  | accessModifier? abstractMethodDeclaration
  ;

abstractMethodDeclaration
  : AbstractKeyword type? Identifier Lparenthesis parameterList? Rparenthesis EndOfLine
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

arrow
  : '->'
  ;

type
  : ClassIdentifier ('<' typeList '>')?
  | '(' (type (',' type)+)? ')'
  | type arrow type
  | '(' type arrow type ')'
  ;

typeList
  : type (',' type)*
  ;

functionDeclaration
  : (type)?
    Identifier
    Lparenthesis parameterList? Rparenthesis ':' EndOfLine
    statementBlock
  ;

generatorDeclaration
    : 'generator' type
      ClassIdentifier
      Lparenthesis parameterList? Rparenthesis ':' EndOfLine
      statementBlock
    ;

defaultParameter
  : variableDeclaration ':=' expression
  ;

parameterList
  : defaultParameter (',' defaultParameter)*
  | variableDeclaration (',' variableDeclaration)*  (',' defaultParameter)*
  ;

parameterListWithoutDefaults
  : variableDeclaration (',' variableDeclaration)*
  ;

statementBlock
  : Indent
      (statement+ | 'pass' EndOfLine)
    Dedent
  ;

statement
  : whileStatement                                                  #whileStm
  | forStatement                                                    #forStm
  | ifStatement                                                     #ifStm
  | tryStatement                                                    #tryStm
  | declaration                                                     #declStm
  | unpackAssignment                                                #unpackAssignStm
  | assignment                                                      #assignStm
  | compoundAssignment                                              #compoundAssign
  | command='return' expression? EndOfLine                          #returnStm
  | command='yield' expression? EndOfLine                           #yieldStm
  | command='raise' expression? EndOfLine                           #raiseStm
  | command='skip' EndOfLine                                        #skipStm
  | command='break' EndOfLine                                       #breakStm
  | functionCall EndOfLine                                          #funcCallStm
  | left=expression operator='.' right=functionCall EndOfLine       #MemberAccessStmt
  | caseStatement                                                   #caseStm
  ;

/* while loop: The expression must be a condition (i.e. Boolean expression). */
whileStatement
  : 'while' expression ':' EndOfLine statementBlock
  ;

/* for loop: The expression must be of type Iterable<T>. */
forStatement
  : 'for' Identifier 'in' expression ':' EndOfLine statementBlock
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

unpackAssignment
  : left=unpackList ':=' right=expression EndOfLine
  ;

unpackList
  : unpackable (',' unpackable)+
  ;

unpackable
  : expression
  | variableDeclaration
  ;

compoundAssignment
  : left=expression compoundSymbol right=expression EndOfLine
  ;

compoundSymbol
  : operator=('+=' | '-=' | '*=' | '/=' | '%=' | '^=')
  ;

functionCall
  : (type | Identifier) '(' expressionList? ')'
  ;

expressionList
  : expression (',' expression)*
  ;

expression
  : functionCall
  | primary
  | functionExpression
  | ifExprThen=expression 'if' ifExpCondition=expression 'else' ifExprElse=expression
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
  | expr=expression asOperator type
  | expr=expression isOperator ClassIdentifier
  | listComprehension
  ;

functionExpression
 : Lparenthesis parameterListWithoutDefaults? Rparenthesis arrow expression
 ;

primary
  : Lparenthesis singleExpression=expression Rparenthesis
  | literal
  | Identifier
  | ConstantIdentifier
  | 'self'
  | 'parent(' parent=type ')'
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
  : operator='.'
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
  | rangeLiteral
  | tupleLiteral
  ;

rangeLiteral
  : Lbracket expression '..' expression Rbracket
  ;

arrayLiteral
  : Lbracket (expression (',' expression)*)? Rbracket
  ;

tupleLiteral
  : Lparenthesis (expression (',' expression)+)? Rparenthesis
  ;

listComprehension
  : Lbracket type expression '|' listGenerator ( ',' listGenerator)? Rbracket
  ;

listGenerator
  : Identifier 'in' expression listFilter?
  ;

listFilter
  : 'if' expression
  ;


/* pattern matching */

caseStatement
  : 'case' expression 'of' ':' EndOfLine caseBlock
  ;

caseBlock
  : Indent
      (pattern ':' EndOfLine statementBlock)+
    Dedent
  ;

pattern
  : typedPattern
  | compoundPattern
  | '_'
  | expression
  ;

typedPattern
  : type Identifier
  | type '_'
  ;

compoundPattern
  : type? '(' parameterList? ')'
  ;

patternList
  : pattern (',' pattern )*
  ;