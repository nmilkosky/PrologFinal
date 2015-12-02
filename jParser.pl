%--------------------------Declarations-------------------------
parse(X) :- once(classDeclaration(X, [])).

classDeclaration --> classModifiers, [class], identifier, super, interfaces, classBody.
identifier --> [I], {atom(I)}.

classModifiers --> classModifier.
classModifiers --> classModifier, classModifiers.
classModifier --> [M], {class_modifier(M)}.
class_modifier(public).
class_modifier(abstract).
class_modifier(final).

super --> [].
super --> [extends], classpackageName.

interfaces --> [].
interfaces --> [implements], classpackageName.

classBody --> ['{'], classBodyDeclarations, ['}'].

classBodyDeclarations --> classBodyDeclaration.
classBodyDeclarations --> classBodyDeclaration, classBodyDeclarations.
classBodyDeclaration --> classMemberDeclaration, constructorDeclaration.
 
classMemberDeclaration --> fieldDeclaration.
classMemberDeclaration --> methodDeclaration.

staticInitializer --> [].
staticInitializer --> [static].

constructorDeclaration --> constructorModifier, constructorDeclarator, throws, constructorBody.
constructorDeclaration --> [].

constructorModifier --> [M], {constructor_modifier(M)}.
constructor_modifier(public).
constructor_modifier(protected).
constructor_modifier(private).

constructorDeclarator --> identifier, ['('], formalParameterList, [')'].

formalParameterList --> [].
formalParameterList --> formalParameter.
formalParameterList --> formalParameter, formalParameterList.
formalParameter --> type, identifier, [';'].
formalParameter --> arrayType, identifier, [';'].

throws --> [].
throws --> [throws], classpackageName.

constructorBody --> ['{'], constructorInvocation, blockStatements, ['}'].

constructorInvocation --> [].
constructorInvocation --> [this], ['('], arguementList, [')'], [';'].
constructorInvocation --> [super], ['('], arguementList, [')'], [';'].

fieldDeclaration --> [].
fieldDeclaration --> fieldModifiers, type, varDeclarators.

fieldModifiers --> fieldModifier.
fieldModifiers --> fieldModifier, fieldModifiers.
fieldModifier --> [M], {field_modifier(M)}.
field_modifier(public).
field_modifier(protected).
field_modifier(private).
field_modifier(static).
field_modifier(final).
field_modifier(transient).
field_modifier(volatile).

varDeclarators --> varDeclatator.
varDeclatators --> varDeclarators, varDeclatator.
varDeclatator --> vardecId, [';'].
varDeclatator --> vardecId, [=], varInit, [';'].

vardecId --> identifier.
vardecId --> identifier, [[]].

methodDeclaration --> [].
methodDeclaration --> methodHeader, methodBody.

methodHeader --> methodModifiers, staticInitializer, resultType, methodDeclarator, throws.

resultType --> type.
resultType --> [void].

methodModifiers --> [].
methodModifiers --> methodModifier.
methodModifiers --> methodModifier, methodModifiers.
methodModifier --> [M], {method_modifier(M)}.

method_modifier(public).
method_modifier(protected).
method_modifier(private).
method_modifier(static).
method_modifier(abstract).
method_modifier(final).
method_modifier(synchronized).
method_modifier(native).

methodDeclarator --> identifier, formalParameterList.

methodBody --> ['{'], block, ['}'].

varInit --> expression.
varInit --> arrayInit.

arrayInit --> ['{'], varInits, ['}'].

varInits --> varInit.
varInits --> varInit, varInits.

%---------------------------Types-----------------------------
type --> primitiveType.
type --> referenceType.

primitiveType --> numeric_type.
primitiveType --> [T], {primitive_type(T)}.
primitive_type(boolean).

numeric_type --> [T], {integral_type(T)}.
numeric_type --> [T], {floating_type(T)}.

integral_type(byte).
integral_type(short).
integral_type(int).
integral_type(long).
integral_type(char).

floating_type(float).
floating_type(double).

referenceType --> classpackageName.

arrayType --> type, [[]], multiDimension.
multiDimension --> [].
multiDimension --> [[]], multiDimension.

%--------------------------Blocks and Commands-------------------
block --> ['{'], block_statements, ['}'].

blockStatements --> blockStatement.
blockStatements --> blockStatements, blockStatement.

blockStatement --> [].
blockStatement --> localvardecStatement.
blockStatement --> statement.

localvardecStatement --> localvardec, [';'].
localvardec --> type, varDeclarators.

statement --> simpleStatement.
statement --> labeledStatement.
statement --> ifStatement.
statement --> if_elseStatement.
statement --> whileStatement.
statement --> forStatement.

%left out synchronized statement
simpleStatement --> block.
simpleStatement --> emptyStatement.
simpleStatement --> expressionStatement. %dependent on expressions.pl
simpleStatement --> switchStatement.
simpleStatement --> doStatement.
simpleStatement --> breakStatement.
simpleStatement --> continueStatement.
simpleStatement --> returnStatement.
simpleStatement --> continueStatement.
simpleStatement --> throwsStatement.
simpleStatement --> tryStatement.

emptyStatement --> [;].

labeledStatement --> identifier, [':'], statement.

expressionStatement --> exprStatement,[';'].

exprStatement --> assignmentExpression.
exprStatement --> preIncrement_expr.
exprStatement --> postIncrement_expr.
exprStatement --> preDecrement_expr.
exprStatement --> postDecrement_expr.
exprStatement --> method_invocation.
exprStatement --> create_class_instance_expr.

%Allow optional brackets (add new rules?)
ifStatement --> [if], ['('], expression, [')'], statement.
if_elseStatement --> [if], ['('], expression, [')'], statement, [else], statement.

switchStatement --> [switch], ['('], expression, [')'], switchBlock.
switchBlock --> ['{'], switchGroups, switchLabels, ['}'].
switchBlock --> ['{'], switchGroups, ['}'].
switchBlock --> ['{'], switchLabels, ['}'].
switchBlock --> ['{'], ['}'].

switchGroups --> switchLabels, blockStatements.

switchLabels --> switchLabel.
switchLabels --> switchLabels, switchLabel.

switchLabel --> [case], constantExpression, [':'].
switchLabel --> [default], [':'].

whileStatement --> [while], ['('], expression, [')'], statement.

doStatement --> [do], statement, [while], expression, [';'].

%Are expression and for_counter optional?
forStatement --> [for], ['('], forInit, [';'], expression, [';'], forCounter, [')'], statement.
forStatement --> [for], ['('], forInit, [';'], expression, [')'], statement.
forStatement --> [for], ['('], forInit, [';'], forCounter, [')'], statement.
forStatement --> [for], ['('], expression, [';'], forCounter, [')'], statement.
forStatement --> [for], ['('], forInit, [')'], statement.
forStatement --> [for], ['('], expression, [')'], statement.
forStatement --> [for], ['('], forCounter, [')'], statement.
forStatement --> [for], ['('], [')'], statement.

forInit --> exprStatements.
forInit --> localvardec.

exprStatements --> exprStatement.
exprStatements --> exprStatements, exprStatement.

breakStatement --> [break], [';'].
breakStatement --> [break], identifier, [';'].

continuteStatement --> [continue], [';'].
continuteStatement --> [continue], identifier, [';'].

returnStatement --> [return], [';'].
returnStatement --> [return], expression, [';'].

throwsStatement --> [throw], expression, [';'].

tryStatement --> [try], block, catches.
tryStatement --> [try], block, finally.
catches --> catch_clause.
catches --> catches, catchClause.
catchClause --> [catch], ['('], formalParameter, [')'], block.
finally --> [finally], block.

%----------------------------Expressions--------------------
constantExpression --> expression.

expression --> assignmentExpression.

assignmentExpression --> conditionalExpression.
assignmentExpression --> assignment.

assignment --> leftSide, assignmentOper, assignmentExpression.

leftSide --> expression_name.
leftSide --> fieldAccess.
leftSide --> arrayAccess.

assignmentOper  --> [AOper], {assignment_operator(AOper)}.
assignment_operator('=').
assignment_operator('*=').
assignment_operator('/=').
assignment_operator('%=').
assignment_operator('+=').
assignment_operator('-=').
assignment_operator('<<=').
assignment_operator('>>=').
assignment_operator('>>>=').
assignment_operator('&=').
assignment_operator('^=').
assignment_operator('|=').

conditionalExpression --> conditionalOrExpression.
conditionalExpression --> conditionalOrExpression, ['?'], expression, [':'], conditionalExpression.

conditionalOrExpression --> conditionalAndExpress.
conditionalOrExpression --> conditionalOrExpression, ['||'], conditionalAndExpress.

conditionalAndExpress --> inclusiveOrExpression.
conditionalAndExpress --> conditionalAndExpress, ['&&'], inclusiveOrExpression.

inclusiveOrExpression --> exclusiveOrExpression.
inclusiveOrExpression --> inclusiveOrExpression, ['|'], exclusiveOrExpression.

exclusiveOrExpression --> andExpression.
exclusiveOrExpression --> exclusiveOrExpression, ['^'], andExpression.

andExpression --> equalityExpression.
andExpression --> andExpression, ['&'],  equalityExpression.

equalityExpression --> relationalExpression.
equalityExpression --> equalityExpression, ['=='], relationalExpression.
equalityExpression --> equalityExpression, ['!='], relationalExpression.

relationalExpression --> shiftExpression.
relationalExpression --> relationalExpression, ['<'], shiftExpression.
relationalExpression --> relationalExpression, ['>'], shiftExpression.
relationalExpression --> relationalExpression, ['<='], shiftExpression.
relationalExpression --> relationalExpression, ['>='], shiftExpression.
relationalExpression --> relationalExpression, ['instanceof'], referenceType.

shiftExpression --> additiveExpression.
shiftExpression --> shiftExpression, ['<<'], additiveExpression.
shiftExpression --> shiftExpression, ['>>'], additiveExpression.
shiftExpression --> shiftExpression, ['>>>'], additiveExpression.

additiveExpression --> multiplicExpression.
additiveExpression --> additiveExpression, ['+'], multiplicExpression.
additiveExpression --> additiveExpression, ['-'], multiplicExpression.

multiplicExpression --> unaryExpression.
multiplicExpression --> multiplicExpression, ['*'], unaryExpression.
multiplicExpression --> multiplicExpression, ['/'], unaryExpression.
multiplicExpression --> multiplicExpression, ['%'], unaryExpression.

castExpression --> ['('], primitiveType, [')'], unaryExpression.
castExpression --> ['('], referenceType, [')'], unaryExpressionNotPM.

unaryExpression --> preincrementExpression.
unaryExpression --> predecrementExpression.
unaryExpression --> ['+'], unaryExpression.
unaryExpression --> ['-'], unaryExpression.
unaryExpression --> unaryExpressionNotPM.

predecrementExpression --> ['--'], unaryExpression.

preincrementExpression --> ['++'], unaryExpression.


unaryExpressionNotPM --> postfixExpression.
unaryExpressionNotPM --> ['~'], unaryExpression.
unaryExpressionNotPM --> ['!'], unaryExpression.
unaryExpressionNotPM --> castExpression.

postdecrementExpression --> postfixExpression, ['--'].

postincrementExpression --> postfixExpression, ['++'].

postfixExpression --> primary.
postfixExpression --> expression_name.
postfixExpression --> postdecrementExpression.
postfixExpression --> postincrementExpression.

methodInvocation --> methodName, ['('], arguementList, [')'].
methodInvocation --> primary, ['.'], identifier, ['('], arguementList, [')'].
methodInvocation --> ['super'], identifier, ['('], arguementList, [')'].

fieldAccess --> primary, ['.'], identifier.
fieldAccess --> ['super'], ['.'], identifier.

primary --> primaryNoNewArray, !.
primary --> arrayCreateExpr.

primaryNoNewArray --> literal.
primaryNoNewArray --> ['this'].
primaryNoNewArray --> ['('], expression, [')'].
primaryNoNewArray --> classInstCreateExpr.
primaryNoNewArray --> fieldAccess.
primaryNoNewArray --> methodInvocation.
primaryNoNewArray --> arrayAccess.

classInstCreateExpr --> ['new'], classType, ['('], arguementList, [')'].

arguementList --> [].
arguementList --> expression.
arguementList --> arguementList, [','], expression.

arrayCreateExpr --> ['new'], primitiveType, dimExprs, dims.
arrayCreateExpr --> ['new'], classInterType, dimExprs, dims.

dimExprs --> dimExpr.
dimExprs --> dimExprs, dimExpr.

dimExpr --> ['['], expression, [']'].

dims --> [].
dims --> ['[]'].
dims --> dims, ['[]'].

arrayAccess --> expression_name, ['['], expression, [']'].
arrayAccess --> primaryNoNewArray, ['['], expression, [']'].

%---------Tokens------------
classpackageName --> typeName, identifier.

typeName --> [I], {string(I)}.

simple_type --> identifier.

expression_name --> identifier.
expression_name --> ambiguous_name, identifier.

method_name --> identifier.
method_name --> ambiguous_name, identifier.

ambiguous_name --> identifier.
ambiguous_name --> ambiguous_name, identifier.

literal --> integer_literal.
literal --> floating_literal.
literal --> boolean_literal.
literal --> character_literal.
literal --> string_literal.
literal --> null_literal.

integer_literal --> decimal_int_literal.

decimal_int_literal --> decimal_numeral.

decimal_numeral --> [I], {integer(I)}.

floating_literal --> [I], {float(I)}.

boolean_literal --> [true].
boolean_literal --> [false].

character_literal --> [I], {atom(I)}.

string_literal --> [S], {string(S)}.

null_literal --> [null].

% Not sure if this is important
keyword --> [abstract].
keyword --> [char].
keyword --> [double].
keyword --> [for].
keyword --> [int].
keyword --> [private].
keyword --> [super].
keyword --> [transient].
keyword --> [boolean].
keyword --> [class].
keyword --> [else].
keyword --> [goto].
keyword --> [interface].
keyword --> [protected].
keyword --> [switch].
keyword --> [try].
keyword --> [break].
keyword --> [const].
keyword --> [extends].
keyword --> [if].
keyword --> [long].
keyword --> [public].
keyword --> [synchronized].
keyword --> [void].
keyword --> [byte].
keyword --> [continue].
keyword --> [final].
keyword --> [implements].
keyword --> [native].
keyword --> [return].
keyword --> [short].
keyword --> [this].
keyword --> [volatile].
keyword --> [case].
keyword --> [default].
keyword --> [finally].
keyword --> [float].
keyword --> [import].
keyword --> [instanceof].
keyword --> [new].
keyword --> [static].
keyword --> [throw].
keyword --> [while].
keyword --> [catch].
keyword --> [do].
keyword --> [package].
keyword --> [throws].
