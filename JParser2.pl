/** 
* File Processor:
* Reads in a file called input.txt. Parses the input into atoms and stores them in a list
* using a whitespace, tabs, and new lines as delimeters.
* 
* Usage: Generate a list for your Java source file with query: ?- parse_file.
*
*/

parse_file :-
 	%Opens a file or throws a file not found exception
 	catch(open('input.txt', read, Input), E, (write('Could not find file.'),fail)), 
    read_file(Input, Lines),
    close(Input),
    atomic_list_concat(Lines, ' ', Atom), %handles new lines
    atom_string(Atom, String),
    normalize_space(atom(Out), String), %handles whitespace for tab characters
    atomic_list_concat(Split, ' ', Out), 
    PasstoParse = writeq(Split), %Atoms that need quotes are quoted. 
    nl,
    parse(Split).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.
    
%--------------------------Declarations-------------------------
parse(X) :- once(classDeclaration(X, [])).

classDeclaration --> classModifiers, [class], identifier, classBody.
classDeclaration --> classModifiers, [class], identifier, super, classBody.
classDeclaration --> classModifiers, [class], identifier, interfaces, classBody.
classDeclaration --> classModifiers, [class], identifier, super, interfaces, classBody.
identifier --> [I], {atom(I)}.

classModifiers --> classModifier.
classModifiers --> classModifier, classModifiers.
classModifier --> [M], {class_modifier(M)}.
class_modifier(public).
class_modifier(abstract).
class_modifier(final).

super --> [extends], classType.
	
interfaces --> [implements], interfaceTypeList.

classBody --> classBlock.

classBlock --> ['{'], ['}'].
classBlock --> ['{'], classBodyDeclarations, ['}'].

classBodyDeclarations --> classBodyDeclaration.
classBodyDeclarations --> classBodyDeclaration, classBodyDeclarations.

classBodyDeclaration --> classMemberDeclaration.
classBodyDeclaration --> constructorDeclaration.
 
classMemberDeclaration --> fieldDeclaration.
classMemberDeclaration --> methodDeclaration.

staticInitializer --> [static].

constructorDeclaration --> constructorModifier, constructorDeclarator, constructorBody.
constructorDeclaration --> constructorModifier, constructorDeclarator, throws, constructorBody.

constructorModifier --> [M], {constructor_modifier(M)}.
constructor_modifier(public).
constructor_modifier(protected).
constructor_modifier(private).

constructorDeclarator --> identifier, ['('], [')'].
constructorDeclarator --> identifier, ['('], formalParameterList, [')'].

formalParameterList --> formalParameter.
formalParameterList --> formalParameter, [','], formalParameterList.
formalParameter --> type, identifier.
formalParameter --> arrayType, identifier.

throws --> [throws], classTypeList.

constructorBlock --> ['{'], ['}'].
constructorBlock --> ['{'], constructorBody, ['}'].

constructorBody --> constructorInvocation.
constructorBody --> blockStatements.
constructorBody --> constructorInvocation, blockStatements.

constructorInvocation --> [this], ['('], [')'], [';'].
constructorInvocation --> [this], ['('], argumentList, [')'], [';'].
constructorInvocation --> [super], ['('], [')'], [';'].
constructorInvocation --> [super], ['('], argumentList, [')'], [';'].

fieldDeclaration --> fieldModifiers, type, varDeclarators, [';'].

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
varDeclatators --> varDeclarator, varDeclatators.
varDeclatator --> vardecId.
varDeclatator --> vardecId, ['='], varInit.

vardecId --> identifier.
vardecId --> identifier, [[]].

methodDeclaration --> methodHeader, methodBody.

methodHeader --> methodModifiers, resultType, methodDeclarator.
methodHeader --> methodModifiers, resultType, methodDeclarator, throws.
methodHeader --> methodModifiers, staticInitializer, resultType, methodDeclarator.
methodHeader --> methodModifiers, staticInitializer, resultType, methodDeclarator, throws.

resultType --> type.
resultType --> [void].

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

methodDeclarator --> identifier, ['('], [')'].
methodDeclarator --> identifier, ['('], formalParameterList, [')'].

methodBody --> block.

varInit --> expression.
varInit --> arrayInit.

arrayInit --> ['{'], varInits, ['}'].

varInits --> varInit.
varInits --> varInits, varInit.

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

arrayType --> type, [[]].
arrayType --> type, [[]], multiDimension.
multiDimension --> [[]].
multiDimension --> [[]], multiDimension.

%--------------------------Blocks and Commands-------------------
block --> ['{'], ['}'].
block --> ['{'], blockStatements, ['}'].

blockStatements --> blockStatement.
blockStatements --> blockStatement, blockStatements.

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

emptyStatement --> [';'].

labeledStatement --> identifier, [':'], statement.

expressionStatement --> exprStatement,[';'].

exprStatement --> expression.
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
switchLabels --> switchLabel, switchLabels.

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
exprStatements --> exprStatement, exprStatements.

breakStatement --> [break], [';'].
breakStatement --> [break], identifier, [';'].

continueStatement --> [continue], [';'].
continueStatement --> [continue], identifier, [';'].

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

leftSide --> initVar.
leftSide --> expression_name.
leftSide --> fieldAccess.
leftSide --> arrayAccess.

initVar --> type, expression_name.

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

conditionalExpression --> equalityExpression.
conditionalExpression --> identifier, ['||'], conditionalExpression.
conditionalExpression --> identifier, ['&&'], conditionalExpression.

equalityExpression --> relationalExpression.
equalityExpression --> identifier, ['=='], equalityExpression.
equalityExpression --> identifier, ['!='], equalityExpression.

relationalExpression --> additiveExpression.
relationalExpression --> identifier, ['<'], relationalExpression.
relationalExpression --> identifier, ['>'], relationalExpression.
relationalExpression --> identifier, ['<='], relationalExpression.
relationalExpression --> identifier, ['>='], relationalExpression.
relationalExpression --> identifier, ['instanceof'], relationalExpression.

additiveExpression --> multiplicExpression.
additiveExpression --> identifier, ['+'], additiveExpression.
additiveExpression --> identifier, ['-'], additiveExpression.

multiplicExpression --> unaryExpression.
multiplicExpression --> identifier, ['*'], multiplicExpression.
multiplicExpression --> identifier, ['/'], multiplicExpression.
multiplicExpression --> identifier, ['%'], multiplicExpression.

castExpression --> ['('], primitiveType, [')'], unaryExpression.
castExpression --> ['('], referenceType, [')'], unaryExpressionNotPM.

unaryExpression --> unaryExpressionNotPM.
unaryExpression --> preincrementExpression.
unaryExpression --> predecrementExpression.


predecrementExpression --> ['--'], identifier.
preincrementExpression --> ['++'], identifier.


unaryExpressionNotPM --> postfixExpression.
unaryExpressionNotPM --> castExpression.

postdecrementExpression --> identifier, ['--'].
postincrementExpression --> identifier, ['++'].

postfixExpression --> methodInvocation.
postfixExpression --> postdecrementExpression.
postfixExpression --> postincrementExpression.
postfixExpression --> expression_name.

methodInvocation --> fieldAccess.
methodInvocation --> identifier, ['('], [')'].
methodInvocation --> identifier, ['('], argumentList, [')'].
methodInvocation --> ['super'], ['('], [')'].
methodInvocation --> ['super'], ['('], argumentList, [')'].
methodInvocation --> identifier, ['.'], methodInvocation.

fieldAccess --> primary.
fieldAccess --> identifier, ['.'], identifier.
fieldAccess --> identifier, ['.'], fieldAccess.

primary --> primaryNoNewArray.
primary --> arrayCreateExpr.

primaryNoNewArray --> literal.
primaryNoNewArray --> ['this'].
primaryNoNewArray --> ['('], expression, [')'].
primaryNoNewArray --> classInstCreateExpr.
%primaryNoNewArray --> fieldAccess.
%primaryNoNewArray --> methodInvocation.
%primaryNoNewArray --> arrayAccess.

classInstCreateExpr --> ['new'], classType, ['('], [')'].
classInstCreateExpr --> ['new'], classType, ['('], argumentList, [')'].

argumentList --> identifier.
argumentList --> identifier, [','], argumentList.

arrayCreateExpr --> ['new'], primitiveType, dimExprs.
arrayCreateExpr --> ['new'], primitiveType, dimExprs, dims.
arrayCreateExpr --> ['new'], classInterType, dimExprs.
arrayCreateExpr --> ['new'], classInterType, dimExprs, dims.

dimExprs --> dimExpr.
dimExprs --> dimExpr, dimExprs.

dimExpr --> ['['], expression, [']'].

dims --> ['[]'].
dims --> ['[]'], dims.

arrayAccess --> expression_name, ['['], expression, [']'].
%arrayAccess --> primaryNoNewArray, ['['], expression, [']'].

%---------Tokens------------
classpackageName --> typeName.

classTypeList --> classType.
classTypeList --> classType, classTypeList.
classType --> typeName.

interfaceTypeList --> interfaceType.
interfaceTypeList --> interfaceType, interfaceTypeList.
interfaceType --> typeName.

typeName --> [I], {atom(I)}.

simple_type --> identifier.

expression_name --> identifier.
%expression_name --> ambiguous_name, identifier.

method_name --> identifier.
%method_name --> ambiguous_name, identifier.

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
