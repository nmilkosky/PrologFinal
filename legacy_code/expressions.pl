constantExpression 	--> expression.

expression		--> assignmentExpression.

assignmentExpression	--> conditionalExpression.
assignmentExpression	--> assignment.

assignment		--> leftSide, assignmentOper, assignmentExpression.

leftSide		--> expressionName.
leftSide		--> fieldAccess.
leftSide		--> arrayAccess.

assignmentOper		--> [AOper], {assignment_operator(AOper)}.
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

conditionalExpression	--> conditionalOrExpression.
conditionalExpression	--> conditionalOrExpression, ['?'], expression, [':'], conditionalExpression.

conditionalOrExpression	--> conditionalAndExpress.
conditionalOrExpression	--> conditionalOrExpression, ['||'], conditionalAndExpress.

conditionalAndExpress	--> inclusiveOrExpression.
conditionalAndExpress	--> conditionalAndExpress, ['&&'], inclusiveOrExpression.

inclusiveOrExpression	--> exclusiveOrExpression.
inclusiveOrExpression	--> inclusiveOrExpression, ['|'], exclusiveOrExpression.

exclusiveOrExpression	--> andExpression.
exclusiveOrExpression	--> exclusiveOrExpression, ['^'], andExpression.

andExpression			-->	equalityExpression.
andExpression			--> andExpression, ['&'],  equalityExpression.

equalityExpression		--> relationalExpression.
equalityExpression		--> equalityExpression, ['=='], relationalExpression.
equalityExpression		--> equalityExpression, ['!='], relationalExpression.

relationalExpression	--> shiftExpression.
relationalExpression	--> relationalExpression, ['<'], shiftExpression.
relationalExpression	--> relationalExpression, ['>'], shiftExpression.
relationalExpression	--> relationalExpression, ['<='], shiftExpression.
relationalExpression	--> relationalExpression, ['>='], shiftExpression.
relationalExpression	--> relationalExpression, ['instanceof'], referenceType.

shiftExpression			--> additiveExpression.
shiftExpression			--> shiftExpression, ['<<'], additiveExpression.
shiftExpression			--> shiftExpression, ['>>'], additiveExpression.
shiftExpression			--> shiftExpression, ['>>>'], additiveExpression.

additiveExpression		--> multiplicExpression.
additiveExpression		--> additiveExpression, ['+'], multiplicExpression.
additiveExpression		--> additiveExpression, ['-'], multiplicExpression.

multiplicExpression		--> unaryExpression.
multiplicExpression		--> multiplicExpression, ['*'], unaryExpression.
multiplicExpression		--> multiplicExpression, ['/'], unaryExpression.
multiplicExpression		--> multiplicExpression, ['%'], unaryExpression.

castExpression			--> ['('], primitiveType, [')'], unaryExpression.
castExpression			--> ['('], referenceType, [')'], unaryExpressionNotPM.

unaryExpression			--> preincrementExpression.
unaryExpression			--> predecrementExpression.
unaryExpression			--> ['+'], unaryExpression.
unaryExpression			--> ['-'], unaryExpression.
unaryExpression			--> unaryExpressionNotPM.

predecrementExpression	--> ['--'], unaryExpression.

preincrementExpression	--> ['++'], unaryExpression.


unaryExpressionNotPM	--> postfixExpression.
unaryExpressionNotPM	--> ['~'], unaryExpression.
unaryExpressionNotPM	--> ['!'], unaryExpression.
unaryExpressionNotPM	--> castExpression.

postdecrementExpression	--> postfixExpression, ['--'].

postincrementExpression	--> postfixExpression, ['++'].

postfixExpression		--> primary.
postfixExpression		--> expressionName.
postfixExpression		--> postdecrementExpression.
postfixExpression		--> postincrementExpression.

methodInvocation		--> methodName, ['('], arguementList, [')'].
methodInvocation		--> primary, ['.'], identifier, ['('], arguementList, [')'].
methodInvocation		--> ['super'], identifier, ['('], arguementList, [')'].

fieldAccess				--> primary, ['.'], identifier.
fieldAccess				--> ['super'], ['.'], identifier.

primary					--> primaryNoNewArray.
primary					--> arrayCreateExpr.

primaryNoNewArray		--> literal.
primaryNoNewArray		--> ['this'].
primaryNoNewArray		--> ['('], expression, [')'].
primaryNoNewArray		--> classInstCreateExpr.
primaryNoNewArray		--> fieldAccess.
primaryNoNewArray		--> methodInvocation.
primaryNoNewArray		--> arrayAccess.

classInstCreateExpr		--> ['new'], classType, ['('], arguementList, [')'].

arguementList			--> [].
arguementList			--> expression.
arguementList			--> arguementList, [','], expression.

arrayCreateExpr			--> ['new'], primitiveType, dimExprs, dims.
arrayCreateExpr			--> ['new'], classInterType, dimExprs, dims.

dimExprs				--> dimExpr.
dimExprs				--> dimExprs, dimExpr.

dimExpr					--> ['['], expression, [']'].

dims					--> [].
dims 					--> ['[]'].
dims					--> dims, ['[]'].

arrayAccess				--> expressionName, ['['], expression, [']'].
arrayAccess				--> primaryNoNewArray, ['['], expression, [']'].

