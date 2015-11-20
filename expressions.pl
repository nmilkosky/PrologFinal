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
inclusiveOrExpression	--> inclusiveOrExpression, ['|'], exclusiveOrExpresion.


