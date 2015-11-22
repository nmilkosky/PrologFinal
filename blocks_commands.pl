<block> --> ['{'], block_statements, ['}']

block_statements --> block_statement.
block_statements --> block_statements, block_statement.

block_statement --> local_var_dec_statement.
block_statement --> statement. 

local_var_dec_statement --> local_var_dec, [';'].
local_var_dec --> type, var_declarators.

statement --> simple_statement.
statement --> labeled_statement.
statement --> if_statement.
statement --> if_else_statement.
statement --> while_statement.
statement --> for_statement.

%left out synchronized statement
simple_statement --> block. 
simple_statement --> empty_statement.
simple_statement --> expression_statement. %dependent on expressions.pl
simple_statement --> switch_statement. 
simple_statement --> do_statement. 
simple_statement --> break_statement. 
simple_statement --> continue_statement. 
simple_statement --> return_statement. 
simple_statement --> continue_statement. 
simple_statement --> throws_statement.
simple_statement --> try_statement.

empty_statement --> [;].

labeled_statement --> identifier, [':'], statement.

expression_statement --> expr_statement,[';'].

expr_statement --> assignmentExpression.
expr_statement --> preIncrement_expr.
expr_statement --> postIncrement_expr.
expr_statement --> preDecrement_expr.
expr_statement --> postDecrement_expr.
expr_statement --> method_invocation.
expr_statement --> create_class_instance_expr.

%Allow optional brackets (add new rules)
if_statement --> [if], ['('], expression, [')'], statement.
if_else_statement --> [if], ['('], expression, [')'], statement, [else], statement.

switch_statement --> [switch], ['('], expression, [')'], switch_block.
switch_block --> ['{'], switch_groups, switch_labels, ['}'].
switch_block --> ['{'], switch_groups, ['}'].
switch_block --> ['{'], switch_labels, ['}'].
switch_block --> ['{'], ['}'].

switch_groups --> switch_labels, block_statements.

switch_labels --> switch_label.
switch_labels --> switch_labels, switch_label.

switch_label --> [case], constant_expression, [':'].
switch_label --> [default], [':'].

while_statement --> [while], ['('], expression, [')'], statement.

do_statement --> [do], statement, [while] expression, [';'].

%Are expression and for_counter optional?
for_statement --> [for], ['('], for_init, [';'], expression, [';'], for_counter, [')'], statement. 
for_statement --> [for], ['('], for_init, [';'], expression, [')'], statement. 
for_statement --> [for], ['('], for_init, [';'], for_counter, [')'], statement. 
for_statement --> [for], ['('], expression, [';'], for_counter, [')'], statement. 
for_statement --> [for], ['('], for_init, [')'], statement. 
for_statement --> [for], ['('], expression, [')'], statement. 
for_statement --> [for], ['('], for_counter, [')'], statement. 
for_statement --> [for], ['('], [')'], statement. 

for_init --> expr_statements.
for_init --> local_var_dec.

expr_statements --> expr_statement. 
expr_statements --> expr_statements, expr_statement. 

break_statement --> [break], [';'].
break_statement --> [break], identifier, [';'].

continute_statement --> [continue], [';'].
continute_statement --> [continue], identifier, [';'].

return_statement --> [return], [';'].
return_statement --> [return], expression, [';'].

throws_statement --> [throw], expression, [';'].

try_statement --> [try], block, catches.
try_statement --> [try], block, finally.
catches --> catch_clause.
catches --> catches, catch_clause.
catch_clause --> [catch], ['('], formal_parameter, [')'], block.
finally --> [finally], block. 
