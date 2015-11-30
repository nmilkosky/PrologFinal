classDeclaration --> classModifiers, [class], identifier, super, interfaces, classBody.
identifier --> [I], {atom(I)}.

classModifiers --> classModifier.
classModifiers --> classModifier, classModifiers.
classModifier --> [M], {class_modifier(M)}.
class_modifier(public).
class_modifier(abstract).
class_modifier(final).

super --> [].
super --> [extends], identifier.

interfaces --> [].
interfaces --> [implements], identifier.

classBody --> ['{'], classBodyDeclaration, ['}'].

classBodyDeclarations --> classBodyDeclaration.
classBodyDeclarations --> classBodyDeclarations, classBodyDeclaration.
classBodyDeclaration --> classMemberDeclaration, staticInitializer, constructorDeclaration.

classMemberDeclaration --> [].
classMemberDeclaration --> fieldDeclaration, methodDeclaration.

staticInitializer --> [].
staticInitializer --> [static].

constructorDeclaration --> constructorModifier, constructorDeclarator, throws, constructorBody.

constructorModifier --> [M], {constructor_modifier(M)}.
constructor_modifier(public).
constructor_modifier(protected).
constructor_modifier(private).

constructorDeclarator --> identifier, ['('], formalParameter, [')'].

%Needs completion
formalParameter --> type.

throws --> [].
throws --> [throws], identifier.

constructorBody --> ['{'], invocation, blockStatements, ['}'].

invocation --> []. %Needs completion

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

blockStatements --> []. %Needs completion

methodHeader --> []. %Needs completion

methodBody --> ['{'], blockStatments, ['}'].

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

%referenceType --> classType.
%referenceType --> interfaceType.
referenceType --> arrayType.

classType --> typeName.

interfaceType --> typeName.

arrayType --> type, ['[]'].

%---------Tokens------------
packageName --> identifier.
packageName --> packageName, identifier.

typeName --> identifier.
typeName --> packageName, identifier.

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
integer_literal --> hex_int_literal.
integer_literal --> octal_int_literal.

decimal_int_literal --> decimal_numeral. % int_suffix.

hex_int_literal --> hex_numeral. % int_suffix.

octal_int_literal --> octal_numeral. % int_suffix.

% int_suffix -->

decimal_numeral --> ['0'].
decimal_numeral --> non_zero, digits.

digits --> digit.
digits --> digits, digit.

digit --> ['0'].
digit --> non_zero.

non_zero --> ['1'].
non_zero --> ['2'].
non_zero --> ['3'].
non_zero --> ['4'].
non_zero --> ['5'].
non_zero --> ['6'].
non_zero --> ['7'].
non_zero --> ['8'].
non_zero --> ['9'].

hex_numeral --> hex_digit.
hex_numeral --> hex_numeral, hex_digit.

hex_digit --> ['0'].
hex_digit --> ['1'].
hex_digit --> ['2'].
hex_digit --> ['3'].
hex_digit --> ['4'].
hex_digit --> ['5'].
hex_digit --> ['6'].
hex_digit --> ['7'].
hex_digit --> ['8'].
hex_digit --> ['9'].
hex_digit --> ['a'].
hex_digit --> ['b'].
hex_digit --> ['c'].
hex_digit --> ['d'].
hex_digit --> ['e'].
hex_digit --> ['f'].
hex_digit --> ['A'].
hex_digit --> ['B'].
hex_digit --> ['C'].
hex_digit --> ['D'].
hex_digit --> ['E'].
hex_digit --> ['F'].

octal_numeral --> octal_digit.
octal_numeral --> octal_numeral, octal_digit.

octal_digit --> ['0'].
octal_digit --> ['1'].
octal_digit --> ['2'].
octal_digit --> ['3'].
octal_digit --> ['4'].
octal_digit --> ['5'].
octal_digit --> ['6'].
octal_digit --> ['7'].

floating_literal --> digits, ['.'], digits.
floating_literal --> ['.'], digits.

boolean_literal --> true.
boolean_literal --> false.

character_literal --> char.
character_literal --> [''].

% Looking into a better way to do this.
char --> ['a'].
char --> ['b'].
char --> ['c'].
char --> ['d'].
char --> ['e'].
char --> ['f'].
char --> ['g'].
char --> ['h'].
char --> ['i'].
char --> ['j'].
char --> ['k'].
char --> ['l'].
char --> ['m'].
char --> ['n'].
char --> ['o'].
char --> ['p'].
char --> ['q'].
char --> ['r'].
char --> ['s'].
char --> ['t'].
char --> ['u'].
char --> ['v'].
char --> ['w'].
char --> ['x'].
char --> ['y'].
char --> ['z'].
char --> ['A'].
char --> ['B'].
char --> ['C'].
char --> ['D'].
char --> ['E'].
char --> ['F'].
char --> ['G'].
char --> ['H'].
char --> ['I'].
char --> ['J'].
char --> ['K'].
char --> ['L'].
char --> ['M'].
char --> ['N'].
char --> ['O'].
char --> ['P'].
char --> ['Q'].
char --> ['R'].
char --> ['S'].
char --> ['T'].
char --> ['U'].
char --> ['V'].
char --> ['W'].
char --> ['X'].
char --> ['Y'].
char --> ['Z'].

string_literal --> string_characters.

string_characters --> string.
string_characters --> string_characters, string.
string_characters --> [""].

string --> char.
string --> digit.

null_literal --> ['null'].

% Not sure if this is important
keyword --> abstract.
keyword --> char.
keyword --> double.
keyword --> for.
keyword --> int.
keyword --> private.
keyword --> super.
keyword --> transient.
keyword --> boolean.
keyword --> class.
keyword --> else.
keyword --> goto.
keyword --> interface.
keyword --> protected.
keyword --> switch.
keyword --> try.
keyword --> break.
keyword --> const.
keyword --> extends.
keyword --> if.
keyword --> long.
keyword --> public.
keyword --> synchronized.
keyword --> void.
keyword --> byte.
keyword --> continue.
keyword --> final.
keyword --> implements.
keyword --> native.
keyword --> return.
keyword --> short.
keyword --> this.
keyword --> volatile.
keyword --> case.
keyword --> default.
keyword --> finally.
keyword --> float.
keyword --> import.
keyword --> instanceof.
keyword --> new.
keyword --> static.
keyword --> throw.
keyword --> while.
keyword --> catch.
keyword --> do.
keyword --> package.
keyword --> throws.