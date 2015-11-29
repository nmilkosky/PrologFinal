type --> primitive_type.
type --> reference_type.

primitive_type --> numeric_type.
primitive_type --> ['boolean'].

numeric_type --> integral_type.
numeric_type --> floating_type.

integral_type --> ['byte'].
integral_type --> ['short'].
integral_type --> ['int'].
integral_type --> ['long'].
integral_type --> ['char'].

floating_type --> ['float'].
floating_type --> ['double'].

reference_type --> class_type.
reference_type --> interface_type.
reference_type --> array_type.

class_type --> type_name.

interface_type --> type_name.

array_type --> type, []. % Not sure if it'll work.

package_name --> identifier.
package_name --> package_name, identifier.

type_name --> identifier.
type_name --> package_name, identifier.

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
