/** 
* Reads in a file called input.txt. Parses the input into elements and stores them in a list
* using a whitespace, tabs, and new lines as delimeters.
* 
* Usage: Generate a list for your Java source file with query: ?- parse_file.
* (list generated may need some minor editing to be compatible with parser)
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
    write(Split),
    nl.

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.
