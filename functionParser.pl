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

%Needs completion
classBodyDeclaration --> classMemberDeclaration, staticInitializer, constructorDeclaration.

classMemberDeclaration --> [].

staticInitializer --> [].
staticInitializer --> [static].

constructorDeclaration --> [].
constructorDeclaration --> constructorModifier, constructorDeclarator, throws, constructorBody.

constructorModifier --> [M], {constructor_modifier(M)}.
constructor_modifier(public).
constructor_modifier(protected).
constructor_modifier(private).

constructorDeclarator --> identifier, ['('], formalParameterList, [')'].

%Needs completion
formalParameterList --> [].
%formalParameterList --> formalParameter.
%formalParameterList --> formalParameter, formalParameterList.
%formalParameter --> []. %Needs completion

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
