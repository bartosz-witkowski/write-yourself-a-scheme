:- module parser.

:- interface.
:- import_module char, list.
:- import_module adts.

:- pred top_level_expression(lisp_val::out, list(char)::in, list(char)::out) is semidet.

:- implementation.

:- import_module bool, string, list, integer.

top_level_expression(X) --> optional_space, expression(X), optional_space.

:- pred expression(lisp_val::out, list(char)::in, list(char)::out) is semidet.
expression(X) -->
  ( atom(Y)   -> { X = Y } 
  ; string(Y) -> { X = Y }
  ; number(Y) -> { X = Y }
  ; quoted(Y) -> { X = Y }
  ; ['('], 
    ( dotted_list(Y) -> { X = Y }
    ; list(X)),
    [')']).

% atom

:- pred atom(lisp_val::out, list(char)::in, list(char)::out) is semidet.
atom(Atom) -->
  parse_atom(Chars),
  {
    AtomName = string.from_char_list(Chars),
    ( AtomName = "#t" ->
      Atom = lisp_bool(yes)
    ; AtomName = "#f" ->
      Atom = lisp_bool(no)
    ; Atom = lisp_atom(AtomName))
  }.


:- pred parse_atom(list(char)::out, list(char)::in, list(char)::out) is semidet.
parse_atom(Name) -->
  ( letter(Y) -> 
    { X = Y }
  ; symbol(X) ),
  parse_atom_rest(Xs),
  {
    Name = [X | Xs]
  }.

:- pred parse_atom_rest(list(char)::out, list(char)::in, list(char)::out) is semidet.
parse_atom_rest(Rest) -->
  ( letter(X) -> parse_atom_rest(Xs), { Rest = [X | Xs] }
  ; digit(X)  -> parse_atom_rest(Xs), { Rest = [X | Xs] }
  ; symbol(X) -> parse_atom_rest(Xs), { Rest = [X | Xs] }
  ; { Rest = [] }).

% string

:- pred string(lisp_val::out, list(char)::in, list(char)::out) is semidet.
string(String) -->
  parse_string(CharList), {
    String = lisp_string(string.from_char_list(CharList))
  }.

:- pred parse_string(list(char)::out, list(char)::in, list(char)::out) is semidet.
parse_string(CharList) --> 
  [ '"' ], 
  string_content(CharList), 
  [ '"' ]. 

:- pred string_content(list(char)::out, list(char)::in, list(char)::out) is semidet.
string_content(Chars) -->  
  ( not_quote(X) ->
    string_content(Xs), { Chars = [X | Xs] }
  ; { Chars = [] }).

:- pred not_quote(char::out, list(char)::in, list(char)::out) is semidet.
not_quote(X) --> [X], { X \= '"' }.

% number

:- pred number(lisp_val::out, list(char)::in, list(char)::out) is semidet.
number(Number) --> parse_number(Cs), { 
                     string.from_char_list(Cs, String),
                     Integer = integer.from_string(String),
                     Number = lisp_number(Integer)
                   }.

:- pred parse_number(list(char)::out, list(char)::in, list(char)::out) is semidet.
parse_number(Chars) --> ( digit(X) ->
                          { Chars = [X | Xs] }, parse_number(Xs)
                        ; { Chars = [] } ).

% quoted

:- pred quoted(lisp_val::out, list(char)::in, list(char)::out) is semidet.
quoted(Quoted) --> 
        [ '\'' ],
        expression(Expression),
        {
                Quoted = lisp_list([lisp_atom("quote"), Expression])
        }.

% list

:- pred list(lisp_val::out, list(char)::in, list(char)::out) is det.
list(List) --> parse_list(Xs), { List = lisp_list(Xs) }.

:- pred parse_list(list(lisp_val)::out, list(char)::in, list(char)::out) is det.
parse_list(List) -->
  ( expression(Expression), space, parse_list(Rest) -> { List = [ Expression | Rest ] }
  ; expression(Expression) -> { List = [Expression] }
  ; { List = [] } ).

% dotted list
:- pred dotted_list(lisp_val::out, list(char)::in, list(char)::out) is semidet.
dotted_list(Dotted_List) -->
  parse_dotted_list(Xs, X), { Dotted_List = lisp_dotted_list(Xs, X) }.

:- pred parse_dotted_list(list(lisp_val)::out, lisp_val::out, list(char)::in, list(char)::out) is semidet.
parse_dotted_list(List, Expression) -->
  parse_list(List), ['.'], space, expression(Expression).

/* helper DCGs */

:- pred digit(char::out, list(char)::in, list(char)::out) is semidet.
digit(X) --> [X], { char.is_digit(X) }.

:- pred letter(char::out, list(char)::in, list(char)::out) is semidet.
letter(X) --> [X], { char.is_alpha(X) }.

:- pred symbol(char::out, list(char)::in, list(char)::out) is semidet.
symbol(X) --> [X], { string.contains_char("!$%&|*+-/:<=>?@^_~#", X) }.

:- pred optional_space(list(char)::in, list(char)::out) is det. 
optional_space -->
  ( [' ']  -> optional_space
  ; ['\t'] -> optional_space
  ; [] ).

:- pred space(list(char)::in, list(char)::out) is semidet. 
space -->
  ( [' ']  -> optional_space
  ; ['\t'], optional_space).
