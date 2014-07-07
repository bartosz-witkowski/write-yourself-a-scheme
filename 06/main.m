:- module main.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, string, list.

:- pred symbol(char::out, list(character)::in, list(character)::out) is semidet.
symbol(C, ListIn, ListOut) :-
    ListIn = [C | ListOut],
    string.contains_char("!$%&|*+-/:<=>?@^_~#", C).

:- pred whitespace1(list(char)::in, list(char)::out).
whitespace1(ListIn, ListOut) :-
    ListIn = [C | ListOut],
    ( C = ' ' ->
      true
    ; C = '\t').

:- pred whitespace(list(char)::in, list(char)::out) is semidet. 
whitespace(ListIn, ListOut) :-
  ( whitespace1(ListIn, Rest) ->
    whitespace(Rest, ListOut)
  ; ListIn = ListOut).

:- pred rule(char::out, list(character)::in, list(character)::out) is semidet.
rule(Symbol, ListIn, ListOut) :-
  whitespace(ListIn, Without_Whitespace),
  symbol(Symbol, Without_Whitespace, ListOut).

main(IO_1, IO_Last) :-
    io.command_line_arguments(Arguments, IO_1, IO_2),
    ( Arguments = [First | _Rest] ->
      string.to_char_list(First, CharList),
      ( rule(C, CharList, _Other_Chars) ->
        io.print("Found match: " ++ char_to_string(C) ++ "\n", IO_2, IO_Last)
      ; io.print("No match!\n", IO_2, IO_Last))
    ; io.print("No arguments given!\n", IO_2, IO_Last)).
