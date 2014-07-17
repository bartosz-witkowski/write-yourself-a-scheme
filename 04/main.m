:- module main.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, string, list.

:- pred symbol(char::out, list(char)::in, list(char)::out) is semidet.
symbol(C, ListIn, ListOut) :-
    ListIn = [C | ListOut],
    string.contains_char("!$%&|*+-/:<=>?@^_~#", C).

main(IO_1, IO_Last) :-
    io.command_line_arguments(Arguments, IO_1, IO_2),
    ( Arguments = [First | _Rest] ->
      string.to_char_list(First, CharList),
      ( symbol(C, CharList, _Other_Chars) ->
        io.write_string("Found match: ", IO_2, IO_3),
        io.write_char(C, IO_3, IO_4),
        io.write_string("\n", IO_4, IO_Last)
      ; io.write_string("No match!\n", IO_2, IO_Last))
    ; io.write_string("No arguments given!\n", IO_2, IO_Last)).
