:- module main.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, string, list.
:- import_module parser.

main(IO_1, IO_Last) :-
  io.command_line_arguments(Arguments, IO_1, IO_2),
  ( Arguments = [First | _Rest] ->
    string.to_char_list(First, CharList),
    ( parser.top_level_expression(_E, CharList, Rest) ->
      ( Rest = [] -> 
        io.print("Found match!\n", IO_2, IO_Last)
      ; io.print("Partial match!\n", IO_2, IO_Last))
    ; io.print("No match!\n", IO_2, IO_Last))
  ; io.print("No arguments given!\n", IO_2, IO_Last)).
