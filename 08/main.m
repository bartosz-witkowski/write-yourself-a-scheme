:- module main.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, string, list.
:- import_module adts, parser, show.

main(IO_1, IO_Last) :-
  io.command_line_arguments(Arguments, IO_1, IO_2),
  ( Arguments = [First | _Rest] ->
    string.to_char_list(First, CharList),
    ( parser.top_level_expression(Exp, CharList, Rest) ->
      ( Rest = [] -> 
        io.write_string(show(Exp) ++ "\n", IO_2, IO_Last)
      ; io.write_string("Cannot parse expression!\n", IO_2, IO_Last))
    ; io.write_string("Cannot parse expression!!\n", IO_2, IO_Last))
  ; io.write_string("No arguments given!\n", IO_2, IO_Last)).
