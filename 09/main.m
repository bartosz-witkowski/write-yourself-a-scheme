:- module main.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, string, list.
:- import_module adts, parser, show, eval.

:- pred read_expr(list(char)::in, lisp_val::out) is semidet.
read_expr(Char_List, Lisp_Val) :-
  parser.top_level_expression(Lisp_Val, Char_List, []).

main(IO_1, IO_Last) :-
  io.command_line_arguments(Arguments, IO_1, IO_2),
  ( Arguments = [First | _Rest] ->
    string.to_char_list(First, CharList),
    ( read_expr(CharList, Exp) ->
      ( eval(Exp, Evaled) ->
        io.write_string(show(Evaled) ++ "\n", IO_2, IO_Last)
      ; io.write_string("Cannot eval expression!\n", IO_2, IO_Last))
    ; io.write_string("Cannot parse expression!\n", IO_2, IO_Last))
  ; io.write_string("No arguments given!\n", IO_2, IO_Last)).
