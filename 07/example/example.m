:- module example.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, string, list.

:- pred program(list(char)::in, list(char)::out) is semidet.
program --> reads, writes.

:- pred reads(list(char)::in, list(char)::out) is det.
reads --> (['r', 'e', 'a', 'd'], var -> reads ; []).

:- pred writes(list(char)::in, list(char)::out) is semidet.
writes --> ( ['w', 'r', 'i', 't', 'e'], write_list -> writes ; [] ).

:- pred write_list(list(char)::in, list(char)::out) is semidet.
write_list --> (var,  [','] -> write_list ; var).


:- pred var(list(char)::in, list(char)::out) is semidet.
var --> ['a'] ; ['b'] ; ['c'] ; ['d'] ; ['e'].

% The rules ignore whitespace so for readability we
% spell out everything separately.
:- func to_parse = string.
to_parse = "read" ++ "a" ++
           "read" ++ "b" ++
           "read" ++ "c" ++
           "write" ++ "a" ++ "," ++ "b" ++ "," ++ "c" ++
           "write" ++ "a".
  
main(IO_1, IO_Last) :-
   ( program(to_char_list(to_parse), _) ->
     io.print("Parsed successfully!\n", IO_1, IO_Last)
   ; io.print("Failed at parsin!\n", IO_1, IO_Last)).
