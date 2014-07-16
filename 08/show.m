:- module show.

:- interface.

:- import_module string, integer, list.

:- typeclass show(T) where [
  pred show(T, string),
  mode show(in, out) is det
].

:- func unwords(list(string)) = string.
:- pred unwords(list(string)::in, string::out) is det.

:- func show(T) = string <= (show(T)).

:- instance show(integer).
:- instance show(string).

:- implementation.

:- instance show(integer) where [
  (show(Int, String) :- String = integer.to_string(Int))
].

:- instance show(string) where [
  (show(String, "\"" ++ String ++ "\""))
].

show(T) = String :- show(T, String).

unwords(List) = String :- unwords(List, String).

unwords([], "").
unwords([Head|Tail], String) :-
  ( Tail = [] ->
    String = Head
  ; unwords_aux(Tail, "", Rest),
    String = Head ++ Rest).

:- pred unwords_aux(list(string)::in, string::in, string::out) is det.
unwords_aux([], Res, Res).
unwords_aux([Head|Tail], Acc, Res) :-
  ( Tail = [] ->
    Res = Acc ++ " " ++ Head
  ; NewAcc = Acc ++ " " ++ Head,
    unwords_aux(Tail, NewAcc, Res)).
