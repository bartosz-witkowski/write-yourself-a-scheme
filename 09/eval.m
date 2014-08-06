:- module eval.

:- interface.

:- import_module adts.

:- pred eval(lisp_val::in, lisp_val::out) is semidet.

:- implementation.

:- import_module list.

eval(X @ lisp_number(_), X).
eval(X @ lisp_string(_),  X).
eval(X @ lisp_bool(_), X).
eval(lisp_list(List), Out) :-
  List = [lisp_atom("quote") | Tail],
  Tail = [Out].
