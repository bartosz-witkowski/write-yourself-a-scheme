:- module adts.

:- interface.

:- import_module show.
:- import_module bool, string, integer, list, maybe, environment, store, io.

:- type port ---> input_port(io.input_stream)
                ; output_port(io.output_stream)
                .

:- type lisp_val(S) ---> lisp_atom(string) 
                       ; lisp_list(list(lisp_val(S)))
                       ; lisp_dotted_list(list(lisp_val(S)), lisp_val(S))
                       ; lisp_number(integer)
                       ; lisp_string(string)
                       ; lisp_boolean(bool)
                       ; lisp_port(port)
                       ; lisp_eof
                       ; lisp_function(
                          params  :: list(string),
                          vararg  :: maybe(string),
                          body    :: list(lisp_val(S)),
                          closure :: environment(S))
                       ; lisp_primitive_function(f_primitive(S))
                       ; lisp_io_function(io_func(S))
                    .

:- inst lisp_val ---> lisp_atom(ground) 
                    ; lisp_list(list_skel(lisp_val))
                    ; lisp_dotted_list(ground, ground)
                    ; lisp_number(ground)
                    ; lisp_string(ground)
                    ; lisp_boolean(ground)
                    ; lisp_port(ground)
                    ; lisp_eof
                    ; lisp_function(ground, ground, list_skel(lisp_val), ground)
                    ; lisp_primitive_function(f_primitive)
                    ; lisp_io_function(io_func)
                    .

:- type lisp_error(S) ---> num_args(integer, list(lisp_val(S)))
                         ; type_mismatch(string, lisp_val(S))
                         ; parser(string)
                         ; bad_special_form(string, lisp_val(S))
                         ; not_function(string, string)
                         ; unbound_var(string, string)
                         ; procedure_application(lisp_val(S), list(lisp_val(S)))
                         ; io_error(string)
                         ; missing_procedure_expression
                         .


:- inst function ---> lisp_function(ground, ground, list_skel(lisp_val), ground) 
                    ; lisp_primitive_function(f_primitive)
                    .

:- inst lisp_function ---> lisp_function(ground, ground, ground, ground).

:- type f_primitive(S) == (pred(list(lisp_val(S)), throws_error(lisp_val(S), S))).
:- inst f_primitive    == (pred(in,             out) is det).

:- type io_func(S) == (pred(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io)).
:- inst io_func    == (pred(in, out, di, uo) is det).

:- type throws_error(T, S) == maybe_error(T, lisp_error(S)).
:- inst throws_error(I) == maybe_error(I).

:- instance show(lisp_val(S)).
:- instance show(lisp_error(S)).

:- implementation.

:- instance show(lisp_val(S)) where [
        pred(show/2) is show_lisp_val
].

:- instance show(lisp_error(S)) where [
        pred(show/2) is show_error
].

:- pred show_error(lisp_error(S)::in, string::out) is det.
show_error(unbound_var(Message, Varname),   Message ++ ": " ++ Varname).
show_error(bad_special_form(Message, Form), Message ++ ": " ++ show(Form)).
show_error(not_function(Message, Func),     Message ++ ": " ++ Func).
show_error(type_mismatch(Expected, Found),  "Invalid type: expected " ++ Expected
                                            ++ ", found " ++ show(Found)).
show_error(parser(ParseErr),               "Parse error: " ++ ParseErr).
show_error(missing_procedure_expression, 
    "Missing procedure expression; probably originally (), which is an illegal empty application").
show_error(procedure_application(Function, Args), String) :- 
    FunctionString = show(Function),
    unwords(list.map(func(X) = show(X), Args), ArgsString),
    String = "Procedure application: expected procedure, given: " ++ FunctionString ++ "; arguments were: " ++ ArgsString.

show_error(io_error(Message), String) :- 
   String = "Io error: " ++ Message.

show_error(num_args(Expected, Found),       String) :- 
    unwords(list.map(func(X) = show(X), Found), FoundString),
    String = "Expected "  ++ show(Expected) ++ " args; found values: " ++ FoundString.


:- pred show_lisp_val(lisp_val(S)::in, string::out) is det.
show_lisp_val(lisp_string(Contents), String) :- String = "\"" ++ Contents ++ "\"".
show_lisp_val(lisp_number(Num), String) :- String = integer.to_string(Num).
show_lisp_val(lisp_atom(Name), Name).
show_lisp_val(lisp_boolean(yes), "#t").
show_lisp_val(lisp_boolean(no),  "#f").
show_lisp_val(lisp_port(_),  "<IO port>").
show_lisp_val(lisp_eof,  "#<eof>").
show_lisp_val(lisp_list(Contents), Out) :-
    list.map(pred(X::in, Y::out) is det :- show_lisp_val(X, Y), Contents, StringList),
    unwords(StringList, OneString),
    Out = "(" ++ OneString ++ ")".
show_lisp_val(lisp_dotted_list(List, Last), Out) :-
    list.map(pred(X::in, Y::out) is det :- show_lisp_val(X, Y), List, StringList),
    unwords(StringList, ListInOneString),
    show_lisp_val(Last, LastString),
    Out = "(" ++ ListInOneString ++ " . " ++  LastString ++ ")".
show_lisp_val(Func @ lisp_function(_, _, _, _), Out) :-
    unwords(list.map(show, Func ^ params), Params),
    (
        Func ^ vararg = no,
        Varargs = ""
    ;
        Func ^ vararg = yes(V),
        Varargs = " . " ++ V
    ),
    Out = "(lambda (" ++ Params ++ Varargs ++ ") ...)".
show_lisp_val(lisp_primitive_function(_), "<primitive>").
show_lisp_val(lisp_io_function(_),  "<IO primitive>").
