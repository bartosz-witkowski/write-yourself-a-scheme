:- module scheme.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
 
:- import_module show, adts, environment.

:- import_module list, stack, string, integer, bool, char, maybe, map, solutions, store, io.

% parsing --[

:- pred char(char::out, list(character)::in, list(character)::out) is semidet.
char(C) --> [C].

:- pred letter(char::out, list(character)::in, list(character)::out) is semidet.
letter(X) --> char(X), 
       { 
            is_alpha(X)
       }.

:- pred digit(char::out, list(character)::in, list(character)::out) is semidet.
digit(X) --> char(X), 
       { 
            is_digit(X)
       }.

:- pred symbol(char::out, list(character)::in, list(character)::out) is semidet.
symbol(X) --> char(X), 
       { 
            contains_char("!$%&|*+-/:<=>?@^_~#", X)
       }.


:- pred number(list(character)::out, list(character)::in, list(character)::out) is semidet.
number(Chars) --> (if  digit(X)
                  then { Chars = [X|Xs] }, number(Xs)
                  else { Chars = [] } ).


:- pred spaces(list(character)::in, list(character)::out) is semidet.
spaces --> (if   char(X), { is_whitespace(X) }
            then spaces 
            else []).


:- pred not_quote(char::out, list(character)::in, list(character)::out) is semidet.
not_quote(X) --> char(X), { X \= '"' }.

:- pred string_without_quotes(list(character)::out, list(character)::in, list(character)::out) is semidet.
string_without_quotes(Chars) -->  
        (if   not_quote(X)
         then { Chars = [X|Xs] }, string_without_quotes(Xs)
         else { Chars = []     }).

:- pred parse_string(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_string(lisp_string(String)) --> 
                char('"'), 
                string_without_quotes(CharList), 
                {
                        string.from_char_list(CharList, String)
                },
                char('"'). 


:- pred atomNameToAtom(string::in, lisp_val(S)::out) is det <= store(S).
atomNameToAtom(AtomName, Res) :-
        (if   AtomName = "#t" 
         then Res = lisp_boolean(yes)
         else (if   AtomName = "#f"
               then Res = lisp_boolean(no)
               else Res = lisp_atom(AtomName))).
        

:- pred parse_atom(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_atom(Atom) -->
        (     if letter(Y) then { X = Y  }
         else if symbol(Y) then { X = Y  }
         else                   { false  }),
        parse_atom_aux(Xs),
        {
                append([X], Xs, CharList),
                string.from_char_list(CharList, AtomName),
                atomNameToAtom(AtomName, Atom)
        }.


:- pred parse_atom_aux(list(character)::out, list(character)::in, list(character)::out) is semidet.
parse_atom_aux(Chars) --> 
       (if      letter(X) then { Chars = [X|Xs] }, parse_atom_aux(Xs)
        else if digit(X)  then { Chars = [X|Xs] }, parse_atom_aux(Xs)
        else if symbol(X) then { Chars = [X|Xs] }, parse_atom_aux(Xs)
        else { Chars = [] }).

:- pred parse_number(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_number(Number) -->
        number(CharList),
        { 
                string.from_char_list(CharList, NumStr),
                Integer = integer.from_string(NumStr),
                Number = lisp_number(Integer) 
        }.

:- pred parse_list_aux(list(lisp_val(S))::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_list_aux(Elems) --> 
        parse_expression(X),
        (if   spaces, parse_list_aux(Ys), spaces
         then { append([X], Ys, Elems) }
         else { Elems = [X]            }).


:- pred parse_list(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_list(lisp_list(Expressions)) --> parse_list_aux(Expressions).

:- pred parse_dotted_list_aux(list(lisp_val(S))::out, lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_dotted_list_aux(Head, Tail) --> 
        parse_list_aux(Head),
        spaces,
        char('.'),
        spaces,
        parse_expression(Tail),
        spaces.

:- pred parse_dotted_list(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_dotted_list(DottedList) -->
        parse_dotted_list_aux(Head, Tail),
        {
                DottedList = lisp_dotted_list(Head, Tail)
        }.


:- pred parse_quoted(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_quoted(Quoted) --> 
        char('\''),
        parse_expression(Expression),
        {
                Quoted = lisp_list([lisp_atom("quote"), Expression])
        }.

:- pred parse_expression(lisp_val(S)::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_expression(X) --> 
        (    if parse_atom(Y)   then { X = Y } 
        else if parse_string(Y) then { X = Y }
        else if parse_number(Y) then { X = Y }
        else if parse_quoted(Y) then { X = Y }
        else char('('),                     
          (     if  parse_dotted_list(Y) then { X = Y  }             % If this is reversed dotted list doesn't parse
           else if  parse_list(Y)        then { X = Y  }             % why?
           else if  spaces               then { X = lisp_list([]) }
           else  { false }
          ),
          char(')')
        ).

:- pred parse_expression_list(list(lisp_val(S))::out, list(character)::in, list(character)::out) is semidet <= store(S).
parse_expression_list(Xs) -->
        parse_expression(X),
        (if   spaces, parse_expression_list(Ys)
         then { append([X], Ys, Xs) }
         else { Xs = [X]            }).

% ]--

% Evaluation --[

:- pred eval(environment(S), lisp_val(S), throws_error(lisp_val(S), S), S, S, io, io) <= store(S).
:- mode eval(in, in(lisp_val), out(throws_error(lisp_val)), di, uo, di, uo) is det.
eval(_Env, X @ lisp_number(_),  ok(X), !Store, !IO).
eval(_Env, X @ lisp_string(_),  ok(X), !Store, !IO).
eval(_Env, X @ lisp_boolean(_), ok(X), !Store, !IO).
eval(_Env, X @ lisp_port(_),    ok(X), !Store, !IO).
eval(_Env, X @ lisp_eof,        ok(X), !Store, !IO).
eval(_Env, X @ lisp_dotted_list(_, _), Out, !Store, !IO) :-
    Out = error(bad_special_form("Unrecognized special form", X)).
eval(_Env, X @ lisp_primitive_function(_), Out, !Store, !IO) :-
    Out = error(bad_special_form("Unrecognized special form", X)).
eval(_Env, X @ lisp_io_function(_), Out, !Store, !IO) :-
    Out = error(bad_special_form("Unrecognized special form", X)).
eval(_Env, X @ lisp_function(_, _, _, _), Out, !Store, !IO) :-
    Out = error(bad_special_form("Unrecognized special form", X)).
eval(Env, lisp_atom(Id), Out, !Store, !IO) :-
    get_var(Env, Id, Out, !Store).
eval(Env, X @ lisp_list(List), Out, !Store, !IO) :-
         if List = [lisp_atom("quote")|Tail] then
            (if   Tail = [Quoted] then
                  Out = ok(Quoted)
             else 
                  Out = error(bad_special_form("Unrecognized special form", X)))
    else if List = [lisp_atom("if"), Pred, Conseq, Alt] then
            eval(Env, Pred, Evaled, !Store, !IO),
            (
                ok(Result) = Evaled,
                (if   Result = lisp_boolean(no) then
                      eval(Env, Alt,    Out, !Store, !IO)
                 else eval(Env, Conseq, Out, !Store, !IO))
            ;
                error(_) = Evaled,
                Out = Evaled
            )
    else if List = [lisp_atom("set!"), lisp_atom(VarName), Form] then
            eval(Env, Form, Evaled, !Store, !IO),
            ( 
                Evaled = ok(LispVal),
                set_var(Env, VarName, LispVal, Result, !Store),
                (
                    Result = ok(_),
                    Out = ok(LispVal)
                ;
                    Result = error(Error),
                    Out = error(Error)
                )
            ;
                Evaled = error(Error),
                Out = error(Error)
            )
    else if List = [lisp_atom("define"), lisp_atom(VarName), Form] then
            eval(Env, Form, Evaled, !Store, !IO),
            ( 
                Evaled = ok(LispVal),
                define_var(Env, VarName, LispVal, !Store),
                Out = ok(LispVal)
            ;
                Evaled = error(Error),
                Out = error(Error)
            )
    else if List = [lisp_atom("define"), lisp_list([lisp_atom(Var) | Params]) | Body] then
            make_normal_func(Env, Params, Body, Func, !Store),
            define_var(Env, Var, Func, !Store),
            Out = ok(Func)
    else if List = [lisp_atom("define"), lisp_dotted_list([lisp_atom(Var) | Params], Varargs) | Body] then
            make_varargs(Env, Varargs, Params, Body, Func, !Store),
            define_var(Env, Var, Func, !Store),
            Out = ok(Func)
    else if List = [lisp_atom("lambda"), lisp_list(Params) | Body] then
            make_normal_func(Env, Params, Body, Func, !Store),
            Out = ok(Func)
    else if List = [lisp_atom("lambda"), lisp_dotted_list(Params, Varargs) | Body] then
            make_varargs(Env, Varargs, Params, Body, Func, !Store),
            Out = ok(Func)
    else if List = [lisp_atom("lambda"), Varargs @ lisp_atom(_) | Body] then
            make_varargs(Env, Varargs, [], Body, Func, !Store),
            Out = ok(Func)
    else if List = [lisp_atom("load"), lisp_string(FileName)] then
            load(FileName, Result, !IO),
            ( Result = ok(Exprs),
              load_expr_list(Env, Exprs, ok(lisp_list([])), Out, !Store, !IO)
            ;
              Result = error(E),
              Out = error(E)
            )
    else if List = [lisp_atom("apply"), Func, lisp_list(Args)] then
            apply(Func, Args, Out, !Store, !IO)
    else if List = [Function|Args] then        
            eval(Env, Function, Result, !Store, !IO),
            (
                Result = ok(Func),
                eval_body(Env, Args, MaybeEvaled, !Store, !IO),
                (
                     MaybeEvaled = ok(EvaledArgs),
                     apply(Func, EvaledArgs, Out, !Store, !IO)
                ;
                     MaybeEvaled = error(E),
                     Out = error(E) 
                )
            ;
                Result = error(E),
                Out = error(E)
            )
       else 
            Out = error(bad_special_form("Unrecognized special form", X)).


:- pred load_expr_list(environment(S), list(lisp_val(S)), throws_error(lisp_val(S), S), throws_error(lisp_val(S), S), S, S, io, io) is det <= store(S).
:- mode load_expr_list(in, in(list_skel(lisp_val)), in, out, di, uo, di, uo) is det.
load_expr_list(Env, Exprs, LastResult, Output, !Store, !IO) :-
    (
       Exprs = [],
       Output = LastResult
    ;
       Exprs = [LispVal | Rest],
       eval(Env, LispVal, Evaled, !Store, !IO),
       (
          Evaled = ok(Result),
          load_expr_list(Env, Rest, ok(Result), Output, !Store, !IO)
       ;
          Evaled = error(E),
          Output = error(E)
       )
    ).

:- pred make_func(environment(S), maybe(string), list(lisp_val(S)), list(lisp_val(S)), lisp_val(S), S, S) <= store(S).
:- mode make_func(in, in, in, in, out(lisp_val), di, uo) is det.
make_func(Env, Varargs, Params, Body, Func, !Store) :-
    list.map(
        (pred(X::in, Y::out) is det :- 
            show(X, Y)), 
        Params,
        Ps),
    environment.add_scope(Env, Closure, !Store),
    Func = lisp_function(Ps, Varargs, Body, Closure).

:- pred make_normal_func(environment(S), list(lisp_val(S)), list(lisp_val(S)), lisp_val(S), S, S) <= store(S).
:- mode make_normal_func(in, in, in, out(lisp_val), di, uo).
make_normal_func(Env, Params, Body, Func, !Store) :-
    make_func(Env, no, Params, Body, Func, !Store).

:- pred make_varargs(environment(S), lisp_val(S), list(lisp_val(S)), list(lisp_val(S)), lisp_val(S), S, S) <= store(S).
:- mode make_varargs(in, in, in, in, out(lisp_val), di, uo).
make_varargs(Env, Varargs, Params, Body, Func, !Store) :-
    show(Varargs, V),
    make_func(Env, yes(V), Params, Body, Func, !Store).

:- pred apply(lisp_val(S), list(lisp_val(S)), throws_error(lisp_val(S), S), S, S, io, io) <= store(S).
:- mode apply(in(lisp_val), in(list_skel(lisp_val)), out(throws_error(lisp_val)), di, uo, di, uo) is det.
apply(LispVal, Args, Output, !Store, !IO) :- 
    (if      LispVal = lisp_primitive_function(Func) then
             Func(Args, Output)
     else if LispVal = lisp_io_function(Func) then
             Func(Args, Output, !IO)
     else if LispVal = lisp_function(Params, Varargs, Body, Closure) then
             list.length(Params, ParamsLength),
             list.length(Args, ArgsLength),
             Error = error(num_args(integer(ParamsLength), Args)),
             (if  ParamsLength \= ArgsLength, Varargs = no then
                  Output = Error
             else 
                  (if   list.split_list(ParamsLength, Args, NonVarargs, RemainingArgs) then
                        (if   zip(Params, NonVarargs, NameValuePairs) then
                              bind_in_scope(Closure, NameValuePairs, !Store),
                              (
                                 Varargs = yes(ArgName),
                                 bind_in_scope(Closure, [{ArgName, lisp_list(RemainingArgs)}], !Store)
                              ;
                                 Varargs = no
                              ),
                              eval_and_get_last_expression(Closure, Body, Output, !Store, !IO)
                         else 
                              Output = Error)
                   else
                        Output = Error))
        else
             Output = error(procedure_application(LispVal, Args))).

:- pred eval_body(environment(S), list(lisp_val(S)), throws_error(list(lisp_val(S)), S), S, S, io, io) <= store(S).
:- mode eval_body(in, in(list_skel(lisp_val)), out(throws_error(list_skel(lisp_val))), di, uo, di, uo) is det.
eval_body(Env, Body, Output, !Store, !IO) :-
    eval_body(Env, Body, ok([]), Result, !Store, !IO),
    (
        Result = error(E),
        Output = error(E)
    ;
        Result = ok(List),
        Output = ok(list.reverse(List))
    ).
    

:- pred eval_and_get_last_expression(environment(S), list(lisp_val(S)), throws_error(lisp_val(S), S), S, S, io, io) <= store(S).
:- mode eval_and_get_last_expression(in, in(list_skel(lisp_val)), out, di, uo, di, uo) is det.
eval_and_get_last_expression(Env, Body, Last, !Store, !IO) :-
    eval_body(Env, Body, ok([]), Output, !Store, !IO),
    (
        Output = error(E),
        Last = error(E)
    ;
        Output = ok(List),
        (
            List = [],
            Last = error(missing_procedure_expression)
        ;
            List = [L | _],
            Last = ok(L)
        )
    ).

:- pred eval_body(environment(S), list(lisp_val(S)), throws_error(list(lisp_val(S)), S), throws_error(list(lisp_val(S)), S), S, S, io, io) is det <= store(S).
:- mode eval_body(in, in(list_skel(lisp_val)), in, out(throws_error(list_skel(lisp_val))), di, uo, di, uo) is det.
eval_body(Env, Body, EvaluationResults, Output, !Store, !IO) :-
    (
        Body = [],
        Output = EvaluationResults
    ;
        Body = [LispVal | Rest],
        (
            EvaluationResults = ok(Results),
            eval(Env, LispVal, Evaled, !Store, !IO),
            (
                Evaled = ok(Result),
                NewResults = [Result | Results],
                eval_body(Env, Rest, ok(NewResults), Output, !Store, !IO)
            ;
                Evaled = error(E),
                Output = error(E)
            )
        ;
            % shouldn't happen
            EvaluationResults = error(_),
            Output = EvaluationResults
        )
    ).

:- pred primitive(string,     pred(list(lisp_val(S)), throws_error(lisp_val(S), S))          ) <= store(S).
:- mode primitive(in,     out(pred(in,             out)                  is det)) is semidet.
:- mode primitive(out,    out(pred(in,             out)                  is det)) is multi.
primitive("+",        numeric_bin_op(func(X, Y) = Z :- Z is X + Y)).
primitive("-",        numeric_bin_op(func(X, Y) = Z :- Z is X - Y)).
primitive("*",        numeric_bin_op(func(X, Y) = Z :- Z is X * Y)).
primitive("/",        numeric_bin_op(func(X, Y) = Z :- Z is X div Y)).
primitive("mod",      numeric_bin_op(func(X, Y) = Z :- Z is X mod Y)).
primitive("quotient", numeric_bin_op(func(X, Y) = Z :- Z is X // Y)).
primitive("reminder", numeric_bin_op(func(X, Y) = Z :- Z is X rem Y)).

primitive("=",        numeric_bool_bin_op(pred(X::in, Y::in) is semidet :- X = Y)).
primitive("<",        numeric_bool_bin_op(pred(X::in, Y::in) is semidet :- X < Y)).
primitive(">",        numeric_bool_bin_op(pred(X::in, Y::in) is semidet :- X > Y)).
primitive("/=",       numeric_bool_bin_op(pred(X::in, Y::in) is semidet :- X \= Y)).
primitive(">=",       numeric_bool_bin_op(pred(X::in, Y::in) is semidet :- X >= Y)).
primitive("<=",       numeric_bool_bin_op(pred(X::in, Y::in) is semidet :- X =< Y)).

primitive("&&",       bool_bool_bin_op(pred(X::in, Y::in) is semidet :- yes = and(X, Y))).
primitive("||",       bool_bool_bin_op(pred(X::in, Y::in) is semidet :- yes = or(X, Y))).

primitive("string=?",   string_bool_bin_op(pred(X::in, Y::in) is semidet :- X = Y)).
%primitive("string<?",  string_bool_bin_op(pred(X::in, Y::in) is semidet :- X < Y)).
%primitive("string>?",  string_bool_bin_op(pred(X::in, Y::in) is semidet :- X > Y)).
%primitive("string<=?", string_bool_bin_op(pred(X::in, Y::in) is semidet :- X >= Y)).
%primitive("string>=?", string_bool_bin_op(pred(X::in, Y::in) is semidet :- X =< Y)).

primitive("car",        car).
primitive("cdr",        cdr).
primitive("cons",       cons).
primitive("eq?",        eqv).
primitive("eqv?",       eqv).
primitive("equal?",     eqv).


:- pred all_primitives(list({string, lisp_val(S)})::out) is det <= store(S).
all_primitives(NamePrimitivePairs) :-
    solutions(
        (pred(X::out) is multi :-
            primitive(Name, Func),
            X = {Name, lisp_primitive_function(Func)}),
        NamePrimitivePairs).

:- pred io_primitive(string,     pred(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io)) <= store(S).
:- mode io_primitive(in,     out(pred(in, out, di, uo) is det)) is semidet.
:- mode io_primitive(out,    out(pred(in, out, di, uo) is det)) is multi.

io_primitive("open-input-file", open_input_file).
io_primitive("open-output-file", open_output_file).
io_primitive("close-input-port", close_port).
io_primitive("close-output-port", close_port).
io_primitive("read", read).
io_primitive("write", write).
io_primitive("read-contents", read_contents).
io_primitive("read-all", read_all).

:- pred all_io_primitives(list({string, lisp_val(S)})::out) is det <= store(S).
all_io_primitives(NamePrimitivePairs) :-
    solutions(
        (pred(X::out) is multi :-
            io_primitive(Name, Func),
            X = {Name, lisp_io_function(Func)}),
        NamePrimitivePairs).
       
:- pred eqv(list(lisp_val(S))::in, throws_error(lisp_val(S), S)::out) is det <= store(S).
eqv(LispVal, Result) :-
    if      LispVal = [lisp_boolean(A), lisp_boolean(B)] then
            Result =  ok(equality(A, B))
    else if LispVal = [lisp_number(A), lisp_number(B)] then
            Result =  ok(equality(A, B))
    else if LispVal = [lisp_string(A), lisp_string(B)] then
            Result = ok(equality(A, B))
    else if LispVal = [lisp_atom(A), lisp_atom(B)] then
            Result = ok(equality(A, B))
    else if LispVal = [lisp_dotted_list(As, A), lisp_dotted_list(Bs, B)] then
            ListA = lisp_list(As ++ [A]),
            LispB = lisp_list(Bs ++ [B]),
            eqv([ListA, LispB], Result)
    else if LispVal = [lisp_list(As), lisp_list(Bs)] then
            (if   zip(As, Bs, Zipped) then
                  EquivalentPair = (pred(Tuple::in) is semidet :-
                     {A, B} = Tuple,
                     eqv([A, B], ok(lisp_boolean(yes)))),
                  AllEquivalent = ((pred) is semidet :-
                     list.all_true(EquivalentPair, Zipped)),
                  Result = ok(lisp_boolean(bool.pred_to_bool(AllEquivalent)))
             else 
                  Result = ok(lisp_boolean(no)))
    else if LispVal = [_, _] then
            Result = ok(lisp_boolean(no))
       else 
            Result = error(num_args(integer(2), LispVal)).

:- pred car(list(lisp_val(S))::in, throws_error(lisp_val(S), S)::out) is det <= store(S).
car(LispVal, Result) :-
    if      LispVal = [lisp_list([X|_])] then
            Result = ok(X)
    else if LispVal = [lisp_dotted_list([X|_], _)] then
            Result = ok(X)
    else if LispVal = [BadArg] then
            Result = error(type_mismatch("pair", BadArg))
       else
            Result = error(num_args(integer(1), LispVal)).

:- pred cdr(list(lisp_val(S))::in, throws_error(lisp_val(S), S)::out) is det <= store(S).
cdr(LispVal, Result) :-
    if      LispVal = [lisp_list([_|Xs])] then
            Result = ok(lisp_list(Xs))
    else if LispVal = [lisp_dotted_list([_|Xs], X)] then
            Result = ok(lisp_dotted_list(Xs, X))
    else if LispVal = [lisp_dotted_list([_], X)] then
            Result = ok(X)
    else if LispVal = [BadArg] then
            Result = error(type_mismatch("pair", BadArg))
       else
            Result = error(num_args(integer(1), LispVal)).

:- pred cons(list(lisp_val(S))::in, throws_error(lisp_val(S), S)::out) is det <= store(S).
cons(LispVal, Result) :-
    if      LispVal = [X, lisp_list([])] then
            Result = ok(lisp_list([X]))
    else if LispVal = [X, lisp_list(Xs)] then
            Result = ok(lisp_list([X] ++ Xs))
    else if LispVal = [X, lisp_dotted_list(Xs, Last)] then
            Result = ok(lisp_dotted_list([X] ++ Xs, Last))
    else if LispVal = [X, Y] then
            Result = ok(lisp_dotted_list([X], Y))
       else 
            Result = error(num_args(integer(2), LispVal)).


:- func equality(A, A) = lisp_val(S) <= store(S).
equality(A, B) = lisp_boolean(bool.pred_to_bool((pred) is semidet :- A = B)).

:- pred zip(list(A)::in, list(B)::in, list({A, B})::out) is semidet.
zip(As, Bs, Zipped) :- zip1(As, Bs, [], Zipped).


:- pred zip1(list(A)::in, list(B)::in, list({A, B})::in, list({A, B})::out) is semidet.
zip1(As, Bs, Acc, Zipped) :-
         if [A | TailA] = As, [B | TailB] = Bs then
            NewAcc = [{A, B}|Acc],
            zip1(TailA, TailB, NewAcc, Zipped)
    else if [] = As, [] = Bs then
            Zipped = reverse(Acc)
       else 
            fail.


:- func numeric_bool_bin_op(pred(integer, integer))     = pred(list(lisp_val(S)), throws_error(lisp_val(S), S)) <= store(S).
:- mode numeric_bool_bin_op(in(pred(in, in) is semidet)) = out(pred(in, out) is det).
numeric_bool_bin_op(Op) = bool_bin_op(unpack_num, Op).

:- func string_bool_bin_op(pred(string, string)) = pred(list(lisp_val(S)), throws_error(lisp_val(S), S)) <= store(S).
:- mode string_bool_bin_op(in(pred(in, in) is semidet)) = out(pred(in, out) is det).
string_bool_bin_op(Op) = bool_bin_op(unpack_str, Op).

:- func bool_bool_bin_op(pred(bool, bool)) = pred(list(lisp_val(S)), throws_error(lisp_val(S), S)) <= store(S).
:- mode bool_bool_bin_op(in(pred(in, in) is semidet)) = out(pred(in, out) is det).
bool_bool_bin_op(Op) = bool_bin_op(unpack_bool, Op).

:- func numeric_bin_op(func(integer, integer) = integer) = pred(list(lisp_val(S)), throws_error(lisp_val(S), S)) <= store(S).
:- mode numeric_bin_op(in) = out(pred(in, out) is det).
numeric_bin_op(BinFunc) = Pred :- 
        BinPred = to_pred(flip(BinFunc)),
        numeric_bin_op(BinPred, Pred).

:- pred numeric_bin_op(pred(integer, integer, integer), pred(list(lisp_val(S)), throws_error(lisp_val(S), S))) <= store(S).
:- mode numeric_bin_op(pred(in, in, out) is det, out(pred(in, out) is det)) is det.
numeric_bin_op(BinPred, Pred) :- 
    Pred = (pred(Args::in, Result::out) is det :-
        list.map(unpack_num, Args, UnpackedList),
        list.filter_map(
            (pred(X::in, Y::out) is semidet :- ok(Y) = X),
            UnpackedList,
            Nums,
            Errors),
        (if   [error(FirstError)|_OtherErrors] = Errors then
              Result = error(FirstError)
         else 
              if   Nums = [First|Rest] then
                   list.foldl(BinPred, Rest, First, FoldResult),
                   Result = ok(lisp_number(FoldResult))
              else Result = error(num_args(integer(1), Args)))).

:- func bool_bin_op(pred(lisp_val(S), throws_error(A, S)), pred(A, A)) = pred(list(lisp_val(S)), throws_error(lisp_val(S), S)) <= store(S).
:- mode bool_bin_op(in(pred(in, out) is det), in(pred(in, in) is semidet)) = out(pred(in, out) is det).
bool_bin_op(Unpacker, Op) = Pred :-
    Pred = (pred(Args::in, Result::out) is det :- 
        if   Args = [Left, Right]                                           then
             Unpacker(Left, UnpackedLeft),
             Unpacker(Right, UnpackedRight),
             (
                ok(L) = UnpackedLeft,
                (
                    ok(R) = UnpackedRight,
                    (if   Op(L, R)                       then
                          ok(lisp_boolean(yes)) = Result
                     else ok(lisp_boolean(no))  = Result)
                ;
                    error(E) = UnpackedRight,
                    Result = error(E)
                )
             ;
                error(E) = UnpackedLeft, 
                Result = error(E)
             )
        else Result = error(num_args(integer(2), Args))).



:- pred unpack_num(lisp_val(S)::in, throws_error(integer, S)::out) is det.
unpack_num(LispVal, Result) :-
        if   LispVal = lisp_number(Int) then 
             Result = ok(Int)
        else 
             Result = error(type_mismatch("number", LispVal)).

:- pred unpack_bool(lisp_val(S)::in, throws_error(bool, S)::out) is det.
unpack_bool(LispVal, Result) :-
        if   LispVal = lisp_boolean(Bool) then 
             Result = ok(Bool)
        else 
             Result = error(type_mismatch("boolean", LispVal)).


:- pred unpack_str(lisp_val(S)::in, throws_error(string, S)::out) is det.
unpack_str(LispVal, Result) :-
        if   LispVal = lisp_string(String) then 
             Result = ok(String)
        else 
             Result = error(type_mismatch("string", LispVal)).

:- func to_func(pred(A,  B,  C)) = (func(A, B) = C).
:- mode to_func(in(pred(in, in, out) is det)) = out is det.
to_func(Pred) = Func :- 
        Func = (func(X, Y) = Z :- Pred(X, Y, Z)).

:- func to_pred(func(A, B) = C) = (pred(A,  B,  C)).
:- mode to_pred(in) =          out(pred(in, in, out) is det) is det.
to_pred(Func) = Pred :- 
        Pred = (pred(X::in, Y::in, Z::out) is det :- Func(X, Y) = Z).


:- func flip(func(A, B) = C) = (func(B, A) = C).
:- mode flip(in) = out.
flip(FuncIn) = FuncOut :- 
    FuncOut = (func(B, A) = FuncIn(A, B)).

:- func io_error(io.error) = throws_error(lisp_val(S), S) <= store(S).
io_error(IoError) = Result :-
    Message = io.error_message(IoError),
    Result = error(io_error(Message)).


:- pred open_input_file(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode open_input_file(in, out, di, uo) is det.
open_input_file(List, Result, !IO) :-
  (if      List = [lisp_string(FileName)] then
           io.open_input(FileName, IoRes, !IO),
           (
             IoRes = ok(InputStream),
             Result = ok(lisp_port(input_port(InputStream)))
           ;
             IoRes = error(IoError),
             Result = io_error(IoError)
           )
   else if List = [Huh] then
           Result = error(type_mismatch("string", Huh))
      else 
           Result = error(num_args(integer(1), List))).

:- pred open_output_file(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode open_output_file(in, out, di, uo) is det.
open_output_file(List, Result, !IO) :-
  (if      List = [lisp_string(FileName)] then
           io.open_output(FileName, IoRes, !IO),
           (
             IoRes = ok(OutputStream),
             Result = ok(lisp_port(output_port(OutputStream)))
           ;
             IoRes = error(IoError),
             Result = io_error(IoError)
           )
   else if List = [Huh] then
           Result = error(type_mismatch("string", Huh))
      else 
           Result = error(num_args(integer(1), List))).

:- pred open_port(list(lisp_val(S)), pred(string, lisp_val(S), io, io), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode open_port(in, in(pred(in, out, di, uo) is det), out, di, uo) is det.
open_port(LispVal, OpenFile, Result, !IO) :-
    (if LispVal = [lisp_string(FileName)] then
        OpenFile(FileName, Port, !IO),
        Result = ok(Port)
    else if LispVal = [BadArg] then
            Result = error(type_mismatch("pair", BadArg))
       else
            Result = error(num_args(integer(1), LispVal))).

:- pred close_port(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode close_port(in, out, di, uo) is det.
close_port(LispVal, Result, !IO) :-
    (if LispVal = [lisp_port(Port)] then
        (
            Port = input_port(InputStream),
            io.close_input(InputStream, !IO)
        ;
            Port = output_port(OutputStream),
            io.close_output(OutputStream, !IO)
        ),
        Result = ok(lisp_boolean(yes))
     else Result = ok(lisp_boolean(no))).


:- pred read(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode read(in, out, di, uo) is det.
read(List, Result, !IO) :-
    (if      List = [] then
             read([lisp_port(input_port(io.stdin_stream))], Result, !IO)
     else if List = [lisp_port(input_port(InputStream))] then
             io.read_line_as_string(InputStream, IoResult, !IO),
             (
                IoResult = ok(Line),
                CharList = string.to_char_list(Line),
                read_expr(CharList, ['\n'], Result)
             ;
                IoResult = eof,
                Result = ok(lisp_eof)
             ;
                IoResult = error(IoError),
                Result = io_error(IoError)
             )
     else if List = [LispVal] then
             Result = error(type_mismatch("input port", LispVal))
        else 
             Result = error(num_args(integer(1), List))).

:- pred write(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode write(in, out, di, uo) is det.
write(List, Result, !IO) :-
    (if      List = [LispVal] then
             write([LispVal, lisp_port(output_port(io.stdout_stream))], Result, !IO)
     else if List = [LispVal, lisp_port(output_port(OutputStream))] then
             show(LispVal, String),
             io.write_string(OutputStream, String, !IO),
             Result = ok(lisp_boolean(yes))
     else if List = [_, Huh] then
             Result = error(type_mismatch("input port", Huh))
        else 
             Result = error(num_args(integer(2), List))).
          

:- pred read_file_as_string(string::in, throws_error(string, S)::out, io::di, io::uo) is det <= store(S).
read_file_as_string(FileName, Result, !IO) :-
     io.open_input(FileName, IoRes, !IO),
     (
        IoRes = ok(InputStream),
        io.read_file_as_string(InputStream, ReadRes, !IO),
        (
            ReadRes = ok(String),
            Result = ok(String)
        ;
            ReadRes = error(_, IoError),
            Message = io.error_message(IoError),
            Result = error(io_error(Message))
        )
     ;
        IoRes = error(IoError),
        Message = io.error_message(IoError),
        Result = error(io_error(Message))
     ).
   

% Reads the contents of a file into a string
:- pred read_contents(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode read_contents(in, out, di, uo) is det.
read_contents(List, Result, !IO) :-
    (if      List = [lisp_string(FileName)] then
             read_file_as_string(FileName, Res, !IO),
             (
                Res = ok(String),
                Result = ok(lisp_string(String))
             ;
                Res = error(Error),
                Result = error(Error)
             )
     else if List = [Huh] then
             Result = error(type_mismatch("string", Huh))
        else 
             Result = error(num_args(integer(1), List))).

:- pred load(string, throws_error(list(lisp_val(S)), S), io, io) <= store(S).
:- mode load(in, out(throws_error(list_skel(lisp_val))), di, uo) is det.
load(FileName, Result, !IO) :-
  read_file_as_string(FileName, X, !IO),
  (
    X = ok(String),
    CharList = string.to_char_list(String),
    read_expr_list(CharList, ['\n'], Result)
  ;
    X = error(E), 
    Result = error(E)
  ).
    

:- pred read_all(list(lisp_val(S)), throws_error(lisp_val(S), S), io, io) <= store(S).
:- mode read_all(in, out, di, uo) is det.
read_all(List, Result, !IO) :-
    (if      List = [lisp_string(FileName)] then
             load(FileName, LoadRes, !IO),
             (
               LoadRes = ok(Xs),
               Result = ok(lisp_list(Xs))
             ;
               LoadRes = error(E),
               Result = error(E)
             )
     else if List = [Huh] then
             Result = error(type_mismatch("string", Huh))
        else 
             Result = error(num_args(integer(1), List))).


% ]--


/*
*/

% environment and scope --[

:- pred get_var(environment(S)::in, string::in, throws_error(lisp_val(S), S)::out, S::di, S::uo) is det <= store(S).
get_var(Env, VarName, Result, !Store) :-
    environment.get_var(VarName, Env, Var, !Store),
    (if Var = yes(V) then
        Result = ok(V)
     else 
          Result = error(unbound_var("Getting an unbound variable", VarName))).

/*
:- pred get_var(environment(S)::in, string::in, throws_error(lisp_val(S), S)::out, S::di, S::uo) is det <= store(S).
get_var(Env, VarName, Result, S0, S) :-
    (if   environment.get_var(VarName, Env, Var, S0, S1) then
          S = S1,
          Result = ok(Var)
     else
          S = S0,
          Result = error(unbound_var("Getting an unbound variable", VarName))).
*/

:- pred set_var(environment(S)::in, string::in, lisp_val(S)::in, throws_error(lisp_val(S), S)::out, S::di, S::uo) is det <= store(S).
set_var(Env, VarName, LispVal, Result, !Store) :-
    environment.set_var(VarName, LispVal, Env, Bool, !Store),
    (if Bool = yes then
        Result = ok(LispVal)
    else 
         Result = error(unbound_var("Seting an unbound variable", VarName))).

:- pred define_var(environment(S)::in, string::in, lisp_val(S)::in, S::di, S::uo) is det <= store(S).
define_var(Env, VarName, LispVal, !Store) :-
    environment.define_var(VarName, LispVal, Env, !Store).


%   :- pred new_bound_scope(list({string, lisp_val(S)})::in, scope::out) is det.
%   new_bound_scope(NamesValuePairs, Scope) :-
%       null_scope(Scope0),
%       bind_in_scope(NamesValuePairs, Scope0, Scope).

:- pred bind_in_scope(environment(S), list({string, lisp_val(S)}), S, S) <= store(S).
:- mode bind_in_scope(in, in, di, uo) is det.
bind_in_scope(Env, NamesValuePairs, !Store) :-
    list.foldl(
        (pred(NameValue::in, S0::di, S1::uo) is det :- 
            NameValue = {Name, Value},
            environment.define_var(Name, Value, Env, S0, S1)),
        NamesValuePairs,
        !Store).

% ]--

:- pred read_expr(list(character), list(character), throws_error(lisp_val(S), S)) <= store(S).
:- mode read_expr(in, in, out(throws_error(lisp_val))) is det.
read_expr(CharsIn, CharsOut, Result) :-
        (if   parse_expression(Exp, CharsIn, CharsOut)
         then Result = ok(Exp)
         else Result = error(parser("Cannot parse expression."))).

:- pred read_expr_list(list(character), list(character), throws_error(list(lisp_val(S)), S)) <= store(S).
:- mode read_expr_list(in, in, out(throws_error(list(lisp_val)))) is det.
read_expr_list(CharsIn, CharsOut, Result) :-
        (if   parse_expression_list(Es, CharsIn, CharsOut)
         then Result = ok(Es)
         else Result = error(parser("Cannot parse expression list."))).


:- pred repl(environment(S)::in, S::di, S::uo, io::di, io::uo) is det <= store(S).
repl(RootScope, !Store, !IO) :-
    print("Lisp>>> ", !IO),
    read_line(Res, !IO),
    (  if Res = ok(Line) then 
          read_expr(Line, ['\n'], Either),
          (
               Either = error(Error),
               show(Error, Str),
               print(Str ++ "\n", !IO),
               repl(RootScope, !Store, !IO)
          ;
               Either = ok(Exp),
               eval(RootScope, Exp, Evaled, !Store, !IO),
               (
                   Evaled = ok(NewExp),
                   show(NewExp, Str),  
                   print(Str ++ "\n", !IO),
                   repl(RootScope, !Store, !IO)
               ;
                   Evaled = error(Error),
                   show(Error, Str),  
                   print(Str ++ "\n", !IO),
                   repl(RootScope, !Store, !IO)
               )
          )
     else true).


:- pred primitive_bindings(environment(S)::in, S::di, S::uo) is det <= store(S).
primitive_bindings(Env, !Store) :- 
    all_primitives(Ps),
    bind_in_scope(Env, Ps, !Store),
    all_io_primitives(Is),
    bind_in_scope(Env, Is, !Store).

:- some[S] pred env_init(environment(S)::out, S::uo) is det => store(S).
env_init(Env, Store) :- 
    environment.root_env(Env, Store0),
    primitive_bindings(Env, Store0, Store).

main(!IO) :-
    %print("Hello World!\n", !IO).
    env_init(Env, Store),
    repl(Env, Store, _, !IO).
