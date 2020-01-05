:- module environment.

:- interface.
:- import_module adts.
:- import_module map, string, store, maybe, bool.

% todo rename to scope

:- type mappings(S) == generic_mutvar(map(string, lisp_val(S)), S).

:- type environment(S) ---> root_scope(mappings(S))
                          ; child_scope(environment(S), mappings(S)).

:- some[S] pred root_env(environment(S)::out, S::uo) is det => store(S).

:- pred add_scope(environment(S)::in, environment(S)::out, S::di, S::uo) is det <= store(S).
:- pred define_var(string::in, lisp_val(S)::in, environment(S)::in, S::di, S::uo) is det <= store(S).
%:- pred get_var(string::in, environment(S)::in, lisp_val(S)::out, S::di, S::uo) is semidet <= store(S).
:- pred get_var(string::in, environment(S)::in, maybe(lisp_val(S))::out, S::di, S::uo) is det <= store(S).
:- pred set_var(string::in, lisp_val(S)::in, environment(S)::in, bool::out, S::di, S::uo) is det <= store(S).

:- implementation.

% "private" predicates and functions
:- func get_mappings(environment(S)) = mappings(S) <= store(S).
get_mappings(root_scope(M)) = M.
get_mappings(child_scope(_, M)) = M.

:- pred parent(environment(S)::in, environment(S)::out) is semidet <= store(S).
parent(child_scope(Parent, _), Parent).

% exported predicates and functions

root_env(Environment, Store) :-
    store.init(Store0),
    store.new_mutvar(map.init, Mappings, Store0, Store),
    Environment = root_scope(Mappings).
    
add_scope(Parent, Child, !Store) :-
    store.new_mutvar(map.init, Mappings, !Store),
    Child = child_scope(Parent, Mappings).


define_var(VarName, LispVal, Env, !Store) :-
    Mappings = get_mappings(Env),
    store.get_mutvar(Mappings, Map, !Store),
    NewMap = map.set(Map, VarName, LispVal),
    store.set_mutvar(Mappings, NewMap, !Store).

/*
get_var(VarName, Env, LispVal, !Store) :-
    get_mappings(Env) = Mappings,
    store.get_mutvar(Mappings, Map, !Store),
    (if   map.search(Map, VarName, Var) then
          LispVal = Var
     else
          parent(Env, Parent),
          get_var(VarName, Parent, LispVal, !Store)).
*/

get_var(VarName, Env, LispVal, !Store) :-
    get_mappings(Env) = Mappings,
    store.get_mutvar(Mappings, Map, !Store),
    (if      map.search(Map, VarName, Var) then
             LispVal = yes(Var)
     else if parent(Env, Parent) then
             get_var(VarName, Parent, LispVal, !Store)
        else 
             LispVal = no).

/*

set_var(VarName, LispVal, Env, !Store) :-
    get_mappings(Env) = Mappings,
    store.get_mutvar(Mappings, Map, !Store),
    (if  map.update(VarName, LispVal, Map, NewMap) then
         store.set_mutvar(Mappings, NewMap, !Store)
    else
         parent(Env, Parent),
         set_var(VarName, LispVal, Parent, !Store)).
*/

set_var(VarName, LispVal, Env, Bool, !Store) :-
    get_mappings(Env) = Mappings,
    store.get_mutvar(Mappings, Map, !Store),
    (if     map.update(VarName, LispVal, Map, NewMap) then
            store.set_mutvar(Mappings, NewMap, !Store),
            Bool = yes
    else if parent(Env, Parent) then
            set_var(VarName, LispVal, Parent, Bool, !Store)
       else
            Bool = no).
