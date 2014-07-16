:- module adts.

:- interface.

:- import_module string, list, integer, bool.
:- import_module show.

:- type lisp_val ---> lisp_atom(string)
                    ; lisp_list(list(lisp_val))
                    ; lisp_dotted_list(list(lisp_val), lisp_val)
                    ; lisp_number(integer)
                    ; lisp_string(string)
                    ; lisp_bool(bool.bool)
                    .
:- instance show(lisp_val).

:- implementation.

:- instance show(lisp_val) where [
  pred(show/2) is show_lisp_val
].

:- pred show_lisp_val(lisp_val::in, string::out) is det.
show_lisp_val(lisp_string(String), show(String)).
show_lisp_val(lisp_number(Integer), show(Integer)).
show_lisp_val(lisp_atom(Name), Name).
show_lisp_val(lisp_bool(yes), "#t").
show_lisp_val(lisp_bool(no),  "#f").
show_lisp_val(lisp_list(List), String) :-
  lisp_val_list_to_string_list(List, String_List),
  unwords(String_List, Single_String),
  String = "(" ++ Single_String ++ ")".
show_lisp_val(lisp_dotted_list(List, Last), String) :-
  lisp_val_list_to_string_list(List, String_List),
  unwords(String_List, List_String),
  show_lisp_val(Last, Last_String),
  String = "(" ++ List_String ++ " . " ++ Last_String ++ ")".
  
:- pred lisp_val_list_to_string_list(list(lisp_val)::in, list(string)::out) is det.
lisp_val_list_to_string_list(Val_List, String_List) :- 
  lisp_val_list_to_string_list_aux(Val_List, [], String_List).

:- pred lisp_val_list_to_string_list_aux(list(lisp_val)::in, list(string)::in, list(string)::out) is det.
lisp_val_list_to_string_list_aux([], Acc, list.reverse(Acc)).
lisp_val_list_to_string_list_aux([H | T], Acc, String_List) :- 
  show_lisp_val(H, String),
  lisp_val_list_to_string_list_aux(T, [String | Acc], String_List).
