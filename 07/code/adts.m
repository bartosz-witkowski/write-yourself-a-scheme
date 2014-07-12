:- module adts.

:- interface.

:- import_module string, list, integer, bool.

:- type lisp_val ---> lisp_atom(string)
                    ; lisp_list(list(lisp_val))
                    ; lisp_dotted_list(list(lisp_val), lisp_val)
                    ; lisp_number(integer)
                    ; lisp_string(string)
                    ; lisp_bool(bool.bool)
                    .
