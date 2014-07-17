:- module hello_world.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

main(Io_In, Io_Out) :-
    io.write_string("Hello world!\n", Io_In, Io_Out).
