
:- module(bar,[]).

:- ensure_loaded(library(pfc_test)).
% :- set_prolog_flag(pfc_version,1.8).
:- set_prolog_flag(pfc_version,v(1,8,2)).

:- expects_dialect(pfc).

a(1).
a(2).

%:- trace.
a(2) <==> b(1).

:- listing(b/1).


