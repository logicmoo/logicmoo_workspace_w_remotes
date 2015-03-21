:- module(rtchecks, []).

% A wrapper for the Ciao Runtime-Checks library
:- reexport(library(swi/rtchecks_lib)).
:- use_module(library(compound_expand)).
:- expects_dialect(ciao).
:- use_package(rtchecks).
:- use_module(rtchecks(rtchecks_tr)).
:- set_prolog_flag(runtime_checks, yes).

% To allow usage of rtchecks in normal SWI programs:

goal_expansion(Goal0, Goal) :-
    '$set_source_module'(M, M),
    rtchecks_goal_tr(Goal0, Goal, M),
    !.

term_expansion((:- include(library(nativeprops))),
	       (:- use_module(library(assertions/native_props)))) :- !.
term_expansion(Term0, Term) :-
    '$set_source_module'(M, M),
    rtchecks_sentence_tr(Term0, Term, M, []),
    !.
