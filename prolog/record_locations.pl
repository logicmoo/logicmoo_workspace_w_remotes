:- module(record_locations, []).

:- use_module(library(apply)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4.

:- volatile rl_tmp/3.
:- dynamic rl_tmp/3. % trick to detect if term_expansion was applied

% Extra location for assertions of a given predicate
extra_location:extra_location(Head, M, assertion(Status, Type), From) :-
    clause(assrt_lib:assertion_head(Head, M, Status, Type, _, _, From), _).

:- multifile skip_record_decl/1.

skip_record_decl(initialization(_)) :- !.
skip_record_decl(Decl) :-
    nonvar(Decl),
    '$set_source_module'(M, M),
    predicate_property(M:Decl, imported_from(assrt_lib)),
    functor(Decl, Type, Arity),
    memberchk(Arity, [1, 2]),
    assrt_lib:assrt_type(Type), !.

:- public record_extra_location/2.

record_extra_location((:- Decl),
		      term_position(_, _, _, _, [DPos])) :-
    ( \+ skip_record_decl(Decl)
    ->record_extra_decl(Decl, DPos)
    ; true
    ).

record_extra_decl(Decl, DPos) :-
    '$set_source_module'(SM, SM),
    declaration_pos(Decl, DPos, SM, M, IdL, ArgL, PosL),
    maplist(assert_declaration(M), IdL, ArgL, PosL),
    !.
record_extra_decl(G, _) :-
    nonvar(G),
    source_location(File, Line),
    retractall(rl_tmp(File, Line, _)),
    asserta(rl_tmp(File, Line, 1)).

declaration_pos(module(M, L), DPos,
		_, M, [module_2, export], [module(M, L), L], [DPos, Pos]) :-
    DPos = term_position(_, _, _, _, [_, Pos]).
declaration_pos(volatile(L), term_position(_, _, _, _, PosL),
		M, M, [volatile], [L], PosL).
declaration_pos(dynamic(L), term_position(_, _, _, _, PosL),
		M, M, [dynamic], [L], PosL).
declaration_pos(reexport(U, L), term_position(_, _, _, _, [_, Pos]),
		M, M, [reexport(U)], [L], [Pos]).
declaration_pos(public(L), term_position(_, _, _, _, PosL),
		M, M, [pulic], [L], PosL).
declaration_pos(multifile(L), term_position(_, _, _, _, PosL),
		M, M, [multifile], [L], PosL).
declaration_pos(discontiguous(L), term_position(_, _, _, _, PosL),
		M, M, [discontiguous], [L], PosL).
declaration_pos(meta_predicate(L), term_position(_, _, _, _, PosL),
		M, M, [meta_predicate], [L], PosL).
declaration_pos(reexport(SM:DU),  DPos,  _, M, ID, U, Pos) :- !,
    declaration_pos(reexport(DU), DPos, SM, M, ID, U, Pos).
declaration_pos(use_module(SM:DU),  DPos,  _, M, ID, U, Pos) :- !,
    declaration_pos(use_module(DU), DPos, SM, M, ID, U, Pos).
declaration_pos(use_module(SM:DU, L), DPos, ID, _, M, U, Pos) :- !,
    declaration_pos(use_module(DU, L), DPos, ID, SM, M, U, Pos).
declaration_pos(include(U),    DPos, M, M, [include],    [U], [DPos]).
declaration_pos(use_module(U), DPos, M, M, [use_module], [U], [DPos]).
declaration_pos(reexport(U),   DPos, M, M, [reexport],   [U], [DPos]).
declaration_pos(consult(U),    DPos, M, M, [consult],    [U], [DPos]).
declaration_pos(use_module(U, L), DPos, M, M,
		[use_module_2, import(U)], [use_module(U, L), L], [DPos, Pos]) :-
    DPos = term_position(_, _, _, _, [_, Pos]).

:- meta_predicate mapsequence(2,?,?).
mapsequence(_, A, _) :-
    var(A),
    !.
    % call(G, A).
mapsequence(_, [], _) :- !.
mapsequence(G, [E|L], list_position(_, _, PosL, _)) :- !,
    maplist(mapsequence(G), [E|L], PosL).
mapsequence(G, (A, B), term_position(_, _, _, _, [PA, PB])) :- !,
    mapsequence(G, A, PA),
    mapsequence(G, B, PB).
mapsequence(G, A, PA) :-
    call(G, A, PA).

assert_declaration(M, Declaration, Sequence, Pos) :-
    mapsequence(assert_declaration_one(Declaration, M), Sequence, Pos).

assert_declaration_one(reexport(U), M, PI, Pos) :- !,
    assert_reexport_declaration_2(PI, U, Pos, M).
assert_declaration_one(Declaration, _, M:PI,
		       term_position(_, _, _, _, [_, Pos])) :- !,
    assert_declaration_one(Declaration, M, PI, Pos).
assert_declaration_one(Declaration, M, F/A, Pos) :- !,
    functor(H, F, A),
    assert_position(H, M, Declaration, Pos).
assert_declaration_one(Declaration, M, F//A0, Pos) :- !,
    A is A0+2,
    functor(H, F, A),
    assert_position(H, M, Declaration, Pos).
assert_declaration_one(Declaration, M, H, Pos) :-
    assert_position(H, M, Declaration, Pos).

assert_reexport_declaration_2((F/A as G), U, Pos, M) :-
    functor(H, G, A),
    assert_position(H, M, reexport(U, [F/A as G]), Pos).
assert_reexport_declaration_2(F/A, U, Pos, M) :-
    functor(H, F, A),
    assert_position(H, M, reexport(U, [F/A]), Pos).
assert_reexport_declaration_2(op(_, _, _), _, _, _).
assert_reexport_declaration_2(except(_),   _, _, _).

assert_position(H, M, Type, Pos) :-
    source_location(File, Line),
    ( nonvar(Pos)
    ->From = file_term_position(File, Pos)
    ; From = file(File, Line, -1, 0)
    ),
    assert_location(H, M, Type, From).

assert_location(H, M, Type, From) :-
    ( \+ have_extra_location(From, H, M, Type)
    ->compile_aux_clauses(extra_location:extra_location(H, M, Type, From))
    ; true
    ).

/*
have_extra_location(file(File, Line, _, _), H, M, Type) :- !,
    extra_location(H, M, Type, From),
    from_to_file(From, File),
    from_to_line(From, Line).
have_extra_location(From, H, M, Type) :-
    extra_location(H, M, Type, From).
*/

have_extra_location(From0, H, M, Type) :-
    extra_location(H, M, Type, From),
    subsumes_from(From0, From).

system:term_expansion(Term, Pos, _, _) :-
    % format(user_error, '~q~n',[Term]),
    source_location(File, Line),
    ( rl_tmp(File, Line, _)
    ->true
    ; retractall(rl_tmp(_, _, _)),
      asserta(rl_tmp(File, Line, 0 )),
      record_extra_location(Term, Pos)
    ),
    fail.

redundant((_,_)).
redundant((_;_)).
redundant((_:_)).
redundant(true).
redundant(!).

assert_position(G, Pos, T) :-
    '$set_source_module'(M, M),
    assert_position(G, M, T, Pos).

:- public rl_goal_expansion/2.
rl_goal_expansion(Goal, Pos) :-
    callable(Goal),
    \+ redundant(Goal),
    source_location(File, Line),
    ( rl_tmp(File, Line, Flag)
    ->Flag == 1
    ; true
    ),
    \+ clause(declaration_pos(Goal, _, _, _, _, _, _), _),
    assert_position(Goal, Pos, goal),
    !.

system:goal_expansion(Goal, Pos, _, _) :-
    rl_goal_expansion(Goal, Pos),
    fail.
