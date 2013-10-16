:- module(rtchecks_basic, [
		collapse_prop/4,
		diff_props/3,
		get_predname/4,
		get_globname/3,
		get_propnames/2,
		get_pretty_names/5,
		checkif_to_lit/3,
		get_checkc/4,
		get_checkc/5,
		get_checkif/8,
		get_prop_args/3,
		insert_posloc/7,
		is_member_prop/2,
		is_same_prop/2,
		list_to_lits/2,
		lists_to_lits/2,
		list_to_disj/2,
		lists_to_disj/2,
		remove_element/3,
		inliner_decl/4,
		push_flags/3,
		pop_flags/3
	    ], [assertions, nortchecks, dcg, hiord]).

:- use_module(library(llists)).
:- use_module(library(terms_vars)).
:- use_module(library(hiordlib)).
:- use_module(library(apply)).
:- use_module(library(varnames(apply_dict))).
:- use_module(library(varnames(pretty_names))).
:- use_module(library(varnames(complete_dict))).
:- use_module(library(lists), [append/3]).
:- use_module(rtchecks(term_list)).

:- doc(author, "Edison Mera").

:- doc(module, "Basic predicates used in rtchecks expansion.").

insert_posloc((UsePredLoc, UseAsrLoc), PredName0, PLoc0, ALoc,
	    PosLocs, PredName, PosLoc) :-
	push_meta(PredName0, PosLocs, PredName),
	( UsePredLoc == yes, nonvar(PLoc0) ->
	  push_term(PLoc0,                   PosLocs, PLoc),
	  push_term(predloc(PredName, PLoc), PosLocs, PosPLoc),
	  PosLoc = [PosPLoc|PosLoc1]
	; PosLoc = PosLoc1
	),
	( UseAsrLoc == yes, nonvar(ALoc) ->
	  push_term(asrloc(ALoc), PosLocs, PosALoc),
	  PosLoc1 = [PosALoc]
	; PosLoc1 = []
	).

get_prop_args(Props, Pred, PropArgs) :-
	varset(Pred, Vars),
	map(Props,    varset,               PropVars),
	map(PropVars, intersect_vars(Vars), PropArgs),
	!.

check_props_names(Check, Name, Value, (\+Check, Name=Value)).

compound_checkc(CheckProps0, Name, Props, CheckC) :-
	list_to_disj(CheckProps0, CheckProps),
	CheckC = findall(Name, CheckProps, Props).

get_checkc(_,      [],    _,     [],         true) :- !.
get_checkc(compat, Props, Names, PropValues, CheckC) :-
	compound_check_props(compat, Props, CheckProps0),
	maplist(check_props_names(Name), CheckProps0, Names, CheckProps),
	compound_checkc(CheckProps, Name, PropValues, CheckC).
get_checkc(call, Props, Names, PropValues, CheckC) :-
	compound_check_props(instance, Props, CheckProps0),
	maplist(check_props_names(Name), CheckProps0, Names, CheckProps),
	compound_checkc(CheckProps, Name, PropValues, CheckC).

explicit_fail(Check, PropValues,
	      (\+ Check -> PropValues = [_|_] ; PropValues = [])).

compound_checkc(CheckProps0, PropValues, CheckC) :-
	list_to_lits(CheckProps0, CheckProps),
	explicit_fail(CheckProps, PropValues, CheckC).

get_checkc(_,      [],    [],         true) :- !.
get_checkc(compat, Props, PropValues, CheckC) :-
	compound_check_props(compat, Props, CheckProps0),
	compound_checkc(CheckProps0, PropValues, CheckC).
get_checkc(call, Props, PropValues, CheckC) :-
	compound_check_props(instance, Props, CheckProps0),
	compound_checkc(CheckProps0, PropValues, CheckC).


compound_check_props(Check, Props, CheckProps) :-
	maplist(compound_check_prop(Check), Props, CheckProps).

compound_check_prop(compat(Prop), _, CheckProp) :- !,
	compound_check_prop(Prop, compat, CheckProp).
compound_check_prop(instance(Prop), _, CheckProp) :- !,
	compound_check_prop(Prop, instance, CheckProp).
compound_check_prop(succeeds(Prop), _, Prop) :- !.
compound_check_prop(Prop, Check, CheckProp) :-
	CheckProp =.. [Check, Prop].

compound_checkif(IfValues, ErrType, PredName, Dict, CheckProps, AsrLocs, PropValue,
		 ( IfValues == [] ->
		   findall(PropValue, CheckProps, Props),
		   send_rtcheck(Props, ErrType, PredName, Dict, AsrLocs)
		 ; true
		 )).

% TODO: fail: Exit \= [].  true: Exit == [].

get_checkif(_, _,    _, _, [], _, _, true) :- !.
get_checkif(_, Exit, _, _, _,  _, _, true) :- Exit \= [], !.
get_checkif(success, Exit, PredName, Dict, Props, Names, AsrLoc, CheckIf) :-
	compound_check_props(instance, Props, CheckProps0),
	maplist(check_props_names(NameProp), CheckProps0, Names, CNs),
	list_to_disj(CNs, CheckProps),
	compound_checkif(Exit, success, PredName, Dict, CheckProps, AsrLoc, NameProp, CheckIf).
get_checkif(compatpos, Exit, PredName, Dict, Props, Names, AsrLoc, CheckIf) :-
	compound_check_props(compat, Props, CheckProps0),
	maplist(check_props_names(NameProp), CheckProps0, Names, CNs),
	list_to_disj(CNs, CheckProps),
	compound_checkif(Exit, success, PredName, Dict, CheckProps, AsrLoc, NameProp, CheckIf).

get_predname(short, _,   Pred, F/A)  :- functor(Pred, F, A).
get_predname(long, Dict, Pred, Name) :- pretty_names(Dict, Pred, Name).

get_globname(short, Pred, F/A) :- functor(Pred, F, A).
get_globname(long,  Pred, Pred).

get_propnames(Prop, Prop).

short_prop_name(Prop, Name-[]) :-
	callable(Prop),
	compound(Prop),
	arg(1, Prop, Arg),
	var(Arg),
	Prop =.. [FName, _|Args],
	gnd(Args) ->
	Name =.. [FName|Args]
    ;
	Name = Prop.

short_prop_names(Props, Names) :-
	map(Props, short_prop_name, Names).

propname_name(Name, Name-_).

propdict_name(PropDict, _-PropDict).

long_prop_names(Props, PropNames, Dict, Names) :-
	map(Props,     select_applicable(Dict), PropDicts),
	map(PropNames, propname_name,           Names),
	map(PropDicts, propdict_name,           Names).

% in this predicate, PredName and the name of each property must be ground
% to avoid undesired unifications.
get_pretty_names(short, Term, Dict, TermName, Dict) :-
	Term = n(Pred, Compat, Call, Succ, Comp),
	functor(Pred, F, A),
	PredName = F/A,
	short_prop_names(Compat, CompatName),
	short_prop_names(Call,   CallName),
	short_prop_names(Succ,   SuccName),
	short_prop_names(Comp,   CompName),
	TermName = n(PredName, CompatName, CallName, SuccName, CompName).
get_pretty_names(long, Term, Dict0, TermName, Dict) :-
	Term = n(_Pred, Compat, Call, Succ, Comp),
	complete_dict(Term, Dict0, [], CDict),
	append(Dict0, CDict, Dict),
	apply_dict(Term, Dict, TermName0),
	TermName0 = n(PredName, CompatName0, CallName0, SuccName0, CompName0),
	long_prop_names(Compat, CompatName0, Dict, CompatName),
	long_prop_names(Call,   CallName0,   Dict, CallName),
	long_prop_names(Succ,   SuccName0,   Dict, SuccName),
	long_prop_names(Comp,   CompName0,   Dict, CompName),
	TermName = n(PredName, CompatName, CallName, SuccName, CompName).

% note that the following predicates make partial unification, and comparison
% over the given term: cui(Compare, Unify, Ignore)

diff_props(L,      [], L) :- !. % Minor optimization
diff_props(L1, [H|L2], L3) :- diff_props_2(L1, [H|L2], L3).

diff_props_2([],     _,  []).
diff_props_2([H|L1], L2, L3) :-
	is_member_prop(L2, H),
	!,
	diff_props(L1, L2, L3).
diff_props_2([H|L1], L2, [H|L3]) :-
	diff_props_2(L1, L2, L3).

is_member_prop([T0|_], T1) :-
	is_same_prop(T0, T1),
	!.
is_member_prop([_|L], X) :- is_member_prop(L, X).

is_same_prop(T0, T1) :-
	T0 = cui(C0, U0, _),
	T1 = cui(C1, U1, _),
	C0 == C1,
	% The unification should be done After the comparison, to avoid
	% problems if [U0,U1] share variables with [C0,C1]:
	U0 = U1.

collapse_prop(T0, T1, Es, Es) :-
	is_same_prop(T0, T1),
	!.
collapse_prop(_, cui(_, _, I), [I|Es], Es).

remove_element(A,     _, A) :- var(A), !.
remove_element([],    _, []).
remove_element([X|Y], E, Z0) :-
	(
	    X == E ->
	    Z0 = Z
	;
	    Z0 = [X|Z]
	),
	remove_element(Y, E, Z).

lists_to_lits(A0, L) :-
	flatten(A0, A1),
	remove_element(A1, true, A2),
	list_to_lits(A2, L).

list_to_lits([],     true).
list_to_lits([X|Xs], Lits) :-
	list_to_lits2(Xs, X, Lits).

list_to_lits2([],     X,  X).
list_to_lits2([X|Xs], X0, (X0, Lits)) :-
	list_to_lits2(Xs, X, Lits).

list_to_disj([],     fail).
list_to_disj([X|Xs], Lits) :-
	list_to_disj2(Xs, X, Lits).

list_to_disj2([],     X,  X).
list_to_disj2([X|Xs], X0, (X0 ; Lits)) :-
	list_to_disj2(Xs, X, Lits).

lists_to_disj(A0, L) :-
	flatten(A0, A1),
	remove_element(A1, fail, A2),
	list_to_disj(A2, L).

checkif_to_lit(CheckPosL, Params, CheckPos) :-
	Params = pos(_Pred, PType),
	CheckPosL = i(AsrLoc, PredName, Dict, Compat, CompatNames, Exit),
	get_checkif(PType, Exit, PredName, Dict, Compat, CompatNames, AsrLoc,
		    CheckPos).

push_flags(inline) --> [].
push_flags(yes) --> [(:- push_prolog_flag(single_var_warnings, off))].

pop_flags(inline) --> [].
pop_flags(yes) --> [(:- pop_prolog_flag(single_var_warnings))].

inliner_decl(yes,    _) --> [].
inliner_decl(inline, Pred) -->
	{functor(Pred, F, A)},
	inline_decl(F, A),
	unfold_decl(F, A).

inline_decl(F, A) -->
	[(:- inline(F/A))].

unfold_decl(F, A) -->
	{
	    functor(Unfold, F, A),
	    (compound(Unfold) -> fill_struct(1, Unfold, yes) ; true)
	},
	[(:- unfold(Unfold))].

fill_struct(N, Unfold, Value) :-
	arg(N, Unfold, Value) ->
	N1 is N + 1,
	fill_struct(N1, Unfold, Value)
    ;
	true.
