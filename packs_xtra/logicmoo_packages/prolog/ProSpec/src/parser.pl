%% ----------------------------------------
%% File: parser.pl
%% Author: Peter Baumgartner
%% ----------------------------------------
%% build disjunctive literal list from prolog clauses 
%% ----------------------------------------
%% $Id: parser.pl,v 1.2 1999/04/14 08:40:30 peter Exp $
%% $Log: parser.pl,v $
%% Revision 1.2  1999/04/14 08:40:30  peter
%% Added foreign term 'dp_flag'
%%
%% Revision 1.1  1998/01/16 14:51:00  bthomas
%% Initial revision
%%
%% 
%% Changes:
%%  15.2.96 :added pred t_list for prospec interface 
%%            benno =:-]
%% ----------------------------------------
 
:- dynamic(ptemplate/1),
   dynamic(template/1),
   dynamic(inf_rule/1),
   dynamic(ht_flag/2),
   dynamic(ground_clause_set/0),
   dynamic(constant/1),
   dynamic(pred_sym/1).

init_parser :-
	retract_preds_dynamic,
	retract_all(ptemplate(_)),
	retract_all(ht_flag(_,_)),
	retract_all(template(_)),
	retract_all(constant(_)),
	retract_all(ground_clause_set),
	assert(ground_clause_set),
	retract_all(inf_rule(_)).

consult_file(File) :-

	(string(File) -> atom_string(TmeFilename,File)
    ;
	concat_atoms(File, '.tme', TmeFilename) ),

	open(TmeFilename, read, TmeFile),
	repeat,
	read(TmeFile, Term),
	translate_cut(Term),
	Term == end_of_file,
	!,
	(constant(_C) ; assert(constant(any))),
	close(TmeFile),
        true.

translate_cut(Term) :- translate(Term), !.

translate(end_of_file) :- !.

%translate(mission_flag(Name, Value) ) :- !, flag(Name, Value).

translate(read(File) ) :- !, consult_file(File).

translate(Term) :- 
	protein_term(Term), 
	!, 
	nl.

translate(Term) :- 

	mission_term(Term), 
	!, 
	nl.

translate( (?- Goal) ) :- !,
	pl_list_con(Goal, GoalList),
	negate_list(GoalList, OI_clause), 
	normalize_assert(OI_clause,query).

% had to disable, since in conflict with pl1-formulas
%translate( (P -> C) ) :- !,
%	pl_list(P, PL),
%	norm_list(PL,PLN),
%	norm_list([C],[CN]),
%	(mydelete((-Goal),PLN,Rest)
%          -> assert(inf_rule((-Goal, Rest -> CN)))
%          ; true).

translate( (Head :- Body) ) :- !,

	pl_list(Head, HeadList), 
	pl_list(Body, BodyList), 

	negate_list(BodyList, NBodyList), 
	append(HeadList, NBodyList, OI_clause),
	normalize_assert(OI_clause,input).

translate(Clause) :-  % should be ';' separated list of literals

	pl_list_dis(Clause, OI_clause), 
	normalize_assert(OI_clause,input).

translate(Clause) :-  %  ',' separated list of literals
	pl_list_con(Clause, _OI_clause), 
	printf("Syntax error: Expression %w  not allowed.  Skipped\n",[Clause]). 

%% ========================================
%% prospec interface preds
%% ========================================

% t_list similiar to translate
% ... quick and dirty =|-|

t_list( (?- Goal) , OI_clause, query) :- !,
	pl_list_con(Goal, GoalList),
	negate_list(GoalList, OI_clause).

t_list( (Head :- Body),OI_clause, input) :- !,
	pl_list(Head, HeadList), 
	pl_list(Body, BodyList), 
	negate_list(BodyList, NBodyList), 
	append(HeadList, NBodyList, OI_clause).

t_list(Clause,OI_clause,input) :-  
	pl_list_dis(Clause, OI_clause).

t_list(Clause, _OI_clause,none) :-  
	pl_list_con(Clause, _OI_clause), 
	printf("Syntax error: Expression %w  not allowed. Skipped\n",[Clause]). 
%% ----------------------------------------
%%  end interface
%% ----------------------------------------

foreign_term(dp_flag(_, _)).
foreign_term(eqtrafo_flag(_, _)).
foreign_term(ht_flag(_, _)).
foreign_term(protein_flag(_, _)).
foreign_term(pttp_flag(_, _)).
foreign_term(calculus(_)).
foreign_term(depth_increment(_)).
foreign_term(theory_costs(_,_)).
foreign_term(prolog_pred(_)).
foreign_term(protein_answer(_)).
foreign_term(mod_flag(_,_)).
foreign_term(begin(_)).
foreign_term(end(_)).
foreign_term(mission_flag(_, _)).


analyze_clause([]).
analyze_clause([-Lit|Rest]) :- !,
	analyze_lit(Lit), 
	analyze_clause(Rest), !.
analyze_clause([Lit|Rest]) :-
	analyze_lit(Lit), 
	analyze_clause(Rest), !.

analyze_lit(L) :- var(L), !.
analyze_lit(L) :-
	functor(L,F,N),
	(pred_sym(F / N) ; assert(pred_sym(F / N))),
	L =.. [_P|Args],
	analyze_term_list(Args).

analyze_term_list([]).
analyze_term_list([T|R]) :-
        retract_all(ground_clause_set),
        analyze_term(T),
	analyze_term_list(R).

analyze_term(X) :- var(X), !.
analyze_term(T) :- 
	functor(T,F,N),
	(N = 0 ->
	    (constant(F) ; assert(constant(F)))
         ;  template_args(N,TemplateArgs),
	    Template =.. [F|TemplateArgs],
	    (template(Template) ; assert(template(Template)))),
	T =.. [_F|Args],
	analyze_term_list(Args).
	     

template_args(0,[]).
template_args(1,[_A_1]).
template_args(2,[_A_1,_A_2]).
template_args(3,[_A_1,_A_2,_A_3]).
template_args(4,[_A_1,_A_2,_A_3,_A_4]).
template_args(5,[_A_1,_A_2,_A_3,_A_4,_A_5]).
template_args(6,[_A_1,_A_2,_A_3,_A_4,_A_5,_A_6]).
template_args(7,[_A_1,_A_2,_A_3,_A_4,_A_5,_A_6,_A_7]).
template_args(8,[_A_1,_A_2,_A_3,_A_4,_A_5,_A_6,_A_7,_A_8]).

normalize_assert(Clause,Type) :-
        norm_clause(Clause,NC),
	(ht_flag(polarity_switch,on) -> 
	    polarity_switch(NC,NCP)
	  ; NCP = NC),
	analyze_clause(NCP),
	assert(oi_clause(NCP,Type)).

polarity_switch([],[]).
polarity_switch([L|R],[L1|R1]) :- negate(L,L1), polarity_switch(R,R1).

premisses( (Prem, More), Norm) :- !,

	positive_literal(Prem, Next, Norm),
	premisses(More, Next).

premisses(Prem, Norm) :- positive_literal(Prem, [], Norm).


conclusio( (Conc, More), Pres, Norm) :- !,

	negative_literal(Conc, Next, Norm),
	conclusio(More, Pres, Next).

conclusio( (Conc; More), Pres, Norm) :- !,

	negative_literal(Conc, Next, Norm),
	conclusio(More, Pres, Next).

conclusio(Conc, Pres, Norm) :- negative_literal(Conc, Pres, Norm).


positive_literal(true, Norm, Norm) :- !.

positive_literal(~ Lit, More, [- Lit | More]) :- !.

positive_literal(Lit, More, [Lit | More]).


negative_literal(false, Norm, Norm) :- !.

negative_literal(~ Lit, More, [Lit | More]) :- !.

negative_literal(Lit, More, [Neg | More]) :- negate(Lit, Neg).


retract_preds_dynamic :-
	pred_sym(P/N),
%	template_args(N,L),
%	Pred =.. [P|L],
%	retract_all(Pred), 
	retract(pred_sym(P/N)),
	fail.
retract_preds_dynamic.

make_preds_dynamic :-
	pred_sym(P/N),
	template_args(N,L),
	Pred =.. [P|L],
	translate((~Pred, Pred -> false)),
	fail.
make_preds_dynamic.


