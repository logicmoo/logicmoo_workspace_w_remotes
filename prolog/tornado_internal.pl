/* tornado predicates

This module performs reasoning over probabilistic description logic knowledge bases.
It reads probabilistic knowledge bases in RDF format or in Prolog format, a functional-like
sintax based on definitions of Thea library, and answers queries by finding the set 
of explanations or computing the probability.

[1] http://vangelisv.github.io/thea/

See https://github.com/rzese/trill/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~rzese/software/trill/manual.html for
details.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

/********************************
  SETTINGS
*********************************/
:- multifile setting_trill/2.
setting_trill(det_rules,[and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule]).
setting_trill(nondet_rules,[or_rule]).

set_up(M):-
  utility_translation:set_up(M),
  M:(dynamic exp_found/2, keep_env/0, tornado_bdd_environment/1).

clean_up(M):-
  utility_translation:clean_up(M),
  M:(dynamic exp_found/2, keep_env/0, tornado_bdd_environment/1),
  retractall(M:exp_found(_,_)),
  retractall(M:keep_env),
  retractall(M:tornado_bdd_environment(_)).

/*****************************
  MESSAGES
******************************/
:- multifile prolog:message/1.

prolog:message(or_in_or) -->
  [ 'Boolean formula wrongly built: or in or' ].

prolog:message(and_in_and) -->
  [ 'Boolean formula wrongly built: and in and' ].

/****************************
  QUERY PREDICATES
*****************************/

/***********
  Utilities for queries
 ***********/

% findall
find_n_explanations(M,QueryType,QueryArgs,Expls,_,Opt):- % This will not check the arg max_expl as TRILLP returns a pinpointing formula
 assert(M:keep_env),
 find_single_explanation(M,QueryType,QueryArgs,Expls,Opt),!.

find_n_explanations(_,_,_,Expls,_,_):-
 empty_expl(_,Expls).

compute_prob_and_close(M,Exps,Prob):-
  compute_prob(M,Exps,Prob),
  retractall(M:keep_env),!.

% checks the explanation
check_and_close(M,Expl,Expl):-
  M:keep_env,!.

check_and_close(M,Expl,dot(Dot)):-
  get_bdd_environment(M,Env),
  create_dot_string(Env,Expl,Dot),
  clean_environment(M,Env).



% checks if an explanations was already found
find_expls(M,[],_,BDD):-
  get_bdd_environment(M,Env),
  one(Env,BDD),!.

% checks if an explanations was already found (instance_of version)
find_expls(M,[Tab|T],[C,I],E):-
  findall(E0,clash(M,Tab,E0),Expls0),!,
  or_all_f(M,Expls0,Expls1),
  find_expls(M,T,[C,I],E1),
  and_f(M,Expls1,E1,E).

% checks if an explanations was already found (property_value version)
find_expls(M,[Tab|T],[PropEx,Ind1Ex,Ind2Ex],E):-
  get_abox(Tab,ABox),
  findall(E0,find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),E0),ABox),Expls0),!,
  or_all_f(M,Expls0,Expls1),
  find_expls(M,T,[PropEx,Ind1Ex,Ind2Ex],E1),
  and_f(M,Expls1,E1,E).
  

find_expls(M,[_Tab|T],Query,Expl):-
  \+ length(T,0),
  find_expls(M,T,Query,Expl).


/****************************/

/****************************
  TABLEAU ALGORITHM
****************************/

% --------------
findClassAssertion4OWLNothing(M,ABox,Expl):-
  findall(Expl1,findClassAssertion('http://www.w3.org/2002/07/owl#Nothing',_Ind,Expl1,ABox),Expls),
  dif(Expls,[]),
  or_all_f(M,Expls,Expl).

/* ************* */

/***********
  update abox
  utility for tableau
************/
modify_ABox(M,Tab0,C,Ind,L0,Tab):-
  get_abox(Tab0,ABox0),
  findClassAssertion(C,Ind,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(M,L0,Expl1,Expl),
  remove_from_abox(ABox0,(classAssertion(C,Ind),Expl1),ABox),
  set_abox(Tab0,[(classAssertion(C,Ind),Expl)|ABox],Tab).
  
modify_ABox(_,Tab0,C,Ind,L0,Tab):-
  get_abox(Tab0,ABox0),
  set_abox(Tab0,[(classAssertion(C,Ind),L0)|ABox0],Tab).


modify_ABox(M,Tab0,P,Ind1,Ind2,L0,Tab):-
  get_abox(Tab0,ABox0),
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(M,L0,Expl1,Expl),
  remove_from_abox(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox),
  set_abox(Tab0,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox],Tab).
  
  
modify_ABox(_,Tab0,P,Ind1,Ind2,L0,Tab):-
  get_abox(Tab0,ABox0),
  set_abox(Tab0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0],Tab).

/* ************* */


/*
  build_abox
  ===============
*/

build_abox(M,Tableau):-
  retractall(M:final_abox(_)),
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  get_bdd_environment(M,Env),
  findall((classAssertion(Class,Individual),BDDCA),(M:classAssertion(Class,Individual),bdd_and(M,Env,[classAssertion(Class,Individual)],BDDCA)),LCA),
  findall((propertyAssertion(Property,Subject, Object),BDDPA),(M:propertyAssertion(Property,Subject, Object),dif('http://www.w3.org/2000/01/rdf-schema#comment',Property),bdd_and(M,Env,[propertyAssertion(Property,Subject, Object)],BDDPA)),LPA),
  % findall((propertyAssertion(Property,Subject,Object),*([subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)])),subProp(M,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),M:classAssertion(oneOf(_),NominalIndividual),LNA),
  findall((differentIndividuals(Ld),BDDDIA),(M:differentIndividuals(Ld),bdd_and(M,Env,[differentIndividuals(Ld)],BDDDIA)),LDIA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  init_tableau(ABox0,Tabs0,Tableau0),
  create_tabs(LCA,Tableau0,Tableau1),
  append([LCA,LPA,LNA,LDIA],AddAllList),
  add_all_to_tableau(AddAllList,Tableau1,Tableau2),
  append([LDIA,LPA],CreateTabsList),
  create_tabs(CreateTabsList,Tableau2,Tableau3),
  findall((sameIndividual(L),BDDSIA),(M:sameIndividual(L),bdd_and(M,Env,[sameIndividual(L)],BDDSIA)),LSIA),
  merge_all_individuals(M,LSIA,Tableau3,Tableau4),
  add_owlThing_list(M,Tableau4,Tableau),
  !.

/**********************

Explanation Management

***********************/

initial_expl(M,BDD):-
  get_bdd_environment(M,Env),
  zero(Env,BDD).

empty_expl(M,BDD):-
  get_bdd_environment(M,Env),
  one(Env,BDD).

and_f_ax(M,Axiom,BDD0,BDD):-
  get_bdd_environment(M,Env),
  bdd_and(M,Env,[Axiom],BDDAxiom),
  and_f(M,BDDAxiom,BDD0,BDD).

% and between two BDDs
and_f(_,[],BDD,BDD):- !.

and_f(_,BDD,[],BDD):- !.

and_f(M,BDD0,BDD1,BDD):-
  get_bdd_environment(M,Env),
  and(Env,BDD0,BDD1,BDD).


% or between two formulae
or_all_f(M,[],BDD):-
  initial_expl(M,BDD),!.

or_all_f(M,[H|T],Expl):-
  or_all_f(M,T,Expl1),
  or_f(M,H,Expl1,Expl),!.

or_f(_,[],BDD,BDD):- !.
  
or_f(_,BDD,[],BDD):- !.
  
or_f(M,BDD0,BDD1,BDD):-
  get_bdd_environment(M,Env),
  or(Env,BDD0,BDD1,BDD).


/**********************

TRILLP SAT TEST

***********************/

test(M,L1,L2,F):-
  %build_f(L1,L2,F),
  %sat(F).
  or_f(M,L1,L2,F),
  dif(L2,F).


/**********************

Choice Points Management

***********************/

get_choice_point_id(_,0).

create_choice_point(_,_,_,_,_,0).

add_choice_point(_,_,Expl,Expl):- !.


/**********************

 TORNADO Probability Computation

***********************/

get_bdd_environment(M,Env):- 
  M:tornado_bdd_environment(Env),!.

get_bdd_environment(M,Env):-
  init(Env),
  M:assert(tornado_bdd_environment(Env)).

clean_environment(M,Env):-
  end(Env),
  retractall(M:tornado_bdd_environment(_)).

build_bdd(_,Env,[],BDD):- !,
  zero(Env,BDD).

build_bdd(_,_Env,BDD,BDD).

bdd_and(M,Env,[X],BDDX):-
  get_prob_ax(M,X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(_M,Env,[_X],BDDX):- !,
  one(Env,BDDX).
