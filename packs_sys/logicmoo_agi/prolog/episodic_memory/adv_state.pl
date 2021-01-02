/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- ensure_loaded('adv_props').

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_main_states')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


save_term_exists(Filename, Term) :-
 absolute_file_name(Filename, AbsFilename), !, Filename \== AbsFilename,
 save_term_exists(AbsFilename, Term), !.

save_term_exists(Filename, Term) :-
 \+ access_file(Filename, exist),
 save_term(Filename, write, Term),
 player_format('Saved to file "~w".~n', [Filename]),!.
save_term_exists(Filename, _) :-
 access_file(Filename, exist),
 player_format('Save FAILED! Does file "~w" already exist?~n', [Filename]).
save_term_exists(Filename, _) :-
 player_format('Failed to open file "~w" for saving.~n', [Filename]).


record_saved(State) :- is_list(State),!,
  sort(State,Term),  
  with_mutex(save_advstate, must_mw1(save_term(library('episodic_memory/adv_state_db.pl'), write ,Term))),!.
record_saved(State):- 
  with_mutex(save_advstate, must_mw1(save_term(library('episodic_memory/adv_state_db.pl'), append ,State))).

save_term(Filename, How, Term) :- 
  is_list(Term),sort(Term,STerm),Term\==STerm,!,
  save_term(Filename, How, STerm).
save_term(Filename, How, Term) :-
 absolute_file_name(Filename, AbsFilename),
 setup_call_cleanup(open(AbsFilename, How, FH),
  format(FH, '~N~@.~N', [mu:adv_pretty_print(Term)]), 
  close(FH)),!.
save_term(Filename, How, _) :-
 player_format('Failed to ~w file "~w" for saving.~n', [How, Filename]),!.



:- multifile(extra_decl/2).
:- dynamic(extra_decl/2).
:- dynamic(undo/2).
%undo([u, u, u, u, u, u, u, u]).
:- dynamic(advstate_db/1).
advstate_db([]).

is_pred1_state(advstate_db).
is_pred1_state(istate).
is_pred1_state(statest).
is_pred1_state(parseFrame(_)).


:- nb_setval(advstate_var, []).
:- thread_initialization(nb_setval(advstate_var, [])).

get_advstate_varname(Varname):- nb_current(advstate_var, Varname), Varname\==[], !.
% get_advstate_varname(advstate_db).

get_advstate_db(State):- var(State), !, findall(State1, advstate_db(State1), StateL), flatten([structure_label(cur_state),StateL], State).
get_advstate_db(State):- advstate_db(State).

into_ref(State,State):-!.
into_ref(State,StateRef):- notrace(get_attr(State,refValue,_)->StateRef=State;put_attr(StateRef,refValue,State)).
from_ref(State,State):-!.
from_ref(StateRef,State):- (var(State)->get_attr(StateRef,refValue,State);StateRef=State).

refValue_unify_hook(State,Value):-  State=Value.
refValue:attr_unify_hook(State,Value):- refValue_unify_hook(State,Value).
refValue:attribute_goals(Var)--> 
  {get_attr(Var,refValue,Value),gensym(store_at_,Store),nb_setval(Store,Value)},
  [restore_refValue(Var,Store)].

restore_refValue(Var,Store):- nb_getval(Store,Value),put_attr(Var,refValue,Value). 

get_advstate(StateRef):- notrace((get_advstate_0(State), into_ref(State,StateRef))).
get_advstate_0(State):- with_mutex(get_advstate,
  ((get_advstate_varname(Var), nb_current(Var, PreState), PreState\==[]) -> State=PreState ; get_advstate_db(State))).

/*add_advstate(State):- from_ref(State,State0),
  with_mutex(get_advstate, must_mw1(set_advstate_db_1(State0))),
  nop(with_mutex(save_advstate, must_mw1(save_term(library('episodic_memory/adv_state_db.pl'), append ,State0)))),!.
*/


set_advstate(StateRef):- notrace((from_ref(StateRef,State), set_advstate_0(State))).

set_advstate_0(State):- 
  with_mutex(get_advstate,((get_advstate_varname(Var), nb_current(Var, PreState), PreState\==[]) -> 
   nb_setval(Var, State) ; set_advstate_db(State))).

set_advstate_db(State):- from_ref(State,State0),
  notrace((set_advstate_db_1(State0),
  nop(record_saved(State0)))),!.


set_advstate_db_1(Nil):- Nil==[], !.
set_advstate_db_1(State):- \+ callable(State), dumpST, dmsg(set_advstate_db(State)), break.
set_advstate_db_1([H|T]):- is_list(T), !, % retractall(advstate_db(_)),
   set_advstate_db_1(H),
   set_advstate_db_1(T).

set_advstate_db_1(structure_label(_Label)).
set_advstate_db_1(State):-
   compound(State),
   % dmsg(set_advstate_db_1(State)),
   compound_name_arguments(State, Pred, [Object|StateL]) ,
   must_be(ground, Object),
   append(WithoutLastArg, [_NewData], StateL),
   append(WithoutLastArg, [_OldData], PreStateL),
   PreState =.. [Pred, Object|PreStateL],
   forall(clause(advstate_db(PreState), true, Ref), erase(Ref)),
   asserta(advstate_db(State)), !.
  
set_advstate_db_1(State):- dumpST, dmsg(set_advstate_db(State)), break.

get_advstate_fork(StateC):- get_advstate(State), duplicate_term(State, StateC).

declared_advstate(Fact):- \+ is_list(Fact), advstate_db(Fact), !.
declared_advstate(Fact):- get_advstate(State), declared(Fact, State).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded(adv_state)).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------------------------
% State may be implemented differently in the future (as a binary tree or
% hash table, etc.), but for now is a List. These (backtrackable) predicates
% hide the implementation:
% assert/record/declare/memorize/think/associate/know/retain/affirm/avow/
% insist/maintain/swear/posit/postulate/allege/assure/claim/proclaim
% retract/erase/forget/un-declare/unthink/repress/supress
% retrieve/remember/recall/ask/thought/think-of/reminisc/recognize/review/
% recollect/remind/look-up/research/establish/testify/sustain/attest/certify/
% verify/prove
% simulation: declare/undeclare/declared
% perception:
% memory: memorize/forget/thought

:- meta_predicate(update_agent_model_props(+, *, *, *)).
update_agent_model_props(Agent,Figment, M0, M1) :-  agent_mem(Agent,M0,M1,A0,A1),  
  memorize_edit_0(Agent,append, Figment, A0, A1).

:- meta_predicate(memorize_edit(+,3, *, *, *)).
memorize_edit_0(_Agent,Pred3, Figment, M0, M2) :- assertion(\+ is_list(Figment)),
 must_mw1((
   Figment =.. [Name, Value], OldFigment =.. [Name, OldValue],
   (select(OldFigment, M0, M1)
     -> ( call(Pred3, OldValue, Value, NewValue), NewFigment =.. [Name, NewValue])
     ; (NewFigment=Figment, M0=M1)),
    append([NewFigment], M1, M2))).
   
memorize_appending(Agent,Figment0, M0, M1) :-  
 must_mw1((agent_mem(Agent,M0,M1,A0,A1), 
  listify(Figment0,Figment),
  episodic_mem(Agent,memorize_appending(Figment)),
  memorize_edit_0(Agent,append, Figment, A0, A1))).

% Manipulate memories (M stands for Memories)
memorize_prepending(Agent, Figment0, M0, M1) :- must_mw1((agent_mem(Agent,M0,M1,A0,A1), 
  listify(Figment0,Figment),
  episodic_mem(Agent,memorize_prepending(Figment)),
  notrace(append(Figment, A0, A1)))).

replace_thought(Agent,Figment0, M0, M1) :-    must_mw1((agent_mem(Agent,M0,M1,A0,A3),
  var(A3),
  must_unlistify(Figment0,Figment),
  old_figment(Figment,_F,_A,FigmentErased),
  select(FigmentErased, A0, A1),
  % episodic_mem(Agent,replace(FigmentErased,Figment)),  
  append([Figment], A1, A3))),!.
 
thought(Agent,Figment0, M) :-           
 must_mw1(( agent_mem(Agent,M,A),
  must_unlistify(Figment0,Figment))),!,
  (declared(Figment, A) -> episodic_mem(Agent,thought(Figment)); (episodic_mem(Agent,considered(Figment)),fail)),!.  

thought_check(Agent,Figment0, M) :-
  must_mw1(( agent_mem(Agent,M,A),
   must_unlistify(Figment0,Figment))),!,
  declared(Figment, A).

/*
memorize(Agent, Figment0, M0, M1):- memorize_prepending(Agent, Figment0, M0, M1),!.


forge t(Agent,Figment0, M0, M1) :-   agent_mem(Agent,M0,M1,A0,A1),
  must_unlistify(Figment0,Figment), !, 
  select_from(Figment, A0, A1),
  nop(episodic_mem(Agent,forget(Figment))).

forget_ always(Agent,Figment0, M0, M1) :-   agent_mem(Agent,M0,M1,A0,A1),
  must_unlistify(Figment0,Figment),  
  select_always(Figment, A0, A1),
  nop(episodic_mem(Agent,forget(Figment))).
*/



  

must_unlistify(Figment0,Figment):- is_list(Figment0),!,must_mw1(Figment0=[Figment]).
must_unlistify(Figment,Figment).

in_agent_model(Agent, Fact, State):- in_model(Fact, State)*-> true ; (agent_thought_model(Agent, ModelData, State), in_model(Fact, ModelData)).

in_model(E, L):- quietly(in_model0(E, L)).

in_model0(E, L):- \+ is_list(L), declared_link(declared, E, L).
in_model0(E, L):- compound(E), E = holds_at(_, _), !, member(E, L).
in_model0(E, L):- member(EE, L), same_element(EE, E).

same_element(E, E) :- !.
same_element(holds_at(E, T), E):- nonvar(T).

agent_mem(_Agent,Mem0,Mem1,AMem0,AMem1):- Mem0=AMem0,Mem1=AMem1.
agent_mem(_Agent,Mem0,AMem0):- Mem0=AMem0.

:- defn_state_getter(agent_thought_model(agent, model)).
agent_thought_model(Agent, ModelData, M0):- var(M0), get_advstate(State), !, member(memories(Agent, M0), State), agent_thought_model(Agent, ModelData, M0).
agent_thought_model(Agent, ModelData, M0):- \+ is_list(M0), !, declared_link(agent_thought_model(Agent), ModelData, M0).
agent_thought_model(Agent, ModelData, M0) :- memberchk(propOf(memories, Agent), M0), ModelData = M0, !.
agent_thought_model(Agent, ModelData, M0):- declared(memories(Agent, M1), M0), !,
  agent_thought_model(Agent, ModelData, M1).


:- export(declare/3).
%:- defn_state_setter(declare(fact)).
select_from(Fact, State, NewState) :- notrace((assertion(var(NewState)), is_list(State))), !, 
   notrace(select_from_list(Fact, State, NewState)).
select_from(Fact, inst(Object), inst(Object)):- !,
   get_advstate(State),
   (declared(props(Object, PropList), State);PropList=[]), !,
   select_from_list(Fact, PropList, NewPropList),
   select_always(props(Object, _), State, MidState),
   append([props(Object, NewPropList)], MidState, NewState),
   set_advstate(NewState).
select_from(Fact, type(Type), type(Type)):- !,
   get_advstate(State),
   (declared(type_props(Type, PropList), State);PropList=[]), !,
   select_from_list(Fact, PropList, NewPropList),
   select_always(type_props(Type, _), State, MidState),
   append([type_props(Type, NewPropList)], MidState, NewState),
   set_advstate(NewState).
select_from(Fact, Pred1Name, Pred1Name):- is_pred1_state(Pred1Name), append_term(Pred1Name, State, DBPred), (retract(DBPred);State=[]), !, select_from_list(Fact, State, NewState), DBPredNewState=..[Pred1Name, NewState], asserta(DBPredNewState).
select_from(Fact, VarName, VarName):- atom(VarName), nb_current(VarName, PropList), select_from_list(Fact, PropList, NewPropList), b_setval(VarName, NewPropList).
select_from(Fact, Object, Object):- callable(Fact), !, Fact=..[F|List],
  Call=..[F, NewArg|List],
  current_predicate(_, Call), !,
  ignore( \+ \+ retract(Call)),
  NewArg=Object,
  asserta(Call).

select_from_list(Item, List, ListWithoutItem):- select(Item, List, ListWithoutItem).

% Like select_from, but always succeeds, for use in deleting.
select_always(Item, List, ListWithoutItem) :- select_from(Item, List, ListWithoutItem) -> true; ListWithoutItem=List.

% Like select_from, but with a default value if not found in List..
%select_default(Item, _DefaultItem, List, ListWithoutItem) :-
% select_from(Item, List, ListWithoutItem).
%select_default(DefaultItem, DefaultItem, ListWithoutItem, ListWithoutItem).

% Manipulate simulation state
% declare(Fact, State):- player_local(Fact, Player), !, declare(wishes(Player, Fact), State).
:- export(declare/3).
:- defn_state_setter(declare(fact)).

declare(Fact, State, NewState):- must_mw1(declare_0(Fact, State, NewState)).

declare_0(Fact, State, NewState) :- notrace((assertion(var(NewState)), is_list(State))), !, notrace(declare_list(Fact, State, NewState)).
declare_0(Fact, inst(Object), inst(Object)):- !,
   get_advstate(State),
   (declared(props(Object, PropList), State);PropList=[]), !,
   declare_list(Fact, PropList, NewPropList),
   select_always(props(Object, _), State, MidState),
   append([props(Object, NewPropList)], MidState, NewState),
   set_advstate(NewState).
declare_0(Fact, type(Type), type(Type)):- !,
   get_advstate(State),
   (declared(type_props(Type, PropList), State);PropList=[]), !,
   declare_list(Fact, PropList, NewPropList),
   select_always(type_props(Type, _), State, MidState),
   append([type_props(Type, NewPropList)], MidState, NewState),
   set_advstate(NewState).
declare_0(Fact, Pred1Name, Pred1Name):- is_pred1_state(Pred1Name), append_term(Pred1Name, State, DBPred), (retract(DBPred);State=[]), !,
   declare_list(Fact, State, NewState), append_term(Pred1Name, NewState, DBPredNewState), asserta(DBPredNewState).
declare_0(Fact, VarName, VarName):- atom(VarName), nb_current(VarName, PropList), declare_list(Fact, PropList, NewPropList), b_setval(VarName, NewPropList).
declare_0(Fact, Object, Object):- callable(Fact), !, Fact=..[F|List],
  Call=..[F, NewArg|List],
  current_predicate(_, Call), !,
  ignore( \+ \+ retract(Call)),
  NewArg=Object,
  asserta(Call).

merge_proplists(AddPropList, OldPropList, NewPropList):-
  append(AddPropList, OldPropList, NewPropListL), list_to_set(NewPropListL,NewPropList),!.

declare_list(Fact, State, NewState) :- assertion(compound(Fact)), assertion(var(NewState)), Fact==[], !, NewState = State.
declare_list((Fact1, Fact2), State, NewState) :- !, declare_list(Fact1, State, MidState), declare_list(Fact2, MidState, NewState).
declare_list([Fact1|Fact2], State, NewState) :- !, declare_list(Fact1, State, MidState), declare_list(Fact2, MidState, NewState).
declare_list(HasList, State, [NewFront|NewState]) :-
  HasList=..[F,Object,AddPropList],
  Old=..[F,Object,OldPropList],
  select_from(Old, State, NewState), !,
  assertion(is_list(OldPropList)),
  merge_proplists(AddPropList, OldPropList, NewPropList),
  NewFront=..[F, Object, NewPropList].
declare_list(HasList, State, [NewFront|NewState]) :-
  safe_functor(HasList, F, A), arg(A, HasList, PropList), is_list(PropList),
  safe_functor(Functor, F, A), \+ \+ type_functor(state, Functor),
  arg(1, HasList, Object), arg(1, Functor, Object),
  select_from(Functor, State, NewState), !,
  arg(A, Functor, OldPropList), assertion(is_list(OldPropList)),
  append(PropList, OldPropList, NewPropList),
  assertion(A=2;A=3), NewFront=..[F, Object, NewPropList].
declare_list(Fact, State, NewState) :- append([Fact], State, NewState).



maybe_undeclare(Fact, State, NewState):- declared(Fact, State), NewState=State.
%maybe_undeclare(Fact, State, NewState):- undeclare(Fact, State, NewState).

append_toplevel_props(perceptq(Agent, Events),S0,S2):- 
 must_mw1((
 maybe_undeclare(perceptq(Agent, Queue), S0, S1),
 append(Queue, Events, NewQueue),
 replace_declare(perceptq(Agent, NewQueue), S1, S2),
 declared(perceptq(Agent, RQueue), S0, _),
 is_list(RQueue))).

replace_declare(Fact, State, NewState):-
 must_mw1((old_figment(Fact, _F, A, Old),
 (undeclare(Old, State, MidState);(State=MidState,ignore(arg(A,Old,[])))),
 nop(episodic_mem($agent,replace(Old,Fact))),
 declare(Fact, MidState, NewState))).

%undeclare(Fact, State):- player_local(Fact, Player), !, undeclare(wishes(Player, Fact), State).
undeclare(Fact, State, NewState):- notrace(undeclare_list(Fact, State, NewState)).
undeclare_list(Fact, State, NewState) :- assertion(is_list(State)), copy_term(State, Copy), select_from(Fact, State, NewState),
 assertion( \+ member(Copy , NewState)).

%undeclare_always(Fact, State):- player_local(Fact, Player), !, undeclare_always(wishes(Player, Fact), State).
undeclare_always(Fact, State, NewState) :- select_always(Fact, State, NewState).

% declared(Fact, State) :- player_local(Fact, Player), !, declared(wishes(Player, Fact), State).

declared(State) :- % dumpST,throw
  (with_mutex(get_advstate, advstate_db(State))).

:- export(declared/2).
:- defn_state_getter(declared(fact)).
declared(Fact, State) :- quietly(declared_0(Fact, State)).

declared_0(Fact, State) :-
  quietly(( is_list(State)->declared_list(Fact, State);declared_link(declared, Fact, State))).

declared_list(Fact, State) :- assertion(is_list(State)), member(Fact, State).
declared_list(Fact, State) :- member(link(VarName), State), declared_link(declared, Fact, VarName).
declared_list(Fact, State) :- member(propOf(_, Object), State), declared_link(declared, Fact, Object).

:- meta_predicate(declared_link(2, ?, *)).
declared_link(Pred2, Fact, VarName):- strip_module(Pred2, _, Var), var(Var), !, declared_link(declared, Fact, VarName).
declared_link(Pred2, Fact, VarName):- atom(VarName), nb_current(VarName, PropList), PropList\==[], !, call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, inst(Obj)):- declared_advstate(props(Obj, PropList)), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, type(Type)):- declared_advstate(type_props(Type, PropList)), call(Pred2, Fact, PropList).
% declared_link(Pred2, Fact, inst_model(Obj, Type)):- declared_advstate(props(Type, PropList)), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, Object):- nonvar(Object), extra_decl(Object, PropList), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, Object):- get_advstate(State), direct_props(Object, PropList, State), call(Pred2, Fact, PropList), !.
declared_link(declared, Fact, Object):- callable(Fact), Fact=..[F|List], Call=..[F, Object|List], 
  current_predicate(_, Call), call(Call), !.
declared_link(declared, Fact, Object):- callable(Fact), Fact=..[F|List], Call=..[F, Object|List], advstate_db(Call), !.
declared_link(Pred2, Fact, Object):- var(Object), get_advstate(State), member(Prop, State), arg(1, Prop, Object), arg(2, Prop, PropList),
  call(Pred2, Fact, PropList).
declared_link(declared, Fact, _Object):- advstate_db(Fact), !.


filter_spec(true, _):- !.
filter_spec( \+ Spec, PropList):- !,
 \+ filter_spec(Spec, PropList).
filter_spec((Spec1;Spec2), PropList):- !, (filter_spec(Spec1, PropList);filter_spec(Spec2, PropList)).
filter_spec((Spec1, Spec2), PropList):- !, filter_spec(Spec1, PropList), filter_spec(Spec2, PropList).
filter_spec( Spec, PropList):- declared(Spec, PropList).


% extra_decl(Object, PropList):- get_advstate(State), direct_props(Object, PropList, State).

% Entire state of simulation & agents is held in one list, so it can be easy
% to roll back. The state of the simulation consists of:
% object properties
% object relations
% percept queues for agents
% memories for agents (actually logically distinct from the simulation)
% Note that the simulation does not maintain any history.
% TODO: change state into a term:
% ss(Objects, Relationships, PerceptQueues, AgentMinds)
% TODO:
% store initial state as clauses which are collected up and put into a list,
% like the operators are, to provide proper prolog variable management.

get_object_props(Agent, Mem):-
   get_advstate(State),
   declared(props(Agent, Mem), State).

:- defn_state_getter(get_object_props(agent, model)).
get_object_props(Obj, ObjectProps, M0):- var(M0), get_advstate(M0), !, get_object_props(Obj, ObjectProps, M0).
get_object_props(Obj, ObjectProps, M0):- is_list(M0), get_object_props_list(Obj, ObjectProps, M0), !.
get_object_props(Obj, ObjectProps, M0):-  declared_link(get_object_props(Obj), ObjectProps, M0).

get_object_props_list(Obj, ObjectProps, M0):- memberchk(propOf(_, Obj), M0), ObjectProps = M0, !.
get_object_props_list(Obj, ObjectProps, M0):- member(props(Obj, ObjectProps), M0), !.
% get_object_props_list(Obj, ObjectProps, M0):- member(type_props(Obj, ObjectProps), M0), !.


get_objects(Spec, Set, State):-
 quietly((must_input_state(State),
  get_objects_(Spec, List, State, im(State)), !,
  list_to_set(List, Set))).
%get_objects(_Spec, [_Player_1, floyd_X1], _State):-!.

get_objects_(_Spec, [], [], im(_)) :- !.
get_objects_(Spec, OutList, [Store|StateList], im(S0)):-
 (( stores_props(Store, Object, PropList) -> filter_spec(Spec, PropList))
 -> OutList = [Object|MidList]
 ; OutList = MidList), !,
 get_objects_(Spec, MidList, StateList, im(S0)).

stores_props(perceptq(Agent, PropList), Agent, PropList).
%stores_props(type_props(Agent, PropList), Agent, PropList).
stores_props(memories(Agent, PropList), Agent, PropList).
stores_props(props(Object, PropList), Object, PropList).



as_first_arg(Object, Prop, Element):-
  callable(Prop), Prop=..[Name| Value], Element =..[Name, Object| Value].


merge_value(F, N, B, A, RO):- text_prop(F), \+ is_list(B), !, merge_value(F, N, [B], A, RO).
merge_value(F, N, B, A, RO):- text_prop(F), \+ is_list(A), !, merge_value(F, N, B, [A], RO).
merge_value(F, _, _, A, R):- single_valued_prop(F), !, A=R.

merge_value(=, 2, _, V, R):- !, R = V.

merge_value(_, _, _, t, R):- !, R = t.
merge_value(_, _, _, f, R):- !, R = f.
merge_value(_, _, _, [], R):- !, R = [].
merge_value(_, _, _, A, R):- number(A), !, A=R.

merge_value(_F, 1, B, A, R):- B == A, !, R = A.

merge_value(_F, 1, B, A, RO):- (is_list(B);is_list(A)), flatten([A, B], R), !, list_to_set(R, RO).

merge_value(_, 1, _, A, R):- number(A), !, A=R.
merge_value(_, 1, _, _, _):- !, fail.
merge_value(_F, _, _B, A, R):- R = A.




:- export(is_state_info/1).

is_state_info(StateInfo):- \+ compound(StateInfo), !, fail.
is_state_info(StateInfo):- safe_functor(StateInfo, F, A),
   (functor_arity_state(F, A)->true; (A>2, functor_arity_state(F, 2))).


functor_arity_state(F, A):- is_type_functor(state, F, A).
functor_arity_state(type, 2).

is_spatial_rel(worn_by).
is_spatial_rel(held_by).
is_spatial_rel(in).
is_spatial_rel(on).
is_spatial_rel(exit).

update_running(StateInfo):- 
  ignore((get_advstate(S0), !, declare(StateInfo, S0, S1), !, set_advstate(S1))), !.
% update_running(_StateInfo).


push_to_state(Info):- must_or_rtrace(push_2_state(Info)).

%push_2_state(State):- push_to_obj(world, State).
push_2_state(StateInfo):- end_of_list == StateInfo, !.
%push_2_state(sp(Adjs,TypeS)):- is_list(TypeS),maplist([E]>>push_2_state(sp(Adjs,E)),TypeS).
%push_2_state(sp(Adjs,Atom)):- push_2_state(type_props(Atom,[inherit(Adjs)])),push_2_state(inherit(Atom)).
push_2_state(StateInfo):- is_codelist(StateInfo), any_to_string(StateInfo, SStateInfo), !, push_2_state(SStateInfo).
push_2_state(StateInfo):- is_charlist(StateInfo), any_to_string(StateInfo, SStateInfo), !, push_2_state(SStateInfo).
push_2_state(StateInfo):- string(StateInfo), parse_kind(state, StateInfo, Logic), push_2_state(Logic).
push_2_state(StateInfo):- is_list(StateInfo), !, maplist(push_2_state, StateInfo).
push_2_state(StateInfo):- \+ compound(StateInfo), trace_or_throw(unknown_push_to_state(StateInfo)), !.
push_2_state(type(Type, Conj)):-  !, push_2_state(props(type(Type), Conj)).
push_2_state(props(type(Type), Conj)):- !, props_to_list(Conj, List), push_2_state(type_props(Type, List)).
push_2_state(props(Obj, Conj)):-  props_to_list(Conj, List) -> Conj\== List, !, push_2_state(props(Obj, List)).
push_2_state(type_props(Obj, Conj0)):-  
  (adv_subst(equivalent, $class, Obj, Conj0, Conj)-> Conj0\==Conj),!, push_2_state(type_props(Obj, Conj)).
push_2_state(type_props(Obj, Conj)):-
  (props_to_list(Conj, List) -> Conj\== List), !, push_2_state(type_props(Obj, List)).

push_2_state(StateInfo):- StateInfo=..[F, Obj, E1, E2|More], functor_arity_state(F, 2), !, StateInfoNew=..[F, Obj, [E1, E2|More]], !, push_2_state(StateInfoNew).
push_2_state(StateInfo):- props_to_list(StateInfo, StateInfo2)->StateInfo2\=[StateInfo], !, push_2_state(StateInfo2).

push_2_state(assert_text(Text)):- must(eng2log(istate, Text, Translation, [])), push_2_state(Translation).
push_2_state(assert_text(Where, Text)):- !, must(eng2log(Where, Text, Translation, [])), push_2_state(Translation).

push_2_state(StateInfo):- is_state_info(StateInfo), !, declare(StateInfo, istate, _), update_running(StateInfo).
push_2_state(StateInfo):- wdmsg(warn(push_2_state(StateInfo))),trace, forall(arg(_, StateInfo, Sub), push_2_state(Sub)).

correct_props(_Obj, PropsIn, PropsOut):- props_to_list(PropsIn, PropsOut), !.

check_atom(Atom):- assertion(atom(Atom)).

props_to_list(Nil, []):- assertion(\+ var(Nil)), Nil==[], !.
props_to_list(end_of_list, []):- !.
props_to_list(Before, AfterL):- (correct_prop(Before, After) -> Before\==After, listify(After,AfterL)), !.
props_to_list(NC, [nc(NC)]):- \+ compound(NC), !.
props_to_list(oper(_, _, _), []):- !.
props_to_list([A|B], ABL):- !,
   props_to_list(A, AL),
   props_to_list(B, BL),
   append(AL, BL, ABL).
props_to_list((A, B), ABL):- !,
   props_to_list(A, AL),
   props_to_list(B, BL),
   append(AL, BL, ABL).
props_to_list(Other, [Other]).


make_class_desc_sp(adjs,Atom,Desc):- string_concat("normally ",Atom,Desc).
make_class_desc_sp(nouns,Atom,Desc):- string_concat("refered to as a ",Atom,Desc).
make_class_desc_sp(nominals,Atom,Desc):- string_concat("related to a ",Atom,Desc).

pos_to_sp(adjs).
pos_to_sp(nouns).
%pos_to_sp(nominals).

negated_boolean(Last,_NegLast):- \+ atomic(Last),!,fail.
%negated_boolean(nil,t).
negated_boolean(Yes,No):- true_2_false(Yes,No),!.
negated_boolean(No,Yes):- true_2_false(Yes,No),!.

true_2_false(t,f).
true_2_false(1,0).
true_2_false(true,false).
true_2_false(y,n).
true_2_false(yes,no).

negate_prop(UnNegated,Negated):- \+ compound_gt(UnNegated,0),!,(negated_boolean(UnNegated,Negated)->true;UnNegated=Negated).
negate_prop(UnNegated,Negated):-
  functor(UnNegated,F,A),arg(A,UnNegated,Last),
  negated_boolean(Last,NegLast),!,
  UnNegated=..[F|Args],
  append(Left,[_],Args),
  append(Left,[NegLast],NewArgs),
  Negated=..[F|NewArgs],!.


correct_prop(NC, NO):- var(NC), !, NC = NO.
correct_prop(NC, nc(NC)):- var(NC), throw(correct_prop(NC, nc(NC))), !.
correct_prop(        (Type), inherit(Type, t)):- atom(Type).
correct_prop(~inherit(Type), inherit(Type, f)):- atom(Type), !.
correct_prop( inherit(Type), inherit(Type, t)):- check_atom(Type), !.
correct_prop(     isa(Type), inherit(Type, t)):- check_atom(Type), !.
correct_prop(    isnt(Type), inherit(Type, f)):- check_atom(Type), !.
correct_prop(NC, nc(NC)):- \+ compound(NC), !.
correct_prop(       ~(Type), Negated):- correct_prop( Type, UnNegated), negate_prop(UnNegated,Negated),!.
correct_prop(       ~(Type), inherit(Type, f)):- atom(Type), !.
correct_prop(AdjsInfo, sp(Adjs,Info)):- pos_to_sp(Adjs), compound_name_arguments(AdjsInfo,Adjs,[Info]).

correct_prop(sp(Adjs,TypeS), Out):- is_list(TypeS), must_maplist(correct_some(Adjs),TypeS,Out).
correct_prop(sp(Adjs,Atom), Out):-  check_atom(Atom),   
  push_to_state(type_props(Atom,[nominals(Atom),sp=Adjs])),!,
  % make_class_desc_sp(Adjs,Atom,ClassDesc), push_to_state(type_props(Atom,[class_desc([ClassDesc])])),
  must(correct_prop(inherit(Atom),Out)).

correct_prop(HPRED, h(FS, X, Y)):- HPRED=..[F, S, X, Y], is_spatial_rel(F), !, FS=..[F, S].
correct_prop(HPRED, h(F, X, Y)):- HPRED=..[F, X, Y], is_spatial_rel(F), !.
correct_prop(          SV, N=V):- SV=..[N, V], single_valued_prop(N), !.

correct_prop( (can(Verb)), can_be(Verb, t)):- nop(check_atom(Verb)).
correct_prop(~(can(Verb)), can_be(Verb, f)):- nop(check_atom(Verb)).
correct_prop( (can(Verb, TF)), can_be(Verb, TF)):- nop(check_atom(Verb)).
correct_prop( (knows_verbs(Verb)), knows_verbs(Verb, t)):- nop(check_atom(Verb)).
correct_prop(~(knows_verbs(Verb)), knows_verbs(Verb, f)):- nop(check_atom(Verb)).
correct_prop( (has_rel(Verb)), has_rel(Verb, t)):- nop(check_atom(Verb)).
correct_prop(~(has_rel(Verb)), has_rel(Verb, f)):- nop(check_atom(Verb)).
correct_prop(  Other, Other).

correct_some(Adjs,E,O):- check_atom(Adjs), must(correct_prop(sp(Adjs,E),O)).


episodic_mem(x(floyd,_),_Figment) :-!.
episodic_mem(Agent,Figment) :- is_list(Figment),!,forall(member(F,Figment),episodic_mem(Agent,F)).
episodic_mem(Agent,Figment) :- notrace((format('~N',[]),in_color(pink,print_tree(episodic_mem(Agent,Figment))),format('~N',[]))),
  overwrote_prompt.

% for  the "TheSims" bot AI which will make the bots do what TheSims characters do... (they dont use the planner they use a simple priority routine)

