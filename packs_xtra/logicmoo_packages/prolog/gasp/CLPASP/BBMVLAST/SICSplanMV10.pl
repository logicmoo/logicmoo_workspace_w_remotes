%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SICStus Interpreter of the B^MV_FD action description language
%%% Updated AUGUST 2010 by DFP. FASTER THAN THE ICLP07 VERSION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(clpfd)). :-use_module(library(lists)). 
:-use_module(library(between)).
:-prolog_flag(unknown,_,fail).   :-use_module(library(terms)).

%:-use_module(library(random)).

%%% In case no STATIC CAUSAL LAWS (caused) or no further constraints are defined, add
:- dynamic(caused/2). 
:- dynamic(action_cost/2). :- dynamic(max_plan_cost/1).
:- dynamic(state_cost/1). :- dynamic(goal_cost/1). :- dynamic(cost_constraints/1).
:- dynamic(holds/2).
% :- dynamic(always/1).
:- dynamic(time_constraint/1). :- dynamic(action_bound/1).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%% Infix operators for fluent constraints  %%%
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

:-op(600,xfx,eq). :-op(600,xfx,neq). :-op(600,xfx,leq).
:-op(600,xfx,geq). :-op(600,xfx,lt). :-op(600,xfx,gt). :-op(500,xfx,@).

%%% LOAD THE ACTION DESCRIPTION

%:-compile(
%'BMVMODELS/15puzzleMV_4moves.txt'
%'BMVMODELS/barrelsMV.txt'
%'BMVMODELS/saw_MV.txt'
%'BMVMODELS/gas_diffusion.txt'
%'trucksMV_00.txt'
%'trucksMV_00_a.txt'
%'trucksMV_00_b.txt'
%).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_labeling implements the search strategy.
%%% Change options here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_labeling(Varsocc, States) :-
    %% no_loop_constraint(States),  %%% To be used in conjunction/alternative with no_loop
    my_labeling_rec(Varsocc,States,1).    

my_labeling_rec([],_,_) :- !.
my_labeling_rec([CurrVars|Vars], States,I) :-
           %%%random_permutation(CurrVars,RanCurrVars),
           %%%labeling([ffc,down],RanCurrVars),
           %%%labeling([ffc],RanCurrVars),
    %labeling([down],CurrVars),   %%%   
    labeling([ffc,down],CurrVars), %%%  
    %labeling([ffc],CurrVars), %%% Good for 15 Puzzle / Barrels / SAW / Gas
    %labeling([],CurrVars), %%% Good for 15 puzzle / Barrels /SAW / ~Gas / Hydraulic
    no_loop(States,I), %%% Useless if no_loop_constraint is uncommented
    I1 is I + 1,
    my_labeling_rec(Vars, States,I1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_executable(Act) :-
    once(executable(Act,_)). %better that findall
%findall(X,executable(Act,X),[_|_]).
%
%%%
% wrapping (11-08-2010)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wrap_action(Act) :- action(Act), is_executable(Act).
wrap_causes(Act,F,L) :- causes(Act,F,L), is_executable(Act).
wrap_caused(C1,C2) :- caused(C1,C2).
wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



sicsplan(N) :-
    M is N + 1,
    retractall(smart_cluster(_,_)),
    main(M,_,_),!.
sicsplan(_) :-
    statistics(runtime,[_,Time]),
    write('No solutions: RunTime: '),write(Time), write('ms '),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(N) :-
   main(N,_,_),!.
main(_) :-
    statistics(runtime,[_,Time]),
    write('No solutions: RunTime: '),write(Time),write('ms '),nl.

main(N, Actionsocc,States):-
    statistics(_,_),
    build_states(N,States,Nf),
    make_action_occs(N,Actionsocc,PlanCost,Na),
    format("There are ~q fluents per state and ~q possible actions~n",[Nf,Na]),

%%%%%%%%%%%%%%%%     ADD CONSTRAINTS     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    (set_initial(States); %%% Help in A.T. DEBUG
      write('Inconsistent initial state'),nl,!,fail),
    write('Initial State:'),nl, States=[S|_],write_state(S),nl,
    (set_goal(States); %%% Help in A.T.  DEBUG
      write('Inconsistent final state'),nl,!,fail),
    (set_intermediate_states(States); %%% Help in A.T.  DEBUG
      write('Inconsistent intermediate state'),nl,!,fail),
    (set_cross_constraints(States); %%% Help in A.T.  DEBUG
      write('Inconsistent cross constraint'),nl,!,fail),
    (set_action_bounds(Actionsocc);
      write('Check Action Bounds '),nl,!,fail),
    write('Final State:'),nl, append(_,[Last],States),write_state(Last),
    !,
    set_transitions(Actionsocc,States),
    write('****transitions   set****'),nl,
    set_executability(Actionsocc,States),
    write('****executability set****'),nl,
    set_cost_constraints(States,PlanCost,GC),
    !,

%%%%%%%%%%%%%%    LABELING     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% REMARK Observe that only action variables are labelled.
%%% Fluent variables obtain their values by propagation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    get_all_actions(Actionsocc, Filtered, FlatActions),
    length(FlatActions,L),write('Still to label '),write(L),write(' variables'),nl,
    statistics(runtime,[_,PT]),
    write('Constraints added in ms: '),write(PT),nl,
    flush_output,!,

    my_labeling(Filtered,States),
    
    %%%% labeling([],FlatActions).  %%% Built-in case. It works for little instances

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    statistics(runtime,[_,Time]), line,
    dump_result(Actionsocc,States),line,
    write('Total Plan Cost = '),write(PlanCost),nl,
    write('Goal Cost = '),write(GC),nl,
    write('Solution found in Time: '),write(Time),nl.

%%% no_loop check (after instantiation) that the new
%%% state is different from all previous ones

no_loop(States,A) :-
    state_select(A,States,StateA,Firsts),
    nonmember(StateA,Firsts).
    
%%% no_loop_constraint: for each pair of states
%%% s_i, s_j a constraint c_ij is set to 1 if they are
%%% equal (namely the values of the same fluents
%%% is the same in the two states). 
%%% We force all those constraints to be false.

no_loop_constraint(States) :-
   no_loop_constraint(States,C),
   bool_or(C,0).
   
no_loop_constraint([],[]) :- !.
no_loop_constraint([_],[]) :- !.
no_loop_constraint([S|Tates],C) :-
   no_loop_constraint(S,Tates,C1),
   no_loop_constraint(Tates,C2),
   append(C1,C2,C).
   
no_loop_constraint(_,[],[]). 
no_loop_constraint(S,[T|Ates],[Cflag|C2]) :-
    all_equal(S,T,C1),
    bool_and(C1,Cflag),
    no_loop_constraint(S,Ates,C2).
   
all_equal([],[],[]).
all_equal([fluent(A,V)|R],[fluent(A,W)|S],[C1|C]) :-
    C1 #<=> V #= W, %%% reified
    all_equal(R,S,C).    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The domain of Each Fluent Variable is set here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_states(N,States,Nf) :-
    findall([F,int(MinF,MaxF)],fluent(F,MinF,MaxF),L1),
    findall([F,set(Dom)],fluent(F,Dom),L2),
    append(L1,L2,Lf),
    length(Lf,Nf),
    make_states(N,Lf,States).
make_states(0,_,[]):-!.
make_states(N,List,[S|STATES]) :-
    N1 is N-1, make_states(N1,List,STATES),
    make_one_state(List,S).
make_one_state([],[]).
make_one_state([[F,int(MinF,MaxF)]|Fluents],[fluent(F,VarF)|VarFluents]) :-
    make_one_state(Fluents,VarFluents),
    VarF in MinF..MaxF. %%% interval case
make_one_state([[F,set(Dom)]|Fluents],[fluent(F,VarF)|VarFluents]) :-
    make_one_state(Fluents,VarFluents),
    VarF in Dom.  %%% set case

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The domain of each action (occurrence) variable is set to {0,1}
%%% In each state transition exactly one action occur (sum constraint)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%a setof not failing if no solution exists
mysetof(A,B,C) :-
        (setof(A, B, C) ; C=[]), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_action_occs(N,Actionsocc,PlanCost,Na) :-
%    setof(A,action(A),La),
    mysetof(A,wrap_action(A),La),
    length(La,Na),
    make_action_occurrences(N,La,Actionsocc,PlanCost).
make_action_occurrences(1,_,[],0):-!.
make_action_occurrences(N,List,[Act|ActionsOcc],Cost) :-
    N1 is N-1,
    make_action_occurrences(N1,List,ActionsOcc,Cost1),
    make_one_action_occurrences(List,Act,Cost2),
    get_action_list(Act,AList),
    fd_only_one(AList), %% fd_one_or_two(AList),
    Cost #= Cost1 + Cost2.
make_one_action_occurrences([],[],0).
make_one_action_occurrences([A|Actions],[ action(A,OccA)|OccActions],Cost) :-
    make_one_action_occurrences(Actions,OccActions,Cost1),
    fd_domain_bool(OccA),
    (action_cost(A,CA),!; CA = 1), %%%Default action cost = 1
     Cost #= OccA*CA + Cost1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initial state info are set here.
%%% In the first state static causal rules are applied by "complete_state"
%%% (for the other states they are applied in set transition)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_initial([Initial|_]) :-
    findall([FE1,OP,FE2],(initially(C),C=..[OP,FE1,FE2]), InitFC),
    set_this_state(InitFC,Initial),
    complete_state(Initial).

set_goal(States) :-
    findall([FE1,OP,FE2],(goal(C),C=..[OP,FE1,FE2]), GoalFC),
    length(States,N),M is N-1,
    set_state(M,GoalFC,States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% intermediate constraints are set here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_intermediate_states(States) :-
    findall([FE1,OP,FE2],(always(C),C=..[OP,FE1,FE2]), Always),
    set_always(Always,States),
    findall([FE1,OP,FE2,No],(holds(C,No),C=..[OP,FE1,FE2]), Holds),
    set_fixed(Holds,States).

set_cross_constraints(States) :-
    findall([FE1,OP,FE2],(time_constraint(C),C=..[OP,FE1,FE2]), Cross),
    set_cross_constraints(Cross,States).

%%%%%%% AUXILIARY Predicates
%%% 1) for "cross" constraints

set_cross_constraints([],_).
set_cross_constraints([[FE1, OP, FE2]|Rest],States) :-
     rel_parsing(FE1,Val1,_,States),
     rel_parsing(FE2,Val2,_,States),
     add_constraint(Val1,OP,Val2),
     set_cross_constraints(Rest,States).

%%% 2) for "always" constraints

set_always([],_).
set_always([FC |Always], States) :-
     length(States,N),
     rec_set_always(0,N,FC,States),
     set_always(Always,States).

rec_set_always(M,M,_,_).
rec_set_always(M,N,FC,States) :-
     M < N, M1 is M + 1,
     set_state(M,[FC],States),
     rec_set_always(M1,N,FC,States).

%%% 3) for "holds" constraints

set_fixed([],_).
set_fixed([[FE1,OP,FE2,No]|Rest],States) :-
     state_select(No,States,StateI),
     set_this_state([[FE1,OP,FE2]],StateI),
     set_fixed(Rest,States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_action_bounds(Actionsocc) :-
    findall([ACT,OP,NUM],(action_bound(AC),AC =..[OP,ACT,NUM]), L),
    set_action_bounds(L, Actionsocc).
set_action_bounds([],_).
set_action_bounds([[ACT,OP,NUM]|R],Actionsocc):-
    list_var(Actionsocc,ACT,VARS),
    sum(VARS,OP,NUM),
    set_action_bounds(R,Actionsocc).
list_var([],_,[]).
list_var([AL|R],ACT,[V_ACT|VARS]) :-
    member(action(ACT,V_ACT),AL),
    list_var(R,ACT,VARS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% set fluent constraints on state I   %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_state(I,FluentConstraints,States) :-
     state_select(I,States,StateI),
     set_this_state(FluentConstraints,StateI).

set_this_state([],_).
set_this_state([[FE1,OP,FE2]|Rest],State) :-
     parsing(FE1,Val1,State),
     parsing(FE2,Val2,State),
     add_constraint(Val1,OP,Val2),
     set_this_state(Rest,State).

complete_state(State) :-
    complete_state(State,State).
complete_state([],_).
complete_state( [fluent(Fluent,_)| Fluents],InitialState) :-
    set_one_static_fluent(Fluent, InitialState),
    complete_state(Fluents ,InitialState).
set_one_static_fluent(Name, State) :-
     findall([OP,FE1,FE2,L],
         (wrap_caused(L,FC),subterm(Name,FC),FC =..[OP,FE1,FE2]), Stat),
     static(Stat,[State],0,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BERLIN-VENEDIG CHANGES - Sept 19 2009 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_cost_constraints(States, PlanCost, GOALCOST) :-
     set_goalcost(States, GOALCOST),
     set_plancost(PlanCost),
     set_statecosts(States).

set_goalcost(States, GC) :-
    findall([OP,Num],(cost_constraint(C), C =.. [OP,goal,Num]), GoalCosts),
    length(States,N),M is N-1,
    state_select(M,States,FinalState),
    (state_cost(FE),!; FE = 1), %%% default state cost = 1
    parsing(FE,GC,FinalState),
    set_goalcost_aux(GoalCosts,GC).
set_goalcost_aux([],_).
set_goalcost_aux([[OP,Num]|GoalCosts],GC) :-
      add_constraint(GC,OP,Num),
      set_goalcost_aux(GoalCosts,GC).

set_plancost(PC) :-
    findall([OP,Num],(cost_constraint(C), C =.. [OP,plan,Num]), PlanCosts),
    set_plancost_aux(PlanCosts,PC).
set_plancost_aux([],_).
set_plancost_aux([[OP,Num]|PlanCosts],PC) :-
     add_constraint(PC,OP,Num),
     set_plancost_aux(PlanCosts,PC).


set_statecosts(States) :-
    findall([I,OP,Num],(cost_constraint(C), C =.. [OP,state(I),Num]), StateCosts),
    set_statecost_aux(StateCosts,States).
set_statecost_aux([],_).
set_statecost_aux([[I,OP,Num]|StateCosts],States) :-
    (state_cost(FE),!; FE = 1),
    rel_parsing(FE,Val,I,States),  
    add_constraint(Val,OP,Num),
    set_statecost_aux(StateCosts,States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% constraints on transitions are set here. First each tranisition
%%% is selected. Then every fluent is analyzed and its new value
%%% is constrained using its previous value and the dynamic and static
%%% rules applied in that state transition.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_transitions(ActOccs,States) :-
    set_transitions(ActOccs,0,States).

set_transitions([],_,_) :- !.
set_transitions([Occ|Occs], M, States) :-
    state_select(M, States, StateM),
    set_transition(StateM, M, Occ, States),
    M1 is M + 1,
    set_transitions(Occs, M1, States).

set_transition([],_,_,_).
set_transition([Fluent|RStateM],M,Occ,States) :-
    set_one_fluent(Fluent, M, Occ, States),
    set_transition(RStateM, M, Occ, States).

%%% REMARK. It works with references on previous states
%%% For future references, the list L below must be computed
%%% in a more complex way

set_one_fluent(fluent(FluentName,IV), M,Occ,States) :-
     findall([Act,OP,FE1,FE2,L],
    (wrap_causes(Act,FC,L),zero_subterm(FluentName,FC),FC =.. [OP,FE1,FE2]),Dyn),
    state_select(M,States,StateM), %%% StateM = FromSt
    M1 is M + 1,
    state_select(M1,States,StateM1), %%% StateM1 = ToSt
    member(fluent(FluentName,EV),StateM1),
    dynamic(Dyn,Occ, StateM,DynFormula, M1, States),
    findall([OP,FE1,FE2,L],
    %% OLD
    %%  (caused(L,FC),zero_subterm(FluentName,FC), FC =..[OP,FE1,FE2]),
    %% NEW:
            (( smart_cluster(FluentName,Fluents),!;
               cluster([FluentName], Fluents),
               assert( smart_cluster(FluentName,Fluents) )),
             member(F,Fluents), caused(L,FC),
             zero_subterm_all(F,[L,FC]), FC =..[OP,FE1,FE2]),
        Stat),
    %%% write('###'),write(FluentName),write(Stat),nl,
    sort(Stat,Statord),
    static(Statord, States, M1, StatFormula),
    bool_or(DynFormula,StatFormula,Formula),
%%% inertia
    #\ Formula #=> EV #= IV.

dynamic([],_,_,[],_,_).
dynamic([[Act,OP,FE1,FE2,Prec]|Rest],Occurrence, State,[ Flag |PF1], M, States):-
    member(action(Act,VA),Occurrence),
    N is M - 1, %%% Looks for preconditions in FromSt and back
    get_precondition_vars(N,Prec,States,ListPV),
    bool_and(ListPV,PFlag),
    %%% The effect is in the next state (M = N + 1)
    rel_parsing(FE1,Val1,M,States),
    rel_parsing(FE2,Val2,M,States),
    exp_constraint(Val1,OP,Val2,C),
    (VA  #/\ PFlag) #<=> Flag, %%%%
    Flag #=> C,
    dynamic(Rest,Occurrence, State,PF1, M, States).


static([],_,_,[]).
static([ [OP,FE1,FE2,Cond] | Others], States, M, [Flag |Fo] ) :-
    get_precondition_vars(M,Cond,States,List), % rel
    bool_and(List,Flag),
    rel_parsing(FE1,Val1,M,States),
    rel_parsing(FE2,Val2,M,States),
    exp_constraint(Val1,OP,Val2,C),
    Flag #=> C,
    static(Others,States,M,Fo).


%%% fixpoint call

cluster(A,B) :-
    collect_fluents_aux(A,C),
    (sort(A,X),sort(C,X), %%% A is equal to C
     !, B=C;
    cluster(C,B)).

collect_fluents_aux([],[]).
collect_fluents_aux([F|Fs],Fluents) :-
    findall(Fnext,( caused(L,C),
                    zero_subterm_all(F,[L,C]),
                    fluent(Fnext,_,_),F \== Fnext,
                    zero_subterm_all(Fnext,[L,C])
                    ), FNEXTS),
    collect_fluents_aux(Fs,Faux),
    union(Faux, [F|FNEXTS], Fluents).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The executability of each action is then related to the
%%% values of the fluents in the previous state. "formula"
%%% store the disjunction of all executability conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_executability(ActionsOcc,States) :-
    findall([Act,C],wrap_executable(Act,C),Conds),
    group_cond(Conds,GroupedConds),
    set_executability(ActionsOcc,0,States,GroupedConds). %%%

set_executability([],_,_,_).
set_executability([ActionsOcc|ARest],M,States,Conds) :-
    set_executability_sub(Conds,ActionsOcc,M,States),
    M1 is M + 1,
    set_executability(ARest,M1, States,Conds).

set_executability_sub([],_,_,_).
set_executability_sub([[Act,C]|CA],ActionsOcc,M,States) :-
    member(action(Act,VA),ActionsOcc),
    preconditions_flags(C, M, States,Flags), % M
    bool_or(Flags,F),
    VA #=> F,
    set_executability_sub(CA,ActionsOcc,M,States).

preconditions_flags([],_,_,[]).
preconditions_flags([C|R],M,States,[Flag|Flags]) :-
      get_precondition_vars(M,C,States,Cs),
      bool_and(Cs,Flag),
      preconditions_flags(R,M,States,Flags).

get_precondition_vars(_,[],_,[]).
get_precondition_vars(M,[FC|Rest],States,[F|LR]) :-
    FC =.. [OP,FE1,FE2],
    get_precondition_vars(M,Rest,States,LR),
    rel_parsing(FE1,Val1,M,States),
    rel_parsing(FE2,Val2,M,States),
    exp_constraint(Val1,OP,Val2,F).

%%%%%% AUXILIARY predicates

state_select(I,States,StateI) :-
    (nth0(I,States,StateI),!;
    J is I + 1, nth(J,States,StateI)).
%%%%%% compatibility: nth in SICStus 3 / nth0 in SICStus 4

state_select(0,[State|_],State,[]) :- !.
state_select(N,[State1|States],State,[State1|States1]) :-
    N1 is N - 1,
    state_select(N1, States,State, States1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_actions(A,T,B) :-
   term_variables_bag(A,C),
   reverse(C,B),
   get_all_actions_step(A,T).
get_all_actions_step([],[]).
get_all_actions_step([A|R],[B|S]) :-
   term_variables_bag(A,B),
   get_all_actions_step(R,S).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
get_action_list([],[]).
get_action_list([action(_,V)|Rest],[V|MRest]) :-
    get_action_list(Rest,MRest).

group_cond([],[]).
group_cond([[Action,C]|R],[[Action,[C|Cs]]|S]) :-
     findall(L,(member([Action,L],R)),Cs),
     filter(Action,R,Others),
     group_cond(Others,S).
filter(_,[],[]).
filter(A,[[A,_]|R],S) :-
      !, filter(A,R,S).
filter(A,[C|R],[C|S]) :-
      !, filter(A,R,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SYNTAX and PARSING OF FLUENT EXPRESSIONS:
%%% FE ::= INTEGER | FD_VAR | FE1 * FE2 | FE1 + FE2 | FE1 - FE2 | FE1 / FE2 | FE1 mod FE2
%%%        abs(FE) | rei( FE OP FE) --- reified constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parsing( Fluent, Val, State ) :-
   rel_parsing(Fluent,Val,0,[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% rel_parsing( +FE1,-Val1,+M,+States)

rel_parsing( Num , Num , _, _ ) :-
   integer(Num), !.

rel_parsing( rei(RC), Val, M, States) :-
   RC =.. [OP,E1,E2],
   rel_parsing( E1, Val1, M, States),
   rel_parsing( E2, Val2, M, States),
   exp_constraint(Val1,OP,Val2,Val),
   !.

rel_parsing( abs(FE), Val, M, States) :-
   rel_parsing(FE, Val1, M, States),
   Val #= abs(Val1),
   !.

rel_parsing( FE, Val, M, States ) :-
   FE =.. [OP,FE1,FE2],
   member(OP,[+,-,mod,/,*]),
   rel_parsing( FE1, Val1, M, States),
   rel_parsing( FE2, Val2, M, States),
   ( OP = + -> Val #= Val1 + Val2;
     OP = - -> Val #= Val1 - Val2;
     OP = * -> Val #= Val1 * Val2;
     OP = / -> Val #= Val1 / Val2;
     OP = mod -> Val #= Val1 mod Val2),
   !.

rel_parsing( Fluent^Delta, Val, M, States ) :-
   H is M + Delta,
   (state_select(H, States, State),
    member(fluent(Fluent,Val), State),!;
    Val = 0, write(Fluent@H),write('###===> delta warning'),nl),
   !.

rel_parsing( Fluent @ Time, Val, _, States ) :-
   state_select(Time,States,State),
   member(fluent(Fluent,Val),State),
   !.

%%% Fluent = Fluent @ M = Fluent^0

rel_parsing( Fluent, Val, M, States ) :-
   state_select(M,States,State),
   member(fluent(Fluent,Val),State).

%%% SYNTAX and BUILDING of a FLUENT CONSTRAINT
%%% FE1 op  FE2, where op in {eq,neq,leq,geq,lt,gt}
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

exp_constraint(L,OP,R,C) :-
   (OP == eq  -> C #<=> L #=  R;
    OP == neq -> C #<=> L #\= R;
    OP == geq -> C #<=> L #>= R;
    OP == leq -> C #<=> L #=< R;
    OP == gt  -> C #<=> L #>  R;
    OP == lt  -> C #<=> L #<  R).

add_constraint(L,OP,R) :-
   (OP == eq  -> L #=  R;
    OP == neq -> L #\= R;
    OP == geq -> L #>= R;
    OP == leq -> L #=< R;
    OP == gt  -> L #>  R;
    OP == lt  -> L #<  R).

%%% END MAIN CODE %%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AUXILIARY PREDICATES for ACTION LANGUAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neq(A,B) :- A \== B.
diff(A,B) :- A \== B.
diff(A,B,C) :- A \== B, A \== C, C \== B.
interval(In,Min,Max) :- between(Min,Max,In).

%%%%%

subterm(A,B) :-
   A==B,!.
subterm(_,B) :-
   var(B),!,fail.
subterm(A,G) :-
   G =.. [B|_],
   A == B,!.
subterm(A,G) :-
   G =.. [_|Args],
   subterm_all(A,Args).
subterm_all(A,[B|_]) :-
   subterm(A,B),!.
subterm_all(A,[_|R]) :-
   subterm_all(A,R).

%%% it looks for a current fluent

zero_subterm(A,B) :-
   A==B,!.
zero_subterm(_,B) :-
   var(B),!,fail.
zero_subterm(A,G) :-
   G =.. [B|_],
   A == B,!.
zero_subterm(A,A^0) :-
   !.
zero_subterm(A,A^_) :-
   !, fail.
zero_subterm(A,G) :-
   G =.. [_|Args],
   zero_subterm_all(A,Args).
zero_subterm_all(A,[B|_]) :-
   zero_subterm(A,B),!.
zero_subterm_all(A,[_|R]) :-
   zero_subterm_all(A,R).

%%% disjoint append

union([],L,L).
union([A|R],S,T) :-
    member(A,S),!,
    union(R,S,T).
union([A|R],S,[A|T]) :-
    union(R,S,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_result(A,S) :-
    dump_result(0,A,S),nl,nl,
    line,
    write('Plan in short:'),nl,
    state_plan(A),nl.

state_plan([]) :- write('stop.'),nl.
state_plan([A|As]) :-
    find_action(A,Name),
    format("~q -> ",[Name]),
    state_plan(As).

dump_result(N,[],[S]) :-
    write('Time '),write(N),write(': '),
    write_state(S).
dump_result(N,[A|B],[S|Rest]) :-
    write('Time '),write(N),write(': '),
    write_state(S),
    write_action(A),
    N1 is N + 1,
    dump_result(N1,B,Rest).

write_state([]) :- nl.
write_state([fluent(Name,Value)|Rest]) :-
    ( fd_var(Value),!, format("~q: <>  ",[Name]);
      format("~q = ~q\t",[Name,Value]) ),
    write_state(Rest).

write_action(A) :-
    find_action(A,Name),
    format(" ---->>   ~q ",[Name]),nl.

find_action([],unknown).
find_action([action(Name,Value)|_], Name) :-
    Value == 1,!.
find_action([_|Rest],Name) :-
    find_action(Rest,Name).

line :-
    write('*****************************************************************************'),
    nl.

write_list([]).
write_list([A|R]) :- write(A),nl,write_list(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% META PREDICATE FOR FINDING THE MINIMUM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta_plan(N,MIN) :-
  (sicsplanfail(N),!,MIN=N;
   M is N + 1, meta_plan(M,MIN)).
sicsplanfail(N) :-
    M is N + 1,  main(M,_,_),!.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% PORTING GNU => SICStus

%%% Replaced #==> with  #=>
%%% changed labeling -was
%%% fd_labeling(AllActions,[variable_method(ff), value_method(min)])
%%% REPLACED
%%% sum([],0).
%%% sum([A|B],Res) :- sum(B,Res1), Res #= A + Res1.
%%% with

sum(X,Res) :- sum(X,#=,Res).


%%% avoiding sicstus's between/3
%   in defining interval/3.
%interval(A,A,_).
%interval(X,A,B) :- A < B, C is A + 1, interval(X,C,B).

%%% ADDED

fd_domain_bool(X) :- X in 0..1.
fd_only_one(X) :- sum(X,#=,1).
fd_one_or_two(X) :- sum(X,#=,AA), AA  in 1..2.

bool_or([],0) :-!.
bool_or(List,Flag) :-  %%% Flag <-> 1 in List
    maximum(Flag,List).%%%    

bool_and([],1):-!.
bool_and(List,Flag) :- %%% Flag <-> 
    minimum(Flag,List).%%% Lists = [1,1,1...1] 

bool_or(A,B,C) :-
  append(A,B,T),
  bool_or(T,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%    END CODE    %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
