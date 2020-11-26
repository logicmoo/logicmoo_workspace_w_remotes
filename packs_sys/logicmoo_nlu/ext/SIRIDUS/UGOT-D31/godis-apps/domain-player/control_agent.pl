:- use_module('$TRINDIKIT/core/prolog/trindikit').
:- ensure_loaded(app_search_paths).
:- use_module(trindikit(tkit_control_access),[control/0]).


startit:-
	setprop(control,yes),
	control_algorithm(Ctrl),
	setprop(control-algorithm,Ctrl),
	setprop(oaa-name,'control'),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa,yes),
	control.


%CONTROL ALGORITHM

control_algorithm(A):-
  setof(Trig=>Algo,Trig=>Algo,A).

init => [input:init,
	 interpret:init,
	 generate:init,
	 output:init,select,generate,output,update].

condition(not $program_state ==  run) => [input:quit,
					  output:quit,
					  interpret:quit,
					  generate:quit].

				%condition($latest_speaker == sys) => update,
condition(is_set(input_queue)) => [if not is_empty($input_queue) then
				     [oaa_Solve(start_timer(input,1.0))]].
condition(alarm(input)) => [InputQueueToInput,
			    interpret,update,print_state |SysTurn]:-

	InputQueueToInput = apply_rule(moveInputQueueToInput,
				       [$input_queue=Q ],
				       [clear(input_queue),
					clear(input),
					forall_do(in(Q,String),
					       [ append(input,String),
						 append(input," ") ] ) ]),
						   
	
	SysTurn = [  repeat( [ select,
			       test(not is_empty($next_moves)),
			       generate,
				%check_condition(to_atom($output,B)),
				%oaa_Solve(outputGuiTxt(B)),
			       %check_condition(to_atom($output_gui,A)),
			       %oaa_Solve(outputGui(A)),
			       %print_state,
			       output,
			       %apply_update(clear(next_moves)),
			       update,
			       print_state,
			       test(not is_empty($/private/agenda)),
			       test(is_empty($/shared/qud))
			     ])%,
		     %if $program_state == run then
		  %[oaa_Solve(start_timer(input,4.0))]
		  ].


%%% init => [input:init,
%%% 	 interpret:init,
%%% 	 generate:init,
%%% 	 output:init,
%%% 	 select].

%%% condition(not $program_state ==  run) => [input:quit,
%%% 					  output:quit,
%%% 					  interpret:quit,
%%% 					  generate:quit].

%%% condition(is_set(input)) => [interpret].

%%% %condition(not empty($next_moves)) => [generate,output,update].

%%% condition($latest_speaker == usr) => [update,select,generate,output,
%%% 				      check_condition(to_atom($output,B)),
%%% 				      oaa_Solve(outputGuiTxt(B)),
%%% 				      check_condition(to_atom($output_gui,A)),
%%% 				      oaa_Solve(outputGui(A)),
%%% 				      update].

