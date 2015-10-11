:- module(send_check, [send_comp_rtcheck/3]).

:- use_module(xlibrary(context_values)).
:- use_module(xlibrary(intercept)).

get_comp_rtcheck_info(Goal, Info) :-
    ( current_context_value(rtchecks_rt:comp_info, Info)
    ->true
    ; Info = info(Goal, [], _)
    ).

send_comp_rtcheck(Goal, PropName, FailName) :-
    FailName =.. [F|Args],
    FailProp =.. [F, PredName|Args],
    get_comp_rtcheck_info(Goal, info(PredName, Dict, ALoc)),
    send_signal(rtcheck(asr, comp, PredName, Dict, [PropName-[FailProp]], ALoc)).
