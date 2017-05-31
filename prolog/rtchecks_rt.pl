/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(rtchecks_rt, 
	  [rtcheck_goal/2,
	   start_rtcheck/2,
	   rtc_call/2,
	   '$with_gloc'/2,
	   '$with_asr_head'/2]).

:- use_module(library(apply)).
:- use_module(library(assertions)).
:- use_module(library(rtchecks_flags)).
:- use_module(library(context_values)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(implementation_module)).
:- use_module(library(resolve_calln)).
:- use_module(library(send_check)).
:- use_module(library(clambda)).
:- use_module(library(ctrtchecks)).
:- reexport(library(ctrtchecks), [check_call/3]).

/** <module> Predicates that are required to implement run-time checks

Algorithm:
----------

pred :- body.

is executed as if where transformed to:

* *Step one*

```
pred :-
     "check entry...", 
     "check exit...",
     'pred$rtc1'.
```

* *Step two*
```
'pred$rtc1' :-                
        "check compat pre..." 
        "check calls...",     
        "check success pre",  
        "check comp..."(      
        'pred$rtc2',          
        "check success pos",  
        "check compat pos..." 

'pred$rtc2' :-
        body.
```

However the current implementation is an interpreter, rather than a compiler,
since SWI-Prolog is fully dynamic and the status of a module could change at
run-time. A future improvement could be to apply a partial evaluator to the
interpreter.

*/

:- meta_predicate '$with_asr_head'(0, ?).
'$with_asr_head'(Comp, AsrHead) :-
    with_value(Comp, '$with_asr_head', AsrHead).

:- meta_predicate '$with_gloc'(0, ?).
'$with_gloc'(Comp, GLoc) :-
    with_value(Comp, '$with_gloc', GLoc).

check_cond(Cond, Check, PredName) :-
    ( Cond
    ->send_check([[]/Check-[]], pp_check, PredName, [])
    ; true
    ).

ppassertion_type_goal(check(Goal), check, Goal).
ppassertion_type_goal(trust(Goal), trust, Goal).
ppassertion_type_goal(true( Goal), true,  Goal).
ppassertion_type_goal(false(Goal), false, Goal).

:- meta_predicate rtcheck_goal(0, 1).
rtcheck_goal(CM:Goal0, Call) :-
    resolve_calln(Goal0, Goal),
    ( ppassertion_type_goal(Goal, Type, Pred)
    ->rtc_call(Type, CM:Pred)
    ; implementation_module(CM:Goal, M),
      collect_rtasr(Goal, CM, Pred, M, RAsrL),
      check_goal(rt, Pred, call(Call, CM:Pred), M, CM, RAsrL)
    ).

:- meta_predicate start_rtcheck(+, 0).
start_rtcheck(M:Goal0, CM:WrappedHead) :-
    resolve_calln(Goal0, Goal),
    collect_rtasr(Goal, CM, Pred, M, RAsrL),
    check_goal(rt, Pred, M:WrappedHead, M, CM, RAsrL).

collect_rtasr(Goal, CM, Pred, M, RAsrL) :-
    qualify_meta_goal(Goal, M, CM, Pred),
    collect_assertions(rt, Pred, M, AsrL),
    maplist(wrap_asr_rtcheck, AsrL, RAsrL).

wrap_asr_rtcheck(Asr, rtcheck(Asr)).

% ----------------------------------------------------------------------------

:- meta_predicate rtc_call(+, 0).

rtc_call(Type, Check) :-
    ignore(do_rtcheck(Type, Check)).

rtcheck_ifnot(Check, PredName) :-
    check_cond(\+ Check, Check, PredName).

do_rtcheck(check, Check) :-
    rtcheck_ifnot(Check, check/1).
do_rtcheck(trust, Check) :-
    current_prolog_flag(rtchecks_trust, yes),
    rtcheck_ifnot(Check, trust/1).
do_rtcheck(true, Check) :-
    current_prolog_flag(rtchecks_true, yes),
    rtcheck_ifnot(Check, true/1).
do_rtcheck(false, Check) :-
    current_prolog_flag(rtchecks_false, yes),
    check_cond(Check, Check, false/1),
    fail.

sandbox:safe_meta_predicate(rtchecks_rt:start_rtcheck/2).
