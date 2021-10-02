:- module(discord_client, [
        discord_start_listener/0,
        discord_say/2,
        discord_send/1,
        discord_ping/0,
        discord_get_websocket/1,
        is_thread_running/1,
        discord_ensure_im/2,
        any_to_id/2
        ]).

/** <module> discord_client - Provides a websocket API to write discord clients

*/

:- multifile(tmp:discord_info/3).
:- volatile(tmp:discord_info/3).
:- dynamic(tmp:discord_info/3).
reload_discord_info:- reconsult(guild_info_file).
load_discord_info:- exists_file(guild_info_file)->consult(guild_info_file);true.
clear_discord_info:- retractall(tmp:discord_info/3).
save_discord_info:- 
  tell(guild_info_file),
  format('
:- multifile(tmp:discord_info/3).
:- volatile(tmp:discord_info/3).
:- dynamic(tmp:discord_info/3).
'),
  R= discord_info(_,_,_),
  TmpR=tmp:R,  
  forall(TmpR,format('~q.~n',[TmpR])),
  told.
:- at_halt(save_discord_info).
:- load_discord_info.

disable_gateway.  % true until we fix our websocket code


:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(url)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/websocket)).

discord_grouping(messages).
discord_grouping(channels).
discord_grouping(roles).
discord_grouping(members).
discord_grouping(guilds).

:- if(exists_source(library(dicts))).
	:- use_module(library(dicts)).
:- endif.

:- if(exists_source(library(logicmoo_utils))).
	:- use_module(library(logicmoo_utils)).
:- endif.

:- if(exists_source(library(udt))).
    :- use_module(library(udt)).

:- oo_class_begin(discord_client).

% url	A WebSocket Message Server URL.
:- oo_class_field(url).

% '@me'	The authenticated bot user.
:- oo_inner_class_begin(clients).

discord_client:clients:new(Ref):- throw(clients:new(Ref)).

:- oo_inner_class_end(clients).



% '@me'	The authenticated bot user.
:- oo_inner_class_begin('@me').
:- oo_inner_class_end('@me').

% guild	Details on the authenticated user's guild.
:- oo_inner_class_begin(guild).
:- oo_inner_class_end(guild).

% users	A hash of user objects by user ID.
:- oo_inner_class_begin(users).
:- oo_inner_class_end(users).


% channels	A hash of channel objects, one for every channel visible to the authenticated user.
:- oo_inner_class_begin(channels).
:- oo_inner_class_end(channels).

% roles	A hash of role objects, one for every role the authenticated user is in.
:- oo_inner_class_begin('@me').
:- oo_inner_class_end('@me').

% ims	A hash of IM objects, one for every direct message channel visible to the authenticated user.
:- oo_inner_class_begin(chats).
:- oo_inner_class_end(chats).

% integrations	Details of the integrations set up on this guild.
:- oo_inner_class_begin(integrations).
:- oo_inner_class_end(integrations).

% text	textual utils.
:- oo_inner_class_begin(text).
:- oo_inner_class_end(text).

% debug	Debugger fidling.
:- oo_inner_class_begin(debug).
:- oo_inner_class_end(debug).

% events	Registered callbacks.
:- oo_inner_class_begin(events).
:- oo_inner_class_end(events).

% files	registered storage.
:- oo_inner_class_begin(files).
:- oo_inner_class_end(files).

:- oo_class_end(discord_client).

:- endif.



/* tests to see if logicmoo utils are installed.. If not, create the predicates it will use */
:- if( \+ current_predicate( wdmsg/1 )).

:- meta_predicate(with_visible_leash(0)).
with_visible_leash(G):-
   '$leash'(A, A),'$visible'(V, V),
   (tracing->CU=trace;CU=notrace),
   (debugging->CU2=debug;CU2=nodebug),!,
   call_cleanup(G, (notrace,'$leash'(_, A),'$visible'(_, V),call(CU2),call(CU))).

:- meta_predicate(rtrace(0)).
rtrace(G):-  with_visible_leash(( notrace,leash(-all),visible(+full),leash(+exception),trace,debug, call(G))).

:- meta_predicate(must(0)).
must(G):- G *->true;throw(must_failed(G)).

fresh_line:- format(user_error,'~N',[]).

nop(_).

:- endif.


if_debug(_).

ddbg(O):- into_dbg_string(O,OO),dmsg(OO).
%ddbg(F,Args):- if_debug((fresh_line, format(user_error,F,Args))).

is_thread_running(ID):-
  is_thread(ID), thread_property(ID,status(What)),!,
   (What==running->true;(thread_join(ID,_ ),!,fail)).


:- dynamic(discord_token/1).

% ===============================================
% How this module might find your token:
% ===============================================

% 1st - Checks for a local declaration 
%  (if the next line is uncommented and replaced by a real token )
% discord_token('xoxb-01234567890-xxxxxxxxxxxxxxxx').

% 2nd - Checks for a local file called ".discord_auth.pl" for discord_token/1 as above
:- if(( \+ discord_token(_) , exists_file('.discord_auth.pl'))).
:- include('.discord_auth.pl').
:- endif.

% 3rd - Checks env for DISCORD_API_TOKEN
%  ( defined by# export DISCORD_API_TOKEN=xoxb-01234567890-xxxxxxxxxxxxxxxx )
:- if(( \+ discord_token(_))).
:- getenv('DISCORD_API_TOKEN',Was)->asserta(discord_token(Was));true.
:- endif.

% 4th - Checks users config directory for file called ".discord_auth.pl"  discord_token/1 as above
:- if(( \+ discord_token(_) , exists_file('~/.discord_auth.pl'))).
:- include('~/.discord_auth.pl').
:- endif.

:- if(( \+ discord_token(_))).
:- throw(missing(discord_token(_))).
:- endif.

% ===============================================
% Utility functions
% ===============================================

discord_token_string(S):-discord_token(T),atom_string(T,S).

%curl -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" 
%  https://discord.com/api/v9/

me:-  discord_http(users/'@me').
guilds:- me, discord_http(users/'@me'/guilds).
channels:- guilds,
   %discord_http(guilds/{guilds-id}),
   discord_http(guilds/{guilds-id}/channels),
   %discord_http(guilds/{guilds-id}/members),
   %discord_http(guilds/{guilds-id}/roles),
   !.


:- flag(op_1,_,0).
discord_ping_op_1:- disable_gateway,!.
discord_ping_op_1:- sleep(41),discord_send({"op": 1,d: $seq}),discord_ping_op_1.

% should return wss://gateway.discord.gg
discord_get_websocket_url('wss://gateway.discord.gg/'):-!.
discord_get_websocket_url(URL):- discord_http(gateway), get_discord(url,URL).

into_discord_url_object(UCmd,Prop):- atomic_list_concat([_,I|ST],'/',UCmd),last([I|ST],Prop2),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(UCmd,Prop):- atomic_list_concat([Prop2,_|_],'?',UCmd),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(Prop,Prop).


discord_http(Cmd):- discord_http(Cmd,[]),!.
discord_http(Cmd,Opts):-
 must_det_l((
  into_discord_url(Cmd,UCmd),
  into_discord_url_object(UCmd,Prop),
  discord_http(Prop,Cmd,Opts))).

discord_http(Prop,Cmd,Opts):-
 must_det_l((
  into_discord_url(Cmd,UCmd),
  bot_discord_token(TokenHeader),
  sformat(URL,'https://discord.com/api/v9/~w',[UCmd]))),
  %wdmsg(URL=Opts),
  http_open(URL, In, [status_code(Status), 
          request_header('Authorization'=TokenHeader), 
          request_header('User-Agent'='DiscordBot'),
          request_header('Content-Type'='application/json')|Opts]),
  ignore((Status\==200,dmsg(status_code=Status))),
  must_det_l((json_read_dict(In,Term), close(In), discord_receive(Prop,Term))),
  nop(listing(tmp:discord_info/3)),!.



into_discord_url(A,O):- \+ compound(A),!,A=O.
into_discord_url('$'(A),O):- !, get_discord(A,M),into_discord_url(M,O),!.
into_discord_url(A / B,O):- !, into_discord_url(A,AA),into_discord_url(B,BB),!,sformat(O,"~w/~w",[AA,BB]).
into_discord_url({A - B},O):- !, get_discord(A,B,M),into_discord_url(M,O).
into_discord_url(A,O):- A=O.

:- dynamic(tmp:discord_websocket/3).

discord_get_websocket(WS):- tmp:discord_websocket(WS,_,_),!.
discord_get_websocket(WS):-
   discord_get_websocket_url(URL),!,
   discord_open_websocket(URL,WS),!.

discord_open_websocket(URL,WS):-
   ignore(tmp:discord_websocket(OLD_WS,_,_)),
   %atom_concat(URL,'?v=9&encoding=json',UrlV9),
   http_open_websocket(URL, WS, []),
   stream_pair(WS,I,O),
   show_call(asserta(tmp:discord_websocket(WS,I,O))),
   (nonvar(OLD_WS)->discord_remove_websocket(OLD_WS);true).

discord_remove_websocket(OLD_WS):-
   ignore(retract(tmp:discord_websocket(OLD_WS,_,_))),
   ignore(catch(ws_close(OLD_WS,1000,''),_,true)).

% ===============================================
% Property Names
% ===============================================
skip_propname(K):- var(K),!.
skip_propname(_-_):-!,fail.
skip_propname(Type):-string(Type),!,string_to_atom(Type,K),!,skip_propname(K).
skip_propname(rtm).
skip_propname(gateway).
skip_propname(data).
skip_propname(var).

discord_propname(Type,var):-var(Type),!.
discord_propname(Type,K):-string(Type),!,string_to_atom(Type,K).
discord_propname(Key-Type,NewType):-!,discord_propname(Key,Type,NewType).
%discord_propname(Dict,NewType):- nonvar(Dict),Dict=Key.Type,nonvar(Type),!,discord_propname(Key,Type,NewType).
%discord_propname(Key.Type,NewType):-!,discord_propname(Key,Type,NewType).
discord_propname(Key,Key).

discord_propname(Type,Key,NewType):- skip_propname(Type),!,discord_propname(Key,NewType).
discord_propname(Key,Type,NewType):- skip_propname(Type),!,discord_propname(Key,NewType).
discord_propname(_Type,Key,NewType):-discord_propname(Key,NewType).

discord_start_listener:- disable_gateway,!.
discord_start_listener:- is_thread_running(discord_start_listener),!,mmake.
discord_start_listener:- thread_create(discord_listener_proc,_,[alias(discord_start_listener)]),!.

discord_listener_proc:-
 call_cleanup((
  repeat,
  once(discord_get_websocket(WS)),
  once(ws_receive(WS,Data,[format(json)])),
  (Data==
    end_of_file->!;
  (once(discord_receive(gateway,Data)),flush_output,fail))),
  discord_remove_websocket(WS)).



undict(ID,IDO):- is_dict(ID),ID.IDK=IDV,IDK=id,IDO=IDV.
undict(ID,ID).


% ignored
discord_event(reconnect_url,_Dict):-!.
/*
  must((Dict.url=URL,
   ddbg(reconnect(URL)),!,
   ddbg(discord_open_websocket(URL,_)))).
*/

% simplify the data objects
discord_event(Type,O):- is_dict(O),O.Key=Data,Key=data,!,discord_receive(Type,Data),!.
% typify the data objects
discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=type,!,discord_receive(Type,O),!.
% typify the data objects
discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=op,!,discord_receive(Type,O),!.


% text:"This content can't be displayed."

% Notice newly created IMs
discord_event(im_open,Dict):- is_dict(Dict),
  Dict.channel=IDI,
  Dict.user=User,
  undict(IDI,ID),
  string_to_atom(ID,IDA),
  add_discord_info(ims, hasInstance- IDA),
  add_discord_info(IDA, id- ID),
  add_discord_info(IDA, user-User).

discord_event(Evt,end_of_file):- throw(discord_event(Evt,end_of_file)).


% discord_event(Type,Data):-add_discord_info(now,Type,Data).

discord_unused(user_typing).
discord_unused(reconnect_url).


discord_receive(Type,Data):- number(Type), gw_op(Type,Dispatch,_,_),!,discord_receive(Dispatch,Data).

discord_receive(Type,Data):- ddbg((discord_receive(Type,Data))),fail.
discord_receive(Type,Data):- add_discord_info(Type,Data),fail.

discord_receive(hello,_Data):- % discord_send({op:11}), 
   sleep(1),discord_send({op:1,d:null}),!,discord_identify. 

discord_receive(Type,Data):- (string(Data),(string_to_dict(Data,Dict)->true;string_to_atom(Data,Dict)))->discord_receive(Type,Dict),!.
discord_receive(Type,Data):- discord_propname(Type,NewType)-> Type\==NewType,!,discord_receive(NewType,Data).
discord_receive(Type,Dict):- type_to_url(K,Type)-> K\==Type,!, discord_receive(K,Dict).
discord_receive(Type,Data):- discord_event(Type,Data),!.
discord_receive(Type,Data):- discord_inform(Type,Data),!.
discord_receive(Type,Data):- discord_unused(Type), nop(ddbg(unused(discord_receive(Type,Data)))),!.
%discord_receive(Type,Data):- ddbg(unknown(discord_receive(Type,Data))).
discord_receive(_Type,_Data).

request_members:- 
discord_send({
  "op": 8,
  d: {
    "guild_id": $guild_id,
    "query": "",
    "limit": 0
  }
}).

discord_identify:- 
 discord_send(
  _{
  'op': 2,
  d: _{
    'token': $token,
    'properties': _{
      '$os': "linux",
      '$browser': "disco",
      '$device': "disco"
    }, 
  'intents': 65535
  }
}).

discord_resume:- 
 discord_send( {
  "op": 6,
  d: {
    "token": $token,
    "session_id": $session_id,
    "seq": $seq
  }
}).




discord_inform(Type,Data):-is_dict(Data),Data.Key=ID,Key=id,!,string_to_atom(ID,Atom),
   add_discord_info(Type,Atom-Data).
discord_inform(rtm,Data):- is_list(Data),!, maplist(discord_receive(rtm),Data).


discord_inform(Type,Key-[A|Data]):-is_dict(A),is_list(Data),!,maplist(discord_receive(Type-Key),[A|Data]).

discord_inform(Type,Key-Data):- % atomic(Key),atomic_list_concat([Type,Key],'_',TypeKey),ddbg(list(discord_receive(TypeKey,Data))),
  is_list(Data),!,
  retractall(tmp:discord_info(Type,Key,_)),
  maplist(add_discord_info3(Type,Key),Data).

discord_inform(Type,Key-Data):- atomic(Data),!,add_discord_info(Type,Key-Data).
discord_inform(Type,Key-Data):- is_dict(Data),dict_pairs(Data,Tag,Pairs),maplist(discord_receive(Type-Key-Tag),Pairs).
discord_inform(Type,Key-Data):- add_discord_info(Type,Key-Data).


% https://discord.com/oauth2/authorize?response_type=code&client_id=157730590492196864&scope=identify+guilds.join&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fgoogle.com&prompt=consent
rtrv_messages:- forall(channel_to_id(_Channel,ID),  (sleep(1),discord_http(channels/ID/'messages?limit=3'))).
rtrv_messages(Channel):- channel_to_id(Channel,ID),  discord_http(channels/ID/messages).

get_kv(K:V,K,V):- must(nonvar(K);throw(get_kv(K:V,K,V))).
get_kv(K-V,K,V).
get_kv(K=V,K,V).


% curl -H "Authorization: Bot $AUTH_TOKEN" -H "User-Agent: DiscordBot" https://discord.com/api/channels/892809238710222930/messages


add_discord_info(Type,Pairs):- is_list(Pairs),select(KV,Pairs,Rest),get_kv(KV,id,ID),!,
  add_discord_info3(ID,isa,Type), add_discord_info3(Type,hasInstance,ID),
  add_discord_info3(ID,id,ID),
  add_discord_info(ID,Rest).
add_discord_info(Type,Pairs):- is_list(Pairs), Pairs\==[], !, maplist(add_discord_info(Type),Pairs).

add_discord_info(Type,KV):- get_kv(KV,K,V),!,add_discord_info3(Type,K,V).
add_discord_info(Type,Data):- is_dict(Data),dict_pairs(Data,_Tag,Pairs),!,add_discord_info(Type,Pairs).
add_discord_info(Type,Data):- %retractall(tmp:discord_info(Type,_,_)),
  add_discord_info3(Type,hasInstance,Data).


%753344235805343785
int_to_name(S,V):- S>1420070400,get_time(T),TT is T + 6000,TT>S,stamp_date_time(S,Date,local),!,
  format_time(string(V),'[%a, %d %b %Y %T PST]',Date,posix).
int_to_name(S,V):- tmp_discord_info(S,name,V),!.
int_to_name(S,V):- tmp_discord_info(S,username,V),!.
%int_to_name(S,V):- tmp_discord_info(V,content,S),!.
int_to_name(S,V):- S> 4194304, id_to_time(S,T),int_to_name(T,TT),sformat(V,'<~q~w>',[S,TT]).

into_dbg_string(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(into_dbg_string,As,AAs),!,compound_name_arguments(V,F,AAs).
into_dbg_string(S,V):- string(S), catch(atom_number(S,N),_,fail), !, into_dbg_string(N,V).
into_dbg_string(S,V):- number(S), int_to_name(S,V),!.
into_dbg_string(V,V).

from_string(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(from_string,As,AAs),!,compound_name_arguments(V,F,AAs).
from_string(S,V):- \+ string(S), \+ atom(S), !, V=S.
from_string(S,V):- atom_length(S,L),L>40,!,S=V.
from_string(S,V):- \+ atom(S), text_to_string(S,SS),string_to_atom(SS,A),catch(atom_number(A,V),_,fail),!.
from_string(S,V):- parse_time(S,_,V),!.
from_string(V,V).

id_to_time(ID,UTC):- integer(ID), UTC is (( ID >> 22) / 1000) + 1420070400.
  
add_discord_info3(ID,Prop,Data):- 
  from_string(ID,ID2), from_string(Prop,Prop2), from_string(Data,Data2),!,
  add_discord_info4(ID2,Prop2,Data2).

add_discord_info4(_,Prop,Data):- default_info(Prop,Data),!.
add_discord_info4(ID,Prop,Data):-
  TmpR=tmp:R,
  R= discord_info(ID,Prop,Data),
  (\+ \+ call(TmpR) -> (retract(TmpR),assert(TmpR)) ; (asserta(TmpR),dmsg(TmpR))).

get_discord(ID,Data):- tmp_discord_info(ID,hasInstance,Data).
get_discord(ID,Prop,Value):- tmp_discord_info(ID,Prop,Value)*->true;get_discord2(ID,Prop,Value).

get_discord2(Type,Prop,Value):- tmp_discord_info(Type,hasInstance,ID),tmp_discord_info(ID,Prop,Value).

get_discord_info(ID,Prop,Data):- tmp_discord_info(ID,Prop,Data)*-> true;
  (\+ integer(ID), \+ var(ID), any_to_id(ID,ID2),!, tmp_discord_info(ID2,Prop,Data)).

%check_unifier(U1,U2):- nonvar(U1),var(U2),!,freeze(U2,check_unifier_final(U1,U2)).
%check_unifier(U2,U1):- nonvar(U1),var(U2),!,freeze(U2,check_unifier_final(U1,U2)).
%check_unifier(U2,U1):- var(U1),var(U2),!,freeze(U2,check_unifier_final(U1,U2)). 
check_unifier(U1,U2):- var(U1),!,check_unifier_var_1(U1,U2).
check_unifier(U2,U1):- var(U1),!,check_unifier_var_1(U1,U2).
check_unifier(U1,U2):- check_unifier_final(U1,U2).

check_unifier_var_1(U1,U2):- nonvar(U2),!,U1=U2.
check_unifier_var_1(U1,U2):- \+ frozen(U2,_), !, freeze(U2,check_unifier(U1,U2)).
check_unifier_var_1(U1,U2):- U1=U2.


check_unifier_final(U1,U2):- any_to_each(U1,ID1),any_to_each(U2,ID2),ID1==ID2,!.

any_to_each(U1,U1).
any_to_each(U1,Each):- nonvar(U1), any_to_each_1(U1,Each), U1\==Each.

any_to_each_1(U1,Each):- catch(text_to_string(U1,Each),_,fail).
any_to_each_1(U1,Each):- term_to_atom(U1,Each). 
any_to_each_1(U1,Each):- from_string(U1,Each), \+ integer(Each).
any_to_each_1(U1,Each):- any_to_id(U1,Each).


discord_name_id_type(Name,ID,Type):- nonvar(Type),  !,
  (tmp_discord_info(Type,hasInstance,ID)*->true;tmp_discord_info(ID,isa,Type)),
  once(tmp_discord_info(ID,name,Name);tmp_discord_info(ID,username,Name)).
discord_name_id_type(Name,ID,Type):- 
  (tmp_discord_info(ID,username,Name);tmp_discord_info(ID,name,Name)),
  integer(ID),
  once(tmp_discord_info(Type,hasInstance,ID);tmp_discord_info(ID,isa,Type)).

tmp_discord_info(A,B,C):- tmp:discord_info(A,B,C).
  %get_discord(Type,hasInstance,ID),
  %get_discord(ID,id,ID),
  %true.872902388623757364


channel_to_id(Name,ID):- discord_name_id_type(Name,ID,channels).
%channel_to_id("prologmud_server",892806433970716692).
%channel_to_id(Name,ID):- any_to_id(Name,ID).

any_to_id(Name,ID):-var(Name),!, fail,ID=Name.
any_to_id(Name,ID):-integer(Name),ID=Name.
any_to_id(Name,ID):-catch(text_to_string(Name,NameS),_,fail),discord_name_id_type(NameS,ID,_),integer(ID),!.
any_to_id(Name,ID):-from_string(Name,ID),integer(ID),!.
%any_to_id(Name,ID):-text_to_string(Name,NameS),get_discord(_,hasInstance,ID), get_discord(ID,_,NameS),!.

same_ids(ID,IDS):-any_to_id(ID,IDA),any_to_id(IDS,IDB),IDA==IDB.

discord_ensure_im2(ID,IM):- get_discord(IM,user,ID),!.

discord_ensure_im(To,IM):- get_discord(IM, name, To), get_discord(IM, is_channel, true),!.
discord_ensure_im(To,IM):- any_to_id(To,ID),!, discord_ensure_im(ID,IM).
discord_ensure_im(To,IM):- discord_ensure_im2(To,IM),!.
% OLD discord_ensure_im(To,IM):- any_to_id(To,ID), discord_send({type:'im_open',user:ID}),!,must(discord_ensure_im2(To,IM)),!.
discord_ensure_im(To,IM):- discord_send({type:'conversations_open',users:To}),!,must(discord_ensure_im2(To,IM)),!.


discord_id_time(ID,TS):-flag(discord_id,OID,OID+1),ID is OID+1,get_time(Time),number_string(Time,TS).


discord_me(Self):-get_discord('@me', id, Self).

%  {"id":2,"type":"ping","time":1484999912}
discord_ping :- discord_id_time(ID,_),get_time(Time),TimeRnd is round(Time),discord_send({"id":ID,"type":"ping", "time":TimeRnd}).


find_discord_info(Str):-
 forall(tmp:discord_info(X,Y,Z),
 ignore((sformat(S,'~q.',[tmp:discord_info(X,Y,Z)]),sub_string(S, _Offset0, _Length, _After, Str),
   ddbg(S)))).

%discord_say:- discord_say(_,'From Prolog').
% {"id":3,"type":"message","channel":"D3U47CE4W","text":"hi there"}
discord_say :- discord_say('#prologmud_server',"test message to logicmoo").
discord_say2:- discord_say(dmiles,"test message to dmiles").
discord_say3:- discord_say(general,"test message to general channel").
% https://discord.com/oauth2/authorize?client_id=772113231757574185&scope=bot&permissions=268823638
discord_say(Channel,Msg):- 
 any_to_id(Channel,ID), 
 any_to_string(Msg,Str),
 % Nick = 'some1',
 % atomic_string_concat(Str,StrO),
 Str=StrO,
  Dict=
    _{username: "irc0", content: StrO,
       avatar : "98fb2a9b870148862265b65d02b5d200",
    %embeds: [_{ title: "Hello, Embed!", description: "This is an embedded message."},
    tts: false},
 %sformat(S,'~q',[Dict]),
 %ddbg(post=S),
 discord_http(channels/ID/messages,[post(json(Dict))]).
discord_say(To,Msg):-
  discord_ensure_im(To,IM),
  discord_send_im(IM,'irc0',Msg).

discord_send_im(IM,From,Msg):-
    discord_send({
            type: "message", 
            username:From,
	    channel: IM,
            text: Msg
	   }),!.

discord_post(Cmd,Params,NewDict):- 
          discord_token(Token),
	  make_url_params(Params,URLParams),
	  format(string(S),'https://discord.com/api/v9/~w',[Cmd,Token,URLParams]),
	  dmsg('~N DISCORD-POST ~q ~n',[S]),!,
	  http_open(S,Out,[]),!,
	  json_read_dict(Out,Dict),
	  dict_append_curls(Dict,Params,NewDict),!.
	  

discord_post(Cmd,Params):-
  discord_post(Cmd,Params,NewDict),
	  discord_receive(Cmd,NewDict).

dict_append_curls(Dict,Params,NewDict):-any_to_curls(Params,Curly),
	dict_append_curls3(Dict,Curly,NewDict).

dict_append_curls3(Dict,{},Dict):-!.
dict_append_curls3(Dict,{Curly},NewDict):-!,dict_append_curls3(Dict,Curly,NewDict).
dict_append_curls3(Dict,(A,B),NewDict):-!,dict_append_curls3(Dict,A,NewDictM),dict_append_curls3(NewDictM,B,NewDict).
dict_append_curls3(Dict,KS:V,NewDict):- string_to_atom(KS,K), put_dict(K,Dict,V,NewDict).

discord_history(To,History):- discord_ensure_im(To,IM),
 % (get_discord(IM, is_channel, true)-> Method = 'channels.history' ; Method = 'conversations.history'),
  Method = 'conversations.history',
  discord_send_receive({type:Method,channel:IM},History).


bot_discord_token(TokenHeader):- discord_token(Token),sformat(TokenHeader,"Bot ~w",[Token]).


string_to_dict:-
 string_to_dict("{\"type\":\"dnd_updated_user\",\"user\":\"U3T3R279S\",\"dnd_status\":{\"dnd_enabled\":false,\"next_dnd_start_ts\":1,\"next_dnd_end_ts\":1},\"event_ts\":\"1485012634.280271\"}",Dict),
  ddbg(Dict).

string_to_dict(String,Dict):-
   open_string(String,Stream),
   catch(json_read_dict(Stream,Dict),_,fail),!.



type_to_url("message",'chat.postMessage').
type_to_url("im_open",'im.open').
type_to_url("conversations_open",'conversations.open').
type_to_url(X,X):-!.

make_url_params({In},Out):-!,make_url_params(In,Out).
make_url_params((A,B),Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A|B],Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A],Out):-!,make_url_params(A,Out).
make_url_params(KV,Out):-get_kv(KV,K,A),www_form_encode(A,AA),format(atom(Out),'~w=~w',[K,AA]).


discord_send(DataI):- any_to_curls(DataI,Data),discord_send000(Data).
discord_send_receive(DataI,Recv):- any_to_curls(DataI,Data),discord_send_receive000(Data,Recv).


discord_send_receive000({"type":TypeT,Params},Recv):- text_to_string(TypeT,Type), type_to_url(Type,Cmd),!, discord_post(Cmd,Params,Recv).
% @TODO comment the above and fix this next block
discord_send_receive000(Data,Recv):- discord_send_ws(WebSocket,Data),!, once(ws_receive(WebSocket,Recv,[format(json)])),!.

discord_send000({"type":TypeT,Params}):- text_to_string(TypeT,Type), type_to_url(Type,Cmd),!, discord_post(Cmd,Params).
% @TODO comment the above and fix this next block
discord_send000(Data):- discord_send_ws(_WebSocket,Data),!.



discord_send_ws(WebSocket,Data):- tmp:discord_websocket(WebSocket, _WsInput, WsOutput), !, flush_output(WsOutput), discord_send_ws(WsOutput,Data),!.
discord_send_ws(WsOutput,Data):- format(WsOutput,'~q',[Data]),flush_output(WsOutput),ddbg(discord_sent(Data)),flush_output.


%dict_to_curly(Dict,{type:Type,Data}):- del_dict(type,Dict,Type,DictOut),dict_pairs(DictOut,_,Pairs),any_to_curls(Pairs,Data).
%dict_to_curly(Dict,{type:Type,Data}):- dict_pairs(Dict,Type,Pairs),nonvar(Type),any_to_curls(Pairs,Data).
dict_to_curly(Dict,{Data}):- dict_pairs(Dict,_,Pairs),list_to_curls(Pairs,Data).

list_to_curls([A],AA):-!,any_to_curls(A,AA).
list_to_curls([A|B],(AA,BB)):-!,any_to_curls(A,AA),list_to_curls(B,BB).

any_to_curls(Dict,Data):- is_dict(Dict),!,dict_to_curly(Dict,Data).
any_to_curls(Var,"var"):- \+ must(\+ var(Var)),!.
any_to_curls(null,null).
any_to_curls(true,true).
any_to_curls(false,false).
any_to_curls(KV,AA:BB):-get_kv(KV,A,B),!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls('$'(Var),ValO):- get_discord(Var,Val),!,any_to_curls(Val,ValO).
any_to_curls({DataI},{Data}):-!,any_to_curls(DataI,Data).
any_to_curls((A,B),(AA,BB)):-!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls(A,O):- string(A),!,from_string(A,O).
any_to_curls(A,O):- catch(text_to_string(A,AA),_,fail),!,any_to_curls(AA,O).
any_to_curls(A,O):- from_string(A,O).

gw_op(0,'dispatch','receive','an event was dispatched.').
gw_op(1,'heartbeat',_,'fired periodically by the client to keep the connection alive.').
gw_op(2,'identify','send','starts a new session during the initial handshake.').
gw_op(3,'presence update','send','update the client''s presence.').
gw_op(4,'voice state update','send','used to join/leave or move between voice channels.').
gw_op(6,'resume','send','resume a previous session that was disconnected.').
gw_op(7,'reconnect','receive','you should attempt to reconnect and resume immediately.').
gw_op(8,'request guild members','send','request information about offline guild members in a large guild.').
gw_op(9,'invalid session','receive','the session has been invalidated. you should reconnect and identify/resume accordingly.').
gw_op(10,'hello','receive','sent immediately after connecting and contains the heartbeat_interval to use.').
gw_op(11,'heartbeat_ack','receive','sent in response to receiving a heartbeat to acknowledge that it has been received.').

default_guild(748871194572226661).

no_default_info(topic).
no_default_info(position).
no_default_info(parent_id).
no_default_info(hasInstance).


default_info(X,_):- nonvar(X), no_default_info(X),!,fail.
default_info(flags,0). % probably this doesnt belong
default_info(accent_color,null).
default_info(attachments,[]).
default_info(avatar,null).
default_info(banner,null).
default_info(banner_color,null).
default_info(components,[]).
default_info(edited_timestamp,null).
default_info(embeds,[]).
default_info(guild_id,GuildID):- default_guild(GuildID).
default_info(last_message_id,null).
default_info(mention_everyone,false).
default_info(mention_roles,[]).
default_info(mentions,[]).
default_info(nsfw,false).
default_info(permission_overwrites,[]).
default_info(pinned,false).
default_info(rate_limit_per_user,0).
default_info(rtc_region,null).
default_info(tts,false).
default_info(type,0).
default_info(user_limit,0).
default_info(public_flags,0).
default_info(email,null).
default_info(features,[]).
default_info(messages,[]).
default_info(owner,false).


default_info(X,Y):- default_info_value(Y),!, dmsg(default_info(X,Y)).

default_info_value(null).
default_info_value(0).
default_info_value([]).
default_info_value(false).


:- fixup_exports.


:- if( \+ prolog_load_context(reloading, true)).
%:- bot_discord_token(TokenHeader), add_discord_info(token,TokenHeader).
:- bot_discord_token(TokenHeader), add_discord_info(token,TokenHeader).
:- get_time(Time), ITime is integer(Time), add_discord_info(time,ITime).
:- add_discord_info(seq,null).
:- channels.
:- endif.

% start discord listener in a thread
:- discord_start_listener.

:- if(( \+ (is_thread_running(discord_start_listener)))).
:- show_call(thread_create(discord_start_listener,_,[alias(discord_start_listener),detached(true)])).
:- endif.

:- if(( \+ (is_thread_running(discord_ping_op_1)))).
:- show_call(thread_create(discord_ping_op_1,_,[alias(discord_ping_op_1),detached(true)])).
:- endif.


end_of_file.


% if the above fails .. run in debug mode
:- if(( \+ (is_thread_running(discord_start_listener)))).
:- discord_listener_proc -> true; rtrace(discord_listener_proc).
:- endif.

% start discord listener in a thread
:- if(( \+ (is_thread_running(discord_start_listener)))).
:- thread_create(discord_start_listener,_,[alias(discord_start_listener),detached(true)]).
:- endif.

% if the above fails .. run in debug mode
:- if(( \+ (is_thread_running(discord_start_listener)))).
:- rtrace(discord_start_listener).
:- endif.


