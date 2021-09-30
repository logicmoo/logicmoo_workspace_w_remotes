:- module(discord_client, [
        discord_start_listener/0,
        discord_say/2,
        discord_send/1,
        discord_ping/0,
        discord_get_websocket/1,
        is_thread_running/1,
        discord_ensure_im/2,
        name_to_id/2
        ]).

/** <module> discord_client - Provides a websocket API to write discord clients

*/


:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(url)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/websocket)).


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

% self	The authenticated bot user.
:- oo_inner_class_begin(clients).

discord_client:clients:new(Ref):- throw(clients:new(Ref)).

:- oo_inner_class_end(clients).



% self	The authenticated bot user.
:- oo_inner_class_begin(self).
:- oo_inner_class_end(self).

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
:- oo_inner_class_begin(self).
:- oo_inner_class_end(self).

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
:- oo_inner_class_begin(self).
:- oo_inner_class_end(self).

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

ddbg(O):- wdmsg(O).
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
discord_ping_op_1:- sleep(41),flag(op_1,X,X+1),discord_send({"op": 1,"d": X}),discord_ping_op_1.

% should return wss://gateway.discord.gg
discord_get_websocket_url('wss://gateway.discord.gg/?v=9&encoding=json'):-!.
discord_get_websocket_url(URL):- discord_http(gateway), get_discord(url,URL).

into_discord_url_prop(UCmd,Prop):- atomic_list_concat(List,'/',UCmd),last(List,Prop).


discord_http(Cmd):- discord_http(Cmd,[]),!.
discord_http(Cmd,Opts):-
 must_det_l((
  into_discord_url(Cmd,UCmd),
  into_discord_url_prop(UCmd,Prop),
  bot_discord_token(TokenHeader),
  sformat(URL,'https://discord.com/api/v9/~w',[UCmd]))),
  wdmsg(URL=Opts),
  http_open(URL, In, [status_code(Status), 
          request_header('Authorization'=TokenHeader), 
          request_header('User-Agent'='DiscordBot'),
          request_header('Content-Type'='application/json')|Opts]),
  ignore((Status\==200,wdmsg(status_code=Status))),
  must_det_l((json_read_dict(In,Term), close(In), discord_receive(Prop,Term))),
  nop(listing(tmp:discord_info/3)),!.



into_discord_url(A,O):- \+ compound(A),!,A=O.
into_discord_url('$'(A),O):- !, get_discord(A,M),into_discord_url(M,O).
into_discord_url(A / B,O):- !, into_discord_url(A,AA),into_discord_url(B,BB),!,sformat(O,"~w/~w",[AA,BB]).
into_discord_url('-'(A , B),O):- !, get_discord(A,B,M),into_discord_url(M,O).
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
  add_discord_info(ims, hasValue- IDA),
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
   discord_send({op:1,"d":null}),!,discord_identify. %heartbeat_ack

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
  "d": {
    "guild_id": $guild_id,
    "query": "",
    "limit": 0
  }
}).

discord_identify:- 
 discord_send(
  {
  "op": 2,
  "d": {
    "token": $token,
    "properties": {
      "$os": "linux",
      "$browser": "disco",
      "$device": "disco"
    },
    "compress?": false,
    "large_threshold?": 250,
    "shard?": [0, 1],
    "presence": {
      "activities": [{
        "name": "irc0",
        "type": 0
      }],
      "status": "dnd",
      "since": $time,
      "afk": false
    },
   % // This intent represents 1 << 0 for GUILDS, 1 << 1 for GUILD_MEMBERS, and 1 << 2 for GUILD_BANS
   % // This connection will only receive the events defined in those three intents
    "intents": 7
  }
}).

discord_resume:- 
 discord_send( {
  "op": 6,
  "d": {
    "token": $token,
    "session_id": $session_id,
    "seq": $seq
  }
}).



:- dynamic(tmp:discord_info/3).

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

post_message:- post_message(_,'From Prolog').
post_message(Channel,Msg):- 
 channel_to_id(Channel,ID), 
 any_to_string(Msg,Str),
  Dict=
    _{username: "irc0", content: Str,
       avatar : "98fb2a9b870148862265b65d02b5d200",
    %embeds: [_{ title: "Hello, Embed!", description: "This is an embedded message."},
    tts: false},
 %sformat(S,'~q',[Dict]),
 %ddbg(post=S),
 discord_http(channels/ID/messages,[post(json(Dict))]).

% https://discord.com/oauth2/authorize?response_type=code&client_id=157730590492196864&scope=identify+guilds.join&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fgoogle.com&prompt=consent
get_messages:- get_messages(_,_).
get_messages(Channel,_Msg):- channel_to_id(Channel,ID), 
 %any_to_string(Msg,Str),
 discord_http(channels/ID/messages).


channel_to_id(_Channel,892806433970716692).
get_kv(K:V,K,V):- must(nonvar(K);throw(get_kv(K:V,K,V))).
get_kv(K-V,K,V).
get_kv(K=V,K,V).


% curl -H "Authorization: Bot $AUTH_TOKEN" -H "User-Agent: DiscordBot" https://discord.com/api/channels/892809238710222930/messages


add_discord_info(Type,Pairs):- is_list(Pairs),select(KV,Pairs,Rest),get_kv(KV,id,ID),!,
  add_discord_info3(ID,isa,Type), add_discord_info3(Type,hasValue,ID),
  add_discord_info3(ID,id,ID),
  add_discord_info(ID,Rest).
add_discord_info(Type,Pairs):- is_list(Pairs), Pairs\==[], !, maplist(add_discord_info(Type),Pairs).

add_discord_info(Type,KV):- get_kv(KV,K,V),!,add_discord_info3(Type,K,V).
add_discord_info(Type,Data):- is_dict(Data),dict_pairs(Data,_Tag,Pairs),!,add_discord_info(Type,Pairs).
add_discord_info(Type,Data):- %retractall(tmp:discord_info(Type,_,_)),
  add_discord_info3(Type,hasValue,Data).


from_string(S,V):- string(S),atom_number(S,V),!.
from_string(S,V):- atom(S),atom_number(S,V),!.
from_string(V,V).
  
add_discord_info3(ID,Prop,Data):-
  from_string(ID,ID2),
  from_string(Data,Data2),
  R= tmp:discord_info(ID2,Prop,Data2),
 asserta(R),
 ddbg(asserta(R)).

discord_name_id_type(Name,ID,Type):- 
  get_discord(ID,name,Name),
  get_discord(ID,isa,Type).

get_discord(ID,Data):- get_discord_info(ID,hasValue,Data).
get_discord(ID,Prop,Value):- get_discord_info(ID,Prop,Value).
get_discord(Type,Prop,Value):- get_discord_info(Type,hasValue,ID),get_discord_info(ID,Prop,Value).

get_discord_info(Type,ID,Data):- into_dtypes(Type,Type2),into_dtypes(ID,ID2),into_dtypes(Data,Data2),
  tmp:discord_info(Type2,ID2,Data2).

into_dtypes(Type,Type):- var(Type),!.
into_dtypes(Type,Type):- nonvar(Type).
into_dtypes(Type,Type2):- \+ string(Type), atom_string(Type,Type2).
into_dtypes(Type,Type2):- \+ atom(Type), atom_string(Type2,Type).


name_to_id(Name,ID):-text_to_string(Name,NameS),get_discord(ID,name,NameS),guild\==ID,!.
%name_to_id(Name,ID):-text_to_string(Name,NameS),get_discord(_,hasValue,ID), get_discord(ID,_,NameS),!.

same_ids(ID,IDS):-text_to_string(ID,IDA),text_to_string(IDS,IDB),IDA==IDB.

discord_ensure_im2(ID,IM):- get_discord(IM,user,ID),!.

discord_ensure_im(To,IM):- get_discord(IM, name, To), get_discord(IM, is_channel, true),!.
discord_ensure_im(To,IM):- name_to_id(To,ID),!, discord_ensure_im(ID,IM).
discord_ensure_im(To,IM):- discord_ensure_im2(To,IM),!.
% OLD discord_ensure_im(To,IM):- name_to_id(To,ID), discord_send({type:'im_open',user:ID}),!,must(discord_ensure_im2(To,IM)),!.
discord_ensure_im(To,IM):- discord_send({type:'conversations_open',users:To}),!,must(discord_ensure_im2(To,IM)),!.


discord_id_time(ID,TS):-flag(discord_id,OID,OID+1),ID is OID+1,get_time(Time),number_string(Time,TS).


discord_self(Self):-get_discord(self, id, Self).

%  {"id":2,"type":"ping","time":1484999912}
discord_ping :- discord_id_time(ID,_),get_time(Time),TimeRnd is round(Time),discord_send({"id":ID,"type":"ping", "time":TimeRnd}).

% {"id":3,"type":"message","channel":"D3U47CE4W","text":"hi there"}
discord_say :- discord_say('#prologmud_server',"test message to logicmoo").
discord_say2:- discord_say(dmiles,"test message to dmiles").
discord_say3:- discord_say(general,"test message to general channel").

find_discord_info(Str):-
 forall(tmp:discord_info(X,Y,Z),
 ignore((sformat(S,'~q.',[tmp:discord_info(X,Y,Z)]),sub_string(S, _Offset0, _Length, _After, Str),
   ddbg(S)))).

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

:- bot_discord_token(TokenHeader), add_discord_info(token,TokenHeader).
:- get_time(Time), ITime is integer(Time), add_discord_info(time,ITime).



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


dict_to_curly(Dict,{type:Type,Data}):- del_dict(type,Dict,Type,DictOut),dict_pairs(DictOut,_,Pairs),any_to_curls(Pairs,Data).
dict_to_curly(Dict,{type:Type,Data}):- dict_pairs(Dict,Type,Pairs),nonvar(Type),any_to_curls(Pairs,Data).
dict_to_curly(Dict,{Data}):- dict_pairs(Dict,_,Pairs),any_to_curls(Pairs,Data).

any_to_curls(Dict,Out):- is_dict(Dict),!,dict_to_curly(Dict,Data),any_to_curls(Data,Out).
any_to_curls(Var,"var"):- \+ must(\+ var(Var)),!.
any_to_curls('$'(Var),Val):- get_discord(Var,Val),!. 
any_to_curls({DataI},{Data}):-!,any_to_curls(DataI,Data).
any_to_curls((A,B),(AA,BB)):-!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls([A|B],(AA,BB)):-!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls([A],AA):-!,any_to_curls(A,AA).
any_to_curls(KV,AA:BB):-get_kv(KV,A,B),!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls(A,AA):- catch(text_to_string(A,AA),_,fail),!.
any_to_curls(A,A).

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

:- fixup_exports.

:- if( \+ prolog_load_context(reloading, true)).
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


