/* Part of sparkle
	Copyright 2014-2015 Samer Abdallah (UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(sparql_dcg,[
      select//3
   ,  construct//3
   ,  describe//1
   ,  describe//2
   ,  ask//1
	]).

/** <module> A simple DCG for generating a subset of SPARQL

   ==
   sparql_goal ---> (sparql_goal, sparql_goal) % conjunction
                  ; (sparql_goal; sparql_goal) % disjunction
                  ; rdf(resource,resource,object)
                  ; filter(cond)
                  .

   resource :< object. % any resource is an object

   literal(+literal)   :: object.  % any ground literal is an object
   atomic              :< literal. % any atomic can be a plain literal
   lang(atom,atom)     :: literal. % literal with language
   type(resource,atom) :: literal. % typed literal

   object   :< expr. % any object is an expr
   number   :< expr. % Prolog numerical values can also be expressions

   condition ---> (cond , cond)
                ; (cond ; cond)
                ; \+ cond
                ; expr == expr
                ; expr \= expr
                ; expr >= expr
                ; expr =< expr
                ; expr < expr
                ; expr > expr
                ; between(expr,expr,expr)
                ; in(object,list(object))
                ; regex(pattern,value)
                ; bound(object)
                ; blank(resource)
                ; uri(object)
                ; literal(object)
                .

   expr ---> expr + expr
           ; expr - expr
           ; expr * expr
           ; expr / expr
           ; +expr
           ; -expr
           ; str(expr)
           ; lang(expr)
           ; datatype(expr)
           .

   var ---> '$VAR'(integer)
          ; '@'        % anonymous blank node
          ; '@'(atom)  % nonymous blank node 
          .

   var :< resource
   var :< literal

   ==   

   Samer Abdallah, Dept. of Computer Science, UCL (2014)
 */

%:- use_module(library(semweb/rdf_db), [rdf_global_object/2, rdf_global_id/2]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- set_prolog_flag(double_quotes, codes).

%% select(+Vars:list(expr), +Goal:sparql_goal, +Options:list(option))// is det.
%
%  Any variables in the query must be represented by '$VAR'/1 terms
%  as generated by numbervars/3.
%
select(_Vars,Goal,Options) -->
        {
         (   Goal=aggregate_group(_,_,_,_)
         ;   Goal=aggregate_group(_,_,_,_,_)
         ;   Goal=aggregate(_,_,_))
        },
        !,
        % TODO
        if_option(distinct(Distinct), if(Distinct=true, " "),Options,O1),
        goal(Goal),
        if_option(order_by(OB), (" ORDER BY ", expr(OB)), O1,O2),
        if_option(limit(Limit), (" LIMIT ", at(Limit)), O2,O3),
        if_option(offset(Offs), (" OFFSET ", at(Offs)), O3,O4),
        {check_remaining_options(O4)}.
        
select(Vars,Goal,Options) -->
   "SELECT ", 
   if_option(distinct(Distinct), if(Distinct=true, " DISTINCT "),Options,O1),
   seqmap_with_sep(" ",expr,Vars), " ",
   where(Goal),
   if_option(order_by(OB), (" ORDER BY ", expr(OB)), O1,O2),
   if_option(limit(Limit), (" LIMIT ", at(Limit)), O2,O3),
   if_option(offset(Offs), (" OFFSET ", at(Offs)), O3,O4),
   {check_remaining_options(O4)}.

construct(Head,Goal,Options) -->
   {O1=Options},
   "CONSTRUCT ", brace(goal(Head)), " ",
   where(Goal),
   if_option(order_by(OB), (" ORDER BY ", expr(OB)), O1,O2),
   if_option(limit(Limit), (" LIMIT ", at(Limit)), O2,O3),
   if_option(offset(Offs), (" OFFSET ", at(Offs)), O3,O4),
   {check_remaining_options(O4)}.

check_remaining_options([]) :- !.
check_remaining_options(Opts) :- throw(unrecognised_options(Opts)).

if_option(Opt,Phrase,O1,O2) -->
   ( {select_option(Opt,O1,O2)} -> call_dcg(Phrase); {O2=O1}).

%% ask(+Goal:sparql_goal)// is det.
%
%  Format an ASK query.
ask(Goal) --> "ASK ", brace(goal(Goal)).


%% describe(+Resource:resource)// is det.
%% describe(+Resource:resource,+Goal:sparql_goal)// is det.
describe(R) --> "DESCRIBE ", resource(R).
describe(RS,Goal) --> 
   "DESCRIBE ", 
   seqmap_with_sep(" ",resource,RS),
   where(Goal).

%% where(+Goal:sparql_goal)// is det.
where(Goal) --> "WHERE ", brace(goal(Goal)).

%% goal(+Goal)// is det.

% simplify/normalize
goal((G,true)) --> !, goal(G).
goal((true,G)) --> !, goal(G).


goal(G1;G2)   --> brace(goal(G1)), " UNION ", brace(goal(G2)).
goal(\+G)     --> "FILTER NOT EXISTS ", brace(goal(G)). %NB consider MINUS { ... } also
goal((G1,G2)) --> goal(G1), " . ", goal(G2).
goal(conj(GS)) --> seqmap_with_sep(" , ",goal,GS).
goal(optional(G))     --> "OPTIONAL ", brace(goal(G)).

goal(service(S,G)) --> "SERVICE ",resource(S)," ",brace(goal(G)).

goal(rdf_path(S,P,O,G)) --> goal(rdf(S,P,O,G)).
goal(rdf_path(S,P,O)) --> goal(rdf(S,P,O)).


goal(rdf(S,P,O)) -->
   { rdf_global_object(O,OO) },
   resource(S), " ",
   property(P), " ",
   object(OO).


goal(rdf(S,P,O,G)) --> "GRAPH ", resource(G), " ", brace(goal(rdf(S,P,O))).

% this does not work on many triplestores
% https://stackoverflow.com/questions/32274562/what-is-the-sparql-query-to-get-the-name-of-all-graphs-existing-in-my-triplestor
goal(rdf_graph(G)) --> "GRAPH ", resource(G), " {}".

goal(rdf_graph_goals(G,Goals)) --> "GRAPH ", resource(G), " ", brace(goal(Goals)).

goal(rdf_predicate(P)) --> "SELECT DISTINCT ", expr(P), " ", where(rdf(_,P,_)).

goal(aggregate(Expr,G,Result)) --> "SELECT ", expr(Expr), " AS ", variable(Result), " ", where(G).
goal(aggregate_group(Expr, GroupVars, G, Result)) -->
        "SELECT (",
        expr(Expr), " AS ", variable(Result),") ",
        seqmap_with_sep(" ",expr,GroupVars),
        " ",
        where(G),
        " GROUP BY ", seqmap_with_sep(" ",expr,GroupVars).
goal(aggregate_group(Expr, GroupVars, G, Having, Result)) -->
        "SELECT (",
        expr(Expr), " AS ", variable(Result),") ",
        seqmap_with_sep(" ",head_expr,GroupVars),
        " ",
        where(G),
        " GROUP BY ", seqmap_with_sep(" ",head_expr_val,GroupVars),
        " HAVING ", cond(Having).



goal(is(V,Expr)) --> goal(bind(Expr,V)).
goal(bind(Expr,V)) --> "BIND( ", expr(Expr), " AS ", variable(V), " )".
goal(filter(Cond)) --> "FILTER ", cond(Cond).

% support for rdf_where/1 in semweb/rdf11    
goal({Cond}) --> "FILTER ", cond(Cond).
goal(rdf_where(Cond)) --> "FILTER ", cond(Cond).

goal(member(Var,Vals)) -->
        {ground(Vals)},
        !,
        goal(values(Var,Vals)).
goal(values(Var,Vals)) -->
        {\+ is_list(Var)}, !,
        "VALUES ",expr(Var)," {",
        seqmap_with_sep(" ",expr,Vals), " }".
goal(values(Vars,Tuples)) --> "VALUES (",
        seqmap_with_sep(" ",expr,Vars), ") {",
        seqmap_with_sep(" ",tuple,Tuples), " }".


goal(true) --> "true" .

% TODO: put this in its own section
goal(str_before(Str, Sep, Sub)) --> goal(bind(str_before(Str, Sep), Sub)).

% allow conditions not wrapped by rdf_where/1
goal(G) --> goal(filter(G)).

% this is necessary for assigning variables in GROUP BY querie
head_expr(is(V,Expr)) --> !, "BIND( ", expr(Expr), " AS ", variable(V), " )".
head_expr(X) --> expr(X).

head_expr_val(is(_V,Expr)) --> !, expr(Expr).
head_expr_val(X) --> expr(X).


tuple(Vals) --> "(", seqmap_with_sep(" ",expr,Vals), ")".


:- op(1150,fx,p).
p(X) --> paren(X).

cond(\+C)   --> p  "! ", cond(C).
cond((X,Y)) --> p cond(X), " && ", cond(Y).
cond((X;Y)) --> p cond(X), " || ", cond(Y).
cond(X=Y)  --> p expr(X), " = ", expr(Y).
cond(X==Y)  --> p expr(X), " = ", expr(Y).
cond(X\=Y)  --> p expr(X), " != ", expr(Y).
cond(X=<Y)  --> p expr(X), " <= ", expr(Y).
cond(X>=Y)  --> p expr(X), " >= ", expr(Y).
cond(X>Y)   --> p expr(X), " > ", expr(Y).
cond(X<Y)   --> p expr(X), " < ", expr(Y).
cond(X@<Y)   --> p expr(str(X)), " < ", expr(str(Y)).
cond(X@=<Y)   --> p expr(str(X)), " <= ", expr(str(Y)).
cond(X@>Y)   --> p expr(str(X)), " > ", expr(str(Y)).
cond(X@>=Y)   --> p expr(str(X)), " >= ", expr(str(Y)).
cond(between(L,U,X)) --> cond((L=<X,X=<U)).

% 17.4.1.9 IN
cond(in(X,Ys))     --> p expr(X), " in ", (p seqmap_with_sep(", ",expr,Ys)).
cond(contains(X,Y))   --> p "contains(", string_literal_expr(X), ",", string_literal_expr(Y), ")".
cond(str_starts(X,Y))   --> p "strStarts(", string_literal_expr(X), ",", string_literal_expr(Y), ")".
cond(str_ends(X,Y))   --> p "strEnds(", string_literal_expr(X), ",", string_literal_expr(Y), ")".
cond(regex(S,P))   --> p "regex(", expr(S), ",", quote(at(P)), ")".
cond(regex(S,P,F)) --> p "regex(", expr(S), ",", quote(at(P)),  ",", quote(at(F)), ")".
cond(regex_str(S,P))   --> "regex(", expr(str(S)), ",", quote(at(P)), ")".
cond(regex_str(S,P,F)) --> "regex(", expr(str(S)), ",", quote(at(P)),  ",", quote(at(F)), ")".
cond(bound(V))     --> "bound(", object(V), ")".

% 17.4.2.1 isIRI
% defined in rdf11
cond(rdf_is_iri(V))       --> "isIRI(", object(V), ")".
cond(is_uri(V))       --> "isURI(", object(V), ")".

% 17.4.2.2 isBlank
% defined in rdf11
cond(rdf_bnode(V))     --> "isBLANK(", object(V), ")".
cond(is_blank(V))     --> "isBLANK(", object(V), ")".
cond(rdf_is_bnode(V))     --> "isBLANK(", object(V), ")".

% 17.4.2.3 isLiteral
% defined in rdf11
cond(rdf_is_literal(V))   --> "isLITERAL(", object(V), ")".
cond(is_literal(V))   --> "isLITERAL(", object(V), ")".

cond(lang(V))     --> "lang(", object(V), ")".
cond(G)            --> {throw(error(cond(G)))}.


string_literal_expr(A) --> {atomic(A),atom_string(A,S)},expr(S).
string_literal_expr(S) --> expr(S).

% 17.4.1.2 IF
expr(if(Expr,Yes,No)) --> "IF(", cond(Expr), ", ", expr(Yes), ", ", expr(No), ")".



% [121] builtin call
expr(str(V))       --> "STR(", object(V), ")".
expr(lang(V))      --> "LANG(", object(V), ")".
expr(langmatches(V,W))      --> "LANGMATCHES(", object(V), ",", object( W), ")".
expr(datatype(V))      --> "DATATYPE(", object(V), ")".
expr(bound(V))      --> "BOUND(", object(V), ")".
expr(uri(V))       --> "URI(", expr(V), ")".
expr(iri(V))       --> "IRI(", expr(V), ")".
expr(rand)       --> "RAND()" .
expr(abs(V))       --> "ABS(", expr(V), ")".
expr(ceil(V))       --> "CEIL(", expr(V), ")".
expr(floor(V))       --> "FLOOR(", expr(V), ")".
expr(round(V))       --> "ROUND(", expr(V), ")".

expr(str_before(Str,Sep)) --> "strBefore(", string_literal_expr(Str), ", ", string_literal_expr(Sep), ")".
expr(str_after(Str,Sep)) --> "strAfter(", string_literal_expr(Str), ", ", string_literal_expr(Sep), ")".
expr(concat(A,B)) --> "concat(", string_literal_expr(A), ", ", string_literal_expr(B), ")".
expr(strlen(V))       --> "STRLEN(", expr(V), ")".
expr(substr(V,W))     --> "SUBSTR(", expr(V), ", ", expr(W), ")". % [123]
expr(substr(V,W,X))   --> "SUBSTR(", expr(V), ", ", expr(W), ", ", expr(X), ")". % [123]
expr(replace(S,P,R)) --> "replace(", string_literal_expr(S), ", ", string_literal_expr(P), ", ", string_literal_expr(R), ")". % [124]
expr(replace(S,P,R,Z)) --> "replace(", string_literal_expr(S), ", ", string_literal_expr(P), ", ", string_literal_expr(R),
        ", ", string_literal_expr(Z), ")".                    % [124]

expr(ucase(A)) --> "ucase(", string_literal_expr(A), ")".
expr(lcase(A)) --> "lcase(", string_literal_expr(A), ")".

% TODO more of 121

expr(S)            --> {string(S)},"\"", at(S), "\"".
expr('^^'(S,T))    --> "\"", at(S), "\"^^", resource(T).
expr('@'(S,Lang))    --> "\"", at(S), "\"@", at(Lang).
expr(distinct(X))     --> "DISTINCT ", expr(X), " ".
expr(datatype(V))  --> "DATATYPE(", object(V), ")".
expr(quote(V))     --> quote(at(V)).

% [127] Aggregate
expr(max(X))   --> "max(", expr(X), ")".
expr(min(X))   --> "min(", expr(X), ")".
expr(sum(X))   --> "sum(", expr(X), ")".
expr(count(X))     --> "COUNT(", expr(X), ")".
expr(sample(X))     --> "SAMPLE(", expr(X), ")".
expr(group_concat(X))     --> "GROUP_CONCAT(", expr(X), ")".
expr(group_concat(X, S))     --> "GROUP_CONCAT(", expr(X), " ;  SEPARATOR = ", expr(S), ")".

expr(+X) -->  p "+ ", expr(X), ")".
expr(-X) -->  p "- ", expr(X), ")".
expr(X+Y) --> p expr(X), " + ", expr(Y).
expr(X-Y) --> p expr(X), " - ", expr(Y).
expr(X*Y) --> p expr(X), " * ", expr(Y).
expr(X/Y) --> p expr(X), " / ", expr(Y).
expr(X) --> {number(X)}, at(X).
expr(X) --> object(X).

% https://www.w3.org/TR/sparql11-query/#pp-language
property(oneOrMore(R)) --> property(R),"+".
property(zeroOrMore(R)) --> property(R),"*".
property(zeroOrOne(R)) --> property(R),"?".
property(inverse(R)) --> "^", property(R).
property(\+R) --> "!(", property(R), ")".
property(\R) --> "^(", property(R), ")".
property(R1/R2) --> "(",property(R1),"/",property(R2),")".
property(R1|R2) --> "(",property(R1),"|",property(R2),")".

property(R) --> resource(R).

resource(R) --> variable(R).
resource(R) --> {rdf_global_id(R,RR)}, uri(RR).

object(literal(Lit)) --> literal(Lit).
object('^^'(Val,Type)) --> quote(wr(Val)), "^^", resource(Type).
object('@'(Val,Lang)) --> quote(at(Val)), "@", at(Lang).
object(S) --> {string(S)}, quote(at(S)).
object(Resource) --> resource(Resource).

% old-style literals
literal(lang(Lang,Val)) --> quote(at(Val)), "@", at(Lang).
literal(type(Type,Val)) --> quote(wr(Val)), "^^", resource(Type).
literal(Lit) --> {atomic(Lit)}, quote(at(Lit)).

uri(U) --> {atom(U)}, "<", at(U), ">".
quote(P) --> "\"", escape_with(0'\\,0'",P), "\"".
variable(v(V))  --> "?", at(V).
variable(V)  --> {var_number(V,N)}, "?v", at(N).
variable('@'(V)) --> "_:", {atomic(V) -> N=V; var_number(V,N)}, at(N).
variable(@)  --> "[]".

