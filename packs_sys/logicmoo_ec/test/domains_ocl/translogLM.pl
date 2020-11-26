/**
 *  All rights reserved. Use of this software is permitted for non-commercial
 *  research purposes, and it may be copied only for that use.  All copies must
 *  include this copyright message.  This software is made available AS IS, and
 *  neither the GIPO team nor the University of Huddersfield make any warranty
 *  about the software or its performance.
 *
 *  Automatically generated OCl Domain from  GIPO Version 1.0
 *
 *  Author: Ron  Simpson, Donghong Liu, Lee McCLuskey
 *  Institution: University of Huddersfield
 *  Date created: August 1999
 *  Date last modified: 2003/05/17 at 04:09:31 PM BST
 *  Description:
 *    Derived from Univ. of Maryland's literal-based specification
 *    Reworking of domain to comply with GIPO and OCL(h)
 *    This model captures the object structure and actions in a "transport
 *    logistics" domain where packages have to be transported around
 *    different locations in different cities, using trucks, trains and planes.
 */

:- style_check(-singleton).
:- op(100,xfy,'=>').

domain_name(translog).

option(hierarchical).

% Sorts
sorts(primitive_sorts,[traincar,train,truck,aircraft,package,train_station,clocation,post_office,airport,city,road_route,rail_route,region]).
sorts(physical_obj,[vehicle,package]).
sorts(vehicle,[railv,truck,aircraft]).
sorts(railv,[traincar,train]).
sorts(location,[city_location,airport,city]).
sorts(city_location,[tcentre,not_tcentre]).
sorts(tcentre,[train_station]).
sorts(not_tcentre,[clocation,post_office]).
sorts(route,[road_route,rail_route]).

% Objects
objects(traincar,[traincar1_x,traincar1_z,traincar1_y,traincar1]).
objects(train,[train1_x,train2_x,train1_z,train2_z,train1_y,train2_y,train1,train2]).
objects(truck,[truck_1_x,truck_2_x,truck_3_x,truck_11_x,truck_22_x,truck_33_x,truck_1_z,truck_2_z,truck_3_z,truck_11_z,truck_22_z,truck_33_z,truck_1_y,truck_2_y,truck_3_y,truck_11_y,truck_22_y,truck_33_y,truck_1,truck_2,truck_3,truck_11,truck_22,truck_33]).
objects(aircraft,[ac1,ac2,ac3,ac4,ac5,ac6,ac7,ac8,ac9,ac10,ac11,ac12,ac13,ac14,ac15,ac16]).
objects(package,[pk_1,pk_2,pk_3,pk_4,pk_5,pk_6,pk_1_x,pk_2_x,pk_3_x,pk_4_x,pk_5_x,pk_6_x,pk_1_z,pk_2_z,pk_3_z,pk_4_z,pk_5_z,pk_6_z,pk_1_y,pk_2_y,pk_3_y,pk_4_y,pk_5_y,pk_6_y]).
objects(train_station,[city1_ts1_x,city2_ts1_x,city3_ts1_x,city1_ts1_y,city2_ts1_y,city3_ts1_y,city1_ts1_z,city2_ts1_z,city3_ts1_z,city1_ts1,city2_ts1,city3_ts1,city1_ts2,city1_ts2_x,city1_ts2_y,city1_ts2_z]).
objects(clocation,[city1_cl1_x,city1_cl2_x,city2_cl1_x,city3_cl1_x,city1_cl1_y,city1_cl2_y,city2_cl1_y,city3_cl1_y,city1_cl1_z,city1_cl2_z,city2_cl1_z,city3_cl1_z,city1_cl1,city1_cl2,city2_cl1,city3_cl1]).
objects(post_office,[post_1]).
objects(airport,[ap1,ap2,ap3,ap4]).
objects(city,[city1_x,city2_x,city3_x,city1_y,city2_y,city3_y,city1_z,city2_z,city3_z,city1,city2,city3]).
objects(road_route,[road_route_21_x,road_route_32_x,road_route_31_x,road_route_21_z,road_route_32_z,road_route_31_z,road_route_21_y,road_route_32_y,road_route_31_y,road_route_21,road_route_32,road_route_31]).
objects(rail_route,[rail_route_2_x,rail_route_3_x,rail_route_2_z,rail_route_3_z,rail_route_2_y,rail_route_3_y,rail_route_2,rail_route_3,rail_route_1,rail_route_1_x,rail_route_1_y,rail_route_1_z]).
objects(region,[east,west,south,north]).

% Predicates
predicates([
    at(physical_obj,location),
    moveable(vehicle),
    available(vehicle),
    busy(vehicle),
    attached(vehicle,vehicle),
    unattached(vehicle),
    waiting(package),
    certified(package),
    uncertified(package),
    loaded(package,vehicle),
    delivered(package),
    ap_serves(airport,city),
    connects(route,location,location),
    in_city(location,city),
    in_region(location,region),
    serves_region(airport,region),
    route_available(route),
    serves(train_station,city)]).

% Object Class Definitions
substate_classes(physical_obj,P,[
    [at(P,L)]]).
substate_classes(railv,V,[
    [unattached(V)],
    [attached(V,V1)]]).
substate_classes(vehicle,T,[
    [moveable(T),available(T)],
    [moveable(T),busy(T)]]).
substate_classes(package,P,[
    [uncertified(P)],
    [waiting(P),certified(P)],
    [loaded(P,V),certified(P)],
    [delivered(P)]]).

% Atomic Invariants
atomic_invariants([
    serves_region(ap1,east),
    in_city(ap1,city1),
    in_region(ap1,east),
    in_city(city1_cl1,city1),
    in_city(city1_ts1,city1),
    in_city(city1_cl2,city1),
    in_city(city1_ts2,city1),
    in_city(city2_cl1,city2),
    in_city(city2_ts1,city2),
    in_city(city3_cl1,city3),
    in_city(city3_ts1,city3),
    serves(city1_ts1,city1),
    serves(city1_ts2,city1),
    serves(city2_ts1,city2),
    serves(city3_ts1,city3),
    route_available(road_route_31),
    connects(road_route_31,city3,city1),
    connects(road_route_31,city1,city3),
    route_available(road_route_32),
    connects(road_route_32,city3,city2),
    connects(road_route_32,city2,city3),
    route_available(rail_route_1),
    connects(rail_route_1,city1_ts2,city1_ts1),
    connects(rail_route_1,city1_ts1,city1_ts2),
    route_available(rail_route_2),
    connects(rail_route_2,city2_ts1,city1_ts1),
    connects(rail_route_2,city1_ts1,city2_ts1),
    connects(road_route_21,city2,city1),
    route_available(road_route_21),
    connects(road_route_21,city1,city2),
    in_region(city1_ts1,east),
    in_region(city1,east),
    in_region(city2_ts1,east),
    in_region(city2,east),
    in_region(city3_ts1,east),
    in_region(city3,east),
    in_region(city1_ts2,east),
    in_region(city1_cl1,east),
    in_region(city1_cl2,east),
    in_region(city2_cl1,east),
    in_region(city3_cl1,east),
    serves_region(ap2,west),
    in_city(ap2,city1_x),
    in_region(ap2,west),
    in_region(city1_ts1_x,west),
    in_region(city1_x,west),
    in_region(city2_ts1_x,west),
    in_region(city2_x,west),
    in_region(city3_ts1_x,west),
    in_region(city3_x,west),
    in_region(city1_ts2_x,west),
    in_region(city1_cl1_x,west),
    in_region(city1_cl2_x,west),
    in_region(city2_cl1_x,west),
    in_region(city3_cl1_x,west),
    in_city(city1_cl1_x,city1_x),
    in_city(city1_ts1_x,city1_x),
    in_city(city1_cl2_x,city1_x),
    in_city(city1_ts2_x,city1_x),
    in_city(city2_cl1_x,city2_x),
    in_city(city2_ts1_x,city2_x),
    in_city(city3_cl1_x,city3_x),
    in_city(city3_ts1_x,city3_x),
    serves(city1_ts1_x,city1_x),
    serves(city1_ts2_x,city1_x),
    serves(city2_ts1_x,city2_x),
    serves(city3_ts1_x,city3_x),
    route_available(road_route_31_x),
    connects(road_route_31_x,city3_x,city1_x),
    connects(road_route_31_x,city1_x,city3_x),
    route_available(road_route_32_x),
    connects(road_route_32_x,city3_x,city2_x),
    connects(road_route_32_x,city2_x,city3_x),
    route_available(rail_route_1_x),
    connects(rail_route_1_x,city1_ts2_x,city1_ts1_x),
    connects(rail_route_1_x,city1_ts1_x,city1_ts2_x),
    route_available(rail_route_2_x),
    connects(rail_route_2_x,city2_ts1_x,city1_ts1_x),
    connects(rail_route_2_x,city1_ts1_x,city2_ts1_x),
    connects(road_route_21_x,city1_x,city2_x),
    connects(road_route_21_x,city2_x,city1_x),
    route_available(road_route_21_x),
    serves_region(ap3,south),
    in_city(ap3,city1_y),
    in_region(ap3,south),
    in_region(city1_ts1_y,south),
    in_region(city1_y,south),
    in_region(city2_ts1_y,south),
    in_region(city2_y,south),
    in_region(city3_ts1_y,south),
    in_region(city3_y,south),
    in_region(city1_ts2_y,south),
    in_region(city1_cl1_y,south),
    in_region(city1_cl2_y,south),
    in_region(city2_cl1_y,south),
    in_region(city3_cl1_y,south),
    in_city(city1_cl1_y,city1_y),
    in_city(city1_ts1_y,city1_y),
    in_city(city1_cl2_y,city1_y),
    in_city(city1_ts2_y,city1_y),
    in_city(city2_cl1_y,city2_y),
    in_city(city2_ts1_y,city2_y),
    in_city(city3_cl1_y,city3_y),
    in_city(city3_ts1_y,city3_y),
    serves(city1_ts1_y,city1_y),
    serves(city1_ts2_y,city1_y),
    serves(city2_ts1_y,city2_y),
    serves(city3_ts1_y,city3_y),
    route_available(road_route_31_y),
    connects(road_route_31_y,city3_y,city1_y),
    connects(road_route_31_y,city1_y,city3_y),
    route_available(road_route_32_y),
    connects(road_route_32_y,city3_y,city2_y),
    connects(road_route_32_y,city2_y,city3_y),
    route_available(rail_route_1_y),
    connects(rail_route_1_y,city1_ts2_y,city1_ts1_y),
    connects(rail_route_1_y,city1_ts1_y,city1_ts2_y),
    route_available(rail_route_2_y),
    connects(rail_route_2_y,city2_ts1_y,city1_ts1_y),
    connects(rail_route_2_y,city1_ts1_y,city2_ts1_y),
    connects(road_route_21_y,city1_y,city2_y),
    connects(road_route_21_y,city2_y,city1_y),
    route_available(road_route_21_y),
    serves_region(ap4,north),
    in_city(ap4,city1_z),
    in_region(ap4,north),
    in_region(city1_ts1_z,north),
    in_region(city1_z,north),
    in_region(city2_ts1_z,north),
    in_region(city2_z,north),
    in_region(city3_ts1_z,north),
    in_region(city3_z,north),
    in_region(city1_ts2_z,north),
    in_region(city1_cl1_z,north),
    in_region(city1_cl2_z,north),
    in_region(city2_cl1_z,north),
    in_region(city3_cl1_z,north),
    in_city(city1_cl1_z,city1_z),
    in_city(city1_ts1_z,city1_z),
    in_city(city1_cl2_z,city1_z),
    in_city(city1_ts2_z,city1_z),
    in_city(city2_cl1_z,city2_z),
    in_city(city2_ts1_z,city2_z),
    in_city(city3_cl1_z,city3_z),
    in_city(city3_ts1_z,city3_z),
    serves(city1_ts1_z,city1_z),
    serves(city1_ts2_z,city1_z),
    serves(city2_ts1_z,city2_z),
    serves(city3_ts1_z,city3_z),
    route_available(road_route_31_z),
    connects(road_route_31_z,city3_z,city1_z),
    connects(road_route_31_z,city1_z,city3_z),
    route_available(road_route_32_z),
    connects(road_route_32_z,city3_z,city2_z),
    connects(road_route_32_z,city2_z,city3_z),
    route_available(rail_route_1_z),
    connects(rail_route_1_z,city1_ts2_z,city1_ts1_z),
    connects(rail_route_1_z,city1_ts1_z,city1_ts2_z),
    route_available(rail_route_2_z),
    connects(rail_route_2_z,city2_ts1_z,city1_ts1_z),
    connects(rail_route_2_z,city1_ts1_z,city2_ts1_z),
    connects(road_route_21_z,city1_z,city2_z),
    connects(road_route_21_z,city2_z,city1_z),
    route_available(road_route_21_z)]).

% Implied Invariants
implied_invariant([loaded(P,V)],[at(V,L),at(P,L)]).

% Inconsistent Constraints
inconsistent_constraint([certified(P),not_insured(P)]).

% Operators
operator(pay_fees(P),
    % prevail
    [],
    % necessary
    [     sc(package,P,[uncertified(P)]=>[waiting(P),certified(P)])],
    % conditional
    []).
operator(fly(A,D1,D2),
    % prevail
    [],
    % necessary
    [     sc(aircraft,A,[at(A,D1),is_of_sort(D2,airport),is_of_sort(D1,airport),ne(D1,D2)]=>[at(A,D2)])],
    % conditional
    [     sc(package,X,[loaded(X,A),certified(X),at(X,D1)]=>[loaded(X,A),certified(X),at(X,D2)])]).
operator(move(V,O,City,L,City1,R),
    % prevail
    [],
    % necessary
    [     sc(truck,V,[at(V,O),is_of_sort(R,road_route),moveable(V),in_city(O,City),in_city(L,City1),ne(City,City1),connects(R,City,City1)]=>[at(V,L)])],
    % conditional
    [     sc(package,X,[loaded(X,V),certified(X),at(X,O)]=>[loaded(X,V),certified(X),at(X,L)])]).
operator(move_in_city(V,O,City,L),
    % prevail
    [],
    % necessary
    [     sc(truck,V,[at(V,O),moveable(V),in_city(O,City),in_city(L,City)]=>[at(V,L)])],
    % conditional
    [     sc(package,X,[loaded(X,V),certified(X),at(X,O)]=>[loaded(X,V),certified(X),at(X,L)])]).
operator(pull_traincar(Train,O,V1,Rt,L),
    % prevail
    [],
    % necessary
    [     sc(train,Train,[at(Train,O),attached(Train,V1),moveable(Train),connects(Rt,O,L),is_of_sort(Rt,rail_route)]=>[at(Train,L),attached(Train,V1)]),
     sc(traincar,V1,[at(V1,O),attached(V1,Train)]=>[at(V1,L),attached(V1,Train)])],
    % conditional
    [     sc(package,P,[loaded(P,V1),certified(P),at(P,O)]=>[loaded(P,V1),certified(P),at(P,L)])]).
operator(move_train(V,O,Rt,L),
    % prevail
    [],
    % necessary
    [     sc(train,V,[at(V,O),unattached(V),moveable(V),available(V),connects(Rt,O,L),is_of_sort(Rt,rail_route)]=>[at(V,L),unattached(V),moveable(V),available(V)])],
    % conditional
    []).
operator(attach_traincar(Train,O,V),
    % prevail
    [],
    % necessary
    [     sc(train,Train,[at(Train,O),moveable(Train),available(Train),unattached(Train)]=>[at(Train,O),attached(Train,V),moveable(Train),busy(Train)]),
     sc(traincar,V,[at(V,O),unattached(V)]=>[at(V,O),attached(V,Train)])],
    % conditional
    []).
operator(detach_traincar(Train,V),
    % prevail
    [],
    % necessary
    [     sc(train,Train,[attached(Train,V),moveable(Train),busy(Train)]=>[unattached(Train),moveable(Train),available(Train)]),
     sc(traincar,V,[attached(V,Train)]=>[unattached(V)])],
    % conditional
    []).
operator(commission(V),
    % prevail
    [],
    % necessary
    [     sc(vehicle,V,[moveable(V),available(V)]=>[moveable(V),busy(V)])],
    % conditional
    []).
operator(load_package(V,L,P),
    % prevail
    [     se(vehicle,V,[at(V,L)])],
    % necessary
    [     sc(package,P,[at(P,L),waiting(P),certified(P)]=>[at(P,L),loaded(P,V),certified(P)])],
    % conditional
    []).
operator(unload_package(P,L,V),
    % prevail
    [],
    % necessary
    [     sc(package,P,[at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]),
     sc(vehicle,V,[at(V,L),moveable(V),busy(V)]=>[at(V,L),moveable(V),available(V)])],
    % conditional
    []).
operator(deliver(P,L),
    % prevail
    [],
    % necessary
    [     sc(package,P,[at(P,L),waiting(P),certified(P)]=>[at(P,L),delivered(P)])],
    % conditional
    []).

% Methods
/****
 * 
 */
method(move_traincar(V,O,L),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(traincar,V,[at(V,O)]=>[at(V,L)])],
    % Static
    [
     is_of_sort(V,traincar),
     connects(R2,O,L),
     is_of_sort(R2,rail_route),
     is_of_sort(Train,train)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     achieve(ss(train,Train,[at(Train,O)])),
     attach_traincar(Train,O,V),
     pull_traincar(Train,O,V,R2,L),
     detach_traincar(Train,V)]
).
/****
 * Transport package between different regions
 */
method(transport(P,O,D),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(package,P,[at(P,O),is_of_sort(P,package)]=>[at(P,D),delivered(P)])],
    % Static
    [
     ne(O,D),
     ne(R1,R2),
     in_region(O,R1),
     in_region(D,R2),
     serves_region(A1,R1),
     serves_region(A2,R2)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4),
     before(4,5)],
    % Decomposition
    [
     achieve(ss(package,P,[waiting(P),certified(P)])),
     carry_direct(P,O,A1),
     carry_via_ap(P,A1,A2),
     carry_direct(P,A2,D),
     deliver(P,D)]
).
/****
 * 
 */
method(carry_via_ap(P,O,D),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(package,P,[at(P,O),waiting(P),certified(P)]=>[at(P,D),waiting(P),certified(P)])],
    % Static
    [
     ne(O,D),
     is_of_sort(O,airport),
     is_of_sort(D,airport),
     is_of_sort(P,package),
     is_of_sort(V,aircraft)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     achieve(ss(aircraft,V,[at(V,O)])),
     load_package(V,O,P),
     fly(V,O,D),
     unload_package(P,D,V)]
).
/****
 * Carry within same city
 */
method(carry_direct(P,O,D),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(package,P,[at(P,O),waiting(P),certified(P)]=>[at(P,D),waiting(P),certified(P)])],
    % Static
    [
     is_of_sort(P,package),
     is_of_sort(V,truck),
     in_city(O,CY),
     in_city(D,CY)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4),
     before(4,5)],
    % Decomposition
    [
     commission(V),
     achieve(ss(truck,V,[at(V,O)])),
     load_package(V,O,P),
     move_in_city(V,O,CY,D),
     unload_package(P,D,V)]
).
/****
 * Carry by train in to different city
 */
method(carry_direct(P,O,D),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(package,P,[at(P,O),waiting(P),certified(P)]=>[at(P,D),waiting(P),certified(P)])],
    % Static
    [
     is_of_sort(P,package),
     is_of_sort(V,traincar),
     is_of_sort(Train,train),
     connects(R,O,D),
     route_available(R),
     is_of_sort(R,rail_route)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4),
     before(4,6),
     before(6,5)],
    % Decomposition
    [
     commission(V),
     achieve(ss(train,Train,[at(Train,O),attached(Train,V)])),
     load_package(V,O,P),
     pull_traincar(Train,O,V,R,D),
     unload_package(P,D,V),
     detach_traincar(Train,V)]
).
/****
 * Carry between cities by truck
 */
method(carry_direct(P,O,D),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(package,P,[at(P,O),waiting(P),certified(P)]=>[at(P,D),waiting(P),certified(P)])],
    % Static
    [
     is_of_sort(P,package),
     is_of_sort(V,truck),
     in_city(O,CY),
     in_city(D,CY1),
     ne(CY,CY1),
     connects(R,CY,CY1),
     is_of_sort(R,road_route),
     route_available(R)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,5),
     before(5,4)],
    % Decomposition
    [
     commission(V),
     achieve(ss(truck,V,[at(V,O)])),
     load_package(V,O,P),
     unload_package(P,D,V),
     move(V,O,CY,D,CY1,R)]
).
/****
 * Move a train car to desired location
 */
method(move_traincar(V,O,L),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(traincar,V,[at(V,O)]=>[at(V,L)])],
    % Static
    [
     is_of_sort(V,traincar),
     connects(R2,O,L),
     is_of_sort(R2,rail_route),
     is_of_sort(Train,train)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     achieve(ss(train,Train,[at(Train,O)])),
     attach_traincar(Train,O,V),
     pull_traincar(Train,O,V,R2,L),
     detach_traincar(Train,V)]
).
/****
 * 
 */
method(transport(Package,Org,Dest),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(package,Package,[uncertified(Package),at(Package,Org)]=>[delivered(Package),at(Package,Dest)])],
    % Static
    [
     in_region(Org,Region),
     in_region(Dest,Region)],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3)],
    % Decomposition
    [
     achieve(ss(package,Package,[waiting(Package),certified(Package),at(Package,Org)])),
     carry_direct(Package,Org,Dest),
     deliver(Package,Dest)]
).

% Domain Tasks

% HTN Domain Tasks
htn_task(1,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(2,
    goal(
          [
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)]))],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(3,
    goal(
          [
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_1,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
            before(1,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(4,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(5,
    goal(
          [
            transport(pk_1,city3_cl1,city1_ts1),
            transport(pk_2,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
            before(1,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(6,
    goal(
          [
            transport(pk_1,city3_cl1,city2_ts1),
            transport(pk_2,city3_cl1,city2_cl1)],
    % Temporal Constraints
          [
            before(1,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(7,
    goal(
          [
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_6,city1_ts1,city3_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(8,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(9,
    goal(
          [
            transport(pk_2,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city2_cl1),
            transport(pk_1,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(10,
    goal(
          [
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_1,city3_cl1,city2_cl1),
            achieve(ss(package,pk_5,[at(pk_5,city1_cl1),delivered(pk_5)]))],
    % Temporal Constraints
          [
            before(1,3)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(11,
    goal(
          [
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_1,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            achieve(ss(package,pk_5,[at(pk_5,city1_cl2),delivered(pk_5)]))],
    % Temporal Constraints
          [
            before(1,3),
            before(2,3)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(12,
    goal(
          [
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_6,city1_ts1,city3_cl1),
            transport(pk_1,city3_cl1,city2_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(13,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1)],
    % Temporal Constraints
          [
            before(1,3),
            before(2,3)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(14,
    goal(
          [
            transport(pk_1,city3_cl1,city2_cl1),
            transport(pk_2,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
            before(1,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(15,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_2,city3_cl1,city2_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(16,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(17,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_5,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(18,
    goal(
          [
            achieve(ss(package,pk_1,[waiting(pk_1),certified(pk_1)])),
            carry_direct(pk_1,city3_cl1,city1_cl2)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(19,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_5,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
            before(1,2),
            before(3,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(20,
    goal(
          [
            transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_ts1),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_6,city1_ts1,city3_ts1),
            transport(pk_5,city3_cl1,city1_cl1)],
    % Temporal Constraints
          [
            before(1,2),
            before(3,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
htn_task(21,
    goal(
          [
            transport(pk_5,city3_cl1,city3_ts1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(package,pk_1,[at(pk_1,city3_cl1),uncertified(pk_1)]),
     ss(package,pk_2,[at(pk_2,city3_cl1),uncertified(pk_2)]),
     ss(package,pk_3,[at(pk_3,city3_cl1),uncertified(pk_3)]),
     ss(package,pk_4,[at(pk_4,city1_cl1),uncertified(pk_4)]),
     ss(package,pk_5,[at(pk_5,city3_cl1),uncertified(pk_5)]),
     ss(package,pk_6,[at(pk_6,city1_ts1),uncertified(pk_6)]),
     ss(truck,truck_1,[at(truck_1,city1_cl1),moveable(truck_1),available(truck_1)]),
     ss(truck,truck_11,[at(truck_11,city1_cl1),moveable(truck_11),available(truck_11)]),
     ss(truck,truck_2,[at(truck_2,city2_cl1),moveable(truck_2),available(truck_2)]),
     ss(truck,truck_22,[at(truck_22,city2_cl1),moveable(truck_22),available(truck_22)]),
     ss(truck,truck_3,[at(truck_3,city3_cl1),moveable(truck_3),available(truck_3)]),
     ss(truck,truck_33,[at(truck_33,city3_cl1),moveable(truck_33),available(truck_33)]),
     ss(traincar,traincar1,[at(traincar1,city2_ts1),unattached(traincar1),moveable(traincar1),available(traincar1)]),
     ss(train,train2,[at(train2,city2_ts1),unattached(train2),moveable(train2),available(train2)]),
     ss(train,train1,[at(train1,city1_ts1),unattached(train1),moveable(train1),available(train1)])]).
