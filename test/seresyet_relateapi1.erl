%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresyet_relateapi1).

-export([father/3, grandfather/3, grandmother/3,
         mother/3, sister/4, brother/4]).

-include_lib("eunit/include/eunit.hrl").

%-define(BREAK, break).     % <- Uncomment to break siblings test, assertMatch breaks.

-ifdef(BREAK).
-rule(brother).
-rule(sister).
-endif.

-rules([mother, father, grandfather, grandmother]).

-define(DATA_WITHOUT_SIBLINGS, [
            {male, bob},
            {male, corrado},
            {male, mark},
            {male, caesar},
            {female, alice},
            {female, sara},
            {female, jane},
            {female, anna},
            {parent, jane, bob},
            {parent, corrado, bob},
            {parent, jane, mark},
            {parent, corrado, mark},
            {parent, jane, alice},
            {parent, corrado, alice},
            {parent, bob, caesar},
            {parent, bob, anna},
            {parent, sara, casear},
            {parent, sara, anna}
           ]).

-define(DATA_WITHOUT_PARENTS, [
            {male, bob},
            {male, corrado},
            {male, mark},
            {male, caesar},
            {female, alice},
            {female, sara},
            {female, jane},
            {female, anna}
            ,{parent, jane, bob}     % <- Uncomment to break, when BREAK is defined
           ]).

%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother (Engine, {female, X}, {parent, X, Y}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {mother, X, Y}),
                                    [{mother, X, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father (Engine, {male, X}, {parent, X, Y}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {father, X, Y}),
                                    [{father, X, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (Y and Z have the same parent X) and (Z is female)
%%    then (Z is Y's sister)
%%
sister (Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {sister, Z, Y}),
                                    [{sister, Z, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (Y and Z have the same parent X) and (Z is male)
%%    then (Z is Y's brother)
%%
brother (Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {brother, Z, Y}),
                                    [{brother, Z, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {grandfather, X, Z}),
                                    [{grandfather, X, Z}
                                     | seresye_engine:get_client_state(Engine)]).

%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother (Engine, {mother, X, Y}, {parent, Y, Z}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {grandmother, X, Z}),
                                    [{grandmother, X, Z}
                                     | seresye_engine:get_client_state(Engine)]).



no_siblings_test() ->

    {ok, Pid} = seresye:start(?MODULE),

    seresye:add_rules(Pid, ?MODULE),

    seresye:assert(Pid, ?DATA_WITHOUT_SIBLINGS),

    {ok, InternalState} = seresye:get_client_state(Pid),

    (catch gen_server:call(Pid, stop)),

    io:format("State: ~p~n", [InternalState]),

    ?assertMatch(true,
                 lists:member({mother,sara,anna}, 
                              InternalState)),
    ?assertMatch(true,
                 lists:member({mother,sara,casear},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({grandfather,corrado,anna},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({grandmother,jane,anna},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({father,bob,anna},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({grandfather,corrado,caesar},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({grandmother,jane,anna},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({father,bob,caesar},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({father,corrado,alice},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({mother,jane,alice},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({father,corrado,mark},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({mother,jane,mark},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({father,corrado,bob},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({mother,jane,bob},
                              InternalState)).
    

no_parents_test() ->

    {ok, Pid} = seresye:start(?MODULE),

    seresye:add_rules(Pid, ?MODULE),

    seresye:assert(Pid, ?DATA_WITHOUT_PARENTS),

    case seresye:get_client_state(Pid) of
        {ok, []} ->
            (catch gen_server:call(Pid, stop)),
            [];
        {ok, InternalState} -> 
            ?assertMatch(true, lists:member({mother,jane,bob}, InternalState)),
            (catch gen_server:call(Pid, stop)),
            InternalState
    end.
