%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011, Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_relatives).

-export([father/3, grandfather/3, grandmother/3,
         mother/3, brother/4, sister/4]).

-include_lib("eunit/include/eunit.hrl").

-rules([mother, father, brother, sister, grandfather,
        grandmother]).

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


rules_test() ->
    Engine0 = seresye_engine:new([]),
    Engine2 =  seresye_engine:add_rules(Engine0, ?MODULE),

    Engine3 = seresye_engine:assert (Engine2,
                                     [{male, bob},
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
                                      {parent, sara, anna}]),

    InternalState = seresye_engine:get_client_state(Engine3),

    ?assertMatch(true,
                 lists:member({mother,sara,anna}, InternalState)),

    ?assertMatch(true,
                 lists:member({sister,anna,casear},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({mother,sara,casear},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({grandfather,corrado,anna},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({sister,anna,caesar},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,caesar,anna},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({father,bob,anna},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({grandmother,jane,anna},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({grandfather,corrado,caesar},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({father,bob,caesar},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({grandmother,jane,caesar},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({sister,alice,mark},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,bob,alice},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,mark,alice},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({father,corrado,alice},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({sister,alice,bob},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({sister,alice,mark},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,bob,alice},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,mark,alice},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({mother,jane,alice},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({sister,alice,bob},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,bob,mark},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({father,corrado,mark},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,mark,bob},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,bob,mark},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({mother,jane,mark},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,mark,bob},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({father,corrado,bob},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({mother,jane,bob},
                              InternalState)),

    ?assertMatch(29, erlang:length(InternalState)).



