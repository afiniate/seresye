%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresyet_simple_relatives).

-export([father/3, grandfather/3, grandmother/3,
         mother/3]).

-include_lib("eunit/include/eunit.hrl").

-rule(mother).
-rule(father).
-rules([{grandfather, 10},
        grandmother]).
%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother(Engine, {female, X}, {parent, X, Y}) ->
    eresye_engine:set_client_state(eresye_engine:assert(Engine,
                                                        {mother, X, Y}),
                                   [{mother, X, Y} | eresye_engine:get_client_state(Engine)]).

%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father(Engine, {male, X}, {parent, X, Y}) ->
    eresye_engine:set_client_state(eresye_engine:assert(Engine,
                                                        {father, X, Y}),
                                   [{father, X, Y} | eresye_engine:get_client_state(Engine)]).

%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather(Engine, {father, X, Y}, {parent, Y, Z}) ->
    eresye_engine:set_client_state(eresye_engine:assert(Engine,
                                                        {grandfather, X, Z}),
                                   [{grandfather, X, Z}
                                    | eresye_engine:get_client_state(Engine)]).

%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother(Engine, {mother, X, Y}, {parent, Y, Z}) ->
    eresye_engine:set_client_state(eresye_engine:assert(Engine,
                                                        {grandmother, X, Z}),
                                   [{grandmother, X, Z}
                                    | eresye_engine:get_client_state(Engine)]).

rules_test() ->
    Engine0 = eresye_engine:new([]),
    Engine2 =
        lists:foldl(fun (X, Engine1) ->
                            eresye_engine:add_rule (Engine1, {?MODULE, X})
                    end,
                    Engine0,
                    [mother, father, grandfather, grandmother]),

    Engine3 = eresye_engine:assert(Engine2, [{male, bob},
                                             {mail, joe},
                                             {male, corrado},
                                             {female, sara},
                                             {parent, bob, joe},
                                             {parent, sara, bob},
                                             {parent, corrado, bob}]),
    InternalState = eresye_engine:get_client_state(Engine3),


    ?assertMatch(true,
                 lists:member({grandmother, sara, joe}, InternalState)),
    ?assertMatch(true,
                 lists:member({grandfather, corrado, joe}, InternalState)),
    ?assertMatch(true,
                 lists:member({mother, sara, bob}, InternalState)),
    ?assertMatch(true,
                 lists:member({father, corrado, bob}, InternalState)),
    ?assertMatch(true,
                 lists:member({father, bob, joe}, InternalState)),
    ?assertMatch(5, erlang:length(InternalState)).
