%%
%%  ERESYE, an ERlang Expert SYstem Engine
%%
%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of Francesca Gangemi, Corrado Santoro may be used
%%       to endorse or promote products derived from this software without
%%       specific prior written permission.
%%
%%
%% THIS SOFTWARE IS PROVIDED BY Francesca Gangemi AND Corrado Santoro ``AS
%% IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
%% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%% SUCH DAMAGE.
%%
-module(eresyet_simple_relatives).

-export([father/3, grandfather/3, grandmother/3,
         mother/3]).

-include_lib("eunit/include/eunit.hrl").

%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother(Engine, {female, X}, {parent, X, Y}) ->
    eresye:set_client_state(eresye:assert(Engine,
                                          {mother, X, Y}),
                            [{mother, X, Y} | eresye:get_client_state(Engine)]).

%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father(Engine, {male, X}, {parent, X, Y}) ->
    eresye:set_client_state(eresye:assert(Engine,
                                          {father, X, Y}),
                            [{father, X, Y} | eresye:get_client_state(Engine)]).

%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather(Engine, {father, X, Y}, {parent, Y, Z}) ->
    eresye:set_client_state(eresye:assert(Engine,
                                          {grandfather, X, Z}),
                            [{grandfather, X, Z}
                             | eresye:get_client_state(Engine)]).

%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother(Engine, {mother, X, Y}, {parent, Y, Z}) ->
    eresye:set_client_state(eresye:assert(Engine,
                                          {grandmother, X, Z}),
                            [{grandmother, X, Z}
                             | eresye:get_client_state(Engine)]).
rules_test() ->
    Engine0 = eresye:new_with_state([]),
    Engine2 =
        lists:foldl(fun (X, Engine1) ->
                            eresye:add_rule (Engine1, {?MODULE, X})
                    end,
                    Engine0,
                    [mother, father, grandfather, grandmother]),

    Engine3 = eresye:assert(Engine2, [{male, bob},
                                      {mail, joe},
                                      {male, corrado},
                                      {female, sara},
                                      {parent, bob, joe},
                                      {parent, sara, bob},
                                      {parent, corrado, bob}]),
    InternalState = eresye:get_client_state(Engine3),


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
