%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2011, Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresyet_12).

-export([given/3, then/3, 'when'/3]).

-include_lib("eunit/include/eunit.hrl").

given([initialized, with, data], Engine, _) ->
    {ok,
     seresye_engine:assert(Engine,
                           [{male, bob}, {mail, joe}, {male, corrado},
                            {female, sara}, {parent, bob, joe}, {parent, sara, bob},
                            {parent, corrado, bob}])};
given([a, seresye, engine, that, is, initialized, with,
       state],
      _, _) ->
    Engine0 = seresye_engine:new([]),
    Engine1 = lists:foldl(fun (X, Engine1) ->
                                  seresye_engine:add_rule(Engine1,
                                                          {seresyet_simple_relatives, X})
                          end,
                          Engine0, [mother, father, grandfather, grandmother]),
    {ok, Engine1}.

'when'([seresye, propagation, is, complete],
       Engine, _) ->
    %% Propagation happens immediately, so nothing to do here. Its
    %% just a placeholder
    {ok, Engine}.

then([the, per, engine, state, is, retrievable],
     Engine, _) ->
    {ok, {Engine, seresye_engine:get_client_state(Engine)}};
then([contains, the, data, populated, by, the, rules],
     State = {_, InternalState}, _) ->
    ?assertMatch(true,
                 (lists:member({grandmother, sara, joe},
                               InternalState))),
    ?assertMatch(true,
                 (lists:member({grandfather, corrado, joe},
                               InternalState))),
    ?assertMatch(true,
                 (lists:member({mother, sara, bob}, InternalState))),
    ?assertMatch(true,
                 (lists:member({mother, sara, bob}, InternalState))),
    ?assertMatch(true,
                 (lists:member({father, corrado, bob}, InternalState))),
    ?assertMatch(true,
                 (lists:member({father, bob, joe}, InternalState))),
    {ok, State}.
