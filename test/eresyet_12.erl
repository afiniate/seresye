%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresyet_12).

-export([given/3, then/3, 'when'/3]).

-include_lib("eunit/include/eunit.hrl").

given([initialized, with, data], Engine, _) ->
    {ok,
     eresye_engine:assert(Engine,
                          [{male, bob}, {mail, joe}, {male, corrado},
                           {female, sara}, {parent, bob, joe}, {parent, sara, bob},
                           {parent, corrado, bob}])};
given([an, eresye, engine, that, is, initialized, with,
       state],
      _, _) ->
    Engine0 = eresye_engine:new_with_state([]),
    Engine1 = lists:foldl(fun (X, Engine1) ->
                                  eresye_engine:add_rule(Engine1,
                                                         {eresyet_simple_relatives, X})
                          end,
                          Engine0, [mother, father, grandfather, grandmother]),
    {ok, Engine1}.

'when'(['when', eresye, propagation, is, complete],
       Engine, _) ->
    %% Propagation happens immediately, so nothing to do here. Its
    %% just a placeholder
    {ok, Engine}.

then([then, the, per, engine, state, is, retrievable],
     Engine, _) ->
    {ok, {Engine, eresye_engine:get_client_state(Engine)}};
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
