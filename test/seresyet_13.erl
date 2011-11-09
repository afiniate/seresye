%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php-module(eresyet_13).
-module(seresyet_13).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([a, seresye, engine, that, is, initialized, with, data], _State, _) ->
    Engine0 = seresye_engine:new([]),
    {ok,
     seresye_engine:assert(Engine0,
                          [{male, bob}, {mail, joe}, {male, corrado},
                           {female, sara}, {parent, bob, joe}, {parent, sara, bob},
                           {parent, corrado, bob}])};
given([rules,from,a,module,with,rules,defined,via,attributes], Engine, _) ->
  {ok, seresye_engine:add_rules(Engine, seresyet_simple_relatives)}.

'when'(['when',seresye,propagation,is,complete], Engine, _) ->
    %% Propagation happens immediately, so nothing to do here. Its
    %% just a placeholder
    {ok, Engine}.

then([contains,the,data,populated,by,the,rules],
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
    {ok, State};
then([the, engine, runs, normally], Engine, _) ->
     {ok, {Engine, seresye_engine:get_client_state(Engine)}}.

