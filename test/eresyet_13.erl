%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2011
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php-module(eresyet_13).
-module(eresyet_13).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([an, eresye, engine, that, is, initialized, with, data], _State, _) ->
    Engine0 = eresye_engine:new_with_state([]),
    {ok,
     eresye_engine:assert(Engine0,
                          [{male, bob}, {mail, joe}, {male, corrado},
                           {female, sara}, {parent, bob, joe}, {parent, sara, bob},
                           {parent, corrado, bob}])};
given([rules,from,a,module,with,rules,defined,via,attributes], Engine, _) ->
  {ok, eresye_engine:add_rules(Engine, eresyet_simple_relatives)}.

'when'(['when',eresye,propagation,is,complete], Engine, _) ->
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
     {ok, {Engine, eresye_engine:get_client_state(Engine)}}.

