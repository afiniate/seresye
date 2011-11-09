%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_sample).
-export ([rule/4, rule1/3, rule2/4, rule3/3, start/0]).

-record (sample_record, { a = nil, b}).

-include_lib("eunit/include/eunit.hrl").


rule (Engine, {hello, world}, {ciao, X}, {X, 10}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule | seresye_engine:get_client_state(Engine)]).

rule1 (Engine0, {hello, world}, {ciao, _X} = F) ->
    Engine1 = seresye_engine:assert (Engine0, {test}),
    Engine2 = seresye_engine:retract (Engine1, F),
    seresye_engine:set_client_state(Engine2,
                                    [rule1a | seresye_engine:get_client_state(Engine2)]);
rule1 (Engine, {hello, world}, {test}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule1b | seresye_engine:get_client_state(Engine)]).


rule2 (Engine, {hello, world}, #sample_record { a = Z }, {mondo, Z}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule2 | seresye_engine:get_client_state(Engine)]).

rule3 (Engine, {hello, [_H|T]}, {test, T}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule3 | seresye_engine:get_client_state(Engine)]).

start () ->

    Engine0 = seresye_engine:add_rule (seresye_engine:new([]), {?MODULE, rule2}),
    Engine1 = seresye_engine:assert (Engine0, [[{ciao, mondo}, {mondo, 20}],
                                               {hello, world},
                                               {ok, world},
                                               #sample_record { a = 10, b = 50}]),
    Engine2 = seresye_engine:add_rule (Engine1, {?MODULE, rule3}),
    Engine3 = seresye_engine:assert (Engine2, [{hello, [ciao, mondo]},
                                               {test, ciao},
                                               {test, [ciao]},
                                               {test, [mondo]},
                                               {hello, [ciao, mondo, world]},
                                               {test, [mondo, world]}]),

    Engine4 = seresye_engine:add_rules (Engine3, [{?MODULE, rule},
                                                  {?MODULE, rule1}]),

    Engine5 = seresye_engine:retract (Engine4, {test}),
    State = seresye_engine:get_client_state(seresye_engine:assert (Engine5, {test})),
    ?assertMatch([rule1b,rule1b,rule1a,rule3,rule3],
                 State).
