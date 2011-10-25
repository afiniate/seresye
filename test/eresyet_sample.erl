%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (eresyet_sample).
-export ([rule/4, rule1/3, rule2/4, rule3/3, start/0]).

-record (sample_record, { a = nil, b}).

-include_lib("eunit/include/eunit.hrl").


rule (Engine, {hello, world}, {ciao, X}, {X, 10}) ->
    eresye_engine:set_client_state(Engine,
                                   [rule | eresye_engine:get_client_state(Engine)]).

rule1 (Engine0, {hello, world}, {ciao, _X} = F) ->
    Engine1 = eresye_engine:assert (Engine0, {test}),
    Engine2 = eresye_engine:retract (Engine1, F),
    eresye_engine:set_client_state(Engine2,
                                   [rule1a | eresye_engine:get_client_state(Engine2)]);
rule1 (Engine, {hello, world}, {test}) ->
    eresye_engine:set_client_state(Engine,
                                   [rule1b | eresye_engine:get_client_state(Engine)]).


rule2 (Engine, {hello, world}, #sample_record { a = Z }, {mondo, Z}) ->
    eresye_engine:set_client_state(Engine,
                                   [rule2 | eresye_engine:get_client_state(Engine)]).

rule3 (Engine, {hello, [_H|T]}, {test, T}) ->
    eresye_engine:set_client_state(Engine,
                                   [rule3 | eresye_engine:get_client_state(Engine)]).

start () ->

    Engine0 = eresye_engine:add_rule (eresye_engine:new([]), {?MODULE, rule2}),
    Engine1 = eresye_engine:assert (Engine0, [[{ciao, mondo}, {mondo, 20}],
                                              {hello, world},
                                              {ok, world},
                                              #sample_record { a = 10, b = 50}]),
    Engine2 = eresye_engine:add_rule (Engine1, {?MODULE, rule3}),
    Engine3 = eresye_engine:assert (Engine2, [{hello, [ciao, mondo]},
                                              {test, ciao},
                                              {test, [ciao]},
                                              {test, [mondo]},
                                              {hello, [ciao, mondo, world]},
                                              {test, [mondo, world]}]),

    Engine4 = eresye_engine:add_rules (Engine3, [{?MODULE, rule},
                                                 {?MODULE, rule1}]),

    Engine5 = eresye_engine:retract (Engine4, {test}),
    State = eresye_engine:get_client_state(eresye_engine:assert (Engine5, {test})),
    ?assertMatch([rule1b,rule1b,rule1a,rule3,rule3],
                 State).



