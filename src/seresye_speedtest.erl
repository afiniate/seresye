%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresye_speedtest).
-export([remove_multiple/3, final_rule/2, run_sieve/0]).


remove_multiple(Engine, {X}, {Y}) when ((X rem Y) == 0) and (X =/= Y)->
    seresye_engine:retract (Engine, {X}).

final_rule(Engine0, {is, started} = X)
  when not ["{is, finished}"];true ->
    Engine1 = seresye_engine:retract (Engine0, X),
    seresye_engine:assert (Engine1, {is, finished}).

run_sieve() ->
    Start = now(),
    Engine1 = seresye_engine:assert (seresye_engine:new(),
                             [{X} || X <- lists:seq (2, 200)]),
    Engine2 = seresye_engine:add_rule (Engine1, {?MODULE, remove_multiple}, 2),
    Engine3 = seresye_engine:add_rule (Engine2, {?MODULE, final_rule}, 1),
    Engine4 = seresye_engine:assert (Engine3, {is, started}),
    End = now(),
    io:format("Kb ~p~n", [seresye_engine:get_kb (Engine4)]),
    R = seresye_engine:get_rules_fired (Engine4),
    io:format ("Rules fired: ~p~n", [R]),
    D = timer:now_diff(End, Start),
    io:format ("Time = ~p sec, ~p rules/sec, rule execution time ~p msec~n",
               [D / 1000000.0,
                R / (D / 1000000.0),
                (D / 1000.0) / R]).
