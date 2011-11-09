%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_sieve).
-export([remove_multiple/3, final_rule/2, run_sieve/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("seresye/include/seresye.hrl").

-rules([{remove_multiple, 2},
        {final_rule, 1}]).

remove_multiple (Engine, {X}, {Y}) when ((X rem Y) == 0) and (X =/= Y)->
    seresye_engine:retract (Engine, {X}).

final_rule (Engine0, {is, started} = X)
  when not {rule, [{is, finished}]} ->
    Engine1 = seresye_engine:retract (Engine0, X),
    seresye_engine:assert (Engine1, {is, finished}).

run_sieve() ->
    Start = now(),
    Engine1 = seresye_engine:assert (seresye_engine:new(),
                             [{X} || X <- lists:seq (2, 100)]),
    Engine2 = seresye_engine:add_rules(Engine1, ?MODULE),
    Engine3 = seresye_engine:assert (Engine2, {is, started}),
    End = now(),
    ?assertMatch([{is,finished},
                  {97},
                  {89},
                  {83},
                  {79},
                  {73},
                  {71},
                  {67},
                  {61},
                  {59},
                  {53},
                  {47},
                  {43},
                  {41},
                  {37},
                  {31},
                  {29},
                  {23},
                  {19},
                  {17},
                  {13},
                  {11},
                  {7},
                  {5},
                  {3},
                  {2}], seresye_engine:get_kb (Engine3)),
    R = seresye_engine:get_rules_fired (Engine3),
    io:format ("Rules fired: ~p~n", [R]),
    D = timer:now_diff(End, Start),
    io:format ("Time = ~p sec, ~p rules/sec, rule execution time ~p msec~n",
               [D / 1000000.0,
                R / (D / 1000000.0),
                (D / 1000.0) / R]).

rules_test_() ->
         {timeout, 160,
          fun() ->
                  run_sieve()
          end}.
