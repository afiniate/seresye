%%%  SERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
%%% ======================================================
%%%   Automotive Expert System
%%%
%%%     This expert system diagnoses some simple
%%%     problems with a car.
%%%
%%%     It is a bare translation of the same example
%%%     provided in CLIPS Version 6.0
%%%
%%%     To execute, type 'auto:start().'
%%% ======================================================
-module(seresyee_auto).

-export([determine_battery_state/2,
         determine_conductivity_test/4, determine_engine_state/2,
         determine_gas_level/3, determine_knocking/2,
         determine_low_output/2, determine_misfiring/2,
         determine_point_surface_state_1/3,
         determine_point_surface_state_2/2,
         determine_rotation_state/2, determine_sluggishness/2,
         no_repairs/2, normal_engine_state_conclusions/2,
         print_repair/3, start/0,
         unsatisfactory_engine_state_conclusions/2]).

-neg_rule({determine_engine_state, [{'working-state', engine, '__IGNORE_UNDERSCORE__'},
                                    {repair, '__IGNORE_UNDERSCORE__'}]}).

-include_lib("seresye/include/seresye.hrl").

%% **********************
%% * ENGINE STATE RULES *
%% **********************
normal_engine_state_conclusions(Engine,
                                {'working-state', engine, normal}) ->
    seresye_engine:assert(Engine,
                         [{repair, "No repair needed."},
                          {'spark-state', engine, normal},
                          {'charge-state', battery, charged},
                          {'rotation-state', engine, rotates}]).

unsatisfactory_engine_state_conclusions(Engine,
                                        {'working-state', engine,
                                         unsatisfactory}) ->
    seresye_engine:assert(Engine,
                         [{'charge-state', battery, charged},
                          {'rotation-state', engine, rotates}]).

%% ***************
%% * QUERY RULES *
%% ***************
determine_engine_state(Engine, {start, _})
  when not {rule, [{'working-state', engine, _}, {repair, _}]} ->
    case ask_yn('Does the engine start (yes/no)? ') of
        true ->
            case ask_yn('Does the engine run normally (yes/no)? ')
            of
                true ->
                    seresye_engine:assert(Engine,
                                         {'working-state', engine, normal});
                _ ->
                    seresye_engine:assert(Engine,
                                         {'working-state', engine, unsatisfactory})
            end;
        _ ->
            seresye_engine:assert(Engine,
                                 {'working-state', engine, 'does-not-start'})
    end.

determine_rotation_state(Engine,
                         {'working-state', engine, 'does-not-start'})
  when not
       {rule, [{'rotation-state', engine, _}, {repair, _}]} ->
    case ask_yn('Does the engine rotate (yes/no)? ') of
        true ->
            seresye_engine:assert(Engine,
                                 [{'rotation-state', engine, rotates},
                                  {'spark-state', engine, 'irregular-spark'}]);
        _ ->
            seresye_engine:assert(Engine,
                                 [{'rotation-state', engine, 'does-not-rotate'},
                                  {'spark-state', engine, 'does-not-spark'}])
    end.

determine_sluggishness(Engine,
                       {'working-state', engine, unsatisfactory})
  when not {rule, [{repair, _}]} ->
    case ask_yn('Is the engine sluggish (yes/no)? ') of
        true ->
            seresye_engine:assert(Engine, {repair, "Clean the fuel line."});
        _ -> Engine
    end.

determine_misfiring(Engine,
                    {'working-state', engine, unsatisfactory})
  when not {rule, [{repair, _}]} ->
    case ask_yn('Does the engine misfire (yes/no)? ') of
        true ->
            seresye_engine:assert(Engine,
                                 [{repair, "Point gap adjustment."},
                                  {'spark-state', engine, 'irregular-spark'}]);
        _ -> Engine
    end.

determine_knocking(E,
                   {'working-state', engine, unsatisfactory})
  when not {rule, [{repair, _}]} ->
    case ask_yn('Does the engine knock (yes/no)? ') of
        true ->
            seresye_engine:assert(E, {repair, "Timing adjustment."});
        _ -> E
    end.

determine_low_output(E,
                     {'working-state', engine, unsatisfactory})
  when not {rule, [{symptom, engine, _}, {repair, _}]} ->
    case
        ask_yn('Is the output of the engine low (yes/no)? ')
    of
        true ->
            seresye_engine:assert(E, {symptom, engine, 'low-output'});
        _ ->
            seresye_engine:assert(E, {symptom, engine, 'not-low-output'})
    end.

determine_gas_level(E,
                    {'working-state', engine, 'does-not-start'},
                    {'rotation-state', engine, rotates})
  when not {rule, [{repair, _}]} ->
    case
        ask_yn('Does the tank have any gas in it (yes/no)? ')
    of
        false -> seresye_engine:assert(E, {repair, "Add gas."});
        _ -> E
    end.

determine_battery_state(E,
                        {'rotation-state', engine, 'does-not-rotate'})
  when not
       {rule, [{'charge-state', battery, _}, {repair, _}]} ->
    case ask_yn('Is the battery charged (yes/no)? ') of
        true ->
            seresye_engine:assert(E, {'charge-state', battery, charged});
        _ ->
            seresye_engine:assert(E,
                                 [{repair, "Charge the battery."},
                                  {'charge-state', battery, dead}])
    end.

determine_point_surface_state_1(E,
                                {'working-state', engine, 'does-not-start'},
                                {'spark-state', engine, 'irregular-spark'})
  when not {rule, [{repair, _}]} ->
    dpss(E).

determine_point_surface_state_2(E,
                                {symptom, engine, 'low-output'})
  when not {rule, [{repair, _}]} ->
    dpss(E).

dpss(E) ->
    case
        ask_question('What is the surface state of the points (normal/burned/contaminated)? ')
    of
        [$b, $u, $r, $n, $e, $d | _] ->
            seresye_engine:assert(E, {repair, "Replace the points."});
        [$c, $o, $n, $t, $a, $m, $i, $n, $a, $t, $e, $d | _] ->
            seresye_engine:assert(E, {repair, "Clean the points."});
        _ -> E
    end.

determine_conductivity_test(E,
                            {'working-state', engine, 'does-not-start'},
                            {'spark-state', engine, 'does-not-spark'},
                            {'charge-state', battery, charged})
  when not {rule, [{repair, _}]}; true ->
    case
        ask_yn('Is the conductivity test for the ignition coil positive (yes/no)? ')
    of
        true ->
            seresye_engine:assert(E,
                                 {repair, "Repair the distributor lead wire."});
        _ ->
            seresye_engine:assert(E, {repair, "Replace the ignition coil."})
    end.

no_repairs(E, {start, _})
  when not {rule, [{repair, _}]}, true ->
    seresye_engine:assert(E,
                         {repair, "Take your car to a mechanic."}).

print_repair(E, {repair, X}, {start, _}) ->
    io:format("Suggested Repair: ~p~n", [X]), E.

ask_yn(Prompt) ->
    [Response | _] = io:get_line(Prompt),
    case Response of
        $y -> true;
        _ -> false
    end.

ask_question(Prompt) -> io:get_line(Prompt).

start() ->
    Engine0 = seresye_engine:new(),
    %% Rules with high priority (10)
    Engine2 = lists:foldl(fun (Rule, Engine1) ->
                                  seresye_engine:add_rule(Engine1, {?MODULE, Rule}, 10)
                          end,
                          Engine0,
                          [normal_engine_state_conclusions,
                           unsatisfactory_engine_state_conclusions,
                           print_repair]),
    %% Rules with normal priority (0)
    Engine3 = lists:foldl(fun (Rule, Engine1) ->
                                  seresye_engine:add_rule(Engine1, {?MODULE, Rule})
                          end,
                          Engine2,
                          [determine_engine_state, determine_rotation_state,
                           determine_sluggishness, determine_misfiring,
                           determine_knocking, determine_low_output,
                           determine_gas_level, determine_battery_state,
                           determine_point_surface_state_1,
                           determine_point_surface_state_2,
                           determine_conductivity_test]),
    %% Rules with low priority (-10)
    Engine4 = seresye_engine:add_rule(Engine3,
                                     {?MODULE, no_repairs}, -10),
    seresye_engine:assert(Engine4, {start, ok}).
