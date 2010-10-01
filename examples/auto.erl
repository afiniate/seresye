%
% auto.erl
%
% -------------------------------------------------------------------------
%
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
%
%% ======================================================
%%   Automotive Expert System
%%
%%     This expert system diagnoses some simple
%%     problems with a car.
%%
%%     It is a bare translation of the same example
%%     provided in CLIPS Version 6.0
%%
%%     To execute, type 'auto:start().'
%% ======================================================
%

-module (auto).

-export ([normal_engine_state_conclusions/2,
          unsatisfactory_engine_state_conclusions/2,
          determine_engine_state/2,
          determine_rotation_state/2,
          determine_sluggishness/2,
          determine_misfiring/2,
          determine_knocking/2,
          determine_low_output/2,
          determine_gas_level/3,
          determine_battery_state/2,
          determine_point_surface_state_1/3,
          determine_point_surface_state_2/2,
          determine_conductivity_test/4,
          no_repairs/2,
          print_repair/3,
          start/0]).


ask_yn (Prompt) ->
  [Response | _] = io:get_line (Prompt),
  case Response of
    $y -> true;
    _ -> false
  end.

ask_question (Prompt) ->
  io:get_line (Prompt).


%% **********************
%% * ENGINE STATE RULES *
%% **********************
normal_engine_state_conclusions (Engine, {'working-state', engine, normal}) ->
   eresye:assert (Engine, {repair, "No repair needed."}),
   eresye:assert (Engine, {'spark-state', engine, normal}),
   eresye:assert (Engine, {'charge-state', battery, charged}),
   eresye:assert (Engine, {'rotation-state', engine, rotates}).


unsatisfactory_engine_state_conclusions (Engine,
                                         {'working-state', engine,
                                          unsatisfactory}) ->
   eresye:assert (Engine, {'charge-state', battery, charged}),
   eresye:assert (Engine, {'rotation-state', engine, rotates}).



%% ***************
%% * QUERY RULES *
%% ***************
determine_engine_state (Engine, {start, _}) when not
["{'working-state', engine, _}", "{repair, _}"]; true ->
  case ask_yn ('Does the engine start (yes/no)? ') of
    true ->
      case ask_yn ('Does the engine run normally (yes/no)? ') of
        true -> eresye:assert (Engine, {'working-state', engine, normal});
        _ -> eresye:assert (Engine, {'working-state', engine, unsatisfactory})
      end;
    _ -> eresye:assert (Engine, {'working-state', engine, 'does-not-start'})
  end.


determine_rotation_state (Engine,
                          {'working-state', engine, 'does-not-start'})
when not ["{'rotation-state', engine, _}", "{repair, _}"]; true ->
  case ask_yn ('Does the engine rotate (yes/no)? ') of
    true -> eresye:assert (Engine, {'rotation-state', engine, rotates}),
            eresye:assert (Engine, {'spark-state', engine, 'irregular-spark'});
    _ -> eresye:assert (Engine, {'rotation-state', engine, 'does-not-rotate'}),
         eresye:assert (Engine, {'spark-state', engine, 'does-not-spark'})
  end.


determine_sluggishness (Engine, {'working-state', engine, unsatisfactory})
when not ["{repair, _}"]; true ->
  case ask_yn ('Is the engine sluggish (yes/no)? ') of
    true -> eresye:assert (Engine, {repair, "Clean the fuel line."});
    _ -> ok
  end.


determine_misfiring (Engine, {'working-state', engine, unsatisfactory})
  when not ["{repair, _}"]; true ->
  case ask_yn ('Does the engine misfire (yes/no)? ') of
     true -> eresye:assert (Engine, {repair, "Point gap adjustment."}),
             eresye:assert (Engine, {'spark-state', engine, 'irregular-spark'});
     _ -> ok
   end.


determine_knocking (E, {'working-state', engine, unsatisfactory})
  when not ["{repair, _}"]; true ->
  case ask_yn ('Does the engine knock (yes/no)? ') of
    true -> eresye:assert (E, {repair, "Timing adjustment."});
    _ -> ok
  end.


determine_low_output (E, {'working-state', engine, unsatisfactory})
  when not ["{symptom, engine, _}", "{repair, _}"]; true ->
  case ask_yn ('Is the output of the engine low (yes/no)? ') of
    true -> eresye:assert (E, {symptom, engine, 'low-output'});
    _ -> eresye:assert (E, {symptom, engine, 'not-low-output'})
  end.


determine_gas_level (E,
                     {'working-state', engine, 'does-not-start'},
                     {'rotation-state', engine, rotates})
  when not ["{repair, _}"]; true ->
  case ask_yn ('Does the tank have any gas in it (yes/no)? ') of
    false -> eresye:assert (E, {repair, "Add gas."});
    _ -> ok
  end.


determine_battery_state (E,
                         {'rotation-state', engine, 'does-not-rotate'})
when not ["{'charge-state', battery, _}", "{repair, _}"]; true ->
  case ask_yn ('Is the battery charged (yes/no)? ') of
    true -> eresye:assert (E, {'charge-state', battery, charged});
    _ -> eresye:assert (E, {repair, "Charge the battery."}),
         eresye:assert (E, {'charge-state', battery, dead})
  end.


determine_point_surface_state_1 (E,
                               {'working-state', engine, 'does-not-start'},
                               {'spark-state', engine, 'irregular-spark'})
  when not ["{repair, _}"]; true ->
  dpss (E).
determine_point_surface_state_2 (E,
                               {symptom, engine, 'low-output'})
  when not ["{repair, _}"]; true ->
  dpss (E).

dpss (E) ->
  case ask_question ('What is the surface state of the points (normal/burned/contaminated)? ') of
    [$b,$u,$r,$n,$e,$d | _] ->
      eresye:assert (E, {repair, "Replace the points."});
    [$c,$o,$n,$t,$a,$m,$i,$n,$a,$t,$e,$d | _] ->
      eresye:assert (E, {repair, "Clean the points."});
    _ -> ok
  end.


determine_conductivity_test (E,
                             {'working-state', engine, 'does-not-start'},
                             {'spark-state', engine, 'does-not-spark'},
                             {'charge-state', battery, charged})
  when not ["{repair, _}"]; true ->
  case ask_yn ('Is the conductivity test for the ignition coil positive (yes/no)? ') of
    true -> eresye:assert (E, {repair, "Repair the distributor lead wire."});
    _ -> eresye:assert (E, {repair, "Replace the ignition coil."})
  end.


no_repairs (E, {start, _}) when not ["{repair, _}"]; true ->
  eresye:assert (E, {repair, "Take your car to a mechanic."}).


print_repair (_, {repair, X}, {start, Pid}) ->
  io:format ("Suggested Repair: ~p~n", [X]),
  Pid ! ok.




start () ->
  eresye:start (auto),
  %% Rules with high priority (10)
  [eresye:add_rule (auto, {?MODULE, X}, 10) ||
    X <- [normal_engine_state_conclusions,
          unsatisfactory_engine_state_conclusions,
          print_repair]],
  %% Rules with normal priority (0)
  [eresye:add_rule (auto, {?MODULE, X}) ||
    X <- [determine_engine_state,
          determine_rotation_state,
          determine_sluggishness,
          determine_misfiring,
          determine_knocking,
          determine_low_output,
          determine_gas_level,
          determine_battery_state,
          determine_point_surface_state_1,
          determine_point_surface_state_2,
          determine_conductivity_test]],
  %% Rules with low priority (-10)
  eresye:add_rule (auto, {?MODULE, no_repairs}, -10),
  eresye:assert (auto, {start, self ()}),
  receive
    _ -> ok
  end,
  eresye:stop (auto).

