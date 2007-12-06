%
% sieve.erl
%
% -------------------------------------------------------------------------
%
%  ERESYE, an ERlang Expert SYstem Engine
%  Copyright (C) 2005-07 Francesca Gangemi (francesca@erlang-consulting.com)
%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%
-module (sieve).
-compile ([export_all]).

remove_multiple (Engine, {X}, {Y}) when ((X rem Y) == 0) and (X =/= Y)->
  eresye:retract (Engine, {X}).

final_rule (Engine, {is, started} = X)
    when not ["{is, finished}"];true ->
  eresye:retract (Engine, X),
  eresye:assert (Engine, {is, finished}).

start () ->
  eresye:start (sieve),
  eresye:assert (sieve,
                 [{X} || X <- lists:seq (2, 100)]),
  eresye:assert (sieve, {is, started}),
  eresye:add_rule (sieve, {sieve, remove_multiple}, 2),
  eresye:add_rule (sieve, {sieve, final_rule}, 1),
  Start = now(),
  eresye:wait_and_retract (sieve, {is, finished}),
  End = now(),
  io:format ("KB: ~p~n", [eresye:get_kb (sieve)]),
  io:format ("Rules fired: ~p~n", [{ok, R} = eresye:get_rules_fired (sieve)]),
  D = timer:now_diff(End, Start),
  io:format ("Time = ~p sec, ~p rules/sec, rule execution time ~p msec~n",
             [D / 1000000.0,
              R / (D / 1000000.0),
              (D / 1000.0) / R]),
  eresye:stop (sieve),
  ok.
