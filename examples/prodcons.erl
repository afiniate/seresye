%
% prodcons.erl
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
-module (prodcons).

-compile (export_all).

start () ->
  eresye:start (pc),
  spawn (prodcons, cons_1, [1]),
  spawn (prodcons, cons_1, [2]),
  spawn (prodcons, cons_1, [3]),
  spawn (prodcons, prod, [0]),
  ok.

prod (20) -> ok;
prod (Index) ->
  eresye:assert (pc, {item, Index}),
  prod (Index + 1).

cons (20) -> ok;%eres:stop (pc);
cons (Index) ->
  Fact = eresye:wait_and_retract (pc, {item, fun (X) -> X == Index end}),
  %%eres:retract (pc, Fact),
  io:format ("Consumer ~p~n", [Fact]),
  cons (Index + 1).

cons_1 (N) ->
  Fact = eresye:wait_and_retract (pc, {item, '_'}),
  %%eres:retract (pc, Fact),
  io:format ("~w: Consumer ~p~n", [N, Fact]),
  timer:sleep (random:uniform(500)),
  cons_1 (N).
