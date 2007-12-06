%
% sample.erl
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
-module (sample).
-export ([rule/4, rule1/3, rule2/4, start/0, start1/0, start2/0]).

-include("sample.hrl").

rule (Engine, {hello, world}, {ciao, X}, {X, 10}) ->
  io:format ("Hit!\n"),
  ok.

rule1 (Engine, {hello, world}, {ciao, X} = F) ->
  io:format ("Hit 1!\n"),
  eresye:assert (Engine, {test}),
  eresye:retract (Engine, F),
  ok;

rule1 (Engine, {hello, world}, {test}) ->
  io:format ("Hit 2!\n"),
  ok.

rule2 (Engine, {hello, world}, #sample_record { a = Z } = X, {mondo, Z}) ->
   io:format ("Hit 3! ~p\n", [X]),
   ok.

start () ->
  eresye:start (myengine),
%%   Z = fun (Engine, Fact) ->
%%           io:format ("FUN!~p~n", [Fact])
%%       end,
%%   gen_server:call (myengine, {add_rule,
%%                               {Z, 0},
%%                               {["{ciao,mondo}"], []}}),
  eresye:add_rule (myengine, {sample, rule2}),
  eresye:assert (myengine, [{ciao, mondo}, {mondo, 20}]),
  eresye:assert (myengine, {hello, world}),
  eresye:assert (myengine, {ok, world}),
  eresye:assert (myengine, #sample_record { a = 10, b = 50}),
  %%eresye:add_rule (myengine, {sample, rule}),
  %%eresye:add_rule (myengine, {sample, rule1}),
  ok.
%  spawn (sample, start2, []).

start1 () ->
  eresye:retract (myengine, {test}),
  eresye:assert (myengine, {test}).

start2 () ->
  F = eresye:wait (myengine, {ok, '_'}),
  io:format ("Got it ~p~n", [F]).
