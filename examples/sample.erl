%
% sample.erl
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
%%
%
-module (sample).
-export ([rule/4, rule1/3, rule2/4, rule3/3, start/0, start1/0, start2/0]).

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

rule3 (Engine, {hello, [H|T]}, {test, T}) ->
  io:format ("Hit 3!\n"),
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

  eresye:add_rule (myengine, {sample, rule3}),
  eresye:assert (myengine, {hello, [ciao, mondo]}),
  eresye:assert (myengine, {test, ciao}),
  eresye:assert (myengine, {test, [ciao]}),
  eresye:assert (myengine, {test, [mondo]}),
  eresye:assert (myengine, {hello, [ciao, mondo, world]}),
  eresye:assert (myengine, {test, [mondo, world]}),
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
