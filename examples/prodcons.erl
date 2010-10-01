%
% prodcons.erl
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
