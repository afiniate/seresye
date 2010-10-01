%
% phil.erl
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
-module (phil).
-compile ([export_all]).

-define (N_PHIL, 5).

start () ->
  eresye:start (restaurant),
  phil_spawn (0).

phil_spawn (?N_PHIL) -> ok;
phil_spawn (N) ->
  eresye:assert (restaurant, {fork, N}),
  spawn (phil, philosopher, [N, init]),
  if
    N < (?N_PHIL - 1) ->
      eresye:assert (restaurant, {room_ticket, N});
    true ->
      ok
  end,
  phil_spawn (N + 1).

philosopher (N, init) ->
  new_seed (),
  philosopher (N, ok);
philosopher (N, X) ->
  think (N),
  Ticket = eresye:wait_and_retract (restaurant, {room_ticket, '_'}),
  eresye:wait_and_retract (restaurant, {fork, N}),
  eresye:wait_and_retract (restaurant, {fork, (N + 1) rem ?N_PHIL}),
  eat (N),
  eresye:assert (restaurant, {fork, N}),
  eresye:assert (restaurant, {fork, (N + 1) rem ?N_PHIL}),
  eresye:assert (restaurant, Ticket),
  philosopher (N, X).

think (N) ->
  io:format ("~w: thinking ...~n", [N]),
  timer:sleep (random:uniform (10) * 1000).

eat (N) ->
  io:format ("~w: eating ...~n", [N]),
  timer:sleep (random:uniform (10) * 1000).


new_seed() ->
  {_,_,X} = erlang:now(),
  {H,M,S} = time(),
  H1 = H * X rem 32767,
  M1 = M * X rem 32767,
  S1 = S * X rem 32767,
  put(random_seed, {H1,M1,S1}).
