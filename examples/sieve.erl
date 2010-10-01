%
% sieve.erl
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
