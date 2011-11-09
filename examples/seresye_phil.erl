%%%  SERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php%
-module(seresye_phil).

-export([start/0, phil_spawn/1, philosopher/2, think/1, eat/1]).

-define(N_PHIL, 5).

start() ->
    application:start(seresye),
    seresye:start(restaurant),
    phil_spawn(0).

phil_spawn(?N_PHIL) -> ok;
phil_spawn(N) ->
    seresye:assert(restaurant, {fork, N}),
    spawn(phil, philosopher, [N, init]),
    if N < (?N_PHIL) - 1 ->
           seresye:assert(restaurant, {room_ticket, N});
       true -> ok
    end,
    phil_spawn(N + 1).

philosopher(N, init) -> new_seed(), philosopher(N, ok);
philosopher(N, X) ->
    think(N),
    Ticket = seresye:wait_and_retract(restaurant,
                                     {room_ticket, '_'}),
    seresye:wait_and_retract(restaurant, {fork, N}),
    seresye:wait_and_retract(restaurant,
                            {fork, (N + 1) rem (?N_PHIL)}),
    eat(N),
    seresye:assert(restaurant, {fork, N}),
    seresye:assert(restaurant,
                  {fork, (N + 1) rem (?N_PHIL)}),
    seresye:assert(restaurant, Ticket),
    philosopher(N, X).

think(N) ->
    io:format("~w: thinking ...~n", [N]),
    timer:sleep(random:uniform(10) * 1000).

eat(N) ->
    io:format("~w: eating ...~n", [N]),
    timer:sleep(random:uniform(10) * 1000).

new_seed() ->
    {_, _, X} = erlang:now(),
    {H, M, S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1, M1, S1}).
