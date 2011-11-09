%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_prodcons).

-export([start/0, prod/1, cons/1, cons_1/1]).

start() ->
    application:start(seresye),
    eresye:start(pc),
    spawn(prodcons, cons_1, [1]),
    spawn(prodcons, cons_1, [2]),
    spawn(prodcons, cons_1, [3]),
    spawn(prodcons, prod, [0]),
    ok.

prod(20) -> ok;
prod(Index) ->
    eresye:assert(pc, {item, Index}), prod(Index + 1).

cons(20) -> ok;
cons(Index) ->
    Fact = eresye:retract(pc,
                          {item, fun (X) -> X == Index end}),
    io:format("Consumer ~p~n", [Fact]),
    cons(Index + 1).

cons_1(N) ->
    Fact = eresye:wait_and_retract(pc, {item, '_'}),
    io:format("~w: Consumer ~p~n", [N, Fact]),
    timer:sleep(random:uniform(500)),
    cons_1(N).
