%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresyee_wine_sample).

-export([rule/2, start/0]).

-include("eresyee_wine.hrl").

rule(Engine, #wine{} = W) ->
    io:format("~p~n", [W]),
    Engine;
rule(Engine, W) ->
    io:format("~p~n", [W]),
    Engine.

start() ->
    Engine1 = eresye_engine:add_rule(eresye_engine:new_with_ontology(eresyee_wine), {?MODULE, rule, 1}),
    eresye_engine:assert(Engine1, [#'Chianti'{},
                                   #wine_grape{}]).

