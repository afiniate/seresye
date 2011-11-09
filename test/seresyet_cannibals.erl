%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
%%% ======================================================
%%%    Cannibals and Missionaries Problem
%%%
%%%     Another classic AI problem. The point is
%%%     to get three cannibals and three missionaries
%%%     across a stream with a boat that can only
%%%     hold two people. If the cannibals outnumber
%%%     the missionaries on either side of the stream,
%%%     then the cannibals will eat the missionaries.
%%%
%%%     It is a bare translation of the same example
%%%     provided in CLIPS Version 6.0
%%%
%%% ======================================================
-module (seresyet_cannibals).

-export([shore_1_move/3,
         shore_2_move/3,
         cannibals_eat_missionaries/2,
         circular_path/3,
         recognize_solution/2]).

-record (status, {'search-depth',
                  'parent',
                  'shore-1-missionaries',
                  'shore-1-cannibals',
                  'shore-2-missionaries',
                  'shore-2-cannibals',
                  'boat-location'}).


-define (INITIAL_MISSIONARIES, 3).
-define (INITIAL_CANNIBALS, 3).

-include_lib("eunit/include/eunit.hrl").

-rules([shore_1_move,
        shore_2_move,
        {cannibals_eat_missionaries, 10},
        {circular_path, 10},
        {recognize_solution, 10}]).

for (Min, Max) when Min > Max -> [];
for (Min, Max) -> lists:seq (Min, Max).

shore_1_move (Engine0,
              #status {'boat-location' = 'shore-1'} = Node,
              {'boat-can-host', Limit}) ->
    Num = Node#status.'search-depth',
    S1M = Node#status.'shore-1-missionaries',
    S1C = Node#status.'shore-1-cannibals',
    S2M = Node#status.'shore-2-missionaries',
    S2C = Node#status.'shore-2-cannibals',

    MaxMissionaries = lists:min ([S1M, Limit]),

    lists:foldl (
      fun (Missionaries, Engine1) ->
              MinCannibals = lists:max ([0, 1 - Missionaries]),
              MaxCannibals = lists:min ([S1C, Limit - Missionaries]),

              %%io:format ("Shore 1 ~p,~p~n", [MinCannibals, MaxCannibals]),

              lists:foldl (
                fun (Cannibals, Engine2) ->
                        NewNode =
                            #status { 'search-depth' = Num + 1,
                                      'parent' = Node,
                                      'shore-1-missionaries' = S1M - Missionaries,
                                      'shore-1-cannibals' =  S1C - Cannibals,
                                      'shore-2-missionaries' =  Missionaries + S2M,
                                      'shore-2-cannibals' = Cannibals + S2C,
                                      'boat-location' = 'shore-2'},

                        seresye_engine:assert(Engine2, NewNode)
                end,
                Engine1,
                for (MinCannibals, MaxCannibals))
      end,
      Engine0,
      for (0, MaxMissionaries)).


shore_2_move (Engine0,
              #status {'boat-location' = 'shore-2'} = Node,
              {'boat-can-host', Limit}) ->
    Num = Node#status.'search-depth',
    S1M = Node#status.'shore-1-missionaries',
    S1C = Node#status.'shore-1-cannibals',
    S2M = Node#status.'shore-2-missionaries',
    S2C = Node#status.'shore-2-cannibals',

    MaxMissionaries = lists:min ([S2M, Limit]),

    lists:foldl(
      fun (Missionaries, Engine1) ->
              MinCannibals = lists:max ([0, 1 - Missionaries]),
              MaxCannibals = lists:min ([S2C, Limit - Missionaries]),

              lists:foldl (
                fun (Cannibals, Engine2) ->
                        NewNode =
                            #status { 'search-depth' = Num + 1,
                                      'parent' = Node,
                                      'shore-1-missionaries' = S1M + Missionaries,
                                      'shore-1-cannibals' = S1C + Cannibals,
                                      'shore-2-missionaries' =  S2M - Missionaries,
                                      'shore-2-cannibals' = S2C - Cannibals,
                                      'boat-location' = 'shore-1'},
                        seresye_engine:assert (Engine2, NewNode)
                end,
                Engine1,
                for (MinCannibals, MaxCannibals))
      end,
      Engine0,
      for (0, MaxMissionaries)).



cannibals_eat_missionaries (Engine0,
                            #status {'shore-1-missionaries' = S1M,
                                     'shore-1-cannibals' = S1C,
                                     'shore-2-missionaries' = S2M,
                                     'shore-2-cannibals' =  S2C} = Node) ->
    if
        ((S2C > S2M) and (S2M =/= 0)) or ((S1C > S1M) and (S1M =/= 0)) ->
            %%io:format ("Invalid ~p~n", [Node]),
            seresye_engine:retract (Engine0, Node);
        true -> Engine0
    end.


circular_path  (Engine0,
                #status {'search-depth' = SD1,
                         'boat-location' = BL,
                         'shore-1-missionaries' = S1M,
                         'shore-1-cannibals' = S1C,
                         'shore-2-missionaries' = S2M,
                         'shore-2-cannibals' = S2C},
                #status {'search-depth' = SD2,
                         'boat-location' = BL,
                         'shore-1-missionaries' = S1M,
                         'shore-1-cannibals' = S1C,
                         'shore-2-missionaries' = S2M,
                         'shore-2-cannibals' = S2C} = Node) ->
    if
        SD1 < SD2 -> seresye_engine:retract (Engine0, Node);
        true -> Engine0
    end.


recognize_solution (Engine0,
                    #status {'shore-2-missionaries' = M,
                             'shore-2-cannibals' = C} = Node) ->
    if
        (M == ?INITIAL_MISSIONARIES) and (C == ?INITIAL_CANNIBALS) ->
            Engine1 = seresye_engine:retract (Engine0, Node),
            seresye_engine:set_client_state(Engine1, Node);
        true -> Engine0
    end.

rules_test() ->
    Engine0 = seresye_engine:new(),
    Engine2 = seresye_engine:add_rules(Engine0, ?MODULE),

    Engine3 =
        seresye_engine:assert (Engine2,
                               #status {'search-depth' =  1,
                                        'parent' =  'no-parent',
                                        'shore-1-missionaries' = ?INITIAL_MISSIONARIES,
                                        'shore-2-missionaries' = 0,
                                        'shore-1-cannibals' = ?INITIAL_CANNIBALS,
                                        'shore-2-cannibals' = 0,
                                        'boat-location' = 'shore-1'}),
    Engine4 = seresye_engine:assert (Engine3, {'boat-can-host', 2}),
    P = seresye_engine:get_client_state(Engine4),
    verify_result(P).

verify_result(Node = #status{'shore-2-missionaries'=S2M,
                             'shore-2-cannibals'=S2C}) ->
    ?assertMatch(?INITIAL_MISSIONARIES, S2M),
    ?assertMatch(?INITIAL_CANNIBALS, S2C),
    verify_result_(Node).

verify_result_(#status{'parent' =  'no-parent'}) ->
    ok;
verify_result_(#status{'parent' =  Parent,
                       'shore-1-missionaries'=S1M,
                       'shore-1-cannibals'=S1C,
                       'shore-2-missionaries'=S2M,
                       'shore-2-cannibals'=S2C}) ->
    ?assertMatch(true, (S1M >= S1C orelse S1M == 0)),
    ?assertMatch(true, (S2M >= S2C orelse S2M == 0)),
    verify_result_(Parent).
