%
% cannibals.erl
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
%
%% ======================================================
%%    Cannibals and Missionaries Problem
%%
%%     Another classic AI problem. The point is
%%     to get three cannibals and three missionaries
%%     across a stream with a boat that can only
%%     hold two people. If the cannibals outnumber
%%     the missionaries on either side of the stream,
%%     then the cannibals will eat the missionaries.
%%
%%     It is a bare translation of the same example
%%     provided in CLIPS Version 6.0
%%
%%     To execute, type 'cannibals:start().'
%% ======================================================

-module (cannibals).
-compile ([export_all]).

-record (status, {'search-depth',
                  'parent',
                  'shore-1-missionaries',
                  'shore-1-cannibals',
                  'shore-2-missionaries',
                  'shore-2-cannibals',
                  'boat-location',
                  'last-move'}).


-record (moves, {id, 'moves-list'}).


-define (INITIAL_MISSIONARIES, 3).
-define (INITIAL_CANNIBALS, 3).

for (Min, Max) when Min > Max -> [];
for (Min, Max) -> lists:seq (Min, Max).

shore_1_move (Engine,
              #status {'boat-location' = 'shore-1'} = Node,
              {'boat-can-host', Limit}) ->
  Num = Node#status.'search-depth',
  S1M = Node#status.'shore-1-missionaries',
  S1C = Node#status.'shore-1-cannibals',
  S2M = Node#status.'shore-2-missionaries',
  S2C = Node#status.'shore-2-cannibals',

  MaxMissionaries = lists:min ([S1M, Limit]),

  lists:foreach (
    fun (Missionaries) ->
        MinCannibals = lists:max ([0, 1 - Missionaries]),
        MaxCannibals = lists:min ([S1C, Limit - Missionaries]),

        %%io:format ("Shore 1 ~p,~p~n", [MinCannibals, MaxCannibals]),

        lists:foreach (
          fun (Cannibals) ->
              NewNode =
                #status { 'search-depth' = Num + 1,
                          'parent' = Node,
                          'shore-1-missionaries' = S1M - Missionaries,
                          'shore-1-cannibals' =  S1C - Cannibals,
                          'shore-2-missionaries' =  Missionaries + S2M,
                          'shore-2-cannibals' = Cannibals + S2C,
                          'boat-location' = 'shore-2',
                          'last-move' = move_string (Missionaries,
                                                     Cannibals,
                                                     "shore-2")},
              eresye:assert (Engine, NewNode)
          end,
          for (MinCannibals, MaxCannibals))
    end,
    for (0, MaxMissionaries)).


shore_2_move (Engine,
              #status {'boat-location' = 'shore-2'} = Node,
              {'boat-can-host', Limit}) ->
  Num = Node#status.'search-depth',
  S1M = Node#status.'shore-1-missionaries',
  S1C = Node#status.'shore-1-cannibals',
  S2M = Node#status.'shore-2-missionaries',
  S2C = Node#status.'shore-2-cannibals',

  MaxMissionaries = lists:min ([S2M, Limit]),

  lists:foreach (
    fun (Missionaries) ->
        MinCannibals = lists:max ([0, 1 - Missionaries]),
        MaxCannibals = lists:min ([S2C, Limit - Missionaries]),

        %%io:format ("Shore 2 ~p,~p~n", [MinCannibals, MaxCannibals]),

        lists:foreach (
          fun (Cannibals) ->
              NewNode =
                #status { 'search-depth' = Num + 1,
                          'parent' = Node,
                          'shore-1-missionaries' = S1M + Missionaries,
                          'shore-1-cannibals' = S1C + Cannibals,
                          'shore-2-missionaries' =  S2M - Missionaries,
                          'shore-2-cannibals' = S2C - Cannibals,
                          'boat-location' = 'shore-1',
                          'last-move' = move_string (Missionaries,
                                                     Cannibals,
                                                     "shore-1")},
              eresye:assert (Engine, NewNode)
          end,
          for (MinCannibals, MaxCannibals))
    end,
    for (0, MaxMissionaries)).



cannibals_eat_missionaries (Engine,
                            #status {'shore-1-missionaries' = S1M,
                                     'shore-1-cannibals' = S1C,
                                     'shore-2-missionaries' = S2M,
                                     'shore-2-cannibals' =  S2C} = Node) ->
  if
    ((S2C > S2M) and (S2M =/= 0)) or ((S1C > S1M) and (S1M =/= 0)) ->
      %%io:format ("Invalid ~p~n", [Node]),
      eresye:retract (Engine, Node);
    true -> ok
  end.


circular_path  (Engine,
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
    SD1 < SD2 -> eresye:retract (Engine, Node);
    true -> ok
  end.


recognize_solution (Engine,
                    #status {parent = Parent,
                             'shore-2-missionaries' = M,
                             'shore-2-cannibals' = C,
                             'last-move' = Move} = Node) ->
  if
    (M == ?INITIAL_MISSIONARIES) and (C == ?INITIAL_CANNIBALS) ->
      eresye:retract (Engine, Node),
      eresye:assert (Engine, #moves { id = Parent, 'moves-list' = Move});
    true -> ok
  end.

move_string (0, 1, Shore) ->
  lists:concat (["Move 1 cannibal to ", Shore]);
move_string (0, N, Shore) ->
  lists:concat (["Move ", N, " cannibals to ", Shore]);
move_string (1, 0, Shore) ->
  lists:concat (["Move 1 missionary to ", Shore]);
move_string (1, 1, Shore) ->
  lists:concat (["Move 1 missionary and 1 cannibal to ", Shore]);
move_string (1, N, Shore) ->
  lists:concat (["Move 1 missionary and ", N, " cannibals to ", Shore]);
move_string (M, 0, Shore) ->
  lists:concat (["Move ", M ," missionaries to ", Shore]);
move_string (M, 1, Shore) ->
  lists:concat (["Move ", M, " missionaries and 1 cannibal to ", Shore]);
move_string (M, N, Shore) ->
  lists:concat (["Move ", M, " missionaries and ", N, " cannibals to ",
                 Shore]).


print_solution (#status { parent = 'no-parent' }) ->
  ok;
print_solution (S) ->
  print_solution (S#status.parent),
  io:format ("~p~n", [S#status.'last-move']).


start () ->
  eresye:start (cannibals),
  eresye:add_rule (cannibals, {cannibals, shore_1_move}),
  eresye:add_rule (cannibals, {cannibals, shore_2_move}),
  eresye:add_rule (cannibals, {cannibals, cannibals_eat_missionaries}, 10),
  eresye:add_rule (cannibals, {cannibals, circular_path}, 10),
  eresye:add_rule (cannibals, {cannibals, recognize_solution}),
  eresye:assert (cannibals,
                 #status {'search-depth' =  1,
                          'parent' =  'no-parent',
                          'shore-1-missionaries' = ?INITIAL_MISSIONARIES,
                          'shore-2-missionaries' = 0,
                          'shore-1-cannibals' = ?INITIAL_CANNIBALS,
                          'shore-2-cannibals' = 0,
                          'boat-location' = 'shore-1',
                          'last-move' =  'No move.'}),
  eresye:assert (cannibals, {'boat-can-host', 2}),
  P = eresye:wait (cannibals, #moves {id = '_', 'moves-list'= '_'}),
  print_solution (P#moves.id),
  io:format ("~p~n", [P#moves.'moves-list']),
  eresye:stop (cannibals).

