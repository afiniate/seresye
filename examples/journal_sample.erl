%
% journal_sample.erl
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
-module (journal_sample).
-compile ([export_all]).
-include ("journal.hrl").


-define (AddDefaultRule (Fun, Class),
         Fun (_,_,_,#Class{}) -> ok;
         Fun (E, T, I, Y) -> Fun (E, T, I, journal:Class (Y))).


ask_question (Prompt) ->
  lists:reverse (tl(lists:reverse (io:get_line (Prompt)))).


national_journal (_, {topic, Topic}, {ifactor, MinimumIF},
             #'national-journal' { main_topic = Topic,
                                   impact_factor = IF} = J)
    when IF >= MinimumIF ->
  io:format ("The journal is ~p~n", [J]);
?AddDefaultRule (national_journal, 'national-journal').


intl_journal (_, {topic, Topic}, {ifactor, MinimumIF},
              #'international-journal' { main_topic = Topic,
                                         impact_factor = IF} = J)
  when IF >= MinimumIF ->
  io:format ("The journal is ~p~n", [J]);
?AddDefaultRule (intl_journal, 'international-journal').


ieee_journal (_, {topic, Topic}, {ifactor, MinimumIF},
             #'IEEE-journal' { main_topic = Topic,
                               impact_factor = IF} = J)
    when IF >= MinimumIF ->
  io:format ("The journal is ~p~n", [J]);
?AddDefaultRule (ieee_journal, 'IEEE-journal').


acm_journal (_, {topic, Topic}, {ifactor, MinimumIF},
             #'ACM-journal' { main_topic = Topic,
                               impact_factor = IF} = J)
    when IF >= MinimumIF ->
  io:format ("The journal is ~p~n", [J]);
?AddDefaultRule (acm_journal, 'ACM-journal').


get_journal_type (Engine, {any_publisher}) ->
  T = ask_question ('Select the type (i)nternational/(n)ational): '),
  case T of
    "i" ->
      eresye:add_rule (Engine, {journal_sample, intl_journal, 1}, 8);
    "n" ->
      eresye:add_rule (Engine, {journal_sample, national_journal, 1}, 8)
  end.


get_publisher (Engine, {start, _})
    when not ["{publisher_selected}"]; true ->
  Pub = ask_question ('Select the publisher (ieee/acm/any): '),
  case Pub of
    "ieee" ->
      eresye:add_rule (Engine, {journal_sample, ieee_journal, 1}, 8),
      eresye:assert (Engine, {publisher_selected});
    "acm" ->
      eresye:add_rule (Engine, {journal_sample, acm_journal, 1}, 8),
      eresye:assert (Engine, {publisher_selected});
    _ ->
      eresye:assert (Engine, [{publisher_selected}, {any_publisher}])
  end.


get_if (Engine, {start, _}) when not ["{ifactor, _}"]; true->
  IF = ask_question ('What is your minimum impact factor: '),
  eresye:assert (Engine, {ifactor, list_to_float (IF)}).

get_topics (Engine, {start, _} = X) when not ["{topic, end_of_list}"]; true->
  Topic = ask_question ('Enter topic of your paper ("q" to end): '),
  case Topic of
    "q" -> eresye:assert (Engine, {topic, end_of_list});
    _ -> eresye:assert (Engine, {topic, list_to_atom (Topic)}),
         eresye:retract (Engine, X),
         eresye:assert (Engine, X)
  end.

final_rule (_, {start, Pid}) ->
  Pid ! ok.


start () ->
  eresye:start (js, journal),

  eresye:add_rule (js, {journal_sample, get_topics}, 10),
  eresye:add_rule (js, {journal_sample, get_if}, 9),
  eresye:add_rule (js, {journal_sample, get_publisher}, 9),
  eresye:add_rule (js, {journal_sample, get_journal_type}, 9),
  eresye:add_rule (js, {journal_sample, final_rule}, 1),

  eresye:assert (js,
                 [#'IEEE-journal' { title = 'Trans. on Soft. Eng.',
                                    main_topic = 'any',
                                    impact_factor = 0.6},
                  #'IEEE-journal' { title = 'Trans. on PAMI',
                                    main_topic = 'artificial intelligence',
                                    impact_factor = 0.8},
                  #'IEEE-journal' { title = 'Trans. on Parallel and ...',
                                    main_topic = 'any',
                                    impact_factor = 0.9},
                  #'IEEE-journal' { title = 'Trans. on Mobile Computing',
                                    main_topic = 'mobile systems',
                                    impact_factor = 0.7},
                  #'IEEE-journal' { title = 'Internet Computing',
                                    main_topic = 'internet',
                                    impact_factor = 0.9},
                  #'ACM-journal' { title = 'Transaction on Internet Computing',
                                   main_topic = 'internet',
                                   impact_factor = 0.92},
                  #'ACM-journal' { title = 'TOPLAS',
                                   main_topic = 'programming languages',
                                   impact_factor = 0.92},
                  #'italian-journal' { title = 'Intelligenza Artificiale',
                                       publisher = 'AI*IA',
                                       main_topic = 'artificial intelligence',
                                       impact_factor = 0.2}]),
  eresye:assert (js, {start, self ()}),
  receive
    _ -> ok
  end,
  eresye:get_rete (js),
  eresye:stop (js).


