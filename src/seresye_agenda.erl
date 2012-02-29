%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_agenda).

-export([new/1, add_activation/3, add_activation/4,
         breadth_order/2, clear_agenda/1, delete_activation/2,
         delete_rule/2, depth_order/2, fifo_order/2,
         get_activation/2, get_activation_from_name/2,
         get_activation_salience/2, get_first_activation/1,
         get_number_of_activations/1, get_rules_fired/1,
         get_strategy/1, pop_rule/1,
         set_activation_salience/3, set_rule_salience/3,
         set_strategy/2]).

-include("internal.hrl").

-record(agenda, {rules_fired, strategy, rule_list,
                 exec_state, id, pending_actions}).

%%====================================================================
%% External functions
%%====================================================================
new(Seresye) ->
    Seresye#seresye{agenda=#agenda{rules_fired=0,
                                 strategy=depth,
                                 rule_list=[],
                                 exec_state=nil,
                                 pending_actions=[],
                                 id=0}}.

add_activation(Agenda, Rule, Args) ->
    add_activation(Agenda, Rule, Args, 0).

add_activation(EngineState0 = #seresye{agenda=
                                          Agenda0 = #agenda{strategy=Strategy,
                                                            rule_list=RuleList0,
                                                            id=Id}},
               Rule, Args, Salience) ->
    RuleList1 = case Strategy of
                    depth ->
                        depth_add(RuleList0, Rule, Args, Salience, Id);
                    breadth ->
                        breadth_add(RuleList0, Rule, Args, Salience, Id);
                    fifo ->
                        fifo_add(RuleList0, Rule, Args, Salience, Id)
                  end,
    Agenda1 = Agenda0#agenda{rule_list=RuleList1, id=Id + 1},

    execute_pending(after_activation_schedule(EngineState0#seresye{agenda=Agenda1})).


%% @doc Remove all activation from Agenda,
%% returns an empty agenda with same past strategy
clear_agenda(EngineState = #seresye{agenda=Agenda0}) ->
    EngineState#seresye{agenda=Agenda0#agenda{rule_list=[], id=0}}.

get_strategy(#seresye{agenda=#agenda{strategy=Strategy}}) ->
    Strategy.

set_strategy(EngineState = #seresye{agenda=Agenda0 = #agenda{rule_list=RuleList0}}, NewStrategy) ->
    RuleList1 = case NewStrategy of
                    depth ->
                        lists:sort(fun depth_order/2, RuleList0);
                    breadth ->
                        lists:sort(fun breadth_order/2,
                                   RuleList0);
                    fifo ->
                        lists:sort(fun fifo_order/2, RuleList0);
                    _ ->
                        erlang:throw({seresye, {invalid_strategy, NewStrategy}})

                end,
    EngineState#seresye{agenda=Agenda0#agenda{strategy=NewStrategy, rule_list=RuleList1}}.

get_rules_fired(#seresye{agenda=#agenda{rules_fired=Fired}}) ->
    Fired.

%% @doc Remove activation with id='Id' or
%% all activation whose id is in the list passed as argument
delete_activation(EngineState, []) -> EngineState;
delete_activation(EngineState = #seresye{agenda=Agenda0 = #agenda{rule_list=RuleList0}},
                 Id)
  when not is_list(Id) ->
    EngineState#seresye{agenda=Agenda0#agenda{rule_list=lists:keydelete(Id, 4, RuleList0)}};
delete_activation(EngineState, [Id | OtherId]) ->
    EngineState1 = delete_activation(EngineState, Id),
    delete_activation(EngineState1, OtherId).

%% @doc Remove all activation associated with rule 'Rule' from the agenda,
%% returns the modified agenda
delete_rule(EngineState = #seresye{agenda=Agenda0 = #agenda{rule_list=RuleList0}}, Rule) ->
    ActList = proplists:lookup_all(Rule, RuleList0),
    RuleList1 = lists:foldl(fun (X, R1) ->
                                    lists:delete(X, R1)
                            end,
                            RuleList0, ActList),
    EngineState#seresye{agenda=Agenda0#agenda{rule_list=RuleList1}}.

%% @doc Returns the Id of activation associated to rule 'Rule' and
%% arguments 'Args', false if it not present
get_activation(#seresye{agenda=#agenda{rule_list=RuleList0}}, {Rule, Args}) ->
    ActList = proplists:lookup_all(Rule, RuleList0),
    case lists:keysearch(Args, 2, ActList) of
        {value, {_, _, _, Id}} -> Id;
        false -> false
    end.

%% @doc Returns the Id of first activation associated to rule 'Rule',
%% false if 'Rule' is not present in the agenda
get_activation_from_name(#seresye{agenda=#agenda{rule_list=RuleList0}}, Rule) ->
    case lists:keysearch(Rule, 1, RuleList0) of
        {value, {_, _, _, Id}} -> Id;
        false -> false
    end.

%% @doc Return the Id associated to first activation in the agenda
%% false if agenda is empty
get_first_activation(#seresye{agenda=#agenda{rule_list=[First | _]}}) ->
    element(4, First).

%% @doc Return the salience value of activation with id='Id'
%% false if Id is not present in the agenda
get_activation_salience(#seresye{agenda=#agenda{rule_list=RuleList0, id=NextId}}, Id) ->
    case NextId < Id of
        true -> false;
        false ->
            case lists:keysearch(Id, 4, RuleList0) of
                {value, {_, _, Salience, _}} -> Salience;
                false -> false
            end
    end.

%% @doc Sets the salience value of activation with id='Id',
%% returns the modified agenda
set_activation_salience(EngineState =
                            #seresye{agenda=Agenda0 =
                                        #agenda{rule_list=RuleList0, strategy=Strategy}}, Id, NewSalience)
  when is_number(NewSalience) and not is_list(Id) ->
    RuleList2 =
        case lists:keysearch(Id, 4, RuleList0) of
            {value, {Rule, Args, _Salience, Id}} ->
                RuleList1 = lists:keyreplace(Id, 4, RuleList0,
                                             {Rule, Args, NewSalience, Id}),
                order(RuleList1, Strategy);
            false ->
                RuleList0
        end,
    EngineState#seresye{agenda=Agenda0#agenda{rule_list=RuleList2}};
set_activation_salience(_EngineState, Id, NewSalience)
  when not is_list(Id) ->
    erlang:throw({seresye, {invalid_salience, NewSalience}});
set_activation_salience(EngineState0, [Id | OtherId],
                        NewSalience) ->
    EngineState1 = set_activation_salience(EngineState0, Id, NewSalience),
    set_activation_salience(EngineState1, OtherId, NewSalience).

%% @doc Sets the salience value of all activations associated to rule 'Rule',
%% returns the modified agenda
set_rule_salience(EngineState0 =
                    #seresye{agenda=#agenda{rule_list=RuleList0}}, Rule, NewSalience) ->
    ActList = proplists:lookup_all(Rule, RuleList0),
    IdList = [Id || {_, _, _, Id} <- ActList],
    set_activation_salience(EngineState0, IdList,
                            NewSalience).


%%====================================================================
%% Internal functions
%%====================================================================
execute_pending(EngineState0 = #seresye{agenda = Agenda0 = #agenda{pending_actions=[PA | Rest]}}) ->
    EngineState1 = PA(EngineState0#seresye{agenda=Agenda0#agenda{pending_actions=Rest}}),
    execute_pending(EngineState1);
execute_pending(EngineState0 = #seresye{agenda = #agenda{pending_actions=[]}}) ->
    EngineState0.

after_activation_schedule(EngineState0 =
                              #seresye{agenda=Agenda0 =
                                          #agenda{rule_list=RuleList0,
                                                  exec_state=ExecState0,
                                                  pending_actions=PA0,
                                                  rules_fired=RF0}}) ->
    {ExecState1, RuleList1, PA1, RF1} =
        if
            ExecState0 == active -> {ExecState0, RuleList0, PA0, RF0};
            true ->
                {RuleToExecute, RL} = pop_rule(RuleList0),
                if
                    RuleToExecute == false ->
                        {nil, RL, PA0, RF0};
                    true ->
                        {active, RL, PA0 ++ [fun(EngineState) ->
                                                     exec(EngineState, RuleToExecute)
                                             end], RF0 + 1}
                end
        end,
    EngineState0#seresye{agenda=Agenda0#agenda{exec_state=ExecState1,
                                              rule_list=RuleList1,
                                              pending_actions=PA1,
                                              rules_fired=RF1}}.

after_execution_schedule(EngineState0 =
                             #seresye{agenda=Agenda0 =
                                         #agenda{rule_list=RuleList0,
                                                 pending_actions=PA0,
                                                 rules_fired=RF0}}) ->
  {RuleToExecute, RL} = pop_rule(RuleList0),
  {ExecState1, RuleList1, PA1, RF1} =
    if
      RuleToExecute == false -> {nil, RL, PA0, RF0};
      true ->
        {active, RL, PA0 ++ [fun(EngineState) ->
                                     exec(EngineState, RuleToExecute)
                             end], RF0 + 1}
    end,
    EngineState0#seresye{agenda=Agenda0#agenda{exec_state=ExecState1,
                                              rule_list=RuleList1,
                                              pending_actions=PA1,
                                              rules_fired=RF1}}.

%% @doc Remove the first activation, returns the rule,
%% function arguments and the modified agenda
pop_rule([]) -> {false, []};
pop_rule([Activation | NewRuleList]) ->
    {Activation, NewRuleList}.

depth_add(RuleList, Rule, Args, Salience, Id) ->
    {L1, L2} = lists:splitwith(fun ({_, _, S, _}) ->
                                       S > Salience
                               end,
                               RuleList),
    L1 ++ [{Rule, Args, Salience, Id} | L2].

breadth_add(RuleList, Rule, Args, Salience, Id) ->
    {L1, L2} = lists:splitwith(fun ({_, _, S, _}) ->
                                       S >= Salience
                               end,
                               RuleList),
    L1 ++ [{Rule, Args, Salience, Id} | L2].

fifo_add(RuleList, Rule, Args, Salience, Id) ->
    RuleList ++ [{Rule, Args, Salience, Id}].

get_number_of_activations({_Strategy, RuleList, _NextId}) ->
    length(RuleList).

order(ActionList, Strategy) ->
    case Strategy of
        breadth ->
            lists:sort(fun breadth_order/2, ActionList);
        depth -> lists:sort(fun depth_order/2, ActionList);
        fifo -> lists:sort(fun fifo_order/2, ActionList);
        _Other -> ActionList
    end.

%% @doc define when an Act1 comes before Act2 in breadth strategy
breadth_order(Act1, Act2)
  when element(3, Act1) > element(3, Act2) ->
    true;
breadth_order(Act1, Act2)
  when element(3, Act1) < element(3, Act2) ->
    false;
breadth_order(Act1, Act2)
  when element(4, Act1) > element(4, Act2) ->
    false;
breadth_order(_Act1, _Act2) -> true.

%% @doc define when an Act1 comes before Act2 in depth strategy
depth_order(Act1, Act2)
  when element(3, Act1) > element(3, Act2) ->
    true;
depth_order(Act1, Act2)
  when element(3, Act1) < element(3, Act2) ->
    false;
depth_order(Act1, Act2)
  when element(4, Act1) < element(4, Act2) ->
    false;
depth_order(_Act1, _Act2) -> true.

%% @doc define when an Act1 comes before Act2 in fifo strategy
fifo_order(Act1, Act2)
  when element(4, Act1) > element(4, Act2) ->
    false;
fifo_order(_Act1, _Act2) -> true.

%%====================================================================
%% Executor functions
%%====================================================================
exec(EngineState0, R) ->
    {Mod, Fun} = case R of
                     {{M, F}, _, _, _} -> {M, F};
                     _ -> {'_', '_'}
                 end,
    EngineState2 =
        case catch execute_rule(EngineState0, R) of
            {'EXIT', {function_clause, [{Mod, Fun, _} | _]}} -> EngineState0;
            {'EXIT', {function_clause, [{Mod, Fun, _, _Location} | _]}} -> EngineState0;
            {'EXIT', Reason} ->
                erlang:throw({seresye, {rule_execution,
                                       [R, Reason]}});
            EngineState1 -> EngineState1
        end,
    after_execution_schedule(EngineState2).


%% Fun = {Module, Function} or
%% Fun = fun (...)
execute_rule(EngineState, {{M,F}, Args, X1, X2}) ->
    L = length(Args) + 1,
    execute_rule(EngineState, {fun M:F/L, Args, X1, X2});
execute_rule(EngineState, {Fun, Args, _, _}) when is_function(Fun) ->
    case proplists:get_value(before_rule, EngineState#seresye.hooks) of
        BF when is_function(BF) ->
            BF(EngineState, Fun, Args);
        _ ->
            ignore
    end,
    Result = apply(Fun, [EngineState#seresye { fired_rule = {Fun, Args} } | Args]),
    case proplists:get_value(after_rule, EngineState#seresye.hooks) of
        AF when is_function(AF) ->
            AF(Result, Fun, Args);
        _ ->
            ignore
    end,
    Result#seresye{ fired_rule = undefined }.

