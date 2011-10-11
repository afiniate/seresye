%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresye_agenda).

-export([new/1, addActivation/3, addActivation/4,
         breadthOrder/2, clearAgenda/1, deleteActivation/2,
         deleteRule/2, depthOrder/2, fifoOrder/2,
         getActivation/2, getActivationFromName/2,
         getActivationSalience/2, getFirstActivation/1,
         getNumberOfActivations/1, getRulesFired/1,
         getStrategy/1, popRule/1,
         setActivationSalience/3, setRuleSalience/3,
         setStrategy/2]).

-include("internal.hrl").

-record(agenda, {rules_fired, strategy, rule_list,
                 exec_state, id, pending_actions}).

%%====================================================================
%% External functions
%%====================================================================
new(Eresye) ->
    Eresye#eresye{agenda=#agenda{rules_fired=0,
                                 strategy=depth,
                                 rule_list=[],
                                 exec_state=nil,
                                 pending_actions=[],
                                 id=0}}.

addActivation(Agenda, Rule, Args) ->
    addActivation(Agenda, Rule, Args, 0).

addActivation(EngineState0 = #eresye{agenda=
                                         Agenda0 = #agenda{strategy=Strategy,
                                                           rule_list=RuleList0,
                                                           id=Id}},
              Rule, Args, Salience) ->
    RuleList1 = case Strategy of
                    depth ->
                        depthAdd(RuleList0, Rule, Args, Salience, Id);
                    breadth ->
                        breadthAdd(RuleList0, Rule, Args, Salience, Id);
                    fifo ->
                        fifoAdd(RuleList0, Rule, Args, Salience, Id)
                  end,
    Agenda1 = Agenda0#agenda{rule_list=RuleList1, id=Id + 1},

    execute_pending(after_activation_schedule(EngineState0#eresye{agenda=Agenda1})).


%% @doc Remove all activation from Agenda,
%% returns an empty agenda with same past strategy
clearAgenda(EngineState = #eresye{agenda=Agenda0}) ->
    EngineState#eresye{agenda=Agenda0#agenda{rule_list=[], id=0}}.

getStrategy(#eresye{agenda=#agenda{strategy=Strategy}}) ->
    Strategy.

setStrategy(EngineState = #eresye{agenda=Agenda0 = #agenda{rule_list=RuleList0}}, NewStrategy) ->
    RuleList1 = case NewStrategy of
                    depth ->
                        lists:sort(fun depthOrder/2, RuleList0);
                    breadth ->
                        lists:sort(fun breadthOrder/2,
                                   RuleList0);
                    fifo ->
                        lists:sort(fun fifoOrder/2, RuleList0);
                    _ ->
                        erlang:throw({eresye, {invalid_strategy, NewStrategy}})

                end,
    EngineState#eresye{agenda=Agenda0#agenda{strategy=NewStrategy, rule_list=RuleList1}}.

getRulesFired(#eresye{agenda=#agenda{rules_fired=Fired}}) ->
    Fired.

%% @doc Remove activation with id='Id' or
%% all activation whose id is in the list passed as argument
deleteActivation(EngineState, []) -> EngineState;
deleteActivation(EngineState = #eresye{agenda=Agenda0 = #agenda{rule_list=RuleList0}},
                 Id)
  when not is_list(Id) ->
    EngineState#eresye{agenda=Agenda0#agenda{rule_list=lists:keydelete(Id, 4, RuleList0)}};
deleteActivation(EngineState, [Id | OtherId]) ->
    EngineState1 = deleteActivation(EngineState, Id),
    deleteActivation(EngineState1, OtherId).

%% @doc Remove all activation associated with rule 'Rule' from the agenda,
%% returns the modified agenda
deleteRule(EngineState = #eresye{agenda=Agenda0 = #agenda{rule_list=RuleList0}}, Rule) ->
    ActList = proplists:lookup_all(Rule, RuleList0),
    RuleList1 = lists:foldl(fun (X, R1) ->
                                    lists:delete(X, R1)
                            end,
                            RuleList0, ActList),
    EngineState#eresye{agenda=Agenda0#agenda{rule_list=RuleList1}}.

%% @doc Returns the Id of activation associated to rule 'Rule' and
%% arguments 'Args', false if it not present
getActivation(#eresye{agenda=#agenda{rule_list=RuleList0}}, {Rule, Args}) ->
    ActList = proplists:lookup_all(Rule, RuleList0),
    case lists:keysearch(Args, 2, ActList) of
        {value, {_, _, _, Id}} -> Id;
        false -> false
    end.

%% @doc Returns the Id of first activation associated to rule 'Rule',
%% false if 'Rule' is not present in the agenda
getActivationFromName(#eresye{agenda=#agenda{rule_list=RuleList0}}, Rule) ->
    case lists:keysearch(Rule, 1, RuleList0) of
        {value, {_, _, _, Id}} -> Id;
        false -> false
    end.

%% @doc Return the Id associated to first activation in the agenda
%% false if agenda is empty
getFirstActivation(#eresye{agenda=#agenda{rule_list=[First | _]}}) ->
    element(4, First).

%% @doc Return the salience value of activation with id='Id'
%% false if Id is not present in the agenda
getActivationSalience(#eresye{agenda=#agenda{rule_list=RuleList0, id=NextId}}, Id) ->
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
setActivationSalience(EngineState =
                          #eresye{agenda=Agenda0 =
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
    EngineState#eresye{agenda=Agenda0#agenda{rule_list=RuleList2}};
setActivationSalience(_EngineState, Id, NewSalience)
  when not is_list(Id) ->
    erlang:throw({eresye, {invalid_salience, NewSalience}});
setActivationSalience(EngineState0, [Id | OtherId],
                        NewSalience) ->
    EngineState1 = setActivationSalience(EngineState0, Id, NewSalience),
    setActivationSalience(EngineState1, OtherId, NewSalience).

%% @doc Sets the salience value of all activations associated to rule 'Rule',
%% returns the modified agenda
setRuleSalience(EngineState0 =
                    #eresye{agenda=#agenda{rule_list=RuleList0}}, Rule, NewSalience) ->
    ActList = proplists:lookup_all(Rule, RuleList0),
    IdList = [Id || {_, _, _, Id} <- ActList],
    setActivationSalience(EngineState0, IdList,
                          NewSalience).


%%====================================================================
%% Internal functions
%%====================================================================
execute_pending(EngineState0 = #eresye{agenda = Agenda0 = #agenda{pending_actions=[PA | Rest]}}) ->
    EngineState1 = PA(EngineState0#eresye{agenda=Agenda0#agenda{pending_actions=Rest}}),
    execute_pending(EngineState1);
execute_pending(EngineState0 = #eresye{agenda = #agenda{pending_actions=[]}}) ->
    EngineState0.

after_activation_schedule(EngineState0 =
                              #eresye{agenda=Agenda0 =
                                          #agenda{rule_list=RuleList0,
                                                  exec_state=ExecState0,
                                                  pending_actions=PA0,
                                                  rules_fired=RF0}}) ->
    {ExecState1, RuleList1, PA1, RF1} =
        if
            ExecState0 == active -> {ExecState0, RuleList0, PA0, RF0};
            true ->
                {RuleToExecute, RL} = popRule(RuleList0),
                if
                    RuleToExecute == false ->
                        {nil, RL, PA0, RF0};
                    true ->
                        {active, RL, PA0 ++ [fun(EngineState) ->
                                                     exec(EngineState, RuleToExecute)
                                             end], RF0 + 1}
                end
        end,
    EngineState0#eresye{agenda=Agenda0#agenda{exec_state=ExecState1,
                                              rule_list=RuleList1,
                                              pending_actions=PA1,
                                              rules_fired=RF1}}.

after_execution_schedule(EngineState0 =
                             #eresye{agenda=Agenda0 =
                                         #agenda{rule_list=RuleList0,
                                                 pending_actions=PA0,
                                                 rules_fired=RF0}}) ->
  {RuleToExecute, RL} = popRule(RuleList0),
  {ExecState1, RuleList1, PA1, RF1} =
    if
      RuleToExecute == false -> {nil, RL, PA0, RF0};
      true ->
        {active, RL, PA0 ++ [fun(EngineState) ->
                                     exec(EngineState, RuleToExecute)
                             end], RF0 + 1}
    end,
    EngineState0#eresye{agenda=Agenda0#agenda{exec_state=ExecState1,
                                              rule_list=RuleList1,
                                              pending_actions=PA1,
                                              rules_fired=RF1}}.

%% @doc Remove the first activation, returns the rule,
%% function arguments and the modified agenda
popRule([]) -> {false, []};
popRule([Activation | NewRuleList]) ->
    {Activation, NewRuleList}.

depthAdd(RuleList, Rule, Args, Salience, Id) ->
    {L1, L2} = lists:splitwith(fun ({_, _, S, _}) ->
                                       S > Salience
                               end,
                               RuleList),
    L1 ++ [{Rule, Args, Salience, Id} | L2].

breadthAdd(RuleList, Rule, Args, Salience, Id) ->
    {L1, L2} = lists:splitwith(fun ({_, _, S, _}) ->
                                       S >= Salience
                               end,
                               RuleList),
    L1 ++ [{Rule, Args, Salience, Id} | L2].

fifoAdd(RuleList, Rule, Args, Salience, Id) ->
    RuleList ++ [{Rule, Args, Salience, Id}].

getNumberOfActivations({_Strategy, RuleList, _NextId}) ->
    length(RuleList).

order(ActionList, Strategy) ->
    case Strategy of
        breadth ->
            lists:sort({agenda, breadthOrder}, ActionList);
        depth -> lists:sort({agenda, depthOrder}, ActionList);
        fifo -> lists:sort({agenda, fifoOrder}, ActionList);
        _Other -> ActionList
    end.

%% @doc define when an Act1 comes before Act2 in breadth strategy
breadthOrder(Act1, Act2)
  when element(3, Act1) > element(3, Act2) ->
    true;
breadthOrder(Act1, Act2)
  when element(3, Act1) < element(3, Act2) ->
    false;
breadthOrder(Act1, Act2)
  when element(4, Act1) > element(4, Act2) ->
    false;
breadthOrder(_Act1, _Act2) -> true.

%% @doc define when an Act1 comes before Act2 in depth strategy
depthOrder(Act1, Act2)
  when element(3, Act1) > element(3, Act2) ->
    true;
depthOrder(Act1, Act2)
  when element(3, Act1) < element(3, Act2) ->
    false;
depthOrder(Act1, Act2)
  when element(4, Act1) < element(4, Act2) ->
    false;
depthOrder(_Act1, _Act2) -> true.

%% @doc define when an Act1 comes before Act2 in fifo strategy
fifoOrder(Act1, Act2)
  when element(4, Act1) > element(4, Act2) ->
    false;
fifoOrder(_Act1, _Act2) -> true.

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
            {'EXIT', Reason} ->
                erlang:throw({eresye, {rule_execution,
                                       [R, Reason]}});
            EngineState1 -> EngineState1
        end,
    after_execution_schedule(EngineState2).


%% Fun = {Module, Function} or
%% Fun = fun (...)
execute_rule(EngineState, {Fun, Args, _, _}) ->
    apply(Fun, [EngineState | Args]).
