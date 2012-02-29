-module(seresye_transform).
-export([parse_transform/2]).

-record(state,{ 
          rules = [],
          rule_functions = [],
          exports = [],
          options
         }).

-record(context, {module,
                  function,
                  arity,
                  file,
                  options}).


parse_transform(Forms, Options) ->
    #context{ file = File } = parse_trans:initial_context(Forms, Options),
    case erl_lint:module(Forms, File, [nowarn_unused_function,nowarn_unused_vars,nowarn_unused_record]) of
        {error, _Errors, _Warnings} ->
            Forms;
        _ ->
            {Forms1, State} = parse_trans:transform(fun do_transform/4, 
                                             #state{ options = Options },
                                                    Forms, Options),
            Result0 = parse_trans:revert(Forms1),
            lists:foldl(fun ({Fun, Arity}, Acc) ->
                                parse_trans:export_function(Fun, Arity, Acc)
                        end, Result0, State#state.rule_functions)
    end.

do_transform(attribute,{attribute, _, export, Exports} = Attr, _Context, #state{} = State) ->
    {Attr, true, State#state{ exports = State#state.exports ++ Exports }};

do_transform(attribute,{attribute, _, rule, Rule} = Attr, _Context, #state{ rules = Rs } = State) ->
    {Attr, false, State#state{ rules = [rule_name(Rule)|Rs] }};

do_transform(attribute,{attribute, _, rules, Rules0} = Attr, _Context, #state{ rules = Rs } = State) ->
    Rules = [ rule_name(R) || R <- Rules0 ],
    {Attr, false, State#state{ rules = Rules ++ Rs }};

do_transform(function, {function, _, Fun, Arity, _Cs} = Form, _Context, #state{ rules = Rules, rule_functions = RFuns } = State) ->
    case lists:member(Fun, Rules) of
        false ->
            {Form, true, State};
        true ->
            {Form, true, State#state{ rule_functions = [{Fun, Arity}|RFuns] -- State#state.exports }}
    end;


do_transform(_Type, Form, _Context, State) ->
    {Form, true, State}.

rule_name(A) when is_atom(A) ->
    A;
rule_name({A, _}) ->
    A.
