%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresye_ontology_resolver).

-export([resolve_ontology/2]).

resolve_ontology(AbstractErlangForm, Ontology) ->
    generate_clauses([], AbstractErlangForm, Ontology).


generate_clauses(Acc, [], _) -> Acc;
generate_clauses(Acc,
                 [{clause, Line, [], Guard, Body} | T], Ontology) ->
    generate_clauses([{clause, Line, [], Guard, Body}
                      | Acc],
                     T, Ontology);
generate_clauses(Acc,
                 [{clause, Line, Head, Guard, Body} | T], Ontology) ->
    Solved = substitute(Head,
                        fun ({match, _, {record, _, C, _}, _}) ->
                                Ontology:is_class(C);
                            (_) -> false
                        end,
                        fun ({X1, X2, {record, X3, C, X4}, X5}) ->
                                [{X1, X2, {record, X3, X, X4}, X5}
                                 || X <- [C | Ontology:childof(C)]]
                        end),
    NewClauses = [{clause, Line, X, Guard, Body}
                  || X <- Solved],
    generate_clauses(NewClauses ++ Acc, T, Ontology).

indexOf(Acc, [], _, _) -> lists:reverse(Acc);
indexOf(Acc, [H | T], Pred, New) ->
    case Pred(H) of
        true -> indexOf([New(H) | Acc], T, Pred, New);
        _ -> indexOf([[H] | Acc], T, Pred, New)
    end.

indexOf(Data, Pred, New) ->
    indexOf([], Data, Pred, New).

substitute(L, Pred, New) ->
    Lists = indexOf(L, Pred, New),
    Arg = tl(lists:foldr(fun (X, Sum) ->
                                 lists:flatten(io_lib:format(",A~w", [X])) ++
                                     Sum
                         end,
                         "", lists:seq(1, length(L)))),
    StringLists = tl(lists:foldr(fun (X, Sum) ->
                                         lists:flatten(io_lib:format(",A~w<-~p",
                                                                     [X,
                                                                      lists:nth(X,
                                                                                Lists)]))
                                             ++ Sum
                                 end,
                                 "", lists:seq(1, length(L)))),
    String = "[ [" ++ Arg ++ "] || " ++ StringLists ++ "].",
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Expr} = erl_parse:parse_exprs(Tokens),
    case catch erl_eval:exprs(Expr, erl_eval:new_bindings())
    of
        {'EXIT', _} -> false;
        {value, Value, _Bindings} -> Value;
        _ -> false
    end.
