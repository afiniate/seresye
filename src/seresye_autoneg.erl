%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2011, Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_autoneg).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    {Head, NewAttributes, Body} =
        lists:foldl(fun parse_forms/2, {[], [], []}, Forms),
    lists:reverse(Head) ++ NewAttributes ++ lists:reverse(Body).

parse_forms({function,Line,FunName, Arity, Clauses0}, {Head, Attrs0, Body}) ->
    {_, Clauses2, Attrs2} =
        lists:foldl(fun(Clause0, {ClauseCount, Clauses1, Attrs1}) ->
                            case parse_clause(Clause0) of
                                {false, Clause1} ->
                                    {ClauseCount + 1,
                                     [Clause1 | Clauses1],
                                     Attrs1};
                                {NewDetail, Clause1} ->
                                    {ClauseCount + 1,
                                     [Clause1 | Clauses1],
                                     [{ClauseCount, NewDetail} | Attrs1]}
                            end
                end, {0, [], []}, Clauses0),

    {Head, lists:map(fun({CC, Detail0}) ->
                             {attribute, Line, rule_neg, {FunName, CC, Detail0}}
                     end, Attrs2) ++ Attrs0,
     [{function, Line, FunName, Arity, lists:reverse(Clauses2)} | Body]};
parse_forms(F = {attribute,_,file,_}, {Head, Attrs0, Body}) ->
    {[F | Head], Attrs0, Body};
parse_forms(F = {attribute,_,module,_}, {Head, Attrs, Body}) ->
    {[F | Head], Attrs, Body};
parse_forms(El, {Head, Attrs, Body}) ->
    {Head, Attrs, [El |Body]}.

parse_clause({clause,Line, Args,
              [[{op,_,'not',
                 {tuple, _,
                  [{atom,_,rule}, Neg]}} | AR] | OR],
              Body}) ->
    NewGuards = case AR of
                    [] ->
                        OR;
                    _ ->
                        [AR | OR]
                end,
    {rewrite_negs(Neg), {clause, Line, Args, NewGuards, Body}};
parse_clause(Clause) ->
    {false, Clause}.

rewrite_negs({tuple, L, Elements}) ->
    {tuple, L, lists:map(fun rewrite_negs/1, Elements)};
rewrite_negs(C = {cons, _, _, _}) ->
    rewrite_cons(C);
rewrite_negs({var, Line, '_'}) ->
    {atom, Line, '___IGNORE___'};
rewrite_negs(Else) ->
    Else.

rewrite_cons({cons, Line, Element, nil}) ->
    {cons, Line, rewrite_negs(Element), nil};
rewrite_cons({cons, Line, E1, Rest}) ->
    {cons, Line, rewrite_negs(E1), rewrite_negs(Rest)}.
