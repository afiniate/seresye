%
% eresye_ontology_resolver.erl
%
% ----------------------------------------------------------------------
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

-module (eresye_ontology_resolver).
-export ([resolve_ontology/2]).


resolve_ontology (AbstractErlangForm, Ontology) ->
  generate_clauses ([], AbstractErlangForm, Ontology).
%%  iterate ([], AbstractErlangForm, Ontology).

iterate (Acc, [], _) -> lists:reverse (Acc);
iterate (Acc, [{function, Line, Name, Arity, Def} | T], Ontology) ->
  Clauses = generate_clauses ([], Def, Ontology),
  iterate ([{function, Line, Name, Arity, Clauses} | Acc], T, Ontology);
iterate (Acc, [H | T], Ontology) ->
  iterate ([H | Acc], T, Ontology).


generate_clauses (Acc, [], _) -> Acc;
generate_clauses (Acc, [{clause, Line, [], Guard, Body} | T], Ontology) ->
  generate_clauses ([{clause, Line, [], Guard, Body} | Acc], T, Ontology);
generate_clauses (Acc, [{clause, Line, Head, Guard, Body} | T], Ontology) ->
  %%io:format ("~p~n", [Head]),
  Solved = substitute (Head,
                       fun ({match, _, {record, _, C, _}, _}) ->
                           Ontology:is_class (C);
                           (_) -> false end,
                       fun ({X1, X2, {record, X3, C, X4}, X5}) ->
                           [ {X1, X2, {record, X3, X, X4}, X5}
                             || X <- [C | Ontology:childof (C)]]
                       end),
  NewClauses = [ {clause, Line, X, Guard, Body} || X <- Solved],
  generate_clauses (NewClauses ++ Acc, T, Ontology).



indexOf (Acc, [], _, _) -> lists:reverse (Acc);
indexOf (Acc, [H | T], Pred, New) ->
  case Pred (H) of
    true -> indexOf ([New (H) | Acc], T, Pred, New);
    _ -> indexOf ([[H] | Acc], T, Pred, New)
  end.

indexOf (Data, Pred, New) -> indexOf ([], Data, Pred, New).


substitute (L, Pred, New) ->
  Lists = indexOf(L, Pred, New),
  Arg = tl(lists:foldr (fun (X,Sum) ->
                            lists:flatten(io_lib:format (",A~w", [X])) ++ Sum
                        end,
                        "",
                        lists:seq (1,length (L)))),
  StringLists  = tl(lists:foldr (
                      fun (X,Sum) ->
                          lists:flatten(
                            io_lib:format (",A~w<-~p",
                                           [X, lists:nth (X, Lists)])) ++ Sum
                      end, "",
                      lists:seq (1,length (L)))),
  String = "[ [" ++ Arg ++ "] || " ++ StringLists ++ "].",
  {ok, Tokens, _} = erl_scan:string (String),
  {ok, Expr} = erl_parse:parse_exprs (Tokens),
  case catch (erl_eval:exprs (Expr, erl_eval:new_bindings ())) of
    {'EXIT', _} -> false;
    {value, Value, Bindings} -> Value;
    _ -> false
  end.

