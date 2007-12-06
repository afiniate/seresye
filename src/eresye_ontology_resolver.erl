%
% eresye_ontology_resolver.erl
%
% ----------------------------------------------------------------------
%
%  ERESYE, an ERlang Expert SYstem Engine
%  Copyright (C) 2005-07 Francesca Gangemi (francesca@erlang-consulting.com)
%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
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

