%
% eres_ontology.erl
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
-module (eresye_ontology).

%%====================================================================
%% Include files
%%====================================================================
-include ("eres_ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================
-export ([compile/1, compile/2]).

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Function: compile/2
%% Description: Compiles an ontology file
%%====================================================================
compile (FileName, Options) ->
  {ok, AbstractErlangForm} = epp:parse_file (FileName ++ ".onto", "", []),
  %%io:format ("~w~n", [AbstractErlangForm]),
  {ok, Classes} = compile_lines ([], list_to_atom (FileName),
                                 AbstractErlangForm),

  %%io:format ("~p~n", [Classes]),
  NewClasses = resolve_inheritance (Classes),
  %%io:format ("~p~n", [NewClasses]),
  IsAHierarchy = generate_hierarchy_tree ([], Classes, Classes),
  %%io:format ("~p~n", [IsAHierarchy]),
  FatherOfHierarchy = reverse_hierarchy_tree ([], IsAHierarchy, IsAHierarchy),
  %%io:format ("~p~n", [FatherOfHierarchy]),

  IncludeLines = generate_include_file (NewClasses),
  {ok, IncludeFile} = file:open (FileName ++ ".hrl", [write]),
  io:format (IncludeFile, "~s", [IncludeLines]),
  file:close (IncludeFile),
  %%

  IsClassLines = generate_is_class ([], IsAHierarchy),
  IsALines = generate_is_a ([], IsAHierarchy),
  {CastClasses, CastLines} =
    generate_cast ({[],[]}, FatherOfHierarchy, NewClasses),
  ChildOfLines = generate_childof ([], FatherOfHierarchy),

  %%
  OntologyFileName = FileName,
  {ok, ConversionFile} = file:open (OntologyFileName ++ ".erl", [write]),
  io:format (ConversionFile, "-module (~s).~n", [OntologyFileName]),
  io:format (ConversionFile, "-include (\"~s.hrl\").~n",
             [FileName]),
  io:format (ConversionFile,
             "-export ([is_class/1, is_a/2, ~s childof/1]).~n~n",
             [lists:flatten (
                [io_lib:format ("'~s'/1,", [X]) || X <- CastClasses]
               )]),
  io:format (ConversionFile, "~s", [IsClassLines]),
  io:format (ConversionFile, "~s", [IsALines]),
  io:format (ConversionFile, "~s", [ChildOfLines]),
  io:format (ConversionFile, "~s", [CastLines]),
  file:close (ConversionFile),
  ok.

%%====================================================================
%% Function: compile/1
%% Description: Compiles an ontology file
%%====================================================================
compile (FileName) -> compile (FileName, []).

%%====================================================================
%% Internal functions
%%====================================================================
%%====================================================================
%% Func: compile_lines/3
%% Description: compile the lines of the ontology, given its erlang
%%              abstract form
%% Returns: {ok,  [#ontology_class]} |
%%          {error, Reason}
%%====================================================================
compile_lines (Accumulator, _, []) -> {ok,
                                       lists:flatten (
                                         lists:reverse (Accumulator))};
compile_lines (Accumulator, OntoName,
               [{function, _, class, _, Clauses} | Tail]) ->
  compile_lines ([compile_clauses ([], Clauses) |  Accumulator],
                 OntoName, Tail);
compile_lines (Accumulator, OntoName,
               [{attribute, _, ontology, OntoName} | Tail]) ->
  compile_lines (Accumulator, OntoName, Tail);
compile_lines (Accumulator, OntoName,
               [{attribute, Line, ontology, _} | Tail]) ->
  {error,{"ontology name does not match with filename in line", Line}};
compile_lines (Accumulator, OntoName, [{attribute, _, file, _} | Tail]) ->
  compile_lines (Accumulator, OntoName, Tail);
compile_lines (Accumulator, OntoName, [{eof, _} | Tail]) ->
  compile_lines (Accumulator, OntoName, Tail);
compile_lines (Accumulator, _, [{_, Line, _, _} | Tail]) ->
  {error,{"syntax error in line", Line}}.


%%====================================================================
%% Func: compile_clauses/2
%% Description: compile function clauses (classes) of the ontology,
%%              given its erlang abstract form
%% Returns: [#ontology_class]
%%====================================================================
compile_clauses (Acc, []) -> lists:reverse (Acc);
compile_clauses (Acc, [H|T]) ->
  compile_clauses ([compile_clause (H) | Acc], T).



%%====================================================================
%% Func: compile_clause/1
%% Description: compile a single function clause (classes) of the ontology,
%%              given its erlang abstract form
%% Returns: #ontology_class
%%====================================================================
compile_clause ({clause, LineNum,
                 [{atom, _, ClassName}], [], [{tuple, _, ClassDef}]}) ->
  #ontology_class { name = ClassName,
                    superclass = nil,
                    properties = compile_properties ([], ClassName, ClassDef)};
compile_clause ({clause, LineNum,
                 [{atom, _, ClassName}], [],
                 [{call,_, {atom, _, is_a}, [{atom, _, SuperClass}]}]}) ->
  #ontology_class { name = ClassName,
                    superclass = SuperClass,
                    properties = []};
compile_clause ({clause, LineNum,
                 [{atom, _, ClassName}], [],
                 [{call,_, {atom, _, is_a}, [{atom, _, SuperClass}]},
                  {tuple, _, ClassDef}]}) ->
  #ontology_class { name = ClassName,
                    superclass = SuperClass,
                    properties = compile_properties ([], ClassName, ClassDef)}.


%%====================================================================
%% Func: compile_properties/3
%% Description: compile the properties of an ontology class,
%%              given the erlang abstract form
%% Returns: [#ontology_property]
%%====================================================================
compile_properties (Acc, _, []) -> lists:reverse (Acc);
compile_properties (Acc, ClassName, [H|T]) ->
  compile_properties ([compile_property (ClassName, H) | Acc],
                      ClassName, T).

%%====================================================================
%% Func: compile_properties/2
%% Description: compile a single property of an ontology class,
%%              given the erlang abstract form
%% Returns: #ontology_property
%%====================================================================
compile_property (ClassName,
                  {match, _, {atom, _, FieldName}, FieldDef}) ->
  L = cons_to_erl_list (FieldDef),
  %%io:format ("~p~n", [L]),
  [FieldType, FieldRequirement, Default |_] = L,
  #ontology_property {name = FieldName,
                      type = FieldType,
                      requirement = FieldRequirement,
                      is_primitive = is_primitive (FieldType),
                      is_digit = is_digit (FieldName),
                      default = Default}.



%%====================================================================
%% Func: cons_to_erl_list/1
%% Description: transforms a "cons" abstract erlang construct to a list
%% Returns: [term()]
%%====================================================================
cons_to_erl_list ({cons, Line, OP1, OP2}) -> [cons_decode (OP1) |
                                              cons_to_erl_list (OP2)];
cons_to_erl_list (X) -> [cons_decode(X)].



%%====================================================================
%% Func: cons_decode/1
%% Description: decodes a single abstract erlang term
%% Returns: term()
%%====================================================================
cons_decode ({atom, _, nodefault}) -> ?NO_DEFAULT;
cons_decode ({atom, _, Option}) -> Option;
cons_decode ({nil, _}) -> nil;
cons_decode ({call, _, {atom, _, set_of}, [{atom,_,Type}]}) ->
  {set_of, Type};
cons_decode ({call, _, {atom, _, sequence_of}, [{atom,_,Type}]}) ->
  {sequence_of, Type};
cons_decode ({call,_, {atom, _, default}, [{atom, _, Value}]}) ->
  Value.
%cons_decode ({match,_, {atom, _, cardinality},
%              {tuple,_,[{_, _, Low},{_, _, High}]}}) ->
%  {cardinality, Low, High}.


%%====================================================================
%% Func: is_primitive/1
%% Description: checks if a type is primitive
%% Returns: true | false
%%====================================================================
is_primitive (string) -> true;
is_primitive (number) -> true;
is_primitive (integer) -> true;
is_primitive (boolean) -> true;
is_primitive (any) -> true;
is_primitive ({sequence_of, X}) -> is_primitive (X);
is_primitive ({set_of, X}) -> is_primitive (X);
is_primitive (_) -> false.



%%====================================================================
%% Func: is_digit/1
%% Description: checks if a slot name is a digit
%% Returns: true | false
%%====================================================================
is_digit ('0') -> true;
is_digit ('1') -> true;
is_digit ('2') -> true;
is_digit ('3') -> true;
is_digit ('4') -> true;
is_digit ('5') -> true;
is_digit ('6') -> true;
is_digit ('7') -> true;
is_digit ('8') -> true;
is_digit ('9') -> true;
is_digit (_) -> false.


%%====================================================================
%% Func: resolve_inheritance/1
%% Description: resolves the inheritances in the list of #ontology_class
%% Returns: [#ontology_class]
%%====================================================================
resolve_inheritance (Classes) ->
  case resolve_inheritance ([], Classes, Classes) of
    {false, NewClassList} -> resolve_inheritance (NewClassList);
    {true, NewClassList} -> NewClassList
  end.


%%====================================================================
%% Func: resolve_inheritance/3
%% Description: resolves the inheritances in the list of #ontology_class
%% Returns: {Solved, [#ontology_class]}
%%====================================================================
resolve_inheritance (Acc, _, []) -> {true, lists:reverse (Acc)};
resolve_inheritance (Acc, Classes,
                     [Class = #ontology_class { superclass = nil} | T]) ->
  resolve_inheritance ([Class | Acc], Classes, T);
resolve_inheritance (Acc, Classes, [Class | T]) ->
  SuperClass = get_class (Class#ontology_class.superclass, Classes),
  NewClass = Class#ontology_class {
               properties =
               lists:foldl (fun (X, Acc) ->
                                override_property ([], Acc, X)
                            end,
                            SuperClass#ontology_class.properties,
                            Class#ontology_class.properties),
               superclass = nil},
  {false, lists:reverse ([NewClass | Acc]) ++ T}.


override_property (Acc, [], _) -> lists:reverse (Acc);
override_property (Acc,
                   [P = #ontology_property { name = N} | T],
                   Property = #ontology_property {name = N}) ->
  override_property ([Property | Acc], T, Property);
override_property (Acc, [P | T], Property)  ->
  override_property ([P | Acc], T, Property).


%%====================================================================
%% Func: get_class/2
%% Description: Searches for a class in the list
%% Returns: #ontology_class
%%====================================================================
get_class (ClassName, []) -> nil;
get_class (ClassName, [Class = #ontology_class { name = ClassName } | T]) ->
  Class;
get_class (ClassName, [_ | T]) ->
  get_class (ClassName, T).



%%====================================================================
%% Func: generate_hierarchy_tree/3
%% Description: generates the tree of hierarchies
%% Returns: [{classname, [classname]}]
%%====================================================================
generate_hierarchy_tree (Acc, [], _) -> lists:reverse (Acc);
generate_hierarchy_tree (Acc, [Class | T], Classes) ->
  Item = {Class#ontology_class.name,
          ancestors_list ([], Class#ontology_class.superclass, Classes)},
  generate_hierarchy_tree ([Item | Acc], T, Classes).


ancestors_list (Acc, nil, Classes) -> lists:reverse (Acc);
ancestors_list (Acc, X, Classes) ->
  C = get_class (X, Classes),
  ancestors_list ([X | Acc], C#ontology_class.superclass, Classes).




reverse_hierarchy_tree (Acc, [], _) -> lists:reverse (Acc);
reverse_hierarchy_tree (Acc, [{Father, _} | T], Classes) ->
  Item = {Father, child_list (Father, Classes)},
  reverse_hierarchy_tree ([Item | Acc], T, Classes).

child_list (Father, Classes) ->
  [ C || {C, Ancestors} <- Classes, lists:member (Father, Ancestors)].


%%====================================================================
%% Func: generate_include_file/1
%% Description: generates the include file from a list of #ontology_class
%% Returns: [string()]
%%====================================================================
generate_include_file (Classes) -> generate_include_file ([], Classes).

generate_include_file (Acc, []) -> lists:flatten (lists:reverse (Acc));
generate_include_file (Acc, [Class | T]) ->
  Head = io_lib:format ("-record('~s',{~n", [Class#ontology_class.name]),
  Properties = generate_include_lines ([], Class#ontology_class.properties),
  Line = lists:flatten ([Head, Properties, "\n"]),
  generate_include_file ([Line | Acc], T).


%%====================================================================
%% Func: generate_include_lines/1
%% Description: generates the lines of properties for an include file
%% Returns: [string()]
%%====================================================================
generate_include_lines (Acc, []) ->
  Line = io_lib:format ("}).~n", []),
  lists:reverse ([Line | Acc]);
generate_include_lines (Acc, [Property = #ontology_property
                              { default = ?NO_DEFAULT }]) ->
  Line = io_lib:format ("  '~s'", [Property#ontology_property.name]),
  generate_include_lines ([Line | Acc], []);
generate_include_lines (Acc, [Property = #ontology_property
                              { default = ?NO_DEFAULT } | T]) ->
  Line = io_lib:format ("  '~s',~n", [Property#ontology_property.name]),
  generate_include_lines ([Line | Acc], T);
generate_include_lines (Acc, [Property]) ->
  Line = io_lib:format ("  '~s' = '~s'",
                        [Property#ontology_property.name,
                         Property#ontology_property.default]),
  generate_include_lines ([Line | Acc], []);
generate_include_lines (Acc, [Property | T]) ->
  Line = io_lib:format ("  '~s' = '~s',~n",
                        [Property#ontology_property.name,
                         Property#ontology_property.default]),
  generate_include_lines ([Line | Acc], T).




%%====================================================================
%% Func: generate_childof/2
%% Description: generates the lines for 'childof' functions
%% Returns: [string()]
%%====================================================================
generate_childof (Acc, []) ->
  lists:flatten (
    lists:reverse (["childof (_) -> exit (undef_class).\n\n" | Acc]));
generate_childof (Acc, [{FatherClassName, Children} | T]) ->
  Line = lists:flatten (
           io_lib:format ("childof ('~s') -> ~p;\n",
                          [FatherClassName, Children])),
  generate_childof ([Line | Acc], T).



%%====================================================================
%% Func: generate_is_a/2
%% Description: generates the lines for 'is_a' functions
%% Returns: [string()]
%%====================================================================
generate_is_a (Acc, []) ->
  lists:flatten (lists:reverse (["is_a (_,_) -> false.\n\n" | Acc]));
generate_is_a (Acc, [{ClassName, []} | T]) ->
  generate_is_a (Acc, T);
generate_is_a (Acc, [{ClassName, Ancestors} | T]) ->
  Line = [lists:flatten (
            io_lib:format ("is_a ('~s','~s') -> true;\n",
                           [ClassName, Ancestor]))
          || Ancestor <- Ancestors],
  generate_is_a ([Line | Acc], T).


%%====================================================================
%% Func: generate_is_class/1
%% Description: generates the lines for 'is_class' functions
%% Returns: boolean
%%====================================================================
generate_is_class (Acc, []) ->
  lists:flatten (lists:reverse (["is_class (_) -> false.\n\n" | Acc]));
generate_is_class (Acc, [{ClassName, _} | T]) ->
  Line = lists:flatten (
            io_lib:format ("is_class ('~s') -> true;\n",
                           [ClassName])),
  generate_is_class ([Line | Acc], T).


%%====================================================================
%% Func: generate_cast/2
%% Description: generates the lines for cast functions
%% Returns: [string()]
%%====================================================================
generate_cast ({Acc1, Acc2}, [], _) ->
  {lists:reverse (Acc1), lists:reverse (Acc2)};
generate_cast ({Acc1, Acc2}, [{ClassName, []} | T], ResolvedClasses) ->
  generate_cast ({Acc1, Acc2}, T, ResolvedClasses);
generate_cast ({Acc1, Acc2}, [{ClassName, Children} | T],
               ResolvedClasses) ->

  Lines = lists:flatten (
            lists:map (
              fun (X) ->
                  DestinationClass = get_class (ClassName, ResolvedClasses),
                  SourceClass = get_class (X, ResolvedClasses),
                  generate_translation_lines (SourceClass,
                                              DestinationClass)
              end, Children)),
  [CR, CR, _ | ReversedList] = lists:reverse (Lines),
  %%replace last semicolon with a dot to end the clause
  NewLines = lists:reverse ([CR, CR, $. | ReversedList]),
  generate_cast ({[ClassName | Acc1] , [NewLines | Acc2]}, T, ResolvedClasses).



generate_translation_lines (SourceClass, DestinationClass) ->
  Lines =
    [ lists:flatten (io_lib:format ("    '~s' = X#'~s'.'~s'",
                                    [X#ontology_property.name,
                                     SourceClass#ontology_class.name,
                                     X#ontology_property.name]))
      || X <- DestinationClass#ontology_class.properties],
  %%io:format ("~p~n", [Lines]),
  XLines = lists:foldl (fun (X, Sum) ->
                            lists:concat ([Sum, ",\n", X])
                        end,
                        "",
                        Lines),
  %%io:format ("~p~n", [XLines]),
  [_,_ | YLines] = XLines,
  Head = lists:flatten (
           io_lib:format ("'~s' (X = #'~s'{}) ->\n  #'~s'{\n",
                          [DestinationClass#ontology_class.name,
                           SourceClass#ontology_class.name,
                           DestinationClass#ontology_class.name])),
  lists:concat ([Head ,
                 lists:flatten (YLines),
                "};\n\n"]).



