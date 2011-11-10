---
layout: default
title: Swarm oriented ERlang Expert SYstem Engine
---

Introduction
------------

SERESYE is a Rete based rules engine written in Erlang, descended
directly from the Eresye project by Francesca Gangemi and Corrado
Santoro. In the following article we will describe how to use the
system.

A rule-based system is composed of a **knowledge base**, which stores
a set of *facts* representing the 'universe of discourse' of a given
application, and a set of **production rules**, which are used to
infer knowledge and/or reason about the knowledge. A rule is activated
when one or more facts match the template(s) given in the rule
declaration: in such a case, the body of the rule contains a code that
is thus executed

In SERESYE, *facts* are expressed by means of Erlang tuples or records,
while rules are written using standard Erlang function clauses, whose
declaration reports, in the clause head, the facts or fact templates
that have to be matched for the rule to be activated and executed.

For more information about SERESYE please refer to the paper docs directory.

For more information about rule-based inference engines and expert
systems, you can refer to the book: *S. Russell and
P. Norvig. **Artificial Intelligence: A Modern Approach/2E.** Prentice
Hall, 2003.*

To write an AI application with SERESYE the following steps have to be
performed:

1. Identify your universe of discourse and determine the facts that
   have to be used to represent such a world;

2. Identify the rules that you need and write them by using, e.g.
   first-order-logic predicates or even natural language;

3. Implement the system by writing your rules as Erlang function
   clauses, according to the modality required by SERESYE.


The Application: the Domain of Relatives
----------------------------------------

We will design a system able to derive new knowledge using some
inference rules and starting from a small set; as a sample
application, we chose the domain of relatives: we will start from some
base concepts, such as *parent*, *male* and *female*, and then, by
means of a proper set of rules, we will derive the concepts of
*mother*, *father*, *sister*, *brother*, *grandmother* and
*grandfather*.

According to the list above, we will first derive the facts that will be
used to represent our concepts. Given the set of relationships above, they
will be represented by means of the following facts:

<table border="1" align="center">
<thead>
  <tr>
    <td><p>#</p></td>
    <td><p>Concept</p></td>
    <td><p>Fact / Erlang tuple</p></td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><p>1</p></td>
    <td><p>X is male</p></td>
    <td><p><tt>{male, X}</tt></p></td>
  </tr>
  <tr>
    <td><p>2</p></td>
    <td><p>X is female</p></td>
    <td><p><tt>{female, X}</tt></p></td>
  </tr>
  <tr>
    <td><p>3</p></td>
    <td><p>X is Y's parent</p></td>
    <td><p><tt>{parent, X, Y}</tt></p></td>
  </tr>
  <tr>
    <td><p>4</p></td>
    <td><p>X is Y's mother</p></td>
    <td><p><tt>{mother, X, Y}</tt></p></td>
  </tr>
  <tr>
    <td><p>5</p></td>
    <td><p>X is Y's father</p></td>
    <td><p><tt>{father, X, Y}</tt></p></td>
  </tr>
  <tr>
    <td><p>6</p></td>
    <td><p>X is Y's sister</p></td>
    <td><p><tt>{sister, X, Y}</tt></p></td>
  </tr>
  <tr>
    <td><p>7</p></td>
    <td><p>X is Y's brother</p></td>
    <td><p><tt>{brother, X, Y}</tt></p></td>
  </tr>
  <tr>
    <td><p>8</p></td>
    <td><p>X is Y's grandmother</p></td>
    <td><p><tt>{grandmother, X, Y}</tt></p></td>
  </tr>
  <tr>
    <td><p>9</p></td>
    <td><p>X is Y's grandfather</p></td>
    <td><p><tt>{grandfather, X, Y}</tt></p></td>
  </tr>
</tbody>
</table>

Concepts 1, 2 and 3 will be used as a base to derive the other ones.

Deriving new concepts by means of rules
---------------------------------------

#### Concept: mother

The rule to derive the concept of mother is quite
straightforward:

    if X is female and X is Y's parent then X is Y's mother.

From the point of view of SERESYE, since knowledge is stored in the
*knowledge base* of the engine, the rule above is translated into the
following one: *if the facts {female, X} and {parent, X, Y} are
*asserted* in the knowledge base, then we assert the fact {mother, X,
Y}.

The rule *mother* can be thus written as follows:

    %%
    %% if (X is female) and (X is Y's parent) then (X is Y's mother)
    %%
    mother (Engine, {female, X}, {parent, X, Y}) ->
      seresye:assert (Engine, {mother, X, Y}).


#### Concept: father

This concept can be easily derived by means of the following rule:

    %%
    %% if (X is male) and (X is Y's parent) then (X is Y's father)
    %%
    father (Engine, {male, X}, {parent, X, Y}) ->
      seresye:assert (Engine, {father, X, Y}).


#### Concept: sister

 This concept can be expressed by the following rule:

    if Y and Z have the same parent and Z is female, then Z
    is the Y's sister.

The SERESYE rule used to map this concept is:

    %%
    %% if (Y and Z have the same parent X) and (Z is female)
    %%    then (Z is Y's sister)
    %%
    sister (Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
      seresye:assert (Engine, {sister, Z, Y}).


Please note the guard, which is needed to ensure that when Y and Z are
bound to the same value, the rule is not activated (indeed this is
possible since the same fact can match both the first and second
'parent' pattern).

#### Concept: brother

Given the previous one, this concept is now quite simple to
implement:


    %%
    %% if (Y and Z have the same parent X) and (Z is male)
    %%    then (Z is Y's brother)
    %%
    brother (Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
      seresye:assert (Engine, {brother, Z, Y}).


#### Concepts: grandmother and grandfather

The former concept can be expressed by means of the rule:

    if X is Y's mother and Y is Z's parent, then X is Z's
    grandmother.</u>* The latter concept is now obvious.

Both can be implemented using the following SERESYE rules:

    %%
    %% if (X is Y's mother) and (Y is Z's parent)
    %%    then (X is Z's grandmother)
    %%
    grandmother (Engine, {mother, X, Y}, {parent, Y, Z}) ->
      seresye:assert (Engine, {grandmother, X, Z}).

    %%
    %% if (X is Y's father) and (Y is Z's parent)
    %%    then (X is Z's grandfather)
    %%
    grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
      seresye:assert (Engine, {grandfather, X, Z}).


Instantiating the Engine and Populating the Knowledge Base
----------------------------------------------------------

After writing the rules, we need to:

- define the rules to seresye
- instantiate the engine;
- populate the knowledge base with a set of initial facts.

We define the rules to SERESYE by defined a 'rules' attribute at the
start of the module.

    %%%
    %%% relatives.erl
    %%%
    -module (relatives).

    -export([father/3, grandfather/3, grandmother/3,
             mother/3, brother/4, sister/4, start/0]).

    -rules([mother, father, brother, sister, grandfather,
            grandmother]).

We continue on to instantiate the engine and popoulate the knowledge
base in the function *start* below:

    start () ->
      application:start(seresye) % Only if it is not already started
      seresye:start(relatives),
      seresye:add_rules(relatives, ?MODULE)

      seresye:assert(relatives,
                     [{male, bob}, {male, corrado}, {male, mark}, {male, caesar},
                      {female, alice}, {female, sara}, {female, jane}, {female, anna},
                      {parent, jane, bob}, {parent, corrado, bob},
                      {parent, jane, mark}, {parent, corrado, mark},
                      {parent, jane, alice}, {parent, corrado, alice},
                      {parent, bob, caesar}, {parent, bob, anna},
                      {parent, sara, casear}, {parent, sara, anna}]),
      ok.

As the listing reports, creating a new SERESYE engine implies to call
the function *seresye:start/1*, giving the name of the engine to be
created

Then, we have to add the rules to the engine by using the function
*seresye:add_rule/2*: it takes two arguments, the name of the engine
and a tuple representing the function in the form *{Module,
FuncName}*; obviously the function *Module:FuncName* must be
exported. Function *add_rule* has to be called for each rule that has
to be added; for this reason, the code above has an iteration over the
list of rules written before.

Finally, we populate the inference engine with a set of sample facts
by giving them, in a list, to the function *seresye:assert/2*.  To test
our rules, we considered the relationships in the Figure below and
assert only the facts for *male*, *female* and *parent*.

Testing the application
-----------------------

The final complete code of our AI application is thus the following:


    %%%
    %%% relatives.erl
    %%%
    -module (relatives).
    -export([father/3, grandfather/3, grandmother/3,
             mother/3, brother/4, sister/4, start/0]).

    %%
    %% if (X is female) and (X is Y's parent) then (X is Y's mother)
    %%
    mother(Engine, {female, X}, {parent, X, Y}) ->
      seresye:assert(Engine, {mother, X, Y}).

    %%
    %% if (X is male) and (X is Y's parent) then (X is Y's father)
    %%
    father(Engine, {male, X}, {parent, X, Y}) ->
      seresye:assert(Engine, {father, X, Y}).

    %%
    %% if (Y and Z have the same parent X) and (Z is female)
    %%    then (Z is Y's sister)
    %%
    sister(Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
      seresye:assert(Engine, {sister, Z, Y}).

    %%
    %% if (Y and Z have the same parent X) and (Z is male)
    %%    then (Z is Y's brother)
    %%
    brother(Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
      seresye:assert(Engine, {brother, Z, Y}).

    %%
    %% if (X is Y's father) and (Y is Z's parent)
    %%    then (X is Z's grandfather)
    %%
    grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
      seresye:assert (Engine, {grandfather, X, Z}).

    %%
    %% if (X is Y's mother) and (Y is Z's parent)
    %%    then (X is Z's grandmother)
    %%
    grandmother(Engine, {mother, X, Y}, {parent, Y, Z}) ->
      seresye:assert(Engine, {grandmother, X, Z}).

    start () ->
      application:start(seresye),
      seresye:start (relatives),
      seresye:add_rules(relatives, ?MODULE)

      seresye:assert (relatives,
                     [{male, bob},
                      {male, corrado},
                      {male, mark},
                      {male, caesar},
                      {female, alice},
                      {female, sara},
                      {female, jane},
                      {female, anna},
                      {parent, jane, bob},
                      {parent, corrado, bob},
                      {parent, jane, mark},
                      {parent, corrado, mark},
                      {parent, jane, alice},
                      {parent, corrado, alice},
                      {parent, bob, caesar},
                      {parent, bob, anna},
                      {parent, sara, casear},
                      {parent, sara, anna}]),
      ok.

Now it's time to test our application:


    Erlang (BEAM) emulator version 5.5 [source] [async-threads:0] [hipe]

    Eshell V5.5  (abort with ^G)
    1> c(relatives).
    {ok,relatives}
    2> relatives:start().
    ok
    3>

Following the call to function *relatives:start/0*, the engine is
created and populated; if no errors occurred, the rules should have
been processed and the new facts derived. To check this, we can use
the function *seresye:get_kb/1*, which returns the list of facts
asserted into the knowledge base of a given engine:


    4> seresye:get_kb(relatives).
    [{brother,bob,mark},
     {sister,alice,bob},
     {sister,alice,mark},
     {brother,bob,alice},
     {brother,mark,alice},
     {grandmother,jane,caesar},
     {grandfather,corrado,caesar},
     {grandmother,jane,anna},
     {grandfather,corrado,anna},
     {sister,anna,caesar},
     {brother,caesar,anna},
     {sister,anna,casear},
     {mother,sara,anna},
     {mother,sara,casear},
     {parent,sara,anna},
     {father,bob,anna},
     {parent,sara,casear},
     {father,bob,caesar},
     {parent,bob,anna},
     {father,corrado,alice},
     {parent,bob,caesar},
     {mother,jane,alice},
     {parent,corrado,alice},
     {father,corrado,mark},
     {parent,jane,alice},
     {mother,jane,mark},
     {parent,corrado|...},
     {brother|...},
     {...}|...]
    5>

The presence of facts representing concepts like *father*, *sister*,
etc., proves that the rules seems to be working as expected.

We can however query the knowledge base using specific fact templates.
For example, if we want to know who are Alice's brothers, we can use
the function *seresye:query_kb/2* as follows:


    6> seresye:query_kb(relatives, {brother, '_', alice}).
    [{brother,bob,alice},{brother,mark,alice}]
    7>

The facts returned conform to the relationships depicted in the figure
above, thus proving that the rules written are really working.

As the example shows, function *seresye:query_kb/2* takes the engine
name as the first argument, while, for the second parameter, a tuple
has to be specified, representing the fact template to be matched; in
such a tuple, the atom *'_'* plays the role of a wildcard. However, to
specify a more complex matching, a *fun* can be used as a tuple
element; this *fun* has to return a boolean value which indicates if
the element matches the template. For example, to select both Alice's
and Anna's brothers, we can use the following function call:


    7> seresye:query_kb(relatives, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
    [{brother,bob,alice},{brother,mark,alice},{brother,caesar,anna}]
    8>


Conclusions
-----------

This HowTo not only shows how to use the SERESYE engine to write an AI
application, but also highlights the versatility of the Erlang language:
the characteristics of functional and symbolic programming, together with
the possibility of performing *introspection* of function declaration,
can be successfully exploited for application domains which are completely
new for Erlang but can surely be very interesting.
