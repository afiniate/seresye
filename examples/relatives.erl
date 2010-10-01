%
% relatives.erl
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
-module (relatives).
-compile ([export_all]).

%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother (Engine, {female, X}, {parent, X, Y}) ->
  eresye:assert (Engine, {mother, X, Y}).


%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father (Engine, {male, X}, {parent, X, Y}) ->
  eresye:assert (Engine, {father, X, Y}).



%%
%% if (Y and Z have the same parent X) and (Z is female)
%%    then (Z is Y's sister)
%%
sister (Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
  eresye:assert (Engine, {sister, Z, Y}).



%%
%% if (Y and Z have the same parent X) and (Z is male)
%%    then (Z is Y's brother)
%%
brother (Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
  eresye:assert (Engine, {brother, Z, Y}).


%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
  eresye:assert (Engine, {grandfather, X, Z}).


%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother (Engine, {mother, X, Y}, {parent, Y, Z}) ->
  eresye:assert (Engine, {grandmother, X, Z}).



start () ->
  eresye:start (relatives),
  lists:foreach (fun (X) ->
                     eresye:add_rule (relatives, {?MODULE, X})
                 end,
                 [mother, father,
                  brother, sister,
                  grandfather, grandmother]),

  eresye:assert (relatives,
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
