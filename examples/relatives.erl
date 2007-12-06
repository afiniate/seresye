%
% relatives.erl
%
% -------------------------------------------------------------------------
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
