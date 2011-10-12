%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresyee_wine).

-include("eresyee_wine.hrl").

-export([childof/1, is_a/2, is_class/1, redwine/1,
         wine/1]).

is_class(wine_grape) -> true;
is_class(wine) -> true;
is_class(redwine) -> true;
is_class(whitewine) -> true;
is_class('Chianti') -> true;
is_class(_) -> false.

is_a(redwine, wine) -> true;
is_a(whitewine, wine) -> true;
is_a('Chianti', redwine) -> true;
is_a('Chianti', wine) -> true;
is_a(_, _) -> false.

childof(wine_grape) -> [];
childof(wine) -> [redwine, whitewine, 'Chianti'];
childof(redwine) -> ['Chianti'];
childof(whitewine) -> [];
childof('Chianti') -> [];
childof(_) -> exit(undef_class).

wine(X = #redwine{}) ->
    #wine{name = X#redwine.name,
          body = X#redwine.body,
          color = X#redwine.color,
          flavor = X#redwine.flavor,
          grape = X#redwine.grape,
          sugar = X#redwine.sugar};
wine(X = #whitewine{}) ->
    #wine{name = X#whitewine.name,
          body = X#whitewine.body,
          color = X#whitewine.color,
          flavor = X#whitewine.flavor,
          grape = X#whitewine.grape,
          sugar = X#whitewine.sugar};
wine(X = #'Chianti'{}) ->
    #wine{name = X#'Chianti'.name,
          body = X#'Chianti'.body,
          color = X#'Chianti'.color,
          flavor = X#'Chianti'.flavor,
          grape = X#'Chianti'.grape,
          sugar = X#'Chianti'.sugar}.

redwine(X = #'Chianti'{}) ->
    #redwine{name = X#'Chianti'.name,
             body = X#'Chianti'.body,
             color = X#'Chianti'.color,
             flavor = X#'Chianti'.flavor,
             grape = X#'Chianti'.grape,
             sugar = X#'Chianti'.sugar}.
