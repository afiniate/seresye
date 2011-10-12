%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-record('wine_grape',{
  'name'}).

-record('wine',{
  'name',
  'body',
  'color',
  'flavor',
  'grape',
  'sugar'}).

-record('redwine',{
  'name',
  'body',
  'color' = 'red',
  'flavor',
  'grape',
  'sugar'}).

-record('whitewine',{
  'name',
  'body',
  'color' = 'white',
  'flavor',
  'grape',
  'sugar'}).

-record('Chianti',{
  'name',
  'body',
  'color' = 'red',
  'flavor',
  'grape',
  'sugar' = 'dry'}).

