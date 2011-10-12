%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php%
-record('journal',{
  'title',
  'main_topic',
  'publisher',
  'type',
  'impact_factor'}).

-record('international-journal',{
  'title',
  'main_topic',
  'publisher',
  'type' = 'international',
  'impact_factor'}).

-record('national-journal',{
  'title',
  'main_topic',
  'publisher',
  'type' = 'national',
  'impact_factor'}).

-record('IEEE-journal',{
  'title',
  'main_topic',
  'publisher' = 'IEEE',
  'type' = 'international',
  'impact_factor'}).

-record('ACM-journal',{
  'title',
  'main_topic',
  'publisher' = 'ACM',
  'type' = 'international',
  'impact_factor'}).

-record('italian-journal',{
  'title',
  'main_topic',
  'publisher',
  'type' = 'national',
  'impact_factor'}).

