-module (journal).
-include ("journal.hrl").
-export ([is_class/1, is_a/2, 'journal'/1,'international-journal'/1,'national-journal'/1, childof/1]).

is_class ('journal') -> true;
is_class ('international-journal') -> true;
is_class ('national-journal') -> true;
is_class ('IEEE-journal') -> true;
is_class ('ACM-journal') -> true;
is_class ('italian-journal') -> true;
is_class (_) -> false.

is_a ('international-journal','journal') -> true;
is_a ('national-journal','journal') -> true;
is_a ('IEEE-journal','international-journal') -> true;
is_a ('IEEE-journal','journal') -> true;
is_a ('ACM-journal','international-journal') -> true;
is_a ('ACM-journal','journal') -> true;
is_a ('italian-journal','national-journal') -> true;
is_a ('italian-journal','journal') -> true;
is_a (_,_) -> false.

childof ('journal') -> ['international-journal',
                        'national-journal',
                        'IEEE-journal',
                        'ACM-journal',
                        'italian-journal'];
childof ('international-journal') -> ['IEEE-journal','ACM-journal'];
childof ('national-journal') -> ['italian-journal'];
childof ('IEEE-journal') -> [];
childof ('ACM-journal') -> [];
childof ('italian-journal') -> [];
childof (_) -> exit (undef_class).

'journal' (X = #'international-journal'{}) ->
  #'journal'{
    'title' = X#'international-journal'.'title',
    'main_topic' = X#'international-journal'.'main_topic',
    'publisher' = X#'international-journal'.'publisher',
    'type' = X#'international-journal'.'type',
    'impact_factor' = X#'international-journal'.'impact_factor'};

'journal' (X = #'national-journal'{}) ->
  #'journal'{
    'title' = X#'national-journal'.'title',
    'main_topic' = X#'national-journal'.'main_topic',
    'publisher' = X#'national-journal'.'publisher',
    'type' = X#'national-journal'.'type',
    'impact_factor' = X#'national-journal'.'impact_factor'};

'journal' (X = #'IEEE-journal'{}) ->
  #'journal'{
    'title' = X#'IEEE-journal'.'title',
    'main_topic' = X#'IEEE-journal'.'main_topic',
    'publisher' = X#'IEEE-journal'.'publisher',
    'type' = X#'IEEE-journal'.'type',
    'impact_factor' = X#'IEEE-journal'.'impact_factor'};

'journal' (X = #'ACM-journal'{}) ->
  #'journal'{
    'title' = X#'ACM-journal'.'title',
    'main_topic' = X#'ACM-journal'.'main_topic',
    'publisher' = X#'ACM-journal'.'publisher',
    'type' = X#'ACM-journal'.'type',
    'impact_factor' = X#'ACM-journal'.'impact_factor'};

'journal' (X = #'italian-journal'{}) ->
  #'journal'{
    'title' = X#'italian-journal'.'title',
    'main_topic' = X#'italian-journal'.'main_topic',
    'publisher' = X#'italian-journal'.'publisher',
    'type' = X#'italian-journal'.'type',
    'impact_factor' = X#'italian-journal'.'impact_factor'}.

'international-journal' (X = #'IEEE-journal'{}) ->
  #'international-journal'{
    'title' = X#'IEEE-journal'.'title',
    'main_topic' = X#'IEEE-journal'.'main_topic',
    'publisher' = X#'IEEE-journal'.'publisher',
    'type' = X#'IEEE-journal'.'type',
    'impact_factor' = X#'IEEE-journal'.'impact_factor'};

'international-journal' (X = #'ACM-journal'{}) ->
  #'international-journal'{
    'title' = X#'ACM-journal'.'title',
    'main_topic' = X#'ACM-journal'.'main_topic',
    'publisher' = X#'ACM-journal'.'publisher',
    'type' = X#'ACM-journal'.'type',
    'impact_factor' = X#'ACM-journal'.'impact_factor'}.

'national-journal' (X = #'italian-journal'{}) ->
  #'national-journal'{
    'title' = X#'italian-journal'.'title',
    'main_topic' = X#'italian-journal'.'main_topic',
    'publisher' = X#'italian-journal'.'publisher',
    'type' = X#'italian-journal'.'type',
    'impact_factor' = X#'italian-journal'.'impact_factor'}.

