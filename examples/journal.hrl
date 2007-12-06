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

