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

