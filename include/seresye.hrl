%% Parse transforms for automatic creating negative specs for rules in
%% a rules file. though the user can write them in a spec it is much
%% nicer to write them in a when clause and let the transform do its
%% work.
-compile({parse_transform, seresye_autoneg}).
-compile({parse_transform, seresye_transform}).
