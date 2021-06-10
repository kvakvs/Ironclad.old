-module(fgsfds).
% test comment
-else.
-author("Dmytro").
%% API
-export([]).
-define(HELLO
).
-ifdef(HELLO).
-error("ERROR").
-endif().
