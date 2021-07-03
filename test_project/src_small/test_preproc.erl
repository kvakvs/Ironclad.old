-module(fgsfds).
-include("some/path/to/include.hrl").

% test comment
-define(AAA, 1).
-ifdef(AAA).
-warning("THIS IS GOOD").
-else.
-warning("THIS IS BAD 1").
-endif.

-ifndef(AAA).
-error("THIS IS BAD 2").
-endif().

fn() -> ok.
