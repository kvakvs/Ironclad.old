-module(fgsfds).

% test comment
-define(AAA, 1).
-ifdef(AAA).
THIS IS GOOD
-else.
THIS IS BAD 1
-endif.

-ifndef(AAA).
THIS IS BAD 2
-endif().

fn() -> ok.
