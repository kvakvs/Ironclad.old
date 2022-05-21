-module(test_core_case).

-export([myfun/1]).

myfun({X, 1}) -> X.
