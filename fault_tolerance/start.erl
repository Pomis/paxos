-module(start).
-export([now/0]).

now() ->
    paxy:stop(),
    paxy:start([0,200,400]).
