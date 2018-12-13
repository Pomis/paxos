-module(start).
-export([main/0]).

main() ->
    paxy:stop(),
    cover:compile_directory(),
    paxy:start([256,189,523]).
