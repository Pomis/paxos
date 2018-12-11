-module(start).
-export([main/1]).

main(_) ->
    paxy:stop(),
    cover:compile_directory(),
    paxy:start([643,1000,130]).