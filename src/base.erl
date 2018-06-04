-module(base).
-export([start/0, stop/0]).

start()->
	case application:start(base) of
        ok -> ok;
        {error, {already_started, base}} -> ok
    end.

stop() ->
    application:stop(base),
    erlang:halt().