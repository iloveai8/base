-module(base_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	start_deps_apps(),
    base_sup:start_link().

stop(_State) ->
    ok.

start_deps_apps()->
    % inets:start(),
    % ssl:start(),
    log:init(),
    application:start(crypto),
    ok.    