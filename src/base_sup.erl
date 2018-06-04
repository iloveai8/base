-module(base_sup).
-behaviour(supervisor).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-export([start_link/0]).
-export([init/1]).


start_link()->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])->
	Childs = [	
		?CHILD(cfg, worker),
		?CHILD(rl, worker)
	],
	{ok, {{one_for_one, 10, 10}, Childs}}.

