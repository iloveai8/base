-module(mu).
-export([init_behaviour/3, ensure_mem_module/2, sprintf/2]).

-callback get_modules() -> Modules::list().

init_behaviour(Behaviour, Module, AppendString)->
	BehaviourModules = get_behaviour_modules(Behaviour),
	HeaderString = sprintf("-module(~s).\n-behaviour(~s).\n-export([get_modules/0]).\nget_modules()->~p.\n", [Module, ?MODULE, BehaviourModules]),
	ensure_mem_module(Module, HeaderString ++ AppendString).

get_behaviour_modules(Behaviour)->
	SysPath = code:lib_dir(),
	UserPath = lists:filter(fun(D)-> string:str(D, SysPath) == 0 end, code:get_path()),
	ModFiles = lists:flatmap(fun(F)-> filelib:wildcard(filename:absname_join(F, "*.beam")) end, UserPath),
	lists:filtermap(
		fun(F)-> 
			Mod = list_to_atom(filename:basename(F, ".beam")),
			case is_behaviour(Mod, Behaviour) of
				true -> {true, Mod};
				false-> false
			end
		end, ModFiles).	

is_behaviour(Module, Behaviour)->
	case lists:keyfind(behaviour, 1, Module:module_info(attributes)) of
		false-> false;
		{behaviour, Behaviours}->
			lists:any(fun(B)-> B == Behaviour end ,Behaviours)
	end.

sprintf(Format, Data)->
	lists:flatten(io_lib:format(Format, Data)).

ensure_mem_module(Module, String)->
	file:write_file(io_lib:format("~s.erl", [Module]), String),
	{Module, Code} = dc:from_string(String),
	case code:load_binary(Module, [], Code) of
		{module, Module}-> ok;
		{error, What}-> {error, What}
	end.