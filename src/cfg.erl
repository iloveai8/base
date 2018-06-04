-module(cfg).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-define(CONFIG_TABLE, config).
-record(config, {key, value}).

-export([get/1, get/2, set/2, show_all/0]).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get(Key) ->
    get(Key, undefined).

get(Key, Default) ->
    case ets:lookup(?CONFIG_TABLE, Key) of
        [] -> Default;
        [Config] -> Config#config.value
    end.

set(Key, Value) ->
    set_value(Key, Value).

show_all() ->
    [{Key, Value} || #config{key = Key, value = Value} <- ets:tab2list(?CONFIG_TABLE)].

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [[]], []).

start_link(Keys) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Keys], []).

init([Keys]) ->
    process_flag(trap_exit, true),
    create_config_ets(),
    do_reload(),
    load_values_from_app_env(Keys),
    erlang:send_after(2000, self(), reload),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(reload, State)->
    do_reload(),
    erlang:send_after(2000, self(), reload),
    {noreply, State};
handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) ->
    delete_config_ets(),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

create_config_ets() ->
    ets:new(?CONFIG_TABLE, [public, set, named_table, {keypos, #config.key}]).
    
delete_config_ets() ->
    ets:delete(?CONFIG_TABLE).

do_reload() ->
    {ok, Cwd} = file:get_cwd(),
    FilePath = filename:join([Cwd, "config"]),
    AbsFiles = [filename:join([FilePath, File]) || File <- filelib:wildcard("*.cfg", FilePath)],
    do_reload(AbsFiles).

do_reload([]) -> ok;
do_reload([File|T]) ->
    case is_reload(File) of
        {true, MTime} ->
            load_config(File, MTime);
        false ->
            ingore
    end,
    do_reload(T).

load_config(File, MTime) ->
    case file:consult(File) of
        {ok, [Props]} -> 
            [begin set_value(Key, Value) end || {Key, Value} <- Props],
            erlang:put(filename:basename(File), MTime),
            ok;
        {error, Error} ->
            log:info("Config Read Error: ~p", [Error]),
            failed
    end.

is_reload(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            case erlang:get(filename:basename(File)) of
                MTime -> 
                    false;
                _ ->
                    {true, MTime}
            end;
        {error, Reason} ->
            log:info("is reload File:~p error read reason: ~p~n", [File, Reason]),
            false
     end.

set_value(Key, Value) ->
    Config = #config{key = Key, value = Value},
    ets:insert(?CONFIG_TABLE, Config),
    {ok, Value}.
    
load_values_from_app_env(Keys) ->
    [load_value(Key) || Key <- Keys].

load_value(Key) ->
    get_app_env(Key),
    get_cmd_argument(Key).

get_app_env(Key) ->
    case application:get_env(Key) of
        undefined -> ok;
        {ok, Value} -> set_value(Key, Value)
    end.

get_cmd_argument(Key) ->  
    case init:get_argument(Key) of
        error -> ok;
        {ok, [[T]]} -> set_value(Key, T)
    end.
