-module(ou).
-compile(export_all).

get_node_name(SNode, IPStr)->
    utl:to_atom(utl:to_list(SNode) ++ "@" ++ utl:to_list(IPStr)).

get_node_sname(Node)->
    case string:tokens(utl:to_list(Node), "@") of
        [NodeName, _Host]-> utl:to_atom(NodeName);
        _-> ""
    end.

get_node_sname_str(Node)->
    case string:tokens(utl:to_list(Node), "@") of
        [NodeName, _Host]-> NodeName;
        _->[]
    end.
%%
%% API Functions
%%
get_local_ips()->
	case inet:getif() of
		{ok,IFs}->
			SortedIFs = lists:sort(fun(IP1,IP2)-> 
										   {{I1,I2,I3,I4},_,_} = IP1,
										   {{J1,J2,J3,J4},_,_} = IP2,
										   if I1 =:= 192 -> true;
											  J1 =:= 192 -> false;
											  I1 =:= 127 -> true;
											  J1 =:= 127 -> false;
											  I1 < J1 -> true;
											  I1 > J1 -> false;
											  I2 < J2 -> true;
											  I2 > J2 -> false;
											  I3 < J3 -> true;
											  I3 > J3 -> false;
											  I4 < J4 -> true;
											  I4 > J4 -> false;
											  true-> false
										   end
								   end, IFs),
			lists:map(fun(IfConfig)->
							case IfConfig of
								{{192,168,I3,I4},_,_}-> "192.168." ++ integer_to_list(I3) ++"." ++ integer_to_list(I4);
								{{127,0,0,I4},_,_}->"127.0.0." ++ integer_to_list(I4);
								{{10,I2,I3,I4},_,_}->sprintf("10.~p.~p.~p", [I2,I3,I4]);
								{{I1,I2,I3,I4},_,_}->sprintf("~p.~p.~p.~p", [I1,I2,I3,I4])
							end
					end,SortedIFs);
		_->[]
	end.

sprintf(Format, Data)->
	lists:flatten(io_lib:format(Format, Data)).

get_local_ip()->
	case get_local_ips() of
		[]->[];
		[IP|_]-> IP
	end.

sure_module(Module) when is_binary(Module)->
	sure_module(binary_to_atom(Module,latin1));
sure_module(Module) when is_list(Module)->
	sure_module(list_to_atom(Module));
sure_module(Module)->
	case code:is_loaded(Module) of
		false-> code:load_file(Module);
		_->nothing
	end.

sure_dir_module(Dir)->
	code:add_patha(Dir),
	Files =  filelib:wildcard(filename:absname_join(Dir, "*.beam")),
	lists:foreach(fun(File)->
						  Module = filename:rootname(filename:basename(File)),
						  sure_module(Module)
				  end, Files).

match_ip(GiveIp)->
	case inet:getif() of
		{ok,IFs}->
			lists:any(fun(IF)->
							  {{IP1,IP2,IP3,IP4},_,_}=IF,
							  IpStr = integer_to_list(IP1) ++[$.] 
					  			++ integer_to_list(IP2) ++ [$.] 
					  		    ++ integer_to_list(IP3) ++ [$.] 
					  			++ integer_to_list(IP4),
							  IpStr =:= GiveIp
					  end, IFs);
		_-> false
	end.

-spec cmd(Command,DataBack)-> atom when
Command :: atom() | io_lib:chars(),
DataBack :: function() | { Module::atom() , Func::atom()}.
cmd(Cmd,DataBack)->
	validate(Cmd),
	case os:type() of
		{unix, _}->
			unix_cmd(Cmd, DataBack);
		{win32, Wtype}->
			Command0 = case {os:getenv("COMSPEC"), Wtype} of
				           {false, windows}->lists:concat(["command.com /c", Cmd]);
				           {false, _}->lists:concat(["cmd /c", Cmd]);
				           {Cspec, _}->lists:concat([Cspec, " /c", Cmd])
			           end,
			%% open_port/2 awaits string() in Command, but io_lib:chars() can be
			%% deep lists according to io_lib module description.
			Command = lists:flatten(Command0),
			Port = open_port({spawn, Command}, [stream, in, eof, hide]),
			get_data(Port,[], DataBack)
	end.


%% Executes the given command in the default shell for the operating system.
-spec cmd_async(Command) -> atom() when
      Command :: atom() | io_lib:chars().
cmd_async(Cmd) ->
	CurPid = self(),
	Fun = fun()->do_cmd_ansync(Cmd,CurPid) end,
	proc_lib:spawn(Fun),
	receive 
		ok-> ok;
		_->error
	end.

-spec cmd_ansync(Command,TimeOut) -> atom() when
      Command :: atom() | io_lib:chars(),
	  TimeOut :: integer().

cmd_ansync(Cmd,infinit) ->
	CurPid = self(),
	Fun = fun()->do_cmd_ansync(Cmd,CurPid) end,
	proc_lib:spawn(Fun),
	receive 
		ok-> ok;
		_->error
	end;
cmd_ansync(Cmd,TimeOut) ->
	CurPid = self(),
	Fun = fun()->do_cmd_ansync(Cmd,CurPid) end,
	proc_lib:spawn(Fun),
	receive 
		ok-> ok;
		_->error
	after TimeOut->
			timeout
	end.

osx_run_cmd(Cmd)->
    SystemParam   =" -e 'tell application \"System Events\"  to tell process \"Terminal\" to keystroke \"t\" using command down'",
    ExecuteParam  = io_lib:format(" -e 'tell application \"Terminal\" to do script \"~s\" in selected tab of the front window'", [Cmd]),
    StartNewTermal = "osascript " ++ SystemParam ++ ExecuteParam,
    os:cmd(StartNewTermal).

do_cmd_ansync(Cmd,MonitorPid) ->
    validate(Cmd),
    case os:type() of
	{unix, _} ->
	    unix_cmd_ansyc(Cmd,MonitorPid);
	{win32, Wtype} ->
	    Command = case {os:getenv("COMSPEC"),Wtype} of
			  {false,windows} -> lists:concat(["command.com /c", Cmd]);
			  {false,_} -> lists:concat(["cmd /c", Cmd]);
			  {Cspec,_} -> lists:concat([Cspec," /c",Cmd])
		      end,
	    Port = open_port({spawn, Command}, [stream, in, eof, hide]),
		MonitorPid ! ok,
		get_data_async(Port, []);
	%% VxWorks uses a 'sh -c hook' in 'vxcall.c' to run os:cmd.
	vxworks ->
		Command = lists:concat(["sh -c '", Cmd, "'"]),
		Port = open_port({spawn, Command}, [stream, in, eof]),
		MonitorPid ! ok,
		get_data_async(Port, []);
	_ ->
	    throw(cannotsupport)
    end.

unix_cmd(Cmd,DataBack) ->
	Tag = make_ref(),
	{Pid,Mref} = erlang:spawn_monitor(
		fun() ->
			process_flag(trap_exit, true),
			Port = start_port(),
			erlang:port_command(Port, mk_cmd(Cmd)),
			exit({Tag,unix_get_data(Port,DataBack)})
		end),
	receive
		{'DOWN',Mref,_,Pid,{Tag,Result}} ->
			Result;
		{'DOWN',Mref,_,Pid,Reason} ->
			exit(Reason)
	end.

unix_cmd_ansyc(Cmd,MonitorPid) ->
    Tag = make_ref(),
    {Pid,Mref} = erlang:spawn_monitor(
		   fun() ->
			   process_flag(trap_exit, true),
			   Port = start_port(),
			   erlang:port_command(Port, mk_cmd(Cmd)),
			   MonitorPid ! ok,
			   exit({Tag,unix_get_data_async(Port)})
		   end),
    receive
	{'DOWN',Mref,_,Pid,{Tag,Result}} ->
	    Result;
	{'DOWN',Mref,_,Pid,Reason} ->
	    exit(Reason)
    end.



%% The -s flag implies that only the positional parameters are set,
%% and the commands are read from standard input. We set the 
%% $1 parameter for easy identification of the resident shell.
%%
-define(SHELL, "/bin/sh -s unix:cmd 2>&1").
-define(PORT_CREATOR_NAME, os_cmd_port_creator).

%%
%% Serializing open_port through a process to avoid smp lock contention
%% when many concurrent os:cmd() want to do vfork (OTP-7890).
%%
-spec start_port() -> port().
start_port() ->
    Ref = make_ref(),
    Request = {Ref,self()},    
    {Pid, Mon} = case whereis(?PORT_CREATOR_NAME) of
		     undefined ->
			 spawn_monitor(fun() ->
					       start_port_srv(Request)
				       end);
		     P ->
			 P ! Request,
			 M = erlang:monitor(process, P),
			 {P, M}
		 end,
    receive
	{Ref, Port} when is_port(Port) ->
	    erlang:demonitor(Mon, [flush]),
	    Port;
	{Ref, Error} ->
	    erlang:demonitor(Mon, [flush]),
	    exit(Error);
	{'DOWN', Mon, process, Pid, _Reason} ->
	    start_port()
    end.


start_port_srv(Request) ->
    %% We don't want a group leader of some random application. Use
    %% kernel_sup's group leader.
    {group_leader, GL} = process_info(whereis(kernel_sup),
				      group_leader),
    true = group_leader(GL, self()),
    process_flag(trap_exit, true),
    StayAlive = try register(?PORT_CREATOR_NAME, self())
		catch
		    error:_ -> false
		end,
    start_port_srv_handle(Request),
    case StayAlive of
	true -> start_port_srv_loop();
	false -> exiting
    end.

start_port_srv_handle({Ref,Client}) ->
    Reply = try open_port({spawn, ?SHELL},[stream]) of
		Port when is_port(Port) ->
		    (catch port_connect(Port, Client)),
		    unlink(Port),
		    Port
	    catch
		error:Reason ->
		    {Reason,erlang:get_stacktrace()}	    
	    end,
    Client ! {Ref,Reply}.


start_port_srv_loop() ->
    receive
	{Ref, Client} = Request when is_reference(Ref),
				     is_pid(Client) ->
	    start_port_srv_handle(Request);
	_Junk ->
	    ignore
    end,
    start_port_srv_loop().


%%
%%  unix_get_data(Port) -> Result
%%
unix_get_data(Port,DataBack) ->
	unix_get_data(Port, [],DataBack).

unix_get_data(Port, Sofar,DataBack) ->
    receive
	{Port,{data, Bytes}} ->
	    case eot(Bytes) of
		{done, Last} ->
			case DataBack of
				undefined-> ignor;
				{Module,Func}-> Module:Func(Bytes);
				_-> DataBack(Bytes)
			end,
			lists:flatten([Sofar|Last]);
		more  ->
		    unix_get_data(Port, [Sofar|Bytes],DataBack)
	    end;
	{'EXIT', Port, _} ->
	    ok
    end.

unix_get_data_async(Port) ->
	unix_get_data_async(Port, []).

unix_get_data_async(Port, Sofar) ->
	receive
		{Port,{data, Bytes}} ->
			case eot(Bytes) of
				{done, Last} ->
					lists:flatten([Sofar|Last]);
				more  ->
					unix_get_data(Port, [Sofar|Bytes],undefined)
			end;
		{'EXIT', Port, _} ->
			lists:flatten(Sofar)
	end.


%%
%% eot(String) -> more | {done, Result}
%%
eot(Bs) ->
    eot(Bs, []).

eot([4| _Bs], As) ->
    {done, lists:reverse(As)};
eot([B| Bs], As) ->
    eot(Bs, [B| As]);
eot([], _As) ->
    more.

%%
%% mk_cmd(Cmd) -> {ok, ShellCommandString} | {error, ErrorString}
%%
%% We do not allow any input to Cmd (hence commands that want
%% to read from standard input will return immediately).
%% Standard error is redirected to standard output.
%%
%% We use ^D (= EOT = 4) to mark the end of the stream.
%%
mk_cmd(Cmd) when is_atom(Cmd) ->		% backward comp.
    mk_cmd(atom_to_list(Cmd));
mk_cmd(Cmd) ->
    %% We insert a new line after the command, in case the command
    %% contains a comment character.
    io_lib:format("(~s\n) </dev/null; echo  \"\^D\"\n", [Cmd]).

validate(Atom) when is_atom(Atom) ->
    ok;
validate(List) when is_list(List) ->
    validate1(List).

validate1([C|Rest]) when is_integer(C), 0 =< C, C < 256 ->
    validate1(Rest);
validate1([List|Rest]) when is_list(List) ->
    validate1(List),
    validate1(Rest);
validate1([]) ->
    ok.
get_data_async(Port, Sofar) ->
	receive
		{Port, {data, Bytes}} ->
			get_data_async(Port, [Sofar|Bytes]);
		{Port, eof} ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					true
			end,
			receive
				{'EXIT',  Port,  _} ->
					ok
			after 1 ->				% force context switch
				ok
			end,
			lists:flatten(Sofar)
	end.

get_data(Port,Sofar, DataBack) ->
    receive
	{Port, {data, Bytes}} ->
		case DataBack of
			undefined-> ignore;
			{Module,Func}-> Module:Func(Bytes);
			_-> DataBack(Bytes)
		end,
	    get_data(Port,[Sofar|Bytes], DataBack);
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end,
		lists:flatten(Sofar)
    end.


get_arguments(Options)->
	Fun = fun(Opt,{ThisKV,KVs})->
				  case Opt of
					  "-"++ Opt1->				
						  NewKVs = case ThisKV of
									   []-> KVs;
									   _-> KVs ++ [ThisKV]
								   end,
						  NewThisKV = {Opt1,[]},
						  {NewThisKV,NewKVs};
					  _->
						  NewKVs = KVs,
						  case ThisKV of
							  []->
								  {[],NewKVs ++ [{Opt}]};
							  {LastKey}-> 
								  NewThisKV = {LastKey,[Opt]},
								  {NewThisKV,NewKVs};
							  {LastKey,LastVal}->
								  NewThisKV = {LastKey,LastVal++[Opt]},
								  {NewThisKV,NewKVs}
						  end
				  end
		  end,
	{ThisKV,KVs} = lists:foldl(Fun, {[],[]}, Options),
	KVs ++ [ThisKV].

get_init_arguments(Args)->
	{Current,Result} = lists:foldl(
		fun([$-|Cmd],{LastRes,TotoalRes})->
			NewTotoalRes = [LastRes|TotoalRes],
			CurRes = {list_to_atom(Cmd),[]},
			{CurRes, NewTotoalRes};
			(Param,{{Cmd,Params},TotoalRes})->
				CurRes = {Cmd,Params++[Param]},
				{CurRes,TotoalRes}
		end,{{undefined,[]},[]},Args),

	case lists:reverse([Current|Result]) of
		[{undefined,[]}]->[];
		[{undefined,[]}|Arguments]->Arguments;
		Arguments->Arguments
	end.