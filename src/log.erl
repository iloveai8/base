-module(log).
-export([init/1, init/0, info/1, info/2, info/3, info/4, error/1, error/2, trace/1, trace/2]).

init()->
	filelib:ensure_dir("./log/"),
	Filename = "./log/" ++ atom_to_list(get_node_sname(node())) ++ "_node.log",
	init(Filename).

init(FileName)->
	case init:get_argument(noinput) of
		error->
			case init:get_argument(noshell) of
				error->ignor;
				_-> error_logger:tty(false)
			end;
		_-> error_logger:tty(false)
	end,
	case error_logger:logfile({open, FileName}) of
		ok -> ok;
		{error, allready_have_logfile}-> ok;
		E -> E
	end.

-ifdef(debug).
	info(Moule, Line, Format, Data)->
		info(atom_to_list(Moule) ++ " Line=" ++ integer_to_list(Line) ++ " " ++ Format, Data).
	info(Moule, Line, Format)->
		info(atom_to_list(Moule) ++ " Line=" ++ integer_to_list(Line) ++ " " ++ Format).
	info(Format, Data)->
		error_logger:info_msg(Format, Data).
	info(Format)->
		error_logger:info_msg(Format).
	error(Format)->
		error_logger:error_msg(Format).
	error(Format, Data)->
		error_logger:error_msg(Format, Data).
-else.
	info(Moule, Line, Format, Data)->
		info(atom_to_list(Moule) ++ " Line=" ++ integer_to_list(Line) ++ " " ++ Format, Data).
	info(Moule, Line, Format)->
		info(atom_to_list(Moule) ++ " Line=" ++ integer_to_list(Line) ++ " " ++ Format).
	info(Format, Data)->
		error_logger:info_msg(Format, Data).
	info(Format)->
		error_logger:info_msg(Format).
	error(Format)->
		error_logger:error_msg(Format).
	error(Format, Data)->
		error_logger:error_msg(Format, Data).
-endif.

-ifdef(debug).
	trace(Format, Data) ->
		error_logger:info_msg(Format, Data).
	trace(Format) ->
		error_logger:info_msg(Format).
-else.
	trace(_, _) ->
		ok.
	trace(_) ->
	ok.
-endif.

get_node_sname(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName, _Host]->list_to_atom(NodeName);
		_->undefined
	end.

