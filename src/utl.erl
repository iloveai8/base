-module(utl).
-compile(export_all).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

%% Get Microsecond
time_now_ms() ->
    time_to_ms(os:timestamp()).

time_to_ms({Mega, Sec, Micro}) ->
    (Mega * 1000000 + Sec) * 1000000 + Micro.

% Get Second
time_now_sec() ->
    time_to_sec(os:timestamp()).

time_to_sec({Mega, Sec, _Micro}) ->
    Mega * 1000000 + Sec.

sec_to_time(Second) ->
    {Second div 1000000, Second rem 1000000, 0}.

sec_to_local_time(Second) ->
    calendar:now_to_local_time(sec_to_time(Second)).
sec_to_local_date(Second) ->
    {{Year, Month, Day}, _} = sec_to_local_time(Second),
    {Year, Month, Day}.

sec_to_world_time(Second) ->
    calendar:now_to_universal_time(sec_to_time(Second)).    
sec_to_world_date(Second) ->
    {{Year, Month, Day}, _} = sec_to_world_time(Second),
    {Year, Month, Day}.    

local_time() ->
    local_time(time_now_sec()).

local_time(Second)->
    sec_to_local_time(Second). 

-define(GREGORIAN_SECONDS_1970, 62167219200).
local_time_to_now(DateTime) ->
    GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
    {ESeconds div 1000000, ESeconds rem 1000000, 0}.

now_to_local_time(Now) ->
    calendar:now_to_local_time(Now).

local_time_to_s(DateTime) ->
    time_to_sec(local_time_to_now(DateTime)).

day_of_week() -> 
    {{Year, Month, Day}, {_Hour, _Minute, _Second}} = local_time(),
    calendar:day_of_the_week(Year, Month, Day).

day_of_week(Second) -> 
    {{Year, Month, Day}, {_Hour, _Minute, _Second}} = sec_to_local_time(Second),
    calendar:day_of_the_week(Year, Month, Day).    

date_to_readable_int({{Y, Mo, D},{H, Mi, _S}})->
    YShort = Y - 2000,
    list_to_integer(lists:flatten(io_lib:format("~2.2w~2.2.0w~2.2.0w~2.2.0w~2.2.0w", [YShort, Mo, D, H, Mi]))).

today_to_readable_int()->
    date_to_readable_int(local_time()).

date_to_readable_int_only_day({{Y,Mo,D},{_H,_Mi,_S}})->
    YShort = Y - 2000,
    list_to_integer(lists:flatten(io_lib:format("~2.2w~2.2.0w~2.2.0w",[YShort, Mo, D]))).

today_to_readable_int_only_day()->
    date_to_readable_int_only_day(local_time()).

get_date_string(Second) ->
    {{Year,Month,Day},_} =  sec_to_local_time(Second),
    lists:flatten(io_lib:format("~4.4.0w_~2.2.0w_~2.2.0w", [Year, Month, Day])).

datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Minute, Second])).

date_to_string({Year, Month, Day}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])).

time_to_string({Hour, Minute, Second}) ->
    lists:flatten(io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute, Second])). 

to_list(undefined) -> "";
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_list(Value)->
    Value;
to_list(Value) when is_integer(Value)->
    integer_to_list(Value);
to_list(Value) when is_atom(Value)->
    atom_to_list(Value);
to_list(Value) when is_tuple(Value)->
    tuple_to_list(Value);
to_list(Value) when is_float(Value)->
    lists:flatten(io_lib:format("~p", [Value]));
to_list(_Value) ->
    "Error".

to_binary(Val) when is_binary(Val) ->
    Val;
to_binary(Val) when is_list(Val)->
    list_to_binary(Val);
to_binary(Val) when is_integer(Val)->
    integer_to_binary(Val);
to_binary(Val) when is_atom(Val)->
    atom_to_binary(Val, latin1);
to_binary(Val) ->
    Val.

to_integer(true) -> 1;
to_integer(false) -> 0;
to_integer(undefined) -> 0;
to_integer([]) -> 0;
to_integer(<<>>) -> 0;
to_integer(Val) when is_integer(Val) ->
    Val;
to_integer(Val) when is_list(Val)->
    list_to_integer(Val);
to_integer(Val) when is_binary(Val)->
    binary_to_integer(Val).

to_atom(Val) when is_binary(Val)->binary_to_atom(Val, latin1);    
to_atom(Val) when is_list(Val)->list_to_atom(Val);    
to_atom(Val) when is_atom(Val)->Val.  

% get Random
get_random(Base) ->
   rand:uniform(Base).
% get problility func
get_probability(Value) ->
	get_probability(Value, 100).

get_probabilityMil(Value) ->
	get_probability(Value, 1000).
    
get_probability(Value, _Base) when Value =< 0 -> false;
get_probability(Value, Base) when Value >= Base -> true;
get_probability(Value, Base) ->
	rand:uniform(Base) =< Value.
    
random_between(Min, Max) ->
	Min + get_random(Max - Min + 1) - 1.

%% Random Value By Weight
random_by_weight(ValueWeights) ->
    WeightSum = lists:foldl(fun({_, Weight}, Sum) -> Sum + Weight end, 0, ValueWeights),
    RandValue = get_random(WeightSum),
    {Value, _} = random_by_weight(RandValue, ValueWeights),
    Value.
random_by_weight(RandValue, ValueWeights) ->
    random_by_weight(RandValue, ValueWeights, 0).

random_by_weight(_RandValue, [], _CompareValue) -> error;
random_by_weight(RandValue, [{Value, Weight}|T], CompareValue) ->
    NewCompareValue = Weight + CompareValue,
    case RandValue =< NewCompareValue of
        true -> {Value, Weight};
        false -> random_by_weight(RandValue, T, NewCompareValue)
    end.

% clamp
clamp(Value, Min, Max) ->
	if
		Value =< Min -> Min;
		true ->
			if Value >= Max ->
				Max;
				true -> Value
			end
	end.

ceiling(Num) when is_integer(Num)-> Num;
ceiling(Num) when is_float(Num) ->
    Trunc=erlang:trunc(Num),
    case Trunc==Num  of
        true->
            Trunc;
        false ->
            Trunc+1
    end.

floor(X) ->
     T = trunc(X),
     case (X < T) of
         true -> T - 1;
         _ -> T
     end.

%%eq:
%% kvlist_set_kv([{key1,value1},{key2,value2}],key3,value3)
%%              ==[{key1,value1},{key2,value2},{key3,value3}]
%% kvlist_set_kv([{key1,value1},{key2,value2}],key2,newvalue2)
%%              ==[{key1,value1},{key2,newvalue2}]
kvlist_set_kv(KVList, Key, Value)->
    case lists:keyfind(Key,1,KVList) of
        false-> [{Key,Value}|KVList];
        {Key,_OldVal}->
            lists:keyreplace(Key,1,KVList,{Key,Value})
    end.
%% Key对应的Value是int,将其值自增1
%% 如果kvlist中不包含{Key,intV},则将其值设为DefaultValue
kvlist_increment_value(KVList,Key,DefaultValue)->
    kvlist_increment_value_with(KVList,Key,1,DefaultValue).

%% Key对应的Value是int,将其值自增 StepNum
%% 如果kvlist中不包含{Key,intV},则将其值设为DefaultValue
%% utility:kvlist_increment_value_with([],key,1,10)==[{key,10}]
%% utility:kvlist_increment_value_with([{key,10}],key,1,10)==[{key,11}]
kvlist_increment_value_with(KVList,Key,StepNum,DefaultValue) ->
    case lists:keyfind(Key,1,KVList) of
        false->
            [{Key,DefaultValue}|KVList];
        {Key,OldVal} when is_integer(OldVal)->
            lists:keyreplace(Key,1,KVList,{Key,OldVal+StepNum})
    end.

%%每个key 对应的值是一个list ,向此list 中插入一个元素(若有重复，则会消除重复再插入)
%%kvlist_add_element([{key1,[value1]},{key2,[value2,value22]}],key3,value33)
kvlist_add_element(KVList, Key, ValueElement)->
    case  lists:keyfind(Key,1,KVList)   of
        false->
            [{Key,[ValueElement]}|KVList];
        {Key,OldVal}->
            lists:keyreplace(Key,1,KVList,{Key,[ValueElement|lists:delete(ValueElement,OldVal)]})
    end.

kvlist_get_value(KVList,Key,DefaultValue)->
    case proplists:get_value(Key,KVList) of
        undefined->
            DefaultValue;
        Value ->
            Value
    end.

string_to_integer(String, Multiple) ->
    case string:to_float(utl:to_list(String)) of
        {error,no_float} ->
            {IntVal,[]} = string:to_integer(utl:to_list(String)),
            IntVal * Multiple;
        {Value,[]} ->
            trunc(Value * Multiple)
    end.
%% utility:string_to_int_list("1,2,31") =[1,2,31]
string_to_int_list(Str)->
    string_to_int_list(Str,",").
string_to_int_list(Str,SplitCharStr)when is_list(Str),is_list(SplitCharStr)->
    [list_to_integer(NStr)||NStr<-string:tokens(Str,SplitCharStr)].

int_list_to_string(List) when is_list(List)->
    string:join([integer_to_list(I)||I<-List],",").

int_list_to_string(List, Separator) when is_list(List)->
    string:join([integer_to_list(I)||I<-List], Separator).

str_array_to_string(List) when is_list(List)->
    string:join([to_list(I)||I<-List],",").

string_to_str_array(Str)->
    string_to_str_array(Str,",").
string_to_str_array(Str, SplitCharStr)when is_list(Str),is_list(SplitCharStr)->
    string:tokens(Str,SplitCharStr).

list_random_count(GetCount, List) ->
    Len=length(List),
    list_random_count(GetCount,Len,List,[],[]).
list_random_count(0,_Len, _List,_Indexes,Res) -> Res;
list_random_count(GetCount,Len, List,Indexes,Res) ->
    N=rand:uniform(Len),
    case lists:member(N,Indexes) of
        true ->
            list_random_count(GetCount, Len, List, Indexes, Res);
        false ->
            list_random_count(GetCount-1, Len, List,[N|Indexes],[lists:nth(N,List)|Res])
    end.

list_random_count_with_dup(GetCount, List) ->
    Len=length(List),
    case Len of
        0-> [];
        _ -> list_random_count2(GetCount,Len,List,[])
    end.

list_random_count2(0,_Len, _List,Res) ->  Res;
list_random_count2(GetCount,Len, List,Res) ->
    N=rand:uniform(Len),
    list_random_count2(GetCount-1,Len, List,[lists:nth(N,List)|Res]).

list_replace_by_index(List,Index,NewEle)->
    Len=length(List),
    lists:sublist(List,1,Index-1)++[NewEle]++lists:sublist(List,Index+1,Len-Index).

list_reorder(List)when is_list(List)->
    list_reorder(List,length(List),[]).

list_reorder(_List,0,NewList)-> NewList;
list_reorder(List,Len,NewList) ->
    Index=rand:uniform(Len),
    E=lists:nth(Index,List),
    DeletedList= lists:sublist(List,1,Index-1)++lists:sublist(List,Index+1,Len-Index),
    list_reorder(DeletedList,Len-1,[E|NewList]).

lists_random([])->
    undef;
lists_random(Lists)->
    Len=length(Lists),
    lists:nth(rand:uniform(Len),Lists).

%% delete all Ele from List
lists_delete_all_eles(List,[])->
    List;
lists_delete_all_eles(List,Eles)->
    lists:foldl(fun(E,Acc)->
                        lists_delete_all(Acc,E)
                        end,List,Eles).
                
lists_delete_all(List,Ele)->
    [L||L<-List,L/=Ele].

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join1(Items, Sep, []))).

string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
    string_join1(Tail, Sep, [Sep, Head | Acc]).

encode_int_key_value_list(KeyValueList) ->
    KeyValueString = [ integer_to_list(Key) ++ "=" ++ integer_to_list(Value)||{Key, Value} <- KeyValueList ],
    "[" ++ string:join(KeyValueString, ",") ++ "]".

decode_int_key_value_list(String) ->
    KeyValueStrings = string:tokens(string:sub_string(String, 2, length(String) - 1), ","),
    [decode_int_key_value(KeyValueString) || KeyValueString <- KeyValueStrings].

decode_int_key_value(KeyValueString) ->
    [Key,Value] = string:tokens(KeyValueString, "="),
    {list_to_integer(Key), list_to_integer(Value)}.

encode_str_key_value_list(KeyValueList) ->
    KeyValueString = [ to_list(Key) ++ "=" ++ to_list(Value)||{Key, Value} <- KeyValueList ],
    "[" ++ string:join(KeyValueString, ",") ++ "]".

decode_str_key_value_list(String) ->
    KeyValueStrings = string:tokens(string:sub_string(String, 2, length(String) - 1), ","),
    [decode_str_key_value(KeyValueString) || KeyValueString <- KeyValueStrings].

decode_str_key_value(KeyValueString) ->
    [Key,Value] = string:tokens(KeyValueString, "="),
    {to_integer(Key), to_binary(Value)}.

%% Get Bit
flag_bit(Pos) ->
    1 bsl Pos.
%% Set Flags
set_flags(Value, Flags) ->
    Value bor Flags.
%% Check Flags whether Set
check_flags(Value, Flags) ->
    (Value band Flags) =/= 0.
% Clear Flags
clear_flags(Value, Flags) ->
    Value band bnot Flags.
% Set Value's Bit
set_bit(Value, Pos) ->
    set_flags(Value, flag_bit(Pos)).
% Clear Value's Bit
clear_bit(Value, Pos) ->
    clear_flags(Value, flag_bit(Pos)).
% Check Value's Bit
check_bit(Value, Pos) ->
    check_flags(Value, flag_bit(Pos)).
    
% Set Value's Bit Value
set_bit(Value, Pos, false) ->
    clear_bit(Value, Pos);
set_bit(Value, Pos, 0) ->
    clear_bit(Value, Pos);   
set_bit(Value, Pos, _Value) ->
    set_bit(Value, Pos).

%% md5
md5_hex(S) ->
    Md5_list = to_list(erlang:md5(S)),
    lists:flatten(list_to_hex(Md5_list)).
    
hmac_encrypto(Type,Key,S) ->
    Hmd5_list = to_list(crypto:hmac(Type,Key,S)),
    lists:flatten(list_to_hex(Hmd5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).    
 
url_decode([$%, Hi, Lo | Tail]) ->
    Hex = list_to_integer([Hi, Lo], 16),
    [Hex | url_decode(Tail)];
    url_decode([$?|T]) ->
           %% Don't decode the query string here, that is parsed separately.
           [$?|T];
    url_decode([H|T]) when is_integer(H) -> 
           [H |url_decode(T)];
    url_decode([]) ->
           [];
    %% deep lists
    url_decode([H|T]) when is_list(H) ->
           [url_decode(H) | url_decode(T)].

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
    quote_plus(mochinum:digits(Float));
quote_plus(String) ->
    quote_plus(String, []).
quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).
    
urlencode(Props) ->
    Pairs = lists:foldr(
              fun ({K, V}, Acc) ->
                      [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&"). 

is_process_alive(PID) when is_pid(PID) ->
    case rpc:call(node(PID), erlang, is_process_alive, [PID]) of
        {badrpc, _Reason} -> false;
        Value -> Value
    end;
is_process_alive(_PID) -> false.