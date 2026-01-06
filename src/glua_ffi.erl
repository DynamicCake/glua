-module(glua_ffi).

-import(luerl_lib, [lua_error/2]).
-include_lib("luerl/include/luerl.hrl").

-export([coerce/1, coerce_nil/0, coerce_userdata/1, wrap_fun/1, sandbox_fun/2, get_table_keys/2, get_table_key/3,
         get_private/2, set_table_keys/3, load/2, load_file/2, eval/2, eval_file/2, encode_table/2, encode_userdata/2,
         eval_chunk/2, call_function/3, classify/1, unwrap_userdata/1, get_table_transform/4,
         get_table_list_transform/4, userdata_exists/2, table_exists/2]).

%% helper to convert luerl return values to a format
%% that is more suitable for use in Gleam code
to_gleam(Value) ->
    case Value of
        {ok, Result, LuaState} ->
            {ok, {LuaState, Result}};
        {ok, _} = Result ->
            Result;
        {lua_error, _, _} = Error ->
            {error, map_error(Error)};
        {error, _, _} = Error ->
            {error, map_error(Error)};
        error ->
            {error, unknown_error}
    end.

classify(nil) ->
    <<"Nil">>;
classify(Bool) when is_boolean(Bool) ->
    <<"Bool">>;
classify(Binary) when is_binary(Binary) ->
    <<"String">>;
classify(N) when is_integer(N) ->
    <<"Int">>;
classify(N) when is_float(N) ->
    <<"Float">>;
classify({tref,_}) ->
    <<"Table">>;
classify({usdref,_}) ->
    <<"UserData">>;
classify({eref,_}) ->
    <<"Unknown">>;
classify({funref,_,_}) ->
    <<"Function">>;
classify({erl_func,_}) ->
    <<"Function">>;
classify({erl_mfa,_,_,_}) ->
    <<"Function">>;
classify(_) ->
    <<"Unknown">>.

get_table_transform(St, #tref{}=T, Acc, Func)
  when is_function(Func, 3) ->
    #table{a=Arr, d=Dict} = luerl_heap:get_table(T, St),
    Acc1 = ttdict:fold(Func, Acc, Dict),
    array:sparse_foldl(Func, Acc1, Arr).

get_table_list_transform(St, #tref{}=T, Acc, Func)
    when is_function(Func, 2) ->
    #table{a=Arr, d=Dict} = luerl_heap:get_table(T, St),
    case Dict of
        empty -> do_list_transform(Arr, Acc, Func);
        _ -> {error, nil}
    end.

do_list_transform(Arr, Acc, Func) ->
    N = array:size(Arr),
    try
        Wrapped = fun(Idx, Val, {Expected, UserAcc}) ->
                 case Idx of
                     Expected ->
                         {Expected - 1, Func(Val, UserAcc)};
                     _        -> throw(hole)
                 end
              end,
        {End, Final} =
            array:sparse_foldr(Wrapped, {N - 1, Acc}, Arr),
        case {N, End} of
            {0, _} -> {ok, Final};
            {_, 0} -> {ok, Final};
            _ -> {error, nil}
        end
    catch
        throw:hole -> {error, nil}
    end.

%% TODO: Improve compiler errors handling and try to detect more errors
map_error({error, [{_, luerl_parse, Errors} | _], _}) ->
    FormattedErrors = lists:map(fun(E) -> list_to_binary(E) end, Errors),
    {lua_compiler_exception, FormattedErrors};
map_error({lua_error, {illegal_index, Tbl, Value}, State}) ->
    FormattedTbl = list_to_binary(io_lib:format("~p", [Tbl])),
    FormattedValue = unicode:characters_to_binary(Value),
    {lua_runtime_exception, {illegal_index, FormattedTbl, FormattedValue}, State};
map_error({lua_error, {error_call, _} = Error, State}) ->
    {lua_runtime_exception, Error, State};
map_error({lua_error, {undefined_function, Value}, State}) ->
    {lua_runtime_exception,
     {undefined_function, list_to_binary(io_lib:format("~p", [Value]))},
     State};
map_error({lua_error, {badarith, Operator, Args}, State}) ->
    FormattedOperator = unicode:characters_to_binary(atom_to_list(Operator)),
    FormattedArgs =
        lists:map(fun(V) ->
                     unicode:characters_to_binary(
                         io_lib:format("~p", [V]))
                  end,
                  Args),
    {lua_runtime_exception, {bad_arith, FormattedOperator, FormattedArgs}, State};
map_error({lua_error, {assert_error, _} = Error, State}) ->
    {lua_runtime_exception, Error, State};
map_error({lua_error, Dynamic, State}) ->
    {lua_runtime_exception, {unknown_exception, Dynamic}, State};
map_error(_) ->
    unknown_error.

coerce(X) ->
    X.

coerce_nil() ->
    nil.

coerce_userdata(X) ->
    {userdata, X}.

unwrap_userdata({userdata, Data}) ->
    {ok, Data};
unwrap_userdata(_) ->
    {error, nil}.

encode_table(State, Values) ->
    {Data, St} = luerl_heap:alloc_table(Values, State),
    {St, Data}.

encode_userdata(State, Values) ->
    {Data, St} = luerl_heap:alloc_userdata(Values, State),
    {St, Data}.

wrap_fun(Fun) ->
    NewFun = fun(Args, StateIn) ->
        {Status, Result} = Fun(StateIn, Args),
        case Status of
            ok ->
                {StateOut, Return} = Result,
                {Return, StateOut};
            error ->
                {StateOut, Msgs} = Result,
                {error, map_error(lua_error({error_call, Msgs}, StateOut))}
        end
    end,
    #erl_func{code=NewFun}.

sandbox_fun(St, Msg) ->
    Fun = fun(_, State) -> 
            {error, map_error(lua_error({error_call, [Msg]}, State))}
    end,
    luerl:encode(Fun, St).

table_exists(State, Table) -> 
    case luerl_heap:chk_table(Table, State) of
        ok -> true;
        error -> false
    end.

userdata_exists(State, Table) -> 
    case luerl_heap:chk_userdata(Table, State) of
        ok -> true;
        error -> false
    end.

get_table_key(Lua, Table, Key) ->
    case luerl:get_table_key(Table, Key, Lua) of
        {ok, nil, _} ->
            {error, key_not_found};
        {ok, Value, Lua} ->
            {ok, {Lua, Value}};
        Other ->
            to_gleam(Other)
    end.

get_table_keys(Lua, Keys) ->
    case luerl:get_table_keys(Keys, Lua) of
        {ok, nil, _} ->
            {error, key_not_found};
        {ok, Value, _} ->
            {ok, Value};
        Other ->
            to_gleam(Other)
    end.

set_table_keys(Lua, Keys, Value) ->
    to_gleam(luerl:set_table_keys(Keys, Value, Lua)).

load(Lua, Code) ->
    to_gleam(luerl:load(
                 unicode:characters_to_list(Code), Lua)).

load_file(Lua, Path) ->
    to_gleam(luerl:loadfile(
                 unicode:characters_to_list(Path), Lua)).

eval(Lua, Code) ->
    to_gleam(luerl:do(
                 unicode:characters_to_list(Code), Lua)).

eval_chunk(Lua, Chunk) ->
    to_gleam(luerl:call_chunk(Chunk, Lua)).

eval_file(Lua, Path) ->
    to_gleam(luerl:dofile(
                 unicode:characters_to_list(Path), Lua)).

call_function(Lua, Fun, Args) ->
    to_gleam(luerl:call(Fun, Args, Lua)).

get_private(Lua, Key) ->
    try
        {ok, luerl:get_private(Key, Lua)}
    catch
        error:{badkey, _} ->
            {error, key_not_found}
    end.
