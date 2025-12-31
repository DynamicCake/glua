-module(glua_ffi).

-import(luerl_lib, [lua_error/2]).

-export([coerce/1, coerce_nil/0, coerce_userdata/1, wrap_fun/1, sandbox_fun/1, get_table_keys/2, get_table_keys_dec/2, get_table_key/3,
         get_private/2, set_table_keys/3, load/2, load_file/2, eval/2, eval_dec/2, eval_file/2,
         eval_file_dec/2, eval_chunk/2, eval_chunk_dec/2, call_function/3, call_function_dec/3,
         alloc/2, classify/1, ref_call_function/3]).

%% turn `{userdata, Data}` into `Data` to make it more easy to decode it in Gleam
maybe_process_userdata(Lst) when is_list(Lst) ->
    lists:map(fun maybe_process_userdata/1, Lst);
maybe_process_userdata({userdata, Data}) ->
    Data;
maybe_process_userdata(Other) ->
    Other.

%% helper to convert luerl return values to a format
%% that is more suitable for use in Gleam code
to_gleam(Value) ->
    case Value of
        {ok, Result, LuaState} ->
            Values = maybe_process_userdata(Result),
            {ok, {LuaState, Values}};
        {ok, _} = Result ->
            maybe_process_userdata(Result);
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
classify(N) when is_float(N) ->
    <<"Number">>;
classify({tref,_}) ->
    <<"Table">>;
classify({usdref,_}) ->
    <<"UserDef">>;
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

%% helper to determine if a value is encoded or not
%% borrowed from https://github.com/tv-labs/lua/blob/5bf2069c2bd0b8f19ae8f3ea1e6947a44c3754d8/lib/lua/util.ex#L19-L35
%% Also see (luerl 1.5.1): https://hexdocs.pm/luerl/luerl.html#t:luerldata/0
is_encoded(nil) ->
    true;
is_encoded(true) ->
    true;
is_encoded(false) ->
    true;
is_encoded(Binary) when is_binary(Binary) ->
    true;
is_encoded(N) when is_number(N) ->
    true;
is_encoded({tref,_}) ->
    true;
is_encoded({usrdef,_}) ->
    true;
% Rationale: https://github.com/rvirding/luerl/blob/8756287ed2083795e7456855edf56a065a49b5aa/src/luerl.erl#L924-L939
% has no mentions of eref
is_encoded({eref,_}) ->
    false;
is_encoded({funref,_,_}) ->
    true;
is_encoded({erl_func,_}) ->
    true;
is_encoded({erl_mfa,_,_,_}) ->
    true;
is_encoded(_) ->
    false.

encode(X, St0) ->
    case is_encoded(X) of
        true -> {X, St0};
        false -> luerl:encode(X, St0)
    end.

encode_list(L, St0) when is_list(L) ->
    Enc = fun(X, {L1, St}) ->
                  {Enc, St1} = encode(X, St),
                  {[Enc | L1], St1}
          end,
    {L1, St1} = lists:foldl(Enc, {[], St0}, L),
    {lists:reverse(L1), St1}.


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
map_error({lua_error, _, State}) ->
    {lua_runtime_exception, unknown_exception, State};
map_error(_) ->
    unknown_error.

coerce(X) ->
    X.

coerce_nil() ->
    nil.

coerce_userdata(X) ->
    {userdata, X}.

alloc(St0, Value) when is_list(Value) ->
    {Enc, St1} = luerl_heap:alloc_table(Value, St0),
    {St1, Enc};
alloc(St0, {usrdef,_}=Value) ->
    {Enc, St1} = luerl_heap:alloc_userdata(Value, St0),
    {St1, Enc};
alloc(St0, Other) ->
    {St0, Other}.

wrap_fun(Fun) ->
    fun(Args, State) ->
            Decoded = luerl:decode_list(Args, State),
            {NewState, Ret} = Fun(State, Decoded),
            luerl:encode_list(Ret, NewState)
    end.

sandbox_fun(Msg) ->
    fun(_, State) -> {error, map_error(lua_error({error_call, [Msg]}, State))} end.


get_table_key(Lua, Table, Key) ->
    case luerl:get_table_keys(Table, Key, Lua) of
        {ok, nil, _} ->
            {error, nil};
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

get_table_keys_dec(Lua, Keys) ->
    case luerl:get_table_keys_dec(Keys, Lua) of
        {ok, nil, _} ->
            {error, key_not_found};
        {ok, Value, _} ->
            {ok, Value};
        Other ->
            to_gleam(Other)
    end.

set_table_keys(Lua, Keys, Value) ->
    SetFun = case is_encoded(Value) of
                 true -> fun luerl:set_table_keys/3;
                 false -> fun luerl:set_table_keys_dec/3
             end,
    to_gleam(SetFun(Keys, Value, Lua)).

load(Lua, Code) ->
    to_gleam(luerl:load(
                 unicode:characters_to_list(Code), Lua)).

load_file(Lua, Path) ->
    to_gleam(luerl:loadfile(
                 unicode:characters_to_list(Path), Lua)).

eval(Lua, Code) ->
    to_gleam(luerl:do(
                 unicode:characters_to_list(Code), Lua)).

eval_dec(Lua, Code) ->
    to_gleam(luerl:do_dec(
                 unicode:characters_to_list(Code), Lua)).

eval_chunk(Lua, Chunk) ->
    to_gleam(luerl:call_chunk(Chunk, Lua)).

eval_chunk_dec(Lua, Chunk) ->
    call_function_dec(Lua, Chunk, []).

eval_file(Lua, Path) ->
    to_gleam(luerl:dofile(
                 unicode:characters_to_list(Path), Lua)).

eval_file_dec(Lua, Path) ->
    to_gleam(luerl:dofile_dec(
                 unicode:characters_to_list(Path), Lua)).

call_function(Lua, Fun, Args) ->
    {EncodedArgs, State} = encode_list(Args, Lua),
    to_gleam(luerl:call(Fun, EncodedArgs, State)).

ref_call_function(Lua, Fun, Args) ->
    to_gleam(luerl:call(Fun, Args, Lua)).

call_function_dec(Lua, Fun, Args) ->
    {EncodedArgs, St1} = encode_list(Args, Lua),
    case luerl:call(Fun, EncodedArgs, St1) of
        {ok, Ret, St2} ->
            Values = luerl:decode_list(Ret, St2),
            {ok, {St2, Values}};
        Other ->
            to_gleam(Other)
    end.

get_private(Lua, Key) ->
    try
        {ok, luerl:get_private(Key, Lua)}
    catch
        error:{badkey, _} ->
            {error, key_not_found}
    end.
