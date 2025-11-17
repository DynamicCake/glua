-module(glua_ffi).
-export([lua_nil/1, encode/2, get_table_keys/2, get_table_keys_dec/2, set_table_keys/3, load/2, load_file/2, eval/2, eval_dec/2, eval_file/2, eval_file_dec/2, eval_chunk/2, eval_chunk_dec/2, call_function/3, call_function_dec/3]).

%% helper to convert luerl return values to a format
%% that is more suitable for use in Gleam code
to_gleam(Value) ->
  case Value of
    {ok, Result, LuaState} -> {ok, {LuaState, Result}};
    {ok, _} = Result -> Result;
    {lua_error, Errors, _} -> {error, Errors};
    {error, L1, L2} -> {error, {L1, L2}};
    error -> {error, nil}
  end.

lua_nil(Lua) ->
  encode(Lua, nil).

encode(Lua, Value) ->
  {Encoded, State} = luerl:encode(Value, Lua),
  {State, Encoded}.

get_table_keys(Lua, Keys) ->
  case luerl:get_table_keys(Keys, Lua) of
    {ok, nil, _} -> {error, nil};
    {ok, Value, _} -> {ok, Value};
    Other -> to_gleam(Other) 
  end.
  
get_table_keys_dec(Lua, Keys) ->
  case luerl:get_table_keys_dec(Keys, Lua) of
    {ok, nil, _} -> {error, nil};
    {ok, Value, _} -> {ok, Value};
    Other -> to_gleam(Other) 
  end.

set_table_keys(Lua, Keys, Value) ->
  to_gleam(luerl:set_table_keys(Keys, Value, Lua)).

load(Lua, Code) ->
  to_gleam(luerl:load(unicode:characters_to_list(Code), Lua)).
  
load_file(Lua, Path) ->
  to_gleam(luerl:loadfile(unicode:characters_to_list(Path), Lua)).

eval(Lua, Code) ->
  to_gleam(luerl:do(unicode:characters_to_list(Code), Lua)).

eval_dec(Lua, Code) ->
  to_gleam(luerl:do_dec(unicode:characters_to_list(Code), Lua)).

eval_chunk(Lua, Chunk) ->
  to_gleam(luerl:call_chunk(Chunk, Lua)).

eval_chunk_dec(Lua, Chunk) ->
  call_function_dec(Lua, Chunk, []).

eval_file(Lua, Path) ->
  to_gleam(luerl:dofile(unicode:characters_to_list(Path), Lua)).

eval_file_dec(Lua, Path) ->
  to_gleam(luerl:dofile_dec(unicode:characters_to_list(Path), Lua)).
  
call_function(Lua, Fun, Args) ->
  to_gleam(luerl:call(Fun, Args, Lua)).

call_function_dec(Lua, Fun, Args) ->
  case luerl:call(Fun, Args, Lua) of
    {ok, Ret, Lua} ->
      Values = luerl:decode_list(Ret, Lua),
      {ok, {Lua, Values}};

    Other -> to_gleam(Other)
  end.
      
