-module(glua_ffi).
-export([load/2, load_file/2, eval/2, eval_chunk/2, eval_file/2]).

%% helper to convert luerl return values to a format
%% that is more suitable for use in Gleam code
to_gleam(Value) ->
  case Value of
    {ok, Result, LuaState} -> {ok, {Result, LuaState}};
    {lua_error, Errors, _} -> {error, Errors};
    {error, L1, L2} -> {error, {L1, L2}};
    error -> {error, nil}
  end.

load(Lua, Code) ->
  to_gleam(luerl:load(unicode:characters_to_list(Code), Lua)).
  
load_file(Lua, Path) ->
  to_gleam(luerl:loadfile(unicode:characters_to_list(Path), Lua)).

eval(Lua, Code) ->
  to_gleam(luerl:do(unicode:characters_to_list(Code), Lua)).

eval_chunk(Lua, Chunk) ->
  to_gleam(luerl:call_chunk(Chunk, Lua)).

eval_file(Lua, Path) ->
  to_gleam(luerl:dofile(unicode:characters_to_list(Path), Lua)).
