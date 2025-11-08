-module(glua_ffi).
-export([eval/2, eval_file/2]).

%% helper to convert luerl return values to a format
%% that is more suitable for use in Gleam code
to_gleam(Value) ->
  case Value of
    {ok, Result, LuaState} -> {ok, {Result, LuaState}};
    {lua_error, Errors, _} -> {error, Errors};
    {error, L1, L2} -> {error, {L1, L2}};
    error -> {error, nil}
  end.   

eval(Lua, Code) ->
  to_gleam(luerl:do(unicode:characters_to_list(Code), Lua)).
  
eval_file(Lua, Path) ->
  to_gleam(luerl:dofile(unicode:characters_to_list(Path), Lua)).
