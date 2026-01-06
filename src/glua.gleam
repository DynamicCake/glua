//// A library to embed Lua in Gleam programs.
////
//// Gleam wrapper around [Luerl](https://github.com/rvirding/luerl).

import gleam/dynamic
import gleam/list
import gleam/pair
import gleam/result
import gleam/string

/// Represents an instance of the Lua VM.
pub type Lua

/// Represents the errors than can happend during the parsing and execution of Lua code
pub type LuaError {
  /// There was an exception when compiling the Lua code.
  LuaCompilerException(messages: List(String))
  /// The Lua environment threw an exception during code execution.
  LuaRuntimeException(exception: LuaRuntimeExceptionKind, state: Lua)
  /// A certain key was not found in the Lua environment.
  KeyNotFound
  /// An error that could not be identified.
  UnknownError
}

/// Represents the kind of exceptions that can happen at runtime during Lua code execution.
pub type LuaRuntimeExceptionKind {
  /// The exception that happens when trying to access an index that does not exists on a table (also happens when indexing non-table values).
  IllegalIndex(value: String, index: String)
  /// The exception that happens when the `error` function is called.
  ErrorCall(messages: List(String))
  /// The exception that happens when trying to call a function that is not defined.
  UndefinedFunction(value: String)
  /// The exception that happens when an invalid arithmetic operation is performed.
  BadArith(operator: String, args: List(String))
  /// The exception that happens when a call to assert is made passing a value that evalues to `false` as the first argument.
  AssertError(message: String)
  /// An exception that could not be identified.
  UnknownException(dynamic.Dynamic)
}

/// Represents a chunk of Lua code that is already loaded into the Lua VM
pub type Chunk

/// Represents an encoded value inside the Lua environment.
pub type Value

/// A `Value` that is a function
pub type Function

@external(erlang, "glua_ffi", "coerce_nil")
pub fn nil() -> Value

@external(erlang, "glua_ffi", "coerce")
pub fn string(v: String) -> Value

@external(erlang, "glua_ffi", "coerce")
pub fn bool(v: Bool) -> Value

@external(erlang, "glua_ffi", "coerce")
pub fn int(v: Int) -> Value

@external(erlang, "glua_ffi", "coerce")
pub fn float(v: Float) -> Value

@external(erlang, "glua_ffi", "encode_table")
pub fn table(lua: Lua, values: List(#(Value, Value))) -> #(Lua, Value)

pub fn table_list(lua: Lua, values: List(Value)) -> #(Lua, Value) {
  list.map_fold(values, 1, fn(acc, val) { #(acc + 1, #(int(acc), val)) })
  |> pair.second()
  |> table(lua, _)
}

@external(erlang, "glua_ffi", "encode_userdata")
pub fn userdata(lua: Lua, val: anything) -> #(Lua, Value)

@external(erlang, "glua_ffi", "wrap_fun")
pub fn function(
  fun: fn(Lua, List(Value)) -> Result(#(Lua, List(Value)), #(Lua, List(String))),
) -> Function

/// Downgrade a `Function` to a `Value`
@external(erlang, "glua_ffi", "coerce")
pub fn func_to_val(func: Function) -> Value

/// Creates a new Lua VM instance
@external(erlang, "luerl", "init")
pub fn new() -> Lua

@external(erlang, "glua_ffi", "classify_type")
pub fn typeof(val: Value) -> String

/// List of Lua modules and functions that will be sandboxed by default
pub const default_sandbox = [
  ["io"],
  ["file"],
  ["os", "execute"],
  ["os", "exit"],
  ["os", "getenv"],
  ["os", "remove"],
  ["os", "rename"],
  ["os", "tmpname"],
  ["package"],
  ["load"],
  ["loadfile"],
  ["require"],
  ["dofile"],
  ["loadstring"],
]

/// Creates a new Lua VM instance with sensible modules and functions sandboxed.
///
/// Check `glua.default_sandbox` to see what modules and functions will be sandboxed.
///
/// This function accepts a list of paths to Lua values that will be excluded from being sandboxed,
/// so needed modules or functions can be enabled while keeping sandboxed the rest.
/// In case you want to sandbox more Lua values, pass to `glua.sandbox` the returned Lua state.
pub fn new_sandboxed(
  allow excluded: List(List(String)),
) -> Result(Lua, LuaError) {
  list_substraction(default_sandbox, excluded)
  |> list.try_fold(from: new(), with: sandbox)
}

@external(erlang, "erlang", "--")
fn list_substraction(a: List(a), b: List(a)) -> List(a)

/// Swaps out the value at `keys` with a function that causes a Lua error when called.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(lua) = glua.new() |> glua.sandbox(["os"], ["execute"])
/// let assert Error(glua.LuaRuntimeException(exception, _)) = glua.eval(
///   state: lua,
///   code: "os.execute(\"rm -f important_file\"); return 0",
///   using: decode.int
/// )
/// // 'important_file' was not deleted
/// assert exception == glua.ErrorCall(["os.execute is sandboxed"])
/// ```
pub fn sandbox(state lua: Lua, keys keys: List(String)) -> Result(Lua, LuaError) {
  let msg = string.join(keys, with: ".") <> " is sandboxed"
  let #(fun, lua) = sandbox_fun(lua, msg)
  set(lua, ["_G", ..keys], fun)
}

@external(erlang, "glua_ffi", "sandbox_fun")
fn sandbox_fun(state: Lua, msg: String) -> #(Value, Lua)

/// Gets a private value that is not exposed to the Lua runtime.
///
/// ## Examples
///
/// ```gleam
/// assert glua.new()
///      |> glua.set_private("private_value", "secret_value")
///      |> glua.get_private("private_value", decode.string)
///   == Ok("secret_value")
/// ```
pub fn get_private(
  state lua: Lua,
  key key: String,
) -> Result(dynamic.Dynamic, LuaError) {
  use value <- result.try(do_get_private(lua, key))
  Ok(value)
}

/// Same as `glua.get`, but returns a reference to the value instead of decoding it
pub fn get(state lua: Lua, keys keys: List(String)) -> Result(Value, LuaError) {
  do_get(lua, keys)
}

/// Sets a value in the Lua environment.
///
/// All nested keys will be created as intermediate tables.
///
/// If successfull, this function will return the updated Lua state
/// and the setted value will be available in Lua scripts.
///
/// ## Examples
///
/// ```gleam
/// let #(lua, encoded) = glua.new() |> glua.int(10)
/// let assert Ok(lua) = glua.set(
///   state: lua,
///   keys: ["my_number"],
///   value: encoded
/// )
///
/// glua.get(state: lua, keys: ["my_number"], using: decode.int)
/// // -> Ok(10)
/// ```
///
/// ```gleam
/// let emails = ["jhondoe@example.com", "lucy@example.com"]
/// let #(lua, encoded) = glua.new() |> glua.list(glua.string, emails)
/// let assert Ok(lua) = glua.set(
///   state: lua,
///   keys: ["info", "emails"],
///   value: encoded
/// )
///
/// let assert Ok(#(_, results)) = glua.eval(
///   state: lua,
///   code: "return info.emails",
///   using: decode.string
/// )
///
/// assert results == emails
/// ```
pub fn set(
  state lua: Lua,
  keys keys: List(String),
  value val: Value,
) -> Result(Lua, LuaError) {
  let state = {
    use acc, key <- list.try_fold(keys, #([], lua))
    let #(keys, lua) = acc
    let keys = list.append(keys, [key])
    case do_get(lua, keys) {
      Ok(_) -> Ok(#(keys, lua))

      Error(KeyNotFound) -> {
        let #(tbl, lua) = do_alloc_table([], lua)
        do_set(lua, keys, tbl)
        |> result.map(fn(lua) { #(keys, lua) })
      }

      Error(e) -> Error(e)
    }
  }

  use #(keys, lua) <- result.try(state)
  do_set(lua, keys, val)
}

/// Sets a value that is not exposed to the Lua runtime and can only be accessed from Gleam.
///
/// ## Examples
/// ```gleam
/// assert glua.new()
///        |> glua.set("secret_value", "private_value")
///        |> glua.get("secret_value")
///   == Ok("secret_value")
/// ```
pub fn set_private(state lua: Lua, key key: String, value value: a) -> Lua {
  do_set_private(key, value, lua)
}

/// Sets a group of values under a particular table in the Lua environment.
pub fn set_api(
  lua: Lua,
  keys: List(String),
  values: List(#(String, Value)),
) -> Result(Lua, LuaError) {
  use state, #(key, val) <- list.try_fold(values, lua)
  set(state, list.append(keys, [key]), val)
}

/// Sets the paths where the Lua runtime will look when requiring other Lua files.
///
/// > **Warning**: This function will not work properly if `["package"]` or `["require"]` are sandboxed
/// > in the provided Lua state. If you constructed the Lua state using `glua.new_sandboxed`,
/// > remember to allow the required values by passing `[["package"], ["require"]]` to `glua.new_sandboxed`.
///
/// ## Examples
///
/// ```gleam
/// let my_scripts_paths = ["app/scripts/lua/?.lua"]
/// let assert Ok(state) = glua.set_lua_paths(
///   state: glua.new(),
///   paths: my_scripts_paths
/// )
///
/// let assert Ok(#(_, [result])) = glua.eval(
///   state:,
///   code: "local my_math = require 'my_script'; return my_math.square(3)"
///   using: decode.int
/// )
///
/// assert result = 9
/// ```
pub fn set_lua_paths(
  state lua: Lua,
  paths paths: List(String),
) -> Result(Lua, LuaError) {
  let paths = string.join(paths, with: ";") |> string
  set(lua, ["package", "path"], paths)
}

// TODO: Fix
@external(erlang, "luerl_heap", "alloc_table")
fn do_alloc_table(content: List(a), lua: Lua) -> #(Value, Lua)

@external(erlang, "glua_ffi", "get_private")
fn do_get_private(lua: Lua, key: String) -> Result(dynamic.Dynamic, LuaError)

@external(erlang, "glua_ffi", "get_table_keys")
fn do_get(lua: Lua, keys: List(String)) -> Result(Value, LuaError)

@external(erlang, "glua_ffi", "set_table_keys")
fn do_set(lua: Lua, keys: List(String), val: a) -> Result(Lua, LuaError)

@external(erlang, "luerl", "put_private")
fn do_set_private(key: String, value: a, lua: Lua) -> Lua

/// Remove a private value that is not exposed to the Lua runtime. 
///
/// ## Examples
///
/// ```gleam
/// let lua = glua.set_private(glua.new(), "my_value", "will_be_removed"
/// assert glua.get(lua, "my_value", decode.string) == Ok("will_be_removed")
///
/// assert glua.delete_private(lua, "my_value")
///        |> glua.get("my_value", decode.string)
///   == Error(glua.KeyNotFound)
/// ```
pub fn delete_private(state lua: Lua, key key: String) -> Lua {
  do_delete_private(key, lua)
}

@external(erlang, "luerl", "delete_private")
fn do_delete_private(key: String, lua: Lua) -> Lua

/// Parses a string of Lua code and returns it as a compiled chunk.
///
/// To eval the returned chunk, use `glua.eval_chunk` or `glua.ref_eval_chunk`.
pub fn load(
  state lua: Lua,
  code code: String,
) -> Result(#(Lua, Chunk), LuaError) {
  do_load(lua, code)
}

@external(erlang, "glua_ffi", "load")
fn do_load(lua: Lua, code: String) -> Result(#(Lua, Chunk), LuaError)

/// Parses a Lua source file and returns it as a compiled chunk.
///
/// To eval the returned chunk, use `glua.eval_chunk` or `glua.ref_eval_chunk`.
pub fn load_file(
  state lua: Lua,
  path path: String,
) -> Result(#(Lua, Chunk), LuaError) {
  do_load_file(lua, path)
}

@external(erlang, "glua_ffi", "load_file")
fn do_load_file(lua: Lua, path: String) -> Result(#(Lua, Chunk), LuaError)

/// Same as `glua.eval`, but returns references to the values instead of decode them
pub fn eval(
  state lua: Lua,
  code code: String,
) -> Result(#(Lua, List(Value)), LuaError) {
  do_eval(lua, code)
}

@external(erlang, "glua_ffi", "eval")
fn do_eval(lua: Lua, code: String) -> Result(#(Lua, List(Value)), LuaError)

/// Same as `glua.eval_chunk`, but returns references to the values instead of decode them
pub fn eval_chunk(
  state lua: Lua,
  chunk chunk: Chunk,
) -> Result(#(Lua, List(Value)), LuaError) {
  do_eval_chunk(lua, chunk)
}

@external(erlang, "glua_ffi", "eval_chunk")
fn do_eval_chunk(
  lua: Lua,
  chunk: Chunk,
) -> Result(#(Lua, List(Value)), LuaError)

/// Same as `glua.eval_file`, but returns references to the values instead of decode them.
pub fn eval_file(
  state lua: Lua,
  path path: String,
) -> Result(#(Lua, List(Value)), LuaError) {
  do_eval_file(lua, path)
}

@external(erlang, "glua_ffi", "eval_file")
fn do_eval_file(lua: Lua, path: String) -> Result(#(Lua, List(Value)), LuaError)

@external(erlang, "glua_ffi", "call_function")
fn do_call_function(
  lua: Lua,
  fun: Function,
  args: List(Value),
) -> Result(#(Lua, List(Value)), LuaError)

pub fn call_function(
  state lua: Lua,
  fun fun: Function,
  args args: List(Value),
) -> Result(#(Lua, List(Value)), LuaError) {
  do_call_function(lua, fun, args)
}

/// Same as `glua.call_function_by_name`, but it chains `glua.ref_get` with `glua.ref_call_function` instead of `glua.call_function`
pub fn call_function_by_name(
  state lua: Lua,
  keys keys: List(String),
  args args: List(Value),
) -> Result(#(Lua, List(Value)), LuaError) {
  use fun <- result.try(get(lua, keys))
  call_function(lua, coerce_function(fun), args)
}

@external(erlang, "glua_ffi", "coerce")
fn coerce_function(func: Value) -> Function
