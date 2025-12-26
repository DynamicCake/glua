//// A library to embed Lua in Gleam programs.
////
//// Gleam wrapper around [Luerl](https://github.com/rvirding/luerl).

import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/order
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
  /// The value returned by the Lua environment could not be decoded using the provided decoder.
  UnexpectedResultType(List(decode.DecodeError))
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
  /// An exception that could not be identified
  UnknownException
}

/// The exception that happens when a functi
/// Represents a chunk of Lua code that is already loaded into the Lua VM
pub type Chunk

/// Represents a value that can be passed to the Lua environment.
pub type Value

/// Represents a reference to a value inside the Lua environment.
///
/// Each one of the functions that returns values from the Lua environment has a `ref_` counterpart
/// that will return references to the values instead of decoding them.
pub type ValueRef

/// A decoded lua function
/// Accepts decoded values and returns decoded values
pub type LuaFunc {
  LuaFunc(func: fn(Lua, List(Value)) -> #(Lua, List(Value)))
}

@external(erlang, "glua_ffi", "is_lua_func")
fn is_lua_func(a: anything) -> Bool

@external(erlang, "glua_ffi", "coerce")
fn coerce_lua_func(a: anything) -> LuaFunc

pub fn decode_func() {
  use then <- decode.then(decode.dynamic)
  echo then
  case is_lua_func(then) {
    True -> decode.success(coerce_lua_func(then))
    False ->
      decode.failure(
        LuaFunc(func: fn(_, _) { panic as "unreachable" }),
        "LuaFunc",
      )
  }
}

// {NewState, Ret} = Fun(State, Transformed),

pub opaque type Output {
  Output(lua: Lua, refs: List(ValueRef))
}

pub opaque type GetOutput {
  SingleOutput(lua: Lua, ref: ValueRef)
}

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

@external(erlang, "glua_ffi", "coerce")
pub fn table(values: List(#(Value, Value))) -> Value

/// Only pure lists work
pub fn table_to_list(dict: dict.Dict(Int, a)) -> Result(List(a), Nil) {
  use <- bool.guard(dict.is_empty(dict), Ok([]))
  let indexes =
    list.sort(dict.keys(dict), fn(a, b) { int.compare(a, b) |> order.negate })
  use state <- result.try(inner_t2l(dict, indexes, FirstT2LState))
  let assert TableToListState(prev, out) = state
  use <- bool.guard(prev != 1, Error(Nil))
  Ok(out)
}

pub type TableToListState(a) {
  FirstT2LState
  TableToListState(prev: Int, out: List(a))
}

/// Pre-conditions
/// 1. `items` cannot be empty
/// 2. `items` must be sorted in reverse int order
/// 3. `items` must contain and only contain the keys of `dict`
/// 4. During first call to function, `state` must be FirstTableToListState
fn inner_t2l(
  dict: dict.Dict(Int, a),
  items: List(Int),
  state: TableToListState(a),
) -> Result(TableToListState(a), Nil) {
  case items {
    [idx, ..other] -> {
      let assert Ok(item) = dict.get(dict, idx)
      case state {
        FirstT2LState ->
          inner_t2l(dict, other, TableToListState(prev: idx, out: [item]))
        TableToListState(prev:, out:) ->
          case idx == prev - 1 {
            True -> {
              inner_t2l(
                dict,
                other,
                TableToListState(prev: idx, out: [item, ..out]),
              )
            }
            False -> Error(Nil)
          }
      }
    }
    [] -> Ok(state)
  }
}

pub fn decode_table_list(decoder: decode.Decoder(a)) -> decode.Decoder(List(a)) {
  use list <- decode.then(decode.dict(decode.int, decoder))
  case table_to_list(list) {
    Ok(list) -> decode.success(list)
    Error(Nil) -> decode.failure([], "Table List")
  }
}

pub fn alloc_table(lua: Lua, values: List(#(Value, Value))) -> #(Lua, Value) {
  let #(val, lua) = do_alloc_table(values, lua)
  #(lua, val)
}

pub fn alloc_userdata(lua: Lua, a: anything) -> #(Lua, Value) {
  let #(val, lua) = do_alloc_userdata(a, lua)
  #(lua, val)
}

pub fn table_decoder(
  keys_decoder: decode.Decoder(a),
  values_decoder: decode.Decoder(b),
) -> decode.Decoder(List(#(a, b))) {
  let inner = {
    use key <- decode.field(0, keys_decoder)
    use val <- decode.field(1, values_decoder)
    decode.success(#(key, val))
  }

  decode.list(of: inner)
}

pub fn function(
  f: fn(Lua, List(dynamic.Dynamic)) -> #(Lua, List(Value)),
) -> Value {
  // we need a little wrapper for functions to satisfy luerl's order of arguments and return value type
  function_transform(f, default_transformation)
}

pub fn function_transform(
  f: fn(Lua, List(dynamic.Dynamic)) -> #(Lua, List(Value)),
  transformation: Transformation,
) -> Value {
  wrap_function(f, transformation)
  // we need a little wrapper for functions to satisfy luerl's order of arguments and return value type
}

pub fn list(encoder: fn(a) -> Value, values: List(a)) -> List(Value) {
  list.map(values, encoder)
}

/// Encodes any Gleam value as a reference that can be passed to a Lua program.
///
/// Deferencing a userdata value inside Lua code will cause a Lua exception.
///
/// ## Examples
///
/// ```gleam
/// pub type User {
///   User(name: String, is_admin: Bool)
/// }
///
/// let user_decoder = {
///   use name <- decode.field(1, decode.string)
///   use is_admin <- decode.field(2, decode.bool)
///   decode.success(User(name:, is_admin:))
/// }
///
/// let state = glua.new()
/// let assert Ok(state) = glua.set(
///   state:,
///   keys: ["a_user"],
///   value: glua.userdata(User(name: "Jhon Doe", is_admin: False))
/// )
///
/// let assert Ok(#(_, [result])) = glua.eval(state:, code: "return a_user", using: user_decoder)
/// assert result == User("Jhon Doe", False)
/// ```
///
/// ```gleam
/// pub type Person {
///   Person(name: String, email: String)
/// }
///
/// let state = glua.new()
/// let assert Ok(lua) = glua.set(
///   state:,
///   keys: ["lucy"],
///   value: glua.userdata(Person(name: "Lucy", email: "lucy@example.com"))
/// )
///
/// let assert Error(glua.LuaRuntimeException(glua.IllegalIndex(_), _)) =
///   glua.eval(state:, code: "return lucy.email", using: decode.string)
/// ```
@external(erlang, "glua_ffi", "coerce_userdata")
pub fn userdata(v: anything) -> Value

@external(erlang, "glua_ffi", "wrap_fun")
fn wrap_function(
  fun: fn(Lua, List(dynamic.Dynamic)) -> #(Lua, List(Value)),
  transformation: Transformation,
) -> Value

@external(erlang, "luerl", "decode_list")
fn do_decode_list(ref: List(ValueRef), lua: Lua) -> List(dynamic.Dynamic)

@external(erlang, "luerl", "decode")
fn do_decode(ref: ValueRef, lua: Lua) -> dynamic.Dynamic

/// Precondition: ONLY luerl decoded terms can be added here or else it will SPECTACULARLY FAIL
@external(erlang, "glua_ffi", "transform")
fn transform(a: luerl_value, transformation: Transformation) -> dynamic.Dynamic

/// Internally in luerl, when a `Value` is decoded it has proplists and difficult to decode functions.
/// This poses some problems; proplists make decoding difficult and erlang functions are hard to decode in gleam.
///
/// Glua has decided to greatly improve type safety at a marginal performance cost.
/// If you wish to remove these transformations for whatever reason, you may use the functions that accepts a `Transformation`.
pub type Transformation {
  Transformation(
    /// If true, transforms proplists (ex. `[#("key", "value")]`) into dicts
    proplist: Bool,
    /// If true, transforms functions to `LuaFunc`s making them callable with glua
    func: Bool,
  )
}

pub const default_transformation = Transformation(proplist: True, func: True)

/// The decoder will always receive a list of values
pub fn dec(
  output: Result(Output, LuaError),
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, a), LuaError) {
  dec_transform(output, decoder, default_transformation)
}

pub fn dec_transform(
  output: Result(Output, LuaError),
  using decoder: decode.Decoder(a),
  transformation tf: Transformation,
) {
  use output <- result.try(output)
  do_decode_list(output.refs, output.lua)
  |> list.map(transform(_, tf))
  |> dynamic.list()
  |> decode.run(decoder)
  |> result.map(pair.new(output.lua, _))
  |> result.map_error(UnexpectedResultType)
}

/// Assume there will be only one item to decode
pub fn dec_one(
  output: Result(Output, LuaError),
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, a), LuaError) {
  dec_one_transform(output, decoder, default_transformation)
}

/// Assume there will be only one item to decode
pub fn dec_one_transform(
  output: Result(Output, LuaError),
  using decoder: decode.Decoder(a),
  transformation tf: Transformation,
) -> Result(#(Lua, a), LuaError) {
  dec_transform(
    output,
    {
      use it <- decode.field(0, decoder)
      decode.success(it)
    },
    tf,
  )
}

/// GetOuput decoder
pub fn gdec(
  output: Result(GetOutput, LuaError),
  using decoder: decode.Decoder(a),
) -> Result(a, LuaError) {
  use output <- result.try(output)
  do_decode(output.ref, output.lua)
  |> decode.run(decoder)
  |> result.map_error(UnexpectedResultType)
}

fn to_output(pair: #(Lua, List(ValueRef))) -> Output {
  Output(lua: pair.0, refs: pair.1)
}

pub fn output_pair(output: Output) -> #(Lua, List(ValueRef)) {
  #(output.lua, output.refs)
}

pub fn ref(output: GetOutput) {
  output.ref
}

/// Creates a new Lua VM instance
@external(erlang, "luerl", "init")
pub fn new() -> Lua

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
  set(lua, ["_G", ..keys], sandbox_fun(msg))
}

@external(erlang, "glua_ffi", "sandbox_fun")
fn sandbox_fun(msg: String) -> Value

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
  using decoder: decode.Decoder(a),
) -> Result(a, LuaError) {
  use value <- result.try(do_get_private(lua, key))
  use decoded <- result.try(
    decode.run(value, decoder) |> result.map_error(UnexpectedResultType),
  )

  Ok(decoded)
}

pub fn get(
  state lua: Lua,
  keys keys: List(String),
) -> Result(GetOutput, LuaError) {
  do_ref_get(lua, keys)
  |> result.map(SingleOutput(lua, _))
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
    case do_ref_get(lua, keys) {
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

@external(erlang, "luerl_heap", "alloc_table")
fn do_alloc_table(content: List(a), lua: Lua) -> #(Value, Lua)

@external(erlang, "luerl_heap", "alloc_userdata")
fn do_alloc_userdata(a: anything, lua: Lua) -> #(Value, Lua)

@external(erlang, "glua_ffi", "get_private")
fn do_get_private(lua: Lua, key: String) -> Result(dynamic.Dynamic, LuaError)

@external(erlang, "glua_ffi", "get_table_keys")
fn do_ref_get(lua: Lua, keys: List(String)) -> Result(ValueRef, LuaError)

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

pub fn eval(state lua: Lua, code code: String) -> Result(Output, LuaError) {
  use #(lua, val) <- result.try(do_eval(lua, code))
  Output(lua, val)
  |> Ok
}

@external(erlang, "glua_ffi", "eval")
fn do_eval(lua: Lua, code: String) -> Result(#(Lua, List(ValueRef)), LuaError)

pub fn eval_chunk(
  state lua: Lua,
  chunk chunk: Chunk,
) -> Result(Output, LuaError) {
  do_eval_chunk(lua, chunk)
  |> result.map(to_output)
}

@external(erlang, "glua_ffi", "eval_chunk")
fn do_eval_chunk(
  lua: Lua,
  chunk: Chunk,
) -> Result(#(Lua, List(ValueRef)), LuaError)

pub fn eval_file(state lua: Lua, path path: String) -> Result(Output, LuaError) {
  do_eval_file(lua, path)
  |> result.map(to_output)
}

@external(erlang, "glua_ffi", "eval_file")
fn do_eval_file(
  lua: Lua,
  path: String,
) -> Result(#(Lua, List(ValueRef)), LuaError)

pub fn call_function_ref(
  state lua: Lua,
  ref fun: ValueRef,
  args args: List(Value),
) -> Result(Output, LuaError) {
  do_call_function_ref(lua, fun, args)
  |> result.map(to_output)
}

@external(erlang, "glua_ffi", "call_function")
fn do_call_function_ref(
  lua: Lua,
  fun: ValueRef,
  args: List(Value),
) -> Result(#(Lua, List(ValueRef)), LuaError)

@external(erlang, "glua_ffi", "call_enc_function")
fn do_call_function(
  lua: Lua,
  fun: LuaFunc,
  args: List(Value),
) -> Result(#(Lua, List(ValueRef)), LuaError)

pub fn call_function(
  state lua: Lua,
  func func: LuaFunc,
  args args: List(Value),
) -> Result(Output, LuaError) {
  do_call_function(lua, func, args)
  |> result.map(to_output)
}

pub fn call_function_by_name(
  state lua: Lua,
  keys keys: List(String),
  args args: List(Value),
) -> Result(Output, LuaError) {
  use fun <- result.try(get(lua, keys))
  call_function_ref(lua, fun.ref, args)
}
