import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/pair
import gleam/result

/// Represents an instance of the Lua VM.
pub type Lua

/// Represents the errors than can happend during the parsing and execution of Lua code
pub type LuaError {
  UnexpectedResultType(List(decode.DecodeError))
  UnknownError
}

/// Represents a chunk of Lua code that is already loaded into the Lua VM
pub type Chunk

/// Represents a value that can be passed to the Lua environment.
pub type Value

/// Represents a reference to a value inside the Lua environment.
///
/// Each one of the functions that returns values from the Lua environment has a `ref_` counterpart
/// that will return references to the values instead of decoding them.
pub type ValueRef

@external(erlang, "glua_ffi", "lua_nil")
pub fn nil(lua: Lua) -> #(Lua, Value)

pub fn string(lua: Lua, v: String) -> #(Lua, Value) {
  encode(lua, v)
}

pub fn bool(lua: Lua, v: Bool) -> #(Lua, Value) {
  encode(lua, v)
}

pub fn int(lua: Lua, v: Int) -> #(Lua, Value) {
  encode(lua, v)
}

pub fn float(lua: Lua, v: Float) -> #(Lua, Value) {
  encode(lua, v)
}

pub fn table(
  lua: Lua,
  encoders: #(fn(Lua, a) -> #(Lua, Value), fn(Lua, b) -> #(Lua, Value)),
  values: List(#(a, b)),
) -> #(Lua, Value) {
  let #(key_encoder, value_encoder) = encoders
  let #(lua, values) =
    list.map_fold(values, lua, fn(lua, pair) {
      let #(lua, k) = key_encoder(lua, pair.0)
      let #(lua, v) = value_encoder(lua, pair.1)
      #(lua, #(k, v))
    })

  encode(lua, values)
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
  lua: Lua,
  f: fn(Lua, List(Dynamic)) -> #(Lua, List(Value)),
) -> #(Lua, Value) {
  // wrapper to satisfy luerl's order of arguments and return value
  let fun = fn(args, lua) { f(lua, decode_list(args, lua)) |> pair.swap }

  encode(lua, fun)
}

pub fn list(
  lua: Lua,
  encoder: fn(Lua, a) -> #(Lua, Value),
  values: List(a),
) -> #(Lua, List(Value)) {
  list.map_fold(values, lua, encoder)
}

@external(erlang, "glua_ffi", "encode")
fn encode(lua: Lua, v: anything) -> #(Lua, Value)

/// Creates a new Lua VM instance
@external(erlang, "luerl", "init")
pub fn new() -> Lua

/// Gets a value in the Lua environment.
///
/// ## Examples
///
/// ```gleam
/// glua.get(state: glua.new(), keys: ["_VERSION"], using: decode.string)
/// // -> Ok("Lua 5.3")
/// ```
///
/// ```gleam
/// let #(lua, encoded) = glua.new() |> glua.bool(True)
/// let assert Ok(lua) = glua.set(state: lua, keys: ["my_table", "my_value"], value: encoded)
/// glua.get(state: lua, keys: ["my_table", "my_value"], using: decode.bool)
/// // -> Ok(True)
/// ```
///
/// ```gleam
/// glua.get(state: glua.new(), keys: ["non_existent"], using: decode.string)
/// // -> Error(NonExistentValue)
/// ```
pub fn get(
  state lua: Lua,
  keys keys: List(String),
  using decoder: decode.Decoder(a),
) -> Result(a, LuaError) {
  let #(keys, lua) = encode_list(keys, lua)

  use value <- result.try(
    do_get(lua, keys) |> result.map_error(parse_lua_error),
  )

  use decoded <- result.try(
    decode.run(value, decoder) |> result.map_error(UnexpectedResultType),
  )

  Ok(decoded)
}

/// Same as `glua.get`, but returns a reference to the value instead of decoding it
pub fn ref_get(
  lua lua: Lua,
  keys keys: List(String),
) -> Result(ValueRef, LuaError) {
  let #(keys, lua) = encode_list(keys, lua)

  do_ref_get(lua, keys) |> result.map_error(parse_lua_error)
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
/// let assert Ok(lua) = glua.set(state: lua, keys: ["my_number"], value: encoded)
/// glua.get(state: lua, keys: ["my_number"], using: decode.int)
/// // -> Ok(10)
/// ```
///
/// ```gleam
/// let emails = ["jhondoe@example.com", "lucy@example.com"]
/// let #(lua, encoded) = glua.new() |> glua.list(glua.string, emails)
/// let assert Ok(lua) = glua.set(state: lua, keys: ["info", "emails"], value: encoded)
/// let assert Ok(#(_, results)) = glua.eval(state: lua, code: "return info.emails", using: decode.string)
/// assert results == emails
/// ```
pub fn set(
  state lua: Lua,
  keys keys: List(String),
  value val: a,
) -> Result(Lua, LuaError) {
  let encoded = encode_list(keys, lua)
  let state = {
    use acc, key <- list.try_fold(encoded.0, #([], encoded.1))
    let #(keys, lua) = acc
    let keys = list.append(keys, [key])
    case do_get(lua, keys) {
      Ok(_) -> Ok(#(keys, lua))
      _ -> {
        let #(tbl, lua) = alloc_table([], lua)
        case do_set(lua, keys, tbl) {
          Ok(lua) -> Ok(#(keys, lua))
          Error(e) -> Error(parse_lua_error(e))
        }
      }
    }
  }
  use #(keys, lua) <- result.try(state)
  do_set(lua, keys, val) |> result.map_error(parse_lua_error)
}

pub fn set_api(
  lua: Lua,
  keys: List(String),
  values: List(#(String, Value)),
) -> Result(Lua, LuaError) {
  use state, #(key, val) <- list.try_fold(values, lua)
  set(state, list.append(keys, [key]), val)
}

@external(erlang, "luerl", "encode_list")
fn encode_list(keys: List(String), lua: Lua) -> #(List(Dynamic), Lua)

@external(erlang, "luerl", "decode_list")
fn decode_list(keys: List(a), lua: Lua) -> List(Dynamic)

@external(erlang, "luerl_emul", "alloc_table")
fn alloc_table(content: List(a), lua: Lua) -> #(a, Lua)

@external(erlang, "glua_ffi", "get_table_keys_dec")
fn do_get(lua: Lua, keys: List(Dynamic)) -> Result(Dynamic, Dynamic)

@external(erlang, "glua_ffi", "get_table_keys")
fn do_ref_get(lua: Lua, keys: List(Dynamic)) -> Result(ValueRef, Dynamic)

@external(erlang, "glua_ffi", "set_table_keys")
fn do_set(lua: Lua, keys: List(Dynamic), val: a) -> Result(Lua, Dynamic)

/// Parses a string of Lua code and returns it as a compiled chunk.
///
/// To eval the returned chunk, use `glua.eval_chunk` or `glua.ref_eval_chunk`.
pub fn load(
  state lua: Lua,
  code code: String,
) -> Result(#(Lua, Chunk), LuaError) {
  do_load(lua, code) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "load")
fn do_load(lua: Lua, code: String) -> Result(#(Lua, Chunk), Dynamic)

/// Parses a Lua source file and returns it as a compiled chunk.
///
/// To eval the returned chunk, use `glua.eval_chunk` or `glua.ref_eval_chunk`.
pub fn load_file(
  state lua: Lua,
  path path: String,
) -> Result(#(Lua, Chunk), LuaError) {
  do_load_file(lua, path) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "load_file")
fn do_load_file(lua: Lua, path: String) -> Result(#(Lua, Chunk), Dynamic)

/// Evaluates a string of Lua code.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(#(_, results)) = glua.eval(state: glua.new(), code: "return 1 + 2", using: decode.int)
/// assert results == [3]
/// ```
///
/// ```gleam
/// let my_decoder = decode.one_of(decode.string, or: [decode.int |> decode.map(int.to_string)])
/// let assert Ok(#(_, results)) = glua.eval(glua.new(), "return 'hello, world!', 10", using: my_decoder)
/// assert results == ["hello, world!", "10"]
/// ```
///
/// ```gleam
/// glua.eval(state: glua.new(), code: "return 1 * ", using: decode.int)
/// // -> Error(SyntaxError)
/// ```
///
/// ```gleam
/// glua.eval(state: glua.new(), code: "return 'Hello, world!'", using: decode.int)
/// // -> Error(UnexpectedResultType())
/// ```
///
/// Note: If you are evaluating the same piece of code multiple times,
/// instead of calling `glua.eval` repeatly it is recommended to first convert
/// the code to a chunk by passing it to `glua.load`, and then
/// evaluate that chunk using `glua.eval_chunk` or `glua.ref_eval_chunk`.
pub fn eval(
  state lua: Lua,
  code code: String,
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, List(a)), LuaError) {
  use #(lua, ret) <- result.try(
    do_eval(lua, code) |> result.map_error(parse_lua_error),
  )
  use decoded <- result.try(
    list.try_map(ret, decode.run(_, decoder))
    |> result.map_error(UnexpectedResultType),
  )

  Ok(#(lua, decoded))
}

@external(erlang, "glua_ffi", "eval_dec")
fn do_eval(lua: Lua, code: String) -> Result(#(Lua, List(Dynamic)), Dynamic)

/// Same as `glua.eval`, but returns references to the values instead of decode them
pub fn ref_eval(
  state lua: Lua,
  code code: String,
) -> Result(#(Lua, List(ValueRef)), LuaError) {
  do_ref_eval(lua, code) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "eval")
fn do_ref_eval(
  lua: Lua,
  code: String,
) -> Result(#(Lua, List(ValueRef)), Dynamic)

/// Evaluates a compiled chunk of Lua code.
///
/// ## Examples
/// ```gleam
/// let assert Ok(#(lua, chunk)) = glua.load(state: glua.new(), code: "return 'hello, world!'")
/// let assert Ok(#(_, results)) = glua.eval_chunk(state: lua, chunk: chunk, using: decode.string)
/// assert results == ["hello, world!"]
/// ```
pub fn eval_chunk(
  state lua: Lua,
  chunk chunk: Chunk,
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, List(a)), LuaError) {
  use #(lua, ret) <- result.try(
    do_eval_chunk(lua, chunk) |> result.map_error(parse_lua_error),
  )
  use decoded <- result.try(
    list.try_map(ret, decode.run(_, decoder))
    |> result.map_error(UnexpectedResultType),
  )

  Ok(#(lua, decoded))
}

@external(erlang, "glua_ffi", "eval_chunk_dec")
fn do_eval_chunk(
  lua: Lua,
  chunk: Chunk,
) -> Result(#(Lua, List(Dynamic)), Dynamic)

/// Same as `glua.eval_chunk`, but returns references to the values instead of decode them
pub fn ref_eval_chunk(
  state lua: Lua,
  chunk chunk: Chunk,
) -> Result(#(Lua, List(ValueRef)), LuaError) {
  do_ref_eval_chunk(lua, chunk) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "eval_chunk")
fn do_ref_eval_chunk(
  lua: Lua,
  chunk: Chunk,
) -> Result(#(Lua, List(ValueRef)), Dynamic)

/// Evaluates a Lua source file.
///
/// ## Examples
/// ```gleam
/// let assert Ok(#(_, results)) = glua.eval_file(state: glua.new(), path: "path/to/hello.lua", using: decode.string)
/// assert results == ["hello, world!"]
/// ```
pub fn eval_file(
  state lua: Lua,
  path path: String,
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, List(a)), LuaError) {
  use #(lua, ret) <- result.try(
    do_eval_file(lua, path) |> result.map_error(parse_lua_error),
  )
  use decoded <- result.try(
    list.try_map(ret, decode.run(_, decoder))
    |> result.map_error(UnexpectedResultType),
  )

  Ok(#(lua, decoded))
}

@external(erlang, "glua_ffi", "eval_file_dec")
fn do_eval_file(
  lua: Lua,
  path: String,
) -> Result(#(Lua, List(Dynamic)), Dynamic)

/// Same as `glua.eval_file`, but returns references to the values instead of decode them.
pub fn ref_eval_file(
  state lua: Lua,
  path path: String,
) -> Result(#(Lua, List(ValueRef)), LuaError) {
  do_ref_eval_file(lua, path) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "eval_file")
fn do_ref_eval_file(
  lua: Lua,
  path: String,
) -> Result(#(Lua, List(ValueRef)), Dynamic)

/// Calls a Lua function by reference.
///
/// ## Examples
/// ```gleam
/// let assert Ok(#(lua, fun)) = glua.ref_eval(state: glua.new(), code: "return math.sqrt")
/// let #(lua, encoded) = glua.int(lua, 81)
/// let assert Ok(#(_, [result])) = glua.call_function(state: lua, ref: fun, args: [encoded], using: decode.int)
/// assert result == 9
/// ```
///
/// ```gleam
/// let code = "function fib(n)
///   if n <= 1 then
///     return n
///   else
///     return fib(n - 1) + fib(n - 2)
///   end
///end
///
///return fib
///"
/// let assert Ok(#(lua, fun)) = glua.ref_eval(state: glua.new(), code:)
/// let #(lua, encoded) = glua.int(lua, 10)
/// let assert Ok(#(_, [result])) = glua.call_function(state: lua, ref: fun, args: [encoded], using: decode.int)
/// assert result == 55 
/// ```
pub fn call_function(
  state lua: Lua,
  ref fun: ValueRef,
  args args: List(Value),
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, List(a)), LuaError) {
  use #(lua, ret) <- result.try(
    do_call_function(lua, fun, args) |> result.map_error(parse_lua_error),
  )
  use decoded <- result.try(
    list.try_map(ret, decode.run(_, decoder))
    |> result.map_error(UnexpectedResultType),
  )

  Ok(#(lua, decoded))
}

@external(erlang, "glua_ffi", "call_function_dec")
fn do_call_function(
  lua: Lua,
  fun: ValueRef,
  args: List(Value),
) -> Result(#(Lua, List(Dynamic)), Dynamic)

/// Same as `glua.call_function`, but returns references to the values instead of decode them.
pub fn ref_call_function(
  state lua: Lua,
  ref fun: ValueRef,
  args args: List(Value),
) -> Result(#(Lua, List(ValueRef)), LuaError) {
  do_ref_call_function(lua, fun, args) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "call_function")
fn do_ref_call_function(
  lua: Lua,
  fun: ValueRef,
  args: List(Value),
) -> Result(#(Lua, List(ValueRef)), Dynamic)

/// Gets a reference to the function at `keys`, then inmediatly calls it with the provided `args`.
///
/// This is a shorthand for `glua.ref_get` followed by `glua.call_function`.
///
/// ## Examples
///
/// ```gleam
/// let #(lua, encoded) = glua.new() |> glua.string("hello from gleam!")
/// let assert Ok(#(_, [s])) = glua.call_function_by_name(
///   state: lua,
///   keys: ["string", "upper"],
///   args: [encoded],
///   using: decode.string
/// )
/// assert s == "HELLO FROM GLEAM!" 
/// ```
pub fn call_function_by_name(
  state lua: Lua,
  keys keys: List(String),
  args args: List(Value),
  using decoder: decode.Decoder(a),
) -> Result(#(Lua, List(a)), LuaError) {
  use fun <- result.try(ref_get(lua, keys))
  call_function(lua, fun, args, decoder)
}

/// Same as `glua.call_function_by_name`, but it chains `glua.ref_get` with `glua.ref_call_function` instead of `glua.call_function`
pub fn ref_call_function_by_name(
  state lua: Lua,
  keys keys: List(String),
  args args: List(Value),
) -> Result(#(Lua, List(ValueRef)), LuaError) {
  use fun <- result.try(ref_get(lua, keys))
  ref_call_function(lua, fun, args)
}

// TODO: Actual error parsing
fn parse_lua_error(_err: Dynamic) -> LuaError {
  UnknownError
}
