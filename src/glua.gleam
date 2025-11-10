import gleam/dynamic.{type Dynamic}
import gleam/result

/// Represents an instance of the Lua VM.
pub type Lua

/// Represents the errors than can happend during the parsing and execution of Lua code
pub type LuaError {
  UnknownError
}

/// Represents a chunk of Lua code that is already loaded into the Lua VM
pub type Chunk

/// Represents a Lua table.
pub type Table

/// Represents a Lua function.
pub type Function

/// Creates a new Lua VM instance
@external(erlang, "luerl", "init")
pub fn new() -> Lua

/// Parses a string of Lua code and returns it as a compiled chunk.
///
/// To eval the returned chunk, use `glua.eval_chunk`.
pub fn load(lua lua: Lua, code code: String) -> Result(#(Chunk, Lua), LuaError) {
  do_load(lua, code) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "load")
fn do_load(lua lua: Lua, code code: String) -> Result(#(Chunk, Lua), Dynamic)

/// Parses a Lua source file and returns it as a compiled chunk.
///
/// To eval the returned chunk, use `glua.eval_chunk`.
pub fn load_file(
  lua lua: Lua,
  code code: String,
) -> Result(#(Chunk, Lua), LuaError) {
  do_load_file(lua, code) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "load_file")
fn do_load_file(
  lua lua: Lua,
  code code: String,
) -> Result(#(Chunk, Lua), Dynamic)

/// Evaluates a string of Lua code.
///
/// ## Examples
///
/// ```gleam
/// eval(new(), "return 1 + 2")
/// // -> Ok(#([3], Lua))
/// ```
///
/// ```gleam
/// eval(new(), "return 'hello, world!', 10")
/// // -> Ok(#(["hello, world!", 10], Lua))
/// ```
///
/// ```gleam
/// eval(new(), "return 1 * ")
/// // -> Error(SyntaxError)
/// ```
/// Note: If you are evaluating the same piece of code multiple times,
/// instead of calling `glua.eval` repeatly it is recommended to first convert
/// the code to a chunk by passing it to `glua.load`, and then
/// evaluate that chunk using `glua.eval_chunk`
pub fn eval(
  lua lua: Lua,
  code code: String,
) -> Result(#(List(Dynamic), Lua), LuaError) {
  do_eval(lua, code) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "eval")
fn do_eval(
  lua lua: Lua,
  code code: String,
) -> Result(#(List(Dynamic), Lua), Dynamic)

/// Evaluates a compiled chunk of Lua code.
///
/// ## Examples
/// ```gleam
/// let assert Ok(#(chunk, lua)) = load(glua.new(), "return 'hello, world!'")
/// eval(lua, chunk)
/// -> Ok(#(["hello, world!"], Lua)) 
/// ```
pub fn eval_chunk(
  lua lua: Lua,
  chunk chunk: Chunk,
) -> Result(#(List(Dynamic), Lua), LuaError) {
  do_eval_chunk(lua, chunk) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "eval_chunk")
fn do_eval_chunk(
  lua lua: Lua,
  chunk chunk: Chunk,
) -> Result(#(List(Dynamic), Lua), Dynamic)

/// Evaluates a Lua source file.
///
/// ## Examples
/// ```gleam
/// eval_file(new(), "path/to/hello.lua")
/// Ok(#(["hello, world!"], Lua))
/// ```
pub fn eval_file(
  lua lua: Lua,
  path path: String,
) -> Result(#(List(Dynamic), Lua), LuaError) {
  do_eval_file(lua, path) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "eval_file")
fn do_eval_file(
  lua: Lua,
  path: String,
) -> Result(#(List(Dynamic), Lua), Dynamic)

/// Calls a Lua function by reference.
///
/// ## Examples
/// ```gleam
/// let assert Ok(#(fun, lua)) = glua.eval(glua.new(), "return math.sqrt")
/// let assert Ok(#([result], _)) = glua.call_function(lua, fun, [49])
/// let decoded = decode.run(result, decode.int)
/// assert decoded == Ok(49)
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
/// let assert Ok(#(fun, lua)) = glua.eval(glua.new(), code)
/// let assert Ok(#([result], _)) = glua.call_function(lua, fun, [10])
/// let decoded = decode.run(result, decode.int)
/// assert decoded == Ok(55)
/// ```
pub fn call_function(
  lua lua: Lua,
  fun fun: Dynamic,
  args args: List(a),
) -> Result(#(List(Dynamic), Lua), LuaError) {
  do_call_function(lua, fun, args) |> result.map_error(parse_lua_error)
}

@external(erlang, "glua_ffi", "call_function")
fn do_call_function(
  lua: Lua,
  fun: Dynamic,
  args: List(a),
) -> Result(#(List(Dynamic), Lua), Dynamic)

// TODO: Actual error parsing
fn parse_lua_error(_err: Dynamic) -> LuaError {
  UnknownError
}
