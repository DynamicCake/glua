import deser
import gleam/dict
import gleam/float
import gleam/list
import glua

/// This file contains all the tests in the readme
pub fn eval_test() {
  let code =
    "
function greet()
  return 'Hello from Lua!'
end

return greet()
"

  // Make a fresh instance lua instance
  let lua = glua.new()
  let assert Ok(#(_state, [result])) = glua.eval(state: lua, code:)

  assert result == glua.string("Hello from Lua!")
}

pub type Project {
  Project(name: String, language: String)
}

pub fn deser_test() {
  let code = "return { name = 'glua', written_in = 'gleam'}"
  let assert Ok(#(lua, [table])) = glua.eval(glua.new(), code)

  let deserializer = {
    use name <- deser.field(glua.string("name"), deser.string)
    use language <- deser.field(glua.string("written_in"), deser.string)
    deser.success(Project(name:, language:))
  }

  let assert Ok(#(_lua, project)) = deser.run(lua, table, deserializer)
  assert project == Project(name: "glua", language: "gleam")
}

pub fn parse_execute_chunk() {
  let code = "return 'this is a chunk of Lua code'"
  let assert Ok(#(state, chunk)) = glua.load(state: glua.new(), code:)
  let assert Ok(#(_state, [result])) = glua.eval_chunk(state:, chunk:)

  assert result == glua.string("this is a chunk of Lua code")
}

pub fn exec_lua_files_test() {
  let assert Ok(#(_state, [n, m])) =
    glua.eval_file(state: glua.new(), path: "./test/lua/two_numbers.lua")

  assert n == glua.int(1) && m == glua.int(2)
}

pub fn sandboxing_test() {
  let assert Ok(lua) = glua.new() |> glua.sandbox(["os", "execute"])
  let assert Error(glua.LuaRuntimeException(exception, _)) =
    glua.eval(state: lua, code: "os.execute('rm -f important_file')")

  // 'important_file' was not deleted
  assert exception == glua.ErrorCall(["os.execute is sandboxed"])
}

pub fn get_global_values() {
  let assert Ok(version) = glua.get(state: glua.new(), keys: ["_VERSION"])

  assert version == glua.string("Lua 5.3")
}

pub fn set_global_values() {
  let lua = glua.new()
  // `keys` is the full path to where the value will be set
  // and any intermediate table will be created if it is not present
  let keys = ["my_table", "my_value"]
  let assert Ok(lua) =
    glua.set(state: lua, keys:, value: glua.string("my_value"))

  // now we can get the value
  let assert Ok(value) = glua.get(state: lua, keys:)

  // or return it from a Lua script
  let assert Ok(#(_lua, [returned])) =
    glua.eval(state: lua, code: "return my_table.my_value")

  assert value == glua.string("my_value")
  assert returned == glua.string("my_value")
}

pub fn table_deocding_test() {
  // we can also encode a list of tuples as a table to set it in Lua
  let my_table = [
    #(glua.string("my_first_value"), glua.float(1.2)),
    #(glua.string("my_second_value"), glua.float(2.1)),
  ]

  let #(lua, encoded) = glua.table(glua.new(), my_table)
  let assert Ok(lua) = glua.set(state: lua, keys: ["my_table"], value: encoded)

  // now we can get its values
  let assert Ok(#(lua, [result])) =
    glua.eval(state: lua, code: "return my_table.my_second_value")

  let assert Ok(#(lua, 2.1)) = deser.run(lua, result, deser.number)
  // or we can get the whole table and decode it back to a list of tuples
  let assert Ok(table) = glua.get(state: lua, keys: ["my_table"])
  let assert Ok(#(_lua, table)) =
    deser.run(lua, table, deser.dict(deser.string, deser.number))

  assert table
    == dict.from_list([#("my_first_value", 1.2), #("my_second_value", 2.1)])
}

pub fn call_functions_test() {
  let lua = glua.new()
  let assert Ok(val) = glua.get(state: lua, keys: ["math", "max"])
  let assert Ok(#(lua, fun)) = deser.run(lua, val, deser.function)
  let args = [1, 20, 7, 18] |> list.map(glua.int)

  let assert Ok(#(lua, [result])) =
    glua.call_function(state: lua, fun: fun, args:)

  let assert Ok(#(lua, result)) = deser.run(lua, result, deser.number)

  assert result == 20.0

  // `glua.call_function_by_name` is a shorthand for `glua.ref_get` followed by `glua.call_function`
  let assert Ok(#(_lua, [result])) =
    glua.call_function_by_name(state: lua, keys: ["math", "max"], args:)

  let assert Ok(#(_lua, result)) = deser.run(lua, result, deser.number)

  assert result == 20.0
}

pub fn expose_functions_test() {
  let lua = glua.new()
  let #(lua, fun) =
    glua.function(lua, fn(lua, args) {
      // Since Gleam is a statically typed language, each and every argument must be decoded
      let assert [x, min, max] = args
      let assert Ok(#(lua, x)) = deser.run(lua, x, deser.number)
      let assert Ok(#(lua, min)) = deser.run(lua, min, deser.number)
      let assert Ok(#(lua, max)) = deser.run(lua, max, deser.number)

      let result = float.clamp(x, min, max)

      #(lua, [glua.float(result)])
    })

  let keys = ["my_functions", "clamp"]

  let assert Ok(lua) = glua.set(state: lua, keys:, value: fun)

  let args = [2.3, 1.2, 2.1] |> list.map(glua.float)
  let assert Ok(#(_lua, [result])) =
    glua.call_function_by_name(state: lua, keys:, args:)

  assert result == glua.float(2.1)
}
