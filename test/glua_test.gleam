import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleeunit
import glua

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn get_test() {
  let state = glua.new()

  let assert Ok(pi) =
    glua.get(state: state, keys: ["math", "pi"], using: decode.float)

  assert pi >. 3.14 && pi <. 3.15

  let keys = ["my_table", "my_value"]
  let #(state, encoded) = glua.bool(state, True)
  let assert Ok(state) = glua.set(state:, keys:, value: encoded)
  let assert Ok(ret) = glua.get(state:, keys:, using: decode.bool)

  assert ret == True

  let code =
    "
  my_value = 10
  return 'ignored'
"
  let assert Ok(#(state, _)) =
    glua.new() |> glua.eval(code:, using: decode.string)
  let assert Ok(ret) = glua.get(state:, keys: ["my_value"], using: decode.int)

  assert ret == 10
}

pub fn set_test() {
  let numbers =
    [2, 4, 7, 12]
    |> list.index_map(fn(n, i) { #(i + 1, n * n) })

  let keys = ["math", "squares"]

  let #(lua, encoded) = glua.table(glua.new(), #(glua.int, glua.int), numbers)
  let assert Ok(lua) = glua.set(lua, keys, encoded)

  assert glua.get(lua, keys, glua.table_decoder(decode.int, decode.int))
    == Ok([#(1, 4), #(2, 16), #(3, 49), #(4, 144)])

  let count_odd = fn(lua: glua.Lua, args: List(dynamic.Dynamic)) {
    let assert [list] = args
    let assert Ok(list) =
      decode.run(list, glua.table_decoder(decode.int, decode.int))

    let count =
      list.map(list, pair.second)
      |> list.count(int.is_odd)

    glua.list(lua, glua.int, [count])
  }

  let #(lua, encoded) = glua.function(glua.new(), count_odd)
  let assert Ok(lua) = glua.set(lua, ["count_odd"], encoded)

  let #(lua, arg) =
    glua.table(
      lua,
      #(glua.int, glua.int),
      list.index_map(list.range(1, 10), fn(i, n) { #(i + 1, n) }),
    )

  let assert Ok(#(_, [result])) =
    glua.call_function_by_name(
      state: lua,
      keys: ["count_odd"],
      args: [arg],
      using: decode.int,
    )

  assert result == 5
}

pub fn load_test() {
  let assert Ok(#(lua, chunk)) =
    glua.load(state: glua.new(), code: "return 5 * 5")
  let assert Ok(#(_, [result])) =
    glua.eval_chunk(state: lua, chunk:, using: decode.int)

  assert result == 25
}

pub fn eval_load_file_test() {
  let assert Ok(#(lua, chunk)) =
    glua.load_file(state: glua.new(), path: "./test/lua/example.lua")
  let assert Ok(#(_, [result])) =
    glua.eval_chunk(state: lua, chunk:, using: decode.string)

  result == "LUA IS AN EMBEDDABLE LANGUAGE"
}

pub fn eval_test() {
  let assert Ok(#(lua, [result])) =
    glua.eval(
      state: glua.new(),
      code: "return 'hello, ' .. 'world!'",
      using: decode.string,
    )

  assert result == "hello, world!"

  let assert Ok(#(_, results)) =
    glua.eval(state: lua, code: "return 2 + 2, 3 - 1", using: decode.int)

  assert results == [4, 2]
}

pub fn eval_file_test() {
  let assert Ok(#(_, [result])) =
    glua.eval_file(
      state: glua.new(),
      path: "./test/lua/example.lua",
      using: decode.string,
    )

  assert result == "LUA IS AN EMBEDDABLE LANGUAGE"
}

pub fn call_function_test() {
  let assert Ok(#(lua, [fun])) =
    glua.ref_eval(state: glua.new(), code: "return string.reverse")

  let #(lua, encoded) = glua.string(lua, "auL")

  let assert Ok(#(lua, [result])) =
    glua.call_function(
      state: lua,
      ref: fun,
      args: [encoded],
      using: decode.string,
    )

  assert result == "Lua"

  let assert Ok(#(lua, [fun])) =
    glua.ref_eval(state: lua, code: "return function(a, b) return a .. b end")

  let #(lua, args) = glua.list(lua, glua.string, ["Lua in ", "Gleam"])

  let assert Ok(#(_, [result])) =
    glua.call_function(state: lua, ref: fun, args:, using: decode.string)

  assert result == "Lua in Gleam"
}

pub fn call_function_by_name_test() {
  let #(lua, args) = glua.new() |> glua.list(glua.int, [20, 10])
  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(
      state: lua,
      keys: ["math", "max"],
      args:,
      using: decode.int,
    )

  assert result == 20

  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(
      state: lua,
      keys: ["math", "min"],
      args:,
      using: decode.int,
    )

  assert result == 10

  let #(lua, arg) = glua.float(lua, 10.2)
  let assert Ok(#(_, [result])) =
    glua.call_function_by_name(
      state: lua,
      keys: ["math", "type"],
      args: [arg],
      using: decode.optional(decode.string),
    )

  assert result == option.Some("float")
}

pub fn nested_function_references_test() {
  let code = "return function() return math.sqrt end"

  let assert Ok(#(lua, [ref])) = glua.ref_eval(state: glua.new(), code:)
  let assert Ok(#(lua, [ref])) =
    glua.ref_call_function(state: lua, ref:, args: [])

  let #(lua, arg) = glua.int(lua, 400)
  let assert Ok(#(_, [result])) =
    glua.call_function(state: lua, ref:, args: [arg], using: decode.float)
  assert result == 20.0
}
