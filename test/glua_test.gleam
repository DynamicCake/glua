import deser
import gleam/bit_array
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleeunit

import glua

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn get_table_test() {
  let lua = glua.new()
  let my_table = [
    #("meaning of life", 42.0),
    #("pi", 3.0),
    #("euler's number", 3.0),
  ]

  let cool_numbers =
    glua.function(fn(lua, _params) {
      let #(lua, table) =
        glua.table(
          lua,
          my_table
            |> list.map(fn(pair) { #(glua.string(pair.0), glua.float(pair.1)) }),
        )
      Ok(#(lua, [table]))
    })
    |> glua.func_to_val

  let assert Ok(lua) = glua.set(lua, ["cool_numbers"], cool_numbers)
  let assert Ok(#(lua, [table])) =
    glua.call_function_by_name(lua, ["cool_numbers"], [])

  let assert Ok(table) =
    deser.run(lua, table, deser.dict(deser.string, deser.number))

  assert table == dict.from_list(my_table)
}

pub fn sandbox_test() {
  let assert Ok(lua) = glua.sandbox(glua.new(), ["math", "max"])
  let args = list.map([20, 10], glua.int)

  let assert Error(glua.LuaRuntimeException(exception, _)) =
    glua.call_function_by_name(state: lua, keys: ["math", "max"], args:)

  assert exception == glua.ErrorCall("math.max is sandboxed", option.None)

  let assert Ok(lua) = glua.sandbox(glua.new(), ["string"])

  let assert Error(glua.LuaRuntimeException(glua.IllegalIndex(_, name), _)) =
    glua.eval(state: lua, code: "return string.upper('my_string')")

  assert index == "upper"

  let assert Ok(lua) = glua.sandbox(glua.new(), ["os", "execute"])

  let assert Error(glua.LuaRuntimeException(exception, _)) =
    glua.eval(
      state: lua,
      code: "os.execute(\"echo 'sandbox test is failing'\"); os.exit(1)",
    )

  assert exception == glua.ErrorCall("os.execute is sandboxed", option.None)

  let assert Ok(lua) = glua.sandbox(glua.new(), ["print"])
  let arg = glua.string("sandbox test is failing")
  let assert Error(glua.LuaRuntimeException(exception, _)) =
    glua.call_function_by_name(state: lua, keys: ["print"], args: [arg])

  assert exception == glua.ErrorCall("print is sandboxed", option.None)
}

pub fn new_sandboxed_test() {
  let assert Ok(lua) = glua.new_sandboxed([])

  let assert Error(glua.LuaRuntimeException(exception, _)) =
    glua.eval(state: lua, code: "return load(\"return 1\")")

  assert exception == glua.ErrorCall("load is sandboxed", option.None)

  let arg = glua.int(1)
  let assert Error(glua.LuaRuntimeException(exception, _)) =
    glua.call_function_by_name(state: lua, keys: ["os", "exit"], args: [arg])

  assert exception == glua.ErrorCall("os.exit is sandboxed", option.None)

  let assert Error(glua.LuaRuntimeException(glua.IllegalIndex(_, name), _)) =
    glua.eval(state: lua, code: "io.write('some_message')")

  assert index == "write"

  let assert Ok(lua) = glua.new_sandboxed([["package"], ["require"]])
  let assert Ok(lua) = glua.set_lua_paths(lua, paths: ["./test/lua/?.lua"])

  let code = "local s = require 'example'; return s"
  let assert Ok(#(_, [result])) = glua.eval(state: lua, code:)

  assert result == glua.string("LUA IS AN EMBEDDABLE LANGUAGE")
}

pub type Userdata {
  Userdata(foo: String, bar: Int)
}

pub fn userdata_test() {
  let lua = glua.new()
  let userdata = Userdata("my-userdata", 1)
  let userdata_decoder = {
    use foo <- decode.field(1, decode.string)
    use bar <- decode.field(2, decode.int)
    decode.success(Userdata(foo:, bar:))
  }

  let #(lua, data) = glua.userdata(lua, userdata)
  let assert Ok(lua) = glua.set(lua, ["my_userdata"], data)
  let assert Ok(#(lua, [result])) = glua.eval(lua, "return my_userdata")
  let assert Ok(result) = deser.run(lua, result, deser.userdata)
  let assert Ok(result) = decode.run(result, userdata_decoder)

  assert result == userdata

  let userdata = Userdata("other_userdata", 2)
  let #(lua, data) = glua.userdata(lua, userdata)
  let assert Ok(lua) = glua.set(lua, ["my_other_userdata"], data)
  let assert Error(glua.LuaRuntimeException(glua.IllegalIndex(value, index), _)) =
    glua.eval(lua, "return my_other_userdata.foo")

  assert index == "foo"
}

pub fn get_test() {
  let lua = glua.new()

  let assert Ok(pi) = glua.get(state: lua, keys: ["math", "pi"])
  let assert Ok(pi) = deser.run(lua, pi, deser.number)

  assert pi >. 3.14 && pi <. 3.15

  let keys = ["my_table", "my_value"]
  let encoded = glua.bool(True)
  let assert Ok(lua) = glua.set(state: lua, keys:, value: encoded)
  let assert Ok(ret) = glua.get(state: lua, keys:)

  assert ret == glua.bool(True)

  let code =
    "
  my_value = 10
  return 'ignored'
"
  let assert Ok(#(lua, _)) = glua.new() |> glua.eval(code:)
  let assert Ok(ret) = glua.get(state: lua, keys: ["my_value"])

  assert ret == glua.int(10)
}

pub fn get_returns_proper_errors_test() {
  let state = glua.new()

  assert glua.get(state:, keys: ["non_existent_global"])
    == Error(glua.KeyNotFound)

  let encoded = glua.int(10)
  let assert Ok(state) =
    glua.set(state:, keys: ["my_table", "some_value"], value: encoded)

  assert glua.get(state:, keys: ["my_table", "my_val"])
    == Error(glua.KeyNotFound)
}

pub fn set_test() {
  let encoded = glua.string("custom version")

  let assert Ok(lua) =
    glua.set(state: glua.new(), keys: ["_VERSION"], value: encoded)
  let assert Ok(result) = glua.get(state: lua, keys: ["_VERSION"])
  let assert Ok(result) = deser.run(lua, result, deser.string)

  assert result == "custom version"

  let numbers =
    [2, 4, 7, 12]
    |> list.index_map(fn(n, i) { #(i + 1, n * n) })

  let keys = ["math", "squares"]

  let #(lua, encoded) =
    glua.table(
      lua,
      numbers |> list.map(fn(pair) { #(glua.int(pair.0), glua.int(pair.1)) }),
    )
  let assert Ok(lua) = glua.set(lua, keys, encoded)

  let assert Ok(val) = glua.get(lua, keys)
  let assert Ok(val) = deser.run(lua, val, deser.list(deser.int))
  assert val == [4, 16, 49, 144]

  let count_odd = fn(lua: glua.Lua, args: List(glua.Value)) {
    let assert [list] = args
    let assert Ok(list) = deser.run(lua, list, deser.list(deser.int))

    let count = list.count(list, int.is_odd)
    Ok(#(lua, list.map([count], glua.int)))
  }

  let encoded = glua.function(count_odd) |> glua.func_to_val
  let assert Ok(lua) = glua.set(lua, ["count_odd"], encoded)

  let #(lua, arg) =
    glua.table(
      lua,
      list.index_map(list.range(1, 10), fn(i, n) { #(glua.int(i), glua.int(n)) }),
    )

  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(state: lua, keys: ["count_odd"], args: [arg])

  assert result == glua.int(5)

  let #(lua, tbl) =
    glua.table(lua, [
      #(
        glua.string("is_even"),
        glua.function(fn(lua, args) {
          let assert [arg] = args
          let assert Ok(arg) = deser.run(lua, arg, deser.int)
          Ok(#(lua, list.map([int.is_even(arg)], glua.bool)))
        })
          |> glua.func_to_val,
      ),
      #(
        glua.string("is_odd"),
        glua.function(fn(lua, args) {
          let assert [arg] = args
          let assert Ok(arg) = deser.run(lua, arg, deser.int)
          Ok(#(lua, list.map([int.is_odd(arg)], glua.bool)))
        })
          |> glua.func_to_val,
      ),
    ])

  let arg = glua.int(4)

  let assert Ok(lua) = glua.set(state: lua, keys: ["my_functions"], value: tbl)

  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(
      state: lua,
      keys: ["my_functions", "is_even"],
      args: [arg],
    )

  assert result == glua.bool(True)

  let assert Ok(#(_, [result])) =
    glua.eval(state: lua, code: "return my_functions.is_odd(4)")

  assert result == glua.bool(False)
}

pub fn set_lua_paths_test() {
  let assert Ok(state) =
    glua.set_lua_paths(state: glua.new(), paths: ["./test/lua/?.lua"])

  let code = "local s = require 'example'; return s"

  let assert Ok(#(_, [result])) = glua.eval(state:, code:)

  assert result == glua.string("LUA IS AN EMBEDDABLE LANGUAGE")
}

pub fn get_private_test() {
  let lua = glua.new() |> glua.set_private("test", [1, 2, 3])
  let assert Ok(priv) = glua.get_private(lua, "test")
  assert decode.run(priv, decode.list(decode.int)) == Ok([1, 2, 3])

  assert glua.new()
    |> glua.get_private("non_existent")
    == Error(glua.KeyNotFound)
}

pub fn delete_private_test() {
  let lua = glua.set_private(glua.new(), "the_value", "that_will_be_deleted")

  let assert Ok(priv) = glua.get_private(lua, "the_value")
  assert decode.run(priv, decode.string) == Ok("that_will_be_deleted")

  let lua = glua.delete_private(lua, "the_value")
  let assert Error(glua.KeyNotFound) =
    glua.get_private(state: lua, key: "the_value")
}

pub fn load_test() {
  let assert Ok(#(lua, chunk)) =
    glua.load(state: glua.new(), code: "return 5 * 5")
  let assert Ok(#(_, [result])) = glua.eval_chunk(state: lua, chunk:)

  assert result == glua.int(25)
}

pub fn eval_load_file_test() {
  let assert Ok(#(lua, chunk)) =
    glua.load_file(state: glua.new(), path: "./test/lua/example.lua")
  let assert Ok(#(_, [result])) = glua.eval_chunk(state: lua, chunk:)

  assert result == glua.string("LUA IS AN EMBEDDABLE LANGUAGE")
}

pub fn eval_test() {
  let assert Ok(#(lua, [result])) =
    glua.eval(state: glua.new(), code: "return 'hello, ' .. 'world!'")

  assert result == glua.string("hello, world!")

  let assert Ok(#(_, [a, b])) =
    glua.eval(state: lua, code: "return 2 + 2, 3 - 1")

  assert a == glua.int(4)
  assert b == glua.int(2)
}

pub fn eval_returns_proper_errors_test() {
  let state = glua.new()

  assert glua.eval(state:, code: "if true then 1 + ")
    == Error(
      glua.LuaCompilerException(messages: ["syntax error before: ", "1"]),
    )

  let assert Error(glua.LuaRuntimeException(
    exception: glua.IllegalIndex(value:, index:),
    state: _,
  )) = glua.eval(state:, code: "return a.b")

  assert value == "nil"
  assert index == "b"

  let assert Error(glua.LuaRuntimeException(
    exception: glua.ErrorCall(message, level),
    state: _,
  )) = glua.eval(state:, code: "error('error message')")

  assert message == "error message"
  assert level == option.None

  let assert Error(glua.LuaRuntimeException(
    exception: glua.ErrorCall(message, level),
    state: _,
  )) =
    glua.eval(state:, code: "error('error with level', 1)", using: decode.int)

  assert message == "error with level"
  assert level == option.Some(1)

  let assert Error(_) = glua.eval(state:, code: "error({1})", using: decode.int)

  let assert Error(glua.LuaRuntimeException(
    exception: glua.UndefinedFunction(value:),
    state: _,
  )) = glua.eval(state:, code: "local a = 5; a()")

  assert value == "5"

  let assert Error(glua.LuaRuntimeException(
    exception: glua.UndefinedMethod(_, method:),
    state: _,
  )) =
    glua.eval(
      state:,
      code: "local i = function(x) return x end; i:call(1)",
      using: decode.string,
    )

  assert method == "call"

  let assert Error(glua.LuaRuntimeException(
    exception: glua.BadArith(operator:, args:),
    state: _,
  )) = glua.eval(state:, code: "return 10 / 0")

  assert operator == "/"
  assert args == ["10", "0"]

  let assert Error(glua.LuaRuntimeException(
    exception: glua.AssertError(message:),
    state: _,
  )) = glua.eval(state:, code: "assert(1 == 2, 'assertion failed')")

  assert message == "assertion failed"

  let assert Error(_) =
    glua.eval(state:, code: "assert(false, {1})", using: decode.int)
}

pub fn eval_file_test() {
  let assert Ok(#(_, [result])) =
    glua.eval_file(state: glua.new(), path: "./test/lua/example.lua")

  assert result == glua.string("LUA IS AN EMBEDDABLE LANGUAGE")
}

pub fn call_function_test() {
  let assert Ok(#(lua, [fun])) =
    glua.eval(state: glua.new(), code: "return string.reverse")

  let encoded = glua.string("auL")
  let assert Ok(fun) = deser.run(lua, fun, deser.function)

  let assert Ok(#(lua, [result])) =
    glua.call_function(state: lua, fun: fun, args: [encoded])

  assert result == glua.string("Lua")

  let assert Ok(#(lua, [fun])) =
    glua.eval(state: lua, code: "return function(a, b) return a .. b end")
  let assert Ok(fun) = deser.run(lua, fun, deser.function)

  let args = list.map(["Lua in ", "Gleam"], glua.string)

  let assert Ok(#(_, [result])) =
    glua.call_function(state: lua, fun: fun, args:)

  assert result == glua.string("Lua in Gleam")
}

pub fn call_function_by_name_test() {
  let args = list.map([20, 10], glua.int)
  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(state: glua.new(), keys: ["math", "max"], args:)

  assert result == glua.int(20)

  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(state: lua, keys: ["math", "min"], args:)

  assert result == glua.int(10)

  let arg = glua.float(10.2)
  let assert Ok(#(lua, [result])) =
    glua.call_function_by_name(state: lua, keys: ["math", "type"], args: [arg])
  let assert Ok(result) = deser.run(lua, result, deser.optional(deser.string))

  assert result == option.Some("float")
}

pub fn nested_function_references_test() {
  let code = "return function() return math.sqrt end"

  let assert Ok(#(lua, [ref])) = glua.eval(state: glua.new(), code:)
  let assert Ok(fun) = deser.run(lua, ref, deser.function)
  let assert Ok(#(lua, [ref])) = glua.call_function(state: lua, fun:, args: [])
  let assert Ok(fun) = deser.run(lua, ref, deser.function)

  let arg = glua.int(400)
  let assert Ok(#(_, [result])) =
    glua.call_function(state: lua, fun:, args: [arg])
  assert result == glua.float(20.0)
}

pub fn alloc_test() {
  let #(lua, table) = glua.table(glua.new(), [])
  let proxy =
    glua.function(fn(lua, _args) { Ok(#(lua, [glua.string("constant")])) })
    |> glua.func_to_val
  let #(lua, metatable) = glua.table(lua, [#(glua.string("__index"), proxy)])
  let assert Ok(#(lua, _)) =
    glua.call_function_by_name(lua, ["setmetatable"], [table, metatable])
  let assert Ok(lua) = glua.set(lua, ["test_table"], table)

  let assert Ok(#(_lua, [ret1])) = glua.eval(lua, "return test_table.any_key")

  let assert Ok(#(_lua, [ret2])) = glua.eval(lua, "return test_table.other_key")

  assert ret1 == glua.string("constant")
  assert ret2 == glua.string("constant")
}

fn bad_arg_error(lua: glua.Lua, errors: List(deser.DeserializeError)) {
  #(lua, errors |> list.map(deser.error_to_string))
}

pub fn throw_error_test() {
  let add_func =
    glua.function(fn(lua, params) {
      let result =
        deser.run_multi(lua, params, {
          use augend <- deser.item(1, deser.number)
          use addend <- deser.item(2, deser.number)
          deser.success(#(augend, addend))
        })
        |> result.map_error(bad_arg_error(lua, _))
      use #(augend, addend) <- result.try(result)
      Ok(#(lua, [glua.float(augend +. addend)]))
    })
  let lua = glua.new()
  let assert Ok(#(lua, [result])) =
    glua.call_function(lua, add_func, [glua.int(2), glua.int(4)])
  let assert Ok(6.0) = deser.run(lua, result, deser.number)
  let assert Error(glua.LuaRuntimeException(
    exception: glua.ErrorCall([
      "Expected Field got Nothing at [\"<MultiVal #1>\"]",
      "Expected Field got Nothing at [\"<MultiVal #2>\"]",
    ]),
    state: _lua,
  )) = glua.call_function(lua, add_func, [])

  let assert Error(glua.LuaRuntimeException(
    exception: glua.ErrorCall([
      "Expected Number got String at [\"<MultiVal #1>\"]",
      "Expected Field got Nothing at [\"<MultiVal #2>\"]",
    ]),
    state: _lua,
  )) = glua.call_function(lua, add_func, [glua.string("1")])
}

pub fn non_utf8_test() {
  let lua = glua.new()
  let assert Ok(chars) = glua.get(lua, ["_G", "utf8", "charpattern"])
  let assert Error([deser.DeserializeError("String", "ByteString", [])]) =
    deser.run(lua, chars, deser.string)

  let assert Ok(str) = deser.run(lua, chars, deser.byte_string)
  assert !bit_array.is_utf8(str)
}

pub fn format_error_test() {
  let state = glua.new()

  let assert Error(e) = glua.ref_eval(state:, code: "1 +")
  assert glua.format_error(e)
    == "Lua compile error: \n\nFailed to parse: error on line 1: syntax error before: 1"

  let assert Error(e) = glua.ref_eval(state:, code: "assert(false)")
  assert glua.format_error(e)
    == "Lua runtime exception: Assertion failed with message: assertion failed\n\nLine 1: assert(false)"

  let assert Error(e) =
    glua.ref_eval(state:, code: "local a = true; local b = 1 * a")
  assert glua.format_error(e)
    == "Lua runtime exception: Bad arithmetic expression: 1 * true"

  let assert Error(e) =
    glua.get(state:, keys: ["non_existent"], using: decode.string)
  assert glua.format_error(e) == "Key \"non_existent\" not found"

  let assert Error(e) = glua.load_file(state:, path: "non_existent_file")
  assert glua.format_error(e)
    == "Lua source file \"non_existent_file\" not found"

  let assert Error(e) =
    glua.eval(state:, code: "return 1 + 1", using: decode.string)
  assert glua.format_error(e) == "Expected String, but found Int"
}
