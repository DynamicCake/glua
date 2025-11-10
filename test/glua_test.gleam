import gleam/dynamic/decode
import gleam/list
import gleeunit
import glua

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn load_test() {
  let assert Ok(#(chunk, lua)) = glua.load(glua.new(), "return 5 * 5")
  let assert Ok(#([result], _)) = glua.eval_chunk(lua, chunk)
  assert decode.run(result, decode.int) == Ok(25)
}

pub fn eval_load_file_test() {
  let assert Ok(#(chunk, lua)) =
    glua.load_file(glua.new(), "./test/lua/example.lua")
  let assert Ok(#([result], _)) = glua.eval_chunk(lua, chunk)

  assert decode.run(result, decode.string)
    == Ok("LUA IS AN EMBEDDABLE LANGUAGE")
}

pub fn eval_test() {
  let lua = glua.new()

  let assert Ok(#([result], _)) = glua.eval(lua, "return 'hello, ' .. 'world!'")
  assert decode.run(result, decode.string) == Ok("hello, world!")

  let assert Ok(#(results, _)) = glua.eval(lua, "return 2 + 2, 3 - 1")
  assert list.try_map(results, decode.run(_, decode.int)) == Ok([4, 2])
}

pub fn eval_file_test() {
  let lua = glua.new()
  let assert Ok(#([result], _)) = glua.eval_file(lua, "./test/lua/example.lua")

  assert decode.run(result, decode.string)
    == Ok("LUA IS AN EMBEDDABLE LANGUAGE")
}
