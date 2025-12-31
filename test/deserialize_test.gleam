import deser
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import glua

/// Warning: Can throw
@external(erlang, "luerl", "encode")
fn encode(a: anything, lua: glua.Lua) -> #(glua.ValueRef, glua.Lua)

@external(erlang, "glua_ffi", "coerce")
fn coerce_dynamic(a: anything) -> dynamic.Dynamic

pub fn basic_test() {
  let lua = glua.new()
  let #(ref, lua) = encode("Hello", lua)
  let assert Ok(#(_lua, "Hello")) = deser.run(lua, ref, deser.string)

  let #(ref, lua) = encode(42.0, lua)
  let assert Ok(#(_lua, 42.0)) = deser.run(lua, ref, deser.number)

  let #(ref, lua) = encode(False, lua)
  let assert Ok(#(_lua, False)) = deser.run(lua, ref, deser.bool)

  let userdef = ["this", "is", "some", "random", "data"] |> glua.userdata
  let #(ref, lua) = encode(userdef, lua)
  let assert Ok(#(_lua, udef)) = deser.run(lua, ref, deser.user_defined)
  assert udef == coerce_dynamic(userdef)
}
