import deser
import gleam/dynamic
import glua

@external(erlang, "glua_ffi", "coerce")
fn coerce_dynamic(a: anything) -> dynamic.Dynamic

pub fn deserializer_test() {
  let lua = glua.new()
  let assert Ok(#(_lua, "Hello")) =
    deser.run(lua, glua.string("Hello"), deser.string)

  let assert Ok(#(_lua, 42.0)) = deser.run(lua, glua.int(42), deser.number)

  let assert Ok(#(_lua, False)) = deser.run(lua, glua.bool(False), deser.bool)

  let data = ["this", "is", "some", "random", "data"]
  let #(lua, ref) = data |> glua.userdata(lua, _)
  let assert Ok(#(_lua, userdefined)) = deser.run(lua, ref, deser.user_defined)
  assert userdefined == coerce_dynamic(data)
}

pub fn field_ok_test() {
  let lua = glua.new()
  let #(lua, data) =
    glua.table(lua, [
      #(glua.string("red herring"), glua.string("not here!")),
      #(glua.string("name"), glua.string("Hina")),
    ])
  let assert Ok(#(_lua, val)) =
    deser.run(lua, data, {
      use str <- deser.field(glua.string("name"), deser.string)
      deser.success(str)
    })
  assert val == "Hina"
}

pub fn subfield_ok_test() {
  let lua = glua.new()
  let #(lua, inner) =
    glua.table(lua, [
      #(glua.int(1), glua.string("Puffy")),
      #(glua.int(2), glua.string("Lucy")),
    ])
  let #(lua, data) =
    glua.table(lua, [
      #(glua.string("name"), glua.string("Hina")),
      #(glua.string("friends"), inner),
    ])
  let assert Ok(#(_lua, val)) =
    deser.run(lua, data, {
      use first <- deser.subfield(
        [glua.string("friends"), glua.int(1)],
        deser.string,
      )
      deser.success(first)
    })
  assert val == "Puffy"
}

pub fn field_metatable_test() {
  let lua = glua.new()
  let #(lua, data) = glua.table(lua, [])
  let #(lua, func) =
    glua.function(lua, fn(lua, _args) { #(lua, [glua.string("pong")]) })
  let #(lua, metatable) =
    glua.table(lua, [
      #(glua.string("__index"), func),
    ])
  let assert Ok(#(lua, [table])) =
    glua.call_function_by_name(lua, ["setmetatable"], [data, metatable])
  let assert Ok(#(_lua, val)) =
    deser.run(lua, table, {
      use pong <- deser.field(
        glua.string("aasdlkjghasddlkjghasddklgjh;ksjdh"),
        deser.string,
      )
      deser.success(pong)
    })
  assert val == "pong"
}
