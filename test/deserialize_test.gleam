import deser
import gleam/dynamic
import glua

/// Warning: Can throw
@external(erlang, "luerl", "encode")
fn encode(a: anything, lua: glua.Lua) -> #(glua.ValueRef, glua.Lua)

@external(erlang, "glua_ffi", "coerce")
fn coerce_dynamic(a: anything) -> dynamic.Dynamic

pub fn decoder_test() {
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

pub fn field_ok_test() {
  let lua = glua.new()
  let data =
    glua.table([
      #(glua.string("red herring"), glua.string("not here!")),
      #(glua.string("name"), glua.string("Hina")),
    ])
  let #(ref, lua) = encode(data, lua)
  let assert Ok(#(_lua, val)) =
    deser.run(lua, ref, {
      use str <- deser.field(glua.str_ref("name"), deser.string)
      deser.success(str)
    })
  assert val == "Hina"
}

pub fn subfield_ok_test() {
  let lua = glua.new()
  let data =
    glua.table([
      #(glua.string("name"), glua.string("Hina")),
      #(
        glua.string("friends"),
        glua.table([
          #(glua.int(1), glua.string("Puffy")),
          #(glua.int(2), glua.string("Lucy")),
        ]),
      ),
    ])
  let #(ref, lua) = encode(data, lua)
  let assert Ok(#(_lua, val)) =
    deser.run(lua, ref, {
      use first <- deser.subfield(
        [glua.str_ref("friends"), glua.int_ref(1)],
        deser.string,
      )
      deser.success(first)
    })
  assert val == "Puffy"
}

pub fn field_metatable_test() {
  let lua = glua.new()
  let #(lua, data) = glua.alloc_table(lua, [])
  let metatable =
    glua.table([
      #(
        glua.string("__index"),
        glua.function(fn(lua, _args) { #(lua, [glua.string("pong")]) }),
      ),
    ])
  let assert Ok(#(lua, [table])) =
    glua.call_function_by_name(lua, ["setmetatable"], [data, metatable])
  let assert Ok(#(_lua, val)) =
    deser.run(lua, table, {
      use pong <- deser.field(
        glua.str_ref("aasdlkjghasddlkjghasddklgjh;ksjdh"),
        deser.string,
      )
      deser.success(pong)
    })
  assert val == "pong"
}
