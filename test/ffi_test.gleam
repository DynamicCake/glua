import gleam/dynamic

@external(erlang, "glua_ffi", "proplist_to_map")
fn proplist_to_map(a: anything) -> dynamic.Dynamic

@external(erlang, "glua_ffi", "coerce")
fn cast(a: anything) -> dynamic.Dynamic

pub fn proplist_to_map_test() {
  let inner = [
    #(dynamic.int(1), dynamic.int(4)),
    #(dynamic.int(2), dynamic.int(2)),
  ]
  let props = fn(inner) {
    [
      #(dynamic.string("num"), dynamic.int(42)),
      #(dynamic.string("parts"), inner),
    ]
  }

  let transformed =
    cast(props(cast(inner)))
    |> proplist_to_map()

  assert transformed == dynamic.properties(props(dynamic.properties(inner)))
}
