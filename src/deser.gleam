//// The deserialize API is similar to the gleam/dynamic/decode API but has some differences.
////
//// The main difference is that it can change the state of a lua program due to metatables.
//// If you do not wish to keep the changed state, discard the new state.

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option}
import glua.{type Lua, type Value, type ValueRef}

pub opaque type Deserializer(t) {
  Deserializer(function: fn(ValueRef) -> #(t, Lua, List(DeserializeError)))
}

pub type DeserializeError {
  DeserializeError(expected: String, found: String, path: List(ValueRef))
}

pub fn field(
  field_path: ValueRef,
  field_decoder: Deserializer(t),
  next: fn(t) -> Deserializer(final),
) -> Deserializer(final) {
  subfield([field_path], field_decoder, next)
}

pub fn subfield(
  field_path: List(ValueRef),
  field_decoder: Deserializer(t),
  next: fn(t) -> Deserializer(final),
) -> Deserializer(final) {
  todo
  // Deserializer(function: fn(data) {
  //   let #(out, errors1) =
  //     index(field_path, [], field_decoder.function, data, fn(data, position) {
  //       let #(default, _) = field_decoder.function(data)
  //       #(default, [DecodeError("Field", "Nothing", [])])
  //       |> push_path(list.reverse(position))
  //     })
  //   let #(out, errors2) = next(out).function(data)
  //   #(out, list.append(errors1, errors2))
  // })
}

pub fn run(
  data: ValueRef,
  deser: Deserializer(t),
) -> Result(#(Lua, t), List(DeserializeError)) {
  let #(maybe_invalid_data, lua, errors) = deser.function(data)
  case errors {
    [] -> Ok(#(lua, maybe_invalid_data))
    [_, ..] -> Error(errors)
  }
}

pub fn at(path: List(segment), inner: Deserializer(a)) -> Deserializer(a) {
  todo
  // Deserializer(function: fn(data) {
  //   index(path, [], inner.function, data, fn(data, position) {
  //     let #(default, _) = inner.function(data)
  //     #(default, [DecodeError("Field", "Nothing", [])])
  //     |> push_path(list.reverse(position))
  //   })
  // })
}

fn index(
  path: List(a),
  position: List(a),
  inner: fn(ValueRef) -> #(b, List(DeserializeError)),
  data: ValueRef,
  handle_miss: fn(ValueRef, List(a)) -> #(b, List(DeserializeError)),
) -> #(b, List(DeserializeError)) {
  todo
  // case path {
  //   [] -> {
  //     data
  //     |> inner
  //     |> push_path(list.reverse(position))
  //   }
  //
  //   [key, ..path] -> {
  //     case bare_index(data, key) {
  //       Ok(Some(data)) -> {
  //         index(path, [key, ..position], inner, data, handle_miss)
  //       }
  //       Ok(None) -> {
  //         handle_miss(data, [key, ..position])
  //       }
  //       Error(kind) -> {
  //         let #(default, _) = inner(data)
  //         #(default, [DecodeError(kind, dynamic.classify(data), [])])
  //         |> push_path(list.reverse(position))
  //       }
  //     }
  //   }
  // }
}

@external(erlang, "gleam_stdlib", "index")
@external(javascript, "../../gleam_stdlib.mjs", "index")
fn bare_index(data: ValueRef, key: anything) -> Result(Option(ValueRef), String)

fn push_path(
  layer: #(t, List(DeserializeError)),
  path: List(key),
) -> #(t, List(DeserializeError)) {
  todo
  // let decoder = one_of(string, [int |> map(int.to_string)])
  // let path =
  //   list.map(path, fn(key) {
  //     let key = cast(key)
  //     case run(key, decoder) {
  //       Ok(key) -> key
  //       Error(_) -> "<" <> dynamic.classify(key) <> ">"
  //     }
  //   })
  // let errors =
  //   list.map(layer.1, fn(error) {
  //     DecodeError(..error, path: list.append(path, error.path))
  //   })
  // #(layer.0, errors)
}

pub fn success(state: Lua, data: t) -> Deserializer(t) {
  Deserializer(function: fn(_) { #(data, state, []) })
}

pub fn de_error(
  expected expected: String,
  found found: ValueRef,
) -> List(DeserializeError) {
  [DeserializeError(expected: expected, found: classify(found), path: [])]
}

@external(erlang, "glua_ffi", "classify")
pub fn classify(a: anything) -> String

pub fn optional_field(
  key: name,
  default: t,
  field_decoder: Deserializer(t),
  next: fn(t) -> Deserializer(final),
) -> Deserializer(final) {
  todo
  // Deserializer(function: fn(data) {
  //   let #(out, errors1) =
  //     case bare_index(data, key) {
  //       Ok(Some(data)) -> field_decoder.function(data)
  //       Ok(None) -> #(default, [])
  //       Error(kind) -> #(default, [
  //         DecodeError(kind, dynamic.classify(data), []),
  //       ])
  //     }
  //     |> push_path([key])
  //   let #(out, errors2) = next(out).function(data)
  //   #(out, list.append(errors1, errors2))
  // })
}

pub fn optionally_at(
  path: List(segment),
  default: a,
  inner: Deserializer(a),
) -> Deserializer(a) {
  todo
  // Deserializer(function: fn(data) {
  //   index(path, [], inner.function, data, fn(_, _) { #(default, []) })
  // })
}

// fn run_dynamic_function(
//   data: ValueRef,
//   name: String,
//   f: fn(ValueRef) -> Result(t, t),
// ) -> #(t, List(DecodeError)) {
//   case f(data) {
//     Ok(data) -> #(data, [])
//     Error(zero) -> #(zero, [DecodeError(name, dynamic.classify(data), [])])
//   }
// }

pub const string: Deserializer(String) = Deserializer(deser_string)

fn deser_string(data: ValueRef) -> #(String, Lua, List(DeserializeError)) {
  todo
  // run_dynamic_function(data, "String", dynamic_string)
}

pub const bool: Deserializer(Bool) = Deserializer(deser_bool)

fn deser_bool(data: ValueRef) -> #(Bool, Lua, List(DeserializeError)) {
  todo
  // case cast(True) == data {
  //   True -> #(True, [])
  //   False ->
  //     case cast(False) == data {
  //       True -> #(False, [])
  //       False -> #(False, decode_error("Bool", data))
  //     }
  // }
}

pub const int: Deserializer(Int) = Deserializer(deser_int)

fn deser_int(data: ValueRef) -> #(Int, Lua, List(DeserializeError)) {
  todo
  // run_dynamic_function(data, "Int", dynamic_int)
}

pub const float: Deserializer(Float) = Deserializer(deser_float)

fn deser_float(data: ValueRef) -> #(Float, Lua, List(DeserializeError)) {
  todo
  // run_dynamic_function(data, "Float", dynamic_float)
}

pub const value_ref: Deserializer(ValueRef) = Deserializer(decode_dynamic)

fn decode_dynamic(data: ValueRef) -> #(ValueRef, Lua, List(DeserializeError)) {
  #(data, todo, [])
}

pub const user_defined: Deserializer(dynamic.Dynamic) = Deserializer(
  deser_user_defined,
)

fn deser_user_defined(
  data: ValueRef,
) -> #(dynamic.Dynamic, Lua, List(DeserializeError)) {
  todo
  // run_dynamic_function(data, "BitArray", dynamic_bit_array)
}

@external(erlang, "gleam_stdlib", "bit_array")
@external(javascript, "../../gleam_stdlib.mjs", "bit_array")
fn dynamic_bit_array(data: ValueRef) -> Result(BitArray, BitArray)

pub fn list(of inner: Deserializer(a)) -> Deserializer(List(a)) {
  todo
  // Deserializer(fn(data) {
  //   decode_list(data, inner.function, fn(p, k) { push_path(p, [k]) }, 0, [])
  // })
}

@external(erlang, "gleam_stdlib", "list")
@external(javascript, "../../gleam_stdlib.mjs", "list")
fn decode_list(
  data: ValueRef,
  item: fn(ValueRef) -> #(t, List(DeserializeError)),
  push_path: fn(#(t, List(DeserializeError)), key) ->
    #(t, List(DeserializeError)),
  index: Int,
  acc: List(t),
) -> #(List(t), List(DeserializeError))

pub fn dict(
  key: Deserializer(key),
  value: Deserializer(value),
) -> Deserializer(Dict(key, value)) {
  todo
  // Deserializer(fn(data) {
  //   case decode_dict(data) {
  //     Error(_) -> #(dict.new(), decode_error("Dict", data))
  //     Ok(dict) ->
  //       dict.fold(dict, #(dict.new(), []), fn(a, k, v) {
  //         // If there are any errors from previous key-value pairs then we
  //         // don't need to run the decoders, instead return the existing acc.
  //         case a.1 {
  //           [] -> fold_dict(a, k, v, key.function, value.function)
  //           [_, ..] -> a
  //         }
  //       })
  //   }
  // })
}

pub fn optional(inner: Deserializer(a)) -> Deserializer(Option(a)) {
  todo
  // Deserializer(function: fn(data) {
  //   case is_null(data) {
  //     True -> #(option.None, [])
  //     False -> {
  //       let #(data, errors) = inner.function(data)
  //       #(option.Some(data), errors)
  //     }
  //   }
  // })
}

pub fn map(decoder: Deserializer(a), transformer: fn(a) -> b) -> Deserializer(b) {
  todo
  // Deserializer(function: fn(d) {
  //   let #(data, errors) = decoder.function(d)
  //   #(transformer(data), errors)
  // })
}

pub fn map_errors(
  decoder: Deserializer(a),
  transformer: fn(List(DeserializeError)) -> List(DeserializeError),
) -> Deserializer(a) {
  todo
  // Deserializer(function: fn(d) {
  //   let #(data, errors) = decoder.function(d)
  //   #(data, transformer(errors))
  // })
}

pub fn collapse_errors(
  decoder: Deserializer(a),
  name: String,
) -> Deserializer(a) {
  todo
  // Deserializer(function: fn(dynamic_data) {
  //   let #(data, errors) as layer = decoder.function(dynamic_data)
  //   case errors {
  //     [] -> layer
  //     [_, ..] -> #(data, decode_error(name, dynamic_data))
  //   }
  // })
}

pub fn then(
  decoder: Deserializer(a),
  next: fn(a) -> Deserializer(b),
) -> Deserializer(b) {
  todo
  // Deserializer(function: fn(dynamic_data) {
  //   let #(data, errors) = decoder.function(dynamic_data)
  //   let decoder = next(data)
  //   let #(data, _) as layer = decoder.function(dynamic_data)
  //   case errors {
  //     [] -> layer
  //     [_, ..] -> #(data, errors)
  //   }
  // })
}

pub fn one_of(
  first: Deserializer(a),
  or alternatives: List(Deserializer(a)),
) -> Deserializer(a) {
  todo
  // Deserializer(function: fn(dynamic_data) {
  //   let #(_, errors) as layer = first.function(dynamic_data)
  //   case errors {
  //     [] -> layer
  //     [_, ..] -> run_decoders(dynamic_data, layer, alternatives)
  //   }
  // })
}

fn run_decoders(
  data: ValueRef,
  failure: #(a, List(DeserializeError)),
  decoders: List(Deserializer(a)),
) -> #(a, List(DeserializeError)) {
  todo
  // case decoders {
  //   [] -> failure
  //
  //   [decoder, ..decoders] -> {
  //     let #(_, errors) as layer = decoder.function(data)
  //     case errors {
  //       [] -> layer
  //       [_, ..] -> run_decoders(data, failure, decoders)
  //     }
  //   }
  // }
}

pub fn failure(zero: a, expected: String) -> Deserializer(a) {
  todo
  // Deserializer(function: fn(d) { #(zero, glua.new(), deser_error(expected, d)) })
}

pub fn recursive(inner: fn() -> Deserializer(a)) -> Deserializer(a) {
  Deserializer(function: fn(data) {
    let decoder = inner()
    decoder.function(data)
  })
}
