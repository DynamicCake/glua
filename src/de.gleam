//// The deserialize API is similar to the gleam/dynamic/decode API but has some differences.
////
//// The main difference is that it can change the state of a lua program due to metatables.
//// If you do not wish to keep the changed state, discard the new state.

import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/option.{type Option}
import glua.{type Lua, type Value, type ValueRef}

pub opaque type Deserializer(t) {
  Deserializer(function: fn(ValueRef) -> #(t, Lua, List(DeserializeError)))
}

pub type DeserializeError {
  DeserializeError(expected: String, found: String, path: List(String))
}

pub fn field(
  field_path: glua.ValueRef,
  field_decoder: Deserializer(t),
  next: fn(t) -> Deserializer(final),
) -> Deserializer(final) {
  Deserializer(function: fn(data) { todo })
}

pub fn subfield(
  field_path: List(name),
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  todo
  // Decoder(function: fn(data) {
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
  decoder: Decoder(t),
) -> Result(#(Lua, t), List(DecodeError)) {
  todo
  // let #(maybe_invalid_data, errors) = decoder.function(data)
  // case errors {
  //   [] -> Ok(maybe_invalid_data)
  //   [_, ..] -> Error(errors)
  // }
}

pub fn at(path: List(segment), inner: Decoder(a)) -> Decoder(a) {
  Decoder(function: fn(data) {
    index(path, [], inner.function, data, fn(data, position) {
      let #(default, _) = inner.function(data)
      #(default, [DecodeError("Field", "Nothing", [])])
      |> push_path(list.reverse(position))
    })
  })
}

fn index(
  path: List(a),
  position: List(a),
  inner: fn(ValueRef) -> #(b, List(DecodeError)),
  data: ValueRef,
  handle_miss: fn(ValueRef, List(a)) -> #(b, List(DecodeError)),
) -> #(b, List(DecodeError)) {
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
  layer: #(t, List(DecodeError)),
  path: List(key),
) -> #(t, List(DecodeError)) {
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
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  todo
  // Decoder(function: fn(data) {
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
  inner: Decoder(a),
) -> Decoder(a) {
  todo
  // Decoder(function: fn(data) {
  //   index(path, [], inner.function, data, fn(_, _) { #(default, []) })
  // })
}

// fn run_dynamic_function(
//   data: Dynamic,
//   name: String,
//   f: fn(Dynamic) -> Result(t, t),
// ) -> #(t, List(DecodeError)) {
//   case f(data) {
//     Ok(data) -> #(data, [])
//     Error(zero) -> #(zero, [DecodeError(name, dynamic.classify(data), [])])
//   }
// }

pub const string: Decoder(String) = Decoder(decode_string)

fn decode_string(data: Dynamic) -> #(String, List(DecodeError)) {
  todo
  // run_dynamic_function(data, "String", dynamic_string)
}

pub const bool: Decoder(Bool) = Decoder(decode_bool)

fn decode_bool(data: Dynamic) -> #(Bool, List(DecodeError)) {
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

pub const int: Decoder(Int) = Decoder(decode_int)

fn decode_int(data: Dynamic) -> #(Int, List(DecodeError)) {
  todo
  // run_dynamic_function(data, "Int", dynamic_int)
}

@external(erlang, "gleam_stdlib", "int")
@external(javascript, "../../gleam_stdlib.mjs", "int")
fn dynamic_int(data: Dynamic) -> Result(Int, Int)

pub const float: Decoder(Float) = Decoder(decode_float)

fn decode_float(data: Dynamic) -> #(Float, List(DecodeError)) {
  run_dynamic_function(data, "Float", dynamic_float)
}

@external(erlang, "gleam_stdlib", "float")
@external(javascript, "../../gleam_stdlib.mjs", "float")
fn dynamic_float(data: Dynamic) -> Result(Float, Float)

pub const value_ref: Deserializer(ValueRef) = Deserializer(decode_dynamic)

fn decode_dynamic(data: ValueRef) -> #(ValueRef, Lua, List(DecodeError)) {
  #(data, [])
}

pub const bit_array: Decoder(BitArray) = Decoder(decode_bit_array)

fn decode_bit_array(data: Dynamic) -> #(BitArray, List(DecodeError)) {
  run_dynamic_function(data, "BitArray", dynamic_bit_array)
}

@external(erlang, "gleam_stdlib", "bit_array")
@external(javascript, "../../gleam_stdlib.mjs", "bit_array")
fn dynamic_bit_array(data: Dynamic) -> Result(BitArray, BitArray)

pub fn list(of inner: Decoder(a)) -> Decoder(List(a)) {
  Decoder(fn(data) {
    decode_list(data, inner.function, fn(p, k) { push_path(p, [k]) }, 0, [])
  })
}

@external(erlang, "gleam_stdlib", "list")
@external(javascript, "../../gleam_stdlib.mjs", "list")
fn decode_list(
  data: Dynamic,
  item: fn(Dynamic) -> #(t, List(DecodeError)),
  push_path: fn(#(t, List(DecodeError)), key) -> #(t, List(DecodeError)),
  index: Int,
  acc: List(t),
) -> #(List(t), List(DecodeError))

pub fn dict(
  key: Decoder(key),
  value: Decoder(value),
) -> Decoder(Dict(key, value)) {
  Decoder(fn(data) {
    case decode_dict(data) {
      Error(_) -> #(dict.new(), decode_error("Dict", data))
      Ok(dict) ->
        dict.fold(dict, #(dict.new(), []), fn(a, k, v) {
          // If there are any errors from previous key-value pairs then we
          // don't need to run the decoders, instead return the existing acc.
          case a.1 {
            [] -> fold_dict(a, k, v, key.function, value.function)
            [_, ..] -> a
          }
        })
    }
  })
}

@external(erlang, "gleam_stdlib", "dict")
@external(javascript, "../../gleam_stdlib.mjs", "dict")
fn decode_dict(data: Dynamic) -> Result(Dict(Dynamic, Dynamic), Nil)

pub fn optional(inner: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(function: fn(data) {
    case is_null(data) {
      True -> #(option.None, [])
      False -> {
        let #(data, errors) = inner.function(data)
        #(option.Some(data), errors)
      }
    }
  })
}

pub fn map(decoder: Decoder(a), transformer: fn(a) -> b) -> Decoder(b) {
  Decoder(function: fn(d) {
    let #(data, errors) = decoder.function(d)
    #(transformer(data), errors)
  })
}

pub fn map_errors(
  decoder: Decoder(a),
  transformer: fn(List(DecodeError)) -> List(DecodeError),
) -> Decoder(a) {
  Decoder(function: fn(d) {
    let #(data, errors) = decoder.function(d)
    #(data, transformer(errors))
  })
}

pub fn collapse_errors(decoder: Decoder(a), name: String) -> Decoder(a) {
  todo
  // Decoder(function: fn(dynamic_data) {
  //   let #(data, errors) as layer = decoder.function(dynamic_data)
  //   case errors {
  //     [] -> layer
  //     [_, ..] -> #(data, decode_error(name, dynamic_data))
  //   }
  // })
}

pub fn then(decoder: Decoder(a), next: fn(a) -> Decoder(b)) -> Decoder(b) {
  Decoder(function: fn(dynamic_data) {
    let #(data, errors) = decoder.function(dynamic_data)
    let decoder = next(data)
    let #(data, _) as layer = decoder.function(dynamic_data)
    case errors {
      [] -> layer
      [_, ..] -> #(data, errors)
    }
  })
}

pub fn one_of(
  first: Decoder(a),
  or alternatives: List(Decoder(a)),
) -> Decoder(a) {
  Decoder(function: fn(dynamic_data) {
    let #(_, errors) as layer = first.function(dynamic_data)
    case errors {
      [] -> layer
      [_, ..] -> run_decoders(dynamic_data, layer, alternatives)
    }
  })
}

fn run_decoders(
  data: Dynamic,
  failure: #(a, List(DecodeError)),
  decoders: List(Decoder(a)),
) -> #(a, List(DecodeError)) {
  case decoders {
    [] -> failure

    [decoder, ..decoders] -> {
      let #(_, errors) as layer = decoder.function(data)
      case errors {
        [] -> layer
        [_, ..] -> run_decoders(data, failure, decoders)
      }
    }
  }
}

pub fn failure(zero: a, expected: String) -> Decoder(a) {
  Decoder(function: fn(d) { #(zero, decode_error(expected, d)) })
}

pub fn recursive(inner: fn() -> Decoder(a)) -> Decoder(a) {
  Decoder(function: fn(data) {
    let decoder = inner()
    decoder.function(data)
  })
}
