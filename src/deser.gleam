//// The deserialize API is similar to the gleam/dynamic/decode API but has some differences.
////
//// The main difference is that it can change the state of a lua program due to metatables.
//// If you do not wish to keep the changed state, discard the new state.

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import glua.{type Lua, type ValueRef}

pub opaque type Deserializer(t) {
  Deserializer(function: fn(Lua, ValueRef) -> Return(t))
}

pub type DeserializeError {
  DeserializeError(expected: String, found: String, path: List(ValueRef))
}

type Return(t) =
  #(t, Lua, List(DeserializeError))

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
  Deserializer(function: fn(lua, data) {
    let #(out, lua, errors1) =
      index(
        lua,
        field_path,
        [],
        field_decoder.function,
        data,
        fn(lua, data, position) {
          let #(default, lua, _) = field_decoder.function(lua, data)
          #(default, lua, [DeserializeError("Field", "Nothing", [])])
          |> push_path(list.reverse(position))
        },
      )
    let #(out, lua, errors2) = next(out).function(lua, data)
    #(out, lua, list.append(errors1, errors2))
  })
}

fn index(
  lua: Lua,
  path: List(ValueRef),
  position: List(ValueRef),
  inner: fn(Lua, ValueRef) -> Return(b),
  data: ValueRef,
  handle_miss: fn(Lua, ValueRef, List(ValueRef)) -> Return(b),
) -> Return(b) {
  case path {
    [] -> {
      data
      |> inner(lua, _)
      |> push_path(list.reverse(position))
    }

    [key, ..path] -> {
      case get_table_key(lua, data, key) {
        Ok(#(lua, data)) -> {
          index(lua, path, [key, ..position], inner, data, handle_miss)
        }
        // NOTE: I don't feel comfortable matching on this
        Error(glua.KeyNotFound)
        | Error(glua.LuaRuntimeException(
            exception: glua.IllegalIndex(_, _),
            state: _,
          )) -> {
          handle_miss(lua, data, [key, ..position])
        }
        Error(err) -> {
          let #(default, lua, _) = inner(lua, data)
          #(default, lua, [DeserializeError("Table", classify(data), [])])
          |> push_path(list.reverse(position))
        }
      }
    }
  }
}

pub fn run(
  lua: Lua,
  data: ValueRef,
  deser: Deserializer(t),
) -> Result(#(Lua, t), List(DeserializeError)) {
  let #(maybe_invalid_data, lua, errors) = deser.function(lua, data)
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

@external(erlang, "glua_ffi", "get_table_key")
fn get_table_key(
  lua: Lua,
  table: ValueRef,
  key: ValueRef,
) -> Result(#(Lua, ValueRef), glua.LuaError)

fn to_string(lua: Lua, val: ValueRef) {
  case classify(val) {
    "Null" -> "<nil>"
    "Number" -> decode(val, lua)
    "String" -> decode(val, lua)
    "Unknown" -> "<unknown>"
    "UserDef" -> "userdefined: " <> string.inspect(val)
    _ -> {
      {
        case glua.call_function_by_name(lua, ["tostring"], [val]) {
          Ok(#(lua, [value])) -> {
            run(lua, value, string)
            |> result.map(pair.second)
            |> result.unwrap("<tostring failure>")
          }
          Error(err) -> "<tostring failure (" <> string.inspect(err) <> ")>"
          _ -> "<tostring failure>"
        }
      }
    }
  }
}

fn push_path(layer: Return(t), path: List(ValueRef)) -> Return(t) {
  let errors =
    list.map(layer.2, fn(error) {
      DeserializeError(..error, path: list.append(path, error.path))
    })
  #(layer.0, layer.1, errors)
}

pub fn success(data: t) -> Deserializer(t) {
  Deserializer(function: fn(lua, _) { #(data, lua, []) })
}

pub fn deser_error(
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

fn run_dynamic_function(
  lua: Lua,
  data: ValueRef,
  expected: String,
  zero: t,
) -> Return(t) {
  let got = classify(data)
  case got == expected {
    True -> #(decode(data, lua), lua, [])
    False -> #(zero, lua, [
      DeserializeError(expected, got, []),
    ])
  }
}

/// Warning: Can panic
@external(erlang, "luerl", "decode")
fn decode(a: ValueRef, lua: Lua) -> a

pub const string: Deserializer(String) = Deserializer(deser_string)

fn deser_string(lua, data: ValueRef) -> Return(String) {
  run_dynamic_function(lua, data, "String", "")
}

pub const bool: Deserializer(Bool) = Deserializer(deser_bool)

fn deser_bool(lua, data: ValueRef) -> Return(Bool) {
  run_dynamic_function(lua, data, "Bool", True)
}

pub const number: Deserializer(Float) = Deserializer(deser_num)

fn deser_num(lua, data: ValueRef) -> Return(Float) {
  run_dynamic_function(lua, data, "Number", 0.0)
}

pub const raw: Deserializer(ValueRef) = Deserializer(decode_raw)

fn decode_raw(lua, data: ValueRef) -> Return(ValueRef) {
  #(data, lua, [])
}

pub const user_defined: Deserializer(dynamic.Dynamic) = Deserializer(
  deser_user_defined,
)

@external(erlang, "glua_ffi", "unwrap_userdata")
fn unwrap_userdata(a: userdata) -> Result(dynamic.Dynamic, Nil)

fn deser_user_defined(lua, data: ValueRef) -> Return(dynamic.Dynamic) {
  let ret = run_dynamic_function(lua, data, "UserDef", dynamic.nil())
  let #(dyn, lua, errs) = ret
  case errs {
    [] ->
      case unwrap_userdata(dyn) {
        Ok(dyn) -> #(dyn, lua, [])
        Error(Nil) -> #(dynamic.nil(), lua, [
          DeserializeError("UserDef", classify(data), []),
        ])
      }
    _ -> ret
  }
}

pub fn list(of inner: Deserializer(a)) -> Deserializer(List(a)) {
  todo
  // Deserializer(fn(data) {
  //   decode_list(data, inner.function, fn(p, k) { push_path(p, [k]) }, 0, [])
  // })
}

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
  Deserializer(function: fn(lua, data) {
    case classify(data) {
      "Nil" -> #(option.None, lua, [])
      _ -> {
        let #(data, lua, errors) = inner.function(lua, data)
        #(option.Some(data), lua, errors)
      }
    }
  })
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
  Deserializer(function: fn(lua, data) {
    let decoder = inner()
    decoder.function(lua, data)
  })
}
