//// Bindings to the functions under `_G.string`

import glua.{type Value}

/// Returns the `string.byte` function.
@external(erlang, "glua_stdlib_ffi", "string_byte")
pub fn byte() -> Value

/// Returns the `string.char` function.
@external(erlang, "glua_stdlib_ffi", "string_char")
pub fn char() -> Value

/// Returns the `string.dump` function.
@external(erlang, "glua_stdlib_ffi", "string_dump")
pub fn dump() -> Value

/// Returns the `string.find` function.
@external(erlang, "glua_stdlib_ffi", "string_find")
pub fn find() -> Value

/// Returns the `string.format` function.
@external(erlang, "glua_stdlib_ffi", "string_format")
pub fn format() -> Value

/// Returns the `string.gmatch` function.   
@external(erlang, "glua_stdlib_ffi", "string_gmatch")
pub fn gmatch() -> Value

/// Returns the `string.gsub` function.
@external(erlang, "glua_stdlib_ffi", "string_gsub")
pub fn gsub() -> Value

/// Returns the `string.len` function.
@external(erlang, "glua_stdlib_ffi", "string_len")
pub fn len() -> Value

/// Returns the `string.lower` function.
@external(erlang, "glua_stdlib_ffi", "string_lower")
pub fn lower() -> Value

/// Returns the `string.match` function.
@external(erlang, "glua_stdlib_ffi", "string_match")
pub fn match() -> Value

/// Returns the `string.rep` function.
@external(erlang, "glua_stdlib_ffi", "string_rep")
pub fn rep() -> Value

/// Returns the `string.reverse` function.
@external(erlang, "glua_stdlib_ffi", "string_reverse")
pub fn reverse() -> Value

/// Returns the `string.sub` function.
@external(erlang, "glua_stdlib_ffi", "string_sub")
pub fn sub() -> Value

/// Returns the `string.upper` function.
@external(erlang, "glua_stdlib_ffi", "string_upper")
pub fn upper() -> Value
