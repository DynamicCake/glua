//// Bindings to the functions under `_G.utf8`

import glua.{type Value}

/// Returns the `utf8.char` function.
@external(erlang, "glua_stdlib_ffi", "utf8_char")
pub fn char() -> Value

/// Returns the `utf8.codes` function.
@external(erlang, "glua_stdlib_ffi", "utf8_codes")
pub fn codes() -> Value

/// Returns the `utf8.codepoint` function.
@external(erlang, "glua_stdlib_ffi", "utf8_codepoint")
pub fn codepoint() -> Value

/// Returns the `utf8.len` function.
@external(erlang, "glua_stdlib_ffi", "utf8_len")
pub fn len() -> Value

/// Returns the `utf8.offset` function.
@external(erlang, "glua_stdlib_ffi", "utf8_offset")
pub fn offset() -> Value
