//// Bindings to the functions under `_G.bit32`.
////
//// As of Lua 5.3, the entire bit32 library is deprecated.

import glua.{type Value}

/// Returns the `bit32.band` function.
@external(erlang, "glua_stdlib_ffi", "bit32_band")
pub fn band() -> Value

/// Returns the `bit32.bnot` function.
@external(erlang, "glua_stdlib_ffi", "bit32_bnot")
pub fn bnot() -> Value

/// Returns the `bit32.bor` function.
@external(erlang, "glua_stdlib_ffi", "bit32_bor")
pub fn bor() -> Value

/// Returns the `bit32.btest` function.
@external(erlang, "glua_stdlib_ffi", "bit32_btest")
pub fn btest() -> Value

/// Returns the `bit32.bxor` function.
@external(erlang, "glua_stdlib_ffi", "bit32_bxor")
pub fn bxor() -> Value

/// Returns the `bit32.lshift` function.
@external(erlang, "glua_stdlib_ffi", "bit32_lshift")
pub fn lshift() -> Value

/// Returns the `bit32.rshift` function.
@external(erlang, "glua_stdlib_ffi", "bit32_rshift")
pub fn rshift() -> Value

/// Returns the `bit32.arshift` function.
@external(erlang, "glua_stdlib_ffi", "bit32_arshift")
pub fn arshift() -> Value

/// Returns the `bit32.lrotate` function.
@external(erlang, "glua_stdlib_ffi", "bit32_lrotate")
pub fn lrotate() -> Value

/// Returns the `bit32.rrotate` function.
@external(erlang, "glua_stdlib_ffi", "bit32_rrotate")
pub fn rrotate() -> Value

/// Returns the `bit32.extract` function.
@external(erlang, "glua_stdlib_ffi", "bit32_extract")
pub fn extract() -> Value

/// Returns the `bit32.replace` function.
@external(erlang, "glua_stdlib_ffi", "bit32_replace")
pub fn replace() -> Value
