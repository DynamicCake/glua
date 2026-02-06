//// Bindings to the functions under `_G.debug`

import glua.{type Value}

/// Returns the `getmetatable` function.
@external(erlang, "glua_stdlib_ffi", "debug_getmetatable")
pub fn get_meta_table() -> Value

/// Returns the `getuservalue` function.
@external(erlang, "glua_stdlib_ffi", "debug_getuservalue")
pub fn get_user_value() -> Value

/// Returns the `setmetatable` function.
@external(erlang, "glua_stdlib_ffi", "debug_setmetatable")
pub fn set_meta_table() -> Value

/// Returns the `setuservalue` function.
@external(erlang, "glua_stdlib_ffi", "debug_setuservalue")
pub fn set_user_value() -> Value
