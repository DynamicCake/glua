import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "debug_getmetatable")
pub fn get_meta_table() -> Value

@external(erlang, "glua_stdlib_ffi", "debug_getuservalue")
pub fn get_user_value() -> Value

@external(erlang, "glua_stdlib_ffi", "debug_setmetatable")
pub fn set_meta_table() -> Value

@external(erlang, "glua_stdlib_ffi", "debug_setuservalue")
pub fn set_user_value() -> Value
