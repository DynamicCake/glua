import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "utf8_char")
pub fn char() -> Value

@external(erlang, "glua_stdlib_ffi", "utf8_codes")
pub fn codes() -> Value

@external(erlang, "glua_stdlib_ffi", "utf8_codepoint")
pub fn codepoint() -> Value

@external(erlang, "glua_stdlib_ffi", "utf8_len")
pub fn len() -> Value

@external(erlang, "glua_stdlib_ffi", "utf8_offset")
pub fn offset() -> Value
