import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "utf8_char")
pub fn char() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "utf8_codes")
pub fn codes() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "utf8_codepoint")
pub fn codepoint() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "utf8_len")
pub fn len() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "utf8_offset")
pub fn offset() -> ValueRef
