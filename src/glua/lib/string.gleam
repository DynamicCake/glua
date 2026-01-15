import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "string_byte")
pub fn byte() -> Value

@external(erlang, "glua_stdlib_ffi", "string_char")
pub fn char() -> Value

@external(erlang, "glua_stdlib_ffi", "string_dump")
pub fn dump() -> Value

@external(erlang, "glua_stdlib_ffi", "string_find")
pub fn find() -> Value

@external(erlang, "glua_stdlib_ffi", "string_format")
pub fn format() -> Value

@external(erlang, "glua_stdlib_ffi", "string_gmatch")
pub fn gmatch() -> Value

@external(erlang, "glua_stdlib_ffi", "string_gsub")
pub fn gsub() -> Value

@external(erlang, "glua_stdlib_ffi", "string_len")
pub fn len() -> Value

@external(erlang, "glua_stdlib_ffi", "string_lower")
pub fn lower() -> Value

@external(erlang, "glua_stdlib_ffi", "string_match")
pub fn match() -> Value

@external(erlang, "glua_stdlib_ffi", "string_rep")
pub fn rep() -> Value

@external(erlang, "glua_stdlib_ffi", "string_reverse")
pub fn reverse() -> Value

@external(erlang, "glua_stdlib_ffi", "string_sub")
pub fn sub() -> Value

@external(erlang, "glua_stdlib_ffi", "string_upper")
pub fn upper() -> Value
