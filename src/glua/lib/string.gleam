import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "string_byte")
pub fn byte() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_char")
pub fn char() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_dump")
pub fn dump() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_find")
pub fn find() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_format")
pub fn format() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_gmatch")
pub fn gmatch() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_gsub")
pub fn gsub() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_len")
pub fn len() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_lower")
pub fn lower() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_match")
pub fn match() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_rep")
pub fn rep() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_reverse")
pub fn reverse() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_sub")
pub fn sub() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "string_upper")
pub fn upper() -> ValueRef
