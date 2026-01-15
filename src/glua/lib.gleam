import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "assert")
pub fn assert_() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "collectgarbage")
pub fn collect_garbage() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "dofile")
pub fn do_file() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "eprint")
pub fn eprint() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "error")
pub fn error() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "getmetatable")
pub fn get_meta_table() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "ipairs")
pub fn ipairs() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "load")
pub fn load() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "loadfile")
pub fn load_file() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "loadstring")
pub fn load_string() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "next")
pub fn next() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "pairs")
pub fn pairs() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "pcall")
pub fn pcall() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "print")
pub fn print() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "rawequal")
pub fn raw_equal() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "rawget")
pub fn raw_get() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "rawlen")
pub fn raw_len() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "rawset")
pub fn raw_set() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "select")
pub fn select() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "setmetatable")
pub fn set_meta_table() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "tonumber")
pub fn to_number() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "tostring")
pub fn to_string() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "type")
pub fn type_() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "unpack")
pub fn unpack() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "require")
pub fn require() -> ValueRef
