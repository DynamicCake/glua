import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "assert")
pub fn assert_() -> Value

@external(erlang, "glua_stdlib_ffi", "collectgarbage")
pub fn collect_garbage() -> Value

@external(erlang, "glua_stdlib_ffi", "dofile")
pub fn do_file() -> Value

@external(erlang, "glua_stdlib_ffi", "eprint")
pub fn eprint() -> Value

@external(erlang, "glua_stdlib_ffi", "error")
pub fn error() -> Value

@external(erlang, "glua_stdlib_ffi", "getmetatable")
pub fn get_meta_table() -> Value

@external(erlang, "glua_stdlib_ffi", "ipairs")
pub fn ipairs() -> Value

@external(erlang, "glua_stdlib_ffi", "load")
pub fn load() -> Value

@external(erlang, "glua_stdlib_ffi", "loadfile")
pub fn load_file() -> Value

@external(erlang, "glua_stdlib_ffi", "loadstring")
pub fn load_string() -> Value

@external(erlang, "glua_stdlib_ffi", "next")
pub fn next() -> Value

@external(erlang, "glua_stdlib_ffi", "pairs")
pub fn pairs() -> Value

@external(erlang, "glua_stdlib_ffi", "pcall")
pub fn pcall() -> Value

@external(erlang, "glua_stdlib_ffi", "print")
pub fn print() -> Value

@external(erlang, "glua_stdlib_ffi", "rawequal")
pub fn raw_equal() -> Value

@external(erlang, "glua_stdlib_ffi", "rawget")
pub fn raw_get() -> Value

@external(erlang, "glua_stdlib_ffi", "rawlen")
pub fn raw_len() -> Value

@external(erlang, "glua_stdlib_ffi", "rawset")
pub fn raw_set() -> Value

@external(erlang, "glua_stdlib_ffi", "select")
pub fn select() -> Value

@external(erlang, "glua_stdlib_ffi", "setmetatable")
pub fn set_meta_table() -> Value

@external(erlang, "glua_stdlib_ffi", "tonumber")
pub fn to_number() -> Value

@external(erlang, "glua_stdlib_ffi", "tostring")
pub fn to_string() -> Value

@external(erlang, "glua_stdlib_ffi", "type")
pub fn type_() -> Value

@external(erlang, "glua_stdlib_ffi", "unpack")
pub fn unpack() -> Value

@external(erlang, "glua_stdlib_ffi", "require")
pub fn require() -> Value
