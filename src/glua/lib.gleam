//// Bindings to the functions under `_G`

import glua.{type Value}

/// Returns the `assert` function.
@external(erlang, "glua_stdlib_ffi", "assert")
pub fn assert_() -> Value

/// Returns the `collectgarbage` function.
@external(erlang, "glua_stdlib_ffi", "collectgarbage")
pub fn collect_garbage() -> Value

/// Returns the `dofile` function.
@external(erlang, "glua_stdlib_ffi", "dofile")
pub fn do_file() -> Value

/// Returns the `eprint` function.
@external(erlang, "glua_stdlib_ffi", "eprint")
pub fn eprint() -> Value

/// Returns the `error` function.
@external(erlang, "glua_stdlib_ffi", "error")
pub fn error() -> Value

/// Returns the `getmetatable` function.
@external(erlang, "glua_stdlib_ffi", "getmetatable")
pub fn get_meta_table() -> Value

/// Returns the `ipairs` function.
@external(erlang, "glua_stdlib_ffi", "ipairs")
pub fn ipairs() -> Value

/// Returns the `load` function.
@external(erlang, "glua_stdlib_ffi", "load")
pub fn load() -> Value

/// Returns the `loadfile` function.
@external(erlang, "glua_stdlib_ffi", "loadfile")
pub fn load_file() -> Value

/// Returns the `loadstring` function.
///
/// As of Lua 5.2, `loadstring` is deprecated.
@external(erlang, "glua_stdlib_ffi", "loadstring")
pub fn load_string() -> Value

/// Returns the `next` function.
@external(erlang, "glua_stdlib_ffi", "next")
pub fn next() -> Value

/// Returns the `pairs` function.
@external(erlang, "glua_stdlib_ffi", "pairs")
pub fn pairs() -> Value

/// Returns the `pcall` function.
@external(erlang, "glua_stdlib_ffi", "pcall")
pub fn pcall() -> Value

/// Returns the `print` function.
@external(erlang, "glua_stdlib_ffi", "print")
pub fn print() -> Value

/// Returns the `rawequal` function.
@external(erlang, "glua_stdlib_ffi", "rawequal")
pub fn raw_equal() -> Value

/// Returns the `rawget` function.
@external(erlang, "glua_stdlib_ffi", "rawget")
pub fn raw_get() -> Value

/// Returns the `rawlen` function.
@external(erlang, "glua_stdlib_ffi", "rawlen")
pub fn raw_len() -> Value

/// Returns the `rawset` function.
@external(erlang, "glua_stdlib_ffi", "rawset")
pub fn raw_set() -> Value

/// Returns the `select` function.
@external(erlang, "glua_stdlib_ffi", "select")
pub fn select() -> Value

/// Returns the `setmetatable` function.
@external(erlang, "glua_stdlib_ffi", "setmetatable")
pub fn set_meta_table() -> Value

/// Returns the `tonumber` function.
@external(erlang, "glua_stdlib_ffi", "tonumber")
pub fn to_number() -> Value

/// Returns the `tostring` function.
@external(erlang, "glua_stdlib_ffi", "tostring")
pub fn to_string() -> Value

/// Returns the `type` function.
@external(erlang, "glua_stdlib_ffi", "type")
pub fn type_() -> Value

/// Returns the `unpack` function.
///
/// As of Lua 5.2, `unpack` was moved to `table.unpack`
@external(erlang, "glua_stdlib_ffi", "unpack")
pub fn unpack() -> Value

/// Returns the `require` function.
@external(erlang, "glua_stdlib_ffi", "require")
pub fn require() -> Value
