//// Bindings to the functions under `_G.os`

import glua.{type Value}

/// Returns the `os.clock` function.
@external(erlang, "glua_stdlib_ffi", "os_clock")
pub fn clock() -> Value

/// Returns the `os.date` function.
@external(erlang, "glua_stdlib_ffi", "os_date")
pub fn date() -> Value

/// Returns the `os.difftime` function.
@external(erlang, "glua_stdlib_ffi", "os_difftime")
pub fn difftime() -> Value

/// Returns the `os.execute` function.
@external(erlang, "glua_stdlib_ffi", "os_execute")
pub fn execute() -> Value

/// Returns the `os.exit` function.
@external(erlang, "glua_stdlib_ffi", "os_exit")
pub fn exit() -> Value

/// Returns the `os.getenv` function.
@external(erlang, "glua_stdlib_ffi", "os_getenv")
pub fn getenv() -> Value

/// Returns the `os.remove` function.
@external(erlang, "glua_stdlib_ffi", "os_remove")
pub fn remove() -> Value

/// Returns the `os.rename` function.
@external(erlang, "glua_stdlib_ffi", "os_rename")
pub fn rename() -> Value

/// Returns the `os.time` function.
@external(erlang, "glua_stdlib_ffi", "os_time")
pub fn time() -> Value

/// Returns the `os.tmpname` function.
@external(erlang, "glua_stdlib_ffi", "os_tmpname")
pub fn tmpname() -> Value
