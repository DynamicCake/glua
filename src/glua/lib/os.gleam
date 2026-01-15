import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "os_clock")
pub fn clock() -> Value

@external(erlang, "glua_stdlib_ffi", "os_date")
pub fn date() -> Value

@external(erlang, "glua_stdlib_ffi", "os_difftime")
pub fn difftime() -> Value

@external(erlang, "glua_stdlib_ffi", "os_execute")
pub fn execute() -> Value

@external(erlang, "glua_stdlib_ffi", "os_exit")
pub fn exit() -> Value

@external(erlang, "glua_stdlib_ffi", "os_getenv")
pub fn getenv() -> Value

@external(erlang, "glua_stdlib_ffi", "os_remove")
pub fn remove() -> Value

@external(erlang, "glua_stdlib_ffi", "os_rename")
pub fn rename() -> Value

@external(erlang, "glua_stdlib_ffi", "os_time")
pub fn time() -> Value

@external(erlang, "glua_stdlib_ffi", "os_tmpname")
pub fn tmpname() -> Value
