import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "os_clock")
pub fn clock() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_date")
pub fn date() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_difftime")
pub fn difftime() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_execute")
pub fn execute() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_exit")
pub fn exit() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_getenv")
pub fn getenv() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_remove")
pub fn remove() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_rename")
pub fn rename() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_time")
pub fn time() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "os_tmpname")
pub fn tmpname() -> ValueRef
