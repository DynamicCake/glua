import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "io_flush")
pub fn flush() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "io_write")
pub fn write() -> ValueRef
