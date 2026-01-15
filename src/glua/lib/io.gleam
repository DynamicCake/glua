import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "io_flush")
pub fn flush() -> Value

@external(erlang, "glua_stdlib_ffi", "io_write")
pub fn write() -> Value
