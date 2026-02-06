import glua.{type Value}

/// Returns the `io.flush` function.
@external(erlang, "glua_stdlib_ffi", "io_flush")
pub fn flush() -> Value

/// Returns the `io.write` function.
@external(erlang, "glua_stdlib_ffi", "io_write")
pub fn write() -> Value
