import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "table_concat")
pub fn concat() -> Value

@external(erlang, "glua_stdlib_ffi", "table_insert")
pub fn insert() -> Value

@external(erlang, "glua_stdlib_ffi", "table_pack")
pub fn pack() -> Value

@external(erlang, "glua_stdlib_ffi", "table_remove")
pub fn remove() -> Value

@external(erlang, "glua_stdlib_ffi", "table_sort")
pub fn sort() -> Value

@external(erlang, "glua_stdlib_ffi", "table_unpack")
pub fn unpack() -> Value
