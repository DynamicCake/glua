import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "table_concat")
pub fn concat() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "table_insert")
pub fn insert() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "table_pack")
pub fn pack() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "table_remove")
pub fn remove() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "table_sort")
pub fn sort() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "table_unpack")
pub fn unpack() -> ValueRef
