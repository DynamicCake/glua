//// Bindings to the functions under `_G.table`

import glua.{type Value}

/// Returns the `table.concat` function.
@external(erlang, "glua_stdlib_ffi", "table_concat")
pub fn concat() -> Value

/// Returns the `table.insert` function.
@external(erlang, "glua_stdlib_ffi", "table_insert")
pub fn insert() -> Value

/// Returns the `table.pack` function.
@external(erlang, "glua_stdlib_ffi", "table_pack")
pub fn pack() -> Value

/// Returns the `table.remove` function.
@external(erlang, "glua_stdlib_ffi", "table_remove")
pub fn remove() -> Value

/// Returns the `table.sort` function.
@external(erlang, "glua_stdlib_ffi", "table_sort")
pub fn sort() -> Value

/// Returns the `table.unpack` function.
@external(erlang, "glua_stdlib_ffi", "table_unpack")
pub fn unpack() -> Value
