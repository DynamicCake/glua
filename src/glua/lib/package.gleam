//// Bindings to the functions under `_G.package`

import glua.{type Value}

/// Returns the `package.searchpath` function.
@external(erlang, "glua_stdlib_ffi", "package_searchpath")
pub fn search_path() -> Value

/// Returns the `package.searchers[1]` function.
@external(erlang, "glua_stdlib_ffi", "package_preload_searcher")
pub fn preload_searcher() -> Value

/// Returns the `package.searchers[2]` function.
@external(erlang, "glua_stdlib_ffi", "package_lua_searcher")
pub fn lua_searcher() -> Value
