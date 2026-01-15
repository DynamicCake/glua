import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "package_searchpath")
pub fn search_path() -> Value

@external(erlang, "glua_stdlib_ffi", "package_preload_searcher")
pub fn preload_searcher() -> Value

@external(erlang, "glua_stdlib_ffi", "package_lua_searcher")
pub fn lua_searcher() -> Value
