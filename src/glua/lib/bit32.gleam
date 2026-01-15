import glua.{type Value}

@external(erlang, "glua_stdlib_ffi", "bit32_band")
pub fn band() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_bnot")
pub fn bnot() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_bor")
pub fn bor() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_btest")
pub fn btest() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_bxor")
pub fn bxor() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_lshift")
pub fn lshift() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_rshift")
pub fn rshift() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_arshift")
pub fn arshift() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_lrotate")
pub fn lrotate() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_rrotate")
pub fn rrotate() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_extract")
pub fn extract() -> Value

@external(erlang, "glua_stdlib_ffi", "bit32_replace")
pub fn replace() -> Value
