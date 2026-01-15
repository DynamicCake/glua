import glua.{type ValueRef}

@external(erlang, "glua_stdlib_ffi", "bit32_band")
pub fn band() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_bnot")
pub fn bnot() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_bor")
pub fn bor() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_btest")
pub fn btest() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_bxor")
pub fn bxor() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_lshift")
pub fn lshift() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_rshift")
pub fn rshift() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_arshift")
pub fn arshift() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_lrotate")
pub fn lrotate() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_rrotate")
pub fn rrotate() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_extract")
pub fn extract() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "bit32_replace")
pub fn replace() -> ValueRef
