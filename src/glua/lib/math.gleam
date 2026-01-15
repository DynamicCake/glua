import glua.{type ValueRef}

pub const huge = 1.7976931348623157e308

pub const max_integer = 9_223_372_036_854_775_807

pub const min_integer = -9_223_372_036_854_775_808

pub const pi = 3.141592653589793

@external(erlang, "glua_stdlib_ffi", "math_abs")
pub fn abs() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_acos")
pub fn acos() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_asin")
pub fn asin() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_atan")
pub fn atan() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_atan2")
pub fn atan2() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_ceil")
pub fn ceil() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_cos")
pub fn cos() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_cosh")
pub fn cosh() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_deg")
pub fn deg() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_exp")
pub fn exp() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_floor")
pub fn floor() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_fmod")
pub fn fmod() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_frexp")
pub fn frexp() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_ldexp")
pub fn ldexp() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_log")
pub fn log() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_log10")
pub fn log10() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_max")
pub fn max() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_min")
pub fn min() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_modf")
pub fn modf() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_pow")
pub fn pow() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_rad")
pub fn rad() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_random")
pub fn random() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_randomseed")
pub fn random_seed() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_sin")
pub fn sin() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_sinh")
pub fn sinh() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_sqrt")
pub fn sqrt() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_tan")
pub fn tan() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_tanh")
pub fn tanh() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_tointeger")
pub fn to_integer() -> ValueRef

@external(erlang, "glua_stdlib_ffi", "math_type")
pub fn type_() -> ValueRef
