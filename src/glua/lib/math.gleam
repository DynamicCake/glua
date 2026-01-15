import glua.{type Value}

pub const huge = 1.7976931348623157e308

pub const max_integer = 9_223_372_036_854_775_807

pub const min_integer = -9_223_372_036_854_775_808

pub const pi = 3.141592653589793

@external(erlang, "glua_stdlib_ffi", "math_abs")
pub fn abs() -> Value

@external(erlang, "glua_stdlib_ffi", "math_acos")
pub fn acos() -> Value

@external(erlang, "glua_stdlib_ffi", "math_asin")
pub fn asin() -> Value

@external(erlang, "glua_stdlib_ffi", "math_atan")
pub fn atan() -> Value

@external(erlang, "glua_stdlib_ffi", "math_atan2")
pub fn atan2() -> Value

@external(erlang, "glua_stdlib_ffi", "math_ceil")
pub fn ceil() -> Value

@external(erlang, "glua_stdlib_ffi", "math_cos")
pub fn cos() -> Value

@external(erlang, "glua_stdlib_ffi", "math_cosh")
pub fn cosh() -> Value

@external(erlang, "glua_stdlib_ffi", "math_deg")
pub fn deg() -> Value

@external(erlang, "glua_stdlib_ffi", "math_exp")
pub fn exp() -> Value

@external(erlang, "glua_stdlib_ffi", "math_floor")
pub fn floor() -> Value

@external(erlang, "glua_stdlib_ffi", "math_fmod")
pub fn fmod() -> Value

@external(erlang, "glua_stdlib_ffi", "math_frexp")
pub fn frexp() -> Value

@external(erlang, "glua_stdlib_ffi", "math_ldexp")
pub fn ldexp() -> Value

@external(erlang, "glua_stdlib_ffi", "math_log")
pub fn log() -> Value

@external(erlang, "glua_stdlib_ffi", "math_log10")
pub fn log10() -> Value

@external(erlang, "glua_stdlib_ffi", "math_max")
pub fn max() -> Value

@external(erlang, "glua_stdlib_ffi", "math_min")
pub fn min() -> Value

@external(erlang, "glua_stdlib_ffi", "math_modf")
pub fn modf() -> Value

@external(erlang, "glua_stdlib_ffi", "math_pow")
pub fn pow() -> Value

@external(erlang, "glua_stdlib_ffi", "math_rad")
pub fn rad() -> Value

@external(erlang, "glua_stdlib_ffi", "math_random")
pub fn random() -> Value

@external(erlang, "glua_stdlib_ffi", "math_randomseed")
pub fn random_seed() -> Value

@external(erlang, "glua_stdlib_ffi", "math_sin")
pub fn sin() -> Value

@external(erlang, "glua_stdlib_ffi", "math_sinh")
pub fn sinh() -> Value

@external(erlang, "glua_stdlib_ffi", "math_sqrt")
pub fn sqrt() -> Value

@external(erlang, "glua_stdlib_ffi", "math_tan")
pub fn tan() -> Value

@external(erlang, "glua_stdlib_ffi", "math_tanh")
pub fn tanh() -> Value

@external(erlang, "glua_stdlib_ffi", "math_tointeger")
pub fn to_integer() -> Value

@external(erlang, "glua_stdlib_ffi", "math_type")
pub fn type_() -> Value
