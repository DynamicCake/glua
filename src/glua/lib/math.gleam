//// Bindings to the functions under `_G.math`

import glua.{type Value}

pub const huge = 1.7976931348623157e308

pub const max_integer = 9_223_372_036_854_775_807

pub const min_integer = -9_223_372_036_854_775_808

pub const pi = 3.141592653589793

/// Returns the `math.abs` function.
@external(erlang, "glua_stdlib_ffi", "math_abs")
pub fn abs() -> Value

/// Returns the `math.acos` function.
@external(erlang, "glua_stdlib_ffi", "math_acos")
pub fn acos() -> Value

/// Returns the `math.asin` function.
@external(erlang, "glua_stdlib_ffi", "math_asin")
pub fn asin() -> Value

/// Returns the `math.atan` function.
@external(erlang, "glua_stdlib_ffi", "math_atan")
pub fn atan() -> Value

/// Returns the `math.atan2` function.
///
/// As of Lua 5.3, `math.atan2` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_atan2")
pub fn atan2() -> Value

/// Returns the `math.ceil` function.
@external(erlang, "glua_stdlib_ffi", "math_ceil")
pub fn ceil() -> Value

/// Returns the `math.cos` function.
@external(erlang, "glua_stdlib_ffi", "math_cos")
pub fn cos() -> Value

/// Returns the `math.cosh` function.
///
/// As of Lua 5.3, `math.cosh` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_cosh")
pub fn cosh() -> Value

/// Returns the `math.deg` function.
@external(erlang, "glua_stdlib_ffi", "math_deg")
pub fn deg() -> Value

/// Returns the `math.exp` function.
@external(erlang, "glua_stdlib_ffi", "math_exp")
pub fn exp() -> Value

/// Returns the `math.floor` function.
@external(erlang, "glua_stdlib_ffi", "math_floor")
pub fn floor() -> Value

/// Returns the `math.fmod` function.
@external(erlang, "glua_stdlib_ffi", "math_fmod")
pub fn fmod() -> Value

/// Returns the `math.frexp` function.
///
/// As of Lua 5.3, `math.frexp` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_frexp")
pub fn frexp() -> Value

/// Returns the `math.ldexp` function.
///
/// As of Lua 5.3, `math.ldexp` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_ldexp")
pub fn ldexp() -> Value

/// Returns the `math.log` function.
@external(erlang, "glua_stdlib_ffi", "math_log")
pub fn log() -> Value

/// Returns the `math.log10` function.
@external(erlang, "glua_stdlib_ffi", "math_log10")
pub fn log10() -> Value

/// Returns the `math.max` function.
@external(erlang, "glua_stdlib_ffi", "math_max")
pub fn max() -> Value

/// Returns the `math.min` function.
@external(erlang, "glua_stdlib_ffi", "math_min")
pub fn min() -> Value

/// Returns the `math.modf` function.
@external(erlang, "glua_stdlib_ffi", "math_modf")
pub fn modf() -> Value

/// Returns the `math.pow` function.
///
/// As of Lua 5.3, `math.pow` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_pow")
pub fn pow() -> Value

/// Returns the `math.rad` function.
@external(erlang, "glua_stdlib_ffi", "math_rad")
pub fn rad() -> Value

/// Returns the `math.random` function.
@external(erlang, "glua_stdlib_ffi", "math_random")
pub fn random() -> Value

/// Returns the `math.randomseed` function.
@external(erlang, "glua_stdlib_ffi", "math_randomseed")
pub fn random_seed() -> Value

/// Returns the `math.sin` function.
@external(erlang, "glua_stdlib_ffi", "math_sin")
pub fn sin() -> Value

/// Returns the `math.sinh` function.
///
/// As of Lua 5.3, `math.sinh` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_sinh")
pub fn sinh() -> Value

/// Returns the `math.sqrt` function.
@external(erlang, "glua_stdlib_ffi", "math_sqrt")
pub fn sqrt() -> Value

/// Returns the `math.tan` function.
@external(erlang, "glua_stdlib_ffi", "math_tan")
pub fn tan() -> Value

/// Returns the `math.tanh` function.
///
/// As of Lua 5.3, `math.tanh` is deprecated
@external(erlang, "glua_stdlib_ffi", "math_tanh")
pub fn tanh() -> Value

/// Returns the `math.tointeger` function.
@external(erlang, "glua_stdlib_ffi", "math_tointeger")
pub fn to_integer() -> Value

/// Returns the `math.type` function.
@external(erlang, "glua_stdlib_ffi", "math_type")
pub fn type_() -> Value
