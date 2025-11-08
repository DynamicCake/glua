import gleam/dynamic

/// Represents an instance of the Lua VM.
pub type Lua

/// Top-level type to represent any Lua value.
/// This type is not precise and often needs to be decoded
/// into another type in order to be useful.
pub type Value =
  dynamic.Dynamic

/// Represents a Lua table.
pub type Table

/// Represents a Lua function.
pub type Function
