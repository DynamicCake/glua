# Glua

A library for embedding Lua in Gleam applications!

[![Package Version](https://img.shields.io/hexpm/v/glua)](https://hex.pm/packages/glua)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glua/)

```sh
gleam add glua@2
```

## Usage

### Executing Lua code

```gleam
  let code =
    "
function greet()
  return 'Hello from Lua!'
end

return greet()
"

  // Make a fresh instance lua instance
  let lua = glua.new()
  let assert Ok(#(_state, [result])) = glua.eval(state: lua, code:)

  assert result == glua.string("Hello from Lua!")
```

### Decoding output
The `deser` api is similar to the `decode`
```gleam
pub type Project {
  Project(name: String, language: String)
}
let code = "return { name = 'glua', written_in = 'gleam'}"
let assert Ok(#(lua, [table])) = glua.eval(glua.new(), code)

let deserializer = {
  use name <- deser.field(glua.string("name"), deser.string)
  use language <- deser.field(glua.string("written_in"), deser.string)
  deser.success(lua, Project(name:, language:))
}

let assert Ok(#(_lua, project)) = deser.run(lua, table, deserializer)
assert project == Project(name: "glua", language: "gleam")
```

### Parsing a chunk, then executing it
```gleam
let code = "return 'this is a chunk of Lua code'"
let assert Ok(#(state, chunk)) = glua.load(state: glua.new(), code:)
let assert Ok(#(_state, [result])) = glua.eval_chunk(state:, chunk:)

assert result == glua.string("this is a chunk of Lua code")
```

### Executing Lua files
```gleam
let assert Ok(#(_state, [n, m])) =
  glua.eval_file(state: glua.new(), path: "./test/lua/two_numbers.lua")

assert n == glua.int(1) && m == glua.int(2)
```

### Sandboxing
```gleam
let assert Ok(lua) = glua.new() |> glua.sandbox(["os", "execute"])
let assert Error(glua.LuaRuntimeException(exception, _)) =
  glua.eval(state: lua, code: "os.execute('rm -f important_file')")

// 'important_file' was not deleted
assert exception == glua.ErrorCall(["os.execute is sandboxed"])
```

### Getting values from Lua

```gleam
let assert Ok(version) = glua.get(state: glua.new(), keys: ["_VERSION"])

assert version == glua.string("Lua 5.3")
```

### Setting values in Lua

```gleam
// we need to encode any value we want to pass to Lua
let lua = glua.new()
// `keys` is the full path to where the value will be set
// and any intermediate table will be created if it is not present
let keys = ["my_table", "my_value"]
let assert Ok(lua) = glua.set(state: lua, keys:, value: glua.string("my_value"))

// now we can get the value
let assert Ok(value) = glua.get(state: lua, keys:)

// or return it from a Lua script
let assert Ok(#(_lua, [returned])) =
  glua.eval(state: lua, code: "return my_table.my_value")

assert value == glua.string("my_value")
assert returned == glua.string("my_value")
```

### Calling Lua functions from Gleam

```gleam
let lua = glua.new()
let assert Ok(val) = glua.get(state: lua, keys: ["math", "max"])
let assert Ok(#(lua, fun)) = deser.run(lua, val, deser.function)
let args = [1, 20, 7, 18] |> list.map(glua.int)

let assert Ok(#(lua, [result])) =
  glua.call_function(state: lua, fun: fun, args:)

let assert Ok(#(lua, result)) = deser.run(lua, result, deser.number)

assert result == 20.0

// `glua.call_function_by_name` is a shorthand for `glua.get` followed by `glua.call_function`
let assert Ok(#(_lua, [result])) =
  glua.call_function_by_name(state: lua, keys: ["math", "max"], args:)

let assert Ok(#(_lua, result)) = deser.run(lua, result, deser.number)

assert result == 20.0
```

### Exposing Gleam functions to Lua

```gleam
let lua = glua.new()
let #(lua, fun) =
  glua.function(lua, fn(lua, args) {
    // Since Gleam is a statically typed language, each and every argument must be decoded
    let assert [x, min, max] = args
    let assert Ok(#(lua, x)) = deser.run(lua, x, deser.number)
    let assert Ok(#(lua, min)) = deser.run(lua, min, deser.number)
    let assert Ok(#(lua, max)) = deser.run(lua, max, deser.number)

    let result = float.clamp(x, min, max)

    #(lua, [glua.float(result)])
  })

let keys = ["my_functions", "clamp"]

let assert Ok(lua) = glua.set(state: lua, keys:, value: fun)

let args = [2.3, 1.2, 2.1] |> list.map(glua.float)
let assert Ok(#(_lua, [result])) =
  glua.call_function_by_name(state: lua, keys:, args:)

assert result == glua.float(2.1)
```

Further documentation can be found at <https://hexdocs.pm/glua>.

## Credits

- [Luerl](https://github.com/rvirding/luerl): This library is powered by Luerl under the hood.
- [Elixir's Lua library](https://github.com/tv-labs/lua) - This library API is inspired by Elixir's Lua library.
