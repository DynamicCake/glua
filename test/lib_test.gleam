import glua
import glua/lib
import glua/lib/bit32
import glua/lib/debug
import glua/lib/io
import glua/lib/math
import glua/lib/os
import glua/lib/package
import glua/lib/string
import glua/lib/table
import glua/lib/utf8

fn call(val: glua.ValueRef, args: List(glua.Value)) {
  glua.ref_call_function(glua.new(), val, args)
}

pub fn lib_basic_test() {
  let assert Error(_err) = call(lib.assert_(), [])
  lib.collect_garbage()
  lib.do_file()
  lib.do_file()
  lib.eprint()
  lib.error()
  lib.get_meta_table()
  lib.ipairs()
  lib.load()
  lib.load_file()
  lib.load_string()
  lib.next()
  lib.pairs()
  lib.pcall()
  lib.print()
  lib.raw_equal()
  lib.raw_get()
  lib.raw_len()
  lib.raw_set()
  lib.select()
  lib.set_meta_table()
  lib.to_number()
  lib.to_string()
  lib.type_()
  lib.unpack()
  lib.require()
}

pub fn lib_package_test() {
  package.lua_searcher()
  package.preload_searcher()
  package.search_path()
}

pub fn lib_bit32_test() {
  bit32.band()
  bit32.bnot()
  bit32.bor()
  bit32.btest()
  bit32.bxor()
  bit32.lshift()
  bit32.rshift()
  bit32.arshift()
  bit32.lrotate()
  bit32.rrotate()
  bit32.extract()
  bit32.replace()
}

pub fn lib_io_test() {
  io.flush()
  io.write()
}

pub fn lib_math_test() {
  math.abs()
  math.acos()
  math.asin()
  math.atan()
  math.atan2()
  math.ceil()
  math.cos()
  math.cosh()
  math.deg()
  math.exp()
  math.floor()
  math.fmod()
  math.frexp()
  math.ldexp()
  math.log()
  math.log10()
  math.max()
  math.min()
  math.modf()
  math.pow()
  math.rad()
  math.random()
  math.random_seed()
  math.sin()
  math.sinh()
  math.sqrt()
  math.tan()
  math.tanh()
  math.to_integer()
  math.type_()
}

pub fn lib_debug_test() {
  debug.get_meta_table()
  debug.get_user_value()
  debug.set_meta_table()
  debug.set_user_value()
}

pub fn lib_table_test() {
  table.concat()
  table.insert()
  table.pack()
  table.remove()
  table.sort()
  table.unpack()
}

pub fn lib_utf8_test() {
  utf8.char()
  utf8.codes()
  utf8.codepoint()
  utf8.len()
  utf8.offset()
}

pub fn lib_string_test() {
  string.byte()
  string.char()
  string.dump()
  string.find()
  string.format()
  string.gmatch()
  string.gsub()
  string.len()
  string.lower()
  string.match()
  string.rep()
  string.reverse()
  string.sub()
  string.upper()
}

pub fn lib_os_test() {
  os.clock()
  os.date()
  os.difftime()
  os.execute()
  os.exit()
  os.getenv()
  os.remove()
  os.rename()
  os.time()
  os.tmpname()
}
