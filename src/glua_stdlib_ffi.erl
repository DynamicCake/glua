-module(glua_stdlib_ffi).

-include_lib("luerl/include/luerl.hrl").

-export([
    %% luerl_lib_basic (top-level)
    assert/0,
    collectgarbage/0,
    dofile/0,
    eprint/0,
    error/0,
    getmetatable/0,
    ipairs/0,
    load/0,
    loadfile/0,
    loadstring/0,
    next/0,
    pairs/0,
    pcall/0,
    print/0,
    rawequal/0,
    rawget/0,
    rawlen/0,
    rawset/0,
    select/0,
    setmetatable/0,
    tonumber/0,
    tostring/0,
    type/0,
    unpack/0,

    %% luerl_lib_package
    require/0,
    package_searchpath/0,
    package_preload_searcher/0,
    package_lua_searcher/0,

    %% luerl_lib_math
    math_abs/0,
    math_acos/0,
    math_asin/0,
    math_atan/0,
    math_atan2/0,
    math_ceil/0,
    math_cos/0,
    math_cosh/0,
    math_deg/0,
    math_exp/0,
    math_floor/0,
    math_fmod/0,
    math_frexp/0,
    math_ldexp/0,
    math_log/0,
    math_log10/0,
    math_max/0,
    math_min/0,
    math_modf/0,
    math_pow/0,
    math_rad/0,
    math_random/0,
    math_randomseed/0,
    math_sin/0,
    math_sinh/0,
    math_sqrt/0,
    math_tan/0,
    math_tanh/0,
    math_tointeger/0,
    math_type/0,

    %% luerl_lib_os
    os_clock/0,
    os_date/0,
    os_difftime/0,
    os_execute/0,
    os_exit/0,
    os_getenv/0,
    os_remove/0,
    os_rename/0,
    os_time/0,
    os_tmpname/0,

    %% luerl_lib_string
    string_byte/0,
    string_char/0,
    string_dump/0,
    string_find/0,
    string_format/0,
    string_gmatch/0,
    string_gsub/0,
    string_len/0,
    string_lower/0,
    string_match/0,
    string_rep/0,
    string_reverse/0,
    string_sub/0,
    string_upper/0,

    %% luerl_lib_utf8
    utf8_char/0,
    utf8_codes/0,
    utf8_codepoint/0,
    utf8_len/0,
    utf8_offset/0,

    %% luerl_lib_table
    table_concat/0,
    table_insert/0,
    table_pack/0,
    table_remove/0,
    table_sort/0,
    table_unpack/0,

    %% luerl_lib_debug
    debug_getmetatable/0,
    debug_getuservalue/0,
    debug_setmetatable/0,
    debug_setuservalue/0,

    %% luerl_lib_bit32
    bit32_band/0,
    bit32_bnot/0,
    bit32_bor/0,
    bit32_btest/0,
    bit32_bxor/0,
    bit32_lshift/0,
    bit32_rshift/0,
    bit32_arshift/0,
    bit32_lrotate/0,
    bit32_rrotate/0,
    bit32_extract/0,
    bit32_replace/0,

    %% luerl_lib_io
    io_flush/0,
    io_write/0
]).

%% ------------------------------------------------------------------
%% luerl_lib_basic (top level)
%% ------------------------------------------------------------------

assert() ->
    #erl_mfa{m=luerl_lib_basic,f=assert}.

collectgarbage() ->
    #erl_mfa{m=luerl_lib_basic,f=collectgarbage}.

dofile() ->
    #erl_mfa{m=luerl_lib_basic,f=dofile}.

eprint() ->
    #erl_mfa{m=luerl_lib_basic,f=eprint}.

error() ->
    #erl_mfa{m=luerl_lib_basic,f=error_call}.

getmetatable() ->
    #erl_mfa{m=luerl_lib_basic,f=getmetatable}.

ipairs() ->
    #erl_mfa{m=luerl_lib_basic,f=ipairs}.

load() ->
    #erl_mfa{m=luerl_lib_basic,f=load}.

loadfile() ->
    #erl_mfa{m=luerl_lib_basic,f=loadfile}.

%% For Lua 5.1 compatibility
loadstring() ->
    #erl_mfa{m=luerl_lib_basic,f=loadstring}.

next() ->
    #erl_mfa{m=luerl_lib_basic,f=next}.

pairs() ->
    #erl_mfa{m=luerl_lib_basic,f=pairs}.

pcall() ->
    #erl_mfa{m=luerl_lib_basic,f=pcall}.

print() ->
    #erl_mfa{m=luerl_lib_basic,f=print}.

rawequal() ->
    #erl_mfa{m=luerl_lib_basic,f=rawequal}.

rawget() ->
    #erl_mfa{m=luerl_lib_basic,f=rawget}.

rawlen() ->
    #erl_mfa{m=luerl_lib_basic,f=rawlen}.

rawset() ->
    #erl_mfa{m=luerl_lib_basic,f=rawset}.

select() ->
    #erl_mfa{m=luerl_lib_basic,f=select}.

setmetatable() ->
    #erl_mfa{m=luerl_lib_basic,f=setmetatable}.

tonumber() ->
    #erl_mfa{m=luerl_lib_basic,f=tonumber}.

tostring() ->
    #erl_mfa{m=luerl_lib_basic,f=tostring}.

type() ->
    #erl_mfa{m=luerl_lib_basic,f=type}.

%% For Lua 5.1 compatibility
unpack() ->
    #erl_mfa{m=luerl_lib_basic,f=unpack}.

%% ------------------------------------------------------------------
%% luerl_lib_package
%% ------------------------------------------------------------------

require() ->
    #erl_mfa{m=luerl_lib_package,f=require}.

package_searchpath() ->
    #erl_mfa{m=luerl_lib_package,f=searchpath}.

%% package.searchers[1]
package_preload_searcher() ->
    #erl_mfa{m=luerl_lib_package,f=preload_searcher}.

%% package.searchers[2]
package_lua_searcher() ->
    #erl_mfa{m=luerl_lib_package,f=lua_searcher}.


%% ------------------------------------------------------------------
%% luerl_lib_math
%% ------------------------------------------------------------------

math_abs() ->
    #erl_mfa{m=luerl_lib_math,f=abs}.

math_acos() ->
    #erl_mfa{m=luerl_lib_math,f=acos}.

math_asin() ->
    #erl_mfa{m=luerl_lib_math,f=asin}.

math_atan() ->
    #erl_mfa{m=luerl_lib_math,f=atan}.

%% Deprecated: For 5.2 backwards compatibility
math_atan2() ->
    #erl_mfa{m=luerl_lib_math,f=atan2}.

math_ceil() ->
    #erl_mfa{m=luerl_lib_math,f=ceil}.

math_cos() ->
    #erl_mfa{m=luerl_lib_math,f=cos}.

%% For 5.2 backwards compatibility
math_cosh() ->
    #erl_mfa{m=luerl_lib_math,f=cosh}.

math_deg() ->
    #erl_mfa{m=luerl_lib_math,f=deg}.

math_exp() ->
    #erl_mfa{m=luerl_lib_math,f=exp}.

math_floor() ->
    #erl_mfa{m=luerl_lib_math,f=floor}.

math_fmod() ->
    #erl_mfa{m=luerl_lib_math,f=fmod}.

%% For 5.2 backwards compatibility
math_frexp() ->
    #erl_mfa{m=luerl_lib_math,f=frexp}.

%% For 5.2 backwards compatibility
math_ldexp() ->
    #erl_mfa{m=luerl_lib_math,f=ldexp}.

math_log() ->
    #erl_mfa{m=luerl_lib_math,f=log}.

%% For 5.1 backwards compatibility
math_log10() ->
    #erl_mfa{m=luerl_lib_math,f=log10}.

math_max() ->
    #erl_mfa{m=luerl_lib_math,f=max}.

math_min() ->
    #erl_mfa{m=luerl_lib_math,f=min}.

math_modf() ->
    #erl_mfa{m=luerl_lib_math,f=modf}.

math_pow() ->
    #erl_mfa{m=luerl_lib_math,f=pow}.

math_rad() ->
    #erl_mfa{m=luerl_lib_math,f=rad}.

math_random() ->
    #erl_mfa{m=luerl_lib_math,f=random}.

math_randomseed() ->
    #erl_mfa{m=luerl_lib_math,f=randomseed}.

math_sin() ->
    #erl_mfa{m=luerl_lib_math,f=sin}.

%% For 5.2 backwards compatibility
math_sinh() ->
    #erl_mfa{m=luerl_lib_math,f=sinh}.

math_sqrt() ->
    #erl_mfa{m=luerl_lib_math,f=sqrt}.

math_tan() ->
    #erl_mfa{m=luerl_lib_math,f=tan}.

%% For 5.2 backwards compatibility
math_tanh() ->
    #erl_mfa{m=luerl_lib_math,f=tanh}.

math_tointeger() ->
    #erl_mfa{m=luerl_lib_math,f=tointeger}.

math_type() ->
    #erl_mfa{m=luerl_lib_math,f=type}.

%% ------------------------------------------------------------------
%% luerl_lib_os
%% ------------------------------------------------------------------

os_clock() ->
    #erl_mfa{m=luerl_lib_os,f=clock}.

os_date() ->
    #erl_mfa{m=luerl_lib_os,f=date}.

os_difftime() ->
    #erl_mfa{m=luerl_lib_os,f=difftime}.

os_execute() ->
    #erl_mfa{m=luerl_lib_os,f=execute}.

os_exit() ->
    #erl_mfa{m=luerl_lib_os,f=lua_exit}.

os_getenv() ->
    #erl_mfa{m=luerl_lib_os,f=getenv}.

os_remove() ->
    #erl_mfa{m=luerl_lib_os,f=remove}.

os_rename() ->
    #erl_mfa{m=luerl_lib_os,f=rename}.

os_time() ->
    #erl_mfa{m=luerl_lib_os,f=time}.

os_tmpname() ->
    #erl_mfa{m=luerl_lib_os,f=tmpname}.

%% ------------------------------------------------------------------
%% luerl_lib_string
%% ------------------------------------------------------------------

string_byte() ->
    #erl_mfa{m=luerl_lib_string,f=byte}.

string_char() ->
    #erl_mfa{m=luerl_lib_string,f=char}.

string_dump() ->
    #erl_mfa{m=luerl_lib_string,f=dump}.

string_find() ->
    #erl_mfa{m=luerl_lib_string,f=find}.

string_format() ->
    #erl_mfa{m=luerl_lib_string,f=format}.

string_gmatch() ->
    #erl_mfa{m=luerl_lib_string,f=gmatch}.

string_gsub() ->
    #erl_mfa{m=luerl_lib_string,f=gsub}.

string_len() ->
    #erl_mfa{m=luerl_lib_string,f=len}.

string_lower() ->
    #erl_mfa{m=luerl_lib_string,f=lower}.

string_match() ->
    #erl_mfa{m=luerl_lib_string,f=match}.

string_rep() ->
    #erl_mfa{m=luerl_lib_string,f=rep}.

string_reverse() ->
    #erl_mfa{m=luerl_lib_string,f=reverse}.

string_sub() ->
    #erl_mfa{m=luerl_lib_string,f=sub}.

string_upper() ->
    #erl_mfa{m=luerl_lib_string,f=upper}.

%% ------------------------------------------------------------------
%% luerl_lib_utf8
%% ------------------------------------------------------------------

utf8_char() ->
    #erl_mfa{m=luerl_lib_utf8,f=utf8_char}.

utf8_codes() ->
    #erl_mfa{m=luerl_lib_utf8,f=codes}.

utf8_codepoint() ->
    #erl_mfa{m=luerl_lib_utf8,f=codepoint}.

utf8_len() ->
    #erl_mfa{m=luerl_lib_utf8,f=utf8_len}.

utf8_offset() ->
    #erl_mfa{m=luerl_lib_utf8,f=offset}.

%% ------------------------------------------------------------------
%% luerl_lib_table
%% ------------------------------------------------------------------

table_concat() ->
    #erl_mfa{m=luerl_lib_table,f=concat}.

table_insert() ->
    #erl_mfa{m=luerl_lib_table,f=insert}.

table_pack() ->
    #erl_mfa{m=luerl_lib_table,f=pack}.

table_remove() ->
    #erl_mfa{m=luerl_lib_table,f=remove}.

table_sort() ->
    #erl_mfa{m=luerl_lib_table,f=sort}.

table_unpack() ->
    #erl_mfa{m=luerl_lib_table,f=unpack}.

%% ------------------------------------------------------------------
%% luerl_lib_debug
%% ------------------------------------------------------------------

debug_getmetatable() ->
    #erl_mfa{m=luerl_lib_debug,f=getmetatable}.

debug_getuservalue() ->
    #erl_mfa{m=luerl_lib_debug,f=getuservalue}.

debug_setmetatable() ->
    #erl_mfa{m=luerl_lib_debug,f=setmetatable}.

debug_setuservalue() ->
    #erl_mfa{m=luerl_lib_debug,f=setuservalue}.

%% ------------------------------------------------------------------
%% luerl_lib_bit32
%% ------------------------------------------------------------------

bit32_band() ->
    #erl_mfa{m=luerl_lib_bit32,f=fband}.

bit32_bnot() ->
    #erl_mfa{m=luerl_lib_bit32,f=fbnot}.

bit32_bor() ->
    #erl_mfa{m=luerl_lib_bit32,f=fbor}.

bit32_btest() ->
    #erl_mfa{m=luerl_lib_bit32,f=fbtest}.

bit32_bxor() ->
    #erl_mfa{m=luerl_lib_bit32,f=fbxor}.

bit32_lshift() ->
    #erl_mfa{m=luerl_lib_bit32,f=flshift}.

bit32_rshift() ->
    #erl_mfa{m=luerl_lib_bit32,f=frshift}.

bit32_arshift() ->
    #erl_mfa{m=luerl_lib_bit32,f=farshift}.

bit32_lrotate() ->
    #erl_mfa{m=luerl_lib_bit32,f=flrotate}.

bit32_rrotate() ->
    #erl_mfa{m=luerl_lib_bit32,f=frrotate}.

bit32_extract() ->
    #erl_mfa{m=luerl_lib_bit32,f=fextract}.

bit32_replace() ->
    #erl_mfa{m=luerl_lib_bit32,f=freplace}.

%% ------------------------------------------------------------------
%% luerl_lib_io
%% ------------------------------------------------------------------

io_flush() ->
    #erl_mfa{m=luerl_lib_io,f=flush}.

io_write() ->
    #erl_mfa{m=luerl_lib_io,f=write}.

