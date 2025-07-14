#include <stdio.h>
#include <string.h>
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"

static int call_lua_func(lua_State* L) {
    // 检查栈顶是否是函数
    if (!lua_isfunction(L, 1)) {
        lua_pushliteral(L, "argument must be a function");
        lua_error(L);  // 抛出错误
    }

    // 调用函数：传入 1 个参数，期望 0 个返回值
    int ret = lua_pcall(L, 0, 0, 0);
    if (ret != LUA_OK) {
        // 获取错误消息并抛出
        lua_error(L);  // 错误消息已在栈顶
    }

    return 1;  // 明确表示无返回值
}

static const luaL_Reg co_funcs[] = {
    {"call_lua_func", call_lua_func},
    {NULL, NULL}
};

int luaopen_mylib(lua_State* L) {
    luaL_newlib(L, co_funcs);
    return 1;  // 返回模块表
}

