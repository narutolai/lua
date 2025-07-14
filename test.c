#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

//gcc -g -O0 test.c -Lsrc -llua -lm -ldl

int main() {
    lua_State *L = luaL_newstate();
    luaL_openlibs(L);
    luaL_loadfile(L, "test.lua");
    lua_pcall(L, 0, 0, 0);
    lua_close(L);
    return 0;
}
