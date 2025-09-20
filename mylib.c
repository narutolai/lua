#include <stdio.h>
#include <string.h>
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"

typedef struct myUdata
{
    int a;
    int b;
}myUdata;


int initUdata(lua_State* L) {
    myUdata *p = (myUdata *)lua_newuserdatauv(L, sizeof(myUdata), 0);
    p->a = 1;
    p->b = 2;
    luaL_newmetatable(L, "myUdata");
    lua_setmetatable(L, -2);
    return 1;  // 明确表示无返回值
}
int getval(lua_State *L)
{
    myUdata *ud = (myUdata *)luaL_checkudata(L, 1, "myUdata");
    if (ud==NULL)
    {
        printf("nodata");
    }

    lua_pushinteger(L,ud->a);
    return 1;
}
static const luaL_Reg co_funcs[] = {
    {"init", initUdata},
    {"getval",getval},
    {NULL, NULL}
};

int luaopen_myUdata(lua_State* L) {
    luaL_newlib(L, co_funcs);
    return 1;  // 返回模块表
}

