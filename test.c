#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"
#include <unistd.h>
#include <stdlib.h> 
//gcc -g -O0 test.c -Lsrc -llua -lm -ldl

int main() {
    // lua_State *L = luaL_newstate();
    // luaL_openlibs(L);
    // luaL_loadfile(L, "main.lua");
    // // int a = 1;
    // // while (1)
    // // {   
    // //     lua_pushvalue(L,1);
    //     lua_pcall(L, 0, 0, 0);
    // //     printf("count : %d\n",a);
    // //     sleep(3);
    // //     a = a + 1;
    // // }
    // lua_close(L);

    const char *str= "9223372036854775808";
    double mode = strtod(str,NULL);
    printf("mode:%e\n", mode);
    return 0;
}
