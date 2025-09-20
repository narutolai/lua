/*
** $Id: lparser.c $
** Lua Parser
** See Copyright Notice in lua.h
*/

#define lparser_c
#define LUA_CORE

#include "lprefix.h"


#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include <stdio.h>
FILE *llex_file = NULL;
//语法分析

void ShowParselog(const char* logstr)
{
  if (llex_file == NULL)
  {
    llex_file = fopen("parse.txt","w");
  }
  fprintf(llex_file,"%s\n",logstr);
}

char* log_expdesc(const expdesc *exp) {
    if (exp == NULL) {
        char *result = malloc(20);
        strcpy(result, "expdesc: NULL");
        return result;
    }
    
    // 计算缓冲区大小
    size_t buffer_size = 512;
    char *buffer = malloc(buffer_size);
    if (buffer == NULL) {
        return NULL;
    }
    
    char *pos = buffer;
    int remaining = buffer_size;
    int written;
    
    // 输出基本信息和类型
    const char *kind_str;
    switch (exp->k) {
        case VVOID:     kind_str = "VVOID"; break;
        case VNIL:      kind_str = "VNIL"; break;
        case VTRUE:     kind_str = "VTRUE"; break;
        case VFALSE:    kind_str = "VFALSE"; break;
        case VK:        kind_str = "VK"; break;
        case VKFLT:     kind_str = "VKFLT"; break;
        case VKINT:     kind_str = "VKINT"; break;
        case VKSTR:     kind_str = "VKSTR"; break;
        case VNONRELOC: kind_str = "VNONRELOC"; break;
        case VLOCAL:    kind_str = "VLOCAL"; break;
        case VUPVAL:    kind_str = "VUPVAL"; break;
        case VCONST:    kind_str = "VCONST"; break;
        case VINDEXED:  kind_str = "VINDEXED"; break;
        case VINDEXUP:  kind_str = "VINDEXUP"; break;
        case VINDEXI:   kind_str = "VINDEXI"; break;
        case VINDEXSTR: kind_str = "VINDEXSTR"; break;
        case VJMP:      kind_str = "VJMP"; break;
        case VRELOC:    kind_str = "VRELOC"; break;
        case VCALL:     kind_str = "VCALL"; break;
        case VVARARG:   kind_str = "VVARARG"; break;
        default:        kind_str = "UNKNOWN";
    }
    
    written = snprintf(pos, remaining, 
                     "expdesc { k=%s(%d), t=%d, f=%d, u=",
                     kind_str, exp->k, exp->t, exp->f);
    if (written < 0 || written >= remaining) {
        free(buffer);
        return NULL;
    }
    pos += written;
    remaining -= written;
    
    // 根据类型输出 union 字段
    switch (exp->k) {
        case VKINT:
            written = snprintf(pos, remaining, "{ival=%lld}", (long long)exp->u.ival);
            break;
            
        case VKFLT:
            written = snprintf(pos, remaining, "{nval=%f}", (double)exp->u.nval);
            break;
            
        case VKSTR:
            written = snprintf(pos, remaining, "{strval=%p", (void*)exp->u.strval);
            pos += written;
            remaining -= written;
            
            if (exp->u.strval != NULL) {
                written = snprintf(pos, remaining, "(\"%s\")}", exp->u.strval->contents);
            } else {
                written = snprintf(pos, remaining, "(NULL)}");
            }
            break;
            
        case VLOCAL:
            written = snprintf(pos, remaining, 
                             "{var.ridx=%u, var.vidx=%u}",
                             (unsigned int)exp->u.var.ridx,
                             (unsigned int)exp->u.var.vidx);
            break;
            
        case VINDEXED:
        case VINDEXUP:
        case VINDEXI:
        case VINDEXSTR:
            written = snprintf(pos, remaining, 
                             "{ind.t=%u, ind.idx=%d}",
                             (unsigned int)exp->u.ind.t,
                             exp->u.ind.idx);
            break;
            
        case VK:
        case VUPVAL:
        case VCONST:
        case VNONRELOC:
        case VJMP:
        case VRELOC:
        case VCALL:
        case VVARARG:
            written = snprintf(pos, remaining, "{info=%d}", exp->u.info);
            break;
            
        case VVOID:
        case VNIL:
        case VTRUE:
        case VFALSE:
        default:
            written = snprintf(pos, remaining, "{}");
            break;
    }
    
    if (written < 0 || written >= remaining) {
        free(buffer);
        return NULL;
    }
    pos += written;
    remaining -= written;
    
    // 结束
    written = snprintf(pos, remaining, " }");
    
    return buffer;
}
#define LOG_BUFFER_SIZE 51
char* log_FuncState(const FuncState* fs) {
    if (fs == NULL) {
        return strdup("FuncState: NULL");
    }

    char* buffer = (char*)malloc(LOG_BUFFER_SIZE);
    if (buffer == NULL) {
        return NULL;
    }

    snprintf(buffer, LOG_BUFFER_SIZE,
        "f=%p, prev=%p, ls=%p, bl=%p, pc=%d, lasttarget=%d, previousline=%d, "
        "nk=%d, np=%d, nabslineinfo=%d, firstlocal=%d, firstlabel=%d, "
        "ndebugvars=%d, nactvar=%u, nups=%u, freereg=%u, iwthabs=%u, needclose=%u",
        (void*)fs->f,
        (void*)fs->prev,
        (void*)fs->ls,
        (void*)fs->bl,
        fs->pc,
        fs->lasttarget,
        fs->previousline,
        fs->nk,
        fs->np,
        fs->nabslineinfo,
        fs->firstlocal,
        fs->firstlabel,
        fs->ndebugvars,
        (unsigned int)fs->nactvar,
        (unsigned int)fs->nups,
        (unsigned int)fs->freereg,
        (unsigned int)fs->iwthabs,
        (unsigned int)fs->needclose
    );

    return buffer;
}
/* maximum number of local variables per function (must be smaller
   than 250, due to the bytecode format) */
#define MAXVARS		200


#define hasmultret(k)		((k) == VCALL || (k) == VVARARG)


/* because all strings are unified by the scanner, the parser
   can use pointer equality for string equality */
#define eqstr(a,b)	((a) == (b))


/*
** nodes for block list (list of active blocks)
*/
typedef struct BlockCnt {
  struct BlockCnt *previous;  /* chain */
  int firstlabel;  /* index of first label in this block */
  int firstgoto;  /* index of first pending goto in this block */
  lu_byte nactvar;  /* # active locals outside the block */
  lu_byte upval;  /* true if some variable in the block is an upvalue */
  lu_byte isloop;  /* true if 'block' is a loop */
  lu_byte insidetbc;  /* true if inside the scope of a to-be-closed var. */
} BlockCnt;

char* log_blockcnt(const BlockCnt *blk) {
    if (blk == NULL) {
        char *result = malloc(20);
        strcpy(result, "BlockCnt: NULL");
        return result;
    }
    
    // 计算大致需要的缓冲区大小
    size_t buffer_size = 256;
    char *buffer = malloc(buffer_size);
    if (buffer == NULL) {
        return NULL;
    }
    
    // 格式化输出
    snprintf(buffer, buffer_size, 
             "BlockCnt { "
             "previous=%p, "
             "firstlabel=%d, "
             "firstgoto=%d, "
             "nactvar=%u, "
             "upval=%s, "
             "isloop=%s, "
             "insidetbc=%s "
             "}",
             (void*)blk->previous,
             blk->firstlabel,
             blk->firstgoto,
             (unsigned int)blk->nactvar,
             blk->upval ? "true" : "false",
             blk->isloop ? "true" : "false",
             blk->insidetbc ? "true" : "false");
    
    return buffer;
}

/*
** prototypes for recursive non-terminal functions
*/
static void statement (LexState *ls);
static void expr (LexState *ls, expdesc *v);


static l_noret error_expected (LexState *ls, int token) {
  luaX_syntaxerror(ls,
      luaO_pushfstring(ls->L, "%s expected", luaX_token2str(ls, token)));
}


static l_noret errorlimit (FuncState *fs, int limit, const char *what) {
  lua_State *L = fs->ls->L;
  const char *msg;
  int line = fs->f->linedefined;
  const char *where = (line == 0)
                      ? "main function"
                      : luaO_pushfstring(L, "function at line %d", line);
  msg = luaO_pushfstring(L, "too many %s (limit is %d) in %s",
                             what, limit, where);
  luaX_syntaxerror(fs->ls, msg);
}


static void checklimit (FuncState *fs, int v, int l, const char *what) {
  if (v > l) errorlimit(fs, l, what);
}


/*
** Test whether next token is 'c'; if so, skip it.
*/
static int testnext (LexState *ls, int c) {
  if (ls->t.token == c) {
    luaX_next(ls);
    return 1;
  }
  else return 0;
}


/*
** Check that next token is 'c'.
*/
static void check (LexState *ls, int c) {
  if (ls->t.token != c)
    error_expected(ls, c);
}


/*
** Check that next token is 'c' and skip it.
*/
static void checknext (LexState *ls, int c) {
  check(ls, c);
  luaX_next(ls);
}


#define check_condition(ls,c,msg)	{ if (!(c)) luaX_syntaxerror(ls, msg); }


/*
** Check that next token is 'what' and skip it. In case of error,
** raise an error that the expected 'what' should match a 'who'
** in line 'where' (if that is not the current line).
*/
static void check_match (LexState *ls, int what, int who, int where) {
  if (l_unlikely(!testnext(ls, what))) {
    if (where == ls->linenumber)  /* all in the same line? */
      error_expected(ls, what);  /* do not need a complex message */
    else {
      luaX_syntaxerror(ls, luaO_pushfstring(ls->L,
             "%s expected (to close %s at line %d)",
              luaX_token2str(ls, what), luaX_token2str(ls, who), where));
    }
  }
}


static TString *str_checkname (LexState *ls) {
  TString *ts;
  check(ls, TK_NAME);
  ts = ls->t.seminfo.ts;
  luaX_next(ls);
  return ts;
}
const char* expkind_to_string(expkind kind) {
  switch (kind) {
    case VVOID:      return "VVOID";      /* 当 'expdesc' 描述一个列表的最后一个表达式时，这种类型表示一个空列表（即没有表达式） */
    case VNIL:       return "VNIL";       /* 常量 nil */
    case VTRUE:     return "VTRUE";      /* 常量 true */
    case VFALSE:    return "VFALSE";     /* 常量 false */
    case VK:        return "VK";         /* 常量在 'k' 中；info = 常量在 'k' 中的索引 */
    case VKFLT:     return "VKFLT";      /* 浮点常量；nval = 数值浮点值 */
    case VKINT:     return "VKINT";      /* 整数常量；ival = 数值整数值 */
    case VKSTR:     return "VKSTR";      /* 字符串常量；strval = TString 地址（字符串由词法分析器固定） */
    case VNONRELOC: return "VNONRELOC";  /* 表达式的值在一个固定的寄存器中；info = 结果寄存器 */
    case VLOCAL:    return "VLOCAL";     /* 局部变量；var.ridx = 寄存器索引；var.vidx = 在 'actvar.arr' 中的相对索引 */
    case VUPVAL:    return "VUPVAL";     /* 上值变量；info = 上值在 'upvalues' 中的索引 */
    case VCONST:    return "VCONST";     /* 编译时 <const> 变量；info = 在 'actvar.arr' 中的绝对索引 */
    case VINDEXED:  return "VINDEXED";   /* 索引变量；ind.t = 表在的寄存器index；ind.idx = 键的 R 索引 */
    case VINDEXUP:  return "VINDEXUP";   /* 索引上值；ind.t = 表在上值的index；ind.idx = 键的 K 索引 */
    case VINDEXI:   return "VINDEXI";    /* 带有常量整数的索引变量；ind.t = 表寄存器；ind.idx = 键的值 */
    case VINDEXSTR: return "VINDEXSTR";  /* 带有字面字符串的索引变量；ind.t = 表寄存器；ind.idx = 键的 K 索引 */
    case VJMP:      return "VJMP";       /* 表达式是一个测试/比较；info = 对应跳转指令的程序计数器 */
    case VRELOC:    return "VRELOC";     /* 表达式可以将结果放在任何寄存器中；info = 指令的程序计数器 */
    case VCALL:     return "VCALL";      /* 表达式是一个函数调用；info = 指令的程序计数器 */
    case VVARARG:   return "VVARARG";    /* 可变参数表达式；info = 指令的程序计数器 */
    default:        return "UNKNOWN";    /* 处理未知的枚举值 */
  }
}


static void init_exp (expdesc *e, expkind k, int i) {
  e->f = e->t = NO_JUMP;
  e->k = k;
  e->u.info = i;
  fprintf(llex_file,"init_exp  %s\n", log_expdesc(e));
}


static void codestring (expdesc *e, TString *s) {
  e->f = e->t = NO_JUMP;
  e->k = VKSTR;
  e->u.strval = s;
  fprintf(llex_file,"init_exp  %s\n", log_expdesc(e));
}


static void codename (LexState *ls, expdesc *e) {
  codestring(e, str_checkname(ls));
}


/*
** Register a new local variable in the active 'Proto' (for debug
** information).
** 注册一个局部变量到proto里
*/
static int registerlocalvar (LexState *ls, FuncState *fs, TString *varname) {
  Proto *f = fs->f;
  int oldsize = f->sizelocvars;
  luaM_growvector(ls->L, f->locvars, fs->ndebugvars, f->sizelocvars,
                  LocVar, SHRT_MAX, "local variables");
  while (oldsize < f->sizelocvars)
    f->locvars[oldsize++].varname = NULL;
  f->locvars[fs->ndebugvars].varname = varname;
  f->locvars[fs->ndebugvars].startpc = fs->pc;
  luaC_objbarrier(ls->L, f, varname);
  return fs->ndebugvars++;
}


/*
** Create a new local variable with the given 'name'. Return its index
** in the function.
** 给定name 创建一个新的局部变量Vardesc,返回变量在函数里的index
** 每出现一个local 就会有这个函数调用
*/
static int new_localvar (LexState *ls, TString *name) {
  fprintf(llex_file,"create a new localval: %s in Dyndata\n",name->contents);
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Dyndata *dyd = ls->dyd;
  Vardesc *var;
  checklimit(fs, dyd->actvar.n + 1 - fs->firstlocal,
                 MAXVARS, "local variables");
  luaM_growvector(L, dyd->actvar.arr, dyd->actvar.n + 1,
                  dyd->actvar.size, Vardesc, USHRT_MAX, "local variables");
  var = &dyd->actvar.arr[dyd->actvar.n++];
  //设置vardesc的 kind 和 name
  var->vd.kind = VDKREG;  /* default */
  var->vd.name = name;
  return dyd->actvar.n - 1 - fs->firstlocal;
}

#define new_localvarliteral(ls,v) \
    new_localvar(ls,  \
      luaX_newstring(ls, "" v, (sizeof(v)/sizeof(char)) - 1));



/*
** Return the "variable description" (Vardesc) of a given variable.
** (Unless noted otherwise, all variables are referred to by their
** compiler indices.)
*/
static Vardesc *getlocalvardesc (FuncState *fs, int vidx) {
  return &fs->ls->dyd->actvar.arr[fs->firstlocal + vidx];
}


/*
** Convert 'nvar', a compiler index level, to its corresponding
** register. For that, search for the highest variable below that level
** that is in a register and uses its register index ('ridx') plus one.
** nvar是当前函数里局部变量的个数吧，找到第一个在栈上的局部变量,因为有的变量可能不在栈上的，
** 然后返回其栈寄存器 +1,那返回的是第一个可用的寄存器
*/
static int reglevel (FuncState *fs, int nvar) {
  while (nvar-- > 0) {
    Vardesc *vd = getlocalvardesc(fs, nvar);  /* get previous variable */
    if (vd->vd.kind != RDKCTC)  /* is in a register? */
      return vd->vd.ridx + 1;
  }
  return 0;  /* no variables in registers  这个函数在栈上没有变量*/
}


/*
** Return the number of variables in the register stack for the given
** function.
** 返回给定函数在栈上的变量个数，为什么不直接返回fs->nactvar呢，因为有的变量它有可能不在栈上
*/
int luaY_nvarstack (FuncState *fs) {
  return reglevel(fs, fs->nactvar);
}


/*
** Get the debug-information entry for current variable 'vidx'.
*/
static LocVar *localdebuginfo (FuncState *fs, int vidx) {
  Vardesc *vd = getlocalvardesc(fs,  vidx);
  if (vd->vd.kind == RDKCTC)
    return NULL;  /* no debug info. for constants */
  else {
    int idx = vd->vd.pidx;
    lua_assert(idx < fs->ndebugvars);
    return &fs->f->locvars[idx];
  }
}


/*
** Create an expression representing variable 'vidx'
** local a = {}
** a[1] = 1
*/
static void init_var (FuncState *fs, expdesc *e, int vidx) {
  e->f = e->t = NO_JUMP;
  e->k = VLOCAL;
  e->u.var.vidx = vidx;
  e->u.var.ridx = getlocalvardesc(fs, vidx)->vd.ridx;
  fprintf(llex_file,"init_var  %s\n", log_expdesc(e));
}


/*
** Raises an error if variable described by 'e' is read only
*/
static void check_readonly (LexState *ls, expdesc *e) {
  FuncState *fs = ls->fs;
  TString *varname = NULL;  /* to be set if variable is const */
  switch (e->k) {
    case VCONST: {
      varname = ls->dyd->actvar.arr[e->u.info].vd.name;
      break;
    }
    case VLOCAL: {
      Vardesc *vardesc = getlocalvardesc(fs, e->u.var.vidx);
      if (vardesc->vd.kind != VDKREG)  /* not a regular variable? */
        varname = vardesc->vd.name;
      break;
    }
    case VUPVAL: {
      Upvaldesc *up = &fs->f->upvalues[e->u.info];
      if (up->kind != VDKREG) /*not a regular variable 不是一个正常的变量*/
        varname = up->name;
      break;
    }
    default:
      return;  /* other cases cannot be read-only */
  }
  if (varname) {
    const char *msg = luaO_pushfstring(ls->L,
       "attempt to assign to const variable '%s'", getstr(varname));
    luaK_semerror(ls, msg);  /* error */
  }
}


/*
** Start the scope for the last 'nvars' created variables.
** 为最新创建的 'nvars' 个变量建立作用域
*/
static void adjustlocalvars (LexState *ls, int nvars) {
  FuncState *fs = ls->fs;                    // 获取当前函数的编译状态
  int reglevel = luaY_nvarstack(fs);         // 获取当前栈层级/下一个可用的寄存器索引
  int i;
  for (i = 0; i < nvars; i++) {              // 为每个新变量处理
    int vidx = fs->nactvar++;                // 激活变量计数增加
    Vardesc *var = getlocalvardesc(fs, vidx); // 获取变量描述符
    //设置变量的寄存器索引和proto索引
    //要注意哈 这个ridx是表示变量在当前函数栈帧内，是栈帧上的第几个变量
    //换句话说每进入一个新的函数，这个ridx或者reglevel都是从0开始的
    var->vd.ridx = reglevel++;               // 分配寄存器索引
    var->vd.pidx = registerlocalvar(ls, fs, var->vd.name); // 注册局部变量
    fprintf(llex_file,"adjustlocalvars var->vd.ridx:%d, name:%s \n",  var->vd.ridx,var->vd.name->contents);
  }
}



/*
** Close the scope for all variables up to level 'tolevel'.
** (debug info.)
** 移除
*/
static void removevars (FuncState *fs, int tolevel) {
  fs->ls->dyd->actvar.n -= (fs->nactvar - tolevel);
  while (fs->nactvar > tolevel) {
    LocVar *var = localdebuginfo(fs, --fs->nactvar);
    if (var)  /* does it have debug information? */
      var->endpc = fs->pc;
  }
}


/*
** Search the upvalues of the function 'fs' for one
** with the given 'name'.
** 搜索fs里一个名字为name的上值
*/
static int searchupvalue (FuncState *fs, TString *name) {
  int i;
  Upvaldesc *up = fs->f->upvalues;
  for (i = 0; i < fs->nups; i++) {
    if (eqstr(up[i].name, name)) return i;
  }
  return -1;  /* not found */
}


static Upvaldesc *allocupvalue (FuncState *fs) {
  Proto *f = fs->f;
  int oldsize = f->sizeupvalues;
  checklimit(fs, fs->nups + 1, MAXUPVAL, "upvalues");
  luaM_growvector(fs->ls->L, f->upvalues, fs->nups, f->sizeupvalues,
                  Upvaldesc, MAXUPVAL, "upvalues");
  while (oldsize < f->sizeupvalues)
    f->upvalues[oldsize++].name = NULL;
  return &f->upvalues[fs->nups++];
}


static int newupvalue (FuncState *fs, TString *name, expdesc *v) {
  Upvaldesc *up = allocupvalue(fs);
  FuncState *prev = fs->prev;
  if (v->k == VLOCAL) {
    up->instack = 1;          //上值在外层函数的栈上
    up->idx = v->u.var.ridx;  //栈上第几个变量
    up->kind = getlocalvardesc(prev, v->u.var.vidx)->vd.kind;
    lua_assert(eqstr(name, getlocalvardesc(prev, v->u.var.vidx)->vd.name));
  }
  else {                
    up->instack = 0;                //上值在外层函数的上值列表里
    up->idx = cast_byte(v->u.info); //上值列表第几个
    up->kind = prev->f->upvalues[v->u.info].kind;
    lua_assert(eqstr(name, prev->f->upvalues[v->u.info].name));
  }
  up->name = name;
  luaC_objbarrier(fs->ls->L, fs->f, name);
  return fs->nups - 1;
}


/*
** Look for an active local variable with the name 'n' in the
** function 'fs'. If found, initialize 'var' with it and return
** its expression kind; otherwise return -1.
** 查找当前函数的所有活跃局部变量
*/
static int searchvar (FuncState *fs, TString *n, expdesc *var) {
  int i;
  for (i = cast_int(fs->nactvar) - 1; i >= 0; i--) {
    Vardesc *vd = getlocalvardesc(fs, i);
    if (eqstr(n, vd->vd.name)) {  /* found? */
      if (vd->vd.kind == RDKCTC)  /* compile-time constant? */
        init_exp(var, VCONST, fs->firstlocal + i);
      else  /* real variable */
        init_var(fs, var, i);
      return var->k;
    }
  }
  return -1;  /* not found */
}


/*
** Mark block where variable at given level was defined
** (to emit close instructions later).
** 标记定义了那个变量的block ，level指示这个变量是在函数栈帧里的第几个变量
*/
static void markupval (FuncState *fs, int level) {
  BlockCnt *bl = fs->bl;
  while (bl->nactvar > level) //bl->nactvar 就是fs->nactvar
    bl = bl->previous;
  //一个FuncState里有很多个BlockCnt
  //退出时 bl->nactvar <= level 
  bl->upval = 1;        // 标记该作用域块有变量被捕获为 Upvalue
  fs->needclose = 1;    // 通知编译器后续需要生成 OP_CLOSE 指令
}


/*
** Mark that current block has a to-be-closed variable.
** 这个to-be-closed 是啥玩意儿
**（待关闭变量）是一种特殊的局部变量，当变量超出作用域时，会自动调用其 __close元方法
** 定义的时候如下 local a <close> = xxxx 
** local a <close> = setmetatable({},{__close=function() print("close called") end})
** 这是 Lua 5.4 引入的特性，主要用于资源管理和自动清理
** 是不是加快gc啊 模拟函数栈结束后，栈上的局部变量立即销毁
*/
static void marktobeclosed (FuncState *fs) {
  BlockCnt *bl = fs->bl;
  bl->upval = 1;        // 标记该块使用上值（upvalue）
  bl->insidetbc = 1;    // 标记该块包含 to-be-closed 变量
  fs->needclose = 1;    // 标记函数需要关闭处理
}


/*
** Find a variable with the given name 'n'. If it is an upvalue, add
** this upvalue into all intermediate functions. If it is a global, set
** 'var' as 'void' as a flag.
*/
static void singlevaraux (FuncState *fs, TString *n, expdesc *var, int base) {
  if (fs == NULL)  /* no more levels? */
    init_exp(var, VVOID, 0);  /* default is global */
  else {
    //先找当前函数的局部变量在 Dyndata中找
    int v = searchvar(fs, n, var);  /* look up locals at current level 返回变量类型*/
    if (v >= 0) {  /* found? */
      if (v == VLOCAL && !base) //只有base为0才会这样 也即是not found的时候
        markupval(fs, var->u.var.vidx);  /* local will be used as an upval */
    }
    else {  /* not found as local at current level; try upvalues */
      //找不到就找当前函数的上值 在fs->Upvaldesc里找
      int idx = searchupvalue(fs, n);  /* try existing upvalues */
      if (idx < 0) {  /* not found? */
        //还找不到就去前一个函数那找(也是先找局部，然后找上值)
        singlevaraux(fs->prev, n, var, 0);  /* try upper levels */
        //如果是local或者上值，则初始化为本层函数的上值
        //注意这种处理方式，假如 fs1-->fs2-->fs3--->fs4 
        //假如fs4引用了fs1的一个局部变量,那么这个局部变量会在fs2,fs3,fs4的上值里
        if (var->k == VLOCAL || var->k == VUPVAL)  /* local or upvalue? */
          idx  = newupvalue(fs, n, var);  /* will be a new upvalue */
        else  /* it is a global or a constant */
          return;  /* don't need to do anything at this level */
      }
      init_exp(var, VUPVAL, idx);  /* new or old upvalue */
    }
  }
}


/*
** Find a variable with the given name 'n', handling global variables
** too.
*/
static void singlevar (LexState *ls, expdesc *var) {
  TString *varname = str_checkname(ls);
  FuncState *fs = ls->fs;
  singlevaraux(fs, varname, var, 1);
  if (var->k == VVOID) {  /* global name? */
    expdesc key;
    // 只能把 varname 当作是全局表的一个key了
    singlevaraux(fs, ls->envn, var, 1);  /* get environment variable */
    lua_assert(var->k != VVOID);  /* this one must exist */
    luaK_exp2anyregup(fs, var);  /* but could be a constant */
    codestring(&key, varname);  /* key is variable name */
    luaK_indexed(fs, var, &key);  /* env[varname] */
  }
}


/*
** Adjust the number of results from an expression list 'e' with 'nexps'
** expressions to 'nvars' values.
*/
static void adjust_assign (LexState *ls, int nvars, int nexps, expdesc *e) {
  FuncState *fs = ls->fs;
  int needed = nvars - nexps;  /* 变量数大于表达式数量 */
 
  if (hasmultret(e->k)) {  /* 最后一个表达式有多重返回? 只有最后一个表达式有多个返回值时可以正常工作*/
    int extra = needed + 1;  /* discount last expression itself */
    if (extra < 0)
      extra = 0;
    luaK_setreturns(fs, e, extra);  /* last exp. provides the difference */
  }
  else {
    if (e->k != VVOID)  /* at least one expression? */
      luaK_exp2nextreg(fs, e);  /* close last expression 关闭最后一个表达式*/

    if (needed > 0)  /* missing values? nvars > nexps */
      luaK_nil(fs, fs->freereg, needed);  /* 使用nil填充变量值 */
  }

  if (needed > 0)
    luaK_reserveregs(fs, needed);  /* registers for extra values */
  else  /* adding 'needed' is actually a subtraction */
    fs->freereg += needed;  /* remove extra values */
}


#define enterlevel(ls)	luaE_incCstack(ls->L)


#define leavelevel(ls) ((ls)->L->nCcalls--)


/*
** Generates an error that a goto jumps into the scope of some
** local variable.
*/
static l_noret jumpscopeerror (LexState *ls, Labeldesc *gt) {
  const char *varname = getstr(getlocalvardesc(ls->fs, gt->nactvar)->vd.name);
  const char *msg = "<goto %s> at line %d jumps into the scope of local '%s'";
  msg = luaO_pushfstring(ls->L, msg, getstr(gt->name), gt->line, varname);
  luaK_semerror(ls, msg);  /* raise the error */
}


/*
** Solves the goto at index 'g' to given 'label' and removes it
** from the list of pending gotos.
** If it jumps into the scope of some variable, raises an error.
*/
static void solvegoto (LexState *ls, int g, Labeldesc *label) {
  int i;
  Labellist *gl = &ls->dyd->gt;  /* list of gotos */
  Labeldesc *gt = &gl->arr[g];  /* goto to be resolved */
  lua_assert(eqstr(gt->name, label->name));
  
  //标签定义处 和 goto 处的局部变量数要一样！！！
  //label的活跃局部变量数 和 goto 位置处的局部变量数要一样
  if (l_unlikely(gt->nactvar < label->nactvar))  /* enter some scope? */
    jumpscopeerror(ls, gt);
  
  //gt处的跳转指令跳转到label处
  luaK_patchlist(ls->fs, gt->pc, label->pc);

  //从待处理列表移除goto 标签
  for (i = g; i < gl->n - 1; i++)  /* remove goto from pending list */
    gl->arr[i] = gl->arr[i + 1];
  gl->n--;
}


/*
** Search for an active label with the given name.
*/
static Labeldesc *findlabel (LexState *ls, TString *name) {
  int i;
  Dyndata *dyd = ls->dyd;
  /* check labels in current function for a match */
  for (i = ls->fs->firstlabel; i < dyd->label.n; i++) {
    Labeldesc *lb = &dyd->label.arr[i];
    if (eqstr(lb->name, name))  /* correct label? */
      return lb;
  }
  return NULL;  /* label not found */
}


/*
** Adds a new label/goto in the corresponding list.
*/
static int newlabelentry (LexState *ls, Labellist *l, TString *name,
                          int line, int pc) {
  int n = l->n;
  luaM_growvector(ls->L, l->arr, n, l->size,
                  Labeldesc, SHRT_MAX, "labels/gotos");
  l->arr[n].name = name;
  l->arr[n].line = line;
  l->arr[n].nactvar = ls->fs->nactvar;  //标签定义处的活跃变量数
  l->arr[n].close = 0;
  l->arr[n].pc = pc;
  l->n = n + 1;
  return n;
}


static int newgotoentry (LexState *ls, TString *name, int line, int pc) {
  return newlabelentry(ls, &ls->dyd->gt, name, line, pc);
}


/*
** Solves forward jumps. Check whether new label 'lb' matches any
** pending gotos in current block and solves them. Return true
** if any of the gotos need to close upvalues.
*/
static int solvegotos (LexState *ls, Labeldesc *lb) {
  Labellist *gl = &ls->dyd->gt;
  int i = ls->fs->bl->firstgoto; //这个block里第一个待处理的goto 下表
  int needsclose = 0;
  while (i < gl->n) {
    if (eqstr(gl->arr[i].name, lb->name)) {
      needsclose |= gl->arr[i].close;
      solvegoto(ls, i, lb);  /* will remove 'i' from the list */
    }
    else
      i++;
  }
  return needsclose;
}


/*
** Create a new label with the given 'name' at the given 'line'.
** 'last' tells whether label is the last non-op statement in its
** block. Solves all pending gotos to this new label and adds
** a close instruction if necessary.
** Returns true if it added a close instruction.
** last的意思是表示这个标签是不是当前所在的block的最后一个语句
*/
static int createlabel (LexState *ls, TString *name, int line,
                        int last) {
  FuncState *fs = ls->fs;
  Labellist *ll = &ls->dyd->label;
  //注册新标签
  int l = newlabelentry(ls, ll, name, line, luaK_getlabel(fs));
  if (last) {  /* label is last no-op statement in the block? */
    /* assume that locals are already out of scope */
    ll->arr[l].nactvar = fs->bl->nactvar;//bl->nactvar这是进入当前块之前的活跃变量数量
    // 为什么要这样做？
    // 1.语义规则：不能从块外 goto 到块内的标签。
    // 2.在块内部，局部变量是有效的。但在块结束时，这些局部变量会超出作用域。
    // 3.如果一个标签位于块的最后一行，那么对于块外的代码来说，这个标签看起来就像是已经随着块一起结束了。
    // 块外的 goto 试图跳转到这个标签时，实际上是在尝试跳入一个已经结束的块，这是非法的。
    // 4.通过将标签的 nactvar 设置为进入块之前的值，模拟了这个标签“位于块外”的假象。
    // 这样，当块外的 goto 尝试跳转到它时，solvegotos 中的检查逻辑 (nactvar != fs->nactvar) 会失败，
    // 从而正确地报告“no visible label 'name' for <goto>”错误。
  }
  //解决所有指向这个新标签的待处理 goto，并根据需要生成指令
  if (solvegotos(ls, &ll->arr[l])) {  /* need close? */
    luaK_codeABC(fs, OP_CLOSE, luaY_nvarstack(fs), 0, 0);
    return 1;
  }
  return 0;
}


/*
** Adjust pending gotos to outer level of a block.
*/
static void movegotosout (FuncState *fs, BlockCnt *bl) {
  int i;
  Labellist *gl = &fs->ls->dyd->gt;
  /* correct pending gotos to current block */
  for (i = bl->firstgoto; i < gl->n; i++) {  /* for each pending goto */
    Labeldesc *gt = &gl->arr[i];
    /* leaving a variable scope? */
    if (reglevel(fs, gt->nactvar) > reglevel(fs, bl->nactvar))
      gt->close |= bl->upval;  /* jump may need a close */
    gt->nactvar = bl->nactvar;  /* update goto level */
  }
}

static void enterblock (FuncState *fs, BlockCnt *bl, lu_byte isloop) {
  bl->isloop = isloop;  //block是否处于循环体中
  bl->nactvar = fs->nactvar;
  bl->firstlabel = fs->ls->dyd->label.n;
  bl->firstgoto = fs->ls->dyd->gt.n;
  bl->upval = 0;
  bl->insidetbc = (fs->bl != NULL && fs->bl->insidetbc);
  bl->previous = fs->bl;
  fs->bl = bl;
  lua_assert(fs->freereg == luaY_nvarstack(fs));
  //fprintf(llex_file,"enterblock: bl->fistlabel: %d, bl->firstgoto: %d  bl->isloop %d bl->\n",bl->firstlabel, bl->firstgoto,bl->isloop);
  fprintf(llex_file,"enterblock:%s \n",log_blockcnt(bl));
}


/*
** generates an error for an undefined 'goto'.
*/
static l_noret undefgoto (LexState *ls, Labeldesc *gt) {
  const char *msg;
  if (eqstr(gt->name, luaS_newliteral(ls->L, "break"))) {
    msg = "break outside loop at line %d";
    msg = luaO_pushfstring(ls->L, msg, gt->line);
  }
  else {
    msg = "no visible label '%s' for <goto> at line %d";
    msg = luaO_pushfstring(ls->L, msg, getstr(gt->name), gt->line);
  }
  luaK_semerror(ls, msg);
}

//--离开block
static void leaveblock (FuncState *fs) {
  BlockCnt *bl = fs->bl;
  LexState *ls = fs->ls;
  int hasclose = 0;
  //计算离开这个块后，栈应该恢复到的寄存器层级。bl->nactvar 是进入块时的活跃变量数，
  //reglevel 将其转换为对应的寄存器索引。这将是释放寄存器的基础。
  int stklevel = reglevel(fs, bl->nactvar);  /* level outside the block */
  removevars(fs, bl->nactvar);  /* remove block locals */
  lua_assert(bl->nactvar == fs->nactvar);  /* back to level on entry */
  
  if (bl->isloop)  /* has to fix pending breaks? 检查当前块是否是一个循环体*/
    hasclose = createlabel(ls, luaS_newliteral(ls->L, "break"), 0, 0);

  //!hasclose: 如果上一步处理 break 时没有生成 OP_CLOSE。
  //bl->previous: 当前块是一个嵌套块（不是最外层的函数块）。
  //bl->upval: 这是一个重要标志，在进入块时被设置。它表示这个块内部有局部变量被内层块引用为 upvalue。
  if (!hasclose && bl->previous && bl->upval)  /* still need a 'close'? */
    luaK_codeABC(fs, OP_CLOSE, stklevel, 0, 0);
  // 结论: 如果一个嵌套块内部创建了上值，但在退出时，之前的逻辑（处理break）还没有生成 OP_CLOSE 指令，
  // 那么必须在这里生成一条。OP_CLOSE stklevel 的作用是关闭所有在寄存器层级 stklevel 之上的、已经超出作用域的上值。


  fs->freereg = stklevel;  /* free registers */
  //bl->firstlabel 记录了进入块时标签列表的长度。
  //将列表的 n 设置回这个值，就相当于移除了在本块内声明的所有标签，防止它们被块外的 goto 看到
  ls->dyd->label.n = bl->firstlabel;  /* remove local labels */
  fs->bl = bl->previous;  /* current block now is previous one */


// 这是最后一步，也是确保 goto 语义正确性的关键。
// 情况一：退出的是嵌套块 (if (bl->previous))
// movegotosout(fs, bl): 调用这个函数。
// 它遍历从 bl->firstgoto 开始到列表末尾的所有待处理的 goto。这些 goto 是在当前块内声明的，但其目标标签尚未找到。
// 作用：将这些 goto 的 nactvar（记录它们所在位置的活跃变量数）更新为离开当前块后的值（即 fs->nactvar）。
// 这样，当这些 goto 在外层块被解决时，它们会与正确的作用域信息进行比较。

// 情况二：退出的是最外层块（函数块）(else)
// if (bl->firstgoto < ls->dyd->gt.n): 检查在进入这个最外层块时，是否还有未被解决的 goto 语句（firstgoto 是进入时的列表长度，
// 如果它小于当前列表长度 n，说明有未解决的）。
// undefgoto(ls, ...): 如果还有未解决的 goto，这是一个编译错误。
// undefgoto 会报告错误：“no visible label 'X' for <goto>”，因为goto要跳转的标签在函数范围内根本不存在。
  if (bl->previous)  /* was it a nested block? */
    movegotosout(fs, bl);  /* update pending gotos to enclosing block */
  else {
    if (bl->firstgoto < ls->dyd->gt.n)  /* still pending gotos? */
      undefgoto(ls, &ls->dyd->gt.arr[bl->firstgoto]);  /* error */
  }
}


/*
** adds a new prototype into list of prototypes
*/
static Proto *addprototype (LexState *ls) {
  Proto *clp;
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Proto *f = fs->f;  /* prototype of current function */
  if (fs->np >= f->sizep) {
    int oldsize = f->sizep;
    luaM_growvector(L, f->p, fs->np, f->sizep, Proto *, MAXARG_Bx, "functions");
    while (oldsize < f->sizep)
      f->p[oldsize++] = NULL;
  }
  f->p[fs->np++] = clp = luaF_newproto(L);
  luaC_objbarrier(L, f, clp);
  return clp;
}


/*
** codes instruction to create new closure in parent function.
** The OP_CLOSURE instruction uses the last available register,
** so that, if it invokes the GC, the GC knows which registers
** are in use at that time.

*/
static void codeclosure (LexState *ls, expdesc *v) {
  FuncState *fs = ls->fs->prev;
  init_exp(v, VRELOC, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np - 1));
  luaK_exp2nextreg(fs, v);  /* fix it at the last register */
}


static void open_func (LexState *ls, FuncState *fs, BlockCnt *bl) {
  Proto *f = fs->f;
  fs->prev = ls->fs;  /* linked list of funcstates */
  fs->ls = ls;
  ls->fs = fs;
  fs->pc = 0;
  fs->previousline = f->linedefined;
  fs->iwthabs = 0;
  fs->lasttarget = 0;
  fs->freereg = 0;
  fs->nk = 0;
  fs->nabslineinfo = 0;
  fs->np = 0;
  fs->nups = 0;
  fs->ndebugvars = 0;
  fs->nactvar = 0;    //open_func的时候是0啊
  fs->needclose = 0;
  fs->firstlocal = ls->dyd->actvar.n; //是下标
  fs->firstlabel = ls->dyd->label.n;  //是下标
  fs->bl = NULL;
  f->source = ls->source;
  luaC_objbarrier(ls->L, f, f->source);
  f->maxstacksize = 2;  /* registers 0/1 are always valid */
  enterblock(fs, bl, 0);
}


static void close_func (LexState *ls) {
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  luaK_ret(fs, luaY_nvarstack(fs), 0);  /* final return */
  leaveblock(fs);
  lua_assert(fs->bl == NULL);
  luaK_finish(fs);
  luaM_shrinkvector(L, f->code, f->sizecode, fs->pc, Instruction);
  luaM_shrinkvector(L, f->lineinfo, f->sizelineinfo, fs->pc, ls_byte);
  luaM_shrinkvector(L, f->abslineinfo, f->sizeabslineinfo,
                       fs->nabslineinfo, AbsLineInfo);
  luaM_shrinkvector(L, f->k, f->sizek, fs->nk, TValue);
  luaM_shrinkvector(L, f->p, f->sizep, fs->np, Proto *);
  luaM_shrinkvector(L, f->locvars, f->sizelocvars, fs->ndebugvars, LocVar);
  luaM_shrinkvector(L, f->upvalues, f->sizeupvalues, fs->nups, Upvaldesc);
  ls->fs = fs->prev;
  luaC_checkGC(L);
}



/*============================================================*/
/* GRAMMAR RULES */
/*============================================================*/


/*
** check whether current token is in the follow set of a block.
** 'until' closes syntactical blocks, but do not close scope,
** so it is handled in separate.
*/
static int block_follow (LexState *ls, int withuntil) {
  switch (ls->t.token) {
    case TK_ELSE: case TK_ELSEIF:
    case TK_END: case TK_EOS:
      return 1;
    case TK_UNTIL: return withuntil;
    default: return 0;
  }
}


static void statlist (LexState *ls) {
  /* statlist -> { stat [';'] } */ 
  while (!block_follow(ls, 1)) {
    if (ls->t.token == TK_RETURN) {
      statement(ls);
      return;  /* 'return' must be last statement */
    }
    statement(ls);
  }
}

//字段选择
static void fieldsel (LexState *ls, expdesc *v) {
  /* fieldsel -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  expdesc key;
  luaK_exp2anyregup(fs, v);
  luaX_next(ls);  /* skip the dot or colon */
  codename(ls, &key);
  luaK_indexed(fs, v, &key);
}


static void yindex (LexState *ls, expdesc *v) {
  /* index -> '[' expr ']' */
  luaX_next(ls);  /* skip the '[' */
  expr(ls, v);
  luaK_exp2val(ls->fs, v);
  checknext(ls, ']');
}


/*
** {======================================================================
** Rules for Constructors
** =======================================================================
*/


typedef struct ConsControl {
  expdesc v;  /* last list item read */
  expdesc *t;  /* table descriptor */
  int nh;  /* total number of 'record' elements */
  int na;  /* number of array elements already stored */
  int tostore;  /* number of array elements pending to be stored */
} ConsControl;


static void recfield (LexState *ls, ConsControl *cc) {
  /* recfield -> (NAME | '['exp']') = exp */
  FuncState *fs = ls->fs;
  int reg = ls->fs->freereg;
  expdesc tab, key, val;
  if (ls->t.token == TK_NAME) {
    checklimit(fs, cc->nh, MAX_INT, "items in a constructor");
    codename(ls, &key);
  }
  else  /* ls->t.token == '[' */
    yindex(ls, &key);
  cc->nh++;
  checknext(ls, '=');
  tab = *cc->t;
  luaK_indexed(fs, &tab, &key);
  expr(ls, &val);
  luaK_storevar(fs, &tab, &val);
  fs->freereg = reg;  /* free registers */
}


static void closelistfield (FuncState *fs, ConsControl *cc) {
  if (cc->v.k == VVOID) return;  /* there is no list item */
  luaK_exp2nextreg(fs, &cc->v);
  cc->v.k = VVOID;
  if (cc->tostore == LFIELDS_PER_FLUSH) {
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);  /* flush */
    cc->na += cc->tostore;
    cc->tostore = 0;  /* no more items pending */
  }
}


static void lastlistfield (FuncState *fs, ConsControl *cc) {
  if (cc->tostore == 0) return;
  if (hasmultret(cc->v.k)) {
    luaK_setmultret(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.info, cc->na, LUA_MULTRET);
    cc->na--;  /* do not count last expression (unknown number of elements) */
  }
  else {
    if (cc->v.k != VVOID)
      luaK_exp2nextreg(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);
  }
  cc->na += cc->tostore;
}


static void listfield (LexState *ls, ConsControl *cc) {
  /* listfield -> exp */
  expr(ls, &cc->v);
  cc->tostore++;
}


static void field (LexState *ls, ConsControl *cc) {
  /* field -> listfield | recfield */
  switch(ls->t.token) {
    case TK_NAME: {  /* may be 'listfield' or 'recfield' */
      if (luaX_lookahead(ls) != '=')  /* expression? */
        listfield(ls, cc);
      else
        recfield(ls, cc);
      break;
    }
    case '[': {
      recfield(ls, cc);
      break;
    }
    default: {
      listfield(ls, cc);
      break;
    }
  }
}


static void constructor (LexState *ls, expdesc *t) {
  /* constructor -> '{' [ field { sep field } [sep] ] '}'
     sep -> ',' | ';' */
  fprintf(llex_file,"constructor: expkind:%s\n",expkind_to_string(t->k));
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);
  ConsControl cc;
  luaK_code(fs, 0);  /* space for extra arg. */
  cc.na = cc.nh = cc.tostore = 0;
  cc.t = t;
  init_exp(t, VNONRELOC, fs->freereg);  /* table will be at stack top */
  luaK_reserveregs(fs, 1);
  init_exp(&cc.v, VVOID, 0);  /* no value (yet) */
  checknext(ls, '{');
  do {
    lua_assert(cc.v.k == VVOID || cc.tostore > 0);
    if (ls->t.token == '}') break;
    closelistfield(fs, &cc);
    field(ls, &cc);
  } while (testnext(ls, ',') || testnext(ls, ';'));
  check_match(ls, '}', '{', line);
  lastlistfield(fs, &cc);
  luaK_settablesize(fs, pc, t->u.info, cc.na, cc.nh);
}

/* }====================================================================== */

//设置可变参数生成可变参数指令
static void setvararg (FuncState *fs, int nparams) {
  fs->f->is_vararg = 1;
  luaK_codeABC(fs, OP_VARARGPREP, nparams, 0, 0);
}


//函数定义时，解析函数参数 parameter list
// ... 必须是最后一个参数
static void parlist (LexState *ls) {
  /* parlist -> [ {NAME ','} (NAME | '...') ] */
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int nparams = 0;
  int isvararg = 0;
  if (ls->t.token != ')') {  /* is 'parlist' not empty?  参数表非空*/
    do {
      switch (ls->t.token) {
        case TK_NAME: {
          //在Dyndata中注册Vardesc
          new_localvar(ls, str_checkname(ls));
          nparams++;
          break;
        }
        case TK_DOTS: {
          luaX_next(ls);
          isvararg = 1;
          break;
        }
        default: luaX_syntaxerror(ls, "<name> or '...' expected");
      }
    } while (!isvararg && testnext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  f->numparams = cast_byte(fs->nactvar);
  if (isvararg)//可变参数
    setvararg(fs, f->numparams);  /* declared vararg */
  luaK_reserveregs(fs, fs->nactvar);  /* reserve registers for parameters */
}


static void body (LexState *ls, expdesc *e, int ismethod, int line) {
  /* body ->  '(' parlist ')' block END */
  fprintf(llex_file,"function body, ismethod:%d, line:%d \n",ismethod, line);
  FuncState new_fs;
  BlockCnt bl;
  new_fs.f = addprototype(ls);
  new_fs.f->linedefined = line; //函数定义处
  open_func(ls, &new_fs, &bl);
  checknext(ls, '(');           //检测左括号
  if (ismethod) {             //如果是表的方法 函数参数表的第一个位置添加了参数self 
    new_localvarliteral(ls, "self");  /* create 'self' parameter */
    adjustlocalvars(ls, 1);
  }
  parlist(ls);              //然后再解析其他参数
  checknext(ls, ')');
  statlist(ls);             //解析函数体
  new_fs.f->lastlinedefined = ls->linenumber;
  check_match(ls, TK_END, TK_FUNCTION, line);
  codeclosure(ls, e); //生成一个closure的指令
  close_func(ls);
}


static int explist (LexState *ls, expdesc *v) {
  /* explist -> expr { ',' expr } */
  int n = 1;  /* at least one expression */
  expr(ls, v);
  while (testnext(ls, ',')) {
    luaK_exp2nextreg(ls->fs, v);
    expr(ls, v);
    n++;
  }
  return n;
}


//函数调用时,解析函数参数
static void funcargs (LexState *ls, expdesc *f, int line) {
  FuncState *fs = ls->fs;
  expdesc args;
  int base, nparams;
  switch (ls->t.token) {
    case '(': {  /* funcargs -> '(' [ explist ] ')' */
      luaX_next(ls);
      if (ls->t.token == ')')  /* arg list is empty? */
        args.k = VVOID;
      else {
        explist(ls, &args);
        if (hasmultret(args.k))
          luaK_setmultret(fs, &args);
      }
      check_match(ls, ')', '(', line);
      break;
    }
    case '{': {  /* funcargs -> constructor */
      constructor(ls, &args);
      break;
    }
    case TK_STRING: {  /* funcargs -> STRING */
      codestring(&args, ls->t.seminfo.ts);
      luaX_next(ls);  /* must use 'seminfo' before 'next' */
      break;
    }
    default: {
      luaX_syntaxerror(ls, "function arguments expected");
    }
  }
  lua_assert(f->k == VNONRELOC);
  base = f->u.info;  /* base register for call */
  if (hasmultret(args.k))
    nparams = LUA_MULTRET;  /* open call */
  else {
    if (args.k != VVOID)
      luaK_exp2nextreg(fs, &args);  /* close last argument */
    nparams = fs->freereg - (base+1);
  }
  init_exp(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams+1, 2));
  luaK_fixline(fs, line);
  fs->freereg = base+1;  /* call remove function and arguments and leaves
                            (unless changed) one result */
}




/*
** {======================================================================
** Expression parsing 表达式解析部分 表达式最终会变成一个e
** =======================================================================
*/


static void primaryexp (LexState *ls, expdesc *v) {
  /* primaryexp -> NAME | '(' expr ')' */
  switch (ls->t.token) {
    case '(': {
      int line = ls->linenumber;
      luaX_next(ls);
      expr(ls, v);
      check_match(ls, ')', '(', line);
      luaK_dischargevars(ls->fs, v);
      return;
    }
    case TK_NAME: {
      singlevar(ls, v);
      return;
    }
    default: {
      luaX_syntaxerror(ls, "unexpected symbol");
    }
  }
}

//后缀表达式
static void suffixedexp (LexState *ls, expdesc *v) {
  /* suffixedexp ->
       primaryexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs } */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  primaryexp(ls, v); //基础表达式
  for (;;) { 
    switch (ls->t.token) {
      case '.': {  /* fieldsel */
        fieldsel(ls, v); // a.b
        break;
      }
      case '[': {  /* '[' exp ']' */
        expdesc key;
        luaK_exp2anyregup(fs, v);
        yindex(ls, &key);
        luaK_indexed(fs, v,  &key);
        break;
      }
      case ':': {  /* ':' NAME funcargs */
        expdesc key;
        luaX_next(ls);
        codename(ls, &key);
        luaK_self(fs, v, &key);
        funcargs(ls, v, line);
        break;
      }
      case '(': case TK_STRING: case '{': {  /* funcargs */
        luaK_exp2nextreg(fs, v);
        funcargs(ls, v, line);
        break;
      }
      default: return;
    }
  }
}


static void simpleexp (LexState *ls, expdesc *v) {
  /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... |
                  constructor | FUNCTION body | suffixedexp */
  switch (ls->t.token) {
    case TK_FLT: {
      init_exp(v, VKFLT, 0);
      v->u.nval = ls->t.seminfo.r;
      break;
    }
    case TK_INT: {
      init_exp(v, VKINT, 0);
      v->u.ival = ls->t.seminfo.i;
      break;
    }
    case TK_STRING: {
      codestring(v, ls->t.seminfo.ts);
      break;
    }
    case TK_NIL: {
      init_exp(v, VNIL, 0);
      break;
    }
    case TK_TRUE: {
      init_exp(v, VTRUE, 0);
      break;
    }
    case TK_FALSE: {
      init_exp(v, VFALSE, 0);
      break;
    }
    case TK_DOTS: {  /* vararg */
      FuncState *fs = ls->fs;
      check_condition(ls, fs->f->is_vararg,
                      "cannot use '...' outside a vararg function");
      init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 0, 1));
      break;
    }
    case '{': {  /* constructor */// 表构造函数 `{1, 2}`
      constructor(ls, v);
      return;
    }
    case TK_FUNCTION: {// 函数定义 `function() end`
      luaX_next(ls);
      body(ls, v, 0, ls->linenumber);// 解析函数体并生成闭包代码
      return;
    }
    default: {
      suffixedexp(ls, v);// 默认处理为后缀表达式
      return;
    }
  }
  luaX_next(ls);
}


static UnOpr getunopr (int op) {
  switch (op) {
    case TK_NOT: return OPR_NOT;
    case '-': return OPR_MINUS;
    case '~': return OPR_BNOT;
    case '#': return OPR_LEN;
    default: return OPR_NOUNOPR;
  }
}


static BinOpr getbinopr (int op) {
  switch (op) {
    case '+': return OPR_ADD;
    case '-': return OPR_SUB;
    case '*': return OPR_MUL;
    case '%': return OPR_MOD;
    case '^': return OPR_POW;
    case '/': return OPR_DIV;
    case TK_IDIV: return OPR_IDIV;
    case '&': return OPR_BAND;
    case '|': return OPR_BOR;
    case '~': return OPR_BXOR;
    case TK_SHL: return OPR_SHL;
    case TK_SHR: return OPR_SHR;
    case TK_CONCAT: return OPR_CONCAT;
    case TK_NE: return OPR_NE;
    case TK_EQ: return OPR_EQ;
    case '<': return OPR_LT;
    case TK_LE: return OPR_LE;
    case '>': return OPR_GT;
    case TK_GE: return OPR_GE;
    case TK_AND: return OPR_AND;
    case TK_OR: return OPR_OR;
    default: return OPR_NOBINOPR;
  }
}


/*
** Priority table for binary operators.
*/
static const struct {
  lu_byte left;  /* left priority for each binary operator */
  lu_byte right; /* right priority */
} priority[] = {  /* ORDER OPR */
   {10, 10}, {10, 10},           /* '+' '-' */
   {11, 11}, {11, 11},           /* '*' '%' */
   {14, 13},                  /* '^' (right associative) */
   {11, 11}, {11, 11},           /* '/' '//' */
   {6, 6}, {4, 4}, {5, 5},   /* '&' '|' '~' */
   {7, 7}, {7, 7},           /* '<<' '>>' */
   {9, 8},                   /* '..' (right associative) */
   {3, 3}, {3, 3}, {3, 3},   /* ==, <, <= */
   {3, 3}, {3, 3}, {3, 3},   /* ~=, >, >= */
   {2, 2}, {1, 1}            /* and, or */
};

#define UNARY_PRIORITY	12  /* priority for unary operators */


/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where 'binop' is any binary operator with a priority higher than 'limit'
*/
static BinOpr subexpr (LexState *ls, expdesc *v, int limit) {
  BinOpr op;
  UnOpr uop;
  enterlevel(ls);
  uop = getunopr(ls->t.token);
  if (uop != OPR_NOUNOPR) {  /* prefix (unary) operator? */
    int line = ls->linenumber;
    luaX_next(ls);  /* skip operator */
    subexpr(ls, v, UNARY_PRIORITY);// 递归解析右侧，优先级设为最高
    luaK_prefix(ls->fs, uop, v, line);// 生成一元操作字节码（如 NEG/BNOT）
  }
  else simpleexp(ls, v);// 处理简单表达式
  /* expand while operators have priorities higher than 'limit' */
  op = getbinopr(ls->t.token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    expdesc v2;
    BinOpr nextop;
    int line = ls->linenumber;
    luaX_next(ls);  /* skip operator */
    luaK_infix(ls->fs, op, v);// 暂存左操作数，准备中缀运算
    /* read sub-expression with higher priority */
    nextop = subexpr(ls, &v2, priority[op].right);// 解析右表达式
    luaK_posfix(ls->fs, op, v, &v2, line);// 同时处理左右表达式的值，生成二元操作字节码
    op = nextop;// 继续处理后续运算符
  }
  leavelevel(ls);
  return op;  /* return first untreated operator */
}


static void expr (LexState *ls, expdesc *v) {
  subexpr(ls, v, 0);
}

/* }==================================================================== */



/*
** {======================================================================
** Rules for Statements
** =======================================================================
*/


static void block (LexState *ls) {
  /* block -> statlist */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  enterblock(fs, &bl, 0);
  statlist(ls);
  leaveblock(fs);
}


/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
struct LHS_assign {
  struct LHS_assign *prev;
  expdesc v;  /* variable (global, local, upvalue, or indexed) */
};


/*
** check whether, in an assignment to an upvalue/local variable, the
** upvalue/local variable is begin used in a previous assignment to a
** table. If so, save original upvalue/local value in a safe place and
** use this safe copy in the previous assignment.
*/
static void check_conflict (LexState *ls, struct LHS_assign *lh, expdesc *v) {
  FuncState *fs = ls->fs;
  int extra = fs->freereg;  /* eventual position to save local variable */
  int conflict = 0;
  for (; lh; lh = lh->prev) {  /* check all previous assignments */
    if (vkisindexed(lh->v.k)) {  /* assignment to table field? */
      if (lh->v.k == VINDEXUP) {  /* is table an upvalue? */
        if (v->k == VUPVAL && lh->v.u.ind.t == v->u.info) {
          conflict = 1;  /* table is the upvalue being assigned now */
          lh->v.k = VINDEXSTR;
          lh->v.u.ind.t = extra;  /* assignment will use safe copy */
        }
      }
      else {  /* table is a register */
        if (v->k == VLOCAL && lh->v.u.ind.t == v->u.var.ridx) {
          conflict = 1;  /* table is the local being assigned now */
          lh->v.u.ind.t = extra;  /* assignment will use safe copy */
        }
        /* is index the local being assigned? */
        if (lh->v.k == VINDEXED && v->k == VLOCAL &&
            lh->v.u.ind.idx == v->u.var.ridx) {
          conflict = 1;
          lh->v.u.ind.idx = extra;  /* previous assignment will use safe copy */
        }
      }
    }
  }
  if (conflict) {
    /* copy upvalue/local value to a temporary (in position 'extra') */
    if (v->k == VLOCAL)
      luaK_codeABC(fs, OP_MOVE, extra, v->u.var.ridx, 0);
    else
      luaK_codeABC(fs, OP_GETUPVAL, extra, v->u.info, 0);
    luaK_reserveregs(fs, 1);
  }
}

/*
** Parse and compile a multiple assignment. The first "variable"
** (a 'suffixedexp') was already read by the caller.
**
** assignment -> suffixedexp restassign
** restassign -> ',' suffixedexp restassign | '=' explist

** 这里是个递归
** a,b,c = 1,2,3
*/
static void restassign (LexState *ls, struct LHS_assign *lh, int nvars) {
  expdesc e;
  check_condition(ls, vkisvar(lh->v.k), "syntax error");
  check_readonly(ls, &lh->v);
  //第一个已经被读取了,如果下一个是
  if (testnext(ls, ',')) {  /* restassign -> ',' suffixedexp restassign */
    struct LHS_assign nv;
    nv.prev = lh;
    suffixedexp(ls, &nv.v);
    if (!vkisindexed(nv.v.k))
      check_conflict(ls, lh, &nv.v);
    enterlevel(ls);  /* control recursion depth */
    restassign(ls, &nv, nvars+1);
    leavelevel(ls);
  }
  else {  /* restassign -> '=' explist */
    int nexps;
    checknext(ls, '=');
    nexps = explist(ls, &e);
    if (nexps != nvars) //变量的数量不等于表达式的数量
      adjust_assign(ls, nvars, nexps, &e);
    else {
      //变量数和表达式数量相同
      luaK_setoneret(ls->fs, &e);  /* close last expression */
      luaK_storevar(ls->fs, &lh->v, &e);
      return;  /* avoid default */
    }
  }
  init_exp(&e, VNONRELOC, ls->fs->freereg-1);  /* default assignment */
  luaK_storevar(ls->fs, &lh->v, &e);
}


static int cond (LexState *ls) {
  /* cond -> exp */
  expdesc v;
  expr(ls, &v);  /* read condition */
  if (v.k == VNIL) v.k = VFALSE;  /* 'falses' are all equal here */
  luaK_goiftrue(ls->fs, &v);
  return v.f;
}


static void gotostat (LexState *ls) {
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  TString *name = str_checkname(ls);  /* label's name */
  Labeldesc *lb = findlabel(ls, name);
  if (lb == NULL)  /* no label? */
    /* forward jump; will be resolved when the label is declared */
    newgotoentry(ls, name, line, luaK_jump(fs));
  else {  /* found a label */
    /* backward jump; will be resolved here */
    int lblevel = reglevel(fs, lb->nactvar);  /* label level */
    if (luaY_nvarstack(fs) > lblevel)  /* leaving the scope of a variable? */
      luaK_codeABC(fs, OP_CLOSE, lblevel, 0, 0);
    /* create jump and link it to the label */
    luaK_patchlist(fs, luaK_jump(fs), lb->pc);
  }
}


/*
** Break statement. Semantically equivalent to "goto break".
*/
static void breakstat (LexState *ls) {
  int line = ls->linenumber;
  luaX_next(ls);  /* skip break */
  newgotoentry(ls, luaS_newliteral(ls->L, "break"), line, luaK_jump(ls->fs));
}


/*
** Check whether there is already a label with the given 'name'.
*/
static void checkrepeated (LexState *ls, TString *name) {
  Labeldesc *lb = findlabel(ls, name);
  if (l_unlikely(lb != NULL)) {  /* already defined? */
    const char *msg = "label '%s' already defined on line %d";
    msg = luaO_pushfstring(ls->L, msg, getstr(name), lb->line);
    luaK_semerror(ls, msg);  /* error */
  }
}


static void labelstat (LexState *ls, TString *name, int line) {
  /* label -> '::' NAME '::' */
  checknext(ls, TK_DBCOLON);  /* skip double colon 跳过第2个:: */
  // Lua 语法允许在标签后出现多个连续的“无操作”语句（空语句或另一个标签）。
  // 这个循环会跳过所有这些无意义的语句，直到遇到一个有实际内容的 token。这确保了解析器处于正确的位置来处理接下来的有效代码
  while (ls->t.token == ';' || ls->t.token == TK_DBCOLON)
    statement(ls);  /* skip other no-op statements 这允许在标签后出现另一个标签（例如 ::a::::b::），这在语法上是允许但无实际作用的。*/
  //检测之前是否存在该同名标签
  checkrepeated(ls, name);  /* check for repeated labels */
  //创建标签
  createlabel(ls, name, line, block_follow(ls, 0));
}

//解析whilestat
static void whilestat (LexState *ls, int line) {
  /* whilestat -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  BlockCnt bl;
  luaX_next(ls);  /* skip WHILE */
  whileinit = luaK_getlabel(fs); //这个返回的就是fs->pc
  condexit = cond(ls); //开始解析 cond  返回的是v.f 
  enterblock(fs, &bl, 1);
  checknext(ls, TK_DO);
  block(ls);                  //如果这个block里有 if xxx then break end
  //！！！while语句的关键,block的最后生成了一个跳转指令跳转回到cond指令 这样就实现了while循环
  luaK_jumpto(fs, whileinit); 
  check_match(ls, TK_END, TK_WHILE, line);
  leaveblock(fs);
  luaK_patchtohere(fs, condexit);  /* false conditions finish the loop */
}

//注意这个lua的repeat until 和 c语言的 do{} while(cond) 是不一样的
//解析repeat语句 repeat block until cond==true 
//为什么这个repeat until 需要2个block呢,因为它会先执行一遍循环体的代码 再判断 cond.
static void repeatstat (LexState *ls, int line) {
  /* repeatstat -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = luaK_getlabel(fs);
  BlockCnt bl1, bl2;
  enterblock(fs, &bl1, 1);  /* loop block ：管理循环的跳转标签和 break 语句的目标*/
  enterblock(fs, &bl2, 0);  /* scope block 管理循环体内声明的局部变量和上值（upvalues）*/
  // 为什么要分开？
  // 因为循环控制和变量作用域是两个不同的概念：
  // 循环需要自己的控制结构（标签、跳转）
  // 循环体需要自己的变量作用域（局部变量、闭包）
  //注意现在的是 b11-->bl2
  luaX_next(ls);  /* skip REPEAT */
  statlist(ls);
  check_match(ls, TK_UNTIL, TK_REPEAT, line);
  //编译条件表达式，返回 jump_pc跳转
  condexit = cond(ls);  /* read condition (inside scope block) */
  leaveblock(fs);  /* finish scope */
  //先离开 scope block
  if (bl2.upval) {  /* upvalues? 怎么样子的lua代码才可以让这个bl2有upval??*/
    // repeat
    //     local a = 1
    //     local function test2()
    //         a = a + 1
    //     end
    // until true
    int exit = luaK_jump(fs);  /* normal exit must jump over fix */
    luaK_patchtohere(fs, condexit);  /* repetition must close upvalues */
    luaK_codeABC(fs, OP_CLOSE, reglevel(fs, bl2.nactvar), 0, 0);
    condexit = luaK_jump(fs);  /* repeat after closing upvalues */
    luaK_patchtohere(fs, exit);  /* normal exit comes to here */
  }
  //所以 a 为 false的时候
  luaK_patchlist(fs, condexit, repeat_init);  /* close the loop */
  leaveblock(fs);  /* finish loop */
  //离开 loop block
}


/*
** Read an expression and generate code to put its results in next
** stack slot.
**
*/
static void exp1 (LexState *ls) {
  expdesc e;
  expr(ls, &e);
  luaK_exp2nextreg(ls->fs, &e);
  lua_assert(e.k == VNONRELOC);
}


/*
** Fix for instruction at position 'pc' to jump to 'dest'.
** (Jump addresses are relative in Lua). 'back' true means
** a back jump.
*/
static void fixforjump (FuncState *fs, int pc, int dest, int back) {
  Instruction *jmp = &fs->f->code[pc];
  int offset = dest - (pc + 1);
  if (back)
    offset = -offset;
  if (l_unlikely(offset > MAXARG_Bx))
    luaX_syntaxerror(fs->ls, "control structure too long");
  SETARG_Bx(*jmp, offset);
}


/*
** Generate code for a 'for' loop.
*/
static void forbody (LexState *ls, int base, int line, int nvars, int isgen) {
  /* forbody -> DO block */
  static const OpCode forprep[2] = {OP_FORPREP, OP_TFORPREP};
  static const OpCode forloop[2] = {OP_FORLOOP, OP_TFORLOOP};
  BlockCnt bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  checknext(ls, TK_DO);
  prep = luaK_codeABx(fs, forprep[isgen], base, 0);
  enterblock(fs, &bl, 0);  /* scope for declared variables */
  adjustlocalvars(ls, nvars);
  luaK_reserveregs(fs, nvars);
  block(ls);
  leaveblock(fs);  /* end of scope for declared variables */
  fixforjump(fs, prep, luaK_getlabel(fs), 0);
  if (isgen) {  /* generic for? */
    luaK_codeABC(fs, OP_TFORCALL, base, 0, nvars);
    luaK_fixline(fs, line);
  }
  endfor = luaK_codeABx(fs, forloop[isgen], base, 0);
  fixforjump(fs, endfor, prep + 1, 1);
  luaK_fixline(fs, line);
}

//for i = 1,1 do xxx end
static void fornum (LexState *ls, TString *varname, int line) {
  /* fornum -> NAME = exp,exp[,exp] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for state)");
  new_localvar(ls, varname);
  checknext(ls, '=');
  exp1(ls);  /* initial value */
  checknext(ls, ',');
  exp1(ls);  /* limit */
  if (testnext(ls, ','))
    exp1(ls);  /* optional step */
  else {  /* default step = 1 */
    luaK_int(fs, fs->freereg, 1);
    luaK_reserveregs(fs, 1);
  }
  adjustlocalvars(ls, 3);  /* control variables */
  forbody(ls, base, line, 1, 0);
}


/*
local function myiter(tbl,index)
    index = (index or 0) + 1
    local val = tbl[index]
    if val then 
        return index,val
    end
end
-- 示例 2: 自定义迭代器（返回三个值）
local tbl = {100, 200, 300}
for idx, val in myiter,tbl do
    print(idx, val)  -- 输出: 1 100 extra, 2 200 extra, 3 300 extra
end
这个explist 其实是 func1,arg1,arg2 这样的,这个func1返回nil的时候，就停止循环了

//explist ==> expr { ',' expr }
//但是for循环主要使用来迭代的
//forlist -> NAME {,NAME} IN explist forbody

//其实它类比下面这么一个while循环
while true do
  expr { ',' expr }
  local name {,name} = expr1(expr2,...,exprn)
  if not idx then
    break
  end
end
*/
//for var1,var2 in pairs()
static void forlist (LexState *ls, TString *indexname) {
  /* forlist -> NAME {,NAME} IN explist forbody */
  FuncState *fs = ls->fs;
  expdesc e;
  int nvars = 5;  /* gen, state, control, toclose, 'indexname' */
  int line;
  int base = fs->freereg;
  /* create control variables */
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for state)");
  /* create declared variables */
  new_localvar(ls, indexname);
  while (testnext(ls, ',')) {
    new_localvar(ls, str_checkname(ls));
    nvars++;
  }
  checknext(ls, TK_IN);
  line = ls->linenumber;
  adjust_assign(ls, 4, explist(ls, &e), &e);
  adjustlocalvars(ls, 4);  /* control variables */
  marktobeclosed(fs);  /* last control var. must be closed */
  luaK_checkstack(fs, 3);  /* extra space to call generator */
  forbody(ls, base, line, nvars - 4, 1);
}

//for语句还蛮复杂的
static void forstat (LexState *ls, int line) {
  /* forstat -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  TString *varname;
  BlockCnt bl;
  enterblock(fs, &bl, 1);  /* scope for loop and control variables */
  luaX_next(ls);  /* skip 'for' */
  varname = str_checkname(ls);  /* first variable name */
  switch (ls->t.token) {
    //for语句的2种迭代方式
    case '=': fornum(ls, varname, line); break; //for i = 1,1 do xxx end
    case ',': case TK_IN: forlist(ls, varname); break; //for var1,var2 in pairs()
    default: luaX_syntaxerror(ls, "'=' or 'in' expected");
  }
  check_match(ls, TK_END, TK_FOR, line);
  leaveblock(fs);  /* loop scope ('break' jumps to this point) */
}

//每一个test jmp jmp 指令
static void test_then_block (LexState *ls, int *escapelist) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  expdesc v;
  int jf;  /* instruction to skip 'then' code (if condition is false) 也就是说跳到下一个block的第一条指令*/
  luaX_next(ls);          /*跳过if或者ifelse*/
  expr(ls, &v);           /* 解析 cond表达式  a < b =>op_test op_jmp*/
  checknext(ls, TK_THEN); /*跳过then*/
  if (ls->t.token == TK_BREAK) {  /* 'if x then break' ? */
    int line = ls->linenumber;
    luaK_goiffalse(ls->fs, &v);  /* will jump if condition is true */
    luaX_next(ls);  /* skip 'break' */
    enterblock(fs, &bl, 0);  /* must enter block before 'goto' */
    newgotoentry(ls, luaS_newliteral(ls->L, "break"), line, v.t);
    while (testnext(ls, ';')) {}  /* skip semicolons */
    if (block_follow(ls, 0)) {  /* jump is the entire block? */
      leaveblock(fs);
      return;  /* and that is it */
    }
    else  /* must skip over 'then' part if condition is false */
      jf = luaK_jump(fs);
  }
  else {  
    
    /* regular case (not a break) */
    luaK_goiftrue(ls->fs, &v);    /* 如果cond是错误直接跳过 then后的block */
    enterblock(fs, &bl, 0);
    jf = v.f; //这个v.f就是 jmp指令的索引                    //记录jump_false列表

  }

  statlist(ls);                   /*解析then之后的block*/
  leaveblock(fs);

  //如果在then block之后 又遇见了 else|elseif,则 新要生成一个跳转指令且插入到escapelist
  if (ls->t.token == TK_ELSE ||
      ls->t.token == TK_ELSEIF)  /* followed by 'else'/'elseif'? */
    {
      // 这个就是 LT JMP JMP的第3个 JMP 第3个JMP最后都要指向外层
      int jmp_pc = luaK_jump(fs);           // 生成无条件跳转 返回的是这条跳转指令的地址（跳过后续 elseif/else）
      luaK_concat(fs, escapelist, jmp_pc); // 将该跳转加入 escapelist
    }

  //就是cond为false时就要跳转到下一个 elseif cond |else block 的第一条指令 
  //将jf的指令的偏移都修正到此刻的pc
  luaK_patchtohere(fs, jf); // 这里是if cond的cond为false的时候跳转到下一个elseif cond
  // jf 就是  LT JMP JMP 第2个jmp的索引
}


static void ifstat (LexState *ls, int line) {
  /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
  FuncState *fs = ls->fs;
  int escapelist = NO_JUMP;  /* exit list for finished parts */
  test_then_block(ls, &escapelist);  /* IF cond THEN block */
  while (ls->t.token == TK_ELSEIF)
    test_then_block(ls, &escapelist);  /* ELSEIF cond THEN block */
  if (testnext(ls, TK_ELSE))
    block(ls);  /* 'else' part */
  check_match(ls, TK_END, TK_IF, line);
  //将escapelist索引为首的跳转指令的跳转地址改为此刻的pc值 
  luaK_patchtohere(fs, escapelist);  /* patch escape list to 'if' end */
}

//一个local function的语句
static void localfunc (LexState *ls) {
  expdesc b;
  FuncState *fs = ls->fs;
  int fvar = fs->nactvar;  /* function's variable index */
  new_localvar(ls, str_checkname(ls));  /* new local variable 这个就是这个函数*/
  adjustlocalvars(ls, 1);  /* enter its scope */
  body(ls, &b, 0, ls->linenumber);  /* function created in next register */
  /* debug information will only see the variable after this point! */
  localdebuginfo(fs, fvar)->startpc = fs->pc;
}


static int getlocalattribute (LexState *ls) {
  /* ATTRIB -> ['<' Name '>'] */
  if (testnext(ls, '<')) {
    const char *attr = getstr(str_checkname(ls));
    checknext(ls, '>');
    if (strcmp(attr, "const") == 0)
      return RDKCONST;  /* read-only variable */
    else if (strcmp(attr, "close") == 0)
      return RDKTOCLOSE;  /* to-be-closed variable */
    else
      luaK_semerror(ls,
        luaO_pushfstring(ls->L, "unknown attribute '%s'", attr));
  }
  return VDKREG;  /* regular variable */
}


static void checktoclose (FuncState *fs, int level) {
  if (level != -1) {  /* is there a to-be-closed variable? */
    marktobeclosed(fs);
    luaK_codeABC(fs, OP_TBC, reglevel(fs, level), 0, 0);
  }
}

static void localstat (LexState *ls) {
 // ShowParselog("enter localstat");
  /* stat -> LOCAL NAME ATTRIB { ',' NAME ATTRIB } ['=' explist] */
  FuncState *fs = ls->fs;
  int toclose = -1;  /* 待关闭变量的索引（如果有的话） */
  Vardesc *var;  /* 最后一个变量 */
  int vidx, kind;  /* 最后一个变量的索引和类型 */
  int nvars = 0;  /* 变量数量 */
  int nexps;  /* 表达式数量 */
  expdesc e;  /* 表达式描述符 */
  do {
    vidx = new_localvar(ls, str_checkname(ls));  /* 创建新的局部变量 */
    kind = getlocalattribute(ls);  /* 获取局部变量的属性 */
    getlocalvardesc(fs, vidx)->vd.kind = kind;  /* 设置变量描述符的类型 */
    if (kind == RDKTOCLOSE) {  /* 如果是待关闭变量 */
      if (toclose != -1)  /* 如果已经有一个待关闭变量 */
        luaK_semerror(ls, "multiple to-be-closed variables in local list");  /* 报错 */
      toclose = fs->nactvar + nvars;  /* 设置待关闭变量的索引 */
    }
    nvars++;  /* 增加变量数量 */
  } while (testnext(ls, ','));  /* 如果有逗号，继续处理下一个变量 */
  if (testnext(ls, '='))  /* 如果有等号，处理表达式列表 */
    nexps = explist(ls, &e);
  else {
    e.k = VVOID;  /* 否则，设置表达式类型为 VVOID */
    nexps = 0;  /* 表达式数量为 0 */
  }
  var = getlocalvardesc(fs, vidx);  /* 获取最后一个变量 */
  if (nvars == nexps &&  /* 如果变量数量和表达式数量相等 */
      var->vd.kind == RDKCONST &&  /* 最后一个变量是常量 */
      luaK_exp2const(fs, &e, &var->k)) {  /* 并且是编译时常量 */
    var->vd.kind = RDKCTC;  /* 设置变量为编译时常量 */
    adjustlocalvars(ls, nvars - 1);  /* 调整局部变量，排除最后一个变量 */
    fs->nactvar++;  /* 增加活动变量数量 */
  }
  else {
    adjust_assign(ls, nvars, nexps, &e);  /* 调整赋值 */
    adjustlocalvars(ls, nvars);  /* 将变量和寄存器索引联系起来 */
  }
  checktoclose(fs, toclose);  /* 检查待关闭变量 */
}


static int funcname (LexState *ls, expdesc *v) {
  /* funcname -> NAME {fieldsel} [':' NAME] */
  int ismethod = 0;
  singlevar(ls, v);
  while (ls->t.token == '.')
    fieldsel(ls, v);
  if (ls->t.token == ':') {
    ismethod = 1;
    fieldsel(ls, v);
  }
  return ismethod;
}

//要么是表的方法，要么是全局函数
static void funcstat (LexState *ls, int line) {
  /* funcstat -> FUNCTION funcname body */
  int ismethod;
  expdesc v, b;
  luaX_next(ls);  /* skip FUNCTION */
  ismethod = funcname(ls, &v);
  body(ls, &b, ismethod, line);
  check_readonly(ls, &v);
  luaK_storevar(ls->fs, &v, &b);
  luaK_fixline(ls->fs, line);  /* definition "happens" in the first line */
}

// a,a[b],a.c = 1,2,3
static void exprstat (LexState *ls) {
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  suffixedexp(ls, &v.v);
  if (ls->t.token == '=' || ls->t.token == ',') { /* stat -> assignment ? */
    v.prev = NULL;
    restassign(ls, &v, 1);
  }
  else {  /* stat -> func */
    Instruction *inst;
    check_condition(ls, v.v.k == VCALL, "syntax error");
    inst = &getinstruction(fs, &v.v);
    SETARG_C(*inst, 1);  /* call statement uses no results */
  }
}

//return 语句
static void retstat (LexState *ls) {
  /* stat -> RETURN [explist] [';'] */
  FuncState *fs = ls->fs;
  expdesc e;
  int nret;  /* number of values being returned */
  int first = luaY_nvarstack(fs);  /* first slot to be returned */
  if (block_follow(ls, 1) || ls->t.token == ';')
    nret = 0;  /* return no values */
  else {
    nret = explist(ls, &e);  /* optional return values */
    if (hasmultret(e.k)) {
      luaK_setmultret(fs, &e);
      if (e.k == VCALL && nret == 1 && !fs->bl->insidetbc) {  /* tail call? */
        SET_OPCODE(getinstruction(fs,&e), OP_TAILCALL);
        lua_assert(GETARG_A(getinstruction(fs,&e)) == luaY_nvarstack(fs));
      }
      nret = LUA_MULTRET;  /* return all values */
    }
    else {
      if (nret == 1)  /* only one single value? */
        first = luaK_exp2anyreg(fs, &e);  /* can use original slot */
      else {  /* values must go to the top of the stack */
        luaK_exp2nextreg(fs, &e);
        lua_assert(nret == fs->freereg - first);
      }
    }
  }
  luaK_ret(fs, first, nret);
  testnext(ls, ';');  /* skip optional semicolon */
}


static void statement (LexState *ls) {
  int line = ls->linenumber;  /* may be needed for error messages */
  enterlevel(ls);
  switch (ls->t.token) {
    case ';': {  /* stat -> ';' (empty statement) */
      luaX_next(ls);  /* skip ';' */
      break;
    }
    case TK_IF: {  /* stat -> ifstat 解析if语句*/
      ifstat(ls, line);
      break;
    }
    case TK_WHILE: {  /* stat -> whilestat 解析while stat*/
      whilestat(ls, line);
      break;
    }
    case TK_DO: {  /* stat -> DO block END */
      luaX_next(ls);  /* skip DO */
      block(ls);
      check_match(ls, TK_END, TK_DO, line);
      break;
    }
    case TK_FOR: {  /* stat -> forstat */
      forstat(ls, line);
      break;
    }
    case TK_REPEAT: {  /* stat -> repeatstat */
      repeatstat(ls, line);
      break;
    }
    case TK_FUNCTION: {  /* stat -> funcstat */
      funcstat(ls, line);
      break;
    }
    case TK_LOCAL: {  /* stat -> localstat */
      luaX_next(ls);  /* skip LOCAL */
      if (testnext(ls, TK_FUNCTION))  /* local function? */
        localfunc(ls);
      else
        localstat(ls);
      break;
    }
    case TK_DBCOLON: {  /* stat -> label */
      luaX_next(ls);  /* skip double colon */
      labelstat(ls, str_checkname(ls), line);
      break;
    }
    case TK_RETURN: {  /* stat -> retstat */
      luaX_next(ls);  /* skip RETURN */
      retstat(ls);
      break;
    }
    case TK_BREAK: {  /* stat -> breakstat */
      breakstat(ls);
      break;
    }
    case TK_GOTO: {  /* stat -> 'goto' NAME */
      luaX_next(ls);  /* skip 'goto' */
      gotostat(ls);
      break;
    }
    default: {  /* stat -> func | assignment */
      exprstat(ls);
      break;
    }
  }
  lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
             ls->fs->freereg >= luaY_nvarstack(ls->fs));
  ls->fs->freereg = luaY_nvarstack(ls->fs);  /* free registers */
  leavelevel(ls);
}

/* }====================================================================== */


/*
** compiles the main function, which is a regular vararg function with an
** upvalue named LUA_ENV
*/
static void mainfunc (LexState *ls, FuncState *fs) {
  BlockCnt bl;
  Upvaldesc *env;
  open_func(ls, fs, &bl);
  setvararg(fs, 0);  /* main function is always declared vararg */
  env = allocupvalue(fs);  /* ...set environment upvalue */
  env->instack = 1;
  env->idx = 0;
  env->kind = VDKREG;
  env->name = ls->envn;
  luaC_objbarrier(ls->L, fs->f, env->name);
  luaX_next(ls);  /* read first token */
  statlist(ls);  /* parse main body */
  check(ls, TK_EOS);
  close_func(ls);
}


LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
                       Dyndata *dyd, const char *name, int firstchar) {
  ShowParselog("start parser------------------------>");
  LexState lexstate;
  FuncState funcstate;
  LClosure *cl = luaF_newLclosure(L, 1);  /* create main closure */
  setclLvalue2s(L, L->top.p, cl);  /* 栈上引用这个main cl 防止被垃圾回收 */
  luaD_inctop(L);
  lexstate.h = luaH_new(L);  /* create table for scanner */
  sethvalue2s(L, L->top.p, lexstate.h);  /* anchor it */
  luaD_inctop(L);
  funcstate.f = cl->p = luaF_newproto(L);
  luaC_objbarrier(L, cl, cl->p);
  funcstate.f->source = luaS_new(L, name);  /* create and anchor TString */
  luaC_objbarrier(L, funcstate.f, funcstate.f->source);
  lexstate.buff = buff;
  lexstate.dyd = dyd;
  dyd->actvar.n = dyd->gt.n = dyd->label.n = 0;
  luaX_setinput(L, &lexstate, z, funcstate.f->source, firstchar);
  mainfunc(&lexstate, &funcstate);
  lua_assert(!funcstate.prev && funcstate.nups == 1 && !lexstate.fs);
  /* all scopes should be correctly finished */
  lua_assert(dyd->actvar.n == 0 && dyd->gt.n == 0 && dyd->label.n == 0);
  L->top.p--;  /* remove scanner's table */
  return cl;  /* closure is on the stack, too */
}

