/*
** $Id: lparser.h $
** Lua Parser
** See Copyright Notice in lua.h
*/

#ifndef lparser_h
#define lparser_h

#include "llimits.h"
#include "lobject.h"
#include "lzio.h"


/*
** Expression and variable descriptor.
** Code generation for variables and expressions can be delayed to allow
** optimizations; An 'expdesc' structure describes a potentially-delayed
** variable/expression. It has a description of its "main" value plus a
** list of conditional jumps that can also produce its value (generated
** by short-circuit operators 'and'/'or').
** 表达式和变量描述符。表达式就是变量的的运算吧 比如加减乘除、取反、异或等 总之最后有一个值
** 变量和表达式的代码生成可以延迟进行，以便进行优化；
一个 `expdesc` 结构体描述了一个可能被延迟处理的变量/表达式。
它包含对其“主要”值的描述，以及一个条件跳转列表，
这些条件跳转也可以产生该表达式的值（由短路运算符 `and`/`or` 生成）。
*/
typedef enum {
  VVOID,      /* 当 'expdesc' 描述一个列表的最后一个表达式时，这种类型表示一个空列表（即没有表达式） */
  VNIL,       /* 常量 nil */
  VTRUE,      /* 常量 true */
  VFALSE,     /* 常量 false */
  VK,         /* 常量在 'k' 中；info = 常量在 'k' 中的索引 */
  VKFLT,      /* 浮点常量；nval = 数值浮点值 */
  VKINT,      /* 整数常量；ival = 数值整数值 */
  VKSTR,      /* 字符串常量；strval = TString 地址（字符串由词法分析器固定） */
  VNONRELOC,  /* 表达式的值在一个固定的寄存器中；info = 结果寄存器 */
  VLOCAL,     /* 局部变量；var.ridx = 寄存器索引；var.vidx = 在 'actvar.arr' 中的相对索引 */
  VUPVAL,     /* 上值变量；info = 上值在 'upvalues' 中的索引 */
  VCONST,     /* 编译时 <const> 变量；info = 在 'actvar.arr' 中的绝对索引 */
  VINDEXED,   /* 索引变量；ind.t = 表在的寄存器index；ind.idx = 键的 R 索引 */
  VINDEXUP,   /* 索引上值；ind.t = 表在上值的index；ind.idx = 键的 K 索引 */
  VINDEXI,    /* 带有常量整数的索引变量；ind.t = 表寄存器；ind.idx = 键的值 */
  VINDEXSTR,  /* 带有字面字符串的索引变量；ind.t = 表寄存器；ind.idx = 键的 K 索引 */
  VJMP,       /* 表达式是一个测试/比较；info = 对应跳转指令的程序计数器 */
  VRELOC,     /* 表达式可以将结果放在任何寄存器中；info = 指令的程序计数器 */
  VCALL,      /* 表达式是一个函数调用；info = 指令的程序计数器 */
  VVARARG     /* 可变参数表达式；info = 指令的程序计数器 */
} expkind;


#define vkisvar(k)	(VLOCAL <= (k) && (k) <= VINDEXSTR)
#define vkisindexed(k)	(VINDEXED <= (k) && (k) <= VINDEXSTR)


typedef struct expdesc {
  expkind k;
  union {
    lua_Integer ival;    /* for VKINT */
    lua_Number nval;  /* for VKFLT */
    TString *strval;  /* for VKSTR */
    int info;  /* for generic use */
    struct {  /* for indexed variables 索引变量*/
      short idx;  /* index (R or "long" K) 索引位置 */
      lu_byte t;  /* table (register or upvalue) 表的位置 */
    } ind;
    struct {  /* for local variables */
      // 两级索引系统
      // vidx: 变量在符号表中的逻辑索引（稳定）
      // ridx: 变量在寄存器中的物理位置（可能变化）
      lu_byte ridx;  /* register holding the variable */
      unsigned short vidx;  /* compiler index (in 'actvar.arr')  */
    } var;
  } u;
  //为真时退出"的跳转列表。它保存了一个指令指针（pc）的链表。当这个表达式的结果为真时，需要跳转到哪里去执行
  int t;  /* patch list of 'exit when true' */
  //"为假时退出"的跳转列表。它保存了一个指令指针（pc）的链表。当这个表达式的结果为假时，需要跳转到哪里去执行。
  int f;  /* patch list of 'exit when false' */
  //编译器在生成条件判断指令时，可能还不知道最终要跳转的目标地址。
  //它会先生成一个跳转指令，但把目标地址先设为“待修补”（通常设为 NO_JUMP 或 -1），
  //并将这条指令的地址记录在 t 或 f 列表中。等后续知道了真正的目标地址后，再回来“修补”（patch）这些指令中的跳转偏移量。
} expdesc;


/* kinds of variables */
#define VDKREG		0   /* regular */
#define RDKCONST	1   /* constant */
#define RDKTOCLOSE	2   /* to-be-closed */
#define RDKCTC		3   /* compile-time constant */

/* description of an active local variable */
typedef union Vardesc {
  struct {
    TValuefields;  /* constant value (if it is a compile-time constant) */
    lu_byte kind;
    lu_byte ridx;  /* register holding the variable */
    short pidx;  /* index of the variable in the Proto's 'locvars' array */
    TString *name;  /* variable name */
  } vd;
  TValue k;  /* constant value (if any) */
} Vardesc;



/* description of pending(待处理的) goto statements and label statements */
typedef struct Labeldesc {
  TString *name;  /* label identifier */
  int pc;  /* position in code */
  int line;  /* line where it appeared */
  // 记录标签定义点或 goto 语句点的局部变量数量
  // 用于验证 goto 是否跨越了不同的变量作用域
  // 防止非法跳转（如跳入变量作用域内部）
  lu_byte nactvar;  /* number of active variables in that position */
  lu_byte close;  /* goto that escapes upvalues 标记 goto 是否跳出了包含上值的作用域*/
} Labeldesc;


/* list of labels or gotos */
typedef struct Labellist {
  Labeldesc *arr;  /* array */
  int n;  /* number of entries in use */
  int size;  /* array size */
} Labellist;


/* dynamic structures used by the parser */
typedef struct Dyndata {
  struct {  /* list of all active local variables */
    Vardesc *arr;
    int n;    //元素个数
    int size; //arr的size
  } actvar;
  Labellist gt;  /* list of pending gotos 遇见goto xxxx时存入gt*/
  Labellist label;   /* list of active labels 遇见 ::xxxx:: 时存入label 并解析gt中的对应项目*/
} Dyndata;


/* control of blocks */
struct BlockCnt;  /* defined in lparser.c */


/* state needed to generate code for a given function */
typedef struct FuncState {
  Proto *f;  /* current function header */
  struct FuncState *prev;  /* enclosing function 闭包函数?? */
  struct LexState *ls;  /* lexical state */
  struct BlockCnt *bl;  /* chain of current blocks */
  int pc;  /* next position to code (equivalent to 'ncode') */
  int lasttarget;   /* 'label' of last 'jump label' */
  int previousline;  /* last line that was saved in 'lineinfo' */
  int nk;  /* number of elements in 'k' */
  int np;  /* number of elements in 'p' */
  int nabslineinfo;  /* number of elements in 'abslineinfo' */
  int firstlocal;  /* index of first local var (in Dyndata array) */
  int firstlabel;  /* index of first label (in 'dyd->label->arr') */
  short ndebugvars;  /* number of elements in 'f->locvars' */
  lu_byte nactvar;  /* number of active local variables */
  lu_byte nups;  /* number of upvalues */
  lu_byte freereg;  /* first free register */
  lu_byte iwthabs;  /* instructions issued since last absolute line info */
  lu_byte needclose;  /* function needs to close upvalues when returning */
} FuncState;


LUAI_FUNC int luaY_nvarstack (FuncState *fs);
LUAI_FUNC LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
                                 Dyndata *dyd, const char *name, int firstchar);


#endif
