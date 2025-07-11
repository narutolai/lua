/*
** $Id: lgc.c $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#define lgc_c
#define LUA_CORE

#include "lprefix.h"

#include <stdio.h>
#include <string.h>


#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"


/*
** Maximum number of elements to sweep in each single step.
** (Large enough to dissipate fixed overheads but small enough
** to allow small steps for the collector.)
*/
#define GCSWEEPMAX	100

/*
** Maximum number of finalizers to call in each single step.
*/
#define GCFINMAX	10


/*
** Cost of calling one finalizer.
*/
#define GCFINALIZECOST	50


/*
** The equivalent, in bytes, of one unit of "work" (visiting a slot,
** sweeping an object, etc.)
*/
#define WORK2MEM	sizeof(TValue)


/*
** macro to adjust 'pause': 'pause' is actually used like
** 'pause / PAUSEADJ' (value chosen by tests)
*/
#define PAUSEADJ		100


/* mask with all color bits */
#define maskcolors	(bitmask(BLACKBIT) | WHITEBITS)

/* mask with all GC bits */
#define maskgcbits      (maskcolors | AGEBITS)


/* macro to erase all color bits then set only the current white bit */
#define makewhite(g,x)	\
  (x->marked = cast_byte((x->marked & ~maskcolors) | luaC_white(g)))

/* make an object gray (neither white nor black) */
#define set2gray(x)	resetbits(x->marked, maskcolors)


/* make an object black (coming from any color) */
#define set2black(x)  \
  (x->marked = cast_byte((x->marked & ~WHITEBITS) | bitmask(BLACKBIT)))


#define valiswhite(x)   (iscollectable(x) && iswhite(gcvalue(x)))

#define keyiswhite(n)   (keyiscollectable(n) && iswhite(gckey(n)))


/*
** Protected access to objects in values
*/
#define gcvalueN(o)     (iscollectable(o) ? gcvalue(o) : NULL)


#define markvalue(g,o) { checkliveness(g->mainthread,o); \
  if (valiswhite(o)) reallymarkobject(g,gcvalue(o)); }

#define markkey(g, n)	{ if keyiswhite(n) reallymarkobject(g,gckey(n)); }

#define markobject(g,t)	{ if (iswhite(t)) reallymarkobject(g, obj2gco(t)); }

/*
** mark an object that can be NULL (either because it is really optional,
** or it was stripped as debug info, or inside an uncompleted structure)
*/
#define markobjectN(g,t)	{ if (t) markobject(g,t); }

static void reallymarkobject (global_State *g, GCObject *o);
static lu_mem atomic (lua_State *L);
static void entersweep (lua_State *L);


/*
** {======================================================
** Generic functions
** =======================================================
*/


/*
** one after last element in a hash array
*/
#define gnodelast(h)	gnode(h, cast_sizet(sizenode(h)))


static GCObject **getgclist (GCObject *o) {
  switch (o->tt) {
    case LUA_VTABLE: return &gco2t(o)->gclist;
    case LUA_VLCL: return &gco2lcl(o)->gclist;
    case LUA_VCCL: return &gco2ccl(o)->gclist;
    case LUA_VTHREAD: return &gco2th(o)->gclist;
    case LUA_VPROTO: return &gco2p(o)->gclist;
    case LUA_VUSERDATA: {
      Udata *u = gco2u(o);
      lua_assert(u->nuvalue > 0);
      return &u->gclist;
    }
    default: lua_assert(0); return 0;
  }
}
static GCObject *getgclist1 (GCObject *o) {
  switch (o->tt) {
    case LUA_VTABLE: return gco2t(o)->gclist;
    case LUA_VLCL: return gco2lcl(o)->gclist;
    case LUA_VCCL: return gco2ccl(o)->gclist;
    case LUA_VTHREAD: return gco2th(o)->gclist;
    case LUA_VPROTO: return gco2p(o)->gclist;
    case LUA_VUSERDATA: {
      Udata *u = gco2u(o);
      lua_assert(u->nuvalue > 0);
      return &u->gclist;
    }
    default: lua_assert(0); return 0;
  }
}

/*
** Link a collectable object 'o' with a known type into the list 'p'.
** (Must be a macro to access the 'gclist' field in different types.)
*/
#define linkgclist(o,p)	linkgclist_(obj2gco(o), &(o)->gclist, &(p))
//头插法
static void linkgclist_ (GCObject *o, GCObject **pnext, GCObject **list) {
  lua_assert(!isgray(o));  /* cannot be in a gray list */
  *pnext = *list;
  *list = o;
  set2gray(o);  /* now it is */
}


/*
** Link a generic collectable object 'o' into the list 'p'.20010052
*/
#define linkobjgclist(o,p) linkgclist_(obj2gco(o), getgclist(o), &(p))



/*
** 清除表中空条目的键。如果条目为空，则将其标记为已死亡。这允许收集键，但保持其在表中的位置：
** 移除它可能会破坏链表并中断表的遍历。
** 其他地方从不操作已死亡的键，因为与其关联的空值已足够表明该条目在逻辑上是空的

** 专门用于 清理哈希表节点（Node）中的键（key）。
** 它的核心作用是确保当表条目（entry）的值（value）已经被标记为空（empty）时，
** 键也能被正确清理，避免内存泄漏或悬垂引用。
*/
static void clearkey (Node *n) {
  lua_assert(isempty(gval(n))); //value为nil时
  if (keyiscollectable(n)) //key是可回收对象时
    setdeadkey(n);  /* unused key; remove it 设置key为dead */
}


/*
** tells whether a key or value can be cleared from a weak
** table. Non-collectable objects are never removed from weak
** tables. Strings behave as 'values', so are never removed too. for
** other objects: if really collected, cannot keep them; for objects
** being finalized, keep them in keys, but not in values

** 用于判断一个键（key）或值（value）是否可以从弱表（weak table）中清除。
** 不可回收的对象永远不会从弱表中移除。字符串的行为类似于“值”，因此也永远不会被移除。
** 对于其他对象：如果已被真正回收，则无法保留它们；
** 对于正在被终结处理（finalized）的对象，保留它们在键中，但不保留在值中
*/
static int iscleared (global_State *g, const GCObject *o) {
  if (o == NULL) return 0;  /* non-collectable value */
  else if (novariant(o->tt) == LUA_TSTRING) {
    markobject(g, o);  /* strings are 'values', so are never weak */
    return 0;
  }
  else return iswhite(o);
}


/*
   屏障函数，用于推动垃圾收集器前进。具体来说，它会标记由黑色对象'o'指向的白色对象'v'。
** 在分代模式下，如果'o'是老对象，那么'v'也必须变成老对象；然而，它不能直接被标记为OLD，
** 因为它可能仍然指向非老的对象。因此，它会被标记为OLD0。在下一个周期中，它会变成OLD1，
** 再下一个周期，它最终会变成OLD（即常规的老对象）。到那时，它所指向的任何对象也都将是老的。
** 如果在增量扫描阶段调用此函数，它会将黑色对象'o'清除为白色（即扫描它），以避免对同一对象进行其他屏障调用。
** （在分代模式下不能这样做，因为其扫描过程不区分白色和已死亡的对象。）
*/
void luaC_barrier_ (lua_State *L, GCObject *o, GCObject *v) {
  global_State *g = G(L);
  lua_assert(isblack(o) && iswhite(v) && !isdead(g, v) && !isdead(g, o));
  if (keepinvariant(g)) {  /* must keep invariant? */
    reallymarkobject(g, v);  /* restore invariant */
    if (isold(o)) {
      lua_assert(!isold(v));  /* white object could not be old */
      setage(v, G_OLD0);  /* restore generational invariant */
    }
  }
  else {  /* sweep phase */
    lua_assert(issweepphase(g));
    if (g->gckind == KGC_INC)  /* incremental mode? */
      makewhite(g, o);  /* mark 'o' as white to avoid other barriers */
  }
}


/*
** barrier that moves collector backward, that is, mark the black object
** pointing to a white object as gray again.
*/
void luaC_barrierback_ (lua_State *L, GCObject *o) {
  global_State *g = G(L);
  lua_assert(isblack(o) && !isdead(g, o));
  lua_assert((g->gckind == KGC_GEN) == (isold(o) && getage(o) != G_TOUCHED1));
  if (getage(o) == G_TOUCHED2)  /* already in gray list? */
    set2gray(o);  /* make it gray to become touched1 */
  else  /* link it in 'grayagain' and paint it gray */
    linkobjgclist(o, g->grayagain);
  if (isold(o))  /* generational mode? */
    setage(o, G_TOUCHED1);  /* touched in current cycle */
}


void luaC_fix (lua_State *L, GCObject *o) {
  global_State *g = G(L);
  lua_assert(g->allgc == o);  /* object must be 1st in 'allgc' list! */
  set2gray(o);  /* they will be gray forever */
  setage(o, G_OLD);  /* and old forever */
  g->allgc = o->next;  /* remove object from 'allgc' list */
  o->next = g->fixedgc;  /* link it to 'fixedgc' list */
  g->fixedgc = o;
}


/*
** create a new collectable object (with given type, size, and offset)
** and link it to 'allgc' list.
*/
GCObject *luaC_newobjdt (lua_State *L, int tt, size_t sz, size_t offset) {
  global_State *g = G(L);
  char *p = cast_charp(luaM_newobject(L, novariant(tt), sz));
  GCObject *o = cast(GCObject *, p + offset);
  o->marked = luaC_white(g);
  o->tt = tt;
  o->next = g->allgc;
  g->allgc = o;
  return o;
}


GCObject *luaC_newobj (lua_State *L, int tt, size_t sz) {
  return luaC_newobjdt(L, tt, sz, 0);
}

/* }====================================================== */



/*
** {======================================================
** Mark functions
** =======================================================
*/


/*
标记一个object
Mark an object. 

没有user value 的userdata 、字符串、关闭的上值直接变黑色
Userdata with no user values, strings, and closed upvalues are visited and turned black here. 

开放上值已经通过它们各自线程在'twups'列表中的间接链接被处理，因此它们不会进入灰色列表
Open upvalues are already indirectly linked through their respective threads in the'twups' list, so they don't go to the gray list; 

然而，它们仍然保持灰色以避免屏障（barrier）操作，因为它们的值将通过线程或'remarkupvals'函数被重新访问
nevertheless(然而), they are kept gray to avoid barriers, as their values will be revisited by the thread or by 'remarkupvals'.  

其他的object添加到gray list 等稍后访问
Other objects are added to the gray list to be visited (and turned black) later.

userdata和upvalue都可以递归调用这个函数，但这种递归最多只有两级  
Both userdata and upvalues can call this function recursively, but this recursion goes for at most two levels: 

一个upvalue不能引用另一个upvalue（只有闭包可以），并且一个userdata的元表必须是一个表
An upvalue cannot refer to another upvalue(only closures can), and a userdata's metatable must be a table.
*/
static void reallymarkobject (global_State *g, GCObject *o) {
  switch (o->tt) {
    case LUA_VSHRSTR://短字符和长字符
    case LUA_VLNGSTR: {
      set2black(o);  /* 长短字符串直接变黑意味着他们永远不会被回收 */
      break;
    }
    case LUA_VUPVAL: {
      UpVal *uv = gco2upv(o);
      if (upisopen(uv))
        set2gray(uv);  /* open upvalues are kept gray 开放上值保持灰色*/
      else
        set2black(uv);  /* closed upvalues are visited here 关闭上值直接置黑色*/
      markvalue(g, uv->v.p);  /* mark its content 标记内容*/
      break;
    }
    case LUA_VUSERDATA: {
      Udata *u = gco2u(o);
      if (u->nuvalue == 0) {  /* no user values? */
        markobjectN(g, u->metatable);  /* mark its metatable */
        set2black(u);  /* nothing else to mark */
        break;
      }
      /* else... */
    }  /* FALLTHROUGH 失败*/
    case LUA_VLCL: case LUA_VCCL: case LUA_VTABLE:
    case LUA_VTHREAD: case LUA_VPROTO: {
      linkobjgclist(o, g->gray);  /* to be visited later */
      break;
    }
    default: lua_assert(0); break;
  }
}


/*
** mark metamethods for basic types
*/
static void markmt (global_State *g) {
  int i;
  for (i=0; i < LUA_NUMTAGS; i++)
    markobjectN(g, g->mt[i]);
}


/*
** mark all objects in list of being-finalized
*/
static lu_mem markbeingfnz (global_State *g) {
  GCObject *o;
  lu_mem count = 0;
  for (o = g->tobefnz; o != NULL; o = o->next) {
    count++;
    markobject(g, o);
  }
  return count;
}


/*
** For each non-marked thread, simulates a barrier between each open
** upvalue and its value. (If the thread is collected, the value will be
** assigned to the upvalue, but then it can be too late for the barrier
** to act. The "barrier" does not need to check colors: A non-marked
** thread must be young; upvalues cannot be older than their threads; so
** any visited upvalue must be young too.) Also removes the thread from
** the list, as it was already visited. Removes also threads with no
** upvalues, as they have nothing to be checked. (If the thread gets an
** upvalue later, it will be linked in the list again.)
  对于每个未被标记的线程，模拟每个开放上值与其值之间的屏障。
  （如果线程被回收，该值会被赋给上值，但此时屏障可能已来不及生效。
  "屏障"无需检查颜色：未被标记的线程必然是新生代；上值的年龄不会超过其所属线程；因此任何被访问的上值也必然是新生代。）
  同时将线程从列表中移除，因为它已被访问过。还会移除没有上值的线程，因为它们无需检查。
  （如果线程后续获得了上值，它会被重新加入列表。）
*/
static int remarkupvals (global_State *g) {
  lua_State *thread;
  lua_State **p = &g->twups;
  int work = 0;  /* estimate of how much work was done here */
  while ((thread = *p) != NULL) {
    work++;
    if (!iswhite(thread) && thread->openupval != NULL)
      p = &thread->twups;  /* keep marked thread with upvalues in the list */
    else {  /* thread is not marked or without upvalues */
      UpVal *uv;
      lua_assert(!isold(thread) || thread->openupval == NULL);
      *p = thread->twups;  /* remove thread from the list */
      thread->twups = thread;  /* mark that it is out of list */
      for (uv = thread->openupval; uv != NULL; uv = uv->u.open.next) {
        lua_assert(getage(uv) <= getage(thread));
        work++;
        if (!iswhite(uv)) {  /* upvalue already visited? */
          lua_assert(upisopen(uv) && isgray(uv));
          markvalue(g, uv->v.p);  /* mark its value */
        }
      }
    }
  }
  return work;
}


static void cleargraylists (global_State *g) {
  g->gray = g->grayagain = NULL;
  g->weak = g->allweak = g->ephemeron = NULL;
}


/*
** mark root set and reset all gray lists, to start a new collection
*/
static void restartcollection (global_State *g) {
  cleargraylists(g);
  markobject(g, g->mainthread); //直接链到灰色链表,标灰色
  markvalue(g, &g->l_registry); //直接链到灰色链表,标灰色 l_registry是TValue
  markmt(g);
  markbeingfnz(g);  /* mark any finalizing object left from previous cycle */
}

/* }====================================================== */


/*
** {======================================================
** Traverse functions
** =======================================================
*/


/*
** Check whether object 'o' should be kept in the 'grayagain' list for
** post-processing by 'correctgraylist'. (It could put all old objects
** in the list and leave all the work to 'correctgraylist', but it is
** more efficient to avoid adding elements that will be removed.) Only
** TOUCHED1 objects need to be in the list. TOUCHED2 doesn't need to go
** back to a gray list, but then it must become OLD. (That is what
** 'correctgraylist' does when it finds a TOUCHED2 object.)
*/
static void genlink (global_State *g, GCObject *o) {
  lua_assert(isblack(o));
  if (getage(o) == G_TOUCHED1) {  /* touched in this cycle? */
    linkobjgclist(o, g->grayagain);  /* link it back in 'grayagain' */
  }  /* everything else do not need to be linked back */
  else if (getage(o) == G_TOUCHED2)
    changeage(o, G_TOUCHED2, G_OLD);  /* advance age */
}


   /*
   ** 遍历一个具有弱值的表，并将其链接到适当的列表中。
   ** 在传播阶段，将其保留在 'grayagain' 列表中，以便在原子阶段再次访问。
   ** 在原子阶段，如果表中有任何白色值，则将其放入 'weak' 列表中，以便清除。
   ** 哈希部分的遍历仅标记键，值的清理延迟到原子阶段
   ** Lua 的 GC 始终 只标记强引用对象，弱引用对象的存活由外部强引用决定
   */
static void traverseweakvalue (global_State *g, Table *h) {
  Node *n, *limit = gnodelast(h);
  /* if there is array part, assume it may have white values (it is not
     worth traversing it now just to check) */
  //数组有内容就表示有要清理 
  int hasclears = (h->alimit > 0);
  for (n = gnode(h, 0); n < limit; n++) {  /* traverse hash part */
    if (isempty(gval(n)))  /* 值是nil  */
      clearkey(n);  /* 设置deadkey（因为键是强引用，但值为 nil 的节点可回收）*/
    else {
      lua_assert(!keyisnil(n));/*键不能为 nil（Lua 表的哈希部分不允许 nil 键）*/
      markkey(g, n); // 标记key（键是强引用，必须标记）

      //iscleared说明value没有被标记,也就是说没有在其他地方被引用,
      //按照弱值表的定义,如果value没有在其他地方被引用就要清理了
      if (!hasclears && iscleared(g, gcvalueN(gval(n))))  /*  值是白色（未标记,说明）? */
        hasclears = 1;  /*  标记表需要后续清理 */
    }
  }
  //如果是在原子阶段发现要清理则放在weak链表里
  if (g->gcstate == GCSatomic && hasclears)
    linkgclist(h, g->weak);  /* has to be cleared later */
  else//否则其实就是传播阶段吧
    linkgclist(h, g->grayagain);  /* must retraverse it in atomic phase */
}


/*
  ** 遍历一个瞬表（ephemeron table  weak key表），并将其链接到适当的列表中。
   ** 如果在遍历过程中标记了任何对象，则返回 true（这意味着收敛必须继续）。
   ** 在传播阶段，将表保留在 'grayagain' 列表中，以便在原子阶段再次访问。
   ** 在原子阶段，如果表中有任何白色->白色条目，则必须在短暂收敛期间重新访问该表（因为该键可能变成黑色）。
   ** 否则，如果表中有任何白色键，则必须在原子阶段清除表。
   ** 在生成模式中，一些表必须保留在某些灰色列表中以进行后处理；这由 'genlink' 完成。
   ** 弱key, 强value
*/
static int traverseephemeron (global_State *g, Table *h, int inv) {
  int marked = 0;  /* 本次遍历是否标记了对象 */
  int hasclears = 0;  /* 表是否有白色（未标记）键 */
  int hasww = 0;  /* 表是否有 "白色键 -> 白色值" 的条目 */
  unsigned int i;
  unsigned int asize = luaH_realasize(h);
  unsigned int nsize = sizenode(h);
  /* traverse array part */
  for (i = 0; i < asize; i++) {
    if (valiswhite(&h->array[i])) {     //值是白色？
      marked = 1;
      reallymarkobject(g, gcvalue(&h->array[i]));/* 标记值 */
    }
  }
  /* traverse hash part; if 'inv', traverse descending
     (see 'convergeephemerons') */
  for (i = 0; i < nsize; i++) {
    Node *n = inv ? gnode(h, nsize - 1 - i) : gnode(h, i);/* 逆序或正序遍历 */
    if (isempty(gval(n)))  /* 条目为空（值为 nil）? */
      clearkey(n);  /*  清理键 因为键值对无效*/
    else if (iscleared(g, gckeyN(n))) {  /* key还未被标记? */
      hasclears = 1;             /* 需要清理key */
      if (valiswhite(gval(n)))  /*值也是白色？ */
        hasww = 1;           /* white-white entry */
    }
    else if (valiswhite(gval(n))) {           /*key已标记，但value是白色？ */
      marked = 1;
      reallymarkobject(g, gcvalue(gval(n)));  /* 标记值 */
    }
  }
  /* link table into proper list */
  if (g->gcstate == GCSpropagate)
    linkgclist(h, g->grayagain);  /*  原子阶段需重新遍历 */
  else if (hasww)  /* 存在 "白色键 -> 白色值? */
    linkgclist(h, g->ephemeron);  /* have to propagate again 需再次传播 */
  else if (hasclears)  /* table has white keys? 存在白色键 */
    linkgclist(h, g->allweak);  /* may have to clean white keys 需清理键 
    键的存活与否会影响对应值的存活（即使值本身是强引用）。*/
  else
    genlink(g, obj2gco(h));  /* check whether collector still needs to see it */
  return marked;
}

//遍历强表 强表就是全部标记 表示被引用
static void traversestrongtable (global_State *g, Table *h) {
  Node *n, *limit = gnodelast(h);
  unsigned int i;
  unsigned int asize = luaH_realasize(h);
  for (i = 0; i < asize; i++)  /* traverse array part */
    markvalue(g, &h->array[i]);
  for (n = gnode(h, 0); n < limit; n++) {  /* traverse hash part */
    if (isempty(gval(n)))  /* entry is empty? */
      clearkey(n);  /* clear its key */
    else {
      lua_assert(!keyisnil(n));
      markkey(g, n);        //标记key
      markvalue(g, gval(n));//标记value
    }
  }
  genlink(g, obj2gco(h));
}

/*据表的 弱引用模式（weak mode） 决定如何处理其键和值
允许表的键或值被垃圾回收，即使它们仍被表引用（常用于缓存、观察者模式等场景）。
例如："k" 模式适合实现弱键字典（如 WeakMap），"v" 模式适合弱值缓存
*/
static lu_mem traversetable (global_State *g, Table *h) {
  const char *weakkey, *weakvalue;
  const TValue *mode = gfasttm(g, h->metatable, TM_MODE);
  markobjectN(g, h->metatable); //标记表的元表（metatable）为活跃对象
  if (mode && ttisstring(mode) &&  /* is there a weak mode? */
      (cast_void(weakkey = strchr(svalue(mode), 'k')),
       cast_void(weakvalue = strchr(svalue(mode), 'v')),
       (weakkey || weakvalue))) {  /* is really weak? */
    if (!weakkey)  /* strong keys? */
      traverseweakvalue(g, h); //弱值模式 仅遍历表的 值，并忽略未被标记的键（弱键会被回收）
    else if (!weakvalue)  /* strong values? */
      traverseephemeron(g, h, 0);/*处理 短暂表（ephemeron），即键弱、值强的表。
                              如果键未被标记，则忽略整个键值对（即使值是活跃的）。
                              如果键被标记，则递归标记值。*/
    else  /* all weak */
      linkgclist(h, g->allweak);  /* nothing to traverse now */
  }
  else  /* not weak */
    traversestrongtable(g, h);
  return 1 + h->alimit + 2 * allocsizenode(h);
}


static int traverseudata (global_State *g, Udata *u) {
  int i;
  markobjectN(g, u->metatable);  /* mark its metatable */
  for (i = 0; i < u->nuvalue; i++)
    markvalue(g, &u->uv[i].uv);
  genlink(g, obj2gco(u));
  return 1 + u->nuvalue;
}


/*
** Traverse a prototype. (While a prototype is being build, its
** arrays can be larger than needed; the extra slots are filled with
** NULL, so the use of 'markobjectN')
*/
static int traverseproto (global_State *g, Proto *f) {
  int i;
  markobjectN(g, f->source);
  for (i = 0; i < f->sizek; i++)  /* mark literals */
    markvalue(g, &f->k[i]);
  for (i = 0; i < f->sizeupvalues; i++)  /* mark upvalue names */
    markobjectN(g, f->upvalues[i].name);
  for (i = 0; i < f->sizep; i++)  /* mark nested protos */
    markobjectN(g, f->p[i]);
  for (i = 0; i < f->sizelocvars; i++)  /* mark local-variable names */
    markobjectN(g, f->locvars[i].varname);
  return 1 + f->sizek + f->sizeupvalues + f->sizep + f->sizelocvars;
}


static int traverseCclosure (global_State *g, CClosure *cl) {
  int i;
  for (i = 0; i < cl->nupvalues; i++)  /* mark its upvalues */
    markvalue(g, &cl->upvalue[i]);
  return 1 + cl->nupvalues;
}

/*
** Traverse a Lua closure, marking its prototype and its upvalues.
** (Both can be NULL while closure is being created.)
*/
static int traverseLclosure (global_State *g, LClosure *cl) {
  int i;
  markobjectN(g, cl->p);  /* mark its prototype */
  for (i = 0; i < cl->nupvalues; i++) {  /* visit its upvalues */
    UpVal *uv = cl->upvals[i];
    markobjectN(g, uv);  /* mark upvalue */
  }
  return 1 + cl->nupvalues;
}


/*
** Traverse a thread, marking the elements in the stack up to its top
** and cleaning the rest of the stack in the final traversal. That
** ensures that the entire stack have valid (non-dead) objects.
** Threads have no barriers. In gen. mode, old threads must be visited
** at every cycle, because they might point to young objects.  In inc.
** mode, the thread can still be modified before the end of the cycle,
** and therefore it must be visited again in the atomic phase. To ensure
** these visits, threads must return to a gray list if they are not new
** (which can only happen in generational mode) or if the traverse is in
** the propagate phase (which can only happen in incremental mode).
*/
static int traversethread (global_State *g, lua_State *th) {
  UpVal *uv;
  StkId o = th->stack.p;
  if (isold(th) || g->gcstate == GCSpropagate)
    linkgclist(th, g->grayagain);  /* insert into 'grayagain' list */
  if (o == NULL)
    return 1;  /* stack not completely built yet */
  lua_assert(g->gcstate == GCSatomic ||
             th->openupval == NULL || isintwups(th));
  for (; o < th->top.p; o++)  /* mark live elements in the stack */
    markvalue(g, s2v(o));
  for (uv = th->openupval; uv != NULL; uv = uv->u.open.next)
    markobject(g, uv);  /* open upvalues cannot be collected */
  if (g->gcstate == GCSatomic) {  /* final traversal? */
    for (; o < th->stack_last.p + EXTRA_STACK; o++)
      setnilvalue(s2v(o));  /* clear dead stack slice */
    /* 'remarkupvals' may have removed thread from 'twups' list */
    if (!isintwups(th) && th->openupval != NULL) {
      th->twups = g->twups;  /* link it back to the list */
      g->twups = th;
    }
  }
  else if (!g->gcemergency)
    luaD_shrinkstack(th); /* do not change stack in emergency cycle */
  return 1 + stacksize(th);
}


/*
** traverse one gray object, turning it to black.
*/
static lu_mem propagatemark (global_State *g) {
  GCObject *o = g->gray;
  nw2black(o);
  g->gray = *getgclist(o);  /* remove from 'gray' list */
  switch (o->tt) {
    case LUA_VTABLE: return traversetable(g, gco2t(o));
    case LUA_VUSERDATA: return traverseudata(g, gco2u(o));
    case LUA_VLCL: return traverseLclosure(g, gco2lcl(o));
    case LUA_VCCL: return traverseCclosure(g, gco2ccl(o));
    case LUA_VPROTO: return traverseproto(g, gco2p(o));
    case LUA_VTHREAD: return traversethread(g, gco2th(o));
    default: lua_assert(0); return 0;
  }
}


static lu_mem propagateall (global_State *g) {
  lu_mem tot = 0;
  while (g->gray)
    tot += propagatemark(g);
  return tot;
}


/*
** Traverse all ephemeron tables propagating marks from keys to values.
** Repeat until it converges, that is, nothing new is marked. 'dir'
** inverts the direction of the traversals, trying to speed up
** convergence on chains in the same table.
**
*/
static void convergeephemerons (global_State *g) {
  int changed;
  int dir = 0;  // 遍历方向（0 或 1，用于优化标记顺序）
  do {
    GCObject *w;
    GCObject *next = g->ephemeron;  // 获取当前 ephemeron 表链表
    g->ephemeron = NULL;  // 清空全局链表，后续可能重新加入
    changed = 0;  // 标记本轮是否有新的对象被标记
    while ((w = next) != NULL) {  // 遍历每个 ephemeron 表
      Table *h = gco2t(w);  // 转换为 Table 结构
      next = h->gclist;  // 保存下一个表（因为链表可能被修改）
      nw2black(h);  // 将表标记为黑色（表示已处理）
      if (traverseephemeron(g, h, dir)) {  // 尝试从 key 传播标记到 value
        propagateall(g);  // 如果有新的标记，立即传播
        changed = 1;  // 需要再次检查所有 ephemeron 表
      }
    }
    dir = !dir;  // 切换遍历方向（优化标记顺序）
  } while (changed);  // 如果本轮有变化，继续下一轮遍历
}


/* }====================================================== */


/*
** {======================================================
** Sweep Functions
** =======================================================
*/


/*
** clear entries with unmarked keys from all weaktables in list 'l'
** 目的：遍历一组弱表（通过链表 l 连接），删除所有 键未被 GC 标记（即未被引用） 的条目。
   适用场景：在 GC 的标记阶段之后、清除阶段之前调用，确保弱表只保留存活的键值对。
*/
static void clearbykeys (global_State *g, GCObject *l) {
  for (; l; l = gco2t(l)->gclist) {
    Table *h = gco2t(l);
    Node *limit = gnodelast(h);
    Node *n;
    for (n = gnode(h, 0); n < limit; n++) {
      if (iscleared(g, gckeyN(n)))  /* unmarked key?  */
        setempty(gval(n));  /* remove entry */
      if (isempty(gval(n)))  /* is entry empty? */
        clearkey(n);  /* clear its key */
    }
  }
}


/*
** clear entries with unmarked values from all weaktables in list 'l' up
** to element 'f'
** 遍历一组弱表（链表结构），清理其中所有 未被 GC 标记的值（即这些值已被判定为垃圾）。
适用场景：
弱表的值为弱引用（__weak 模式）时，当值未被其他强引用持有，需自动移除对应的键值对
** 先发现是kv 然后才会link到allweak上
*/
static void clearbyvalues (global_State *g, GCObject *l, GCObject *f) {
  for (; l != f; l = gco2t(l)->gclist) {
    Table *h = gco2t(l);
    Node *n, *limit = gnodelast(h);
    unsigned int i;
    unsigned int asize = luaH_realasize(h);
    for (i = 0; i < asize; i++) {
      TValue *o = &h->array[i];
      if (iscleared(g, gcvalueN(o)))  /* 最后调用的iswhite(o) 没有被其他的引用*/
        setempty(o);  /* remove entry luavempty*/
    }
    for (n = gnode(h, 0); n < limit; n++) {
      if (iscleared(g, gcvalueN(gval(n))))  /* unmarked value? */
        setempty(gval(n));  /* remove entry */
      if (isempty(gval(n)))  /* is entry empty? */
        clearkey(n);  /* clear its key set dead key*/
    }
  }
}


static void freeupval (lua_State *L, UpVal *uv) {
  if (upisopen(uv))
    luaF_unlinkupval(uv);
  luaM_free(L, uv);
}


static void freeobj (lua_State *L, GCObject *o) {
  switch (o->tt) {
    case LUA_VPROTO:
      luaF_freeproto(L, gco2p(o));
      break;
    case LUA_VUPVAL:
      freeupval(L, gco2upv(o));
      break;
    case LUA_VLCL: {
      LClosure *cl = gco2lcl(o);
      luaM_freemem(L, cl, sizeLclosure(cl->nupvalues));
      break;
    }
    case LUA_VCCL: {
      CClosure *cl = gco2ccl(o);
      luaM_freemem(L, cl, sizeCclosure(cl->nupvalues));
      break;
    }
    case LUA_VTABLE:
      luaH_free(L, gco2t(o));
      break;
    case LUA_VTHREAD:
      luaE_freethread(L, gco2th(o));
      break;
    case LUA_VUSERDATA: {
      Udata *u = gco2u(o);
      luaM_freemem(L, o, sizeudata(u->nuvalue, u->len));
      break;
    }
    case LUA_VSHRSTR: {
      TString *ts = gco2ts(o);
      luaS_remove(L, ts);  /* remove it from hash table */
      luaM_freemem(L, ts, sizelstring(ts->shrlen));
      break;
    }
    case LUA_VLNGSTR: {
      TString *ts = gco2ts(o);
      luaM_freemem(L, ts, sizelstring(ts->u.lnglen));
      break;
    }
    default: lua_assert(0);
  }
}


/*
** 从一个GCObject对象的列表中最多扫描'countin'个元素，删除那些被标记为旧（非当前）白色的死亡对象，
** 将所有非死亡对象重新标记为白色，为下一个垃圾收集周期做准备。返回继续遍历的位置，
** 如果列表遍历完成则返回NULL。('*countout'用于获取遍历的元素数量。)
** 它的作用是遍历一个对象链表，释放未被标记（dead）的对象，并更新存活对象的标记状态
*/
static GCObject **sweeplist (lua_State *L, GCObject **p, int countin,
                             int *countout) {
  global_State *g = G(L);
  //假如一开始的luaC_white是3 后来atomic过后进行翻转,luaC_white就是4了，然后 otherwhite依旧是3
  int ow = otherwhite(g);   //获取当前非活跃的白色标记（非当前回收周期）4
  int i;
  int white = luaC_white(g);  /* current white 当前回收周期的白色标记*/
  for (i = 0; *p != NULL && i < countin; i++) {
    GCObject *curr = *p;
    int marked = curr->marked; //3
    if (isdeadm(ow, marked)) {  /* &运算大于0 说明是同一种白那么就要删除 为被标记 is 'curr' dead?  对象是否未被标记*/
      *p = curr->next;  /* remove 'curr' from list  从链表中移除对象*/
      freeobj(L, curr);  /* erase 'curr'  释放对象内存*/
    }
    else {  /* change mark to 'white' 否则是被标记成黑色或者另一种白,然后要把marked统一搞成另一种白*/
      curr->marked = cast_byte((marked & ~maskgcbits) | white); //清0再或上另一种白
      p = &curr->next;  /* go to next element */
    }
  }
  if (countout)
    *countout = i;  /* number of elements traversed */
  return (*p == NULL) ? NULL : p;
}


/*
** sweep a list until a live object (or end of list)
*/
static GCObject **sweeptolive (lua_State *L, GCObject **p) {
  GCObject **old = p;
  do {
    p = sweeplist(L, p, 1, NULL);
  } while (p == old);
  return p;
}

/* }====================================================== */


/*
** {======================================================
** Finalization
** =======================================================
*/

/*
** If possible, shrink string table.
*/
static void checkSizes (lua_State *L, global_State *g) {
  if (!g->gcemergency) {
    if (g->strt.nuse < g->strt.size / 4) {  /* string table too big? */
      l_mem olddebt = g->GCdebt;
      luaS_resize(L, g->strt.size / 2);
      g->GCestimate += g->GCdebt - olddebt;  /* correct estimate */
    }
  }
}


/*
** Get the next udata to be finalized from the 'tobefnz' list, and
** link it back into the 'allgc' list.
** 把它链接回allgclist 
*/
static GCObject *udata2finalize (global_State *g) {
  GCObject *o = g->tobefnz;  /* get first element */
  lua_assert(tofinalize(o));
  g->tobefnz = o->next;  /* remove it from 'tobefnz' list */
  o->next = g->allgc;  /* return it to 'allgc' list */
  g->allgc = o;
  resetbit(o->marked, FINALIZEDBIT);  /* object is "normal" again */
  if (issweepphase(g))
    makewhite(g, o);  /* "sweep" object */
  else if (getage(o) == G_OLD1)
    g->firstold1 = o;  /* it is the first OLD1 object in the list */
  return o;
}


static void dothecall (lua_State *L, void *ud) {
  UNUSED(ud);
  luaD_callnoyield(L, L->top.p - 2, 0);
}


static void GCTM (lua_State *L) {
  global_State *g = G(L);
  const TValue *tm;
  TValue v;
  lua_assert(!g->gcemergency);
  setgcovalue(L, &v, udata2finalize(g)); //这里会把tobefnz上的obj取下来放回 allgc链表
  tm = luaT_gettmbyobj(L, &v, TM_GC);
  if (!notm(tm)) {  /* is there a finalizer? */
    int status;
    lu_byte oldah = L->allowhook;
    int oldgcstp  = g->gcstp;
    g->gcstp |= GCSTPGC;  /* avoid GC steps */
    L->allowhook = 0;  /* stop debug hooks during GC metamethod */
    setobj2s(L, L->top.p++, tm);  /* push finalizer... */
    setobj2s(L, L->top.p++, &v);  /* ... and its argument */
    L->ci->callstatus |= CIST_FIN;  /* will run a finalizer */
    status = luaD_pcall(L, dothecall, NULL, savestack(L, L->top.p - 2), 0);
    L->ci->callstatus &= ~CIST_FIN;  /* not running a finalizer anymore */
    L->allowhook = oldah;  /* restore hooks */
    g->gcstp = oldgcstp;  /* restore state */
    if (l_unlikely(status != LUA_OK)) {  /* error while running __gc? */
      luaE_warnerror(L, "__gc");
      L->top.p--;  /* pops error object */
    }
  }
}


/*
** Call a few finalizers
*/
static int runafewfinalizers (lua_State *L, int n) {
  global_State *g = G(L);
  int i;
  for (i = 0; i < n && g->tobefnz; i++)
    GCTM(L);  /* call one finalizer */
  return i;
}


/*
** call all pending finalizers
*/
static void callallpendingfinalizers (lua_State *L) {
  global_State *g = G(L);
  while (g->tobefnz)
    GCTM(L);
}


/*
** find last 'next' field in list 'p' list (to add elements in its end)
*/
static GCObject **findlast (GCObject **p) {
  while (*p != NULL)
    p = &(*p)->next;
  return p;
}


/*
** Move all unreachable objects (or 'all' objects) that need
** finalization from list 'finobj' to list 'tobefnz' (to be finalized).
** (Note that objects after 'finobjold1' cannot be white, so they
** don't need to be traversed. In incremental mode, 'finobjold1' is NULL,
** so the whole list is traversed.)
**将所有不可达的（或‘所有’需要终结的）对象从 finobj 列表移动到 tobefnz（待终结）列表。
（注意：finobjold1 之后的对象不可能是白色（未被标记），因此无需遍历。在增量模式下，finobjold1 为 NULL，
 此时会遍历整个列表。）
*/
static void separatetobefnz (global_State *g, int all) {
  GCObject *curr;
  GCObject **p = &g->finobj;
  GCObject **lastnext = findlast(&g->tobefnz);
  while ((curr = *p) != g->finobjold1) {  /* traverse all finalizable objects */
    lua_assert(tofinalize(curr));
    if (!(iswhite(curr) || all))  /* not being collected? */
      p = &curr->next;  /* don't bother with it */
    else {
      if (curr == g->finobjsur)  /* removing 'finobjsur'? */
        g->finobjsur = curr->next;  /* correct it */
      *p = curr->next;  /* remove 'curr' from 'finobj' list */
      curr->next = *lastnext;  /* link at the end of 'tobefnz' list */
      *lastnext = curr;
      lastnext = &curr->next;
    }
  }
}


/*
** If pointer 'p' points to 'o', move it to the next element.
*/
static void checkpointer (GCObject **p, GCObject *o) {
  if (o == *p)
    *p = o->next;
}


/*
** Correct pointers to objects inside 'allgc' list when
** object 'o' is being removed from the list.
*/
static void correctpointers (global_State *g, GCObject *o) {
  checkpointer(&g->survival, o);
  checkpointer(&g->old1, o);
  checkpointer(&g->reallyold, o);
  checkpointer(&g->firstold1, o);
}


/*
** if object 'o' has a finalizer, remove it from 'allgc' list (must
** search the list to find it) and link it in 'finobj' list.
** 如果一个obj有__gc方法，把它从allgc链表移除然后移到finobj链表
** 这个函数会在设置table或者userdata的元表时调用
*/
void luaC_checkfinalizer (lua_State *L, GCObject *o, Table *mt) {
  global_State *g = G(L);
  if (tofinalize(o) ||                 /* obj. is already marked... */
      gfasttm(g, mt, TM_GC) == NULL ||    /* or has no finalizer... */
      (g->gcstp & GCSTPCLS))                   /* or closing state? */
    return;  /* nothing to be done */
  else {  /* move 'o' to 'finobj' list */
      GCObject **p;
      if (issweepphase(g)) {
        makewhite(g, o);  /* "sweep" object 'o' */
        if (g->sweepgc == &o->next)  /* should not remove 'sweepgc' object */
          g->sweepgc = sweeptolive(L, g->sweepgc);  /* change 'sweepgc' */
      }
      else
        correctpointers(g, o);
      /* search for pointer pointing to 'o' */
      for (p = &g->allgc; *p != o; p = &(*p)->next) { /* empty */ }
      *p = o->next;  /* remove 'o' from 'allgc' list */
      o->next = g->finobj;  /* link it in 'finobj' list */
      g->finobj = o;
      l_setbit(o->marked, FINALIZEDBIT);  /* mark it as such */
  }
}

/* }====================================================== */


/*
** {======================================================
** Generational Collector
** =======================================================
*/


/*
** Set the "time" to wait before starting a new GC cycle; cycle will
** start when memory use hits the threshold of ('estimate' * pause /
** PAUSEADJ). (Division by 'estimate' should be OK: it cannot be zero,
** because Lua cannot even start with less than PAUSEADJ bytes).
设置启动新一轮垃圾回收（GC）周期前的“等待时间”；当内存使用量达到阈值（即 'estimate' * pause / PAUSEADJ）时，
周期将启动。
（对 'estimate' 的除法运算是安全的：它不可能为零，因为 Lua 启动时所需的内存不可能低于 PAUSEADJ 字节）。
*/
static void setpause (global_State *g) {
  l_mem threshold, debt;
  int pause = getgcparam(g->gcpause);
  l_mem estimate = g->GCestimate / PAUSEADJ;  /* adjust 'estimate' */
  lua_assert(estimate > 0);
  threshold = (pause < MAX_LMEM / estimate)  /* overflow? */
            ? estimate * pause  /* no overflow */
            : MAX_LMEM;  /* overflow; truncate to maximum */
  debt = gettotalbytes(g) - threshold;
  if (debt > 0) debt = 0;
  luaE_setdebt(g, debt);
}


/*
** Sweep a list of objects to enter generational mode.  Deletes dead
** objects and turns the non dead to old. All non-dead threads---which
** are now old---must be in a gray list. Everything else is not in a
** gray list. Open upvalues are also kept gray.
*/
static void sweep2old (lua_State *L, GCObject **p) {
  GCObject *curr;
  global_State *g = G(L);
  while ((curr = *p) != NULL) {
    if (iswhite(curr)) {  /* is 'curr' dead? */
      lua_assert(isdead(g, curr));
      *p = curr->next;  /* remove 'curr' from list */
      freeobj(L, curr);  /* erase 'curr' */
    }
    else {  /* all surviving objects become old */
      setage(curr, G_OLD);
      if (curr->tt == LUA_VTHREAD) {  /* threads must be watched */
        lua_State *th = gco2th(curr);
        linkgclist(th, g->grayagain);  /* insert into 'grayagain' list */
      }
      else if (curr->tt == LUA_VUPVAL && upisopen(gco2upv(curr)))
        set2gray(curr);  /* open upvalues are always gray */
      else  /* everything else is black */
        nw2black(curr);
      p = &curr->next;  /* go to next element */
    }
  }
}


/*
** Sweep for generational mode. Delete dead objects. (Because the
** collection is not incremental, there are no "new white" objects
** during the sweep. So, any white object must be dead.) For
** non-dead objects, advance their ages and clear the color of
** new objects. (Old objects keep their colors.)
** The ages of G_TOUCHED1 and G_TOUCHED2 objects cannot be advanced
** here, because these old-generation objects are usually not swept
** here.  They will all be advanced in 'correctgraylist'. That function
** will also remove objects turned white here from any gray list.
*/
static GCObject **sweepgen (lua_State *L, global_State *g, GCObject **p,
                            GCObject *limit, GCObject **pfirstold1) {
  static const lu_byte nextage[] = {
    G_SURVIVAL,  /* from G_NEW */
    G_OLD1,      /* from G_SURVIVAL */
    G_OLD1,      /* from G_OLD0 */
    G_OLD,       /* from G_OLD1 */
    G_OLD,       /* from G_OLD (do not change) */
    G_TOUCHED1,  /* from G_TOUCHED1 (do not change) */
    G_TOUCHED2   /* from G_TOUCHED2 (do not change) */
  };
  int white = luaC_white(g);
  GCObject *curr;
  while ((curr = *p) != limit) {
    if (iswhite(curr)) {  /* is 'curr' dead? */
      lua_assert(!isold(curr) && isdead(g, curr));
      *p = curr->next;  /* remove 'curr' from list */
      freeobj(L, curr);  /* erase 'curr' */
    }
    else {  /* correct mark and age */
      if (getage(curr) == G_NEW) {  /* new objects go back to white */
        int marked = curr->marked & ~maskgcbits;  /* erase GC bits */
        curr->marked = cast_byte(marked | G_SURVIVAL | white);
      }
      else {  /* all other objects will be old, and so keep their color */
        setage(curr, nextage[getage(curr)]);
        if (getage(curr) == G_OLD1 && *pfirstold1 == NULL)
          *pfirstold1 = curr;  /* first OLD1 object in the list */
      }
      p = &curr->next;  /* go to next element */
    }
  }
  return p;
}


/*
** Traverse a list making all its elements white and clearing their
** age. In incremental mode, all objects are 'new' all the time,
** except for fixed strings (which are always old).
*/
static void whitelist (global_State *g, GCObject *p) {
  int white = luaC_white(g);
  for (; p != NULL; p = p->next)
    p->marked = cast_byte((p->marked & ~maskgcbits) | white);
}


/*
** Correct a list of gray objects. Return pointer to where rest of the
** list should be linked.
** Because this correction is done after sweeping, young objects might
** be turned white and still be in the list. They are only removed.
** 'TOUCHED1' objects are advanced to 'TOUCHED2' and remain on the list;
** Non-white threads also remain on the list; 'TOUCHED2' objects become
** regular old; they and anything else are removed from the list.
*/
static GCObject **correctgraylist (GCObject **p) {
  GCObject *curr;
  while ((curr = *p) != NULL) {
    GCObject **next = getgclist(curr);
    if (iswhite(curr))
      goto remove;  /* remove all white objects */
    else if (getage(curr) == G_TOUCHED1) {  /* touched in this cycle? */
      lua_assert(isgray(curr));
      nw2black(curr);  /* make it black, for next barrier */
      changeage(curr, G_TOUCHED1, G_TOUCHED2);
      goto remain;  /* keep it in the list and go to next element */
    }
    else if (curr->tt == LUA_VTHREAD) {
      lua_assert(isgray(curr));
      goto remain;  /* keep non-white threads on the list */
    }
    else {  /* everything else is removed */
      lua_assert(isold(curr));  /* young objects should be white here */
      if (getage(curr) == G_TOUCHED2)  /* advance from TOUCHED2... */
        changeage(curr, G_TOUCHED2, G_OLD);  /* ... to OLD */
      nw2black(curr);  /* make object black (to be removed) */
      goto remove;
    }
    remove: *p = *next; continue;
    remain: p = next; continue;
  }
  return p;
}


/*
** Correct all gray lists, coalescing them into 'grayagain'.
*/
static void correctgraylists (global_State *g) {
  GCObject **list = correctgraylist(&g->grayagain);
  *list = g->weak; g->weak = NULL;
  list = correctgraylist(list);
  *list = g->allweak; g->allweak = NULL;
  list = correctgraylist(list);
  *list = g->ephemeron; g->ephemeron = NULL;
  correctgraylist(list);
}


/*
** Mark black 'OLD1' objects when starting a new young collection.
** Gray objects are already in some gray list, and so will be visited
** in the atomic step.
*/
static void markold (global_State *g, GCObject *from, GCObject *to) {
  GCObject *p;
  for (p = from; p != to; p = p->next) {
    if (getage(p) == G_OLD1) {
      lua_assert(!iswhite(p));
      changeage(p, G_OLD1, G_OLD);  /* now they are old */
      if (isblack(p))
        reallymarkobject(g, p);
    }
  }
}


/*
** Finish a young-generation collection.
*/
static void finishgencycle (lua_State *L, global_State *g) {
  correctgraylists(g);
  checkSizes(L, g);
  g->gcstate = GCSpropagate;  /* skip restart */
  if (!g->gcemergency)
    callallpendingfinalizers(L);
}


/*
** Does a young collection. First, mark 'OLD1' objects. Then does the
** atomic step. Then, sweep all lists and advance pointers. Finally,
** finish the collection.
*/
static void youngcollection (lua_State *L, global_State *g) {
  GCObject **psurvival;  /* to point to first non-dead survival object */
  GCObject *dummy;  /* dummy out parameter to 'sweepgen' */
  lua_assert(g->gcstate == GCSpropagate);
  if (g->firstold1) {  /* are there regular OLD1 objects? */
    markold(g, g->firstold1, g->reallyold);  /* mark them */
    g->firstold1 = NULL;  /* no more OLD1 objects (for now) */
  }
  markold(g, g->finobj, g->finobjrold);
  markold(g, g->tobefnz, NULL);
  atomic(L);

  /* sweep nursery and get a pointer to its last live element */
  g->gcstate = GCSswpallgc;
  psurvival = sweepgen(L, g, &g->allgc, g->survival, &g->firstold1);
  /* sweep 'survival' */
  sweepgen(L, g, psurvival, g->old1, &g->firstold1);
  g->reallyold = g->old1;
  g->old1 = *psurvival;  /* 'survival' survivals are old now */
  g->survival = g->allgc;  /* all news are survivals */

  /* repeat for 'finobj' lists */
  dummy = NULL;  /* no 'firstold1' optimization for 'finobj' lists */
  psurvival = sweepgen(L, g, &g->finobj, g->finobjsur, &dummy);
  /* sweep 'survival' */
  sweepgen(L, g, psurvival, g->finobjold1, &dummy);
  g->finobjrold = g->finobjold1;
  g->finobjold1 = *psurvival;  /* 'survival' survivals are old now */
  g->finobjsur = g->finobj;  /* all news are survivals */

  sweepgen(L, g, &g->tobefnz, NULL, &dummy);
  finishgencycle(L, g);
}


/*
** Clears all gray lists, sweeps objects, and prepare sublists to enter
** generational mode. The sweeps remove dead objects and turn all
** surviving objects to old. Threads go back to 'grayagain'; everything
** else is turned black (not in any gray list).
*/
static void atomic2gen (lua_State *L, global_State *g) {
  cleargraylists(g);
  /* sweep all elements making them old */
  g->gcstate = GCSswpallgc;
  sweep2old(L, &g->allgc);
  /* everything alive now is old */
  g->reallyold = g->old1 = g->survival = g->allgc;
  g->firstold1 = NULL;  /* there are no OLD1 objects anywhere */

  /* repeat for 'finobj' lists */
  sweep2old(L, &g->finobj);
  g->finobjrold = g->finobjold1 = g->finobjsur = g->finobj;

  sweep2old(L, &g->tobefnz);

  g->gckind = KGC_GEN;
  g->lastatomic = 0;
  g->GCestimate = gettotalbytes(g);  /* base for memory control */
  finishgencycle(L, g);
}


/*
** Set debt for the next minor collection, which will happen when
** memory grows 'genminormul'%.
*/
static void setminordebt (global_State *g) {
  luaE_setdebt(g, -(cast(l_mem, (gettotalbytes(g) / 100)) * g->genminormul));
}


/*
** Enter generational mode. Must go until the end of an atomic cycle
** to ensure that all objects are correctly marked and weak tables
** are cleared. Then, turn all objects into old and finishes the
** collection.
*/
static lu_mem entergen (lua_State *L, global_State *g) {
  lu_mem numobjs;
  luaC_runtilstate(L, bitmask(GCSpause));  /* prepare to start a new cycle */
  luaC_runtilstate(L, bitmask(GCSpropagate));  /* start new cycle */
  numobjs = atomic(L);  /* propagates all and then do the atomic stuff */
  atomic2gen(L, g);
  setminordebt(g);  /* set debt assuming next cycle will be minor */
  return numobjs;
}


/*
** Enter incremental mode. Turn all objects white, make all
** intermediate lists point to NULL (to avoid invalid pointers),
** and go to the pause state.
*/
static void enterinc (global_State *g) {
  whitelist(g, g->allgc);
  g->reallyold = g->old1 = g->survival = NULL;
  whitelist(g, g->finobj);
  whitelist(g, g->tobefnz);
  g->finobjrold = g->finobjold1 = g->finobjsur = NULL;
  g->gcstate = GCSpause;
  g->gckind = KGC_INC;
  g->lastatomic = 0;
}


/*
** Change collector mode to 'newmode'.
*/
void luaC_changemode (lua_State *L, int newmode) {
  global_State *g = G(L);
  if (newmode != g->gckind) {
    if (newmode == KGC_GEN)  /* entering generational mode? */
      entergen(L, g);
    else
      enterinc(g);  /* entering incremental mode */
  }
  g->lastatomic = 0;
}


/*
** Does a full collection in generational mode.
*/
static lu_mem fullgen (lua_State *L, global_State *g) {
  enterinc(g);
  return entergen(L, g);
}


/*
** Does a major collection after last collection was a "bad collection".
**
** When the program is building a big structure, it allocates lots of
** memory but generates very little garbage. In those scenarios,
** the generational mode just wastes time doing small collections, and
** major collections are frequently what we call a "bad collection", a
** collection that frees too few objects. To avoid the cost of switching
** between generational mode and the incremental mode needed for full
** (major) collections, the collector tries to stay in incremental mode
** after a bad collection, and to switch back to generational mode only
** after a "good" collection (one that traverses less than 9/8 objects
** of the previous one).
** The collector must choose whether to stay in incremental mode or to
** switch back to generational mode before sweeping. At this point, it
** does not know the real memory in use, so it cannot use memory to
** decide whether to return to generational mode. Instead, it uses the
** number of objects traversed (returned by 'atomic') as a proxy. The
** field 'g->lastatomic' keeps this count from the last collection.
** ('g->lastatomic != 0' also means that the last collection was bad.)
*/
static void stepgenfull (lua_State *L, global_State *g) {
  lu_mem newatomic;  /* count of traversed objects */
  lu_mem lastatomic = g->lastatomic;  /* count from last collection */
  if (g->gckind == KGC_GEN)  /* still in generational mode? */
    enterinc(g);  /* enter incremental mode */
  luaC_runtilstate(L, bitmask(GCSpropagate));  /* start new cycle */
  newatomic = atomic(L);  /* mark everybody */
  if (newatomic < lastatomic + (lastatomic >> 3)) {  /* good collection? */
    atomic2gen(L, g);  /* return to generational mode */
    setminordebt(g);
  }
  else {  /* another bad collection; stay in incremental mode */
    g->GCestimate = gettotalbytes(g);  /* first estimate */;
    entersweep(L);
    luaC_runtilstate(L, bitmask(GCSpause));  /* finish collection */
    setpause(g);
    g->lastatomic = newatomic;
  }
}


/*
** Does a generational "step".
** Usually, this means doing a minor collection and setting the debt to
** make another collection when memory grows 'genminormul'% larger.
**
** However, there are exceptions.  If memory grows 'genmajormul'%
** larger than it was at the end of the last major collection (kept
** in 'g->GCestimate'), the function does a major collection. At the
** end, it checks whether the major collection was able to free a
** decent amount of memory (at least half the growth in memory since
** previous major collection). If so, the collector keeps its state,
** and the next collection will probably be minor again. Otherwise,
** we have what we call a "bad collection". In that case, set the field
** 'g->lastatomic' to signal that fact, so that the next collection will
** go to 'stepgenfull'.
**
** 'GCdebt <= 0' means an explicit call to GC step with "size" zero;
** in that case, do a minor collection.
*/
static void genstep (lua_State *L, global_State *g) {
  if (g->lastatomic != 0)  /* last collection was a bad one? */
    stepgenfull(L, g);  /* do a full step */
  else {
    lu_mem majorbase = g->GCestimate;  /* memory after last major collection */
    lu_mem majorinc = (majorbase / 100) * getgcparam(g->genmajormul);
    if (g->GCdebt > 0 && gettotalbytes(g) > majorbase + majorinc) {
      lu_mem numobjs = fullgen(L, g);  /* do a major collection */
      if (gettotalbytes(g) < majorbase + (majorinc / 2)) {
        /* collected at least half of memory growth since last major
           collection; keep doing minor collections. */
        lua_assert(g->lastatomic == 0);
      }
      else {  /* bad collection */
        g->lastatomic = numobjs;  /* signal that last collection was bad */
        setpause(g);  /* do a long wait for next (major) collection */
      }
    }
    else {  /* regular case; do a minor collection */
      youngcollection(L, g);
      setminordebt(g);
      g->GCestimate = majorbase;  /* preserve base value */
    }
  }
  lua_assert(isdecGCmodegen(g));
}

/* }====================================================== */


/*
** {======================================================
** GC control
** =======================================================
*/


/*
** Enter first sweep phase.
** The call to 'sweeptolive' makes the pointer point to an object
** inside the list (instead of to the header), so that the real sweep do
** not need to skip objects created between "now" and the start of the
** real sweep.
问题背景：
在清除阶段开始后，用户代码可能继续运行，创建新对象（这些对象会被添加到 allgc 链表头部）。
如果 sweepgc 直接指向链表头 allgc，后续清除操作会错误地跳过这些新对象。
解决方案：
sweeptolive 返回链表中第一个 待清除对象 的地址（而非链表头），确保后续的 sweep 操作从正确位置开始。
*/
static void entersweep (lua_State *L) {
  global_State *g = G(L);
  g->gcstate = GCSswpallgc;
  lua_assert(g->sweepgc == NULL);
  g->sweepgc = sweeptolive(L, &g->allgc);
}


/*
** Delete all objects in list 'p' until (but not including) object
** 'limit'.
*/
static void deletelist (lua_State *L, GCObject *p, GCObject *limit) {
  while (p != limit) {
    GCObject *next = p->next;
    freeobj(L, p);
    p = next;
  }
}


/*
** Call all finalizers of the objects in the given Lua state, and
** then free all objects, except for the main thread.
*/
void luaC_freeallobjects (lua_State *L) {
  global_State *g = G(L);
  g->gcstp = GCSTPCLS;  /* no extra finalizers after here */
  luaC_changemode(L, KGC_INC);
  separatetobefnz(g, 1);  /* separate all objects with finalizers */
  lua_assert(g->finobj == NULL);
  callallpendingfinalizers(L);
  deletelist(L, g->allgc, obj2gco(g->mainthread));
  lua_assert(g->finobj == NULL);  /* no new finalizers */
  deletelist(L, g->fixedgc, NULL);  /* collect fixed objects */
  lua_assert(g->strt.nuse == 0);
}


static lu_mem atomic (lua_State *L) {
  global_State *g = G(L);
  lu_mem work = 0;                  // 记录工作量（标记的对象数量）
  GCObject *origweak, *origall;
  GCObject *grayagain = g->grayagain;  /* 保存原始的 grayagain 列表 */
  g->grayagain = NULL;              /*重置 grayagain 列表*/
  lua_assert(g->ephemeron == NULL && g->weak == NULL);
  lua_assert(!iswhite(g->mainthread));
  g->gcstate = GCSatomic;              // 切换到原子阶段(只有这个函数里会赋值这个阶段)
  markobject(g, L);                     /* mark running thread  标记当前运行的线程（L） */
  /* registry and global metatables may be changed by API */
  markvalue(g, &g->l_registry);
  markmt(g);                            /* mark global metatables 标记全局元表*/
  work += propagateall(g);              /* empties 'gray' list 清空gray链表*/
  /* remark occasional upvalues of (maybe) dead threads 处理可能被遗漏的 upvalue（因线程死亡导致 upvalue 未被标记）*/
  work += remarkupvals(g);               // 重新标记可能死亡的线程的 upvalue
  work += propagateall(g);              /* propagate changes */
  g->gray = grayagain;                     // 恢复 grayagain 列表
  work += propagateall(g);                  /* traverse 'grayagain' list */
  convergeephemerons(g);                  // 处理 ephemeron 表的标记 若键未被标记，则删除键值对；否则保留。
  
  /* at this point, all strongly accessible objects are marked. */
  /* Clear values from weak tables, before checking finalizers 
    清理弱表中值为弱引用的无效条目（值未被标记时删除
    weak 和 allweak：分别对应 __mode = "v" 和 __mode = "kv" 的表*/
  clearbyvalues(g, g->weak, NULL);
  clearbyvalues(g, g->allweak, NULL);
  origweak = g->weak; origall = g->allweak;
  separatetobefnz(g, 0);  /* separate objects to be finalized finobj--->tbbefnz// 分离待终结对象 */
  work += markbeingfnz(g);  /* mark objects that will be finalized 标记待终结对象*/
  work += propagateall(g);  /* remark, to propagate 'resurrection' 传播复活对象的标记*/
  convergeephemerons(g);          // 再次处理 ephemeron 表

  /* at this point, all resurrected objects are marked. */
  /* remove dead objects from weak tables */
  clearbykeys(g, g->ephemeron);  /* clear keys from all ephemeron tables */
  clearbykeys(g, g->allweak);  /* clear keys from all 'allweak' tables */

  /* clear values from resurrected weak tables */
  clearbyvalues(g, g->weak, origweak);
  clearbyvalues(g, g->allweak, origall);

  luaS_clearcache(g);              // 清理字符串缓存
  g->currentwhite = cast_byte(otherwhite(g));  /* flip current white */
  lua_assert(g->gray == NULL);
  return work;  /* estimate of slots marked by 'atomic' */
}


/*
在增量式 GC 中逐步释放未被标记的对象内存
L：当前 Lua 状态。
g：全局状态（含 GC 控制字段）。
nextstate：当前清扫阶段完成后，GC 状态机应跳转的下一个状态。
nextlist：下一个待清扫的对象链表（用于多阶段清扫）
返回值：本次实际清扫的对象数量（用于计算 GC 工作量）
*/
static int sweepstep (lua_State *L, global_State *g,
                      int nextstate, GCObject **nextlist) {
  if (g->sweepgc) { // 当前待清扫链表非空
    l_mem olddebt = g->GCdebt;
    int count;
    //每次100个元素
    g->sweepgc = sweeplist(L, g->sweepgc, GCSWEEPMAX, &count);
    g->GCestimate += g->GCdebt - olddebt;  /*// 更新内存估算*/
    return count;
  }
  else {  /* enter next state */
    //当前链表已清扫完成进入下一阶段
    g->gcstate = nextstate;
    g->sweepgc = nextlist;
    return 0;  /* no work done */
  }
}

/*这段代码是 Lua 垃圾回收（GC）系统中增量式垃圾回收（Incremental GC）的核心函数 singlestep，
用于单步执行垃圾回收的一个阶段。它的设计目标是将 GC 工作拆分为小步骤，避免长时间阻塞主程序*/
static lu_mem singlestep (lua_State *L) {
  global_State *g = G(L);
  lu_mem work;
  lua_assert(!g->gcstopem);  /* collector is not reentrant */
  g->gcstopem = 1;  /* 禁止紧急collection */
  //状态机驱动
  switch (g->gcstate) {
    case GCSpause: {
      restartcollection(g); //// 重置 GC 计数器和标记重新开收集
      g->gcstate = GCSpropagate;// 进入标记传播阶段
      work = 1;             // 返回最小工作量（表示已推进状态）
      break;
    }
    case GCSpropagate: {
      if (g->gray == NULL) {  /* 灰色对象链表为空？ */
        g->gcstate = GCSenteratomic;  /* // 进入原子标记阶段 */
        work = 0;
      }
      else
        work = propagatemark(g);  /* // 处理一个灰色对象（就是标记其引用的对象）同时灰->黑 */
      break;
    }
    case GCSenteratomic: {
      work = atomic(L);  /* // 完成剩余标记（不可中断） */
      entersweep(L);        // 进入清扫阶段
      g->GCestimate = gettotalbytes(g);  /* first estimate // 更新内存估算*/;
      break;
    }
    //// 分步清扫不同对象类型：
    case GCSswpallgc: {  /* sweep "regular" objects // 普通对象*/
      work = sweepstep(L, g, GCSswpfinobj, &g->finobj);    //清扫allgc链表
      break;
    }
    case GCSswpfinobj: {  /* sweep objects with finalizers 带终结器的对象 存放待 __gc 的对象*/
      work = sweepstep(L, g, GCSswptobefnz, &g->tobefnz);  //清扫finobj链表 (有__gc元方法的表或者userdata)
      break;
    }
    case GCSswptobefnz: {  /* sweep objects to be finalized 待终结对象存放  已调 __gc 的对象*/
      work = sweepstep(L, g, GCSswpend, NULL);           //清扫tobefnz链表 ()
      break;
    }
    case GCSswpend: {  /* finish sweeps 清扫完成*/
      checkSizes(L, g);           // 检查内存块大小
      g->gcstate = GCScallfin;    // 进入终结器调用阶段
      work = 0;
      break;
    }
    //调用gc的元方法
    case GCScallfin: {  /* call remaining finalizers */
      if (g->tobefnz && !g->gcemergency) {
        g->gcstopem = 0;  /* ok collections during finalizers call _gc*/
        work = runafewfinalizers(L, GCFINMAX) * GCFINALIZECOST;
      }
      else {  /* emergency mode or no more finalizers */
        g->gcstate = GCSpause;  /* finish collection 结束 GC 周期*/
        work = 0;
      }
      break;
    }
    default: lua_assert(0); return 0;
  }
  g->gcstopem = 0;
  return work;
}


/*
** advances the garbage collector until it reaches a state allowed
** by 'statemask'
*/
void luaC_runtilstate (lua_State *L, int statesmask) {
  global_State *g = G(L);
  while (!testbit(statesmask, g->gcstate))
    singlestep(L);
}



/*
** Performs a basic incremental step. The debt and step size are
** converted from bytes to "units of work"; then the function loops
** running single steps until adding that many units of work or
** finishing a cycle (pause state). Finally, it sets the debt that
** controls when next step will be performed.
执行一个基本的增量步骤。债务（debt）和步长（step size）会从字节转换为“工作单位”；
然后，该函数循环运行单一步骤，直到完成相应的工作单位或结束一个周期（暂停状态）。
最后，它会设置控制下一次步骤执行时机的债务值

** 这个函数 incstep 是 Lua 垃圾回收（GC）系统的一部分，负责执行**增量式垃圾回收（incremental GC）**的单个步骤。
** 它的核心逻辑是：根据当前 GC 的负载情况，计算需要执行的工作量，并逐步执行回收操作，直到完成目标或进入暂停状态
*/
static void incstep (lua_State *L, global_State *g) {

  //GC 步进乘数（避免除零，强制至少为 1）
  int stepmul = (getgcparam(g->gcstepmul) | 1);  /* avoid division by 0 */
  //GCdebt是待回收的字节数,通过 WORK2MEM 转换为工作单元，再乘以 stepmul 调整步进速度
  //待处理的回收单元数?
  l_mem debt = (g->GCdebt / WORK2MEM) * stepmul; //待处理的 GC 债务（单位：工作单元TValue）
  //计算 stepsize（单步最大工作量）
  l_mem stepsize = (g->gcstepsize <= log2maxs(l_mem))
                 ? ((cast(l_mem, 1) << g->gcstepsize) / WORK2MEM) * stepmul
                 : MAX_LMEM;  /* overflow; keep maximum value */
                 
  do {  /* repeat until pause or enough "credit" (negative debt) */
    //执行一次 GC 子阶段（如标记、清扫等），返回实际完成的工作量。
    lu_mem work = singlestep(L);  /* 执行一步 * */
    debt -= work; //剩余要回收的单元数量
  } while (debt > -stepsize && g->gcstate != GCSpause);//仍有待处理的工作 GC 未进入暂停状态

  if (g->gcstate == GCSpause)
    setpause(g);  /* pause until next cycle */
  else {
    debt = (debt / stepmul) * WORK2MEM;  /* convert 'work units' to bytes */
    luaE_setdebt(g, debt);
  }
}

/*
** Performs a basic GC step if collector is running. (If collector is
** not running, set a reasonable debt to avoid it being called at
** every single check.)
*/
void luaC_step (lua_State *L) {
  global_State *g = G(L);
  if (!gcrunning(g))  /* not running? */
    luaE_setdebt(g, -2000);
  else {
    if(isdecGCmodegen(g))
      genstep(L, g);
    else
      incstep(L, g);
  }
}


/*
** Perform a full collection in incremental mode.
** Before running the collection, check 'keepinvariant'; if it is true,
** there may be some objects marked as black, so the collector has
** to sweep all objects to turn them back to white (as white has not
** changed, nothing will be collected).
** invariant 是不变式,就是黑色对象不会直引用白色对象
** 白色：未扫描，可能回收。
** 灰色：已扫描但子对象未扫描。
** 黑色：已完全扫描，存活对象
*/
static void fullinc (lua_State *L, global_State *g) {
  //如果存在黑色对象，强制进入清扫阶段（Sweep），
  //将所有对象重置为白色（未标记状态），确保后续标记阶段能正确处理所有对象
  if (keepinvariant(g))  /* black objects? */
    entersweep(L); /* sweep everything to turn them back to white */
  
  /* finish any pending sweep phase to start a new cycle */
  /*推动 GC 状态机运行，直到达到目标状态 GCSpause（暂停状态）
  目的是完成当前未完成的 GC 周期（如果有），避免与新周期冲突。*/
  
  luaC_runtilstate(L, bitmask(GCSpause));
  /*推动 GC 状态机到 GCScallfin 状态：
  依次经历标记（Mark）、清扫（Sweep）等阶段，并执行终结器（__gc 元方法）。
  这是增量 GC 的核心逻辑：分阶段完成标记和回收*/
  
  luaC_runtilstate(L, bitmask(GCScallfin));  /* run up to finalizers */
  /* estimate must be correct after a full GC cycle */
  //估算值 是否等于实际值
  lua_assert(g->GCestimate == gettotalbytes(g));
  /*再次推动到 GCSpause：确保 GC 周期完全结束。
  setpause(g): 根据当前内存使用情况，设置下一次触发 GC 的暂停阈值（g->gcpause）*/
  luaC_runtilstate(L, bitmask(GCSpause));  /* finish collection */

  setpause(g); //根据当前内存使用情况，设置下一次触发 GC 的暂停阈值（g->gcpause
}

//http://cwqqq.com/2023/01/30/lua54_generational_collection
/*
** Performs a full GC cycle; if 'isemergency', set a flag to avoid
** some operations which could change the interpreter state in some
** unexpected ways (running finalizers and shrinking some structures).
** 若 isemergency 为真，GC 会跳过一些以不可预期的方式改变解释器状态的高风险操作，
** 比如如执行对象的 __gc 元方法、收缩内存等
*/
void luaC_fullgc (lua_State *L, int isemergency) {
  global_State *g = G(L);
  lua_assert(!g->gcemergency);
  g->gcemergency = isemergency;  /* set flag */
  if (g->gckind == KGC_INC)
    fullinc(L, g); //增量gc
  else 
    fullgen(L, g); //分代gc
  g->gcemergency = 0;
}

/* }====================================================== */

void ShowGcobjaddress

void ShowGcList(global_State *g, const char *title, GCObject *list) {
  printf("%s:\n", title);
  for (GCObject *o = list; o != NULL; o = o->next) {
    printf("  %p: ", o);
    if (iswhite(o)) printf("white ");
    else if (isgray(o)) printf("gray ");
    else if (isblack(o)) printf("black ");
    else printf("unknown ");
    printf("-->");
    //printf("age=%d\n", getage(o));
  }
}
void showgraylist(global_State *g,const char *title, GCObject *list) {
  printf("%s:\n", title);
  for (GCObject *o = list; o != NULL; ) {
    printf("gray  %p: ", o);
    printf("--->");
    o = getgclist1(o);
  }
}

void ShowGcState(global_State *g) {
  ShowGcList(L, "allgc", g->allgc);
  ShowGcList(L, "grayagain", g->grayagain);
  ShowGcList(L, "weak", g->weak);
  ShowGcList(L, "allweak", g->allweak);
  ShowGcList(L, "ephemeron", g->ephemeron);
}
