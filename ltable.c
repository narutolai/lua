/*
** $Id: ltable.c $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/

#define ltable_c
#define LUA_CORE

#include "lprefix.h"


/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest 'n' such that
** more than half the slots between 1 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the 'original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
   表（也称为数组、对象或哈希表）的实现。
   表将其元素存储在两个部分：数组部分和哈希部分
   所有非负整数键都可能被存入数组部分。
   数组的实际大小是满足以下条件的最大 n：在 1 到 n 的槽位中，超过一半的槽位已被占用
    哈希部分采用了链式散列表与 Brent 变体的混合策略
    这些表的一个主要不变式是：如果一个元素不在其主位置（即其哈希值计算出的原始位置），
    那么与该位置冲突的元素一定位于它自己的主位置。
    因此，即使负载因子达到 100%，性能依然保持良好
    注释：
  仅对连续整数键（如 1, 2, 3...）分配数组存储，提升访问速度。
  数组大小动态调整，确保至少一半槽位被占用（避免空间浪费）。

  使用 链式散列 + Brent 变体：冲突元素若不能放在主位置，则必须占据冲突者的主位置（避免“二次聚集”问题）。
  即使表完全填满（负载因子 100%），仍能保持高效查找。

  如果新元素 A 的主位置被元素 B 占用，
  且元素 B 本身不在自己的主位置，那么将 B 踢到其他位置，让 A 占据该主位置

  如果主位置被流浪元素占用，会把流浪元素踢走
*/

#include <math.h>
#include <limits.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


/*
** MAXABITS is the largest integer such that MAXASIZE fits in an
** unsigned int.
*/
#define MAXABITS	cast_int(sizeof(int) * CHAR_BIT - 1)


/*
** MAXASIZE is the maximum size of the array part. It is the minimum
** between 2^MAXABITS and the maximum size that, measured in bytes,
** fits in a 'size_t'.
*/
#define MAXASIZE	luaM_limitN(1u << MAXABITS, TValue)

/*
** MAXHBITS is the largest integer such that 2^MAXHBITS fits in a
** signed int.
*/
#define MAXHBITS	(MAXABITS - 1)


/*
** MAXHSIZE is the maximum size of the hash part. It is the minimum
** between 2^MAXHBITS and the maximum size such that, measured in bytes,
** it fits in a 'size_t'.
*/
#define MAXHSIZE	luaM_limitN(1u << MAXHBITS, Node)


/*
** When the original hash value is good, hashing by a power of 2
** avoids the cost of '%'.
*/
#define hashpow2(t,n)		(gnode(t, lmod((n), sizenode(t))))

/*
** for other types, it is better to avoid modulo by power of 2, as
** they can have many 2 factors.
*/
#define hashmod(t,n)	(gnode(t, ((n) % ((sizenode(t)-1)|1))))


#define hashstr(t,str)		hashpow2(t, (str)->hash)
#define hashboolean(t,p)	hashpow2(t, p)


#define hashpointer(t,p)	hashmod(t, point2uint(p))


#define dummynode		(&dummynode_)

static const Node dummynode_ = {
  {{NULL}, LUA_VEMPTY,  /* value's value and type */
   LUA_VNIL, 0, {NULL}}  /* key type, next, and key value */
};


static const TValue absentkey = {ABSTKEYCONSTANT};


/*
** Hash for integers. To allow a good hash, use the remainder operator
** ('%'). If integer fits as a non-negative int, compute an int
** remainder, which is faster. Otherwise, use an unsigned-integer
** remainder, which uses all bits and ensures a non-negative result.
** 整数哈希方法，使用取余操作符, 如果整数是个非负数,直接取余.
** 否则 使用无符号取余,确保结果是非负数 就是如果是个负数,直接强转成无符号整形
*/
static Node *hashint (const Table *t, lua_Integer i) {
  lua_Unsigned ui = l_castS2U(i);
  if (ui <= cast_uint(INT_MAX))
    return hashmod(t, cast_int(ui));
  else
    return hashmod(t, ui);
}


/*
** Hash for floating-point numbers.
** The main computation should be just
**     n = frexp(n, &i); return (n * INT_MAX) + i
** but there are some numerical subtleties.
** In a two-complement representation, INT_MAX does not has an exact
** representation as a float, but INT_MIN does; because the absolute
** value of 'frexp' is smaller than 1 (unless 'n' is inf/NaN), the
** absolute value of the product 'frexp * -INT_MIN' is smaller or equal
** to INT_MAX. Next, the use of 'unsigned int' avoids overflows when
** adding 'i'; the use of '~u' (instead of '-u') avoids problems with
** INT_MIN.

** 
**  3  011
**  2  010
**  1  001
**  0  000 
** -1  111 
** -2  110
** -3  101
** -4  100
*/
#if !defined(l_hashfloat)
static int l_hashfloat (lua_Number n) {
  int i;
  lua_Integer ni;
  n = l_mathop(frexp)(n, &i) * -cast_num(INT_MIN);
  if (!lua_numbertointeger(n, &ni)) {  /* is 'n' inf/-inf/NaN? */
    lua_assert(luai_numisnan(n) || l_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
  }
  else {  /* normal case */
    unsigned int u = cast_uint(i) + cast_uint(ni);
    return cast_int(u <= cast_uint(INT_MAX) ? u : ~u);
  }
}
#endif


/*
** returns the 'main' position of an element in a table (that is,
** the index of its hash value).
** 返回元素的main position
*/
static Node *mainpositionTV (const Table *t, const TValue *key) {
  switch (ttypetag(key)) {
    case LUA_VNUMINT: {
      lua_Integer i = ivalue(key);
      return hashint(t, i);
    }
    case LUA_VNUMFLT: {
      lua_Number n = fltvalue(key);
      return hashmod(t, l_hashfloat(n));
    }
    case LUA_VSHRSTR: {
      TString *ts = tsvalue(key);
      return hashstr(t, ts);
    }
    case LUA_VLNGSTR: {
      TString *ts = tsvalue(key);
      return hashpow2(t, luaS_hashlongstr(ts));
    }
    case LUA_VFALSE:
      return hashboolean(t, 0);
    case LUA_VTRUE:
      return hashboolean(t, 1);
    case LUA_VLIGHTUSERDATA: {
      void *p = pvalue(key);
      return hashpointer(t, p);
    }
    case LUA_VLCF: {
      lua_CFunction f = fvalue(key);
      return hashpointer(t, f);
    }
    default: {
      GCObject *o = gcvalue(key);
      return hashpointer(t, o);
    }
  }
}


l_sinline Node *mainpositionfromnode (const Table *t, Node *nd) {
  TValue key;
  getnodekey(cast(lua_State *, NULL), &key, nd);
  return mainpositionTV(t, &key);
}


/*
** Check whether key 'k1' is equal to the key in node 'n2'. This
** equality is raw, so there are no metamethods. Floats with integer
** values have been normalized, so integers cannot be equal to
** floats. It is assumed that 'eqshrstr' is simply pointer equality, so
** that short strings are handled in the default case.
** A true 'deadok' means to accept dead keys as equal to their original
** values. All dead keys are compared in the default case, by pointer
** identity. (Only collectable objects can produce dead keys.) Note that
** dead long strings are also compared by identity.
** Once a key is dead, its corresponding value may be collected, and
** then another value can be created with the same address. If this
** other value is given to 'next', 'equalkey' will signal a false
** positive. In a regular traversal, this situation should never happen,
** as all keys given to 'next' came from the table itself, and therefore
** could not have been collected. Outside a regular traversal, we
** have garbage in, garbage out. What is relevant is that this false
** positive does not break anything.  (In particular, 'next' will return
** some other valid item on the table or nil.)
*/
static int equalkey (const TValue *k1, const Node *n2, int deadok) {
  if ((rawtt(k1) != keytt(n2)) &&  /* not the same variants? */
       !(deadok && keyisdead(n2) && iscollectable(k1)))
   return 0;  /* cannot be same key */
  switch (keytt(n2)) {
    case LUA_VNIL: case LUA_VFALSE: case LUA_VTRUE:
      return 1;
    case LUA_VNUMINT:
      return (ivalue(k1) == keyival(n2));
    case LUA_VNUMFLT:
      return luai_numeq(fltvalue(k1), fltvalueraw(keyval(n2)));
    case LUA_VLIGHTUSERDATA:
      return pvalue(k1) == pvalueraw(keyval(n2));
    case LUA_VLCF:
      return fvalue(k1) == fvalueraw(keyval(n2));
    case ctb(LUA_VLNGSTR):
      return luaS_eqlngstr(tsvalue(k1), keystrval(n2));
    default:
      return gcvalue(k1) == gcvalueraw(keyval(n2));
  }
}


/*
** True if value of 'alimit' is equal to the real size of the array
** part of table 't'. (Otherwise, the array part must be larger than
** 'alimit'.)
*/
#define limitequalsasize(t)	(isrealasize(t) || ispow2((t)->alimit))


/*
** Returns the real size of the 'array' array
** 数组部分的大小一定得是2的幂次
** 如果 alimit 已经是 真实大小（即 limitequalsasize(t) 为真），则直接返回 alimit。
** 否则，计算 不小于 alimit 的最小 2 的幂次方，并返回该值
** 优化目标：减少内存碎片，提高哈希表访问速度，支持动态扩容
*/
LUAI_FUNC unsigned int luaH_realasize (const Table *t) {
  if (limitequalsasize(t))
    return t->alimit;  /* this is the size 数组的预估大小,不是实际大小*/
  else {
    unsigned int size = t->alimit;
    /* compute the smallest power of 2 not smaller than 'n' */
    size |= (size >> 1);
    size |= (size >> 2);
    size |= (size >> 4);
    size |= (size >> 8);
#if (UINT_MAX >> 14) > 3  /* unsigned int has more than 16 bits */
    size |= (size >> 16);
#if (UINT_MAX >> 30) > 3
    size |= (size >> 32);  /* unsigned int has more than 32 bits */
#endif
#endif
    size++;
    lua_assert(ispow2(size) && size/2 < t->alimit && t->alimit < size);
    return size;
  }
}


/*
** Check whether real size of the array is a power of 2.
** (If it is not, 'alimit' cannot be changed to any other value
** without changing the real size.)
*/
static int ispow2realasize (const Table *t) {
  return (!isrealasize(t) || ispow2(t->alimit));
}


static unsigned int setlimittosize (Table *t) {
  t->alimit = luaH_realasize(t);
  setrealasize(t);
  return t->alimit;
}


#define limitasasize(t)	check_exp(isrealasize(t), t->alimit)



/*
** "Generic" get version. (Not that generic: not valid for integers,
** which may be in array part, nor for floats with integral values.)
** See explanation about 'deadok' in function 'equalkey'.
** 通用键查找函数（非整数键或非整数值的浮点数键）
** 参数 `deadok` 解释见 `equalkey` 函数 
*/
static const TValue *getgeneric (Table *t, const TValue *key, int deadok) {
  Node *n = mainpositionTV(t, key);
  for (;;) {  /* check whether 'key' is somewhere in the chain  参数 deadok 控制是否允许检查已被标记为 dead 的节点（*/
    if (equalkey(key, n, deadok))
      return gval(n);  /* that's it */
    else {
      int nx = gnext(n);
      if (nx == 0)
        return &absentkey;  /* not found */
      n += nx;
    }
  }
}


/*
** returns the index for 'k' if 'k' is an appropriate key to live in
** the array part of a table, 0 otherwise.
*/
static unsigned int arrayindex (lua_Integer k) {
  if (l_castS2U(k) - 1u < MAXASIZE)  /* 'k' in [1, MAXASIZE]? */
    return cast_uint(k);  /* 'key' is an appropriate array index */
  else
    return 0;
}


/*
** returns the index of a 'key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signaled by 0.
** 返回表遍历时的键索引号：先遍历数组部分，再遍历哈希部分。
** 遍历开始的信号为 0。
为 next 函数或 pairs 迭代器提供键的 连续逻辑索引，实现表的顺序遍历。
*/
static unsigned int findindex (lua_State *L, Table *t, TValue *key,
                               unsigned int asize) {
  unsigned int i;
  if (ttisnil(key)) return 0;  /* first iteration */
  i = ttisinteger(key) ? arrayindex(ivalue(key)) : 0;
  if (i - 1u < asize)  /* is 'key' inside array part? */
    return i;  /* yes; that's the index */
  else {
    const TValue *n = getgeneric(t, key, 1);
    if (l_unlikely(isabstkey(n)))
      luaG_runerror(L, "invalid key to 'next'");  /* key not found */
    i = cast_int(nodefromval(n) - gnode(t, 0));  /* key index in hash table */
    /* hash elements are numbered after array ones */
    return (i + 1) + asize;
  }
}


int luaH_next (lua_State *L, Table *t, StkId key) {
  unsigned int asize = luaH_realasize(t);
  unsigned int i = findindex(L, t, s2v(key), asize);  /* find original key */
  for (; i < asize; i++) {  /* try first array part */
    if (!isempty(&t->array[i])) {  /* a non-empty entry? */
      setivalue(s2v(key), i + 1);
      setobj2s(L, key + 1, &t->array[i]);
      return 1;
    }
  }
  for (i -= asize; cast_int(i) < sizenode(t); i++) {  /* hash part */
    if (!isempty(gval(gnode(t, i)))) {  /* a non-empty entry? */
      Node *n = gnode(t, i);
      getnodekey(L, s2v(key), n);
      setobj2s(L, key + 1, gval(n));
      return 1;
    }
  }
  return 0;  /* no more elements */
}


static void freehash (lua_State *L, Table *t) {
  if (!isdummy(t))
    luaM_freearray(L, t->node, cast_sizet(sizenode(t)));
}


/*
** {=============================================================
** Rehash
** ==============================================================
*/

/*
** Compute the optimal(最适宜的) size for the array part of table 't'. 'nums' is a
** "count array" where 'nums[i]' is the number of integers in the table
** between 2^(i - 1) + 1 and 2^i. 'pna' enters with the total number of
** integer keys in the table and leaves with the number of keys that
** will go to the array part; return the optimal size.  (The condition
** 'twotoi > 0' in the for loop stops the loop if 'twotoi' overflows.)
** 计算表 t 的数组部分（array part）的最优容量。
** 其中 nums[i] 表示表中array部分里键介于 2^(i - 1) + 1 到 2^i 范围内的整数键的数量。
** *pna：
** 输入：表中所有整数键的总数。
** 输出：实际分配到数组部分的键的数量
** 通过遍历 nums 数组，逐步尝试 2^i 作为候选容量，直到满足以下条件：
** 当前累计的键数量 (a) 超过候选容量的一半（a > twotoi/2）。
** 确保数组部分的空间利用率 > 50%，避免内存浪费。
*/
static unsigned int computesizes (unsigned int nums[], unsigned int *pna) {
  int i;
  unsigned int twotoi;  /* 2^i (candidate for optimal size) */
  unsigned int a = 0;  /* number of elements smaller than 2^i */
  unsigned int na = 0;  /* number of elements to go to array part */
  unsigned int optimal = 0;  /* optimal size for array part */
  /* loop while keys can fill more than half of total size */
  for (i = 0, twotoi = 1;
       twotoi > 0 && *pna > twotoi / 2;
       i++, twotoi *= 2) {
    a += nums[i];
    if (a > twotoi/2) {  /* more than half elements present? */
      optimal = twotoi;  /* optimal size (till now) */
      na = a;  /* all elements up to 'optimal' will go to array part */
    }
  }
  lua_assert((optimal == 0 || optimal / 2 < na) && na <= optimal);
  *pna = na;
  return optimal;
}


static int countint (lua_Integer key, unsigned int *nums) {
  unsigned int k = arrayindex(key);
  if (k != 0) {  /* is 'key' an appropriate array index? */
    nums[luaO_ceillog2(k)]++;  /* count as such */
    return 1;
  }
  else
    return 0;
}


/*
** Count keys in array part of table 't': Fill 'nums[i]' with
** number of keys that will go into corresponding slice and return
** total number of non-nil keys.
*/
static unsigned int numusearray (const Table *t, unsigned int *nums) {
  int lg;
  unsigned int ttlg;  /* 2^lg */
  unsigned int ause = 0;  /* summation(总和) of 'nums' */
  unsigned int i = 1;  /* count to traverse all array keys */
  unsigned int asize = limitasasize(t);  /* real array size */
  /* traverse each slice  遍历每一个切片*/
  for (lg = 0, ttlg = 1; lg <= MAXABITS; lg++, ttlg *= 2) {
    unsigned int lc = 0;  /* counter */
    unsigned int lim = ttlg;
    if (lim > asize) {
      lim = asize;  /* adjust upper limit */
      if (i > lim)
        break;  /* no more elements to count */
    }

    /* 统计在范围 (2^(lg - 1), 2^lg] 内的元素*/
    for (; i <= lim; i++) {
      if (!isempty(&t->array[i-1]))
        lc++;
    }
    nums[lg] += lc;
    ause += lc;
  }
  return ause;
}


static int numusehash (const Table *t, unsigned int *nums, unsigned int *pna) {
  int totaluse = 0;  /* total number of elements */
  int ause = 0;  /* elements added to 'nums' (can go to array part) */
  int i = sizenode(t);
  while (i--) {
    Node *n = &t->node[i];
    if (!isempty(gval(n))) {
      if (keyisinteger(n)) //如果是整数就看是否落在
        ause += countint(keyival(n), nums);
      totaluse++;
    }
  }
  *pna += ause;
  return totaluse;
}


/*
** Creates an array for the hash part of a table with the given
** size, or reuses the dummy node if size is zero.
** The computation for size overflow is in two steps: the first
** comparison ensures that the shift in the second one does not
** overflow.
*/
static void setnodevector (lua_State *L, Table *t, unsigned int size) {
  if (size == 0) {  /* no elements to hash part? */
    t->node = cast(Node *, dummynode);  /* use common 'dummynode' */
    t->lsizenode = 0;
    t->lastfree = NULL;  /* signal that it is using dummy node */
  }
  else {
    //比如 size 传进来是7 那么lsize 就是3 ，因为2^3 = 8 > 7，8是第一个大于7的2的幂
    int i;
    int lsize = luaO_ceillog2(size); // 计算不小于 size 的最小2的幂次,计算幂
    if (lsize > MAXHBITS || (1u << lsize) > MAXHSIZE)
      luaG_runerror(L, "table overflow");

    //所以这里的size就变成8了
    size = twoto(lsize);//2^lsize 次方
    //最终分配了8个node的大小
    t->node = luaM_newvector(L, size, Node);
    for (i = 0; i < cast_int(size); i++) {
      Node *n = gnode(t, i);
      gnext(n) = 0;         //链表指针置空
      setnilkey(n);         //键设为 nil
      setempty(gval(n));    //值设为空（nil）
    }
    // lsizenode就是等于3
    t->lsizenode = cast_byte(lsize);
    t->lastfree = gnode(t, size);  /* all positions are free */
  }
}


/*
** (Re)insert all elements from the hash part of 'ot' into table 't'.
*/
static void reinsert (lua_State *L, Table *ot, Table *t) {
  int j;
  int size = sizenode(ot);
  for (j = 0; j < size; j++) {
    Node *old = gnode(ot, j);
    if (!isempty(gval(old))) {
      /* doesn't need barrier/invalidate cache, as entry was
         already present in the table */
      TValue k;
      getnodekey(L, &k, old);
      luaH_set(L, t, &k, gval(old));
    }
  }
}


/*
** Exchange the hash part of 't1' and 't2'.
*/
static void exchangehashpart (Table *t1, Table *t2) {
  lu_byte lsizenode = t1->lsizenode;
  Node *node = t1->node;
  Node *lastfree = t1->lastfree;

  t1->lsizenode = t2->lsizenode;
  t1->node = t2->node;
  t1->lastfree = t2->lastfree;
  
  t2->lsizenode = lsizenode;
  t2->node = node;
  t2->lastfree = lastfree;
}


/*
** Resize table 't' for the new given sizes. Both allocations (for
** the hash part and for the array part) can fail, which creates some
** subtleties. If the first allocation, for the hash part, fails, an
** error is raised and that is it. Otherwise, it copies the elements from
** the shrinking part of the array (if it is shrinking) into the new
** hash. Then it reallocates the array part.  If that fails, the table
** is in its original state; the function frees the new hash part and then
** raises the allocation error. Otherwise, it sets the new hash part
** into the table, initializes the new part of the array (if any) with
** nils and reinserts the elements of the old hash back into the new
** parts of the table.
** newasize: 数组部分的新容量
** nhsize:   哈希部分的新容量
*/
void luaH_resize (lua_State *L, Table *t, unsigned int newasize,
                                          unsigned int nhsize) {
  unsigned int i;
  Table newt;  /* 临时表，用于保存新哈希部分 */
  unsigned int oldasize = setlimittosize(t);
  TValue *newarray;
  /* create new hash part with appropriate size into 'newt' */
  setnodevector(L, &newt, nhsize);//为新hash分配内存
  
  //若数组需要缩容，将即将被裁减的数组元素迁移至新哈希中
  if (newasize < oldasize) {  /* will array shrink? */
    t->alimit = newasize;  /* // 临时修改数组边界 */
    exchangehashpart(t, &newt);  /* 交换新旧哈希 */
    /* 将即将消失的数组元素迁移到新哈希 */
    for (i = newasize; i < oldasize; i++) {
      if (!isempty(&t->array[i]))
        luaH_setint(L, t, i + 1, &t->array[i]);//键为 i+1（Lua 数组从 1 开始）
    }
    t->alimit = oldasize;  /* restore current size... */
    exchangehashpart(t, &newt);  /* and hash (in case of errors) */
  }

  /* allocate new array */
  newarray = luaM_reallocvector(L, t->array, oldasize, newasize, TValue);
  if (l_unlikely(newarray == NULL && newasize > 0)) {  /* allocation failed? */
    freehash(L, &newt);  /*  释放临时哈希 */
    luaM_error(L);  /* 抛出内存错误（表 `t` 未被修改） */
  }

  /* allocation ok; initialize new part of the array */
  exchangehashpart(t, &newt);  /* 't' has the new hash ('newt' has the old) */
  t->array = newarray;  /* set new array part */
  t->alimit = newasize;
  
  // 初始化扩容新增的数组槽位
  for (i = oldasize; i < newasize; i++)  /* clear new slice of the array */
     setempty(&t->array[i]);
  
  //迁移旧hash数据
  /* re-insert elements from old hash part into new parts */
  reinsert(L, &newt, t);  /* 'newt' now has the old hash 将旧哈希数据插入新表结构*/
  freehash(L, &newt);  /* free old hash part  释放旧哈希内存*/
}


void luaH_resizearray (lua_State *L, Table *t, unsigned int nasize) {
  int nsize = allocsizenode(t);
  luaH_resize(L, t, nasize, nsize);
}

/*
** 重hash函数
** nums[i] = number of keys 'k' where 2^(i - 1) < k <= 2^i
** 其实统计table的hash部分和array部分的 整数 key 整理出nums[i]数组
*/
static void rehash (lua_State *L, Table *t, const TValue *ek) {
  unsigned int asize;   /* array部分优化后的尺寸 */
  unsigned int na;      /* array部分的key数量 */
  unsigned int nums[MAXABITS + 1];
  int i;
  int totaluse;
  for (i = 0; i <= MAXABITS; i++) nums[i] = 0;  /* reset counts */
  setlimittosize(t);
  na = numusearray(t, nums);  /* count keys in array part */
  totaluse = na;  /* all those keys are integer keys */
  totaluse += numusehash(t, nums, &na);  /* count keys in hash part */
  /* count extra key */
  if (ttisinteger(ek))
    na += countint(ivalue(ek), nums);
  totaluse++;
  /* compute new size for array part */
  asize = computesizes(nums, &na);
  /* resize the table to new computed sizes */
  luaH_resize(L, t, asize, totaluse - na);
}



/*
** }=============================================================
*/


Table *luaH_new (lua_State *L) {
  GCObject *o = luaC_newobj(L, LUA_VTABLE, sizeof(Table));
  Table *t = gco2t(o);
  t->metatable = NULL;
  t->flags = cast_byte(maskflags);  /* table has no metamethod fields */
  t->array = NULL;
  t->alimit = 0;
  setnodevector(L, t, 0);
  return t;
}


void luaH_free (lua_State *L, Table *t) {
  freehash(L, t);
  luaM_freearray(L, t->array, luaH_realasize(t));
  luaM_free(L, t);
}


static Node *getfreepos (Table *t) {
  if (!isdummy(t)) {
    while (t->lastfree > t->node) {
      t->lastfree--;
      if (keyisnil(t->lastfree))
        return t->lastfree;
    }
  }
  return NULL;  /* could not find a free place */
}



/*
** inserts a new key into a hash table; first, check whether key's main
** position is free. If not, check whether colliding node is in its main
** position or not: if it is not, move colliding node to an empty place and
** put new key in its main position; otherwise (colliding node is in its main
** position), new key goes to an empty position.
** 向哈希表插入新键：
** 1. 首先检查键的主位置是否空闲；
** 2. 若被占用，检查冲突节点是否在其自身的主位置：
**    - 若否，将冲突节点移至空闲位置，新键占据主位置；
**    - 若是，新键放入空闲位置。
*/
void luaH_newkey (lua_State *L, Table *t, const TValue *key, TValue *value) {
  Node *mp;
  TValue aux;
  if (l_unlikely(ttisnil(key)))   //键不可为nil
    luaG_runerror(L, "table index is nil");
  else if (ttisfloat(key)) {   // 处理浮点数键
    lua_Number f = fltvalue(key);
    lua_Integer k;
    if (luaV_flttointeger(f, &k, F2Ieq)) {  /*键可转为整数？ */
      setivalue(&aux, k); // 转为整数值
      key = &aux;  /* // 作为整数插入 */
    }
    else if (l_unlikely(luai_numisnan(f)))
      luaG_runerror(L, "table index is NaN");
  }
  if (ttisnil(value))
    return;               /* 值不可为nil  */
  
  /* 获取键的主位置节点 */
  mp = mainpositionTV(t, key);
  //主位置已被占用或表为虚拟表（dummy）
  if (!isempty(gval(mp)) || isdummy(t)) {  /* main position is taken? */
    Node *othern;
    Node *f = getfreepos(t);  /* 获取空闲节点位置 */
    if (f == NULL) {  /* 无空闲位置：触发扩容并重新插入 */
      rehash(L, t, key);  /* grow table */
      /* whatever called 'newkey' takes care of TM cache */
      luaH_set(L, t, key, value);  /* insert key into grown table */
      return;
    }
    lua_assert(!isdummy(t));
    // 冲突节点的实际主位置
    othern = mainpositionfromnode(t, mp);
    /* 情况1：冲突节点不在其主位置 */
    if (othern != mp) {  

      while (othern + gnext(othern) != mp)  /* 找到冲突节点的前驱节点*/
        othern += gnext(othern);

      gnext(othern) = cast_int(f - othern);  /* 重链前驱指向空闲位置f */
      *f = *mp;  /* copy colliding node into free pos. (mp->next also goes)复制冲突节点到空闲位置（包括 next 指针） */
      
       /* 维护 next 指针 */
      if (gnext(mp) != 0) {
        gnext(f) += cast_int(mp - f);  /* correct 'next'修正空闲位置的 next */
        gnext(mp) = 0;  /* now 'mp' is free 原主位置置为空闲*/
      }

      setempty(gval(mp));       // 清空原主位置的值
    }
    else {  
      
      /*  情况2：冲突节点在其自身主位置*/   
      /*  新键放入空闲位置*/
      if (gnext(mp) != 0)
        gnext(f) = cast_int((mp + gnext(mp)) - f);  /* chain new position */
      else lua_assert(gnext(f) == 0);

      gnext(mp) = cast_int(f - mp);     // 主位置节点指向新位置
      mp = f;                           // 更新操作节点为空闲位置
    }
  }
  /* 插入键值对 */
  setnodekey(L, mp, key);               // 写入键
  luaC_barrierback(L, obj2gco(t), key); // 触发GC写屏障
  lua_assert(isempty(gval(mp)));        // 断言值位为空
  setobj2t(L, gval(mp), value);         // 写入值
}


/*
** Search function for integers. If integer is inside 'alimit', get it
** directly from the array part. Otherwise, if 'alimit' is not equal to
** the real size of the array, key still can be in the array part. In
** this case, try to avoid a call to 'luaH_realasize' when key is just
** one more than the limit (so that it can be incremented without
** changing the real size of the array).
** 如果整数键< alimit 则直接从数组获取他, 否则如果 alimit 不等于数组的实际尺寸,
** key 依旧可能在array part 在这种情况下,试着避免调用luaH_realasize 当key
** 获取一个整数键
*/
const TValue *luaH_getint (Table *t, lua_Integer key) {
  if (l_castS2U(key) - 1u < t->alimit)  /* 'key' in [1, t->alimit]? */
    return &t->array[key - 1];
  else if (!limitequalsasize(t) &&  /* key still may be in the array part? */
           (l_castS2U(key) == t->alimit + 1 ||
            l_castS2U(key) - 1u < luaH_realasize(t))) {
    t->alimit = cast_uint(key);  /* probably '#t' is here now */
    return &t->array[key - 1];
  }
  else {
    //计算hash值获取Node 的位置 然后沿着冲突链表往下寻找
    Node *n = hashint(t, key);
    for (;;) {  /* check whether 'key' is somewhere in the chain */
      if (keyisinteger(n) && keyival(n) == key)
        return gval(n);  /* that's it */
      else {
        int nx = gnext(n);
        if (nx == 0) break;
        n += nx;
      }
    }
    return &absentkey;
  }
}


/*
** search function for short strings
*/
const TValue *luaH_getshortstr (Table *t, TString *key) {
  Node *n = hashstr(t, key);
  lua_assert(key->tt == LUA_VSHRSTR);
  for (;;) {  /* check whether 'key' is somewhere in the chain */
    if (keyisshrstr(n) && eqshrstr(keystrval(n), key))
      return gval(n);  /* that's it */
    else {
      int nx = gnext(n);
      if (nx == 0)
        return &absentkey;  /* not found */
      n += nx;
    }
  }
}


const TValue *luaH_getstr (Table *t, TString *key) {
  if (key->tt == LUA_VSHRSTR)
    return luaH_getshortstr(t, key);
  else {  /* for long strings, use generic case */
    TValue ko;
    setsvalue(cast(lua_State *, NULL), &ko, key);
    return getgeneric(t, &ko, 0);
  }
}


/*
** main search function
*/
const TValue *luaH_get (Table *t, const TValue *key) {
  switch (ttypetag(key)) {
    case LUA_VSHRSTR: return luaH_getshortstr(t, tsvalue(key));
    case LUA_VNUMINT: return luaH_getint(t, ivalue(key));
    case LUA_VNIL: return &absentkey;
    case LUA_VNUMFLT: {
      lua_Integer k;
      if (luaV_flttointeger(fltvalue(key), &k, F2Ieq)) /* integral index? */
        return luaH_getint(t, k);  /* use specialized version */
      /* else... */
    }  /* FALLTHROUGH */
    default:
      return getgeneric(t, key, 0);
  }
}


/*
** Finish a raw "set table" operation, where 'slot' is where the value
** should have been (the result of a previous "get table").
** Beware: when using this function you probably need to check a GC
** barrier and invalidate the TM cache.
*/
void luaH_finishset (lua_State *L, Table *t, const TValue *key,
                                   const TValue *slot, TValue *value) {
  if (isabstkey(slot))
    luaH_newkey(L, t, key, value);
  else
    setobj2t(L, cast(TValue *, slot), value);
}


/*
** beware: when using this function you probably need to check a GC
** barrier and invalidate the TM cache.
*/
void luaH_set (lua_State *L, Table *t, const TValue *key, TValue *value) {
  const TValue *slot = luaH_get(t, key);
  luaH_finishset(L, t, key, slot, value);
}


void luaH_setint (lua_State *L, Table *t, lua_Integer key, TValue *value) {
  const TValue *p = luaH_getint(t, key);
  if (isabstkey(p)) {
    TValue k;
    setivalue(&k, key);
    luaH_newkey(L, t, &k, value);
  }
  else
    setobj2t(L, cast(TValue *, p), value);
}


/*
** Try to find a boundary in the hash part of table 't'. From the
** caller, we know that 'j' is zero or present and that 'j + 1' is
** present. We want to find a larger key that is absent from the
** table, so that we can do a binary search between the two keys to
** find a boundary. We keep doubling 'j' until we get an absent index.
** If the doubling would overflow, we try LUA_MAXINTEGER. If it is
** absent, we are ready for the binary search. ('j', being max integer,
** is larger or equal to 'i', but it cannot be equal because it is
** absent while 'i' is present; so 'j > i'.) Otherwise, 'j' is a
** boundary. ('j + 1' cannot be a present integer key because it is
** not a valid integer in Lua.)
*/
static lua_Unsigned hash_search (Table *t, lua_Unsigned j) {
  lua_Unsigned i;
  if (j == 0) j++;  /* the caller ensures 'j + 1' is present */
  do {
    i = j;  /* 'i' is a present index */
    if (j <= l_castS2U(LUA_MAXINTEGER) / 2)
      j *= 2;
    else {
      j = LUA_MAXINTEGER;
      if (isempty(luaH_getint(t, j)))  /* t[j] not present? */
        break;  /* 'j' now is an absent index */
      else  /* weird case */
        return j;  /* well, max integer is a boundary... */
    }
  } while (!isempty(luaH_getint(t, j)));  /* repeat until an absent t[j] */
  /* i < j  &&  t[i] present  &&  t[j] absent */
  while (j - i > 1u) {  /* do a binary search between them */
    lua_Unsigned m = (i + j) / 2;
    if (isempty(luaH_getint(t, m))) j = m;
    else i = m;
  }
  return i;
}


static unsigned int binsearch (const TValue *array, unsigned int i,
                                                    unsigned int j) {
  while (j - i > 1u) {  /* binary search */
    unsigned int m = (i + j) / 2;
    if (isempty(&array[m - 1])) j = m;
    else i = m;
  }
  return i;
}


/*
** 指表中某个键（如 t[i]）关联了一个非 nil 的值。
  示例：t = {10, 20, nil, 40} 中，t[1]、t[2]、t[4] 是 "present"。
  Absent（不存在）

  指表中某个键未关联值或显式赋值为 nil。
  示例：上述 t[3] 是 "absent"。

** Try to find a boundary in table 't'. (A 'boundary' is an integer index
** such that t[i] is present and t[i+1] is absent, or 0 if t[1] is absent
** and 'maxinteger' if t[maxinteger] is present.)
** (In the next explanation, we use Lua indices, that is, with base 1.
** The code itself uses base 0 when indexing the array part of the table.)
** The code starts with 'limit = t->alimit', a position in the array
** part that may be a boundary.
** 尝试在table t中找到一个边界值(边界就是比如 如果t[i]存在,t[i+1]不存在,那么t[i]就是边界 ，如果t[1]没有
** 那么0就是边界，如果t[maxinteger]有那么 t[maxinteger]就是边界值 )。在下面的解释中，我们使用lua索引，也就是从1开始
** 当然代码中索引array是从0开始的，代码从limit 开始搜索边界值。

** (1) If 't[limit]' is empty, there must be a boundary before it.
** As a common case (e.g., after 't[#t]=nil'), check whether 'limit-1'
** is present. If so, it is a boundary. Otherwise, do a binary search
** between 0 and limit to find a boundary. In both cases, try to
** use this boundary as the new 'alimit', as a hint for the next call.
** (1) 如果 t[limit] 是空的，那么边界值肯定在其之前
** 在0和limit之间找到边界，试着使用边界作为新的alimit作为下一次调用


** (2) If 't[limit]' is not empty and the array has more elements
** after 'limit', try to find a boundary there. Again, try first
** the special case (which should be quite frequent) where 'limit+1'
** is empty, so that 'limit' is a boundary. Otherwise, check the
** last element of the array part. If it is empty, there must be a
** boundary between the old limit (present) and the last element
** (absent), which is found with a binary search. (This boundary always
** can be a new limit.)
** 如果t[limit] 不是空的 然后 limit之后还有一些元素 ，那么要在limit后面找边界值。
** 先看特殊情况，究竟是 t[limit+1] 不存在 那么 t[limit]就是边界值，否则检测数组的
** 最后一个元素，如果它是空的，那么在limit和　最后一个元素之间一定存在边界，我们使用
** 二分搜索方法来进行搜索
** 
** (3) The last case is when there are no elements in the array part
** (limit == 0) or its last element (the new limit) is present.
** In this case, must check the hash part. If there is no hash part
** or 'limit+1' is absent, 'limit' is a boundary.  Otherwise, call
** 'hash_search' to find a boundary in the hash part of the table.
** (In those cases, the boundary is not inside the array part, and
** therefore cannot be used as a new limit.)
** 最后一个情况是数组部分没有元素或者它的最后一个元素是nil 
** 这种情况下，要检查hash部分，如果hash部分没有
** 这种情况下 边界值就不在array部分,因此也不能被作位新的limit来进行使用
** 输入：一个 Lua 表 t（包含数组部分和哈希部分）。
** 输出：返回表的 “边界”索引（即 t[i] 存在但 t[i+1] 不存在）。
*/
lua_Unsigned luaH_getn (Table *t) {
  unsigned int limit = t->alimit;

  if (limit > 0 && isempty(&t->array[limit - 1])) {  /* (1)? */
    /* there must be a boundary before 'limit' */
    if (limit >= 2 && !isempty(&t->array[limit - 2])) {
      /* 'limit - 1' is a b oundary; can it be a new limit? */
      if (ispow2realasize(t) && !ispow2(limit - 1)) {
        t->alimit = limit - 1;
        setnorealasize(t);  /* now 'alimit' is not the real size */
      }
      return limit - 1;
    }
    else {  /* must search for a boundary in [0, limit] */
      unsigned int boundary = binsearch(t->array, 0, limit);
      /* can this boundary represent the real size of the array? */
      if (ispow2realasize(t) && boundary > luaH_realasize(t) / 2) {
        t->alimit = boundary;  /* use it as the new limit */
        setnorealasize(t);
      }
      return boundary;
    }
  }

  /* 'limit' is zero or present in table */
  if (!limitequalsasize(t)) {  /* (2)? */
    /* 'limit' > 0 and array has more elements after 'limit' */
    if (isempty(&t->array[limit]))  /* 'limit + 1' is empty? */
      return limit;  /* this is the boundary */
    /* else, try last element in the array */
    limit = luaH_realasize(t);
    if (isempty(&t->array[limit - 1])) {  /* empty? */
      /* there must be a boundary in the array after old limit,
         and it must be a valid new limit */
      unsigned int boundary = binsearch(t->array, t->alimit, limit);
      t->alimit = boundary;
      return boundary;
    }
    /* else, new limit is present in the table; check the hash part */
  }

  /* (3) 'limit' is the last element and either is zero or present in table */
  lua_assert(limit == luaH_realasize(t) &&
             (limit == 0 || !isempty(&t->array[limit - 1])));
  if (isdummy(t) || isempty(luaH_getint(t, cast(lua_Integer, limit + 1))))
    return limit;  /* 'limit + 1' is absent */
  else  /* 'limit + 1' is also present */
    return hash_search(t, limit);
}



#if defined(LUA_DEBUG)

/* export these functions for the test library */

Node *luaH_mainposition (const Table *t, const TValue *key) {
  return mainpositionTV(t, key);
}

#endif
