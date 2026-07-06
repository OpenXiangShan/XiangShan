# 主表地址复用与违例场景窗口生成方案

本文记录 mem_ut 主表生成阶段用于提高地址相关、RAW / memory violation 场景概率的地址复用方案。
本方案从 `active_sequence_success_count_refactor_plan.md` 中拆出，专门解决主表生成阶段候选扫描、
窗口有效性和参数命名重复问题。

## 1. 背景与目标

主表生成时需要有能力构造类似以下场景：

```text
前面某条 load/store 访问地址 A；
当前 transaction 也访问地址 A；
通过受控概率提高地址相关或违例场景出现概率。
```

旧的全表扫描方式存在两个问题：

```text
1. 每生成一个 uid 都扫完整 main table，main_trans_num 大时接近 O(N^2)。
2. 从完整历史候选里随机选，可能选到距离当前 uid 很远的 transaction，
   该 transaction 对应 LSQ entry 可能已经出队，无法形成有效相关。
```

目标：

```text
1. 不扫完整主表。
2. 候选必须落在 uid 距离窗口内。
3. 支持通过 plus 权重控制是否启用违例构造和四类构造方向。
4. 避免新增与现有地址复用参数重复的方向权重。
5. 第一版逻辑简单、可实现、可验证。
```

## 2. 数据结构

使用两个 queue，而不是一个混合 queue：

```systemverilog
memblock_uid_t recent_load_uid_q[$];
memblock_uid_t recent_store_uid_q[$];
```

原因：

```text
四类地址复用场景最终都只需要按“参考类型”访问 load 或 store 候选；
使用两个 queue 后，LOAD_AFTER_LOAD / STORE_AFTER_LOAD 直接访问 load queue，
LOAD_AFTER_STORE / STORE_AFTER_STORE 直接访问 store queue；
如果使用混合 queue，每次选择都要遍历区分 load/store，逻辑更复杂，性能也更差。
```

queue 中只保存 `uid`，实际 transaction 内容仍通过主表获取：

```text
ref_tr = data.get_main_transaction(ref_uid)
```

## 3. 窗口语义

窗口不是 queue size，而是 uid 距离：

```text
cur_uid - ref_uid <= addr_ref_window
```

例如：

```text
addr_ref_window = 16
store uid = 10
cur_uid = 201
```

即使 `recent_store_uid_q` 里只有这一个 store，它也必须被淘汰，因为：

```text
201 - 10 > 16
```

该 store 距离当前 uid 太远，可能已经出队，不能作为有效地址相关参考。

## 4. Queue 维护函数

### 4.1 按 uid 距离淘汰过期项

由于主表按 uid 递增生成，queue 里的 uid 也按递增顺序 push，所以只需要从队头淘汰。

文字伪代码：

```text
prune_recent_q(q, cur_uid, addr_ref_window):
  while q 非空:
    if cur_uid - q.front() > addr_ref_window:
      q.pop_front()
    else:
      break
```

调用时机：

```text
每次生成当前 uid 后、选择参考前，对 recent_load_uid_q 和 recent_store_uid_q 都执行 prune。
```

### 4.2 从 queue 随机取一个 uid

随机取候选时是否删除由场景决定：

```text
LOAD_AFTER_LOAD / STORE_AFTER_STORE：
  同类型同地址容易重复滚雪球，取出后从 queue 删除。

LOAD_AFTER_STORE / STORE_AFTER_LOAD：
  跨类型场景更利于制造 load/store 相关，取出后不删除，允许继续复用。
```

文字伪代码：

```text
random_pick_uid(q, output ref_uid, delete_after_pick):
  if q 为空:
    return 0

  idx = random(0, q.size()-1)
  ref_uid = q[idx]
  if delete_after_pick:
    q.delete(idx)
  return 1
```

说明：

```text
即使 delete_after_pick=1，删除的也只是候选 queue 中的轻量 uid，不删除主表 transaction。
主表仍保留 ref_uid 对应 transaction，用于追踪和 debug。
```

### 4.3 当前项重新入队

当前 transaction 完成类型修正、地址复用和主表写入后，再根据最终类型入队：

```text
if current_tr 是 load:
  recent_load_uid_q.push_back(cur_uid)

if current_tr 是 store:
  recent_store_uid_q.push_back(cur_uid)
```

注意：入队使用的是当前项的最终类型，不是初始随机类型。

## 5. 违例场景随机控制

### 5.1 Enable 随机

新增一个违例构造 enable 随机，使用 plus 权重控制：

```text
MEMBLOCK_ADDR_REUSE_EN_1_WT
MEMBLOCK_ADDR_REUSE_EN_0_WT
```

语义：

```text
每生成一个主表 transaction，先随机一次 addr_reuse_en。

如果 addr_reuse_en == 0：
  不做地址复用 directed 修正；
  当前 transaction 保持原随机结果；
  按最终类型入 recent queue。

如果 addr_reuse_en == 1：
  再随机一次地址复用场景枚举；
  本次只允许选择并执行一种场景。

默认建议 MEMBLOCK_ADDR_REUSE_EN_1_WT=1、MEMBLOCK_ADDR_REUSE_EN_0_WT=19，
即每条 transaction 约 5% 概率尝试地址复用，默认低比例开启，不明显破坏原始随机分布。
```

### 5.2 场景枚举

新增枚举：

```systemverilog
typedef enum int unsigned {
    MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE,
    MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD,
    MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD,
    MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE
} memblock_addr_reuse_kind_e;
```

命名解释：

```text
AFTER 表示当前 transaction 位于参考 transaction 之后。
例如 LOAD_AFTER_STORE 表示“当前 load 复用前面某个 store 的地址”。
相比 *_BEF_* 命名，AFTER 更符合主表按 uid 递增生成时的视角。

LOAD_AFTER_STORE：
  使用 store 队列作为参考队列。
  如果 store 队列非空：当前项修正为 load，并复用历史 store 地址。
  如果 store 队列为空：当前项修正为 store，不做地址复制。

LOAD_AFTER_LOAD：
  使用 load 队列作为参考队列。
  如果 load 候选队列非空：当前项修正为 load，并复用历史 load 地址。
  如果 load 候选队列为空：当前项修正为 load，不做地址复制。

STORE_AFTER_LOAD：
  使用 load 队列作为参考队列。
  如果 load 队列非空：当前项修正为 store，并复用历史 load 地址。
  如果 load 队列为空：当前项修正为 load，不做地址复制。

STORE_AFTER_STORE：
  使用 store 队列作为参考队列。
  如果 store 候选队列非空：当前项修正为 store，并复用历史 store 地址。
  如果 store 候选队列为空：当前项修正为 store，不做地址复制。
```

这里的枚举执行规则是：

```text
枚举前半部分表示候选非空时当前项希望修正成的类型；
枚举后半部分表示参考队列类型，也表示候选为空时当前项 fallback 类型。
```

例如 `LOAD_AFTER_STORE`：

```text
STORE -> 查 store 队列；
store 队列非空 -> 当前项修正为 LOAD，并复用历史 store 地址；
store 队列为空 -> 当前项修正为 STORE，不做地址复制。
```

### 5.3 场景枚举权重

每种枚举取值由 plus 权重控制：

```text
MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT
MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT
MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT
MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT
```

随机流程：

```text
如果 addr_reuse_en 随机命中 1：
  按上述四个权重随机 memblock_addr_reuse_kind_e；
  本次只执行随机到的一种场景；
  不在同一个 transaction 中连续尝试多种场景。
```

## 6. 参数命名与已有参数关系

源码已有：

```text
MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT
MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT
```

这两个参数原本表达 load/store 方向的地址复用权重。为避免同一功能出现两套方向权重，本方案不再新增：

```text
MEMBLOCK_STORE_FROM_LOAD_ADDR_REF_WEIGHT
MEMBLOCK_LOAD_FROM_STORE_ADDR_REF_WEIGHT
```

最终处理策略：

```text
删除旧参数：
  MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT
  MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT

删除旧后处理入口：
  inject_ls_addr_reuse_by_fuoptype()
  select_load_addr_ref()
  select_prior_store_addr_ref()
```

原因：

```text
1. 新四类枚举已经覆盖旧的两个方向。
2. 继续保留旧参数会让同一功能出现两套入口。
3. 旧入口是在主表全部生成后再扫表注入，无法自然维护 uid 距离窗口。
4. 删除旧参数后，地址复用控制只通过 enable 权重、四类枚举权重和窗口参数完成。
```

同步范围：

```text
mem_ut/ver/ut/memblock/env/plus.sv：
  删除旧 MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT / MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT；
  新增本方案的 enable、枚举权重、窗口和 ROB start 参数。

mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv：
  删除旧 getter / cached field；
  新增新参数的合法化、clamp、warning 和 getter。

mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg：
  删除旧 plus key；
  添加新 plus key 的默认值。

mem_ut/ver/ut/memblock/seq/plus_cfg/*.cfg：
  如果现有 testcase cfg 使用旧 key，必须同步替换成新 key；
  不保留旧 key 兼容入口。

相关设计文档和规则文档：
  删除“LD_TO_ST / ST_TO_LD 后处理注入”表述；
  改为“主表生成过程中基于 recent queue 和 uid 窗口做地址复用”。
```

实现完成后建议检查：

```bash
rg -n "MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT|MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT|inject_ls_addr_reuse_by_fuoptype|select_load_addr_ref|select_prior_store_addr_ref" \
  mem_ut/ver/ut/memblock AI_DOC
```

除迁移说明或历史方案存档外，不应再有真实实现依赖旧入口。

## 7. 窗口参数

窗口大小使用独立 plus 参数控制：

```text
MEMBLOCK_ADDR_REF_WINDOW_FIXED
MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT
MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT
MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT
```

选择规则：

```text
如果 MEMBLOCK_ADDR_REF_WINDOW_FIXED > 0：
  addr_ref_window = fixed

否则：
  按 small / medium / large 三个权重选择一个区间；
  再在选中区间内随机具体窗口大小。
```

第一版窗口上限建议：

```text
addr_ref_window <= min(LQ_SIZE, SQ_SIZE)
```

如果后续需要更精细：

```text
参考 load 时窗口上限可用 LQ_SIZE；
参考 store 时窗口上限可用 SQ_SIZE。
```

无论使用哪种上限，最终都必须 clamp：

```text
1 <= addr_ref_window <= 合法 LSQ 窗口上限
```

第一版窗口区间建议：

```text
SMALL:
  [1, max(1, 窗口上限 / 4)]

MEDIUM:
  [max(1, 窗口上限 / 4), max(1, 窗口上限 / 2)]

LARGE:
  [max(1, 窗口上限 / 2), 窗口上限]
```

如果区间上下界因为窗口上限太小而交叉，需要在 `seq_csr_common` 中 clamp 到合法范围，并打印
`uvm_warning`。权重全部为 0 时也需要给 warning，并回退到默认窗口。

默认建议：

```text
MEMBLOCK_ADDR_REF_WINDOW_FIXED = 0
MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT = 0
MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT = 0
MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT = 1
```

这样第一版默认窗口接近 LSQ 容量上限，更容易覆盖真实 LSQ 内仍可能同时存在的 load/store。

## 8. 主表生成流程

文字伪代码：

```text
build_random_main_table:
  recent_load_uid_q  = {}
  recent_store_uid_q = {}
  addr_ref_window = choose_addr_ref_window()

  for cur_uid = 0 到 main_trans_num-1:
    randomize current_tr 基础字段和初始类型

    prune_recent_q(recent_load_uid_q,  cur_uid, addr_ref_window)
    prune_recent_q(recent_store_uid_q, cur_uid, addr_ref_window)

    addr_reuse_en = weighted_random(EN_1_WT, EN_0_WT)

    if addr_reuse_en:
      reuse_kind = weighted_random(
        LOAD_AFTER_STORE_WT,
        LOAD_AFTER_LOAD_WT,
        STORE_AFTER_LOAD_WT,
        STORE_AFTER_STORE_WT
      )

      case reuse_kind:
        LOAD_AFTER_STORE:
          if random_pick_uid(recent_store_uid_q, ref_uid, delete_after_pick=0):
            current_tr 类型修正为 load
            current_tr 地址字段复制 ref_uid 的地址字段
          else:
            current_tr 类型修正为 store

        LOAD_AFTER_LOAD:
          if random_pick_uid(recent_load_uid_q, ref_uid, delete_after_pick=1):
            current_tr 类型修正为 load
            current_tr 地址字段复制 ref_uid 的地址字段
          else:
            current_tr 类型修正为 load

        STORE_AFTER_LOAD:
          if random_pick_uid(recent_load_uid_q, ref_uid, delete_after_pick=0):
            current_tr 类型修正为 store
            current_tr 地址字段复制 ref_uid 的地址字段
          else:
            current_tr 类型修正为 load

        STORE_AFTER_STORE:
          if random_pick_uid(recent_store_uid_q, ref_uid, delete_after_pick=1):
            current_tr 类型修正为 store
            current_tr 地址字段复制 ref_uid 的地址字段
          else:
            current_tr 类型修正为 store

    如果对应候选 queue 为空：
      按枚举后半部分类型修正当前项；
      不做地址复制；
      当前 transaction 继续作为新候选入队。

    data.set_main_transaction(cur_uid, current_tr)

    if current_tr 最终是 load:
      recent_load_uid_q.push_back(cur_uid)

    if current_tr 最终是 store:
      recent_store_uid_q.push_back(cur_uid)
```

这里需要注意：

```text
1. 地址复用在主表边生成边完成，不再等整张主表生成后统一扫描。
2. 每个 uid 只随机一次 addr_reuse_en；如果命中 enable，只随机并执行一种 reuse_kind。
3. 候选 queue 为空时，也要按枚举后半部分修正当前项类型，使当前项成为后续候选。
4. 当前项入队必须发生在 data.set_main_transaction() 之后，且使用修正后的最终类型。
5. random_pick_uid() 删除候选只影响 recent queue，不影响主表中 ref_uid 的记录。
```

## 9. ROB 起始值随机化方案

当前主表随机生成时，ROB 从固定 0 开始：

```text
uid=0   -> robIdx flag=0 value=0
uid=1   -> robIdx flag=0 value=1
...
uid=351 -> robIdx flag=0 value=351
uid=352 -> robIdx flag=1 value=0
```

后续需要支持 uid=0 的 ROB 起始值随机化。随机后，后续 uid 仍从该起始 ROB 连续递增，并使用
`rob_order_util::rob_advance()` 按 `MEMBLOCK_ROB_SIZE` 回绕和翻转 flag。

目标：

```text
默认行为仍从 robIdx value=0, flag=0 开始；
可通过 plus 指定固定起始值；
也可通过 dist 权重从 0、中间范围、快越界范围中随机起始值；
后续 uid 仍保持 ROB 顺序连续递增。
```

推荐参数：

```text
MEMBLOCK_ROB_START_FIXED_EN
MEMBLOCK_ROB_START_FIXED_VALUE
MEMBLOCK_ROB_START_ZERO_WT
MEMBLOCK_ROB_START_MID_WT
MEMBLOCK_ROB_START_NEAR_WRAP_WT
```

默认值建议：

```text
MEMBLOCK_ROB_START_FIXED_EN = 1
MEMBLOCK_ROB_START_FIXED_VALUE = 0
MEMBLOCK_ROB_START_ZERO_WT = 1
MEMBLOCK_ROB_START_MID_WT = 0
MEMBLOCK_ROB_START_NEAR_WRAP_WT = 0
```

这样默认仍完全等价于当前从 0 开始。

随机规则：

```text
if MEMBLOCK_ROB_START_FIXED_EN:
  rob_start = clamp(MEMBLOCK_ROB_START_FIXED_VALUE, 0, MEMBLOCK_ROB_SIZE-1)
else:
  按 ZERO / MID / NEAR_WRAP 三个权重选择范围：

  ZERO:
    rob_start = 0

  MID:
    rob_start 在中间范围随机，例如 [MEMBLOCK_ROB_SIZE/4, MEMBLOCK_ROB_SIZE*3/4]

  NEAR_WRAP:
    rob_start 在快越界范围随机，例如 [MEMBLOCK_ROB_SIZE-16, MEMBLOCK_ROB_SIZE-1]

  最终 rob_start 仍 clamp 到 [0, MEMBLOCK_ROB_SIZE-1]
```

说明：

```text
ZERO 是精确值 0，不是一个范围。
MID 和 NEAR_WRAP 是为了覆盖 ROB value 非 0 和接近回绕边界的场景。
默认通过 MEMBLOCK_ROB_START_FIXED_EN=1、MEMBLOCK_ROB_START_FIXED_VALUE=0 保持现有行为。
当需要随机起点时，testcase cfg 或 plus_arg 显式把 MEMBLOCK_ROB_START_FIXED_EN 设为 0，
再配置 ZERO / MID / NEAR_WRAP 权重。
```

主表生成伪代码：

```text
rob_key.flag  = 0
rob_key.value = choose_rob_start_value()

for uid = 0 到 main_trans_num-1:
  randomize_main_transaction(tr, uid, rob_key)
  data.set_main_transaction(uid, tr)
  rob_key = rob_order_util::rob_advance(rob_key, 1)
```

约束：

```text
只随机 uid=0 的起始 robIdx value；
后续 uid 的 robIdx 必须连续递增，不能每条 transaction 单独随机；
robIdx flag 默认从 0 开始，后续由 rob_advance 在 value 回绕时翻转；
如果后续需要随机初始 flag，应单独加参数，第一版不做。
```

## 10. 与 `post_randomize()` 的关系

用户期望该逻辑可以放在主表 transaction 约束和 `post_randomize()` 后完成，使主表生成函数每次随机 transaction 后即可得到修正后的结果。

建议边界：

```text
main_control_transaction::post_randomize():
  只做单个 transaction 内部字段合法性修正。
  例如根据最终 load/store 类型修正 fuType/fuOpType/rfWen/fpWen、LSQ flow 等派生字段。

主表生成 helper：
  负责 recent_load_uid_q / recent_store_uid_q。
  负责选择 ref_uid。
  负责把 ref_uid 的地址字段复制到 current_tr。
  负责调用 transaction 的合法性修正函数。
```

原因：

```text
post_randomize() 不适合直接维护跨 transaction 的 recent queue，
因为 queue 是主表生成过程的上下文状态，不属于单个 transaction 自身状态。
```

因此第一版建议实现为：

```text
build_random_main_table()
  randomize tr
  addr_reuse_builder.apply(cur_uid, tr, recent queues)
  tr.fixup_after_addr_reuse()
  set_main_transaction(cur_uid, tr)
```

这样主表生成函数仍然简单，但跨 transaction 状态由专门 helper 管理，避免把全局生成上下文塞进 transaction `post_randomize()`。

`fixup_after_addr_reuse()` 必须在强制类型修正后重新对齐派生字段。不能只修改一个 load/store 类型标志。
至少需要覆盖：

```text
memblock_lsq_flow_e
fuType / fuOpType 合法组合
rfWen / fpWen
load/store issue target 路由所依赖的行为字段
后续 LQ/SQ 分配和主表状态初始化所依赖的派生字段
```

如果候选队列为空，也仍然要调用该修正函数，因为当前项的最终类型已经被枚举强制修正。

类型修正建议拆成两个层次：

```text
set_transaction_ls_kind(tr, final_kind):
  只表达“当前项最终要变成 load/store”；
  不直接手写所有派生字段。

fixup_after_addr_reuse(tr):
  复用现有 apply_minimal_op_template() 或等价公共 helper；
  根据 final_kind 重新选择合法 fuType/fuOpType；
  修正 lsq_flow、rfWen/fpWen、issue route 和 LQ/SQ 行为；
  重新计算 vaddr = src + imm。
```

这样可以避免地址复用逻辑散落理解 `fuOpType`、`lsq_flow`、写回字段等细节。后续如果
load/store 合法模板变化，只需要维护统一模板 helper。

手动主表导入模式：

```text
默认不执行本方案的地址复用 directed 修正。
如果后续仍保留 MEMBLOCK_MANUAL_ADDR_REUSE_EN 或等价开关，需要明确迁移到新枚举/窗口逻辑；
不能继续调用旧 inject_ls_addr_reuse_by_fuoptype()。
```

## 11. 参数默认值建议

第一版推荐默认值：

```text
MEMBLOCK_ADDR_REUSE_EN_1_WT = 1
MEMBLOCK_ADDR_REUSE_EN_0_WT = 19

MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT = 1
MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT  = 0
MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT = 1
MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT = 0

MEMBLOCK_ADDR_REF_WINDOW_FIXED = 0
MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT = 0
MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT = 0
MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT = 1

MEMBLOCK_ROB_START_FIXED_EN = 1
MEMBLOCK_ROB_START_FIXED_VALUE = 0
MEMBLOCK_ROB_START_ZERO_WT = 1
MEMBLOCK_ROB_START_MID_WT = 0
MEMBLOCK_ROB_START_NEAR_WRAP_WT = 0
```

默认语义：

```text
1. 地址复用默认关闭，避免改变现有随机主表行为。
2. 打开地址复用后，默认优先覆盖跨类型相关：LOAD_AFTER_STORE 和 STORE_AFTER_LOAD。
3. same-type 场景默认权重为 0，需要 testcase 显式开启。
4. ROB 起点默认固定 0，与现有行为一致。
```

新增参数必须遵循公共测试框架参数路径：

```text
env/plus.sv -> seq_csr_common.sv -> getter -> 主表生成 helper
```

`seq_csr_common.sv` 需要负责：

```text
1. 所有权重非负 clamp。
2. enable 权重全 0 时 warning，并回退到 EN_0=1。
3. 枚举权重全 0 时 warning，并回退到跨类型默认权重。
4. 窗口 fixed / dist 结果 clamp 到合法范围。
5. ROB start fixed value clamp 到 [0, MEMBLOCK_ROB_SIZE-1]。
```

## 12. 实现 Checklist

- [ ] 新增 `memblock_addr_reuse_kind_e` 枚举。
- [ ] 新增地址复用 enable、窗口参数、四类枚举权重参数。
- [ ] 删除旧 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT` / `MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 参数入口。
- [ ] 删除旧 `inject_ls_addr_reuse_by_fuoptype()` / `select_load_addr_ref()` / `select_prior_store_addr_ref()` 后处理扫描入口。
- [ ] 同步更新 `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/default.cfg` 和使用旧 key 的现有 testcase cfg。
- [ ] 实现 `prune_recent_q()`，按 `cur_uid - ref_uid > addr_ref_window` 淘汰。
- [ ] 实现 `random_pick_uid()`，支持随机读取，且仅 `LOAD_AFTER_LOAD` / `STORE_AFTER_STORE` 场景删除候选。
- [ ] 主表生成时维护 `recent_load_uid_q` / `recent_store_uid_q`。
- [ ] 地址复制后调用 transaction 合法性修正，确保类型、fuOpType、LSQ flow、写回字段一致。
- [ ] 不再从完整主表扫描候选。
- [ ] 新增 ROB 起始值 fixed/dist 参数，默认保持 robIdx value=0。
- [ ] 主表随机生成只随机 uid=0 的 ROB 起始值，后续 uid 继续使用 `rob_order_util::rob_advance()` 连续递增。
- [ ] 更新相关设计文档，删除旧 LD_TO_ST / ST_TO_LD 注入描述。
