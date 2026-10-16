# 问题 2 后续：同 sample Load MMIO 与 Redirect 的 Owner 解析分析

分析日期：2026-10-14

适用版本：V2 `mem_ut_uvm_v2`

分析范围：真实 seed `37617171` 中，原 R+2 stale `loadMmio` owner/epoch 修复后新出现的
`MMIO_RESOLVE` fatal。本文只分析日志和现有 SystemVerilog 测试框架源码，不修改 RTL、
SystemVerilog、cfg、参数或仿真配置，也未运行仿真。

有效日志：

`mem_ut/ver/ut/memblock/sim/fault12_original_seed_fullcfg_20261014_002/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_problem12_seed37617171_fullcfg_natural_20261014_002.log`

## 1. 术语与抽象功能说明

### 1.1 术语

| 英文术语 | 本文中的中文含义 | 对应代码对象 | 示例 |
|---|---|---|---|
| `raw` | ctrl monitor 在一个 DUT sample 冻结的原始旁带事实 | `dispatch_raw_ctrl_t` | 同一 raw 可同时保存 `loadMmio`、deq 和 memoryViolation |
| `sample` | 多个 monitor 共用的 DUT 单调采样序号，不等于 service cycle 或 flush epoch | `mmio_sample_seq`、`redirect_sample_seq` | 本次 fatal 的两个序号均为 `2214` |
| `observation epoch` | monitor 看到 MMIO 脉冲时冻结的当前 framework flush epoch，不直接代表脉冲 producer | `mmio_flush_epoch` | redirect 已申请后采到的旧脉冲也可能带新 epoch |
| `activation epoch` | 当前动态实例进入 active map 时看到的 flush epoch | `active_instance_flush_epoch` | 小于 redirect epoch 表示实例在该 redirect 之前已激活 |
| `active owner` | 当前仍由完整 ROB key 映射到 uid 的动态实例 | `uid_by_active_rob` | resolver 对同一 ROB value 分别 probe flag 0/1 |
| `redirect-deleted owner` | redirect flush 删除 active LQ/SQ owner 前保存的单代短时旧身份 | `redirect_deleted_lq_owner_by_key`、`redirect_deleted_sq_owner_by_key` | R+2 active owner 已消失时用于历史证明 |
| `covered` | 完整 ROB key 按 redirect 边界应被 flush | `rob_order_util::rob_need_flush()` | redirect 自身在 `flush_itself=1` 时也属于 covered |
| `uncovered` | active Load 比 redirect 旧，且完整 ROB key 不在 flush 范围 | `overlap_uncovered_count` | 本次 fatal 唯一 active 候选被归入该类 |
| `surviving owner` | 不被当前 redirect 覆盖、应继续存活的 active owner | 本文建议显式增加的分类语义 | 不是 stale owner，也不是 redirect 后新 owner |
| `stale-drop` | 已证明 raw 属于被杀旧实例，只丢 raw，不写 MMIO tag | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | 原 R+2 修复的成功结果 |
| `CURRENT` | 已证明 raw 属于当前 active 动态实例，允许写 MMIO tag | `MEMBLOCK_MMIO_RESOLVE_CURRENT` | 本文建议用于唯一 surviving owner |
| `wrap ambiguity` | 同一 ROB value 的 flag 0/1 可能表示不同完整 ROB key | `memblock_rob_key_t` | value-only raw 不能仅凭 value 猜 flag |

### 1.2 关键函数的抽象职责

| 函数/task | 抽象功能描述 | 职责边界 |
|---|---|---|
| ctrl monitor `mon_data()` | 在同一 DUT sample 冻结 `loadMmio` valid/value、observation epoch、sample 序号和时间 | 不解析 uid，不判断 redirect 覆盖，不写 status |
| `drain_lsq_timing_sidebands()` | 把 redirect anchor 和 cancel snapshot 从公共 raw queue 转入 `common_data` 历史 | 不应用 redirect flush，不解析 MMIO owner |
| `collect_monitor_event_batch()` | 形成 semantic batch，先做 redirect-first feedback 仲裁，再应用 deferred ctrl raw | MMIO tag 解析发生在 active map 的 deq 删除之前 |
| `request_redirect_flush()` | 为一次 redirect 分配 epoch/record，冻结 issue，并建立 `active_redirect` | 不等待 monitor anchor，也不扫描 active uid |
| `bind_redirect_anchors_to_cancel_records()` | 按 FIFO 把 monitor anchor 绑定到尚未绑定的 framework redirect record | 不判断 MMIO owner |
| `apply_redirect_flush_range()` | 在 anchor 和 LSQ sample 边界满足后扫描 active window，按完整 ROB key flush covered uid | 不处理 value-only MMIO raw |
| `resolve_mmio_uid_by_rob_value()` | 把 value-only MMIO raw 分类为 CURRENT、已证明 stale-drop，或无法证明的 fatal | 不应把时间 overlap 本身当作 stale 的充分条件 |
| `find_unique_redirect_deleted_load_mmio_owner()` | 精确 R+2 且 active owner 为零时，用 status/main table 和 deleted-owner 证明唯一旧 Load | 不服务本次 `R` 同 sample active-owner 分支 |

## 2. 问题摘要

原问题 2 的 R+2 修复在本次真实 seed 重跑中已经实际生效：fatal 前共有 19 条
`loadMmio sample=R+2` 被唯一匹配到 redirect-deleted Load owner 并 stale-drop。随后在
`11268.400ns` 出现新的 fatal：

```text
UVM_FATAL @ 11268.400ns ... [MMIO_RESOLVE]
cannot prove LOAD MMIO stale ownership sample=2214 redirect_sample=2214
redirect_record=39 active=1 old_covered=0 new=0 uncovered=1 incompatible=0
```

该事件不是 R+2 历史证明失败，而是 `loadMmio` 与 redirect anchor 同为 sample `2214` 的
`R` 场景。resolver 找到一个兼容 active Load owner；该 owner 在 redirect 前已经激活，
但完整 ROB key 不被本次 redirect 覆盖，因此被计入 `uncovered=1`。当前决策树对 redirect
overlap 只允许“covered old owner -> stale-drop”，没有“唯一 uncovered surviving owner ->
CURRENT”路径，最终直接 fatal。

当前可确认的直接根因是 **resolver 的 redirect-overlap 分类和决策顺序缺少合法 surviving
owner 分支**。同拍采样是触发条件，不是已证明的 monitor race；`rob_need_flush()` 返回不覆盖
是 fatal 的直接输入，不应被忽略或强行改成 covered。

## 3. 日志证据

### 3.1 原 R+2 修复没有回归

日志统计：

| 项目 | 数量 | 结论 |
|---|---:|---|
| `MMIO_RAW` 信息总数 | 43 | fatal 前已有大量真实 MMIO overlap |
| active covered overlap stale-drop | 23 | R/R+1 既有路径正常工作 |
| R+2 deleted-owner stale-drop | 19 | 原 R+2 修复真实命中 19 次 |
| 未捕获 `MMIO_RESOLVE` fatal | 1 | 即本次 sample 2214 的 uncovered active owner |

19 条 R+2 成功记录对应的 redirect record 为：

```text
1, 1, 3, 5, 6, 7, 8, 9, 13, 14, 16, 16, 20, 20, 25, 28, 30, 32, 35
```

同一 record 出现两次表示同一 redirect 下有两个不同 `loadMmio` port/value，不能按 record
去重后宣称只有 16 次。代表性证据：

```text
UVM_INFO @ 868.400ns ... [MMIO_RAW] ... sample=134 is R+2 of redirect sample=132
and uniquely matches deleted uid=61 ROB=0/132 epoch=1 record=1

UVM_INFO @ 9428.400ns ... [MMIO_RAW] ... sample=1846 is R+2 of redirect sample=1844
and uniquely matches deleted uid=98 ROB=1/9 epoch=35 record=35
```

因此，新 fatal 只是 seed 继续向后运行后暴露的下一条独立边界，不构成原 R+2 修复失效证据。

### 3.2 本次 fatal 能直接证明的事实

| 字段 | 已知值/关系 | 证据来源 |
|---|---|---|
| fatal 时间 | `11268.400ns` | fatal 原文 |
| raw sample | `2214` | fatal 原文 |
| redirect sample | `2214` | fatal 原文 |
| redirect record id | `39` | fatal 原文 |
| active ROB-value 候选数 | `1` | `active=1` |
| covered old 候选数 | `0` | `old_covered=0` |
| redirect 后新实例候选数 | `0` | `new=0` |
| redirect 前但未被覆盖候选数 | `1` | `uncovered=1` |
| kind/dispatch 不兼容候选数 | `0` | `incompatible=0` |
| 候选操作类型 | 兼容 scalar Load 且 `load_dispatched=1` | 只有通过源码兼容性过滤后才会进入 uncovered |
| activation epoch 关系 | `active_instance_flush_epoch < redirect_epoch` | 否则会计入 `new`，不会计入 `uncovered` |
| ROB 覆盖结果 | `rob_need_flush(active_key, redirect)==0` | uncovered 分支的唯一进入条件 |

### 3.3 现有日志不能证明的字段

以下字段未出现在 overlap fatal，也没有其它同时间日志可无歧义关联，禁止根据相邻 WB 日志猜测：

| 缺失字段 | 为什么重要 |
|---|---|
| raw `loadMmio` port | 确认是哪个物理 MMIO 输出 lane |
| raw ROB value | 构造 flag 0/1 两个完整候选的基础字段 |
| active candidate UID | 不能把相邻的 uid 46、121 或 55 当作 MMIO owner |
| active candidate 完整 ROB key | 确认 wrap flag 和 `rob_need_flush()` 输入 |
| active candidate dynamic epoch | 排除错误 active map 或旧动态实例残留 |
| active/load target issue epoch | 确认该动态实例已真实 issue，且 raw 时序与当前 target 一致 |
| active activation epoch 精确值 | 当前只能证明其小于 redirect epoch |
| redirect epoch 精确值 | record id 通常与 epoch 同步递增，但不能据此假定二者相等 |
| redirect 完整 payload | 缺少 `flush_itself/level/is_vls_exception/ROB flag/value` |
| record 39 当时是否已绑定 anchor | resolver 可从 anchored record 或未绑定 anchor FIFO 两条路径得到 record 39 |
| 同 ROB value 的 inactive/历史 Load 候选 | 用于排除 value-only raw 来自已退出或将被覆盖的其它完整 ROB key |
| redirect-deleted owner 候选 | 本次是 R 且 active 不为零，R+2 helper 根本没有执行；“未查询”不等于“候选数为零” |

### 3.4 最小诊断打印

最小修改不是新增另一套状态，而是在所有 overlap fatal 前调用已有
`dump_mmio_resolve_failure_context()`，并把当前 fatal 补充为结构化字段：

1. raw：port、ROB value、sample、cycle、observation epoch、kind。
2. redirect：record id、redirect epoch、anchor sample、完整 payload。
3. 每个 active 候选：uid、完整 ROB key、dynamic epoch、activation epoch、load issue epoch、
   `load_dispatched`、`rob_need_flush()` 结果和最终分类。
4. 若进入 R+2 历史路径：打印每个 deleted-owner 候选的 uid、ROB/LQ key、old dynamic epoch、
   old activation epoch、record id，以及被拒绝的具体条件。

现有 `dump_mmio_resolve_failure_context()` 已能打印第 1 至第 3 类的大部分字段及所有 cancel
record，但当前只在普通 owner 解析最终失败路径调用；本次 line 1423 的 overlap fatal 没有调用它。
因此最小诊断动作是复用该 helper，而不是先新增大范围 tracing。

## 4. 源码控制流与逐 sample 时序

### 4.1 Raw 采样

源码位置：
`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv:239`

抽象功能描述：ctrl monitor 在一个 monitor callback 内同时采样 deq、memoryViolation 和
`loadMmio/storeMmio`；只要任一 MMIO valid，就把当前 `dispatch_flush_epoch` 和同拍
`sample_seq` 固定进 raw。

关键顺序：

1. 读取各 MMIO lane 的 valid/value。
2. 把 `mmio_flush_epoch` 写为采样时的 framework 全局 epoch。
3. 把 `mmio_sample_seq` 写为当前 DUT sample。
4. 把完整 raw 压入 `raw_ctrl_q`。

该 monitor 不读取 active map，因此不会在采样点决定 raw 属于 covered 还是 uncovered owner。

### 4.2 Redirect epoch 与 record 建立

源码位置：
`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:4120`，函数
`request_redirect_flush()`。

抽象功能描述：recovery owner 决定发起 redirect 时，先增加 `dispatch_flush_epoch`，再创建
cancel record，并把 payload 保存为 `active_redirect`。monitor 之后看到的 MMIO 即使来自 redirect
前流水，也会冻结这个已经增加后的 observation epoch。

关键状态变化：

1. `dispatch_flush_in_progress=1`。
2. `dispatch_flush_epoch++`。
3. 新建 `cancel_record_q` 项并分配 `cancel_record_id`。
4. 保存完整 redirect payload。
5. 建立 `active_redirect` 并冻结 issue。

### 4.3 Redirect anchor 采样与 record 绑定

源码位置：

- `redirect_agent_agent_monitor.sv:62`：采样 DUT redirect 输入并生成 anchor。
- `dispatch_monitor_event_adapter.sv:1428`：drain anchor。
- `common_data_transaction.sv:3479`：按 FIFO 绑定 anchor 和 framework record。

抽象功能描述：redirect monitor 只记录“DUT 在哪个 sample 看到了哪个 redirect payload”。anchor
可以先进入 `redirect_anchor_history_q`，稍后在 reconcile 中绑定 record。MMIO resolver 同时支持：

- 直接读取已绑定 anchor 的 record。
- 从未绑定 anchor FIFO 与最老未绑定 record 做只读配对。

因此 record 39 在 fatal 时是否已经正式绑定，不影响 resolver 得到
`redirect_sample=2214/redirect_record=39`；但现有日志无法区分使用了哪条路径。

### 4.4 Service 调用顺序

源码位置：
`memblock_main_dispatch_auto_build_main_table_base_sequence.sv:182`，task
`service_monitor_once()`。

抽象功能描述：单次 dispatch service 先收集当前 sample 的 sideband 和 semantic event，再处理
deferred ctrl，之后才推进 redirect recovery 和 cancel reconcile。

与本问题相关的固定顺序：

1. `drain_lsq_timing_sidebands()`：把 sample 2214 redirect anchor 加入本地历史。
2. `collect_monitor_event_batch()`：收集 WB/IQ/ctrl semantic event。
3. `process_monitor_event_batch()`：执行 redirect-first feedback 仲裁。
4. `apply_deferred_ctrl_updates_batch()`：对 sample 2214 ctrl raw 先解析 MMIO tag，再处理 deq。
5. `service_redirect_deleted_owner_window()`。
6. `exception_redirect_replay_task()`：可能推进或应用 redirect flush。
7. 第二次 drain sideband，并执行本 service tick 唯一的 cancel reconcile。

本次 fatal 发生在第 4 步。由于 fatal 提前终止，第 6、7 步不会完成。并且
`apply_redirect_flush_range()` 要求 record 已绑定 anchor 且到达 `redirect_lsq_sample_seq`；所以本次
raw 被解析时，当前 active map 仍是 flush scan 前的状态。这一点支持把唯一 uncovered owner 理解为
“本次 redirect 的 surviving active owner”，而不是 flush 后新建 owner。

### 4.5 Resolver 决策顺序

源码位置：
`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:1119`，函数
`resolve_mmio_uid_by_rob_value()`。

抽象功能描述：resolver 先找唯一 redirect timing overlap，再 probe 同一 ROB value 的两个完整
active key；overlap 分支把 active Load 分类后决定 stale-drop 或 fatal。

当前真实执行顺序：

1. 对 Load 扫描未完成 redirect record/anchor，接受 `R..R+2`。
2. sample 2214 唯一命中 record 39 的 redirect sample 2214。
3. 分别 probe `{flag=0,value=raw_value}` 和 `{flag=1,value=raw_value}`。
4. 只命中一个 active owner，因此 `active_candidate_count=1`。
5. 该 owner 是已 dispatch scalar Load，因此 `incompatible=0`。
6. 该 owner activation epoch 小于 redirect epoch，因此 `new=0`。
7. `rob_need_flush()` 对其完整 key 返回 0，因此进入 `uncovered=1`。
8. covered-old stale-drop 条件失败。
9. R+2 deleted-owner 条件失败，因为当前是 R 且 active 不为零。
10. 直接触发 overlap fatal。

当前决策树没有回到普通 CURRENT 路径。也就是说，一旦 `load_overlap_observed=1`，一个完全合法的
redirect surviving active owner也会被永久排除在 CURRENT 之外。

## 5. 逐拍/逐 sample 重建

| 阶段 | sample/时间 | 可证明状态 | 不可证明字段 |
|---|---|---|---|
| redirect request | 早于 sample 2214 | record 39 已创建，否则 resolver 无法配对；全局 flush epoch 已增加 | 申请的精确时间、redirect epoch、完整 payload |
| redirect drive/monitor | sample 2214，`11268.400ns` 对应 service | redirect monitor 产生 sample 2214 anchor | redirect ROB key、level、flush_itself、VLS 标志 |
| MMIO monitor | sample 2214 | 同拍出现至少一个 Load MMIO raw | port、ROB value、observation epoch |
| sideband drain | sample 2214 service 开始 | anchor 进入本地 history | 此时是否已经和 record 39 正式绑定 |
| MMIO resolve | sample 2214 deferred ctrl apply | 唯一 active compatible Load；旧于 redirect；完整 key 不被覆盖 | uid、ROB flag/value、dynamic/issue/activation epoch 精确值 |
| deleted-owner probe | 未执行 | R+2 helper 的入口条件不满足 | deleted-owner map 对同 value 是否存在其它历史歧义 |
| fatal | `11268.400ns` | `active=1 old_covered=0 new=0 uncovered=1 incompatible=0` | raw 到真实 DUT producer 的最终一一对应关系 |

候选 owner 清单只能按证据写为：

| 候选类型 | UID | ROB key | epoch | 覆盖证据 | 结论 |
|---|---|---|---|---|---|
| active Load candidate 0 | 未打印 | 未打印 | 只知 activation epoch `<` redirect epoch | `rob_need_flush=0`，因此 `uncovered=1` | 唯一 active compatible survivor 候选 |
| 第二个 active candidate | 无 | 同 raw value 的另一 flag 未命中 active map | 无 | 无 | `active=1` 证明不存在第二个 active 命中 |
| redirect-deleted candidate | 未查询 | 未查询 | 未查询 | 未查询 | 本次不是 R+2/active=0，不能写成 0 个候选 |
| 其它 inactive 历史 candidate | 未扫描/未打印 | 未打印 | 未打印 | 未打印 | 现有 fatal 无法排除，需诊断 dump |

## 6. 根因结论与待确认项

### 6.1 直接根因

直接根因是 **redirect overlap 分支把“时间上与 redirect 接近”错误地收敛为“只能证明 stale，
否则 fatal”**。它已经计算出完整 ROB key 不被 redirect 覆盖，却没有把该结果作为合法 CURRENT
证据使用。

`uncovered` 不是应被忽略的异常计数。对 redirect 前已激活且不被覆盖的唯一 scalar Load，
`uncovered` 的直接语义是 surviving owner 候选；只有存在其它 covered/new/incompatible 或历史歧义时，
才应继续 fatal。

### 6.2 分类判断

| 候选类别 | 判断 | 说明 |
|---|---|---|
| 同拍采样顺序 | 触发条件，不是直接根因 | sample provenance 一致，service 顺序确定；没有证据表明是 monitor 非确定 race |
| redirect 覆盖语义 | 直接相关 | `rob_need_flush=0` 是本次唯一候选未被 flush 的核心事实 |
| resolver 决策顺序 | 直接根因 | overlap 分支没有 CURRENT 出口 |
| 候选歧义 | 当前 active 候选不歧义；历史歧义待确认 | active count 恰为 1，但 fatal 未扫描/打印 inactive 历史候选 |
| active 生命周期 | 不是当前证据支持的根因 | owner 仍 active，且有 activation provenance、Load 类型和 dispatch 事实 |
| epoch 边界 | 不是当前证据支持的根因 | `new=0/uncovered=1` 说明 resolver 正确识别其为 redirect 前实例 |
| `rob_need_flush()` 实现错误 | 无证据 | 缺完整 key/payload，不能反向断言覆盖 helper 错误 |
| DUT 发错 `loadMmio` | 未确认 | 缺 raw ROB value、真实 producer 和候选详情，不能把 framework fatal直接归为 DUT bug |

### 6.3 待确认项

在实施功能修复前，至少用最小诊断确认一次真实 seed：

1. 唯一 uncovered owner 的 UID、完整 ROB key和操作类型。
2. record 39 的完整 redirect payload和 redirect epoch。
3. raw port、ROB value和 observation epoch。
4. 同 ROB value 是否存在 inactive、已 issue、且被 record 39 覆盖的其它 Load 历史。
5. surviving owner 的 load target issue epoch/instance flush epoch是否与 raw observation 一致。

若第 4 项存在可信 covered 历史 owner，则 value-only raw 仍有歧义，必须保持 fatal或引入更强 provenance；
不得直接把 raw 写给 surviving owner。

## 7. 修改方案对比

### 7.1 方案 A：增加 R/R+1 唯一 surviving active owner 的 CURRENT 分支

核心语义：保留现有 active probe 和完整 ROB 覆盖计算，把 `uncovered` 重命名或补充为
`surviving_current`。只在 flush scan 尚未发生的 R/R+1 窗口内，允许唯一 surviving owner 返回
CURRENT。

建议判定条件：

1. `raw_sample_seq` 为 `R` 或 `R+1`。
2. `active_candidate_count==1`。
3. `surviving_current_count==1`。
4. `old_covered==0`、`new==0`、`incompatible==0`。
5. 候选是已 dispatch scalar Load，activation epoch 严格小于 redirect epoch。
6. 候选完整 ROB key 的 `rob_need_flush()` 为 0。
7. 最小诊断未发现同 ROB value 的可信 covered 历史 owner；若发现则仍 fatal。

建议决策顺序：

1. 唯一 covered old active owner：保持 STALE_DROP。
2. R/R+1 唯一 surviving active owner：返回 CURRENT并写该 uid 的 MMIO tag。
3. 精确 R+2 且 active owner 为零：保持现有 deleted-owner 唯一历史证明。
4. covered 与 surviving 混合、new owner、incompatible、多 active、历史歧义、R+2 uncovered active：fatal。

优点：

- 修改范围最小，直接修复当前 R 同拍 false fatal。
- 使用已经计算出的完整 ROB key、activation epoch和覆盖结果，不新增第二套 owner 真源。
- 不丢弃 raw，而是把 raw 写给明确存活的 current owner，不会用 stale-drop 掩盖 DUT 错误。
- R+2 helper、deleted-owner 生命周期和 19 次已验证 stale-drop不变。

风险：

- raw 仍只有 ROB value。若存在一个已退出 active map但仍可能产生脉冲的 covered 同 value Load，
  surviving owner与真实 producer 仍可能歧义。
- 因此推荐实现必须先补 fatal 诊断，并把 R+2 active-uncovered 保持 fatal；不能把所有
  `uncovered` 无条件改为 CURRENT。

影响：`common_data_transaction.sv` 的 resolver和 directed test；不需要修改 monitor raw、cfg、
interface或 RTL。

### 7.2 方案 B：为 Load MMIO 建立短时 producer provenance token

核心语义：在 Load issue/LoadQueueUncache 可对应的框架边界建立短时 token，保存 uid、完整 ROB key、
dynamic epoch、issue epoch、port和允许到达的 sample 窗口；`loadMmio` 到达时先按 token 匹配，再用
redirect coverage决定 CURRENT或 stale-drop。

优点：

- 能直接区分同 ROB value 的两个 wrap flag和多个动态实例。
- R、R+1、R+2 的 covered/uncovered/new owner均可使用统一 producer provenance判断。
- 如果 DUT 发出无法对应任何 token 的 `loadMmio`，可以明确 fatal，不会误写 survivor。

风险与影响：

- 需要定义 token 的建立点、端口映射、最大延迟、redirect清理、正常消费、重复脉冲和 wrap生命周期。
- 容易形成与 active map、issue queue、deleted-owner并行的第三套 owner状态。
- 若 V2 RTL 输出 lane与 issue port没有稳定一一对应，token模型本身可能错误。
- 需要修改 transaction/type、公共状态、issue路径、resolver和大量定向测试，风险显著高于当前问题。

适用条件：最小诊断证明 R/R+1 也可能存在“一个 surviving active owner加一个不可见 covered历史
producer”的真实歧义，或后续 R+2 active-uncovered需要无 fatal 支持时，再采用该方案。

### 7.3 方案 C：通过调整 drain/消费顺序绕开同 sample overlap

示例做法是先应用 MMIO raw，再 drain redirect anchor，或让 same-sample raw看不到当前 anchor。

该方案不推荐：

- 同一个 DUT sample 的事实会因软件 service顺序被人为拆开。
- covered old owner可能被误当 CURRENT并写 tag，直接破坏已有 R/R+1 stale-drop。
- 原 R+2 record/anchor证明依赖稳定的 sample provenance和 FIFO配对。
- 它只隐藏 `uncovered`，没有解释覆盖语义，也不能处理 R+1/R+2。

## 8. 推荐方案

推荐 **方案 A：R/R+1 唯一 surviving active owner 显式返回 CURRENT，并保持其它边界 fatal**。

推荐理由：

1. 本次证据已经证明唯一 active 候选不是 covered old owner，也不是 redirect 后新 owner。
2. 在当前 service 顺序中，R/R+1 的 MMIO resolver运行在 redirect flush scan之前；covered active owner
   如果存在，仍应出现在两个 flag的 active probe中。唯一 uncovered候选因此是最小可用的 current证据。
3. 返回 CURRENT不会忽略 raw，也不会把不确定事件静默 stale-drop；tag仍进入正常 MMIO状态检查。
4. 对 mixed、多候选、new、incompatible、R+2 active-uncovered和历史歧义继续 fatal，保留错误暴露能力。
5. 原 R+2 helper只在 `sample==R+2 && active==0` 时运行，与新增 R/R+1 CURRENT分支互斥。

不推荐把判定简化为“`uncovered==1` 就 CURRENT”。必须同时约束 sample offset、候选总数、类型、
activation epoch、覆盖结果及历史歧义诊断。

## 9. 与原 R+2 修复的关系

两者不冲突，原因如下：

| 原 R+2 逻辑 | 推荐方案影响 |
|---|---|
| overlap窗口仍为 `R..R+2` | 不变 |
| R/R+1 covered active old owner stale-drop | 不变，且优先于 surviving CURRENT |
| R+2 active=0调用 deleted-owner helper | 不变 |
| deleted-owner要求同一 record、old dynamic epoch、old activation epoch | 不变 |
| R+2 new active owner fatal | 不变 |
| R+2 wrap历史多候选 fatal | 不变 |
| R+3及以后不使用当前 redirect解释 | 不变 |

防回归关键是不要把 surviving CURRENT 分支放在 covered-old stale-drop之前，也不要让它在
`active_candidate_count==0` 时执行。真实 seed 验收必须逐条核对 fatal 前已经出现的 19 条 R+2
stale-drop仍匹配相同 raw sample、redirect sample、uid、完整 ROB key、epoch和record。

## 10. 建议实施步骤

本文不执行 coding。后续获得明确 coding授权后，建议按以下顺序实施：

1. 在所有 overlap fatal 前复用 `dump_mmio_resolve_failure_context()`；补充 raw port和每个 active候选的
   分类日志。先用真实 seed确认本文待确认字段。
2. 在 resolver中增加独立的 `overlap_surviving_current_count`、uid和完整 ROB key，不继续把合法
   survivor只命名为 `uncovered`。
3. 保持 covered-old stale-drop为第一优先级。
4. 增加 R/R+1唯一 surviving active Load返回 CURRENT的严格条件；任何混合分类仍 fatal。
5. 保持 R+2 active=0历史 helper原样；R+2存在 active uncovered时先保持 fatal。
6. 扩展 pending-MMIO directed sequence，覆盖本节验证矩阵。
7. 更新 writeback flow和原问题2 implementation review，明确 overlap同时允许 covered-stale与
   uncovered-current两种语义，不能再写成“无法证明覆盖一律 fatal”。
8. 完成定向测试后重跑真实 seed；若 seed继续出现后续独立错误，按时间和错误 ID单独记录。

## 11. Directed 测试与真实 seed 验收矩阵

### 11.1 Directed 测试矩阵

| 编号 | sample偏移 | owner状态 | redirect覆盖 | wrap/候选 | 预期 |
|---|---|---|---|---|---|
| D1 | R | active old Load | covered | 唯一 | STALE_DROP，不写 tag，保持既有行为 |
| D2 | R+1 | active old Load | covered | 唯一 | STALE_DROP，不写 tag，保持既有行为 |
| D3 | R | active old Load | uncovered | 唯一 | CURRENT，写 surviving uid tag |
| D4 | R+1 | active old Load | uncovered | 唯一 | CURRENT，写 surviving uid tag |
| D5 | R | active old Load | 一个covered、一个uncovered | flag 0/1同 value | fatal，不得猜 producer |
| D6 | R+1 | active old Load | 一个covered、一个uncovered | flag 0/1同 value | fatal |
| D7 | R | active owner | uncovered | kind不是Load或未dispatch | fatal |
| D8 | R | active owner | uncovered | activation epoch等于/晚于redirect | 按 new分类 fatal |
| D9 | R+2 | active=0、deleted唯一 | covered | 唯一历史 | STALE_DROP，验证原修复 |
| D10 | R+2 | active=0、deleted两个 | covered | wrap flag 0/1历史歧义 | fatal |
| D11 | R+2 | active new owner | 任意 | 唯一 | fatal，保持现有negative case |
| D12 | R+2 | active old survivor | uncovered | 无额外证明 | 推荐最小方案仍 fatal |
| D13 | R+2 | active survivor加deleted covered历史 | 混合 | 同 value | fatal |
| D14 | R+3 | active=0/deleted存在 | covered | 任意 | fatal，不扩大窗口 |
| D15 | 无redirect overlap | active Load | 不适用 | 唯一 | 普通 CURRENT |
| D16 | R/R+1 | active Store | 任意 | 任意 | 不进入Load overlap特判，保持Store原规则 |

### 11.2 ROB wrap 专项

至少构造以下完整 key：

1. redirect=`0/(ROB_SIZE-1)`，survivor=`0/(ROB_SIZE-2)`，验证 older survivor不被覆盖。
2. redirect=`0/(ROB_SIZE-1)`，victim=`1/0`，验证跨 wrap younger victim被覆盖。
3. raw value相同、active flag 0/1各一个，分别构造 covered+uncovered与双 covered。
4. redirect `flush_itself=0/1` 各一组，验证同 ROB key是否覆盖的差异。
5. VLS exception使用 effective level路径，避免只测普通 `flush_itself`。

### 11.3 真实 seed `37617171` 验收

| 检查项 | 验收标准 |
|---|---|
| 原 R+2修复 | fatal前19条R+2 stale-drop逐条保持相同 sample、uid、ROB key、epoch和record |
| sample 2214 | 不再出现当前 `uncovered=1` false fatal |
| sample 2214 owner | 诊断确认唯一 surviving active Load后，resolver返回 CURRENT并写该uid tag |
| 歧义保护 | 若诊断发现同 value covered历史producer，必须继续 fatal，不能强行通过seed |
| matcher | 不关闭、不降级，不允许全局忽略 `loadMmio` |
| 后续错误 | seed继续运行后的其它 UVM_ERROR/UVM_FATAL单独归类，不与本问题合并 |
| 最终通过标准 | 若无其它独立问题，`TEST CASE PASSED`且未捕获`UVM_ERROR/UVM_FATAL`均为0 |

## 12. 风险

1. **value-only固有限制**：raw没有 ROB flag。即使 active候选唯一，也必须防止已退出active map的历史
   producer与survivor共享 value。本文通过先补诊断、仅开放R/R+1、混合场景fatal来限制风险。
2. **错误扩大到R+2**：若把所有 uncovered直接CURRENT，会绕过原 R+2新/旧owner歧义保护。推荐方案
   明确禁止该做法。
3. **决策顺序回归**：surviving CURRENT如果先于 covered-old stale-drop，原19条R+2或23条active
   covered overlap可能被错误打 tag。
4. **诊断性能**：全表 dump只能放在 fatal/debug路径；正常 MMIO高频路径仍应只probe两个active key。
5. **文档与实现漂移**：`writeback_function_call_flow.md`当前写成“无法证明覆盖均fatal”，实施后必须同步
   为“covered old stale、唯一 surviving current、其余fatal”。
6. **DUT问题尚未排除**：现有日志不足以证明 sample 2214 raw一定属于 surviving owner。若最小诊断显示
   raw更可能来自 covered历史owner，应保持fatal并转向方案B，而不是为了跑过seed放宽matcher。

## 13. 最终结论

本次新 fatal 的现象是“同 sample出现 `loadMmio` 与 redirect，唯一 active Load owner未被 redirect
覆盖”；直接根因是 resolver overlap分支缺少 surviving CURRENT语义，而不是原 R+2 deleted-owner
证明失效。原修复已由19次真实 R+2唯一匹配证明有效。

推荐最小修复是：先补齐 overlap fatal诊断，再仅对 R/R+1 的唯一、兼容、redirect前已激活且完整
ROB key不被覆盖的 active Load返回 CURRENT；covered old仍stale-drop，R+2 deleted-owner helper保持原样，
所有混合、new、incompatible、wrap歧义和R+2 active-uncovered继续fatal。该方案显式处理
`uncovered`，不关闭matcher，也不以静默丢弃掩盖潜在DUT错误。

## 14. 实施 Review 与测试闭环

实施日期：2026-10-14。

### 14.1 Plan review

实施方案与第 8 节保持一致，没有采用 `uncovered==1 -> CURRENT` 的宽松判定：

1. covered old scalar Load 的 `STALE_DROP` 分支保持最高优先级。
2. 只有 sample 为 R/R+1、active候选总数为1、uncovered为1，且 covered/new/incompatible均为0时返回
   `CURRENT`。
3. 精确R+2只有active为0时才进入deleted-owner唯一历史证明；R+2存在active survivor时保持fatal。
4. mixed/new/incompatible/multi-active/wrap ambiguity/R+2 no-history/R+3均保持fatal。
5. 所有owner解析fatal及active provenance不变量fatal前均调用只读
   `dump_mmio_resolve_failure_context()`，不修改owner、redirect或status状态。

Review结论：方案条件充分收紧，新增分支与原R+2 helper互斥，没有把observation epoch当作producer
身份，也没有关闭或降级MMIO matcher。

### 14.2 Coding review

核心实现位于：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`
- `AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md`

代码自审结论：

- survivor uid和完整ROB key只在uncovered分类时记录，最终仍由候选总数及其它分类计数共同约束。
- R/R+1 CURRENT位于covered-old STALE_DROP之后、R+2 deleted-owner之前。
- R+2 active survivor不会调用deleted-owner helper，也不会写MMIO tag。
- 新增D13定向场景构造同ROB value的`flag0` active survivor与`flag1` covered deleted owner；R+2
  raw命中fatal且两个owner均未写tag，证明现有实现不会因“唯一active”忽略历史producer歧义。
- `git diff --check`通过；未修改或规避独立signed Load扩展比较逻辑。

### 14.3 编译与 directed 测试

最终编译命令：

```bash
make eda_compile tc=basicTest \
  ts=memblock_pending_mmio_directed_vseq \
  mode=fault2_same_sample_fix_20261014 \
  cfg=default wave=off seed=666666
```

编译日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_same_sample_fix_20261014/log/vcs_compile_rtl.log`

最终directed命令：

```bash
make eda_batch_run tc=basicTest \
  ts=memblock_pending_mmio_directed_vseq \
  mode=fault2_same_sample_fix_20261014 \
  cfg=default wave=off seed=666666 \
  note=problem2_final_directed_review_20261014
```

directed日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_same_sample_fix_20261014/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl_problem2_final_directed_review_20261014.log`

结果：`TEST_PASS`，caught expected `UVM_FATAL=11`，最终`UVM_ERROR=0`、`UVM_FATAL=0`，
`$finish at 365.300ns`。其中D13在sample 28确认：`uid0 ROB=0/7`仍active/uncovered，
`uid1 ROB=1/7`已deleted，resolver保持fatal且不写tag。

### 14.4 真实 seed：当前 cfg 1:98:1

绝对cfg共提取212个plusargs，`plus_file=`为空；启动值确认
`MAIN_TRANS_NUM=10000`、dispatch/LSQ三项开关均为1。

```bash
CFG=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_fault_2pct_no_trigger.cfg
plus_arg="$(awk '/^[[:space:]]*\+[A-Za-z0-9_]+=/ {printf "%s ", $1}' "$CFG")"
make eda_batch_run tc=basicTest \
  ts=memblock_dispatch_real_smoke_vseq \
  mode=fault2_same_sample_fix_20261014 \
  cfg=tc_l2tlb_fault_2pct_no_trigger wave=off seed=37617171 \
  plus_file= plus_arg="$plus_arg" \
  note=problem2_seed37617171_current_1_98_1_final_20261014
```

日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_same_sample_fix_20261014/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_problem2_seed37617171_current_1_98_1_final_20261014.log`

仿真自然结束于`8573.400ns`：`TEST_PASS`未出现，`UVM_ERROR=1`、`UVM_FATAL=1`。fatal是sample
1675/R+2 active-uncovered：唯一active `uid160 ROB=1/71`不被redirect `ROB=1/76`覆盖。现有value-only
provenance不能排除deleted covered producer，因此按方案保持fatal；这不是R/R+1 survivor修复回归。
该轨迹中的独立`UVM_ERROR`是RM Load exception比较：uid14 expected exception为0，status/actual均为
`0x80000`，与MMIO owner解析无关。

### 14.5 真实 seed：历史 cfg 1:1:1轨迹

当前绝对cfg与历史命令的212个`MEMBLOCK_*`参数逐项比较后，唯一差异是
`MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT=98`对历史值`1`。复现时只在命令构造阶段覆盖该项，不修改cfg文件：

```bash
CFG=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_fault_2pct_no_trigger.cfg
plus_arg="$(awk '/^[[:space:]]*\+[A-Za-z0-9_]+=/ {token=$1; if (token == "+MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT=98") token="+MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT=1"; printf "%s ", token}' "$CFG")"
make eda_batch_run tc=basicTest \
  ts=memblock_dispatch_real_smoke_vseq \
  mode=fault2_same_sample_fix_20261014 \
  cfg=tc_l2tlb_fault_2pct_no_trigger wave=off seed=37617171 \
  plus_file= plus_arg="$plus_arg" \
  note=problem2_seed37617171_historical_1_1_1_final_20261014
```

日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_same_sample_fix_20261014/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_problem2_seed37617171_historical_1_1_1_final_20261014.log`

验收结果：

- 原19条R+2 deleted-owner stale-drop全部保持。
- sample 2214不再fatal；两个Load MMIO port分别唯一匹配survivor `uid121 ROB=1/32`和
  `uid55 ROB=0/126`，均返回CURRENT。
- 仿真继续运行到sample 2246，随后因另一条R+2 active-uncovered `uid149 ROB=1/60`自然fatal。
- 最终`TEST_PASS`未出现，`UVM_ERROR=1`、`UVM_FATAL=1`、`$finish at 11428.400ns`。
- `UVM_ERROR`是独立的signed Load扩展比较：expected `0x0000000000001eea`，DUT
  `0xffffffffffff1eea`，不属于MMIO owner修复。

### 14.6 最终判断

历史问题2中的原R+2 stale owner和本次same-sample R/R+1 false fatal均已解决，并由19条保留记录、
sample 2214 survivor CURRENT及directed positive/negative矩阵验证。当前完整随机测试仍未全通过，原因是
新的R+2 active-uncovered边界缺少足够producer provenance；在现有value-only接口下不能安全扩展为CURRENT，
因此继续fatal是预期保护。若后续需要支持该边界，应增加lane/issue token等可区分producer的provenance，
不能仅放宽`uncovered==1`。
