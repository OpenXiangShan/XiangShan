# memblock RM/L2 DCache 无清除错误状态账本修改方案（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 待 coding，未实施 |
| 方案类型 | RM plan |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 关联分析 | `AI_DOC/analysis/framework_design/memblock_mmu_sv39_rm_exception_mismatch_analysis_20260827.md` |
| 前置模型方案 | [`memblock_rm_pma_pmp_model_plan_20260828.md`](./memblock_rm_pma_pmp_model_plan_20260828.md) |
| 代码修改授权 | 当前未获得；本文仅描述方案，不修改 RTL、RM、sequence、cfg 或 interface |
| 创建日期 | 2026-08-28 |

## 1. 术语与职责

| 术语 | 中文含义 | 计划中的状态或代码落点 |
| --- | --- | --- |
| `sticky error` | 物理 DCache 行一旦出现 L2 D-channel 错误，测试用例剩余时间内不再清除 | `l2_d_error_live_by_line` |
| `physical line` | 以物理地址 `paddr[47:6]` 标识的一条 64B cache line | ledger 的关联数组 key |
| `live map` | 当前 testcase 内已经生效的 sticky 行错误集合 | RM 和 DCache responder 的只读查询源 |
| `pending reservation` | A request 已接受但最后一个 D beat 尚未 fire 的错误结果预留 | `l2_d_error_pending_by_line` |
| `response snapshot` | 一笔 D response 在建立时固定的 opcode、line、denied、corrupt 和 data | `dcache_response_record_t` |
| `activation sample` | 最后一个带错误 D beat 真正 `fire` 的采样序号或周期 | 行记录中的错误生效时间 |
| `RM query sample` | RM 判断某笔 Load/Store 实际观察错误的时间 | Load 使用 LDA WB，Store 使用实际 STA/STD fault/WB |
| `DCache-eligible access` | 翻译成功且确实走 DCache 数据路径的访问 | RM 查询前的资格判断 |
| `operation-level error` | CBOAck、ReleaseAck 或 Uncache response 自身的错误 | 不写入 DCache 行账本 |
| `raw error` | 原始 `denied/corrupt` 两个 bit，不提前折叠成单一异常 | `l2_d_error_view_t` |

本方案把三种职责分开：DCache responder 负责产生并固定合法 D response；共享账本负责记录
该 response 对物理行造成的持久错误；RM 只通过值型只读 API 查询账本，不直接持有关联数组或
修改 responder 状态。

## 2. 背景与当前问题

当前 `memblock_rm::observer_build_commit_item()` 将 `main_view` 中的
`tlb_af/tlb_pf/tlb_gpf/pma_af/denied/corrupt` 作为期望异常的一部分。主表字段是建表时的
静态 transaction 属性，不是 DTLB、PMP 或 L2 D-channel 的运行期事实，因而会产生异常向量
漏预测。当前 DCache responder 又在 `GrantData` response record 创建时独立随机
`denied/corrupt`，RM 没有按物理地址取得该结果的共同状态。

本方案针对用户提出的约束：不支持 CBO、Probe、eviction 等运行期清除，只在 reset 或
testcase shared-state 初始化时清空错误状态。这样可省略可清除模型中的完整 history queue 和
各类清除回调，同时仍需保留 activation sample，避免后发错误污染早发访问。

## 3. 目标与明确不做的事项

### 3.1 目标

1. RM 不再读取 `main_view` 中任何异常字段。
2. TLB PF/AF/GPF 与 translation-side PMA AF 只从冻结 CSR context 和 `tlb_entry_by_key`
   推导。
3. DCache `GrantData` 的 `denied/corrupt` 以物理 64B 行建立 sticky ledger；后续同一物理行
   的适用访问复用该状态。
4. RM 查询使用实际访问 sample，支持跨 64B 行访问，并且不进行全表扫描。
5. 多 beat、D ready backpressure 和同线 pending request 不会导致错误位重新随机或丢失。
6. CBOAck、ReleaseAck、Uncache response 和 C-channel corrupt 不污染该 DCache 行账本。

### 3.2 不做的事项

- 不修改任何 Scala/Chisel RTL。
- 不在本方案中重复实现 post-TLB PMA/PMP `pmpcfg/pmpaddr/pmacfg/pmaaddr`；相关权限、属性和
  CSR 快照依赖独立的 [PMA/PMP 模型方案](./memblock_rm_pma_pmp_model_plan_20260828.md)。
- 不把 `CBO.clean/flush/inval` 或 Probe 当作清除操作。
- 不把 `dcache_corrupt_byte_mask_by_line` 改造成 L2 D-response 错误表；两者用途和粒度不同。
- 不将 CBOAck 的 denied/corrupt 传播为同地址普通 Load/Store 的 sticky 错误。
- 不为了强行覆盖非法协议组合而允许 `GrantData(denied=1, corrupt=0)`。

## 4. 选定语义：只在 reset 时清空

### 4.1 生命周期

```text
testcase/reset 初始化
    -> 清空 live map、pending map、generation 和诊断计数
运行期间
    -> NONE 只能变为 CORRUPT 或 DENIED
    -> CORRUPT 只能保持或升级为 DENIED
    -> 任何 CBO/Probe/eviction 不改变 ledger
testcase 结束
    -> 由下一次初始化统一清空
```

`sticky` 是本专项回归的显式测试语义，不是对真实 DCache replacement 生命周期的完整断言。
真实 RTL 的 L1 error meta 可能随 line replacement 被重建；本方案故意以物理地址为 key 保留
错误，因此同一物理地址即使重新 acquire，也会继续返回错误。该限制必须在 cfg 和回归名称中
明确标注。

### 4.2 行状态

建议使用单调状态，而不是两个互不关联的 bit：

```systemverilog
typedef enum bit [1:0] {
    L2_D_ERROR_NONE    = 2'd0,
    L2_D_ERROR_PENDING = 2'd1, // 仅用于 pending reservation 的候选状态
    L2_D_ERROR_CORRUPT = 2'd2,
    L2_D_ERROR_DENIED  = 2'd3  // 语义上同时包含 corrupt
} l2_d_error_state_e;

typedef struct {
    l2_d_error_state_e state;
    longint unsigned   generation;
    longint unsigned   corrupt_activation_sample;
    longint unsigned   denied_activation_sample;
    bit                source_valid;
    bit [9:0]          source;
    bit [1:0]          origin_kind;
} l2_d_error_line_record_t;

typedef struct {
    bit                valid;
    l2_d_error_state_e proposed_state;
    longint unsigned   accept_sample;
    longint unsigned   generation;
    bit [9:0]           source;
    int unsigned       response_record_id;
} l2_d_error_pending_record_t;
```

实际实现可以根据现有类型宽度调整字段，但必须保持以下不变量：

- `DENIED` 查询结果始终返回 `denied=1, corrupt=1`。
- `CORRUPT` 查询结果返回 `denied=0, corrupt=1`。
- `NONE` 返回 `denied=0, corrupt=0`。
- `PENDING` 只供 responder 复用 response snapshot；在最后一个 D beat fire 前不能作为 RM
  已生效的 live error。
- `generation` 只递增，不回退；无运行期清除时不需要 `clear_sample` 或 history queue。

### 4.3 与现有状态的隔离

新表必须与以下状态分离：

| 现有状态 | 粒度/用途 | 不能替代新表的原因 |
| --- | --- | --- |
| `main_mem` / `write_overlay_mem` | 1 KiB backing line 的数据和写覆盖 | 不记录 D response error |
| `dcache_corrupt_byte_mask_by_line` | C-channel `ProbeAckData/ReleaseData` 写回不可比较字节 | 是 C data 观察状态，不是 L2 D response 状态 |
| `cached_line_by_addr` | DCache 副本 alias/lifecycle | CBO/Probe 会删除它，而 sticky ledger 不允许清除 |
| `dcache_response_record_t` | 一笔尚未完成的 D response | 只保存短期 snapshot，不能承担后续访问查询 |

## 5. DCache responder 逻辑

### 5.1 A.fire 时准备 response snapshot

**抽象功能描述：** `accept_dcache_a_request()` 在 `AcquireBlock A.fire` 后生成唯一的
`GrantData` response record。它读取 live/pending ledger，决定本次 response 的固定错误位，
但不在此时把新错误发布为已生效状态。

源码级伪代码：

```text
accept AcquireBlock A.fire(line, source)：
  key = line[47:6]

  if live[key].state == DENIED:
      record.denied  = 1
      record.corrupt = 1
  else if live[key].state == CORRUPT:
      record.denied  = 0
      record.corrupt = 1
  else if pending[key].valid:
      record.denied  = pending[key].proposed_state == DENIED
      record.corrupt = 1
  else:
      record.denied = sample(GRANTDATA_DENIED_WT)
      if record.denied:
          record.corrupt = 1
          proposed_state = DENIED
      else:
          record.corrupt = sample(GRANTDATA_CORRUPT_WT)
          proposed_state = record.corrupt ? CORRUPT : NONE
      if proposed_state != NONE:
          pending[key] = reservation(record, proposed_state, A.fire sample)

  record.line_addr = aligned line
  record.denied/corrupt/data/source/sink/beat_count 固定写入 response queue
```

中文文字伪代码：先用 live map 判断该行是否已经污染；若已污染，后续 response 直接复用，
不再调用随机函数。若该行有尚未完成的 pending reservation，也复用候选状态。只有完全没有
状态时才按既有权重采样一次，并将非零结果放入 pending map。`record` 创建之后，scheduler、
多 beat 和 `D.ready` hold 只搬运字段，不能再次随机。

如果当前 DCache responder 对同一 physical line 已有 alias/owner 冲突限制，该限制继续保留；
pending map 不是放宽协议并发能力，而是防止允许的同线 outstanding 在错误结果上分叉。

### 5.2 最后一个 D.fire 时发布 sticky 状态

**抽象功能描述：** `process_d_fire()` 在一笔 `GrantData` 的最后一个 beat 真正 fire 后，
把 pending reservation 原子提交到 live map，并记录 activation sample。它不改变现有
GrantAck/E、cached-line 或 response queue 生命周期。

源码级伪代码：

```text
process last GrantData D.fire(record)：
  key = record.line_addr[47:6]
  if record.denied || record.corrupt:
      proposed = record.denied ? DENIED : CORRUPT
      if live[key].state == NONE:
          live[key].state = proposed
          live[key].generation++
          if proposed == CORRUPT:
              live[key].corrupt_activation_sample = current_sample
          else:
              live[key].corrupt_activation_sample = current_sample
              live[key].denied_activation_sample  = current_sample
      else if live[key].state == CORRUPT && proposed == DENIED:
          live[key].state = DENIED
          live[key].generation++
          live[key].denied_activation_sample = current_sample
      remove or resolve pending[key] for this response
  continue existing GrantAck/E handling
```

中文文字伪代码：错误 response 的可见时间定义为最后一个 D beat fire，而不是 A.fire、
response queue 入队或 scheduler 选中时刻。这样 RM 不会把尚未送达 DUT 的错误提前归因给
更早访问。若一条已为 `CORRUPT` 的行后来收到合法 `DENIED`，状态只能升级，不能回退；升级
前后通过各自 activation sample 区分访问时序。

### 5.3 不更新账本的 response

以下路径继续使用现有 response record，但不得调用 ledger 激活 helper：

- `AcquirePerm -> Grant`。
- `CBO.clean/flush/inval -> CBOAck`，包括 direct miss。
- `Release/ReleaseData -> ReleaseAck`。
- B/C channel 的 `ProbeAck/ProbeAckData/ReleaseData` corrupt 观察。
- `sbuffer_agent` Uncache 的 `AccessAck/AccessAckData`。

特别是 CBOAck 的 `denied/corrupt` 是 CMO 操作本身的错误，不代表该地址后续普通数据访问
必然错误；把它写入物理行表会造成错误污染。

## 6. RM 查询逻辑

### 6.1 异常来源边界

`observer_build_commit_item()` 的期望异常构造改为：

```text
TLB PF/AF/GPF/PMA AF
  <- frozen CSR request context + tlb_entry_by_key

DCache denied/corrupt
  <- query_l2_d_error_at_sample(pa, access_sample)

post-TLB PMA/PMP AF
  <- 独立 [PMA/PMP model](./memblock_rm_pma_pmp_model_plan_20260828.md)

main_view 的所有异常字段
  <- 禁止读取
```

主表仍可提供 opcode、VA、ROB、源寄存器和立即数，但不再复制或 OR 异常字段到 RM item。

### 6.2 按 sample 查询 sticky 状态

**抽象功能描述：** `query_l2_d_error_at_sample()` 根据物理 64B 行和访问 sample 返回该行
在该时刻已经生效的 raw error。该函数只做一次关联数组查找，不扫描所有行。

源码级伪代码：

```text
query_l2_d_error_at_sample(key, sample, output error)：
  error = {denied:0, corrupt:0, valid:1}
  if !live.exists(key): return 1
  rec = live[key]
  if rec.state == DENIED && rec.denied_activation_sample <= sample:
      error.denied  = 1
      error.corrupt = 1
  else if rec.state == CORRUPT && rec.corrupt_activation_sample <= sample:
      error.corrupt = 1
  return 1
```

如果状态尚未在 `D.fire` 提交，或者 response record 与 ledger 的 generation 无法对应，
调用者返回“事实未就绪”并等待；不能把未找到记录解释为确定的无错误。

### 6.3 Load、Store 和跨行访问

1. **Load**：使用 `memblock_rm_dut_writeback_observer` 关联 LDA record 的 `sample_cycle`。
   该时间代表 Load 的异常向量和数据有效性实际被观察到的 writeback 阶段。
2. **Store**：使用实际 STA/STD fault 或 store writeback observer event 的 sample。当前若
   observer 只有 `sta_cycle/std_cycle`，应新增值型 `store_actual_sample_view_t` 或明确选取
   产生异常的那个阶段；不能使用更晚的 ROB commit cycle 替代。
3. **跨 64B 行**：对 `item.pa_by_byte[]` 的有效 byte 计算 `paddr[47:6]`，每个不同 line
   最多查询一次，OR 所有 raw `denied/corrupt`。标量 8B 访问最多涉及两条 line。
4. **翻译、AF 和 DCache 事实门控**：PF/GPF/AF 阻止 PA 生成时不查询本账本。翻译成功后，RM 先
   从 PMA/PMP AF-only view 得到不依赖 DCache 事实的基础 AF；基础 AF 已产生时立即结束，既不读取
   也不等待 `dcache_fact`。仅基础 AF 为 0、PMA `C=0` 的标量数据访问才用 `dcache_fact` 完成
   cache-path AF 判定：`YES` 产生 AF，`NO` 不产生 C 属性 AF，`UNKNOWN` 仅在此时等待。只有最终
   无 AF 且 cache observer 已确认 `dcache_fact=YES` 时才查询 DCache ledger；`NO/UNKNOWN` 均不得
   查询。RM 不读取 `mmio/cacheable` 分类来作此判断，也不把 L2 `A.fire` 或“尚未见 A.fire”当成
   唯一 DCache 访问事实；PMA/PMP 与 cache observer 的边界以
   [PMA/PMP 模型方案](./memblock_rm_pma_pmp_model_plan_20260828.md) 为准。CBO 一律不查询本账本。
5. **访问先后**：只有 `activation_sample <= access_sample` 才将 sticky 错误应用到该访问。
   后发 request 的错误不会追溯到更早的访问；在此规则下不需要完整 history queue。

### 6.4 raw bit 到异常向量的映射

ledger 始终返回两个 raw bit，RM 通过单独 helper 按实际 DUT consumer 映射：

```text
denied=1, corrupt=1
  -> 对应数据路径的 Access Fault

denied=0, corrupt=1
  -> 对应路径的 hardwareError/数据错误（受 DUT cache_error_enable 语义约束）

denied=0, corrupt=0
  -> 不增加 DCache response error
```

当前 V2 scalar Load 路径已经确认 `denied` 产生 `loadAccessFault`，
`corrupt && !denied` 产生 `hardwareError`。Scalar Store 是否消费相同 DCache error meta，
必须以实际 `StorePipe/StoreUnit` writeback contract 为准；若该路径没有对应异常输出，RM
不得为了对称性擅自添加。该映射与 TLB PF/GPF 的异常位独立，最后再由既有 V2 priority
比较逻辑处理主 cause。

## 7. 建议的接口和实现落点

### 7.1 共享 memory/DCache 状态

文件：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`

新增或等价实现以下 helper：

```systemverilog
extern static function void reset_l2_d_error_ledger();
extern static function bit prepare_l2_d_error_for_grant(
    input  bit [47:0] line_addr,
    input  longint unsigned sample,
    output bit denied,
    output bit corrupt,
    output bit has_pending_reservation
);
extern static function void commit_l2_d_error_on_grant_d_fire(
    input  bit [47:0] line_addr,
    input  bit denied,
    input  bit corrupt,
    input  longint unsigned sample,
    input  bit [9:0] source
);
extern static function bit query_l2_d_error_at_sample(
    input  bit [47:0] line_addr,
    input  longint unsigned sample,
    output bit denied,
    output bit corrupt
);
```

这些 helper 的状态属于 DCache ledger owner；RM 不能直接访问关联数组。重置 helper 只在
testcase 初始化或 reset 生命周期调用，不出现在高频每拍路径之外的普通 CBO/Probe 分支中。

### 7.2 DCache responder

文件：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`

- `accept_dcache_a_request()` 的 `AcquireBlock` 分支调用 prepare helper。
- `process_d_fire()` 仅在最后一个 `GrantData` beat fire 时调用 commit helper。
- `build_current_d_xaction()` 继续从 response record 驱动 D bits，不增加第二个随机点。
- `clear_runtime_state()` 只清 response/pending runtime owner；不得误删 sticky live map，除非
  当前调用属于 testcase 初始化的显式 reset helper。

### 7.3 RM 只读 façade

文件：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv`

新增值型结构 `l2_d_error_view_t` 和 `read_l2_d_error_for_rm(line_addr, sample, view)`。
该 API 必须：

- 返回复制后的 bit 和 generation/sample，不返回 map handle。
- 对不存在行区分“合法 NONE”与“ledger 尚未 ready”；初始化完成后前者才可返回 valid=1。
- 不触发懒分配、不修改 ledger、不清除状态。

### 7.4 RM 构造和比较

文件：

```text
mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv
mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv
```

修改要点：

1. 从 `main_transaction_view_t` 复制异常字段的逻辑中移除 RM 使用路径；必要时删除 view 字段。
2. 在 PA geometry 完成后判断 DCache-eligible，再按 Load/Store sample 查询 ledger。
3. 把 raw DCache error 与 TLB fault 分别写入 RM item，不做来源混合。
4. 保留完整 raw expected/status/writeback 诊断；架构主 cause 比较继续使用 V2 priority。
5. Store sample 若现有 observer 无法唯一定位，应先扩展 observer 的值型事实，再接入 RM，
   不以 commit 时间或主表 delay 猜测。

## 8. 参数与 cfg 建议

为避免 sticky 语义污染普通回归，建议新增显式开关：

| 参数 | 默认值 | sticky 专项 preset | 作用 |
| --- | ---: | ---: | --- |
| `MEMBLOCK_L2_D_ERROR_STICKY_EN` | `0` | `1` | 是否启用物理行错误永久保持 |
| `MEMBLOCK_L2_GRANTDATA_DENIED_WT` | `0` | 按场景设置 | 首次无状态 `GrantData.denied` 采样权重 |
| `MEMBLOCK_L2_GRANTDATA_CORRUPT_WT` | `0` | 按场景设置 | 未 denied 时 `GrantData.corrupt` 采样权重 |

参数链路应遵守现有 `env/plus.sv -> seq_csr_common -> getter -> sequence` 规则。开关关闭时，
保持当前无 ledger 污染的旧行为；开关打开但两个权重均为 0 时，表仍可读取已有状态，但不会
产生新错误。若为了本专项固定采用 sticky，也必须通过专用 cfg 显式打开，而不是在 sequence
中写死。

## 9. 性能、并发和错误处理约束

- DCache A.fire、D.fire 和 RM 单笔查询均为关联数组 O(1) 操作，不扫描完整主表或完整行表。
- 跨行查询只扫描该访问覆盖的 byte/line，普通标量访问最多两条物理 line。
- 不允许在每拍 service loop 中遍历所有 ledger entry；reset/debug dump 属于低频例外。
- response record 的错误属性必须在创建点固定；D.ready backpressure、两拍 beat 和 scheduler
  reorder 不得重采样。
- 如果同线 pending reservation 与现有 alias owner 规则矛盾，报告协议/模型状态错误，不能
  静默合并成多个不一致状态。
- RM 查不到尚未发布但理论上应存在的 response fact 时，返回事实未就绪并等待；不能默认
  `denied=0/corrupt=0`。
- 所有 debug 一致性检查（例如 generation 不连续、重复 commit）遵循
  `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN`，不改变正常 response 主路径。

## 10. 验证计划与验收标准

### 10.1 定向场景

| 场景 | 预期结果 |
| --- | --- |
| 首次 `GrantData 0/0` | ledger 不建错误行，后续访问保持 0/0 |
| 首次 `GrantData 0/1` | 最后 D.fire 后行变为 `CORRUPT`；后续同线适用访问返回/预测 corrupt |
| 首次 `GrantData 1/1` | 最后 D.fire 后行变为 `DENIED`；后续查询返回 1/1 |
| 同线两个 pending request | 两笔 response 使用同一候选状态，不独立随机 |
| 错误 D.fire 前的早发访问 | 不应用 pending 错误 |
| 错误激活后的后发访问 | 按 sticky 状态应用错误 |
| `CBO.clean/flush/inval`、Probe(toB/toN)、eviction | ledger 状态保持不变 |
| reset/testcase 初始化 | live/pending map 和 generation 清空 |
| 跨 64B 行 8B 访问 | 逐行查询，任一行错误即可 OR 到访问结果 |
| TLB PF/GPF 或基础 PMA/PMP AF（即使 `dcache_fact=UNKNOWN`） | 立即结束，不读取/等待 DCache fact，也不查询 ledger |
| 基础 AF 为 0、PMA `C=0` + `dcache_fact=YES` | 产生 cache-path AF，不查询 ledger |
| 基础 AF 为 0、PMA `C=0` + `dcache_fact=NO/UNKNOWN` | `NO` 不产生 C 属性 AF 并结束；`UNKNOWN` 等待；两者均不查询 ledger |
| 最终无 AF 且 `dcache_fact=NO/UNKNOWN` | `NO` 结束，`UNKNOWN` 仅为 ledger 资格等待；均不凭空叠加 DCache error |
| CBOAck/Uncache error | 只影响该操作，不污染同物理行 |

### 10.2 RM 比较标准

1. `main_view` 异常字段被修改为任意值时，RM 结果不应改变。
2. TLB table 产生的 PF/AF/GPF 仍能独立预测。
3. LDA/Store 实际 sample 早于 activation sample 时不报 DCache 错误；晚于 activation 时
   才报错。
4. raw `denied/corrupt` 诊断与 DUT response record 一致；不合法 `1/0` 不得出现。
5. sticky 专项回归完成目标笔数时，`UVM_ERROR/UVM_FATAL` 为 0；若出现 RTL 侧异常，记录
   日志、FSDB 和最小复现信息，不修改 RTL。

## 11. 实施顺序与完成条件

### 11.1 实施顺序

1. 取得明确代码修改授权并确认工作区状态。
2. 先在共享 sequence 中实现 ledger 类型、初始化、prepare、commit、query helper。
3. 将 DCache `GrantData` response record 接入 prepare/commit，保持原有 D/E/Probe 生命周期。
4. 扩展 RM readonly API 和 writeback observer 的 Store sample 事实。
5. 修改 RM 异常构造，移除 `main_view` 异常来源并接入 DCache query。
6. 增加 plus/getter/preset 配置和必要的参数管理文档。
7. 运行定向测试、编译检查，再执行目标回归。

### 11.2 完成条件

- 代码、参数、cfg、sequence/filelist 和只读 API 均通过编译。
- 定向场景覆盖表中的状态转换和无清除行为全部通过。
- RM 不再引用 `main_view` 异常字段。
- sticky 状态仅在 reset/testcase 初始化清空，CBO/Probe 不会删除。
- 10000 笔目标回归通过，或根据用户既定规则定位到 RTL 问题并记录波形路径后停止。

## 12. 已知风险与后续增强

1. sticky ledger 按物理地址持久化，不能代表真实 replacement 后 L1 error meta 的生命周期；
   只能用于显式专项 preset。
2. 如果后续需要验证 CBO/Probe 清除或 line replacement 语义，应另建“可清除 generation/history
   方案”，不能在本 plan 中偷偷恢复清除。
3. 当前 V2 scalar Store 对 DCache `corrupt-only` 的最终异常消费路径需要以实际 WB/LSQ
   信号确认；未确认前只记录 raw diagnostic，不擅自增加期望异常 bit。
4. post-TLB PMA/PMP AF、CSR context 以及实际 CACHE 请求的 PMA `C=0` cache-path AF 由独立
   [PMA/PMP 模型方案](./memblock_rm_pma_pmp_model_plan_20260828.md) 负责；U 态默认 PMP deny
   导致的基础 AF 不能由本 DCache ledger 解释，也不得等待 `dcache_fact`。RM 不消费
   `mmio/cacheable` 分类；两套状态表实现时必须保持值型只读接口和“基础 AF -> C 属性 AF ->
   DCache ledger”的调用顺序。

本文完成后仍处于 `undo`，因为尚未获得代码修改授权，也未进行 coding 或仿真验证。
