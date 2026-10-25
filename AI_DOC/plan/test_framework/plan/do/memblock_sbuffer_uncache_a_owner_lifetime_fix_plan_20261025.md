# MemBlock SBuffer Uncache A Owner 生命周期修复计划

## 1. 术语、目标与责任边界

| 术语 | 通俗解释 | 代码落点 |
| --- | --- | --- |
| slot generation | 一个 `LoadQueueUncache` slot 从 `req_valid` 首次置 1 到可观察清零的单次占用；连续 valid 不允许换 ROB | DUT 16 个 Uncache entry；待新增 slot cache |
| slot owner cache | slot generation 首次可见时冻结的旧动态实例身份 | 待新增 `uncache_slot_owner_state[16]` |
| source-bound owner | `slaveAccept/slaveId` 有效后，与 TL A source 绑定的 slot owner | slot state 的 `source_valid/source` |
| A-consumed slot | 已参与真实 A.fire、但仍可能等待 writeback 而保持 `req_valid=1` 的旧 generation | slot state 的 `a_consumed` |
| armed A snapshot | responder 驱动 A.ready 后，等待下一 sample 确认 A.fire 的 A payload 与 owner 副本 | 现有 `a_accept_armed/armed_a_req_xact` 及待新增 owner snapshot |
| armed slot mask | A arm 时冻结的精确物理 slot 集合 | `armed_a_owner_slot_mask` |
| active map | 当前 ROB/LQ/SQ 到 active UID 的公共映射；redirect 后必须删除旧实例 | `lookup_active_uid_by_rob()` 等 |
| redirect-deleted owner | redirect 删除 active map 前保存的单代旧 UID/epoch/LQ 身份 | `redirect_deleted_lq_owner_by_key` |
| immutable ledger | A.fire 建立、D.fire 完成，固定 source generation 与 owner 的记录 | Uncache D-error ledger |
| global stop | 主流程不再接纳新工作，只允许 stop 前已拥有的请求自然 drain | `is_global_stop_requested()` |

目标是修复测试框架在外部 `A.fire` 时才回查 active ROB owner 的过晚冻结。新实现只调整 owner provenance 生命周期，不修改 RTL/Scala、DUT 接口、RM expected、active map retirement、Uncache A/D 激励、shared memory 数据或 D-error ledger 结果语义。

## 2. 首错证据与最终行为

seed `133409275` 中 UID240/ROB `0/74` 在 `32552.600ns` 被 redirect 退休，slot 5 在 `32555.600ns` 才形成 A.fire。seed `940141520` 中 UID654/ROB `1/8` 最近一次在 `153552.700ns` 退休，slot 12 在 `153660.200ns` 才 A.fire。两例 DUT slot 都在合法 drain，框架却在 A.fire 回查已删除 active map 并 fatal。

最终行为：

```text
slot 首次 req_valid
  -> 从 active ROB 冻结 UID/epoch/ROB/LQ；若 active map 已被同 sample redirect 删除，
     仅允许从当前单代 redirect-deleted owner 精确恢复同 ROB 的旧 UID/epoch/LQ
slaveAccept/slaveId
  -> 给冻结 owner 绑定 source
当前 A.valid 可接收
  -> 按 A source 把未消费的 source-bound owner 复制进 armed snapshot，冻结 slot mask并驱动 A.ready
下一 sample A.fire
  -> 使用 armed snapshot 建 response record 与 immutable ledger；成功后只消费 mask 中 slot
A.valid withdrawal
  -> body 原子清 armed payload、owner snapshot与mask，slot cache 保留且不消费
slot req_valid=0
  -> 清该 slot generation
```

## 3. 字段语义与唯一真源

修改文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
```

新增字段的源码级伪代码：

```systemverilog
typedef struct packed {
    bit valid;
    bit existed_before_global_stop;
    bit source_valid;
    bit [3:0] source;
    bit a_consumed;
    uncache_d_error_owner_t owner;
} uncache_slot_owner_state_t;

uncache_slot_owner_state_t uncache_slot_owner_state[16];
bit armed_a_owner_snapshot_valid;
bit [3:0] armed_a_owner_source;
int unsigned armed_a_owner_count;
bit [15:0] armed_a_owner_slot_mask;
uncache_d_error_owner_t armed_a_owners[16];
```

中文详细伪代码：`valid` 表示当前 slot generation 已冻结 owner，唯一设置点是首次 `req_valid` 捕获，唯一清理点是 `req_valid=0` 或 reset。`source_valid/source` 只由 `slaveAccept/slaveId` 首次绑定。`a_consumed` 只在真实 A.fire 成功建立 response 后置位，withdrawal不置位，并随 generation 结束/reset清零。armed owner字段和slot mask由A arm建立，并由`body()`在A.fire、withdrawal或reset路径与A payload一起原子清理。

不新增 plus/cfg 参数。16 slot 每拍扫描是固定上界 O(16)，属于 responder 高频路径可接受的物理窗口扫描，不扫描主表或 transaction 总数。

## 4. 主流程实现 Flow

### 4.1 `sbuffer_mem_access_base_sequence::body()`

抽象功能描述：维持 Uncache responder 的 A/D 握手、response scheduler 和 global-stop drain。修改后它还是 armed 状态的唯一 owner，负责调用 slot cache service，并原子管理 A payload 与 owner snapshot。

源码级伪代码：

```text
on each sbuffer drv_cb sample:
  sample A.valid/D.ready and reset
  if reset:
    clear_runtime_state()
  else:
    service_uncache_slot_owner_cache(global_stop)
    if a_accept_armed:
      if sampled A.valid:
        capture fired payload and check stable
        create response using armed owners
        mark exact armed slot mask A-consumed
      clear all armed payload/owner/mask fields atomically
    process D.fire and watchdogs

  preserve existing pre-stop A payload logic
  build response/idle outputs
  if current A can be accepted:
    capture A payload
    capture_armed_uncache_load_owners(opcode, source)
    set a_accept_armed and A.ready
  publish outputs
```

中文详细伪代码：每个非 reset sample 先维护16项slot cache。上一拍已arm且A.valid保持时，用显式owner snapshot建response/ledger，成功后逐位校验slot mask仍指向同source且未消费的generation，再置`a_consumed=1`；withdrawal不建record也不消费slot。两条路径都原子清payload、owner和mask。当前拍GET只从未消费的source-bound cache复制owner并冻结精确mask；PUT建立count=0、mask=0的空snapshot。这样source在D返回后被复用时，尚未清零的旧slot不会再次归属。

### 4.2 Global-stop Flow

抽象功能描述：global stop 后禁止新的 slot generation，只允许 stop 前已冻结的 slot generation 继续完成 source bind、A/D handshake 和 ledger drain。

源码级伪代码：

```text
if global stop:
  new req_valid rising on uncached slot -> fatal
  cached pre-stop slot remains valid even if source binds or A.valid appears after stop
  A.valid source matches at least one pre-stop source-bound slot -> may arm and drain
  A.valid has no pre-stop owner and is not existing pre_stop_a_snapshot -> fatal
  terminal only when slot cache empty, armed snapshot empty, A/D/response state empty
```

中文详细伪代码：`service_uncache_slot_owner_cache()` 在 global stop 时遇到 state invalid 而 `req_valid=1`，说明 stop 后首次出现新 generation，立即 fatal；stop 前已冻结的 state 继续等待 `slaveAccept` 并绑定 source。body 的 stop 后 A admission 不再只依赖 `pre_stop_a_snapshot_valid`：GET 只要 source 能从 `existed_before_global_stop=1` 的 source-bound slot cache构造 armed owner snapshot，就允许驱动 A.ready；PUT 仍只允许现有 pre-stop A payload。无法匹配旧 owner 的 stop 后 GET 属于新请求并 fatal。terminal 和 audit 加入 slot cache 与 armed owner 状态，绝不主动清除仍由 DUT `req_valid` 持有的 generation。

## 5. 关键 Helper 细节

### 5.1 `service_uncache_slot_owner_cache()`

抽象功能描述：在每个 SBuffer responder sample 维护 16 个物理 slot generation 的冻结 owner和 source 绑定；它不建立 response，不修改 active map，也不消费 redirect 记录。

输入/输出与副作用：输入来自 `uncache_owner_vif.mon_cb` 和 global-stop 状态；输出保存在 `uncache_slot_owner_state[16]`。首次捕获会只读 `common_data_transaction`；错误身份、连续 valid reuse或非法 stop 后 generation 会 fatal。

源码级伪代码：

```text
for slot 0..15:
  validate req_valid/ROB/slave fields
  if req_valid == 0:
    clear slot state
    continue
  if state.valid == 0:
    if global stop: fatal new post-stop generation
    owner = capture owner by active ROB
    if active ROB missing:
      owner = find unique redirect-deleted LQ owner by exact ROB
      if no exact unique match: fatal
    state.valid = 1; state.existed_before_global_stop = 1
  else if observed ROB != frozen ROB:
    fatal occupied slot changed identity without req_valid low
  if slaveAccept:
    if !source_valid: bind slaveId
    else if source changed: fatal
```

中文详细伪代码：helper 先验证 `req_valid` 和有效 slot 的 ROB/slave 字段没有 X/Z。`req_valid=0` 表示 generation 结束并清除含`a_consumed`的全部state。首次捕获时`a_consumed=0`；active miss只允许单代redirect fallback。连续valid的ROB改变fatal，同generation source改变fatal。redirect、needFlush、status inactive不清缓存；已消费generation可继续等待writeback/`req_valid=0`，但不再参与后续同source的owner capture。

### 5.2 `find_unique_redirect_deleted_load_owner_by_rob()`

抽象功能描述：只在 slot 首次可见但 active ROB 已被同 sample/相邻 sample redirect 删除时，从当前单代 redirect-deleted LQ owner 表恢复精确旧实例；这是窄化 fallback，不是普通 owner 查询路径。

输入/输出与副作用：输入完整 ROB key；输出 `memblock_redirect_deleted_owner_t` 和 LQ key；只读当前 redirect-deleted 表与 main/status，不删除记录、不延长窗口、不修改 active map。

源码级伪代码：

```text
matched = 0
for each redirect_deleted_lq_owner entry:
  validate entry belongs to current single redirect generation
  main = get_main_transaction(entry.uid)
  status = get_status(entry.uid)
  if main ROB == requested ROB and status.dynamic_epoch == old_epoch + 1:
    save owner and LQ key; matched++
return matched == 1
```

中文详细伪代码：函数遍历当前受限的 redirect-deleted LQ owner关联数组；每项先确认 valid，且其 redirect epoch/cancel record属于当前唯一 redirect generation。函数用 owner UID读取 main transaction取得旧请求固定 ROB，用 status确认 reissue后 dynamic epoch正好是 `old_dynamic_epoch+1`。只有 ROB完整匹配且 epoch关系成立才计为候选，并保存该 map key对应的 LQ flag/value。最终必须恰好一个候选；零个表示无同拍 fallback证据，多个表示身份不唯一，两者都返回失败。该扫描仅在首次 active ROB lookup miss时发生，不在普通每拍路径扫描。

### 5.3 `capture_armed_uncache_load_owners()`

抽象功能描述：在准备驱动 A.ready 时按 A source 从 slot cache构建不可变 armed owner snapshot；它不查询 active map。

输入/输出与副作用：输入 opcode、source和 global-stop状态；写 armed owner valid/source/count/array。GET至少一个 owner，PUT count=0。

源码级伪代码：

```text
require no existing armed owner snapshot
clear temporary owner array
if opcode is PUT: set valid snapshot with count 0; return
for each slot state:
  if valid && !a_consumed && source_valid && source matches:
    if global stop && !existed_before_global_stop: fatal
    append frozen owner
    set corresponding armed slot mask bit
if count == 0: fatal
publish armed snapshot
```

中文详细伪代码：函数先确认没有遗留armed snapshot并清mask。PUT建立count=0、mask=0的空snapshot。GET只复制valid、未消费、已绑定且source相同的owner，并设置对应slot mask；global stop时还要求stop前冻结。零匹配fatal；否则一次性发布valid/source/count/owners/mask。真实A.fire后消费mask中slot，withdrawal只清snapshot不消费。

### 5.4 `create_uncache_response_record()`

抽象功能描述：对已经确认 A.fire 的 payload执行内存访问、错误注入、response record和 D-error ledger建立；修改后 owner完全由调用者传入的 armed snapshot提供。

源码级伪代码：

```text
create_uncache_response_record(req, accept_cycle, owner_count, owners):
  check response capacity and decode opcode
  perform shared memory access
  fill response payload
  if GET: require owner_count > 0
  if PUT: require owner_count == 0
  copy explicit owners into response record
  sample denied/corrupt once
  begin immutable ledger
  note source accept and enqueue response
```

中文详细伪代码：task保持现有capacity检查、opcode解码、shared memory读写和错误随机。它不再调用旧 `capture_uncache_load_owners()`。GET要求body传入至少一个owner，PUT要求零owner；随后把显式owner复制进 response record。`begin_uncache_d_error_record()`继续建立不可变source generation账本，`note_uncache_source_accept()`继续维护A/D source outstanding，最后将record入队。task不清armed字段，清理职责全部留在body。

### 5.5 `clear_runtime_state()` 与 audit

抽象功能描述：reset时清全部 responder私有状态；terminal audit只读报告残留。

源码级伪代码：

```text
clear_runtime_state:
  abort existing pending ledger using current rule
  clear A payload, owner snapshot, all 16 slot states, response/D/source state

audit:
  check A/D/queue/timer/write batches
  report every valid slot owner and any armed owner snapshot
  do not delete residual state
```

中文详细伪代码：reset路径在现有 pending ledger abort之后，清空16项slot state和所有armed owner字段，再执行原response/source/watchdog清理。audit在原A/D/queue检查之后遍历固定16项，打印残留 UID/epoch/ROB/LQ/source和slot；armed snapshot残留打印source/count。audit只置 `clean=0` 并报告错误，不修复或删除状态。

## 6. 不变量与保持边界

1. 正常新 generation 必须观察到 `req_valid=0 -> 1`；连续 valid ROB变化无条件 fatal，与 RTL occupied-slot invariant一致。
2. redirect不清 slot cache；active map删除后，普通路径不再回查它。
3. redirect fallback只接受当前单代临时表、完整ROB唯一匹配和 `old_epoch+1`，不接受历史或猜测 owner。
4. armed payload和armed owner由body在 fire、withdrawal、reset原子清理；record helper不拥有清理职责。
5. global stop 后禁止新 slot generation；stop前冻结generation可在stop后完成source bind、A/D和ledger drain。
6. PUT不创建Load owner；GET无owner fatal。
7. source outstanding/reuse、response capacity、D backpressure、response reorder、shared memory和错误注入保持不变。

## 7. 文档与验证 Flow

文档：更新本周 RM/测试框架 Buglist；coding后新增 implementation review，并检查 `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md` 是否需要同步 slot owner生命周期。

验证：

1. 静态：`git diff --check`；旧 A.fire active-map捕获函数不得再被生产路径调用。
2. 编译：新 mode VCS及Verdi KDB 0 error。
3. 原始 seeds `133409275`、`940141520`，原 testcase/cfg/1000 requests，要求 PASS。
4. directed/software：首次req_valid正常冻结；首次req_valid与redirect retirement同sample命中精确 fallback；redirect在source bind前、arm前、fire前；A.valid withdrawal/reappearance；slot低电平后reuse；连续valid ROB变化fatal专项；多slot同source；PUT空owner；reset。
5. global stop：stop前冻结但stop后才source bind/A.valid可drain；stop后新slot generation fatal；terminal cache/armed均为空。
6. backpressure：capacity stall、A.ready延迟、D.ready延迟、response reorder下owner不丢不重复。
7. 至少10个新 `tc_dispatch_real_mmu_sv39_smoke` 随机seed全PASS，`UVM_ERROR=0`、`UVM_FATAL=0`。
8. implementation review覆盖字段、fallback、主循环、stop、reset、audit与plan对齐；专项通过后独立git commit。

## 8. 验收标准

- 独立plan review最终为 `REVIEW_PASS`。
- 原始2 seeds、directed和随机专项通过。
- A.fire不再查询active ROB；旧UID/epoch正确进入D-error ledger。
- 同拍redirect fallback只有精确单代唯一匹配可用。
- global stop、withdrawal、slot reuse、source reuse、reset无泄漏或误绑定。
- 不修改RTL/Scala/RM，不混入其他问题提交。

## 与初步 plan 差异说明

修改目的：首轮review指出global-stop admission、连续valid reuse、同拍redirect顺序和armed清理职责未闭合，因此正式方案收紧为可证明的单generation生命周期。

修改前逻辑行为：初步方案允许连续`req_valid=1`时ROB变化后重新捕获；global stop仍沿用仅`pre_stop_a_snapshot`接纳；active map miss没有精确fallback；`create_uncache_response_record()`和body共同清armed状态。

修改后逻辑行为：连续valid ROB变化无条件fatal；global stop后只有stop前已冻结slot generation或既有pre-stop PUT payload可以继续；active lookup miss只调用 `find_unique_redirect_deleted_load_owner_by_rob()`，该helper从当前单代redirect表按完整ROB与epoch关系唯一恢复；body独占armed payload/owner的建立与原子清理，record task只消费显式snapshot。

差异影响：新增一个受限fallback helper和slot state中的stop前标志，不新增参数，不延长active map或redirect临时表生命周期。普通owner捕获仍是O(1) active ROB查询；仅miss时扫描受限redirect-deleted LQ集合。global stop语义从“A.valid必须stop前可见”扩展为“DUT内部slot generation必须stop前已冻结”，与被动responder自然drain一致。

### 新增 helper：`service_uncache_slot_owner_cache()`

函数名：`service_uncache_slot_owner_cache()`。

抽象功能描述：该 helper 在 `sbuffer_mem_access_base_sequence::body()` 每次非 reset 采样开始时维护 16 个 DUT uncache slot 的 owner generation。它冻结 slot 首次可见时的 Load owner，并在后续 `slaveAccept` 出现时给该 generation 绑定 TileLink source。

添加原因：原实现直到 TileLink A.fire 才按 ROB 查询 active owner；redirect 已经退休该 ROB 时，DUT 内部仍合法 drain 的 slot 会失去框架 owner。必须在 slot 生命周期更早阶段保存身份，且不能通过延长 active map 生命周期掩盖问题。

功能说明：该 helper 是 DUT slot 状态与 responder armed snapshot 之间的生命周期桥梁。它只维护 slot cache，不建立 A response、不创建 D-error ledger，也不删除 transaction 或 redirect 记录。

详细文字伪代码：函数按 slot index 0 到 15 固定扫描 `uncache_owner_vif.mon_cb`。每个 slot 先检查 `req_valid`、ROB、`slaveAccept/slaveId` 中当前需要使用的字段没有 X/Z。若 `req_valid=0`，说明当前 generation 已结束，函数清除此 slot 的 valid、stop 标志、source 绑定和 owner 后处理下一项。若 `req_valid=1` 且缓存无 valid generation，global stop 已生效时将其判定为 stop 后新请求并 fatal；否则先调用 active ROB lookup，该查询用完整 ROB flag/value 返回当前 active UID，随后读取 main/status 校验 UID、ROB、LQ 和 dynamic epoch 并构造 frozen owner。active lookup miss 时调用 `find_unique_redirect_deleted_load_owner_by_rob()`；该 helper 只从当前单代 redirect-deleted LQ owner 集合恢复一个完整 ROB 和 epoch 关系都匹配的旧 owner。两条路径都不能得到唯一 owner时 fatal。捕获成功后设置 state valid，并把 `existed_before_global_stop` 记录为真。若缓存已有 valid generation，函数比较观测 ROB 与 frozen ROB；连续 `req_valid=1` 而 ROB 改变表示 occupied slot 被覆盖，直接 fatal，不执行 reuse。最后处理 source：`slaveAccept=1` 且 source 尚未绑定时保存 `slaveId`；已经绑定而 `slaveId` 改变时 fatal。函数副作用是更新固定 slot cache，供当前或后续 A admission 构造 armed snapshot。

### 新增 helper：`find_unique_redirect_deleted_load_owner_by_rob()`

函数名：`find_unique_redirect_deleted_load_owner_by_rob()`。

抽象功能描述：该 helper 仅服务于 slot 首次捕获时 active ROB lookup miss 的窄化恢复路径。它从当前 redirect generation 的临时 deleted-LQ owner 集合中，按完整 ROB 和 dynamic epoch 关系寻找唯一旧 owner。

添加原因：redirect retirement 与 slot `req_valid` 可能在同一采样顺序内发生，active map 会先删除 owner。普通历史搜索会引入 UID/ROB reuse 误绑定，因此需要一个只接受当前单代证据的独立 helper。

功能说明：该 helper 只读 redirect-deleted owner、main transaction 和 status，不消费记录、不延长保存窗口、不修改 active map。返回成功仅表示存在一个可证明与当前 slot 相同的 redirect 前 Load generation。

详细文字伪代码：函数将匹配计数和输出 owner/LQ key 初始化为空，然后遍历当前 redirect-deleted LQ owner 关联数组。每个 entry 先验证属于当前唯一 redirect generation，并取得其中保存的旧 UID、old dynamic epoch 和 LQ key。函数调用 main transaction 查询，以旧 UID 读取固定 ROB；调用 status 查询，以同一 UID 读取 redirect 后 dynamic epoch。只有 main ROB 与输入 ROB 的 flag/value 完整相等，且 status dynamic epoch 等于 `old_dynamic_epoch + 1` 时才构造候选。首个候选保存 UID、old epoch、ROB、LQ 和 slot 所需 owner 字段；后续候选继续累计而不覆盖唯一性判断。遍历结束后，匹配数恰好为 1 才返回成功并输出 owner；0 表示缺少同代 redirect 证据，超过 1 表示身份不唯一，均返回失败且调用者 fatal。该 helper 不产生其他状态副作用。

### 新增 helper：`capture_armed_uncache_load_owners()`

函数名：`capture_armed_uncache_load_owners()`。

抽象功能描述：该 helper 在 `body()` 决定驱动 A.ready 时，把与当前 A source 关联的 frozen slot owners 复制成不可变 armed snapshot。下一 sample 的 A.fire 只消费该 snapshot，不再查询 transaction active 状态。

添加原因：slot cache 会继续随 `req_valid` 和 source bind 变化，而已 arm 的 TileLink A payload 必须在 backpressure、redirect 或 slot drain 后保持同一身份。因此需要独立且原子发布的 armed owner 副本。

功能说明：该 helper 连接 source-bound slot cache 与 `create_uncache_response_record()`。GET 产生至少一个 Load owner，PUT 产生 owner count 为 0 的合法空 snapshot；它不执行内存访问或 ledger 操作。

详细文字伪代码：函数先确认当前没有残留 armed owner snapshot，并清零临时 count、owner array和slot mask。PUT发布count=0、mask=0的snapshot。GET只选择valid、`a_consumed=0`、source匹配的generation，并设置对应mask位；global stop时还要求stop前冻结。零匹配fatal，否则一次性发布valid/source/count/owners/mask。真实A.fire成功建record后仅消费mask中slot；withdrawal只清snapshot/mask，不消费slot。

### 修改函数：`sbuffer_mem_access_base_sequence::body()`

函数名：`sbuffer_mem_access_base_sequence::body()`。

修改原因：原主循环只冻结 A payload，没有在 DUT slot 生命周期冻结 owner，且 A.fire 时调用 record helper 回查 active ROB。redirect 和 backpressure 会让该查询晚于 owner retirement。

抽象功能描述：`body()` 是 SBuffer responder 主循环，负责采样 A/D、驱动 ready/valid、调度 response、处理 reset/global stop，并管理所有 armed 状态。修改后它同时成为 armed payload 和 armed owner snapshot 的唯一生命周期 owner。

修改前文字伪代码：每个 sample 先处理 reset、上一拍 armed A accept、D.fire 和 watchdog。上一拍已经驱动 A.ready 且当前 A.valid 时，body 检查 payload 稳定后调用 `create_uncache_response_record(req, cycle)`；该 task 内部按 A source/ROB 查询 active owner、执行 shared memory、建立 response 与 ledger。当前 A 可接收时，body 只冻结 A opcode/source/address/mask/data 并设置 `a_accept_armed`。global stop 后 A admission 主要依赖 `pre_stop_a_snapshot`，slot 内部请求在 stop 前存在但 A.valid 尚未出现时无法证明。A.fire 或 withdrawal 的 payload 清理与 record task 内 owner 捕获职责分散。

修改后文字伪代码：每个非 reset sample 开始先调用 `service_uncache_slot_owner_cache()`，由该 helper维护 fixed 16-slot frozen owner/source generation。随后处理上一拍 armed A：A.valid 保持时验证 payload 稳定，并调用 `create_uncache_response_record(req, cycle, armed_count, armed_owners)`；该 task只消费显式 owner，不查询 active map。record成功建立后按armed slot mask校验并将精确generation置`a_consumed=1`。A.valid撤销时不创建record也不消费slot。无论fire还是withdrawal，body在分支尾部同时清`a_accept_armed`、A payload、armed owner valid/source/count/array/mask。之后保持原 D.fire、response scheduler、source outstanding 和 watchdog 流程。当前 A 可接收时，body 先冻结 payload，再调用 `capture_armed_uncache_load_owners()`；该helper对GET只从未消费的source-bound slot cache复制owner并冻结slot mask，对PUT创建count=0、mask=0 snapshot，成功后 body 才驱动 A.ready。global stop 下 GET admission 接受 stop 前冻结且可能 stop 后才 bind source 的 slot generation，PUT仍使用已有 pre-stop payload证明。reset 调用 `clear_runtime_state()` 原子清所有 payload、slot 和 owner状态。

差异影响：A.fire 的 owner解析从晚期 active-map查询变为 slot-first freeze 和 arm-time复制，改变的是测试框架身份生命周期，不改变 TileLink响应、内存数据或错误随机语义。每拍新增固定 O(16) slot扫描，A arm新增固定 O(16) source匹配。

### 修改函数：`create_uncache_response_record()`

函数名：`create_uncache_response_record()`。

修改原因：该 task 当前同时承担 owner查询和 response创建，导致 owner捕获发生在 redirect可能已经删除 active map 的 A.fire 时刻，并造成 armed清理职责不清。

抽象功能描述：该 task 在 A.fire 后执行 shared-memory 请求、生成 response record、采样错误注入并建立不可变 D-error ledger。修改后 owner成为调用者提供的显式输入。

修改前文字伪代码：task 检查 response capacity并解码 opcode，执行 shared memory读写和 response payload填充。GET路径调用 `capture_uncache_load_owners(source/ROB)`；该旧 helper 从当前 active ROB/transaction 状态收集 Load owner，lookup miss会 fatal。task随后采样 denied/corrupt，调用 `begin_uncache_d_error_record()`建立 ledger，调用 `note_uncache_source_accept()`登记 source outstanding，并把 record 入队；部分 owner/armed相关状态由task和body共同维护。

修改后文字伪代码：task 增加 `owner_count` 和 `owners[]` 输入，先保持原 capacity、opcode、shared memory和 payload流程。GET要求输入 count 大于0，PUT要求 count 等于0；不再调用任何 active ROB owner捕获 helper。task把输入 snapshot逐项复制到 response record，随后保持原 denied/corrupt单次采样。`begin_uncache_d_error_record()`用record中的显式 owner创建 immutable ledger，`note_uncache_source_accept()`保持 source outstanding规则，最后入队。task不修改或清理 armed owner字段，调用者body负责生命周期。

差异影响：task接口增加显式 owner参数，删除生产路径的 A.fire active-map依赖。内存访问、错误概率、response排序和 source reuse语义不变。

### 修改函数：`clear_runtime_state()`

函数名：`clear_runtime_state()`。

修改原因：新增 slot cache 和 armed owner snapshot 后，reset必须与现有 A/D/response状态一起原子清除，否则 reset 后可能复用旧 generation。

抽象功能描述：该函数在 responder reset 时终止或清理当前私有运行状态，使下一拍从空闲状态重新开始。它是 reset 清理的唯一入口。

修改前文字伪代码：函数按现有规则 abort pending D-error ledger，然后清 A payload/armed标志、response queue、D drive、source outstanding、timer、write batch 和 watchdog 状态。函数不知道 DUT slot owner generation，也没有 armed owner array。

修改后文字伪代码：函数先保持原 pending ledger abort顺序，再同时清A payload、armed owner valid/source/count/array和slot mask。随后遍历16项`uncache_slot_owner_state`，清valid、stop标志、source绑定、`a_consumed`和owner字段。最后执行原 response queue、D drive、source outstanding、timer、write batch和watchdog清理。该函数不尝试从 reset前 slot恢复owner。

差异影响：reset覆盖新增状态，确保 owner generation不会跨reset泄漏；原 ledger abort和其他 responder状态语义不变，固定增加 O(16) 清理。

### 修改函数：`audit_sbuffer_responder_state()`

函数名：`audit_sbuffer_responder_state()`。

修改原因：terminal/global-stop audit 若不检查新增 slot和armed owner状态，可能在 DUT request仍占用或 snapshot泄漏时误报 responder clean。

抽象功能描述：该函数在终止检查阶段只读验证 responder 是否完全 drain，并报告所有残留状态。它不修复或删除状态。

修改前文字伪代码：audit检查 armed A payload、D drive、response queue、source outstanding、timer、write batch和现有 ledger；发现残留时打印上下文并将 clean置0。函数不检查 slot generation或 armed owner array。

修改后文字伪代码：audit先执行全部原检查，再遍历16项 slot state；每个valid项报告slot、UID、dynamic epoch、ROB、LQ、source bind、`a_consumed`和stop前标志，并将clean置0。若armed owner snapshot valid或slot mask非零，则报告A source、owner count、mask以及每项owner身份，并将 clean置0；若 snapshot valid/count/opcode关系不合法也报告一致性错误。函数只读这些字段，不调用clear helper、不消费redirect记录，最终把 clean结果返回上层 terminal flow。

差异影响：终止条件纳入新增生命周期状态，防止错误提前结束；只增加固定 O(16) audit成本，不影响正常握手路径。
