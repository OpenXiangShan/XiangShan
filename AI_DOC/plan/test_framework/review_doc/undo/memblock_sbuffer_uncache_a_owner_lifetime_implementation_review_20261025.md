# SBuffer Uncache slot owner 生命周期实现 Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `slot generation` | 一个 LoadQueueUncache 物理 slot 从 `req_valid=1` 到归零的单次占用 | `uncache_slot_owner_state[16]` | slot 5 持有 UID240；redirect 后仍属于同一 generation |
| `frozen owner` | slot 首次有效时保存的旧动态实例身份 | `uncache_slot_owner_state[].owner` | UID、dynamic epoch、ROB、LQ、slot 一起冻结 |
| `active map` | 当前动态实例的 ROB 到 UID 反查表 | `uid_by_active_rob` / `lookup_active_uid_by_rob()` | redirect 退休旧实例后正确删除，不能延长生命周期 |
| `redirect-deleted owner` | 当前单代 redirect 删除 active LQ owner 前保存的短时身份 | `redirect_deleted_lq_owner_by_key` | slot 首次有效与 redirect 删除同拍时的唯一 fallback |
| `armed owner snapshot` | A.ready arm 时按 source 复制的不可变 frozen owner 集合 | `armed_a_owner_*` | A.valid withdrawal 或 A.fire 后与 payload 原子清除 |
| `A-consumed slot` | 已参与真实 A.fire、但仍可能等待 writeback 而保持 `req_valid=1` 的旧 generation | `uncache_slot_owner_state[].a_consumed` | 排除 source 返回后被新请求复用时的旧 owner |
| `armed slot mask` | 当前 A arm 精确捕获的物理 slot 集合 | `armed_a_owner_slot_mask` | A.fire 只消费 mask 中 slot；withdrawal 不消费 |
| `global stop` | 主表和 runtime 已收敛后 responder 只允许旧请求 drain 的终止阶段 | `is_global_stop_requested()` | stop 前 slot 可在 stop 后 bind source，stop 后新 slot fatal |
| `D-error ledger` | Uncache A/D generation 与 Load owner 的不可变错误来源账本 | `begin_uncache_d_error_record()` | D.denied/corrupt 归属到正确旧 UID/epoch |

本 review 对应正式 plan：`AI_DOC/plan/test_framework/plan/do/memblock_sbuffer_uncache_a_owner_lifetime_fix_plan_20261025.md`。计划已经独立 review 为 `REVIEW_PASS`；本轮只检查 coding 是否按 plan 落地。

## 2. 功能特性与修改前后

### 2.1 修改前逻辑

修改前 responder 只在外部 TileLink A.fire 时调用 `capture_uncache_load_owners()`。该函数读取当前 DUT slot 的 ROB/source，再查询 active ROB map。redirect 已经退休并删除旧 ROB owner、但 DUT slot 继续合法 drain 时，A.fire 时查询必然 miss，框架错误 fatal。

### 2.2 修改后逻辑

修改后以 DUT slot generation 为 owner 生命周期真源：首次 `req_valid` 冻结 UID/epoch/ROB/LQ/slot，后续 `slaveAccept` 绑定 source；A arm 时只复制尚未消费的 owner，并冻结精确 slot mask；A.fire 只消费 snapshot 建 response/ledger，成功后把 mask 中 slot 标记为 `a_consumed`。redirect 不清 slot cache，`req_valid=0` 才结束 generation。active lookup 同拍 miss 时只允许当前单代 redirect-deleted LQ owner按完整 ROB、LQ 与 `old_epoch+1` 唯一恢复。

### 2.3 正确性检查

| 场景 | 修改后结果 | 防错机制 |
| --- | --- | --- |
| redirect 在 A.fire 前退休 owner | 旧 slot 继续使用 frozen owner | A.fire 不查 active map |
| slot 首次有效与 redirect 删除同拍 | 从当前单代临时表恢复旧 owner | 完整 ROB、LQ、epoch、唯一匹配 |
| A.valid withdrawal | 不创建 response | body 同时清 payload 与 owner snapshot |
| source 已返回、旧 slot 尚未清零 | 新同 source A 只匹配未消费 slot | `a_consumed` 排除旧 generation，armed mask 精确消费 |
| PUT | owner count 为 0 | response task 对 Store 非零 owner fatal |
| 连续 valid ROB 改变 | fatal | 禁止 occupied slot 隐式 reuse |
| reset | 全部清空 | `clear_runtime_state()` 清 slot、armed、queue、source |
| global stop | 只允许 stop 前 generation drain | stop 后首次 slot generation fatal；terminal 等待 slot cache 为空 |

## 3. 字段实现 Review

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:4839-4850`。

抽象功能描述：字段组保存 slot generation 和 A handshake 两个不同生命周期的 owner；slot state 随 DUT `req_valid`，armed snapshot 随 responder A.ready/A.fire。

```systemverilog
typedef struct packed {
    bit                     valid;
    bit                     existed_before_global_stop;
    bit                     source_valid;
    bit [3:0]               source;
    bit                     a_consumed;
    uncache_d_error_owner_t owner;
} uncache_slot_owner_state_t;
uncache_slot_owner_state_t uncache_slot_owner_state[UNCACHE_D_ERROR_MAX_OWNERS];
bit                        armed_a_owner_snapshot_valid;
bit [3:0]                  armed_a_owner_source;
int unsigned               armed_a_owner_count;
bit [15:0]                 armed_a_owner_slot_mask;
uncache_d_error_owner_t    armed_a_owners[UNCACHE_D_ERROR_MAX_OWNERS];
```

中文伪代码：slot state 在首次 `req_valid` 设置，在 `req_valid=0` 或 reset 清除；source 只在 `slaveAccept` 首次绑定。`a_consumed` 只在真实 A.fire 成功建 response 后置位，withdrawal 不置位。armed 字段和 slot mask 在 body 决定驱动 A.ready 前一次性建立，在下一 sample A.fire 或 withdrawal 后一次性清除。两个层级分开保存，避免 slot 后续变化修改已经 arm 的请求身份，同时防止已完成 A/D 的旧 slot 被复用 source 再次捕获。

## 4. 关键函数 Review

### 4.1 `find_unique_redirect_deleted_load_owner_by_rob()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:4321-4362`。

抽象功能描述：该函数只在 active ROB lookup miss 时，从当前唯一 redirect generation 的 deleted-LQ owner 集合恢复一个旧 Load owner；它只读公共状态，不消费记录。

```systemverilog
foreach (redirect_deleted_lq_owner_by_key[map_key]) begin
    candidate_owner = redirect_deleted_lq_owner_by_key[map_key];
    if (!candidate_owner.valid ||
        candidate_owner.redirect_epoch != redirect_deleted_owner_epoch ||
        candidate_owner.cancel_record_id != redirect_deleted_owner_record_id) begin
        continue;
    end
    main_tr = main_table_by_uid[candidate_owner.uid];
    status = status_by_uid[candidate_owner.uid];
    candidate_lq_key = memblock_lq_key_t'(map_key);
    if (main_tr.get_rob_key() != rob_key ||
        main_tr.get_lq_key() != candidate_lq_key ||
        status.dynamic_epoch != candidate_owner.old_dynamic_epoch + 1) begin
        continue;
    end
    matched_count++;
    matched_owner = candidate_owner;
    matched_lq_key = candidate_lq_key;
end
return matched_count == 1;
```

中文伪代码：函数先要求当前 redirect owner window 有效；再遍历该窗口内的 LQ owner。每项必须属于当前 epoch/record，并存在 main/status。随后把 map key还原为完整 LQ key，检查 main 中的完整 ROB、固定 LQ 和 status 中的新 dynamic epoch。只有 `old_epoch+1` 的同代 reissue 才计数。恰好一个候选返回成功；零个或多个由调用者 fatal。函数不删除 map，也不改变 cancel 生命周期。

### 4.2 `service_uncache_slot_owner_cache()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:5028-5150`。

抽象功能描述：该函数是每拍 slot owner 的唯一维护入口。它从 DUT monitor 输入冻结 generation、绑定 source，并拒绝 stop 后新请求或连续 valid identity 改变。

```systemverilog
if (req_valid[slot] !== 1'b1) begin
    uncache_slot_owner_state[slot] = '{default:'0};
    continue;
end
if (!uncache_slot_owner_state[slot].valid) begin
    if (global_stop_requested) fatal;
    if (data.lookup_active_uid_by_rob(rob_key, uid)) begin
        freeze active owner;
    end else if (data.find_unique_redirect_deleted_load_owner_by_rob(...)) begin
        freeze redirect-deleted owner;
    end else begin
        fatal;
    end
end else if (frozen ROB != observed ROB) begin
    fatal;
end
if (slave_valid) begin
    bind source once; changed source fatal;
end
```

中文伪代码：函数固定扫描16个 slot。无 `req_valid` 时清 generation；首次有效时先拒绝 global-stop 后新请求，再优先用 active ROB owner冻结身份。active miss 时调用窄化 redirect fallback；fallback负责证明同一旧动态实例，失败或不唯一即 fatal。已有 generation 时只比较完整 ROB，变化表示 occupied slot 被覆盖。最后根据 `slaveAccept/slaveId`绑定 source；相同 generation 的 source 不允许改变。redirect 本身不清 state。

### 4.3 `capture_armed_uncache_load_owners()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:5152-5195`。

抽象功能描述：该函数在 A admission 时把 source-bound slot owners 复制到不可变 armed snapshot；不查询 active map，不建立 response。

```systemverilog
if (opcode is PUT) begin
    armed snapshot valid with count=0;
    return;
end
foreach (uncache_slot_owner_state[slot]) begin
    if (valid && !a_consumed && source_valid && source matches) begin
        armed_a_owners[armed_a_owner_count] = owner;
        armed_a_owner_slot_mask[slot] = 1'b1;
        armed_a_owner_count++;
    end
end
if (armed_a_owner_count == 0) fatal;
armed_a_owner_snapshot_valid = 1'b1;
```

中文伪代码：函数先确认没有旧 armed snapshot并清临时数组和 slot mask。PUT建立 owner count 为0、mask为0的 snapshot；GET按slot顺序只复制同source且尚未 `a_consumed` 的 frozen owner，并记录对应 slot mask。global stop 时每项还必须标记为 stop 前存在。GET零匹配 fatal。成功后一次性发布 valid/source/count/mask，下一 sample 只读使用。

### 4.4 `create_uncache_response_record()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:5593-5665`。

抽象功能描述：该 task 消费真实 A.fire payload和显式 owner snapshot，执行原 shared-memory和error injection流程，建立 response与D-error ledger。

```systemverilog
if (LOAD && owner_count == 0) fatal;
if (STORE && owner_count != 0) fatal;
response_record.owner_count = owner_count;
copy owners into response_record;
apply_uncache_d_error_injection(...);
begin_uncache_d_error_record(... response_record.owners ...);
note_uncache_source_accept(...);
uncache_rsp_q.push_back(response_record);
```

中文伪代码：task保持原capacity、opcode解码和memory读写。Load要求显式owner，Store要求空owner；随后复制snapshot。错误注入只决定D字段，ledger使用同一response record中的不可变owner建立generation。source outstanding登记后record入队。task不清armed state，避免与body分裂所有权。

### 4.5 `body()` 和 reset/audit

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:5915-6100`，reset/audit 位于 `4991-5024`、`5235-5303`。

抽象功能描述：body 是 slot service、A/D handshake、scheduler 和 stop drain 的唯一主调度；reset清所有私有状态，audit只读报告残留。

```systemverilog
service_uncache_slot_owner_cache(global_stop);
if (a_accept_armed) begin
    if (A.valid) begin
        create response from armed owners;
        validate armed slot mask and mark exact slots A-consumed;
    end
    clear armed payload, owner snapshot and slot mask atomically;
end
...
if (global stop && queues empty && slot cache empty && armed empty) publish terminal idle;
...
if (A.valid && capacity) begin
    capture_armed_uncache_load_owners(opcode, source, global_stop);
    arm payload and drive A.ready;
end
```

中文伪代码：每个非reset sample先维护slot cache，再确认上一拍A.ready是否形成fire。fire时检查payload/source稳定并用armed owners建record，随后逐位校验mask仍指向同source、未消费slot并置 `a_consumed=1`；withdrawal不建record也不消费slot；两条路径都清payload、owner和mask。随后推进D和watchdog。stop prepare摘要包含slot/armed变化。global stop terminal要求slot cache、armed、queue、D和write batch全部为空；GET可依赖stop前未消费slot继续drain，PUT必须有pre-stop payload。reset清slot/armed/mask；audit逐项报告未清owner和非零mask，均不偷偷修复状态。

## 5. 调用关系

| 顺序 | 函数 | 在本流程中的功能 |
| --- | --- | --- |
| 1 | `body()` | 每拍主调度，决定 reset、slot service、A/D fire、scheduler 和 stop |
| 2 | `service_uncache_slot_owner_cache()` | 冻结/维护 slot generation owner和source |
| 3 | `lookup_active_uid_by_rob()` | 普通路径取得当前 active owner |
| 4 | `find_unique_redirect_deleted_load_owner_by_rob()` | active miss时证明同代旧owner |
| 5 | `capture_armed_uncache_load_owners()` | A arm时构造不可变owner snapshot |
| 6 | `create_uncache_response_record()` | A.fire后创建memory response和D-error ledger |
| 7 | `audit_sbuffer_responder_state()` | terminal/timeout只读检查残留 |

## 6. 验证证据

### 6.1 首轮独立源码 Review 阻断与修订

首轮独立源码 review 结论为 `REVIEW_FAIL`：原实现把所有 valid 且 source 相同的 slot 都复制到 armed snapshot，但旧 slot 在 A/D 已完成后仍可能因等待 Load writeback 保持 `req_valid=1`，此时 TileLink source 已可被新请求复用。若不区分已参与过 A.fire 的 generation，新请求会同时继承旧 owner，污染 D-error ledger/RM 归属。

修订实现增加每 slot 的 `a_consumed` 和每次 arm 的 `armed_a_owner_slot_mask`。GET capture 排除 consumed slot；真实 A.fire 成功建 response 后只消费 mask 中精确 slot；A.valid withdrawal 不消费；`req_valid=0`/reset 清 generation。以下验证结果必须全部使用该修订源码重新编译，旧二进制随机结果不计入最终结论。

### 6.2 修订后验证

- 编译 mode：`sbuffer_owner_lifetime_20261025`。
- 首轮 owner-lifetime 实现的两个原 seed PASS 日志早于 `a_consumed/armed slot mask` 修订，明确标记为旧二进制历史证据，不计入最终门禁。
- `a_consumed/armed slot mask` 修订后第一轮12-seed（含两个原失败 seed和10个新随机seed）为12/12 PASS；随后独立review要求补充audit的count/mask、GET/PUT和owner明细一致性检查，因此该轮只证明核心数据路径，不作为最终提交版本证据。结果目录：`mem_ut/ver/ut/memblock/sim/sbuffer_owner_lifetime_20261025_results/final_source_12seeds_20261025_124617/`。
- audit完整性修订后的最终源码重新编译通过。最终同一12-seed批次12/12 PASS，全部`rc=0`、`uvm_count=0`，包含原失败seed `133409275`、`940141520`和10个新随机seed `2026207230,2026311959,2026416688,2026521417,2026626146,2026730875,2026835604,2026940333,2027045062,2027149791`。结果目录：`mem_ut/ver/ut/memblock/sim/sbuffer_owner_lifetime_20261025_results/audit_final_12seeds_20261025_125328/`。
- 最终独立源码review：subagent `Hegel`，首轮因旧验证证据和audit完整性返回`REVIEW_FAIL`；补齐count/mask、GET/PUT和owner明细检查并重新编译后复审为`REVIEW_PASS`，未发现剩余代码阻断项。

## 7. Plan 对齐与风险

实现与修订后正式 plan 对齐：slot-first freeze、单代 redirect fallback、排除已消费generation、armed不可变snapshot与精确slot mask、body原子清理、global-stop旧generation drain、reset/audit纳入新状态均已落地。未修改RTL/Scala、plus参数、shared-memory数据语义、response delay/reorder、source outstanding和D error injection。

最终门禁已关闭：原失败seed、10个新随机seed、最终源码编译和独立源码review均通过。剩余风险仅是12-seed随机样本无法穷举所有source复用与stop-drain交错，后续全量30 cfg回归继续提供系统级覆盖。
