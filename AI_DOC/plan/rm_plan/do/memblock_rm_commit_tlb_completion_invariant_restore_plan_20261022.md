# memblock RM 提交边界恢复 TLB 翻译完成不变量方案（2026-10-22）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已完成 coding、implementation review、远端验证并已归档 |
| 方案类型 | V2 RM/checker plan |
| 工作分支 | `mem_ut_uvm_v2` |
| 关联提交 | `7632adee72`（核心实现）+ `6599301026`（跨页非冻结测试补充） |
| 目标问题 | 采用非冻结查询方案，收窄当前 RM 对“L2TLB request 已 fire、response 尚未完成”的等待逻辑，恢复 commit 边界的强检查语义；已完成 UID record 仍可作为完成事实使用 |
| 计划目录 | `AI_DOC/plan/rm_plan/undo/` |
| coding 门禁 | 本文只提供 plan；只有用户明确授权 coding 后才能修改源码或测试 |

## 1. 术语与抽象功能说明

### 1.1 专有名词

| 术语 | 中文含义 | 代码落点 | 本方案中的判断 |
| --- | --- | --- | --- |
| `RM` | Reference Model，参考模型；根据冻结的测试框架事实构造 Load/Store 期望并比较 DUT 结果 | `memblock_rm.sv`、`rm_ls_core.sv` | 负责发现错误，不负责替测试框架修复生命周期顺序 |
| `UID` | 一条主表事务的唯一编号 | `memblock_uid_t`、`uid_tlb_record_by_uid` | RM 按 UID 顺序处理 commit 检查 |
| `ROB commit` | 事务已经通过框架 commit gate 并被标记为提交 | `status_view.rob_commit` | 是 RM 开始检查该 UID 的边界 |
| `commit gate` | 框架允许 UID 标记 `rob_commit` 的前置条件集合 | `lsq_commit_handler::uid_is_normal_commit_candidate()` | 至少要求 writeback、pass、required targets 完成，且没有 replay/redirect/flush 等状态 |
| `L2TLB request fire` | DTLB 与 L2TLB responder 之间真实握手成功的一拍 | `uid_tlb_first_request_fire_sample_seq`、`capture_fired_request()` | 只证明请求已经发出，不证明 response 已发布 |
| `WAITING UID` | UID 已登记翻译请求，但尚未完成 UID TLB record | `MEMBLOCK_UID_TLB_WAITING` | 在 commit 边界仍为该状态属于生命周期异常候选 |
| `completed translation` | L2TLB response 已被采样，并已将 entry payload 回填到 UID record | `record_completed_tlb_response()`、`complete_waiting_uid_records_by_response()` | 正常 commit candidate 应已具备该事实 |
| `translation pending` | 当前修改引入的中间状态：request 已 fire、`pte_valid=0`、RM 暂不报错并等待下一次 poll | `observer_note_translation_pending()` 等 | 本方案删除该状态在 RM commit 判定中的特殊语义 |
| `RM poll` | RM 以 `#1ns` 周期重复观察当前 commit cursor | `observer_poll_process()` | 只等待 `rob_commit`/状态事实到达，不作为 L2TLB response timeout 机制 |
| `L2TLB idle stop cycle` | L2TLB responder 判断自身连续空闲并停止的配置阈值 | `get_l2tlb_idle_stop_cycle()` | 不得作为 RM 的 commit 等待超时 |
| `issue provenance` | UID issue 时记录的上下文和事件来源，用于诊断，不作为 RM 最终翻译结果的权威来源 | `memblock_uid_tlb_record` | RM 仍以当前唯一合法 lookup 结果为准；来源记录不能绕过缺失或重叠错误 |

### 1.2 关键函数的抽象职责

`lsq_commit_handler::uid_is_normal_commit_candidate()`负责判断一个 UID 是否满足框架的正常 commit
前置条件；它不是 RM 的翻译查询函数。

`memblock_l2tlb_base_sequence::complete_driving_response()`负责在真实 response sample 后发布完成的
L2TLB payload，并调用 `complete_waiting_uid_records_by_response()` 更新匹配的 WAITING UID。

`memblock_rm::observer_build_commit_item()`根据 UID 的 issue-time CSR context 和当前唯一合法翻译结果构造
RM item；在 commit 边界不得把尚未完成的翻译当成可重试的正常中间态。

`memblock_rm::observer_process_current_commit()`负责在 `status.rob_commit` 成立后调用 RM item 构造、
实际结果采集和 compare；翻译查询失败必须在本次 commit 检查中形成可诊断错误。

`memblock_rm_readonly_api::resolve_tlb_entry_for_rm()`是带语义检查的只读翻译解析入口；普通 query miss、
UID record 缺失、context 不一致和未完成翻译都不能被静默转成成功。

## 2. 方案背景与当前修改的来源

### 2.1 当前提交引入的行为

`2829a232b2` 在 `memblock_rm.sv` 与 `memblock_rm_readonly_api.sv` 中加入了以下逻辑：

1. 用 `uid_tlb_translation_pending_for_rm()` 判断 UID 是否处于
   `WAITING && first_request_fire_sample_seq != 0 && !pte_valid`。
2. `observer_build_commit_item()` 通过 `translation_pending` 输出把该状态返回给调用者。
3. `observer_process_current_commit()` 发现 pending 后调用 `observer_note_translation_pending()`，保持当前
   UID，不报告 `RM_LS_ERR_TLB_ENTRY_NOT_READY`，等待下一轮 `observer_poll_process()`。
4. 等待次数达到 `get_l2tlb_idle_stop_cycle()` 后才报告错误。
5. 跨页访问在逐 byte lookup 前也检查 pending。
6. `soft_test_tc_rm_pbmt_effective_fault.sv` 增加了 pending 成功重试、pending timeout 和跨页 pending
   的软件专项断言。

该修改的直接动机是某个 L2TLB overlap 专项 seed 中观察到 RM commit 查询早于 L2TLB response 完成，旧逻辑
将中间状态报告为 `TLB_ENTRY_NOT_READY`。但当前提交记录、W43 RM/测试框架缺陷记录和已有 L2TLB flow
没有证明这是 DUT 允许的“正常 commit 后 response 晚到”协议；它更像是测试框架生命周期顺序、response
采样或 UID 回填链路的问题。

### 2.2 当前框架的时序不变量

当前正常 UID commit gate 至少要求：

```text
status.writeback == 1
&& status.pass == 1
&& required_targets_done(uid) == 1
&& 无 fault/replay/redirect/flush/killed 状态
```

L2TLB response 完成路径为：

```text
真实 response sample
  -> record_completed_tlb_response()
  -> complete_waiting_uid_records_by_response()
  -> UID TLB record.mark_completed()
  -> pte_valid = 1
  -> writeback/pass/required_targets_done
  -> rob_commit
  -> RM commit check
```

因此本方案恢复以下强不变量：

```text
在正常 rob_commit 检查开始时，当前 UID 所需的翻译必须已完成；
否则 RM 当次立即报告翻译不可用，而不是等待一个没有协议依据的后续 poll。
```

该不变量检查的是验证框架的可观测生命周期合同，不声称所有 DUT pipeline 设计都必须先完成 L2TLB response
才可产生某种内部状态；如果未来发现该合同与 DUT 真实协议冲突，应修正 commit barrier、response monitor 或
UID completion owner，而不是在 RM 中加入无来源的时间等待。

### 2.3 方案选型：采用非冻结查询

本 plan 采用“非冻结查询”作为 RM 的权威翻译路径；这里的“非冻结”是指不使用 UID issue 时保存的
`issue_hit_snapshot` 覆盖当前查询结果。已经由真实 response 完成并回填的 UID record payload 属于已完成事实，
不属于 issue-time 冻结快照；该 payload 仍可按现有 completed translation 流程使用。

```text
UID commit 检查
  -> 对当前访问的每个 VPN 执行当前只读 lookup
  -> 0 个候选：立即报 translation unavailable
  -> 1 个候选：使用该 entry
  -> 多个候选：立即报 overlap/lifecycle fatal
```

选择依据如下：

1. `publish_live_tlb_entry()`已在发布前执行 live overlap 检查；合法 live 状态下，一个 VPN 至多有一个实际
   raw-hit entry。对于尚未形成 completed UID record 的查询，RM 直接读取当前 live 表；对于已经 completed 的
   UID，RM 可使用真实 response 回填的 record payload。
2. `find_tlb_range_hit_by_req()`对多个 live candidate 已定义为非法重叠，而不是选择最大覆盖范围。
3. SFENCE 必须等待其前序 ROB 指令完成；SFENCE 之前的 UID 应在 entry 删除前完成 RM 检查，SFENCE 之后的 UID
   使用新的翻译生命周期。因此合法流程不应出现“UID 已 issue、entry 已被删除、UID 后续才 commit”的窗口。
4. 如果 RM 在 commit 时查询不到 entry，应该暴露测试框架的 SFENCE、response、index 或 commit 顺序错误，不能
   用 UID issue 时的历史快照把当前缺失转换成成功。
5. 一个非对齐 UID 最多涉及两个 page VPN。RM 必须对首 VPN 和跨页 VPN分别解析各自的翻译来源：首 VPN若已有
   completed UID record，可使用真实 response payload；否则执行当前 live lookup；跨页 VPN始终执行自己的
   当前 lookup。不能用只有一份 `issue_hit_payload` 的 UID 级快照代表整笔访问。

因此，issue-hit snapshot 若继续保留，只能作为 debug/provenance 记录；不得由 RM 在当前 lookup 失败时作为
备用翻译结果。实现时优先删除其 RM authoritative resolver 和不再使用的生产调用，避免形成两套翻译真源。

## 3. 当前问题与风险判断

### 3.1 为什么当前等待机制与 RM commit 语义冲突

1. `rob_commit` 已经是 RM 入口条件，不是普通 issue/dispatch 观察点。
2. `WAITING + request fire` 只说明 request 已经发出，不能证明 response 一定会到达，也不能证明 UID
   commit 合法。
3. RM 暂缓报错会把以下问题延迟或隐藏：
   - response 没有被 monitor 采样；
   - response 已采样但 UID payload 没有回填；
   - UID 提前进入 commit gate；
   - response、UID completion 和 commit owner 的事件顺序错误；
   - request/response context 或 token provenance 不匹配。
4. `get_l2tlb_idle_stop_cycle()`的单位是 responder sample/cycle 空闲阈值，而 RM 采用 `#1ns` poll 次数；
   两者不是同一时钟或同一进度定义，直接比较会产生错误 timeout 语义。
5. 当前 pending helper 以 `record.vpn` 匹配 request VPN；对于跨页访问，它只能证明 UID 首个等待记录，
   不能完整证明第二页的翻译状态，因此“逐 byte 等待”并没有建立完整跨页 pending 模型。

### 3.2 不属于本方案的结论

本方案不据此认定已经存在 DUT RTL 缺陷，也不修改 Scala/Chisel、生成 RTL 或 L2TLB responder 的协议。
本方案只恢复 RM 的检查边界，并把真实问题保留为可定位的 framework/L2TLB lifecycle 错误。

## 4. 目标功能 Flow

### 4.1 正常 completed translation

抽象功能：RM 在 `rob_commit` 成立后读取已经完成的 UID translation，构造 item 并继续现有异常、地址和
数据 compare 流程。

```text
observer_process_current_commit()
  -> 读取 status/main view
  -> 确认 status.rob_commit
  -> observer_build_commit_item(main_view, status_view, item)
  -> resolve_tlb_entry_for_rm()
  -> add_item/capture_actual/compare
  -> 推进 RM UID cursor
```

中文伪代码：当当前 UID 尚未 `rob_commit` 时，RM 保持现有 poll 行为；当 `rob_commit` 已成立时，立即构造
item。分页路径使用 UID 保存的 issue-time CSR context：已完成的首 VPN可使用真实 response 回填的 completed
payload，未完成首 VPN和跨页 VPN分别查询当前唯一合法 entry，并检查 entry 与 context/key 一致。成功后继续
现有 compare，不建立 pending owner，也不记录 RM translation timeout。

### 4.1.1 非对齐跨页访问

抽象功能：对一个 UID 的每个实际访问 VPN 独立完成当前 translation lookup，避免把首 VPN 的单份快照错误扩展
为整笔访问的所有页。

```text
计算 byte_va
  -> 得到 byte_vpn
   -> byte_vpn == UID 首 VPN 且 record 已完成：使用真实 response 回填的 UID completed payload
  -> 其它情况：按 byte_vpn 执行当前 exact/range lookup
  -> 0 个候选：立即报当前 byte 的 TLB_ENTRY_NOT_READY
  -> 多个候选：报 overlap/lifecycle fatal
  -> 使用该 VPN entry 计算当前 byte 的 PA
```

首 VPN的 `issue_hit_payload`不得作为跨页第二 VPN的替代物；RM不保存“第一份还是第二份”的UID级二选一快照。
如果第二 VPN lookup失败，这表示该页的 translation 生命周期没有闭合，不能回退到首 VPN issue snapshot 或
等待下一轮 poll。这里允许使用的首 VPN completed payload，必须已经由真实 response 完成路径写入 UID record，
不能由 issue-time matcher 快照伪造。

### 4.2 commit 边界发现未完成翻译

抽象功能：RM 在 commit 检查中发现翻译 entry 不可用时，立即形成 `RM_LS_ERR_TLB_ENTRY_NOT_READY` 或更具体
的已有 translation error，并停止该 UID 的正常 compare。

```text
rob_commit == 1
  -> observer_build_commit_item()
  -> resolve_tlb_entry_for_rm() 失败
  -> 保留原始只读 query/error 证据
  -> 设置 RM_LS_ERR_TLB_ENTRY_NOT_READY
  -> observer_process_current_commit() 报告一次 RM_LS_COMPARE
  -> observer_mismatch_reported = 1
  -> 不推进 current_uid，不等待 l2tlb_idle_stop_cycle
```

若可以读取 UID view，错误日志至少包含：UID、ROB key、请求 VPN、`wait_state`、`pte_valid`、
`uid_wait_start_sample_seq`、`first_request_fire_sample_seq`、L2TLB request key 和 CSR context sequence。
这些字段用于证明“请求是否 fire、是否完成、是否 context 一致”，但不改变立即失败决策。

### 4.3 非 commit 状态

`observer_poll_process()`仍可每 `#1ns` 调用 `observer_process_current_commit()`，但该轮询只等待
`status.rob_commit` 和其它 commit status 被框架发布。它不再根据 UID pending 状态进行重试计数，也不再把
`get_l2tlb_idle_stop_cycle()`解释为 RM timeout。

## 5. 源码修改步骤

### 5.1 `memblock_rm.sv`：移除 RM pending 状态机

抽象功能：删除只服务于 translation pending 的状态与 helper，让 RM commit flow 回到单次解析、失败即报错的
路径。

修改内容：

1. 删除 class 字段：
   - `observer_trace_translation_pending_emitted`
   - `observer_translation_pending_active`
   - `observer_translation_pending_uid`
   - `observer_translation_pending_poll_count`
2. 删除 `observer_note_translation_pending()` 和 `observer_clear_translation_pending()` 的声明、实现及
   `build_phase()` 初始化。
3. 将 `observer_build_commit_item()` 的接口恢复为只输出 `item`，删除 `output bit translation_pending`。
4. 删除首个 VPN lookup 前的 `uid_tlb_translation_pending_for_rm()` 特判。
5. 删除跨页逐 byte lookup 前的 pending 特判；每个 byte 直接进入既有
   `resolve_tlb_entry_for_rm()`，缺失时立即使用 `RM_LS_ERR_TLB_ENTRY_NOT_READY`。
6. 保留已有 `resolve_tlb_entry_for_rm()`、entry context 一致性、PMA/PMP、PBMT、Bare identity 和跨页
   stop 逻辑；调整其首 VPN issue-hit 分支，使其与跨页 VPN统一使用当前 lookup，不借此修改其它异常语义。

### 5.2 `observer_process_current_commit()`：恢复立即失败

抽象功能：在 RM 已确认 `rob_commit` 后执行一次确定性的翻译解析和 compare；不为当前 UID 建立独立等待状态。

源码级伪代码：

```systemverilog
if (!observer_build_commit_item(main_view, status_view, item)) begin
    if (!observer_mismatch_reported) begin
        `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
        observer_mismatch_reported = 1'b1;
    end
    return 1'b0;
end

if (!ls_model.add_item(item)) begin
    ...
end
```

中文伪代码：构造 item 失败时直接沿用既有 RM compare error 处理；不调用 pending note，不清理 pending
状态，也不返回“本次只是等待”的成功假象。只有 item 构造成功，才允许进入 `add_item()` 和后续实际结果
采集。

### 5.3 `observer_poll_process()`：保留状态轮询，删除翻译轮询语义

抽象功能：持续观察 commit cursor 是否已经获得可检查的 `rob_commit` 状态；不承担 L2TLB response 调度。

源码级伪代码：

```systemverilog
task memblock_rm::observer_poll_process();
    forever begin
        void'(observer_process_current_commit());
        #1ns;
    end
endtask
```

该 task 可以保持现状。修改重点是保证 `observer_process_current_commit()`在 commit 已成立且 translation
缺失时不会因为 poll 继续而改变结论。

### 5.4 `memblock_rm_readonly_api.sv`：统一使用非冻结 lookup

抽象功能：只读 API 为 RM 提供统一的当前 translation lookup；首 VPN和跨页 VPN遵循同一候选唯一性规则，
不使用 UID 级 issue-hit 快照绕过当前表状态。

修改内容：

1. 删除 `uid_tlb_translation_pending_for_rm()` 的声明和实现。
2. 删除 `resolve_uid_issue_hit_entry_for_rm()` 的 RM 生产接口、实现和调用；不得在当前 lookup 失败时回退到
   `issue_hit_payload`。
3. 修改 `resolve_tlb_entry_for_rm()`：
   - `record.pte_valid && wait_state==COMPLETED` 的首 VPN继续使用 UID completed payload；
   - `WAITING` 的首 VPN不论 request 是否已经 fire，都改用 `resolve_tlb_entry_key_for_rm()`查询当前唯一合法 entry；
   - `request_vpn != record.vpn` 的跨页 VPN继续使用同一个当前 lookup入口；
   - 查询结果为 0 个候选时立即返回 query miss；多个 live candidate由既有 overlap fatal暴露。
4. 保留 `read_uid_tlb_for_rm()`及其已有只读字段导出；如错误诊断需要，复用现有
   `uid_tlb_view_t`中的 `wait_state`、`pte_valid`、`uid_wait_start_sample_seq`、
   `first_request_fire_sample_seq`，不新增 runtime 参数。
5. 不修改 `resolve_tlb_entry_for_rm()` 的普通 query miss 语义；它仍应区分 missing record、cancelled record、
   context mismatch、range miss 和 completed entry unavailable。

本 plan 保留现有 `issue_hit_snapshot` 字段和 capture helper 作为 debug/provenance 记录，以降低本次修改范围；
必须在注释和测试中明确它们不参与 RM correctness，也不能在 query miss 时恢复 RM compare。后续若需要删除这套
诊断记录，应另立清理 plan，不能在本 plan 中改变 UID record 的公共生命周期。

### 5.5 `soft_test_tc_rm_pbmt_effective_fault.sv`：替换旧 pending 专项

抽象功能：把原先验证“RM 可以等待 pending”的专项改成验证“commit 边界 pending 必须立即报错”，并验证
首 VPN与跨页 VPN统一走非冻结 lookup，同时保留正常 completed translation 和普通 query miss 的覆盖。

修改内容：

1. 删除对 `observer_note_translation_pending()`、`observer_clear_translation_pending()`、
   `observer_translation_pending_*` 和 `translation_pending` 输出参数的调用。
2. 删除 `l2tlb_idle_stop_cycle` 临时修改和 pending timeout catcher；RM 不再使用该参数作为 timeout。
3. 新增 software-only 负向场景：
   - 建立有效 UID/main/status view；
   - 设置 `status.rob_commit=1`；
   - UID record 为 `WAITING`；
   - `first_request_fire_sample_seq != 0`；
   - `pte_valid=0`；
   - 调用生产 `observer_build_commit_item()`或 commit path；
   - 断言当次返回失败并产生 `RM_LS_ERR_TLB_ENTRY_NOT_READY`，不产生第二次 poll 成功路径。
4. 保留并复用以下正向场景：completed UID translation、Bare identity、正常分页、已有 PBMT/PMA/PMP
   异常和跨页首字节停止。
5. 增加跨页负向场景：首 VPN 已完成但后续 byte 所需 translation 缺失时，立即报告对应 byte 的
   `TLB_ENTRY_NOT_READY`，不把首 VPN 的 issue snapshot 或 fire marker 当成第二 VPN 的 translation。
6. 增加非冻结一致性场景：首 VPN 与第二 VPN分别有唯一合法 entry时，RM使用两个 VPN各自的entry；不允许
   使用首 VPN快照覆盖第二 VPN。

## 6. 诊断与错误分类

### 6.1 错误分类原则

| 场景 | 期望结果 |
| --- | --- |
| `rob_commit=0`，status 尚未就绪 | RM 保持现有 poll，不报翻译错误 |
| `rob_commit=1`，completed entry 有效 | 进入正常 RM compare |
| `rob_commit=1`，UID 为 WAITING 且已 fire、`pte_valid=0` | 当次报 `RM_LS_ERR_TLB_ENTRY_NOT_READY`，保留 lifecycle 诊断 |
| `rob_commit=1`，UID record 缺失 | 保留 readonly query miss 和 `TLB_ENTRY_NOT_READY` |
| UID 已 cancel、context 不一致或普通 range miss | 保留现有具体错误，不转换为 pending |
| L2TLB response 后完成 UID record | 由既有 response completion flow 更新，RM 下一次观察到完成后正常 compare |

### 6.2 日志要求

错误日志不得只输出“等待超时”。应优先说明 RM 看到的 commit 边界事实，例如：

```text
uid=<uid> rob=<flag>/<value> committed translation unavailable:
vpn=<vpn> wait_state=<state> pte_valid=<0/1>
wait_start_sample=<n> request_fire_sample=<n>
request_key=<...> csr_seq=<n>
```

日志字段只是诊断信息，不得改变错误分类，也不得把 `first_request_fire_sample_seq != 0`解释为 response
已经成功。

## 7. 影响文件与范围

| 文件 | 计划修改 |
| --- | --- |
| `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv` | 删除 RM pending 状态机和两个 pending 分支；恢复 commit 边界立即解析/报错；补充 pending lifecycle 诊断 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv` | 删除 pending helper；移除 issue-hit snapshot 的 RM 权威解析，首 VPN和跨页 VPN统一使用当前 lookup |
| `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv` | 删除旧 pending 等待专项，新增 commit 边界立即失败与跨页缺失负向专项 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv` | 将单份 issue-hit snapshot 作为结果来源的专项改为非冻结首 VPN/跨页 VPN lookup和多候选错误检查 |

本 plan 不修改：

- `memblock_l2tlb_base_sequence.sv` 的 response token、response sample 和 UID completion 逻辑；
- `common_data_transaction.sv` 的 WAITING UID multicast、record lifecycle、logical fanout 容量和 issue-hit snapshot capture；
- `memblock_tlb_entry.sv` 的 UID issue-hit snapshot 字段；这些字段本轮只保留为 debug/provenance，不作为 RM 结果来源；
- DUT RTL/Scala、L2TLB interface、driver/monitor 协议；
- `get_l2tlb_idle_stop_cycle()`参数定义、plusarg 或 testcase preset；
- 其它与本 pending 机制无关的 sequence、TLB overlap builder、PMA/PMP 和 replay 修复。

本 plan 不把 `issue_hit_payload`作为 RM correctness fallback；若保留该字段，仅用于日志和 provenance 断言，
不得在 query miss 时恢复 RM compare。

## 8. 验证计划

### 8.1 静态检查

1. `rg`确认 `observer_note_translation_pending`、`observer_clear_translation_pending`、
   `uid_tlb_translation_pending_for_rm`、`translation_pending`旧接口只剩 plan/历史文档中的说明，源码和
   testcase 不再有调用。
2. 检查 `observer_build_commit_item()`所有调用点的函数签名一致。
3. 检查 `resolve_uid_issue_hit_entry_for_rm()`不再有 RM 生产调用；`issue_hit_payload`只出现在诊断或
   provenance 专项，不出现在 RM 成功回退路径。
4. 检查 `get_l2tlb_idle_stop_cycle()`不再出现在 RM commit error 或 RM pending retry 路径。
5. 执行：

```text
git diff --check -- AI_DOC mem_ut/ver/ut/memblock
```

### 8.2 软件专项

至少覆盖以下场景：

1. completed translation：response completion 后 UID record 为 completed，RM 正常构造 item。
2. commit pending negative：`rob_commit=1`且 UID 为 fired WAITING，RM 当次立即报告
   `TLB_ENTRY_NOT_READY`。
3. ordinary miss negative：没有 UID record 或没有 request fire marker，仍走原有 query miss，不被识别为
   pending。
4. cross-page negative：首 VPN completed、后续页 translation 缺失，立即报告具体 byte 的错误。
5. response-after-error diagnosis：在 RM 报错后再补 response，只验证日志和状态能说明“commit 先于
   completion”，不要求 RM 回滚已报告的错误。
6. cross-page positive：两个 VPN各有唯一合法 entry，确认首 VPN和第二 VPN分别解析自身 entry，不能复用
   首 VPN issue snapshot。
7. multiple-candidate negative：人为构造或注入多个 live candidate，确认直接报告 overlap/lifecycle fatal，
   不选择最大 coverage rank继续运行。
8. 既有 PBMT/PMA/PMP、Bare identity、跨页停止和 provenance 专项不回归。

### 8.3 远端编译与回归

按项目 V2 远端流程执行：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

专项 testcase 编译/仿真应使用实际新增或调整的 testcase preset；结果必须满足 `TEST CASE PASSED`，并且
`UVM_ERROR=0`、`UVM_FATAL=0`。负向场景若使用 report catcher，必须确认 catcher 只吸收预期的 RM error，
测试结束时不残留未预期错误。

本轮实际补充验证：

- `tc_rm_pbmt_effective_fault/virtual_base_sequence`，seed `2026102011`，mode
  `rm_nonfreeze_pbmt_20261022_r4`：首 VPN completed、第二 VPN缺失的跨页负向场景通过，
  `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- `tc_l2tlb_range_lookup_smoke/virtual_base_sequence`，seed `2026102011`，mode
  `rm_nonfreeze_range_20261022_r1`：live overlap expected-fatal 场景通过，2个预期 fatal 被 catcher 捕获，
  `UVM_ERROR=0`、`UVM_FATAL=0`。

## 9. 验收标准

1. `rob_commit=0`时，RM 仍能等待 status/main view 到达；没有因删除 pending helper而误报。
2. `rob_commit=1`且 UID translation 已完成时，正常 compare 行为和结果不变。
3. `rob_commit=1`且 UID translation 为 fired WAITING、`pte_valid=0`时，RM 当次立即报告
   `RM_LS_ERR_TLB_ENTRY_NOT_READY`，不等待 `get_l2tlb_idle_stop_cycle()`，不推进错误 UID 的正常 compare。
4. 普通 query miss、cancel、context mismatch、cross-page missing entry 与真实 response 丢失均保持可观测，
   不被归类为正常 pending。
5. 首 VPN和跨页 VPN均按各自完整生命周期解析：已完成首 VPN可使用真实 response 回填的 completed payload，
   未完成首 VPN和跨页 VPN使用各自的当前唯一合法 entry；不存在“首 VPN使用 issue 冻结快照、第二 VPN重新查询”
   的混合权威语义。
6. 多个 live candidate时直接报告 overlap/lifecycle错误，不按最大 coverage rank选择。
7. RM 日志能够同时给出 UID、ROB、VPN、wait state、`pte_valid`、request fire sample 和 CSR context 等
   定位信息。
8. `memblock_l2tlb_base_sequence.sv`的 response completion 仍负责完成 UID record；本 plan 不通过修改
   RM 等待逻辑掩盖该 owner 的问题。
9. 专项和 `tc_sanity/base_fun` 远端验证通过，未引入新的编译、UVM error/fatal 或 RM compare 回归。

## 10. 风险、回退与后续调查

### 10.1 风险

若专项证明当前框架确实允许合法的“commit 已发布、L2TLB response 尚未发布”时序，则本方案的强不变量
不适用。此时必须先提供 response sample、UID completion、commit gate 的完整时序证据，重新确认协议边界；
不能仅凭单个 `WAITING` 观察结果恢复等待机制。

### 10.2 回退原则

如果发现问题来自同一仿真时刻的 delta-cycle 可见性竞争，应修复 monitor/responder/commit barrier 的采样顺序，
例如保证 response completion 在 commit gate 读取前可见；不得以 `#1ns` 重试作为最终修复。

### 10.3 后续调查入口

若删除等待后复现“commit 早于 response”的错误，按以下顺序定位：

1. 检查 `complete_driving_response()`是否已经确认真实 response sample。
2. 检查 `complete_waiting_uid_records_by_response()`是否命中正确的 UID/context。
3. 检查 `uid_is_normal_commit_candidate()`及 `mark_rob_commit_uid()`是否在翻译完成前允许 commit。
4. 检查 `observer_process_current_commit()`读取 status 与 UID view 的 sample/delta 顺序。
5. 只有确认框架合同与 DUT 协议真实允许该顺序后，才重新设计跨组件 completion barrier；不要恢复当前
   没有统一时间基准的 idle-stop poll timeout。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

以下内容是在 coding 和仿真过程中根据实际源码行为补充的实现细节，不改变方案 A 的核心选择：

1. `[IMPLEMENTATION_DELTA]` `soft_test_rm_provenance_followup_sequence.sv` 增加
   `RM_READONLY_API` 预期 query-miss catcher。原因是非冻结方案在删除 live entry 后必须让 readonly API
   产生可观察 query miss，而 directed test 需要只吸收该预期错误，避免它被误判为 testcase 失败。
2. `[IMPLEMENTATION_DELTA]` issue snapshot lifecycle 检查在删除同 key live entry 后重新发布 completed response
   cache，再验证 reissue capture。原因是 `delete_live_tlb_entry_by_anchor_key()`按公共 owner 规则同时删除
   同 key completed cache；该补充只修复测试前置条件，不改变 RM 真源。
3. `[IMPLEMENTATION_DELTA]` 当前 live lookup 的 directed 断言允许
   `resolve_tlb_entry_key_for_rm()`重新 derive `request_translation_valid`。该字段来自当前 entry 的
   request-derived translation，不再沿用 issue snapshot 的旧字段。
4. `[IMPLEMENTATION_DELTA]` `soft_test_tc_rm_pbmt_effective_fault.sv` 对 fired WAITING 和跨页缺失
   两个负向场景分别捕获一次 `tlb_entry_exact_or_range` query miss，并保留立即失败断言；这使测试语义
   与删除 pending retry 后的生产路径一致。
5. `[IMPLEMENTATION_DELTA]` 跨页负向场景进一步收紧为“首 VPN 已完成、第二 VPN 缺失”。原因是只有该
   组合才能直接证明跨页查询不会把首 VPN 的 completed payload 当作第二 VPN 的 translation fallback。
