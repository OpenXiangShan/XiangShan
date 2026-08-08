# mem_ut V2 `isStoreException` Agent 迁移 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_is_store_exception_agent_migration_coding_plan_20260727.md` |
| 目标版本 | V2，`mem_ut_uvm_v2` |
| review 范围 | `io_ooo_to_mem_isStoreException` owner 迁移、fault sideband 生成/保持、software fault smoke 和相关文档 |
| 不在范围 | DUT exception address checker、RM/scoreboard、vector LS、redirect/replay 主状态机、normal commit/deq 语义 |
| 本 agent 结论 | 已完成静态检查和两项仿真；最终独立 review 结论见第 10 节 |

## 1. 术语与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 典型场景 |
|---|---|---|---|
| `agent owner` | 唯一负责某个 DUT 端口的 interface、xaction、driver、monitor 和 connect 的 agent | `lsqcommit_agent_agent` | 不能同时由 vecissue 和 lsqcommit force 同一个输入 |
| `fault head` | 当前 modeled ROB head 中已进入 fault/exception terminal 路径的 UID | `lsq_commit_handler::select_fault_head_candidate()` | fault UID0 在普通 commit 前取得 exception commit token |
| `level sideband` | 没有独立 valid，空拍不能被随意清零的 DUT 输入 | `io_ooo_to_mem_isStoreException` | store fault 写入 1 后，normal/idle 周期仍保持 1 |
| `fault type latch` | 最近一笔已成功进入 fault commit 的 load/store 类型 | `latched_is_store_exception` | fault transaction 已发送且 `mark_fault_rob_commit_uid()` 成功后更新 |
| `raw IQ hit` | 用 SQ key 标识当前 STA issue 实例已被 IQ 接受的 synthetic raw event | `dispatch_raw_iq_feedback_t` | strict STA real/fault writeback 前的已有语义前置条件 |
| `recovery event` | fault writeback 落表后等待 recovery owner 消费的事件 | `common_data_transaction::exception_event_q` | software smoke 也必须调用 `exception_redirect_replay_task()` 排空 |
| `mon_sw` | agent monitor 是否创建和运行的配置开关 | 各 agent cfg | software-only smoke 关闭不消费的 DUT output monitor |

本专项的抽象功能分为三段：

1. 端口归属：把 ROB exception context 的输入从 vector issue agent 移到 LSQ commit agent，保证只有一个 force/readback 链。
2. sideband 生命周期：handler 从 fault UID 的操作类别导出 load/store bit；driver 把该 bit 作为 level 输入保持，不让 normal commit 或气泡改变它。
3. software smoke：用既有 IQ feedback 和 recovery handler 完成测试构造的前置/收敛，不在 smoke 中复制或改写公共 writeback、fault、commit、deq 状态机。

## 2. Review 范围与结论

本 review 逐项对照关联 plan 的 4.1 至 4.4、8 和 9 节，并检查本次 `git diff` 的所有有行为意义的 SystemVerilog 修改。

初步结论：实现没有把 `isStoreException` 混入 `pendingst`、`scommit`、pass/fail 或 terminal；新逻辑只改变该 fault address-selection sideband 的 owner、生成时机和空拍保持。software fault smoke 的三项补充均调用已有 owner，没有直接改公共队列或状态表。

## 3. Agent Owner 与字段链路

### 3.1 vecissue 移除，lsqcommit 接管

修改前：`vecissue_agent_agent` 声明、采样、驱动并 connect `io_ooo_to_mem_isStoreException`，但 driver 固定把它清为 0。该字段与 vector issue 的 `issueVldu` 没有语义从属关系，store fault 无法表达。

修改后：vecissue interface/xaction/driver/monitor/connect 删除该字段；lsqcommit 的五段字段链完整接管：

```text
lsqcommit xaction
  -> lsqcommit driver send_pkt
  -> lsqcommit interface drv_cb
  -> lsqcommit connect
  -> DUT io_ooo_to_mem_isStoreException

DUT/readback
  -> lsqcommit interface mon_cb
  -> lsqcommit monitor local sample
```

`lsqcommit_agent_agent_xaction` 将字段定义为非 `rand bit`，构造默认值为 0，并补齐 field automation、`psdisplay()` 和 `compare()`。因此原有通用 random sequence 不会无意把 fault type 随机化。

`lsqcommit_agent_connect.sv` 的 `MEMBLOCK_UT` 分支只把 interface 值 force 到 DUT；非 UT 分支仍反向 force RTL readback 到 interface。monitor 仅在 `MEMBLOCK_UT` 下执行该字段的 X/Z 检查，避免对完整 core 中没有 reset-valid 锚点的 ROB latch 作错误假设。

### 3.2 迁移检查结果

```text
检查 vecissue 源码和 vecissue connect：无 isStoreException 残留。
检查 lsqcommit 源码和 lsqcommit connect：只存在 interface/xaction/driver/monitor/connect 与 handler 消费点。
检查 DUT 顶层：端口名未变化，不新增顶层 wrapper。
```

这是一项字段归属适配，不改变 LSQ enqueue、issueVldu、normal commit、LQ/SQ deq 或 vector flow。

## 4. `lsq_commit_handler` 的 fault type latch

### 4.1 `fault_uid_is_store_exception()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

抽象功能描述：该纯派生 helper 根据权威 main table 中 fault UID 的 operation behavior，返回 V2 ROB exception commit 所需的 scalar store bit；它不写 status、active map、LSQ pointer 或 latch。

```systemverilog
function bit fault_uid_is_store_exception(input memblock_uid_t uid);
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;

    ensure_handles();
    if (!data.main_table_ready || uid >= data.main_trans_num) begin
        `uvm_fatal("LSQ_COMMIT", "cannot classify invalid fault uid")
    end
    main_tr = data.get_main_transaction(uid);
    if (main_tr == null) begin
        `uvm_fatal("LSQ_COMMIT", "fault uid has null main transaction")
    end
    behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
    return memblock_op_behavior_util::is_scalar_rob_store_commit(behavior);
endfunction
```

文字伪代码：

```text
先确认主表已完成且 UID 在有效范围内；
读取该 UID 的主表 transaction，空对象立即 fatal；
复用 operation behavior 的统一分类，不按 target 名称猜测；
只有 scalar ROB store commit 返回 1，load 和不支持组合沿既有分类/错误路径处理；
不修改任何运行时状态，因此 build 和 mark 可使用同一个权威判断。
```

### 4.2 xaction 构造与 fault commit

抽象功能描述：`clear_lsqcommit_xaction()` 为每拍 transaction 建立安全基线，`build_lsqcommit_xaction()` 只在选中 fault head 时覆盖本拍值，`mark_fault_rob_commit_uid()` 在 fault token 真正提交成功后更新 latch。

```systemverilog
clear_lsqcommit_xaction(tr):
    tr.pendingst             = 0;
    tr.pendingMMIOld         = 0;
    tr.scommit               = 0;
    tr.flushSb               = 0;
    tr.isStoreException      = latched_is_store_exception;

build_lsqcommit_xaction(...):
    clear_lsqcommit_xaction(tr);
    if (has_fault_head) begin
        tr.isStoreException = fault_uid_is_store_exception(fault_uid);
    end

mark_fault_rob_commit_uid(uid):
    check fault token and modeled ROB head;
    fault_is_store_exception = fault_uid_is_store_exception(uid);
    mark status.rob_commit and fault token;
    retire/sync modeled head;
    latched_is_store_exception = fault_is_store_exception;
```

文字伪代码：

```text
构造 transaction 时先继承最近一次已提交 fault 的 latch；
若本拍没有 fault head，normal head 只计算 pendingst/pendingMMIOld 和 scommit，不能覆盖 latch；
若本拍选中 fault head，则只把该 UID 的 derived store bit 放进本拍 transaction，并保持 pendingst、pendingMMIOld、scommit 为 0；
fault token 经过既有合法性检查并成功更新 rob_commit 后，才把同一 derived bit 写入 latch；
因此 build 后未发送/未提交的 transaction 不会提前污染后续 level 值。
```

这一点保留了原框架的 commit/deq owner：fault type 只影响 DUT 的异常地址来源选择，不能推进 `status.pass`、`terminal_done` 或 LQ/SQ free count。

## 5. lsqcommit Driver 的 level 保持

### 5.1 `send_pkt()` 与 `drive_active_idle()`

源码位置：`mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv`。

抽象功能描述：driver 只传输 handler 已构造的 transaction，并缓存可持续的 ROB sideband；它不查看 main table，也不自行判断 fault 类型。

```systemverilog
send_pkt(tr):
    drv_cb.isStoreException <= tr.isStoreException;
    cached_is_store_exception = tr.isStoreException;
    cached_sideband_valid = 1;

drive_active_idle():
    drv_cb.pendingPtr/pendingst/pendingMMIOld <= cached values;
    drv_cb.isStoreException <= cached_sideband_valid ?
                               cached_is_store_exception : 0;
    drv_cb.scommit <= 0;
    drv_cb.flushSb <= 0;
```

文字伪代码：

```text
收到 sequence item 时，驱动 transaction 的 fault type 并缓存；
no-item、pre-gap 和 post-gap 时，不重新 randomize 或清除 fault type；
只对单拍字段 scommit 和 flushSb 驱动 0；
reset 或从未发送过 item 时，cache 无效，驱动安全 0；
configured DRV_1/DRV_X/DRV_RAND/DRV_LST 仍遵循原有 debug 模式语义。
```

这修复了旧 vecissue idle 周期固定清 0 的问题，且不改变 driver 的 item 握手和 gap 时序。

## 6. Software Smoke 的最小补充

### 6.1 `configure_smoke_env_cfg()` 与环境连接保护

抽象功能描述：`tc_smoke` 提供环境创建前的 cfg hook；software-only 子类只关闭本场景不消费的 DUT output monitor，`memblock_env::connect_phase()` 只在 monitor 真正创建时连接该 monitor 的 analysis port。

```text
tc_smoke::build_phase:
  创建 smoke_cfg，并保留原有 X/Z 配置；
  调用可覆写 configure_smoke_env_cfg(smoke_cfg)；
  把 cfg 放入 env config_db；

soft_test_tc_dispatch_smoke::configure_smoke_env_cfg:
  将 ctrl、int_wb、vec_wb、wakeup、iq_feedback 的 mon_sw 设为 OFF；

memblock_env::connect_phase:
  RM 继续连接各自已创建的 FIFO；
  仅 mon_sw=ON 时连接对应 agent.mon_item_port 到 FIFO analysis_export。
```

原因和边界：software smoke 直接注入 synthetic event，不应由未驱动 DUT output 的 X 值抢先触发 monitor fatal。默认 real smoke 不覆写此 hook，仍保持 `mon_sw=ON`，本次 real smoke 已验证该路径。

### 6.2 `run_fault_case()`、STA IQ hit 与 recovery drain

抽象功能描述：fault smoke 复用公共 admission/issue/writeback/commit helper 构造 load fault 和 store fault 两轮检查；它只补足既有 strict STA 和 recovery 队列前置条件，不改 handler 的主体逻辑。

```text
run_fault_case:
  reset LSQ model 和 commit handler 私有状态；
  建立 UID0 fault head 和 UID1 normal load；
  检查 reset 后 isStoreException 为 0；
  admission/issue 后注入 target fault 和其它必要 normal pass；
  对每个 STA writeback 先推同 SQ key 的 raw IQ hit，再由 adapter/handler 记录当前 issue 成功；
  调用 exception_redirect_replay_task 消费 exception_event_q；
  构造 fault transaction，检查 load=0 或 store=1；
  提交 fault token，检查 waiting idle、younger normal commit、terminal idle 均保持该值；
  按 behavior 释放 LQ/SQ mapping，检查终态。
```

`submit_raw_sta_iq_feedback()` 使用 replay smoke 已有的 `raw_iq_feedback_q -> collect_monitor_event_batch()` 入口，不直接设置 `sta_issue_feedback_success`。`exception_redirect_replay_task()` 使用 `handle_fault_event()` 消费队列，也不重复调用 `mark_target_fault()`。这两个补充使 software smoke 遵循原有 owner，而不是在 testcase 中复制状态机。

## 7. 与原 plan 的一致性和执行中差异

| 原 plan 项 | 当前实现 | 结论 |
|---|---|---|
| 4.1 agent ownership | vecissue 全链移除，lsqcommit interface/xaction/driver/monitor/connect 接管 | 一致 |
| 4.2 handler latch | fault helper 派生；fault commit 成功后写 latch | 一致 |
| 4.3 driver level 保持 | send cache，active idle 和 DRV_0 保持 | 一致 |
| 4.4 load/store fault smoke | load=0、STA store=1、hold/terminal 保持 | 一致 |
| 9.1 helper 改名 | `commit_and_deq_fault_lsq()` 避免 virtual task 参数列表冲突 | 合理的编译期补充 |
| 9.2 output monitor 隔离 | software cfg hook 与 guarded connection | 仿真发现后最小修正 |
| 9.3 strict STA IQ hit | synthetic STA writeback 前复用 raw IQ-hit adapter path | 保持既有 STA 合同 |
| 9.4 recovery drain | synthetic fault 后复用 `exception_redirect_replay_task()` | 保持既有 recovery owner |

9.1 至 9.4 都已在 plan 的 `IMPLEMENTATION_DELTA` 中明确记录原方案、实际实现、原因和范围；没有把新增行为伪装成原始 plan 内容。

## 8. 文档同步检查

已同步的有效文档：

- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`：说明 fault type latch、driver level 保持及不影响 commit/deq。
- `AI_DOC/mem_ut_flow_doc/fault_exception_flow.md`：说明 V2 STA IQ-hit 前置和 software fault recovery drain。
- `AI_DOC/mem_ut_flow_doc/soft_test_and_mixed_directed_flow.md`：说明 software-only monitor 隔离、raw IQ 和 recovery 消费。
- `AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md`：更新字段 owner 为 lsqcommit。
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/lsq_commit_handler.md`、`memblock_lsqcommit_dispatch_sequence.md`、`soft_test_memblock_dispatch_smoke_sequence.md`：更新函数职责、sideband 生命周期和 smoke 构造。

文档没有把本字段误写成 L2TLB、L2Cache 或 vector issue 功能，也没有把 fault type 写成 `pendingst` 的别名。

## 9. 验证记录

| 检查 | 命令/场景 | 结果 |
|---|---|---|
| 格式与旧 owner | `git diff --check`；检索 vecissue/lsqcommit `isStoreException` | 通过；vecissue 无残留，lsqcommit 为唯一 owner |
| 专项 fault smoke | `make clean mode=base_fun` 后 `make eda_run tc=tc_dispatch_fault_smoke mode=base_fun` | 通过；`TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0` |
| 默认 real smoke | `make clean mode=base_fun` 后 `make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke` | 通过；real monitor 均为 ON，`UVM_ERROR=0`，`UVM_FATAL=0` |

专项日志还确认：load fault 的 target fault 为 LOAD，store fault 的 target fault 为 STA；STA 的 IQ hit 先被记录，fault recovery event 被 `EXC_REDIRECT` 消费，末尾 `runtime_drain_complete()` 成立。

## 10. 最终归档前复核结论

本 agent 在归档前重新对照 plan、当前源码、专项 smoke 和已同步文档，结论如下：

| 等级 | 结论 | 复核依据 |
|---|---|---|
| P0 | 无 | 字段只有 lsqcommit connect 在 `MEMBLOCK_UT` 下 force，vecissue 链路无残留。 |
| P1 | 无 | fault UID 分类、fault token 后 latch 更新、normal sideband 隔离和 active-idle 保持均与 plan 一致。 |
| P2 | 无 | software fault smoke 覆盖 load=0、store=1、waiting/terminal 保持、reset、STA IQ-hit 和 recovery drain；历史专项与 real smoke 均通过。 |
| P3 | 无阻塞项 | 不新增 DUT exception address checker、RM、vector LS 或完整 core ROB readback，均为 plan 明确边界。 |

本次归档复核只更新文档状态和路径，不修改功能源码，因此沿用第 9 节已有的专项 fault smoke 与 real smoke
验证记录。当前实现与 plan 的原始内容及第 9 节 `IMPLEMENTATION_DELTA` 一致，未发现遗漏的行为修改。

**最终结论：FINAL PASS。** 关联 plan 可从 `undo` 归档到 `do`。

## 11. 剩余边界

- 本专项 software fault smoke 在 handler 层构造 transaction，因此不单独通过 `start_item/finish_item` 观察 fault transaction 的 VIF 波形或每一种 driver gap；这是 plan 明确的软件 smoke 边界。
- real smoke 已覆盖正常 lsqcommit driver、接口 force、monitor `ON` 的环境拓扑，但没有构造真实 DUT store exception address checker。该 checker 不属于本 plan。
- vector LS、`issueVldu` 和完整 core ROB reset/readback 仍不在本专项支持范围。
