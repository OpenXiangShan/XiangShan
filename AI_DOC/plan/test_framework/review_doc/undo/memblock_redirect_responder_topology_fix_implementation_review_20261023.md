# memblock legacy real-smoke redirect responder 拓扑修复 Implementation Review

## 1. 范围、术语与抽象功能说明

关联计划：
`AI_DOC/plan/test_framework/plan/do/memblock_redirect_responder_topology_fix_plan_20261023.md`

关联方案 review：
`AI_DOC/plan/test_framework/review_doc/undo/memblock_redirect_responder_topology_fix_plan_review_20261023.md`

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `default sequence` | UVM 在 sequencer phase 自动启动的默认 sequence | `uvm_config_db` 的 `default_sequence` | legacy testcase 启动 responder |
| `responder` | 从公共请求队列取出 payload 并驱动 DUT 的被动服务 sequence | `memblock_redirect_dispatch_base_sequence` | drive redirect payload |
| `override` | 派生 testcase 对同一 config-db key 的后写覆盖 | `configure_real_smoke_default_sequences()` | 错误地替换基类 responder |
| `anchor` | redirect monitor 观测到 DUT redirect 后生成的采样锚点 | redirect anchor queue | 将 cancel record 绑定到精确 target sample |
| `cancel record` | 一次 redirect 对应的软件预期、drive 和比较生命周期记录 | `cancel_record_q` | 保存预期 LQ/SQ cancel 数 |
| `baseline snapshot` | 未命中精确比较目标的周期性 DUT cancel 采样 | `cancel_snapshot_history_q` | 验证 held cancel 值稳定 |

本轮只修复 legacy real-smoke 的 redirect responder ownership。它不改变 redirect payload、DUT 接口、
monitor、cancel count、queue 深度或 reconcile 算法。

## 2. 修改前逻辑

`tc_base` 已为 redirect sequencer 安装 queue-driven responder，但派生类
`tc_dispatch_real_smoke::configure_real_smoke_default_sequences()` 又把同一路径覆盖成通用空闲 sequence。
recovery handler 创建 cancel record 并将 payload 放入 `pending_redirect_drive_q` 后，没有 consumer 出队和
drive；DUT 不产生 redirect monitor anchor，后续 baseline snapshot 因未锚定 record 被正确保留，累计到
15 条后触发深度 14 的保护 fatal。

该现象证明 fatal 是缺失 responder 的次生结果，不是 cancel queue 删除失败，也不是 DUT cancel 计数错误。

## 3. 修改后逻辑

### 3.1 基类继续持有唯一 legacy responder

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_base.sv`，函数：`build_phase()`。

抽象功能描述：`tc_base::build_phase()` 为 legacy testcase 的 redirect sequencer 安装唯一 queue-driven
responder，使 recovery 创建的 payload 能被驱动到 DUT；它不负责生成 redirect 请求或计算 cancel 数。

```systemverilog
uvm_config_db#(uvm_object_wrapper)::set(
    this,
    "env.u_redirect_agent_agent.sqr.main_phase",
    "default_sequence",
    memblock_redirect_dispatch_base_sequence::type_id::get()
);
```

中文伪代码：legacy testcase build 时，把 redirect sequencer 的 `main_phase` 默认 sequence 设置为
`memblock_redirect_dispatch_base_sequence`。UVM 进入 main phase 后自动启动该 responder；该配置是 legacy
拓扑的单一 ownership 来源。

### 3.2 派生 testcase 不再覆盖 redirect default sequence

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`，函数：
`configure_real_smoke_default_sequences()`。

抽象功能描述：该函数只把不参与真实 dispatch 服务的 agent 切换为空闲默认 sequence；修改后不再接管
redirect agent，因此继承基类 responder。

```systemverilog
virtual function void configure_real_smoke_default_sequences();
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_backendToTopBypass_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(backendToTopBypass_agent_agent_xaction)::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_fence_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(fence_agent_agent_xaction)::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_csr_ctrl_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(csr_ctrl_agent_agent_xaction)::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_int_sink_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(int_sink_agent_agent_xaction)::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_itlb_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(itlb_agent_agent_xaction)::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_other_ctrl_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(other_ctrl_agent_agent_xaction)::type_id::get());
endfunction:configure_real_smoke_default_sequences
```

中文伪代码：派生 testcase 仍逐项把 bypass、fence、CSR control、interrupt sink、ITLB 和 other-control
agent 设置为空闲 sequence。函数不再写 redirect sequencer 的 config-db key，所以不会覆盖
`tc_base` 已安装的 responder。没有新增第二个 sequence，也没有修改其它 agent 的行为。

### 3.3 responder 消费队列并完成 drive 生命周期

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_redirect_dispatch_base_sequence.sv`，task：
`body()`。

抽象功能描述：该 responder 周期性尝试从公共 redirect drive queue 取出 payload，成功时驱动 DUT，空闲时
发送安全 idle；global stop 且无 pending/active redirect 后退出。它不创建 cancel record。

```systemverilog
if (data.try_pop_redirect_drive(payload)) begin
    drive_redirect_payload(payload);
    idle_count = 0;
end else begin
    drive_idle_once($sformatf("redirect_idle_tr_%0d", idle_count));
    if (data.active_redirect.valid &&
        !data.redirect_drive_done_for(data.active_redirect)) begin
        idle_count++;
    end else begin
        idle_count = 0;
    end
end
```

中文伪代码：每轮先调用 `try_pop_redirect_drive()`；该 helper 只在队列非空且没有 inflight payload 时出队，
并建立 inflight ownership。出队成功后 `drive_redirect_payload()` 把字段送到 driver，握手完成后调用
`mark_redirect_drive_done()` 更新 cancel record 的 drive 完成时间和 anchor deadline。没有 payload 时发送
全零 idle；仅当 active redirect 长期未完成时累积 timeout 计数。

## 4. 正确性检查

| 检查项 | 结论 |
|---|---|
| legacy 双 responder | 不存在；派生 override 被删除，config-db key 只由 `tc_base` 配置 |
| explicit VSEQ 双 responder | 不存在；`basicTest` 不继承 `tc_base`，VSEQ 显式启动唯一 responder |
| cancel 语义是否被放宽 | 否；queue 深度 14、exact target、deadline、比较和 fatal 均未修改 |
| DUT 激励是否伪造 | 否；恢复的是 recovery 已创建但此前无人消费的合法 redirect payload |
| stop/drain | responder 在 queue 和 active redirect 清空后发送安全 idle 并退出 |

## 5. 验证结果

| 验证 | 结果 | 结果目录 |
|---|---|---|
| 最终源码编译 | PASS | `mem_ut/ver/ut/memblock/sim/cancel_redirect_inherit_fix_20261023` |
| 代表原失败 `tc_dispatch_real_store_smoke seed=893303783` | 1/1 PASS | `mem_ut/ver/ut/memblock/sim/cancel_redirect_inherit_fix_seed893303783_20261023` |
| 七个 legacy cfg 的有效历史失败 seed | 22/22 PASS | `mem_ut/ver/ut/memblock/sim/cancel_redirect_inherit_known_*_20261023` |
| cancel-reconcile 显式 VSEQ 随机专项 | 20/20 PASS | `mem_ut/ver/ut/memblock/sim/cancel_redirect_inherit_cancel_reconcile_random20_20261023` |
| real-smoke 显式 VSEQ 随机专项 | 7 个收敛 seed PASS；3 个独立 replay livelock seed 中止；10 个均无 cancel-depth fatal | `mem_ut/ver/ut/memblock/sim/cancel_redirect_inherit_explicit_vseq_random10_20261023` |

原结果中 `default.cfg` 的 5 个 seed 不属于本问题。该 cfg 关闭 issue sequence，而 functional runner 又
强制 `MAIN_TRANS_NUM=1000`，导致 `terminal_done_uid=0`；已作为独立测试框架配置问题继续处理。

显式 VSEQ 的 seed `165489856`、`1242164593`、`898041954` 分别固定在 terminal UID 192、462、737，
同一 uncache load UID 的 redirect/reissue 动态 epoch 增长到 23724、19461、20717，形成独立 replay
livelock。三个日志都没有 cancel snapshot depth fatal，远端进程已正常中止并确认退出；该现象不阻塞
本专项对 redirect responder ownership 的结论，但必须作为后续独立问题关闭。

## 6. 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；最终 coding 按方案删除派生 testcase 的错误 override，由基类保持唯一
legacy redirect responder。

## 7. Plan 未说明但 Coding 落实的细节

未新增 Plan 之外的运行期逻辑。排查期间尝试的 incremental reconcile、held-tail 压缩和诊断修改均已
撤销，不进入最终 diff。

## 8. Review 结论

独立方案 reviewer 未发现功能性 blocker，并确认删除 override 是正确责任层。主 agent 对最终 diff、
legacy/VSEQ topology 和专项结果复核后，未发现改变 cancel 语义、重复 responder 或掩盖 RTL 错误的行为。
