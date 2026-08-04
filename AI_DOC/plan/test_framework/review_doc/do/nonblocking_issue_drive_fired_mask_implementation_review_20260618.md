# Nonblocking Issue Drive Fired Mask 实现 Review

## 1. Review 结论

本次实现与 `AI_DOC/plan/test_framework/plan/do/nonblocking_issue_drive_fired_mask_plan_20260616.md` 的核心方案一致：新增 `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN`，默认保持旧阻塞行为；打开后 lintsissue driver 只采样一次 `valid&&ready`，只把真实 fire port 写入 `memblock_dispatch_fired_mask`；sequence 正常返回路径只对 mask bit 为 1 的 item 调用 `mark_fired_items()`，未 ready item 保留在 issue queue，下轮重新仲裁。

本次 review 未发现阻塞性问题。补充实现了一个 plan 未显式要求但更接近旧行为的细节：非阻塞 driver 在当前 xaction 没有任何 valid port 时直接返回，不额外等待一个 driver clock。

## 2. 修改前逻辑

旧逻辑是阻塞等待模型。sequence 选中 issue item 后，driver 会保持本次 xaction 的 valid，直到所有 selected port 都完成 `valid&&ready`。因此旧正常返回路径可以用 `7'h7f` 表示“本轮 selected item 全部 fire”。

问题是：如果某个 port 长时间 `ready=0`，整个 xaction 会卡在 driver 内，sequence 不能进入下一轮选择；即使其他 port 已经 fire，也不能马上补新的 item。

## 3. 修改后逻辑

新逻辑增加非阻塞模式。非阻塞模式下，driver 一拍只采样一次 ready：

- ready 的 port：置 `fired_mask[port]=1`，sequence 后续删除 issue queue entry 并置 dispatched。
- 未 ready 的 port：`fired_mask[port]=0`，sequence 不调用 `mark_issue_fire()`，该 item 保留在 issue queue。
- flush/redirect 边界：driver 采样前后都检查 flush 状态，若发生 flush/epoch 变化，只按已记录的 `fired_mask` 处理 partial fire。

## 4. 调用关系

| 调用顺序 | 函数/task | 在本流程中的功能 |
|---|---|---|
| 1 | `memblock_lintsissue_dispatch_sequence::send_issue_cycle()` | 创建 xaction，写入非阻塞开关，选择 issue item，并在 driver 返回后按 effective fired mask 更新状态。 |
| 2 | `lintsissue_agent_agent_driver::main_phase()` | 从 sequencer 获取 xaction，按 xaction 字段选择阻塞等待或非阻塞采样。 |
| 3 | `lintsissue_agent_agent_driver::drive_dispatch_issue_one_cycle()` | 非阻塞模式下只采样一次 ready，回填真实 fired mask，清掉剩余 valid。 |
| 4 | `lintsissue_agent_agent_driver::clear_ready_dispatch_issue_ports()` | 逐 port 判断 `valid&&ready`，只对真实 fire 的 port 置 fired mask。 |
| 5 | `memblock_lintsissue_dispatch_sequence::mark_fired_items()` | 只处理 fired mask 为 1 的 item；未 fire item 不出队。 |
| 6 | `issue_queue_scheduler::mark_issue_fire()` | 对真实 fire item 删除 issue queue、设置 dispatched、记录 issue snapshot。 |

## 5. 源码 Review

### 5.1 plus 参数定义与加载

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，对象：`MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN`

函数功能简析：`plus.sv` 是测试框架 plusarg 的原始声明和命令行解析入口。本次新增参数默认值为 0，表示默认不改变旧回归行为。

```systemverilog
// issue driver 非阻塞上流水开关。
// 0：保持旧行为，当前 xaction 内所有 valid port 都 ready 后才结束。
// 1：每个 xaction 只采样一次 ready，未 fire item 留在 issue queue 下轮重试。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN, bit, 1'b0)
```

中文伪代码：

```text
定义 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
默认值为 0：
  不启用非阻塞发射，保持 driver 阻塞等待旧行为。
如果用户通过 cfg 或 plus_arg 设置为 1：
  后续 seq_csr_common 会读取该值，传给 lintsissue xaction。
```

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，函数：`reload_from_cmdline()`

函数功能简析：`reload_from_cmdline()` 从命令行或 cfg 展开的 plusarg 中刷新参数值。本次修改让新增非阻塞开关支持 cfg/plusarg 覆盖。

```systemverilog
load_bit("MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN", MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN);
```

中文伪代码：

```text
从 plusarg 中查找 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
如果用户指定该 key：
  覆盖 plus.sv 中保存的参数值。
如果用户没有指定：
  保持默认 0。
该函数只负责解析参数，不直接影响 driver 行为。
```

### 5.2 seq_csr_common 缓存与 getter

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，字段：`dispatch_issue_nonblocking_en`

函数功能简析：`seq_csr_common` 是公共测试框架参数的统一读取层。新增字段把 plus 参数缓存到 sequence 公共配置，避免 sequence 到处直接访问 `plus::`。

```systemverilog
// 中文注释：lintsissue 非阻塞发射模式开关。
// 为 1 时 driver 只采样一次 ready，未 fire 的 issue item 不出队，后续重新仲裁。
static bit          dispatch_issue_nonblocking_en = 1'b0;
```

中文伪代码：

```text
声明 dispatch_issue_nonblocking_en。
字段含义是当前 testcase 是否启用 lintsissue 非阻塞发射。
默认值为 0：
  sequence 默认按旧阻塞等待模型构造 xaction。
读取者是 get_dispatch_issue_nonblocking_en()。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数：`load_from_plus()`

函数功能简析：`load_from_plus()` 把 `plus.sv` 的原始参数同步到公共 sequence 配置字段。本次新增从 `plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN` 加载到缓存字段。

```systemverilog
dispatch_issue_nonblocking_en = plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN;
```

中文伪代码：

```text
读取 plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
把读取结果写入 dispatch_issue_nonblocking_en。
后续 sequence 不直接读 plus，而是通过 getter 读取该缓存值。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数：`get_dispatch_issue_nonblocking_en()`

函数功能简析：该 getter 是 sequence 读取非阻塞开关的唯一入口，并复用 `check_initialized()` 防止配置未初始化就被使用。

```systemverilog
static function bit get_dispatch_issue_nonblocking_en();
    check_initialized("get_dispatch_issue_nonblocking_en");
    return dispatch_issue_nonblocking_en;
endfunction:get_dispatch_issue_nonblocking_en
```

中文伪代码：

```text
调用 check_initialized：
  如果 seq_csr_common 还未 init，按公共规则 fatal。
返回 dispatch_issue_nonblocking_en。
调用者通过返回值决定本次 xaction 是否启用非阻塞 driver 路径。
```

### 5.3 xaction 模式字段

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv`，字段：`memblock_dispatch_nonblocking_issue`

函数功能简析：该字段是 sequence 与 agent driver 之间的协作字段。sequence 写入当前 xaction 是否非阻塞，driver 只读 xaction，不跨层访问 `seq_csr_common`。

```systemverilog
// 中文注释：本次 issue xaction 是否使用非阻塞 ready 采样。
// 置位：memblock_lintsissue_dispatch_sequence 从 seq_csr_common 读取开关后写入。
// 作用：为 1 时 driver 只采样一次 valid&&ready，未 fire port 不设置 fired_mask，sequence 后续不出队这些 item。
bit memblock_dispatch_nonblocking_issue;
```

中文伪代码：

```text
在 lintsissue xaction 中增加 memblock_dispatch_nonblocking_issue。
该字段不属于 DUT payload。
sequence 构造 xaction 时写入它。
driver main_phase 读取它：
  为 0 时走旧 wait_dispatch_issue_ready。
  为 1 时走 drive_dispatch_issue_one_cycle。
```

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv`，函数：`new()`

函数功能简析：构造 xaction 时把非阻塞字段默认清 0，保证普通 agent 或旧 sequence 未显式设置时仍走旧路径。

```systemverilog
memblock_dispatch_nonblocking_issue = 1'b0;
```

中文伪代码：

```text
创建新的 lintsissue xaction。
默认设置 memblock_dispatch_nonblocking_issue=0。
如果后续 sequence 没有覆盖该字段：
  driver 会继续使用阻塞等待行为。
```

### 5.4 sequence 写入 xaction 并按 effective mask 更新状态

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，task：`send_issue_cycle()`

函数功能简析：`send_issue_cycle()` 是 LOAD/STA/STD issue 发射主入口。本段负责创建 xaction 并把公共配置中的非阻塞开关写到 xaction。

```systemverilog
tr.memblock_dispatch_wait_ready = 1'b1;
// nonblocking_issue=1 表示 driver 只采样一次 ready；未 fire port 不出队，下轮重新参与仲裁。
tr.memblock_dispatch_nonblocking_issue = seq_csr_common::get_dispatch_issue_nonblocking_en();
tr.memblock_dispatch_ready_timeout = seq_csr_common::get_dispatch_ready_timeout();
tr.memblock_dispatch_aborted_by_redirect = 1'b0;
tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
tr.memblock_dispatch_fired_mask = '0;
```

中文伪代码：

```text
构造本拍 issue xaction。
设置 wait_ready=1：
  driver 需要按 intIssue valid/ready 协议处理。
读取 get_dispatch_issue_nonblocking_en：
  把公共配置写入 tr.memblock_dispatch_nonblocking_issue。
设置 ready_timeout：
  阻塞模式仍使用它限制 wait ready 周期。
清 aborted_by_redirect。
记录当前 flush_epoch。
清 fired_mask，等待 driver 回填真实 fire port。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，task：`send_issue_cycle()`

函数功能简析：本段是 driver 正常返回后的状态更新。修改前固定使用 `7'h7f`，修改后非阻塞模式改用 driver 回填的真实 `fired_mask`。

```systemverilog
if (fired_items.size() != 0) begin
    bit [6:0] effective_fired_mask;

    if (tr.memblock_dispatch_nonblocking_issue) begin
        effective_fired_mask = tr.memblock_dispatch_fired_mask;
    end else begin
        effective_fired_mask = 7'h7f;
    end

    if (effective_fired_mask != '0) begin
        mark_fired_items(fired_items, effective_fired_mask);
        has_fire = 1'b1;
    end
end
```

中文伪代码：

```text
如果本拍没有 selected item：
  不做 fire 状态更新。
如果本拍有 selected item：
  判断 xaction 是否是非阻塞模式。
  如果是非阻塞模式：
    effective_fired_mask 使用 driver 回填的真实 fired_mask。
    只有 valid&&ready 的 port 才会被 mark。
  如果是阻塞模式：
    effective_fired_mask 使用 7'h7f。
    因为旧路径 driver 返回时已等待所有 selected port fire。
  如果 effective_fired_mask 非 0：
    调用 mark_fired_items，只处理 mask bit 为 1 的 item。
    设置 has_fire=1，用于 issue loop no-progress 统计。
  如果 effective_fired_mask 为 0：
    不调用 mark_fired_items。
    所有 selected item 都留在 issue queue。
```

### 5.5 driver main_phase 分流

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`，task：`main_phase()`

函数功能简析：driver 主循环收到 xaction 后，先 drive payload，再按 xaction 字段选择旧阻塞等待或新非阻塞采样。

```systemverilog
this.send_pkt(req);
if (req.memblock_dispatch_wait_ready) begin
    if (req.memblock_dispatch_nonblocking_issue) begin
        this.drive_dispatch_issue_one_cycle(req);
    end else begin
        this.wait_dispatch_issue_ready(req);
    end
end
```

中文伪代码：

```text
driver 收到 req。
调用 send_pkt(req)：
  把当前 xaction 的 intIssue valid/payload 驱动到 DUT。
如果 req.memblock_dispatch_wait_ready=0：
  不处理 ready 等待，保持普通 agent 行为。
如果 wait_ready=1：
  如果 memblock_dispatch_nonblocking_issue=1：
    调用 drive_dispatch_issue_one_cycle。
    只采样一次 ready 并回填 fired_mask。
  否则：
    调用 wait_dispatch_issue_ready。
    保持旧行为，等待所有 valid port fire 或 timeout/flush abort。
```

### 5.6 非阻塞 driver 采样路径

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`，task：`drive_dispatch_issue_one_cycle()`

函数功能简析：这是新增的非阻塞 driver 路径。输入是已经由 `send_pkt()` 驱动过一次的 xaction；输出是 `memblock_dispatch_fired_mask` 和可能的 `memblock_dispatch_aborted_by_redirect`。

```systemverilog
task lintsissue_agent_agent_driver::drive_dispatch_issue_one_cycle(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "drive_dispatch_issue_one_cycle got null xaction")
    end

    tr.memblock_dispatch_fired_mask = '0;
    if (!has_dispatch_issue_pending(tr)) begin
        return;
    end

    @this.vif.drv_mp.drv_cb;

    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end

    clear_ready_dispatch_issue_ports(tr);

    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end

    clear_dispatch_issue_ports(tr);
    this.send_pkt(tr);
endtask:drive_dispatch_issue_one_cycle
```

中文伪代码：

```text
检查 tr 是否为空：
  为空则 fatal。
清空 fired_mask。
调用 has_dispatch_issue_pending：
  如果当前 xaction 没有任何 valid port，直接返回，不额外等待一个 clock。
等待一个 driver clocking block。
采样 ready 前检查 flush 状态：
  如果 dispatch_flush_in_progress=1 或 flush_epoch 已变化：
    清掉所有 valid。
    调用 send_pkt 把 valid=0 驱动出去。
    设置 aborted_by_redirect=1。
    返回。
调用 clear_ready_dispatch_issue_ports：
  对每个 valid&&ready port 设置 fired_mask，并清掉该 port valid。
  对未 ready port 不设置 fired_mask。
采样 ready 后再次检查 flush 状态：
  如果 flush 在边界发生：
    清掉剩余 valid。
    drive idle。
    设置 aborted_by_redirect=1。
    返回。
正常非阻塞结束：
  清掉所有剩余 valid。
  drive idle。
  返回 sequence。
```

### 5.7 cfg preset 默认值

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` 以及 `tc_dispatch_real_*` preset cfg

函数功能简析：cfg 文件是 testcase 参数落点。本次新增 key 在 default 和已启用 dispatch issue 的 real smoke preset 中显式设置为 0，保持默认不改变旧行为。

```text
+MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0
```

中文伪代码：

```text
cfg 展开 plusarg 时加入 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0。
默认运行 testcase：
  非阻塞开关为 0。
  driver 仍走 wait_dispatch_issue_ready。
用户需要验证新模式时：
  可通过 plus_arg=+MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=1 覆盖 cfg。
```

## 6. 正确性检查

| 检查点 | 结论 |
|---|---|
| driver 是否直接依赖 `seq_csr_common` | 否。driver 只读 xaction 字段 `memblock_dispatch_nonblocking_issue`。 |
| 非阻塞正常路径是否仍用 `7'h7f` | 否。非阻塞时使用 `tr.memblock_dispatch_fired_mask`。 |
| 阻塞旧行为是否保持 | 是。默认参数 0，阻塞路径仍使用 `wait_dispatch_issue_ready()` 和 `7'h7f`。 |
| 未 ready item 是否会被误删 | 不会。`fired_mask=0` 时 `mark_fired_items()` 直接跳过，不调用 `mark_issue_fire()`。 |
| redirect/flush partial fire 是否保留 | 保留。abort 分支仍只按 `tr.memblock_dispatch_fired_mask` 处理。 |
| cfg key 是否有默认落点 | 有。`default.cfg` 和 dispatch real smoke preset 均显式配置 0。 |

## 7. Plan 对齐检查

对应 plan：`AI_DOC/plan/test_framework/plan/do/nonblocking_issue_drive_fired_mask_plan_20260616.md`。

### 7.1 与 plan 不一致项

存在一处实现增强，不改变 plan 目标：

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`，task：`drive_dispatch_issue_one_cycle()`

```systemverilog
if (!has_dispatch_issue_pending(tr)) begin
    return;
end
```

中文伪代码：

```text
非阻塞 driver 清空 fired_mask 后先检查当前 xaction 是否有任何 valid port。
如果没有 valid port：
  直接返回，不等待下一个 driver clock。
原因：
  旧阻塞路径在 no pending 时 while 不进入，会立即返回。
  非阻塞路径补这个判断后，空轮询行为和旧路径一致，避免没有 issue item 时额外耗一拍。
```

plan 原有逻辑：`drive_dispatch_issue_one_cycle()` 清空 fired mask 后直接等待一个 driver clock 再检查 flush/ready。

当前实现逻辑：清空 fired mask 后先检查 pending valid；如果没有 valid，立即返回。

修改原因：这是性能和行为一致性优化。空 xaction 没有任何 port 需要采样 ready，等待一个 clock 没有意义，且会让非阻塞模式在队列为空时比旧模式慢一拍。

### 7.2 Plan 未说明但 coding 落实的细节

#### 7.2.1 testcase preset cfg 显式补默认值

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_smoke.cfg` 等 dispatch real preset cfg

```text
+MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0
```

中文伪代码：

```text
每个启用 MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1 的 real smoke preset：
  显式设置 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0。
作用：
  新参数在 testcase cfg 中可见。
  默认不改变旧回归行为。
  用户仍可用命令行 plus_arg 覆盖成 1。
```

该细节功能：参数管理规则要求新增公共测试框架参数后 `plus.sv`、`seq_csr_common`、`default.cfg` 和 testcase preset cfg 同步。plan 重点描述了 `default.cfg`，本次实现补齐了各 real smoke preset。

## 8. 验证结果

已执行：

```bash
git diff --check
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke plus_arg=+MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=1
```

结果：

- `git diff --check`：通过。
- `eda_compile tc_sanity/base_fun`：通过，KDB elaboration 0 error / 0 warning。
- `tc_sanity/base_fun`：TEST CASE PASSED，UVM_ERROR=0，UVM_FATAL=0；存在既有 main table wait warning。
- `tc_dispatch_real_smoke/base_fun` 且 `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=1`：TEST CASE PASSED，UVM_WARNING=0，UVM_ERROR=0，UVM_FATAL=0。

## 9. 同步文档清单

本次除代码和 cfg 外，同步更新以下说明文档：

| 文档 | 同步内容 |
|---|---|
| `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md` | 更新 issue flow 图、`send_issue_cycle()`、driver main phase、`drive_dispatch_issue_one_cycle()` 和 fire marking 分支说明。 |
| `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md` | 更新 dispatch control 网页调用图中的 lintsissue driver 非阻塞路径。 |
| `AI_DOC/project_management/mem_ut_parameter_management.md` | 补充 `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN` 的参数归类、默认值和读取路径约束。 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 补充 lintsissue dispatch sequence 参数分组和非阻塞开关说明。 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/seq_csr_common.md` | 补充 `dispatch_issue_nonblocking_en` 字段含义。 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lintsissue_dispatch_sequence.md` | 补充 sequence 对非阻塞参数、xaction 字段和 fired mask 的使用。 |
| `AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md` | 补充 `memblock_dispatch_nonblocking_issue` 与 `drive_dispatch_issue_one_cycle()` 的设计含义。 |

## 10. 剩余风险

非阻塞模式下单个 xaction 不再长时间等待，所以 `MEMBLOCK_DISPATCH_READY_TIMEOUT` 不再覆盖“某 port 长期 ready=0”的场景。当前按 plan 依赖 active sequence no-progress warning 观察整体无进展；若后续需要硬性失败语义，应新增 per-item 或 per-port no-fire watchdog。
