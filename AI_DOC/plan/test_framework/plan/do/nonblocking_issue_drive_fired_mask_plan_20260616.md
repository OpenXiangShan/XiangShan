# 非阻塞 issue drive 与 fired_mask 复用方案

## 1. 背景

当前 mem_ut 的 `lintsissue` 发射路径采用阻塞等待模型：

```text
sequence 从 issue queue 选择 LOAD/STA/STD item
driver 把这些 item 驱动到 DUT intIssue port
driver 等待当前 xaction 内所有 valid port 都完成 valid&&ready
sequence 再统一根据 fire 结果更新状态
```

这个模型功能安全，但吞吐偏保守。若同一批中某个 port 长时间 `ready=0`，当前 xaction 就不能结束，sequence 不能进入下一轮 `select_issue_candidates()`。即使其他 port 已经 fire 并空出来，也不能马上补发新的 item。

本方案目标是新增一种非阻塞上流水模式：

```text
一个 issue xaction 只采样一次 DUT ready。
ready 的 port 通过 fired_mask 标记为已 fire。
未 ready 的 port 不标记 fire、不从 issue queue 删除。
本次 xaction 结束，下一轮 issue select 继续进行。
未 fire 的 uid/target 仍留在 issue queue，后续重新参与仲裁和发射。
```

通俗理解：

```text
当前模式：这一批必须全部发完，才发下一批。
新模式：这一拍谁 ready 谁先走，没走成的留队列，下拍重新竞争。
```

## 2. 当前源码依据

### 2.1 driver 当前阻塞等待位置

文件：

```text
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv
```

当前 driver 在 `main_phase()` 中先驱动 transaction：

```systemverilog
@this.vif.drv_mp.drv_cb;
this.send_pkt(req);
if (req.memblock_dispatch_wait_ready) begin
    this.wait_dispatch_issue_ready(req);
end
```

`wait_dispatch_issue_ready()` 内部持续等待所有 valid port 清空：

```systemverilog
while (has_dispatch_issue_pending(tr)) begin
    @this.vif.drv_mp.drv_cb;
    clear_ready_dispatch_issue_ports(tr);
    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end
    this.send_pkt(tr);
    wait_cycles++;
    ...
end
```

这里的关键点是 `while (has_dispatch_issue_pending(tr))`。只要任意 port 还保持 valid，driver 就不结束当前 xaction。

逻辑目的：

```text
说明当前 driver 为什么会形成“整批等待”的阻塞行为。
该源码依据证明：当前不是每拍只尝试一次 issue，而是同一个 xaction 会一直占用 driver，直到所有 valid port 都被 DUT ready 接收，或者遇到 redirect/flush/timeout。
```

文字伪代码：

```text
driver main_phase 收到 req:
  等待 driver clocking block。
  调用 send_pkt(req)，把 req 中所有 valid port 驱动给 DUT。

  如果 req.memblock_dispatch_wait_ready=1:
    进入 wait_dispatch_issue_ready(req)。

wait_dispatch_issue_ready(req):
  清空 fired_mask。

  while 当前 req 中仍有任意 intIssue port valid=1:
    等待下一拍 driver clocking block。
    调用 clear_ready_dispatch_issue_ports(req):
      对已经 valid&&ready 的 port:
        fired_mask[port]=1。
        清掉该 port valid。

    如果检测到 dispatch_flush_in_progress=1 或 flush_epoch 变化:
      清掉所有剩余 valid。
      drive idle 到 DUT。
      设置 aborted_by_redirect=1。
      返回 sequence。

    调用 send_pkt(req)，继续把未 fire port 的 valid 保持给 DUT。
    wait_cycles++。

    如果 wait_cycles 超过 ready_timeout 且仍有 pending valid:
      打印 timeout 信息并 fatal。

  所有 valid port 都清空后:
    wait_dispatch_issue_ready 返回。
    当前 xaction 才结束。
```

### 2.2 fired_mask 当前语义

同一文件中，`clear_ready_dispatch_issue_ports()` 按 port 独立判断 `valid && ready`：

```systemverilog
if (tr.io_ooo_to_mem_intIssue_X_0_valid &&
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_X_0_ready) begin
    report_dispatch_issue_fire(X, tr);
    tr.memblock_dispatch_fired_mask[X] = 1'b1;
    tr.io_ooo_to_mem_intIssue_X_0_valid = 1'b0;
end
```

因此 `fired_mask[X]=1` 的含义已经很明确：

```text
DUT 的 intIssue port X 在本次 xaction 中真实发生过 valid&&ready。
```

逻辑目的：

```text
说明 fired_mask 的原始语义不是“sequence 选择了哪个 item”，而是“driver 观察到哪个 port 被 DUT 接收”。
这个语义是非阻塞方案可以复用 fired_mask 的基础：只要未 ready port 不设置 fired_mask，它就不会被 sequence 当作已发射。
```

文字伪代码：

```text
clear_ready_dispatch_issue_ports(tr):
  对每个 intIssue port X:
    如果 tr.portX.valid=1 且 DUT.portX.ready=1:
      记录 report_dispatch_issue_fire(X)。
      设置 tr.memblock_dispatch_fired_mask[X]=1。
      清掉 tr.portX.valid=0。

    否则:
      tr.memblock_dispatch_fired_mask[X] 保持原值。
      如果 tr.portX.valid 原来为 1:
        该 port 仍是 pending，是否继续保持 valid 由外层 driver 模式决定。

结果:
  fired_mask[X]=1 只表示 port X 真实 valid&&ready。
  fired_mask[X]=0 表示 port X 没有被确认接收。
```

### 2.3 sequence 当前状态更新位置

文件：

```text
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv
```

`mark_fired_items()` 只处理 `fired_mask` 为 1 的 item：

```systemverilog
if (!fired_mask[port_idx]) begin
    continue;
end

if (data.issue_blocked_by_global_flush()) begin
    fire_marked = issue_sched.mark_issue_fire_already_accepted(fired_items[idx]);
end else begin
    fire_marked = issue_sched.mark_issue_fire(fired_items[idx]);
end
```

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv
```

`mark_issue_fire()` 才真正删除 issue queue 并更新状态：

```systemverilog
data.mark_issue_snapshot(item.uid, item.target, issue_epoch);
data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
set_target_queued(item.uid, item.target, 1'b0);
set_target_dispatched(item.uid, item.target, 1'b1);
data.clear_replay_target_after_fire(item.uid, item.target);
```

所以当前状态机天然支持以下语义：

```text
fired_mask=1：
  该 item 真正 fire，删除 issue queue，queued_xxx=0，xxx_dispatched=1。

fired_mask=0：
  该 item 没有真正 fire，不调用 mark_issue_fire()，不删除 issue queue。
```

逻辑功能：

```text
本段说明 sequence 侧如何根据 driver 返回的 fired_mask 更新状态。
driver 只负责把 valid/ready 采样结果写入 fired_mask，不直接删除 issue queue，也不直接把 uid 标记为 dispatched。
真正改变 uid 状态、删除 issue queue 的动作发生在 mark_fired_items() 内部，并且只对 fired_mask 对应 bit 为 1 的 item 执行。
因此非阻塞方案的关键正确性基础是：未 ready port 的 fired_mask 必须保持 0，这样对应 item 会留在 issue queue，后续可以重新发射。
```

文字伪代码：

```text
mark_fired_items(fired_items, fired_mask):
  1. 遍历本轮 sequence 已经选中、并记录在 fired_items 中的每个 item。
     fired_items 的来源是 select_issue_candidates() 的选择结果，表示“本轮尝试驱动到 DUT 的候选”，不等于“已经被 DUT 接收”。

  2. 根据 item.target 和 item.uop_index 计算 port_idx。
     如果 target 是 LOAD：
       port_idx = item.uop_index，对应 load issue port。
     如果 target 是 STA：
       port_idx = item.uop_index + 3，对应 sta issue port。
     如果 target 是 STD：
       port_idx = item.uop_index + 5，对应 std issue port。
     这个 port_idx 用来读取 fired_mask 中同一个物理 issue port 的接收结果。

  3. 检查 fired_mask[port_idx]。
     如果 fired_mask[port_idx] 为 0：
       说明该 item 对应 port 本轮没有完成 valid&&ready。
       直接 continue 到下一个 item。
       不调用 mark_issue_fire()。
       不删除 issue queue entry。
       不清 queued_xxx。
       不置 xxx_dispatched。
       因此该 item 后续仍能被 select_issue_candidates() 再次选中。

  4. 如果 fired_mask[port_idx] 为 1：
     说明该 item 对应 port 本轮已经被 DUT valid&&ready 接收。
     需要把测试框架状态从“queued”推进到“dispatched”。

  5. 调用 data.issue_blocked_by_global_flush()：
     这个 helper 判断当前是否处于全局 flush/redirect 阻塞阶段。
     如果返回 1，表示本轮 fire 发生在 redirect/flush 边界，item 已经被 DUT 接收，但后续普通发射状态推进需要走保守路径。

  6. 如果 data.issue_blocked_by_global_flush() 返回 1：
     调用 issue_sched.mark_issue_fire_already_accepted(item)。
     该 helper 的作用是记录“这个 item 已经被 DUT 接收”，但按 flush 边界语义避免重复执行普通 fire 路径中不应执行的动作。
     它的输入是当前 item，副作用是更新和 accepted/fire 相关的状态。

  7. 如果 data.issue_blocked_by_global_flush() 返回 0：
     调用 issue_sched.mark_issue_fire(item)。
     该 helper 是正常发射成功路径，负责删除 issue queue entry、记录 issue snapshot、更新 queued/dispatched 状态，并清理 replay target。

mark_issue_fire(item):
  1. 为本次 issue fire 分配或读取 issue_epoch。
     issue_epoch 用于区分同一 uid/target 的不同 fire 实例，后续 writeback/replay 匹配会使用它。

  2. 调用 data.mark_issue_snapshot(item.uid, item.target, issue_epoch)。
     该 helper 把 uid、target、issue_epoch 等信息记录到公共状态表中，表示这个 target 已经形成一次真实 issue 实例。

  3. 调用 data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1)。
     该 helper 从对应 LOAD/STA/STD issue queue 中删除 uid、target、replay_seq 匹配的 entry。
     replay_seq 用于避免误删旧 replay 轮次或新 replay 轮次的同 uid entry。

  4. 调用 set_target_queued(item.uid, item.target, 1'b0)。
     该 helper 清除当前 target 的 queued 状态，例如 queued_load/queued_sta/queued_std。
     清除后表示该 target 不再停留在待发射队列中。

  5. 调用 set_target_dispatched(item.uid, item.target, 1'b1)。
     该 helper 设置当前 target 的 dispatched 状态，例如 load_dispatched/sta_dispatched/std_dispatched。
     设置后表示该 target 已经真实送入 DUT issue 接口。

  6. 调用 data.clear_replay_target_after_fire(item.uid, item.target)。
     该 helper 清理当前 target 的 replay 请求标记。
     作用是防止 replay 重发成功后，旧 replay_pending target 继续影响后续 route/issue 判断。
```

## 3. 修改目标

新增一个可控模式：

```text
MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN
```

行为：

```text
0：保持当前阻塞等待模式。
1：启用非阻塞 issue drive。
```

非阻塞模式下：

```text
driver 不再等待当前 xaction 中所有 valid port 都 ready。
driver 只采样一次 ready。
driver 通过 fired_mask 告诉 sequence 哪些 port 真实 fire。
driver 清掉未 fire port 的 valid，结束当前 xaction。
sequence 只 mark fired_mask=1 的 item。
未 fire item 留在 issue queue，下一轮重新参与 select。
```

## 4. 参数方案

### 4.1 plus.sv

文件：

```text
mem_ut/ver/ut/memblock/env/plus.sv
```

新增：

```systemverilog
// 中文注释：issue driver 非阻塞上流水开关。
// 0：保持旧行为，当前 xaction 内所有 valid port 都 ready 后才结束。
// 1：每个 xaction 只采样一次 ready，未 fire item 留在 issue queue 下轮重试。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN, bit, 1'b0)
```

`reload_from_cmdline()` 增加：

```systemverilog
load_bit("MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN",
         MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN);
```

逻辑目的：

```text
把非阻塞 issue drive 做成可控 plusarg，默认保持旧阻塞行为。
回归默认不改变现有测试语义；只有显式打开 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN 时，才启用一次 ready 采样模式。
```

文字伪代码：

```text
plus.sv 初始化:
  定义 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN，类型为 bit，默认值为 0。

reload_from_cmdline():
  从命令行读取同名 plusarg。
  如果用户传入该 plusarg：
    覆盖 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
  如果用户没有传入：
    保持默认值 0。

最终效果:
  MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0:
    driver 仍使用 wait_dispatch_issue_ready() 的旧阻塞路径。
  MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=1:
    sequence 后续把非阻塞模式写入 xaction，driver 走一次 ready 采样路径。
```

### 4.2 seq_csr_common.sv

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv
```

新增静态字段：

```systemverilog
// 中文注释：lintsissue 非阻塞发射模式开关。
// 为 1 时 driver 只采样一次 ready，未 fire 的 issue item 不出队，后续重新仲裁。
static bit dispatch_issue_nonblocking_en = 1'b0;
```

`load_from_plus()` 中读取：

```systemverilog
dispatch_issue_nonblocking_en = plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN;
```

新增 getter：

```systemverilog
static function bit get_dispatch_issue_nonblocking_en();
    check_initialized("get_dispatch_issue_nonblocking_en");
    return dispatch_issue_nonblocking_en;
endfunction:get_dispatch_issue_nonblocking_en
```

逻辑目的：

```text
把 plus.sv 中的原始 plusarg 缓存在 sequence 通用 CSR 层，供 memblock sequence 读取。
避免各个 sequence 直接散落访问 plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
通过 getter 统一做 initialized 检查，防止配置尚未 load_from_plus() 就被使用。
```

文字伪代码：

```text
seq_csr_common 声明阶段:
  增加 static bit dispatch_issue_nonblocking_en，默认 0。

load_from_plus():
  在其他 CSR 字段加载完成的同一流程中：
    dispatch_issue_nonblocking_en = plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
  该值成为本次 test 的 sequence 侧稳定配置。

get_dispatch_issue_nonblocking_en():
  先调用 check_initialized("get_dispatch_issue_nonblocking_en")。
  如果 seq_csr_common 尚未初始化：
    按现有 check_initialized 规则报错。
  否则：
    返回 dispatch_issue_nonblocking_en。
```

## 5. driver 修改方案

文件：

```text
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv
```

### 5.1 新增 task

新增：

```systemverilog
extern task drive_dispatch_issue_one_cycle(lintsissue_agent_agent_xaction tr);
```

逻辑目的：

```text
为非阻塞模式提供独立 driver 路径。
该路径只观察一个 driver clocking block 中的 ready，记录真实 valid&&ready 到 fired_mask，然后主动撤掉剩余 valid。
它不删除 issue queue，也不决定 item 状态；状态更新仍由 sequence 根据 fired_mask 统一完成。
```

实现伪代码：

```systemverilog
task lintsissue_agent_agent_driver::drive_dispatch_issue_one_cycle(
    lintsissue_agent_agent_xaction tr
);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "drive_dispatch_issue_one_cycle got null xaction")
    end

    tr.memblock_dispatch_fired_mask = '0;
    if (!has_dispatch_issue_pending(tr)) begin
        return;
    end

    // main_phase 中 send_pkt(req) 已经把 valid/payload 驱动到 DUT。
    // 下一拍先检查 flush/redirect，再采样 ready。
    // 这样可以避免 flush 已经进入恢复流程时，还先把边界拍 ready 当作普通 fire。
    @this.vif.drv_mp.drv_cb;

    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end

    // 只有确认本拍还没有进入 flush/redirect 恢复时，才采样 valid&&ready。
    clear_ready_dispatch_issue_ports(tr);

    // 采样后再检查一次 flush epoch，覆盖采样拍边界状态变化。
    // 如果这里发现 flush，sequence 会走 aborted_by_redirect 分支，只处理 fired_mask 已记录的 port。
    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end

    // 非阻塞模式不继续等未 ready port。
    // 清掉未 fire port 的 valid，结束本次 xaction。
    clear_dispatch_issue_ports(tr);
    this.send_pkt(tr);
endtask
```

文字伪代码：

```text
drive_dispatch_issue_one_cycle(tr):
  如果 tr 为空，fatal。
  清空 fired_mask。
  如果当前 xaction 没有任何 intIssue valid：
    直接返回。
    这样空 xaction 不会额外等待一个 driver clock，保持旧阻塞路径无 pending 时立即返回的行为。

  等待一个 driver clocking block。

  采样 ready 前，先检查全局 flush 状态：
    如果 dispatch_flush_in_progress=1，或者 flush_epoch 已变化：
      清掉所有 intIssue valid。
      drive idle 到 DUT。
      标记 aborted_by_redirect=1。
      返回。

  调用 clear_ready_dispatch_issue_ports(tr)：
    对每个 port：
      如果 tr.valid=1 且 DUT ready=1：
        fired_mask[port]=1。
        清掉该 port 的 tr.valid。
      否则：
        fired_mask[port] 保持 0。
        tr.valid 暂时保持原值。

  采样 ready 后，再检查一次 flush 状态：
    如果 flush 在采样边界出现：
      清掉剩余 valid。
      drive idle 到 DUT。
      标记 aborted_by_redirect=1。
      返回。

  正常非阻塞结束：
    清掉所有剩余 valid，包含本拍未 ready 的 port。
    drive idle 到 DUT。
    返回 sequence。
```

### 5.2 修改 main_phase

当前：

```systemverilog
this.send_pkt(req);
if (req.memblock_dispatch_wait_ready) begin
    this.wait_dispatch_issue_ready(req);
end
```

修改为：

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

说明：

```text
阻塞模式保持旧逻辑。
非阻塞模式只采一次 ready，不因单个 port 不 ready 卡住整个 driver。
```

实现规则：

```text
driver 不直接调用 seq_csr_common。
非阻塞开关由 sequence 写入 xaction，driver 只读取 xaction 字段。
```

新增 `lintsissue_agent_agent_xaction` 的 memblock 专用字段：

```systemverilog
bit memblock_dispatch_nonblocking_issue;
```

原因：

```text
lintsissue_agent_agent_driver 属于 agent 层。
seq_csr_common 属于 memblock sequence/config 层。
driver 直接引用 seq_csr_common 会引入跨层依赖，不利于 agent 复用。
通过 xaction 字段传递模式开关，层次关系更清晰。
```

逻辑目的：

```text
在 driver 主循环中保留旧阻塞路径，同时按 xaction 字段选择新的非阻塞路径。
这样同一个 driver 可以同时支持旧回归和新实验模式，模式来源由 sequence 决定，driver 不感知 plus/CSR 层。
```

文字伪代码：

```text
main_phase 收到 req 后:
  等待 driver clocking block。
  send_pkt(req)，把 payload 和 valid 驱动到 DUT。

  如果 req.memblock_dispatch_wait_ready=0:
    不进入 issue ready 等待逻辑，按原普通 agent 行为结束本次 item。

  如果 req.memblock_dispatch_wait_ready=1:
    如果 req.memblock_dispatch_nonblocking_issue=1:
      调用 drive_dispatch_issue_one_cycle(req)。
      只采样一次 ready，并用 req.memblock_dispatch_fired_mask 回传真实 fire 结果。
    否则:
      调用 wait_dispatch_issue_ready(req)。
      保持旧行为，等待当前 xaction 内所有 valid port fire 或 timeout/redirect。
```

### 5.3 xaction 字段

文件：

```text
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv
```

新增字段：

```systemverilog
bit memblock_dispatch_nonblocking_issue;
```

逻辑目的：

```text
把 sequence 侧的非阻塞模式选择传递给 driver。
该字段只表示本次 xaction 是否使用非阻塞 issue drive，不改变 payload、valid 或 fired_mask 的既有含义。
```

文字伪代码：

```text
xaction 创建时:
  memblock_dispatch_nonblocking_issue 默认值为 0。

sequence 构造 issue xaction 时:
  从 seq_csr_common.get_dispatch_issue_nonblocking_en() 读取模式。
  写入 tr.memblock_dispatch_nonblocking_issue。

driver 处理 req 时:
  仅读取 req.memblock_dispatch_nonblocking_issue。
  如果为 0:
    走 wait_dispatch_issue_ready()。
  如果为 1:
    走 drive_dispatch_issue_one_cycle()。

约束:
  driver 不直接读取 plus.sv。
  driver 不直接调用 seq_csr_common。
  fired_mask 仍只由 driver 根据 valid&&ready 设置。
```

### 5.4 driver 修改后的完整程序流

逻辑目的：

```text
把第 5 章中新增 task、main_phase 分支、已有 helper 的关系串起来，避免只看到局部代码而不清楚完整执行顺序。
本节说明 driver 修改后每个函数的职责、输入、输出，以及这些函数如何协同保证“ready 的 port 出队，未 ready 的 port 留队”。
```

#### 5.4.1 `main_phase()` 程序流

输入：

```text
req：sequence 通过 sequencer 送来的 lintsissue_agent_agent_xaction。
req.memblock_dispatch_wait_ready：是否需要按 intIssue valid/ready 语义等待或采样。
req.memblock_dispatch_nonblocking_issue：是否启用非阻塞 issue drive。
```

输出：

```text
req.memblock_dispatch_fired_mask：driver 回填的真实 fire port mask。
req.memblock_dispatch_aborted_by_redirect：等待或采样期间遇到 redirect/flush 时置 1。
DUT intIssue port：由 send_pkt(req) 驱动 valid/payload 或 idle。
```

文字伪代码：

```text
main_phase:
  循环从 seq_item_port.try_next_item(req) 获取 xaction。

  如果 req 为空:
    driver 进入 idle 行为。
    继续下一轮。

  如果 req 非空:
    先处理 pre_pkt_gap:
      每个 gap 周期 drive_idle。

    等待一个 driver clocking block。
    调用 send_pkt(req):
      把 req 中所有 intIssue valid/payload 驱动到 DUT。

    如果 req.memblock_dispatch_wait_ready=0:
      不处理 fired_mask。
      直接进入 post_pkt_gap 和 item_done。

    如果 req.memblock_dispatch_wait_ready=1:
      如果 req.memblock_dispatch_nonblocking_issue=1:
        调用 drive_dispatch_issue_one_cycle(req)。
        该 task 最多等待一个采样周期，并回填 fired_mask。
      否则:
        调用 wait_dispatch_issue_ready(req)。
        旧路径会一直等待所有 valid port fire、redirect/flush abort 或 timeout。

    处理 post_pkt_gap:
      每个 gap 周期 drive_idle。

    调用 seq_item_port.item_done()。
```

#### 5.4.2 `drive_dispatch_issue_one_cycle()` 程序流

输入：

```text
tr：当前 issue xaction，已由 main_phase 调用 send_pkt(tr) 驱动过一次。
tr.memblock_dispatch_flush_epoch：发射开始时记录的全局 flush epoch。
memblock_sync_pkg::dispatch_flush_in_progress：当前是否处于 flush/redirect 恢复。
memblock_sync_pkg::dispatch_flush_epoch：当前全局 flush 版本。
DUT intIssue ready：各 port 当前 ready 值。
```

输出：

```text
tr.memblock_dispatch_fired_mask：本次一次采样中真实 valid&&ready 的 port。
tr.memblock_dispatch_aborted_by_redirect：采样前后检测到 flush/redirect 时置 1。
DUT intIssue port：未 fire port 最终被清 valid，driver 不继续保持本 xaction。
```

文字伪代码：

```text
drive_dispatch_issue_one_cycle(tr):
  检查 tr 是否为空，为空 fatal。
  清空 tr.memblock_dispatch_fired_mask。
  调用 has_dispatch_issue_pending(tr)：
    如果没有任何 port valid，直接返回 main_phase。
    该分支只处理空 xaction，不修改 fired_mask、不设置 aborted_by_redirect。

  等待一个 driver clocking block。

  采样 ready 前先检查 flush:
    如果 dispatch_flush_in_progress=1 或 flush_epoch 已变化:
      调用 clear_dispatch_issue_ports(tr) 清掉所有 valid。
      调用 send_pkt(tr) 把 idle/valid=0 驱动到 DUT。
      设置 tr.memblock_dispatch_aborted_by_redirect=1。
      返回 main_phase。

  调用 clear_ready_dispatch_issue_ports(tr):
    对每个 port 根据 valid&&ready 更新 fired_mask。
    ready 的 port 清 valid。
    不 ready 的 port 暂时保持 valid，等待后续统一清理。

  采样 ready 后再次检查 flush:
    如果 dispatch_flush_in_progress=1 或 flush_epoch 已变化:
      调用 clear_dispatch_issue_ports(tr) 清掉剩余 valid。
      调用 send_pkt(tr) 驱动 idle/valid=0。
      设置 tr.memblock_dispatch_aborted_by_redirect=1。
      返回 main_phase。

  正常非阻塞结束:
    调用 clear_dispatch_issue_ports(tr) 清掉所有剩余 valid。
    调用 send_pkt(tr) 驱动 idle/valid=0。
    返回 main_phase。
```

#### 5.4.3 `clear_ready_dispatch_issue_ports()` 程序流

输入：

```text
tr：当前 xaction，包含每个 intIssue port 的 valid。
vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_*_ready：DUT 每个 port 的 ready。
```

输出：

```text
tr.memblock_dispatch_fired_mask[port]：valid&&ready 的 port 置 1。
tr.io_ooo_to_mem_intIssue_*_valid：已经 fire 的 port 清 0。
```

文字伪代码：

```text
clear_ready_dispatch_issue_ports(tr):
  如果 tr 为空，fatal。

  对 port6 到 port0 分别执行:
    如果 tr.port.valid=1 且 DUT.port.ready=1:
      调用 report_dispatch_issue_fire(port, tr) 打印 debug 信息。
      tr.memblock_dispatch_fired_mask[port]=1。
      tr.port.valid=0。
    否则:
      不修改 fired_mask。
      不修改该 port valid。

  函数返回后:
    fired_mask 表示本次采样中真实 fire 的 port。
    仍保持 valid=1 的 port 表示本次未 ready，是否继续等由外层 driver 路径决定。
```

#### 5.4.4 `clear_dispatch_issue_ports()` 程序流

输入：

```text
tr：当前 xaction。
```

输出：

```text
tr 中所有 intIssue valid 被清 0。
payload 字段不作为状态真源，可保持原值；DUT 侧只看 valid。
```

文字伪代码：

```text
clear_dispatch_issue_ports(tr):
  如果 tr 为空，fatal。

  将 intIssue_6_0_valid 清 0。
  将 intIssue_5_0_valid 清 0。
  将 intIssue_4_0_valid 清 0。
  将 intIssue_3_0_valid 清 0。
  将 intIssue_2_0_valid 清 0。
  将 intIssue_1_0_valid 清 0。
  将 intIssue_0_0_valid 清 0。

  不修改 fired_mask。
  不修改 queued/dispatched/pass 等公共状态。
```

#### 5.4.5 `has_dispatch_issue_pending()` 程序流

输入：

```text
tr：当前 xaction。
```

输出：

```text
返回是否仍有任意 intIssue port valid=1。
```

文字伪代码：

```text
has_dispatch_issue_pending(tr):
  如果 tr 为空，fatal。

  如果 port6 valid=1，返回 1。
  如果 port5 valid=1，返回 1。
  如果 port4 valid=1，返回 1。
  如果 port3 valid=1，返回 1。
  如果 port2 valid=1，返回 1。
  如果 port1 valid=1，返回 1。
  如果 port0 valid=1，返回 1。

  否则返回 0。

使用场景:
  阻塞模式 wait_dispatch_issue_ready() 用它决定是否继续 while 等待。
  非阻塞模式不使用它做长等待，只通过 clear_dispatch_issue_ports() 主动结束本 xaction。
```

## 6. sequence 修改方案

文件：

```text
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv
```

### 6.1 sequence 写入 xaction

构造 lintsissue xaction 时写入非阻塞模式：

```systemverilog
tr.memblock_dispatch_nonblocking_issue =
    seq_csr_common::get_dispatch_issue_nonblocking_en();
```

逻辑目的：

```text
sequence 是了解 memblock 配置和 issue 发射语义的层次。
它负责把全局 CSR 配置落到具体 xaction 上，让 agent driver 只按 xaction 执行，不跨层读取配置。
```

文字伪代码：

```text
send_issue_cycle 或构造 issue xaction 的等价位置:
  创建 tr。
  设置 tr.memblock_dispatch_wait_ready=1。
  设置 tr.memblock_dispatch_flush_epoch 为当前 dispatch_flush_epoch。
  填入本轮 selected LOAD/STA/STD item 对应的 port valid 和 payload。

  读取 cfg_nonblocking = seq_csr_common::get_dispatch_issue_nonblocking_en()。
  tr.memblock_dispatch_nonblocking_issue = cfg_nonblocking。

  start_item/finish_item 或等价发送流程把 tr 交给 driver。

driver 返回后:
  sequence 不重新猜测 driver 模式。
  继续使用 tr.memblock_dispatch_nonblocking_issue 和 tr.memblock_dispatch_fired_mask 判定 mark 行为。
```

### 6.2 正常返回路径 effective_fired_mask

当前正常路径使用：

```systemverilog
if (fired_items.size() != 0) begin
    mark_fired_items(fired_items, 7'h7f);
    has_fire = 1'b1;
end
```

这个逻辑依赖旧阻塞模式假设：

```text
driver 返回时，所有 selected item 都已经 fire。
```

非阻塞模式下该假设不成立。硬性规则：

```text
非阻塞模式下，正常路径严禁使用 7'h7f 代表全部 fire。
非阻塞模式必须只使用 tr.memblock_dispatch_fired_mask。
```

需要改成：

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

文字伪代码：

```text
send_issue_cycle 正常返回后:
  如果 fired_items 为空：
    本拍没有尝试发射 item，直接结束。

  如果 tr.memblock_dispatch_nonblocking_issue=1：
    effective_fired_mask = tr.memblock_dispatch_fired_mask。
    含义：只处理 driver 确认 valid&&ready 的 port。

  否则：
    effective_fired_mask = 7'h7f。
    含义：旧阻塞模式下 driver 返回时 selected port 已全部完成 fire。

  如果 effective_fired_mask 非 0：
    调用 mark_fired_items(fired_items, effective_fired_mask)。
    只有 mask bit 为 1 的 item 会进入 mark_issue_fire()。
    mask bit 为 0 的 item 不出队、不置 dispatched。
```

逻辑目的：

```text
把“selected item”与“真正 fire 的 item”拆开。
阻塞模式下二者等价，所以可以继续使用 7'h7f 保持旧行为。
非阻塞模式下二者不等价，必须使用 driver 回填的真实 fired_mask，防止未 ready item 被误删队列。
```

redirect abort 分支不需要改变，因为它当前已经使用真实 `tr.memblock_dispatch_fired_mask`：

```systemverilog
if (tr.memblock_dispatch_aborted_by_redirect) begin
    if (tr.memblock_dispatch_fired_mask != '0) begin
        mark_fired_items(fired_items, tr.memblock_dispatch_fired_mask);
        has_fire = 1'b1;
    end
    return;
end
```

### 6.3 redirect/flush abort 返回路径

逻辑目的：

```text
保持 redirect/flush partial fire 的旧语义。
当 driver 标记 aborted_by_redirect 时，sequence 不能假设 selected item 全部完成，只能处理 fired_mask 中已经被 DUT 接收的 item。
```

文字伪代码：

```text
driver 返回后:
  如果 tr.memblock_dispatch_aborted_by_redirect=1:
    如果 tr.memblock_dispatch_fired_mask 非 0:
      调用 mark_fired_items(fired_items, tr.memblock_dispatch_fired_mask)。
      只让 fired_mask=1 的 item 进入 mark_issue_fire 或 mark_issue_fire_already_accepted。
      设置 has_fire=1。
    如果 tr.memblock_dispatch_fired_mask 为 0:
      不 mark 任何 item。
      所有 selected item 都保持未 fire 状态，等待 flush/replay/队列清理逻辑处理。
    直接 return，不进入正常路径 effective_fired_mask 逻辑。
```

## 7. 未 fire item 后续如何重新发射

未 fire item 不会进入 `mark_issue_fire()`，因此状态保持：

```text
queued_load/queued_sta/queued_std = 1
load_dispatched/sta_dispatched/std_dispatched = 0
issue queue entry 仍然存在
```

下一轮 `select_issue_candidates()` 会重新扫描 issue queue。只要满足：

```text
active=1
enq=1
issue_ready=1
flushed=0
redirect_pending=0
exception_pending=0
issue_killed=0
replay_seq 匹配
ready_cycle=0
对应 target dispatched=0
```

该 item 仍是 eligible，会重新参与仲裁。

是否再次被选中取决于：

```text
send_pri/ROB age 仲裁结果
本拍 sample_*_pip_num() 的可选 port 数
是否启用 global_send_pri filter
是否被 redirect/flush/replay 状态过滤
```

第一版不建议给未 fire item 增加 retry delay。若后续发现同一 item 因同一 port 长期不 ready 而反复被选中，影响其他 item，再考虑增加 `ready_cycle=1` 或 per-port retry backoff。

逻辑目的：

```text
非阻塞模式下，未 fire item 不能丢，也不能提前置 dispatched。
它必须继续保留在 issue queue 中，依赖现有 eligible/select 仲裁逻辑重新发射，从而复用已有队列、公平性和 replay_seq 检查。
```

文字伪代码：

```text
mark_fired_items(fired_items, fired_mask):
  遍历 fired_items 中每个 selected item。
  取出该 item 对应 port_idx。

  如果 fired_mask[port_idx]=0:
    continue。
    不调用 mark_issue_fire()。
    不调用 delete_issue_queue_entry()。
    不清 queued_load/queued_sta/queued_std。
    不置 load_dispatched/sta_dispatched/std_dispatched。
    不清 replay target。

  如果 fired_mask[port_idx]=1:
    按现有逻辑 mark_issue_fire()。
    删除 issue queue entry。
    清 queued_xxx。
    置 xxx_dispatched。
    清 replay target。

下一轮 select_issue_candidates():
  扫描 issue queue。
  对未 fire item 重新执行 active/enq/ready/flushed/redirect/replay_seq/dispatched 等 eligible 检查。
  如果仍 eligible 且仲裁胜出:
    重新写入新的 issue xaction。
  如果不 eligible:
    留给现有 flush/replay/exception/kill 逻辑处理。
```

## 8. 对 redirect/flush 场景的影响

### 8.1 原有 partial fire 场景

当前 `fired_mask` 原本就用于 redirect/flush 边界拍：

```text
等待 ready 期间，如果 flush epoch 变化：
  已经 valid&&ready 的 port 写入 fired_mask。
  未 fire port 清 valid。
  memblock_dispatch_aborted_by_redirect=1。
  sequence 只 mark fired_mask=1 的 item。
```

非阻塞模式不会破坏这个语义。它只是把正常路径也改成“以真实 fired_mask 为准”。

逻辑目的：

```text
在 redirect/flush 边界，保证已经被 DUT 接收的 issue 不丢，未被接收的 issue 不误报 fire。
非阻塞模式复用 fired_mask，而不是引入第二套 partial fire 状态，避免普通路径和 abort 路径语义分裂。
```

文字伪代码：

```text
driver 等待或采样 ready 的过程中:
  如果某个 port 已经 valid&&ready:
    fired_mask[port]=1。
    该 port valid 清 0。

  如果检测到 dispatch_flush_in_progress=1 或 flush_epoch 变化:
    清掉所有剩余 valid。
    send_pkt(tr) 把 idle 驱动到 DUT。
    tr.memblock_dispatch_aborted_by_redirect=1。
    返回 sequence。

sequence 看到 aborted_by_redirect=1:
  只按 tr.memblock_dispatch_fired_mask mark 已 fire item。
  fired_mask=0 的 selected item 不出队。
  active redirect/flush 后续由已有全局 flush/replay/清理路径接管。
```

### 8.1.1 fired_mask 复用的核心约束

`fired_mask` 可以同时服务普通非阻塞发射和 redirect/flush partial fire，但必须遵守以下约束：

```text
约束 1：
  只有 driver 确认某个 port 发生 valid&&ready，才能设置 fired_mask[port]=1。

约束 2：
  sequence 只有看到 fired_mask[port]=1，才能调用 mark_issue_fire() 删除对应 item。

约束 3：
  非阻塞正常路径不能使用 7'h7f 伪造全 fire，必须使用真实 tr.memblock_dispatch_fired_mask。

约束 4：
  driver 采样 ready 前必须先检查 flush/redirect 状态。
  采样 ready 后还要再检查一次 flush/redirect 状态。

约束 5：
  active redirect/flush 已开始时，issue route/select/mark 仍由 issue_blocked_by_global_flush() 阻止。
```

这些约束的目的：

```text
保证 fired_mask 始终只表达“DUT 实际接收的 port”。
避免未 ready item 被错误出队。
避免 flush 已经开始后，旧 epoch item 继续按普通 fire 更新状态。
保持 redirect/flush partial fire 旧语义不变。
```

### 8.2 场景对比

| 场景 | 阻塞模式旧行为 | 非阻塞模式行为 | 影响 |
|---|---|---|---|
| 无 redirect/flush，所有 selected port ready | 所有 selected item 出队并置 dispatched | `fired_mask` 全 1，所有 selected item 出队 | 不影响 |
| 无 redirect/flush，部分 port 不 ready | driver 继续等待，直到所有 port fire 或 timeout | ready port 出队；未 ready port 留队，下轮重试 | 有意改变，提升非阻塞吞吐 |
| 等待 ready 时发生 redirect/flush | 已 fire port 通过 `fired_mask` 处理，未 fire port 不出队 | 同样通过 `fired_mask` 处理已 fire port | 不破坏原 partial fire 语义 |
| active redirect/flush 已开始 | `issue_blocked_by_global_flush()` 阻止 select/mark | 仍由 `issue_blocked_by_global_flush()` 阻止 select/mark | 不影响 |
| replay 后旧 item 残留 | replay_seq/eligible 检查过滤旧 item | 仍依赖 replay_seq/eligible 检查 | 不影响，但要求 eligible 检查保持严格 |
| ready timeout | 单个 port 长期不 ready 会触发 timeout fatal | 单个 xaction 不再长等，原 timeout 不再覆盖该场景 | 行为改变，需要 no-progress/watchdog 补充 |

### 8.3 timeout 语义变化

非阻塞模式下，`MEMBLOCK_DISPATCH_READY_TIMEOUT` 的作用会变弱，因为 driver 不再在一个 xaction 内长时间等待同一个 port ready。

建议第一版：

```text
保留 active sequence no-progress warning。
不新增复杂 watchdog。
```

后续如果需要严格检测长期不 ready，可以新增：

```text
每个 issue item 记录连续被选中但未 fire 次数。
超过 MEMBLOCK_DISPATCH_ISSUE_NO_FIRE_LIMIT 后 fatal/warning。
```

第一版暂不加入，避免方案过重。

逻辑目的：

```text
明确非阻塞模式对 timeout 的验收口径。
原 wait_ready timeout 只适合“一个 xaction 长时间不结束”的阻塞模型；非阻塞模式下 xaction 会快速结束，所以长期 no-ready 需要由 sequence 级 no-progress 或后续 per-item watchdog 覆盖。
```

文字伪代码：

```text
非阻塞模式 driver:
  每个 issue xaction 只等待一个 driver clocking block。
  不在 wait_dispatch_issue_ready() 中累计 wait_cycles。
  因此单个 port 长期 ready=0 不会触发原 MEMBLOCK_DISPATCH_READY_TIMEOUT。

第一版验收:
  保留已有 active sequence no-progress warning。
  当长期没有任何 item 真实 fire 时:
    由现有 no-progress 机制提示整体停滞。
  不新增 per-item no-fire 计数器。

后续增强方案:
  对每个 uid/target 记录连续 selected 但 fired_mask=0 的次数。
  每次 selected 且未 fire:
    no_fire_count++。
  每次真正 fire、flush 清理、replay 重建或 item 删除:
    no_fire_count 清 0。
  如果 no_fire_count 超过 MEMBLOCK_DISPATCH_ISSUE_NO_FIRE_LIMIT:
    根据配置报 warning 或 fatal。
```

### 8.4 风险总结与处理策略

| 风险 | 原因 | 处理策略 |
|---|---|---|
| flush 边界拍误把旧 item 当作普通 fire | 如果 driver 先采 `valid&&ready`，后检查 flush，可能在 flush 已经开始时设置 `fired_mask` | `drive_dispatch_issue_one_cycle()` 必须在采样 ready 前后各检查一次 `dispatch_flush_in_progress/flush_epoch` |
| 未 ready item 被误删队列 | 非阻塞模式下如果仍用 `7'h7f`，会把所有 selected item 都当作 fire | 非阻塞模式正常路径必须使用真实 `tr.memblock_dispatch_fired_mask` |
| agent driver 跨层依赖 seq 配置 | driver 直接调用 `seq_csr_common` 会让 agent 层依赖 sequence 层 | 用 `memblock_dispatch_nonblocking_issue` xaction 字段传递开关 |
| 原 ready timeout 失效 | 非阻塞模式不再长时间等待单个 xaction | 第一版依赖 active sequence no-progress warning；后续再加 per-item no-fire watchdog |
| 同一 item 反复未 fire 影响公平性 | 未 ready item 留队后可能持续被高优先级选中 | 第一版不加 retry delay；如出现问题，再加 `ready_cycle=1` 或 retry backoff |
| redirect/flush partial fire 语义被改坏 | fired_mask 被普通路径和 abort 路径共用 | 保持 abort 分支只处理 `tr.memblock_dispatch_fired_mask`；active flush 下 mark 使用已有 `mark_issue_fire_already_accepted()` 保护 |

## 9. 与 send_pri / pipe 数随机方案的关系

非阻塞 issue drive 与 send_pri/pipe 数随机是正交关系：

```text
send_pri/global_send_pri：决定本轮从 queue 中优先选谁。
sample_*_pip_num()：决定本轮最多尝试驱动几个 port。
非阻塞 issue drive：决定 selected port 中没 ready 的 item 是否等待。
fired_mask：决定最终哪些 selected item 真正 fire 并从 queue 删除。
```

因此 `sample_*_pip_num()` 的含义必须写清楚：

```text
本轮最多选择并尝试驱动的 port 数，不保证本轮实际 fire 数。
实际 fire 数永远以 fired_mask 为准。
```

例子：

```text
sample_load_pip_num() = 3
本轮选择 load uid10/uid11/uid12，驱动 load port0/1/2。
DUT ready 只有 port0/2 为 1。

fired_mask[0]=1
fired_mask[1]=0
fired_mask[2]=1

uid10/uid12 出队并置 load_dispatched=1。
uid11 留在 load_issue_q，queued_load=1，load_dispatched=0。
下一轮 uid11 可继续参与 select。
```

## 10. 推荐 coding 顺序

1. 在 `plus.sv` 新增 `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN` 并解析命令行。
2. 在 `seq_csr_common.sv` 新增缓存字段和 getter。
3. 在 `lintsissue_agent_agent_xaction.sv` 增加 `memblock_dispatch_nonblocking_issue` 字段，driver 必须通过 xaction 字段获取非阻塞开关，不直接依赖 `seq_csr_common`。
4. 在 `memblock_lintsissue_dispatch_sequence.sv` 构造 xaction 时写入非阻塞模式字段。
5. 在 `lintsissue_agent_agent_driver.sv` 新增 `drive_dispatch_issue_one_cycle()`。
6. 修改 driver `main_phase()`，根据非阻塞开关选择阻塞 wait 或一次 ready 采样。
7. 修改 sequence 正常路径，非阻塞模式下使用真实 `tr.memblock_dispatch_fired_mask` 调 `mark_fired_items()`。
8. 更新 `load_sta_std_issue_flow.md` 和网页 flow 文档。
9. 运行 smoke/compile，重点检查普通 fire、partial fire、redirect/flush、replay 场景。

说明：

```text
本小节是后续实现阶段的 coding 顺序。
本次文档补全任务只更新当前 plan 文档，不修改源码、git 状态或其他 flow 文档。
```

## 11. 验收点

### 11.1 普通非阻塞 partial fire

构造：

```text
本轮选择 3 个 load item。
DUT port0/2 ready=1，port1 ready=0。
```

预期：

```text
fired_mask[0]=1，fired_mask[1]=0，fired_mask[2]=1。
uid0/uid2 从 load_issue_q 删除，load_dispatched=1。
uid1 仍留在 load_issue_q，queued_load=1，load_dispatched=0。
下一轮 uid1 可以重新被选中。
```

### 11.2 redirect/flush partial fire

构造：

```text
等待 ready 或一次 ready 采样期间发生 dispatch_flush_epoch 变化。
部分 port 已 valid&&ready，部分 port 未 ready。
```

预期：

```text
memblock_dispatch_aborted_by_redirect=1。
sequence 只处理 tr.memblock_dispatch_fired_mask 中为 1 的 item。
未 fire item 不出队。
active redirect/flush 后续清理逻辑仍负责阻止旧 epoch 继续 route/select。
```

### 11.3 replay 场景

构造：

```text
某 uid target replay 后重新入 issue queue。
非阻塞模式下第一次尝试未 fire。
```

预期：

```text
未 fire 不分配新的 issue_epoch。
未 fire 不清 replay target。
后续真正 fire 后才 mark_issue_fire()，并通过 replay_seq/issue_epoch 进行后续匹配。
```

### 11.4 timeout/no-progress

构造：

```text
某 port 长期 ready=0。
```

预期：

```text
非阻塞模式下单个 xaction 不触发原 wait_ready timeout。
active sequence no-progress warning 仍能提示整体无进展。
如后续需要强约束，再增加连续未 fire watchdog。
```

验收逻辑目的：

```text
确认 timeout 语义变化是有意行为，而不是漏检。
验收重点不是要求原 wait_ready timeout 继续触发，而是确认非阻塞模式不会卡死在 driver 内，并且整体无进展仍有可观察告警路径。
```

验收文字伪代码：

```text
测试输入:
  打开 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN。
  构造某个 intIssue port 长期 ready=0。
  持续让 sequence 能选择到至少一个目标 item。

执行期望:
  每次 driver 收到 issue xaction:
    drive_dispatch_issue_one_cycle() 只采样一次 ready。
    对 ready=0 的 port 保持 fired_mask=0。
    清掉该 port valid 后返回 sequence。
  driver 不应停留在 wait_dispatch_issue_ready()。
  不应触发旧 MEMBLOCK_DISPATCH_READY_TIMEOUT fatal。

状态期望:
  长期未 fire 的 item 仍留在 issue queue。
  如果所有候选长期都无法 fire:
    active sequence no-progress warning 应能提示整体无进展。

后续 watchdog 验收预留:
  如果未来实现连续未 fire watchdog:
    同一 uid/target 连续 selected 但 fired_mask=0 达到阈值后，应按配置 warning/fatal。
```

## 12. 2026-06-18 实现记录

状态：

```text
已按本 plan 完成源码和说明文档同步。
本 plan 暂保留在 undo 目录，review 文档由后续主 agent 按 diff 和 review 规则生成。
```

本轮源码修改：

```text
mem_ut/ver/ut/memblock/env/plus.sv
  新增 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN，默认 0，并在 reload_from_cmdline() 中解析。

mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
  新增 +MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0，使 runtime cfg 默认项可见。

mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv
  新增 dispatch_issue_nonblocking_en 缓存字段。
  load_from_plus() 从 plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN 加载。
  新增 get_dispatch_issue_nonblocking_en() 统一给 sequence 读取。

mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv
  新增 memblock_dispatch_nonblocking_issue 字段。
  默认值为 0，加入 UVM field 和 psdisplay。
  该字段只表示本次 xaction 是否采用非阻塞 issue drive。

mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv
  main_phase() 根据 req.memblock_dispatch_nonblocking_issue 选择：
    0：继续调用 wait_dispatch_issue_ready()。
    1：调用新增 drive_dispatch_issue_one_cycle()。
  新增 drive_dispatch_issue_one_cycle()：
    清空 fired_mask。
    等待一个 driver clocking block。
    采样 ready 前检查 dispatch_flush_in_progress/flush_epoch。
    调 clear_ready_dispatch_issue_ports() 只把真实 valid&&ready port 置入 fired_mask。
    采样 ready 后再次检查 flush/epoch。
    正常返回前清掉剩余 valid 并 send_pkt() drive idle。

mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv
  构造 xaction 时写入 tr.memblock_dispatch_nonblocking_issue。
  正常返回路径新增 effective_fired_mask：
    nonblocking=1 时使用 tr.memblock_dispatch_fired_mask。
    nonblocking=0 时保留 7'h7f。
  只有 effective_fired_mask 非 0 时才 mark_fired_items() 并置 has_fire。
```

本轮文档同步：

```text
AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md
  更新 issue flow 图、send_issue_cycle()、driver main_phase()、新增 drive_dispatch_issue_one_cycle() 说明、Driver 协作字段和 fire marking 分支优先级。

AI_DOC/web/memblock_dispatch_control_flow_callgraph.md
  更新网页 flow 文档中的 lintsissue driver 路径和 fired_mask 说明。

mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md
  补充 MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN 的参数归类、默认值和 timeout 语义变化。

AI_DOC/analysis/source_sv/dispatch_framework_sv/seq_csr_common.md
  补充 dispatch_issue_nonblocking_en 字段说明。

AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lintsissue_dispatch_sequence.md
  补充非阻塞开关、xaction 字段和未 ready item 保留队列语义。

AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md
  补充 memblock_dispatch_nonblocking_issue、drive_dispatch_issue_one_cycle() 和非阻塞路径 fired_mask 语义。
```

核心实现结论：

```text
fired_mask 仍只由 driver 根据 valid&&ready 置位。
非阻塞正常路径不再用 7'h7f 伪造全部 fire。
未 ready item 不调用 mark_issue_fire()，不会删除 issue queue，不会清 queued，不会置 dispatched。
未 ready item 下一轮继续通过 select_issue_candidates() 重新参与仲裁。
driver 不直接读取 seq_csr_common，模式通过 xaction 字段传递。
```

未验证项/风险：

```text
本轮仅计划执行静态检查，未在本记录生成时声明远端 compile/run 已完成。
非阻塞模式下 MEMBLOCK_DISPATCH_READY_TIMEOUT 不再覆盖单个 port 长期 ready=0 的场景，
第一版仍依赖 active sequence no-progress warning；如后续需要硬约束，应增加 per-item no-fire watchdog。
review 文档尚未生成，后续主 agent 需要按 diff 和 mem_ut_code_review_document_rule.md 生成。
```
