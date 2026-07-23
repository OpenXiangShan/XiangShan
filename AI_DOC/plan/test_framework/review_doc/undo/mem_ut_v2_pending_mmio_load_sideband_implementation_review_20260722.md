# mem_ut V2 pending-MMIO load sideband 实现 Review

| 项目 | 内容 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md` |
| 关联 LSQ owner plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md` |
| Review 日期 | 2026-07-22 |
| 当前结论 | coding和`stale_reason`修复后最终复验已完成；最后一轮独立终审 `FINAL PASS` |
| Plan 状态 | coding、文档同步、VCS compile、专项/相邻回归和归档均完成，Plan位于`plan/do` |

## 1. Review 范围

### 1.1 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `raw` | monitor 在 DUT 采样拍冻结的接口事实，不是已经归一化的状态事件 | `dispatch_raw_ctrl_t` | load port 1 valid 时保存 ROB value、observation epoch与sample provenance |
| `accessor` | 隔离 V2 扁平端口和可选接口能力的采样函数 | `sample_mmio_outputs()`、`sample_sq_deq_ptr()` | V2 SQ pointer accessor 恒返回 invalid |
| `observation epoch` | monitor 观察 MMIO output 时的环境 flush epoch，不表示该脉冲由哪个请求产生 | `dispatch_raw_ctrl_t::mmio_flush_epoch` | 旧 load request 的迟到脉冲可带 redirect 后的新 observation epoch |
| `producer provenance` | MMIO output 所在 DUT sample 的单调序号，用于和 redirect 采样锚点建立来源关系 | `dispatch_raw_ctrl_t::mmio_sample_seq`、`get_dut_sample_seq()` | `loadMmio` sample 为 `R` 或 `R+1` 时进入 overlap 判定 |
| `sample watermark` | ctrl monitor 已真实推进到的最新 DUT sample 序号 | `dut_sample_seq`、`peek_latest_dut_sample_seq()` | directed helper只能等待该值，不得自行递增 |
| `redirect sample anchor` | redirect 输入被 DUT monitor 采到的 sample 序号及完整 payload | `redirect_sample_seq`、`redirect_anchor_history_q` | anchor sample记为`R`，后一拍为`R+1` |
| `anchored record` | 已绑定 redirect sample anchor、但 cancel 生命周期尚未完成的 record | `cancel_record_q[*].redirect_anchor_valid=1` | resolver直接读取record中的redirect和`R` |
| `unbound anchor FIFO` | 已由monitor采到、尚未绑定到未完成cancel record的anchor队列 | `redirect_anchor_history_q` | resolver按FIFO顺序与未绑定record配对验证 |
| `overlap` | LOAD MMIO sample恰好等于某个未完成redirect的`R`或`R+1` | `resolve_mmio_uid_by_rob_value()` | 只有唯一旧owner可证明被覆盖才drop |
| `old owner` | activation epoch早于redirect epoch，且完整ROB key被redirect覆盖的active scalar load实例 | active ROB map与`active_instance_flush_epoch` | 符合全部条件时返回`STALE_DROP` |
| `new owner` | activation epoch等于或晚于redirect epoch的active实例 | active ROB map | overlap只命中新owner必须`MMIO_RESOLVE` fatal |
| `canonical tag` | 每个 UID 动态实例唯一且 load/store 互斥的 MMIO 属性 | `mmio_tag_valid`、`is_mmio_load/store` | 同 UID 不允许同时成为 MMIO load 和 store |
| `active provenance` | active 实例创建时的 flush epoch 来源证明 | `active_instance_flush_epoch` | 判断旧 raw 是否早于 redirect 后的新实例 |
| `dynamic epoch` | 同一 UID redirect/reissue 后的实例版本号 | `dynamic_epoch`、`mmio_tag_dynamic_epoch` | query 拒绝读取旧实例 tag |
| `staging` | 同一 raw 在写 status 前暂存并去重的 tag 集合 | `memblock_mmio_tag_stage_t` | 三个 load port 命中同 UID 时只提交一次 |
| `preflight` | 不修改状态的全量预检阶段 | `set_uid_mmio_tag(..., apply_update=0)` | 任一 kind 冲突时整条 raw 不产生部分写入 |
| `owner` | 对某类生命周期状态拥有唯一写权限的对象 | `common_data_transaction`、singleton `lsq_commit_handler` | adapter 不创建第二个 commit/deq cursor |
| `level sideband` | 连续周期保持当前 ROB head 状态的 DUT 输入 | `pendingPtr/pendingMMIOld/pendingst` | MMIO load 尚未 writeback 时也可保持 grant |
| `singleton` | 全环境只共享一个有状态实例 | `lsq_commit_handler::get()` | directed 与 adapter 共用同一 commit cursor |
| `factory` | UVM 注册、查找并按wrapper创建对象的机制 | `find_wrapper_by_name()`、`create_object_by_type()`、`type_id::create()` | basicTest按`+VSEQ_MAIN`直接创建目标vseq |
| `explicit vseq start` | testcase在`main_phase()`创建并显式启动顶层vseq | `basicTest::main_phase()` | `main_vseq.start(env.vsqr)`同步等待完整生命周期 |
| `testcase objection` | testcase在顶层`start()`前raise、返回后drop的phase保活对象 | `basicTest::main_phase()` | 派生vseq不需要依赖`pre_body/post_body` objection保活 |
| `diagnostic reason` | resolver通过输出参数返回的归属判定原因文本 | `resolve_mmio_uid_by_rob_value()::stale_reason` | expected-fatal被catcher捕获后仍保留完整owner计数 |
| `bind` | 把 consumer 显式连接到已有 owner handle | `bind_lsq_ctrl()`、`bind_commit_handler()` | helper 重入时也重新确认连接 |
| `pre_body` | sequence 正文前的 UVM 生命周期回调 | `memblock_dispatch_base_sequence::pre_body()` | `uvm_do_on` 子 sequence 路径可能不调用它 |
| `idempotent` | 重复调用只补缺失 handle和绑定，不复制对象或推进状态 | `ensure_directed_helpers()` | pre_body 已运行和未运行两种路径结果一致 |
| `collector` | 只搬运 monitor 采样事实、不执行语义对账的采集入口 | `drain_lsq_timing_sidebands()` | 同一 service tick 可前后调用两次 |
| `reconcile` | 把软件 cancel 记录与指定 DUT snapshot 做一次对账 | `service_lsq_timing_reconcile()` | 每个 service tick 只执行一次 |
| `stale raw` | 可证明早于当前 active 实例的旧采样事实 | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | redirect 后旧 MMIO port 只按 port 丢弃 |
| `active idle` | driver 已进入 main phase但当前没有有效 item 的气泡拍 | `drive_active_idle()` | no-item、pre-gap、post-gap 保持 level |
| `configured idle` | reset 或显式空闲模式按 `drv_mode` 产生的接口值 | `drive_idle(cfg.drv_mode)` | reset 可继续使用 X、RAND、LST 等原模式 |
| `cache` | driver 保存最近一次有效 transaction 的三项 level | `cached_pending_*` | 下一 active 气泡重驱同一 pending head |
| `pulse` | 只在发送 transaction 当拍有效、气泡拍必须清零的控制 | `scommit`、`flushSb` | active idle 不重复提交或重复 flush |
| `deferred FIFO` | 已完成semantic转换、等待full-raw owner成功应用的持久队列 | `memblock_sync_pkg::deferred_raw_ctrl_q` | resync mismatch保留队首，MMIO normalization不重复生成semantic event |

本 review 覆盖 raw producer、MMIO tag 生命周期、adapter 归一化、行为矩阵、directed sequence、
package/filelist，以及并行 review 要求的 timing sideband 单次 reconcile 修复。RM、checker、coverage、
MMIO 地址生成和响应正确性不在本轮范围。

### 1.2 修改前逻辑、修改后逻辑与正确性总览

| 功能特性 | 修改前逻辑 | 修改后逻辑 | 正确性检查 |
|---|---|---|---|
| MMIO raw producer | monitor 只读局部 MMIO 变量，raw 不携带 tag事实 | accessor 生成 packed mirror，monitor valid-gated 入队 | MMIO-only 周期也入队，invalid payload 不读取 |
| canonical tag | load/store 两套 valid/source 可同时置位 | 单一 valid、互斥 kind、source 和 dynamic epoch | kind 冲突、旧实例 tag、非法 source 均 fatal |
| value-only resolver | 没有 ROB value 到 active UID 的 MMIO 归一化入口 | 只 probe 两个完整 ROB key，并结合 observation epoch、sample provenance和activation epoch；LOAD额外处理`R/R+1` | 只有唯一旧scalar load owner可证明被redirect覆盖时stale drop；新/无/多/不兼容owner均fatal |
| deferred raw consumer | automatic列表调用void handler，resync warning后会丢raw | 本拍raw转入持久FIFO；先原子落MMIO tag，再把完整raw给singleton，success才pop | deq前保留active map；失败队首和后续raw不丢失 |
| directed 启动 | `body()` 假定继承的 `pre_body()` 已初始化 helper | `body()` 首行调用幂等 task `ensure_directed_helpers()` | pre_body 是否执行都绑定同一 LSQ singleton；helper 不 reset 状态 |
| timing sideband | 两次 collector 调用各执行一次 reconcile | collector 可调用两次，redirect 后独立 reconcile 一次 | 一个 service tick 只有一个对账入口 |
| directed provenance | 未覆盖redirect与迟到`loadMmio`同拍/后一拍关系 | 覆盖normal raw、`R`、`R+1` stale和精确expected-fatal | watermark helper只等待monitor推进；catcher不吞其它fatal |
| 顶层vseq入口 | base wrapper factory override加phase default resource间接启动 | wrapper按名称查找、对象按类型创建、`main_phase()`显式`start(env.vsqr)` | `VSEQ_BODY` start/complete证明目标body实际进入并返回 |
| expected-fatal诊断 | fatal message完整，但`stale_reason`输出仍为空 | 先给`stale_reason`赋同一文本，再以该变量fatal | 不改fatal/tag/pass/fail，只补被catch后的日志原因 |

### 1.3 端到端调用关系

| 调用顺序 | 函数或 task | 当前流程中的功能 | 输出或副作用 |
|---|---|---|---|
| 1 | `sample_mmio_outputs()` | 把 V2 扁平 MMIO port 映射到 packed mirror | 只写 accessor 输出参数 |
| 2 | `mon_data()` | valid-gated 构造 `dispatch_raw_ctrl_t`，仅在MMIO valid时冻结同拍sample seq | 向 `raw_ctrl_q` 入队，不改 status |
| 3 | `collect_ctrl_redirect_events_batch()` | 保留完整 raw，并先提取 memoryViolation semantic event | raw 进入 `deferred_ctrl` |
| 4 | `process_monitor_event_batch()` | 完成 redirect-first semantic 仲裁 | 未在此处消费 MMIO tag或 deq |
| 5 | `apply_deferred_ctrl_updates_batch()` | 本拍raw追加到持久FIFO并按队首success消费 | resync失败保留队首并阻止runtime drain |
| 6 | `apply_raw_ctrl_deq()` | 先调用 MMIO normalization，再转交完整 raw并返回success | 调用 singleton LSQ owner |
| 7 | `apply_raw_ctrl_mmio_tags()` | resolve、LOAD overlap分类、去重、全量preflight后提交tag | 只写 canonical MMIO tag；STORE不套用LOAD overlap规则 |
| 8 | `lsq_commit_handler::apply_raw_ctrl_deq()` | 联合预检并提交 LQ/SQ deq和 `sbIsEmpty` | 推进 LSQ mapping/free count，但不复制 tag owner |
| 9 | `build_lsqcommit_xaction()` | 查询当前 modeled head 的 MMIO load tag | 产生 level `pendingMMIOld`，不等待 writeback |

## 2. Raw Producer 与 Accessor

### 2.1 `sample_mmio_outputs()` / `sample_sq_deq_ptr()`

**抽象功能描述：** ctrl monitor 在采样边界调用这两个函数，把 profile-specific 端口转换为稳定 mirror；
函数不检查 X/Z、不解析 UID、不写 raw queue，也不推进 LSQ 状态。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv`，函数：`sample_mmio_outputs()`。

```systemverilog
function automatic void sample_mmio_outputs(
    output logic [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0] load_valid,
    output logic [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0]
                 load_rob_value,
    output logic store_valid,
    output logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] store_rob_value
);
    load_valid = '0;
    load_rob_value = '0;
    store_valid = mon_cb.io_mem_to_ooo_lsqio_storeMmio;
    store_rob_value = mon_cb.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;
    load_valid[0] = mon_cb.io_mem_to_ooo_lsqio_loadMmio_0;
    load_valid[1] = mon_cb.io_mem_to_ooo_lsqio_loadMmio_1;
    load_valid[2] = mon_cb.io_mem_to_ooo_lsqio_loadMmio_2;
    load_rob_value[0] = mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
    load_rob_value[1] = mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
    load_rob_value[2] = mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
endfunction:sample_mmio_outputs
```

中文伪代码：

1. 本函数把当前 V2 扁平 MMIO output转换为参数化packed mirror，不承担UID解析、队列写入或状态推进。
2. 先把load valid和value数组清零，再复制一个store valid/value；随后按固定V2三个物理load port逐项
   复制valid和ROB value。这样invalid lane的payload保持中性零值。
3. 函数只更新四个output参数；monitor返回后才做valid-gated X/Z检查、sample provenance冻结和raw入队。

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`，函数：`get_dut_sample_seq()`、`peek_latest_dut_sample_seq()`。

**抽象功能描述：** sample accessor把同一仿真时刻的所有monitor调用归并到同一个DUT sample序号，
并向只读consumer发布最新watermark；它不识别MMIO种类，也不等待时钟。

```systemverilog
function longint unsigned get_dut_sample_seq(input longint unsigned sample_time);
    if (!dut_sample_time_valid) begin
        dut_sample_time = sample_time;
        dut_sample_time_valid = 1'b1;
        dut_sample_seq = 1;
    end else if (sample_time < dut_sample_time) begin
        `uvm_fatal("MEMBLOCK_SAMPLE_SEQ",
                   $sformatf("DUT sample time moved backwards: previous=%0d current=%0d",
                             dut_sample_time, sample_time))
    end else if (sample_time != dut_sample_time) begin
        dut_sample_time = sample_time;
        dut_sample_seq++;
    end
    return dut_sample_seq;
endfunction:get_dut_sample_seq

function longint unsigned peek_latest_dut_sample_seq();
    return dut_sample_seq;
endfunction:peek_latest_dut_sample_seq
```

中文伪代码：

1. 这两个函数维护和读取全环境共享的DUT sample watermark，不承担MMIO owner解析。
2. 首次写访问把sample time登记为有效并把序号置1；时间回退立即fatal；时间前进时更新time并递增序号；
   同一仿真时刻的重复访问返回同一序号。只读函数直接返回当前序号，不修改任何同步状态。
3. ctrl monitor在MMIO valid时调用写accessor冻结producer provenance；directed等待helper只能调用只读
   accessor并等待watermark变化，因此sequence不能伪造一个未来sample。

V2 accessor 显式映射三个 load port和一个 store port；elaboration check 要求 load 数量为 3 且
`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0`。SQ accessor 不引用不存在的 pointer 成员，输出始终清零。

### 2.2 `io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()`

**抽象功能描述：** monitor 是 MMIO raw 的唯一 producer，负责 valid-gated payload 采样和 FIFO 入队；
它不访问 active map、不调用 deq owner、不修改 pass/fail/terminal。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`，task：`mon_data()` 的 MMIO raw 段。

```systemverilog
bit any_mmio_valid;

any_mmio_valid = store_mmio_valid === 1'b1;
foreach (load_mmio_valid[port]) begin
    any_mmio_valid |= load_mmio_valid[port] === 1'b1;
end
```

中文伪代码：

1. 本段只计算当前采样是否存在任一MMIO output，不写raw或运行期状态。
2. 先读取store valid，再遍历参数化load valid数组做逻辑或；只有严格等于1的lane才贡献有效事实，X/Z
   不会被误当成valid，后续独立X/Z检查仍负责报错。
3. `any_mmio_valid`随后决定是否创建MMIO-only raw，并控制observation epoch和sample provenance是否写入。

源码位置同上，task：`mon_data()` 的完整 ctrl raw 构造分支。

**抽象功能描述：** 本分支把同拍deq、memoryViolation、SBuffer状态与MMIO事实合并成一个完整ctrl raw；
只要任一MMIO valid就冻结同拍observation epoch和producer provenance，然后按FIFO入队。

```systemverilog
if (io_mem_to_ooo_lqDeq != '0 ||
    io_mem_to_ooo_sqDeq != '0 ||
    io_mem_to_ooo_memoryViolation_valid ||
    memblock_sync_pkg::dispatch_flushsb_waiting_empty ||
    any_mmio_valid) begin
    raw_ctrl = memblock_sync_pkg::make_empty_raw_ctrl();
    raw_ctrl.valid = 1'b1;
    raw_ctrl.lq_deq = io_mem_to_ooo_lqDeq;
    raw_ctrl.sq_deq = io_mem_to_ooo_sqDeq;
    raw_ctrl.lq_deq_ptr_flag = io_mem_to_ooo_lqDeqPtr_flag;
    raw_ctrl.lq_deq_ptr_value = io_mem_to_ooo_lqDeqPtr_value;
    raw_ctrl.sq_deq_ptr_valid = sq_deq_ptr_valid;
    raw_ctrl.sq_deq_ptr_flag = sq_deq_ptr_flag;
    raw_ctrl.sq_deq_ptr_value = sq_deq_ptr_value;
    foreach (load_mmio_valid[port]) begin
        if (load_mmio_valid[port] === 1'b1) begin
            raw_ctrl.load_mmio_valid[port] = 1'b1;
            raw_ctrl.load_mmio_rob_value[port] = load_mmio_rob_value[port];
        end
    end
    if (store_mmio_valid === 1'b1) begin
        raw_ctrl.store_mmio_valid = 1'b1;
        raw_ctrl.store_mmio_rob_value = store_mmio_rob_value;
    end
    if (any_mmio_valid) begin
        raw_ctrl.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        // 中文伪代码：MMIO valid 时把本次 monitor sample 的单调序号
        // 固定到 raw；后续 adapter 不得用消费时刻的序号覆盖它。
        raw_ctrl.mmio_sample_seq = memblock_sync_pkg::get_dut_sample_seq($time);
    end
    raw_ctrl.memory_violation_valid = io_mem_to_ooo_memoryViolation_valid;
    raw_ctrl.memory_violation_rob_valid = io_mem_to_ooo_memoryViolation_valid;
    raw_ctrl.memory_violation_rob_flag = io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
    raw_ctrl.memory_violation_rob_value = io_mem_to_ooo_memoryViolation_bits_robIdx_value;
    raw_ctrl.memory_violation_ftq_flag = io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
    raw_ctrl.memory_violation_ftq_value = io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
    raw_ctrl.memory_violation_ftq_offset = io_mem_to_ooo_memoryViolation_bits_ftqOffset;
    raw_ctrl.memory_violation_is_rvc = io_mem_to_ooo_memoryViolation_bits_isRVC;
    raw_ctrl.memory_violation_level = io_mem_to_ooo_memoryViolation_bits_level;
    raw_ctrl.sb_is_empty = io_mem_to_ooo_sbIsEmpty;
    raw_ctrl.cycle = $time;
    memblock_sync_pkg::push_raw_ctrl(raw_ctrl);
end
```

中文伪代码：

1. 本分支负责构造一个完整ctrl raw，不负责消费、归一化或LSQ状态更新。
2. deq、memoryViolation、flushSb等待或MMIO任一条件成立时，先用empty helper把所有字段清零，再复制
   deq/pointer字段；load payload只在对应valid为1时复制，store payload也只在store valid时复制。
3. 任一MMIO valid时，把当前`dispatch_flush_epoch`保存为observation epoch，并调用sample accessor冻结
   同拍`mmio_sample_seq`；MMIO全invalid时两个字段保持empty默认值，adapter不得在消费拍回填。
4. 最后复制memoryViolation与`sbIsEmpty`，写入采样cycle并调用`push_raw_ctrl()`保持FIFO。push helper只在
   monitor capture开启且raw valid时入队，不修改active map、tag、deq或terminal。

新增 raw 字段均由 `make_empty_raw_ctrl()` 清零。MMIO-only 周期也会入队；payload 仅在对应 valid 为 1
时检查和复制。`mmio_flush_epoch`是observation epoch而不是producer epoch；`mmio_sample_seq`是唯一的
sample provenance。`sq_deq_ptr_valid` 与 payload 分离，V2 非法 pointer 会立即 fatal。

## 3. Canonical Tag 生命周期

### 3.1 `activate_uid()` / `clear_uid_dispatch_result()`

**抽象功能描述：** admission owner 在 active map 建立时记录实例 activation epoch；redirect/reissue
清理 owner 在 `dynamic_epoch` 递增前清除 provenance 和 MMIO tag。target issue epoch 继续独立维护，
不再覆盖 activation provenance。

```systemverilog
status.active_instance_flush_epoch_valid = 1'b1;
status.active_instance_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
```

中文伪代码：

1. 本段由 `activate_uid()` 在 active ROB map建立完成后记录当前动态实例的来源 epoch，用于后续
   value-only raw 归属证明。
2. 先确认 UID 未 terminal、未 active且没有残留 provenance；完成 ROB/LQ/SQ key 冲突检查和 map插入后，
   置 active并保存当前 flush epoch。redirect/reissue 清理时，`clear_uid_dispatch_result()` 先调用
   `clear_uid_mmio_tag()` 清除 tag，再清 provenance，之后外层流程才递增 `dynamic_epoch`。
3. `clear_uid_mmio_tag()` 只清 canonical tag字段；active map删除仍由 retire/redirect owner负责，因此
   tag生命周期不会额外推进 mapping、commit或terminal。

状态字段的写者边界如下：

| 字段 | 设置者 | 清理者 | 读取者 |
|---|---|---|---|
| `active_instance_flush_epoch{_valid}` | `activate_uid()` | `clear_uid_dispatch_result()`、reset | MMIO resolver/query |
| canonical MMIO tag | `set_uid_mmio_tag()` | `clear_uid_mmio_tag()`、reset | MMIO query、LSQ sideband owner |
| target instance epoch | issue fire owner | redirect clear | writeback/feedback attach |

### 3.2 `set_uid_mmio_tag()` / query

**抽象功能描述：** setter 是唯一 tag 写入口，验证 active instance、scalar op kind、source、dynamic epoch
和既有 tag 冲突；query 只读取当前动态实例，不推进 commit/deq/head。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数：`set_uid_mmio_tag()`。

```systemverilog
function void set_uid_mmio_tag(input memblock_uid_t uid,
                               input memblock_mmio_kind_e kind,
                               input memblock_mmio_tag_source_e source,
                               input bit apply_update = 1'b1);
    status_transaction       status;
    memblock_op_behavior_t  behavior;
    memblock_mmio_tag_source_e next_source;

    status = get_status(uid);
    if (kind != MEMBLOCK_MMIO_KIND_LOAD && kind != MEMBLOCK_MMIO_KIND_STORE) begin
        `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d got invalid MMIO kind=%0d", uid, kind))
    end
    if (source != MEMBLOCK_MMIO_TAG_DIRECTED &&
        source != MEMBLOCK_MMIO_TAG_MONITOR) begin
        `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d got invalid MMIO source=%0d", uid, source))
    end
    if (!status.active || status.terminal_done || status.flushed ||
        status.issue_killed || status.redirect_pending) begin
        `uvm_fatal("MMIO_TAG",
                   $sformatf("uid=%0d MMIO tag requires current active instance: active=%0d terminal=%0d flushed=%0d killed=%0d redirect=%0d",
                             uid, status.active, status.terminal_done, status.flushed,
                             status.issue_killed, status.redirect_pending))
    end
    if (!status.active_instance_flush_epoch_valid) begin
        `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d has no activation flush-epoch provenance", uid))
    end

    behavior = memblock_op_behavior_util::derive_op_behavior(get_main_transaction(uid));
    if (kind == MEMBLOCK_MMIO_KIND_LOAD &&
        (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD || !behavior.commit_is_load)) begin
        `uvm_fatal("MMIO_TAG",
                   $sformatf("uid=%0d cannot receive LOAD MMIO tag for behavior=%0d",
                             uid, behavior.kind))
    end
    if (kind == MEMBLOCK_MMIO_KIND_STORE &&
        (behavior.kind != MEMBLOCK_OP_BEHAVIOR_STORE || !behavior.commit_is_store)) begin
        `uvm_fatal("MMIO_TAG",
                   $sformatf("uid=%0d cannot receive STORE MMIO tag for behavior=%0d",
                             uid, behavior.kind))
    end

    next_source = source;
    if (status.mmio_tag_valid) begin
        if (status.mmio_tag_dynamic_epoch != status.dynamic_epoch) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d carries stale MMIO tag epoch=%0d current=%0d",
                                 uid, status.mmio_tag_dynamic_epoch, status.dynamic_epoch))
        end
        if (status.is_mmio_load == status.is_mmio_store) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d canonical tag has invalid load/store bits=%0d/%0d",
                                 uid, status.is_mmio_load, status.is_mmio_store))
        end
        if ((kind == MEMBLOCK_MMIO_KIND_LOAD && !status.is_mmio_load) ||
            (kind == MEMBLOCK_MMIO_KIND_STORE && !status.is_mmio_store)) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d MMIO kind conflict existing load/store=%0d/%0d incoming=%0d",
                                 uid, status.is_mmio_load, status.is_mmio_store, kind))
        end
        if (status.mmio_tag_source != MEMBLOCK_MMIO_TAG_DIRECTED &&
            status.mmio_tag_source != MEMBLOCK_MMIO_TAG_MONITOR) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d existing MMIO source=%0d is invalid",
                                 uid, status.mmio_tag_source))
        end
        if (status.mmio_tag_source == MEMBLOCK_MMIO_TAG_MONITOR ||
            source == MEMBLOCK_MMIO_TAG_MONITOR) begin
            next_source = MEMBLOCK_MMIO_TAG_MONITOR;
        end else begin
            next_source = MEMBLOCK_MMIO_TAG_DIRECTED;
        end
    end

    if (apply_update) begin
        status.mmio_tag_valid = 1'b1;
        status.is_mmio_load = kind == MEMBLOCK_MMIO_KIND_LOAD;
        status.is_mmio_store = kind == MEMBLOCK_MMIO_KIND_STORE;
        status.mmio_tag_source = next_source;
        status.mmio_tag_dynamic_epoch = status.dynamic_epoch;
    end
endfunction:set_uid_mmio_tag
```

中文伪代码：

1. 本函数是canonical MMIO tag唯一写入口，同时支持只预检和真实提交，不负责ROB/LSQ推进。
2. 先读取uid status，检查kind、source、active生命周期和activation provenance；再调用纯行为矩阵确认
   LOAD只能绑定普通scalar load、STORE只能绑定普通scalar store。任何不兼容组合立即`MMIO_TAG` fatal。
3. 已有tag时，依次检查dynamic epoch、load/store互斥位、incoming kind和旧source；同kind重复允许幂等，
   任一来源是真实monitor时把最终source升级为monitor，directed不能把monitor来源降级。
4. `apply_update=0`在全部检查完成后直接返回而不写字段；为1时原子写valid、互斥kind、最终source和当前
   dynamic epoch。adapter先对全部staged uid dry-run，再统一commit，因此一个raw不会半写。

旧 `mmio_load_tag_valid/mmio_store_tag_valid` 和 `mark_uid_mmio_load/store()` 已删除，避免第二 owner。

### 3.3 `resolve_mmio_uid_by_rob_value()`

**抽象功能描述：** resolver 把 value-only ROB 事实归属到唯一 current active UID，或在证据充分时判为
旧 raw。LOAD 会先用同拍 sample provenance 检查未完成 redirect 的 `R/R+1` overlap；STORE 不使用该
特例。函数只读取有界 timing record、两个完整 ROB key、active map和status，不扫描主表、不写tag。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数：`resolve_mmio_uid_by_rob_value()`；以下三段按源码顺序覆盖完整函数。

```systemverilog
function memblock_mmio_resolve_result_e resolve_mmio_uid_by_rob_value(
    input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
    input memblock_mmio_kind_e expected_kind,
    input int unsigned raw_sample_flush_epoch,
    input longint unsigned raw_sample_seq,
    output memblock_uid_t uid,
    output string stale_reason
);
    int unsigned current_candidate_count;
    int unsigned active_candidate_count;
    int unsigned newer_candidate_count;
    bit load_overlap_observed;
    int unsigned overlap_redirect_match_count;
    memblock_redirect_payload_t overlap_redirect;
    int unsigned overlap_redirect_epoch;
    longint unsigned overlap_redirect_sample_seq;
    int unsigned overlap_old_covered_count;
    int unsigned overlap_new_candidate_count;
    int unsigned overlap_uncovered_count;
    int unsigned overlap_incompatible_count;
    memblock_rob_key_t overlap_old_key;

    uid = 0;
    stale_reason = "";
    current_candidate_count = 0;
    active_candidate_count = 0;
    newer_candidate_count = 0;
    load_overlap_observed = 1'b0;
    overlap_redirect_match_count = 0;
    overlap_redirect = '{default:'0};
    overlap_redirect_epoch = 0;
    overlap_redirect_sample_seq = 0;
    overlap_old_covered_count = 0;
    overlap_new_candidate_count = 0;
    overlap_uncovered_count = 0;
    overlap_incompatible_count = 0;
    overlap_old_key = '{default:'0};
    if (expected_kind != MEMBLOCK_MMIO_KIND_LOAD &&
        expected_kind != MEMBLOCK_MMIO_KIND_STORE) begin
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("ROB value=%0d got invalid expected kind=%0d",
                             rob_value, expected_kind))
    end
    if (raw_sample_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("future raw epoch=%0d current=%0d ROB value=%0d",
                             raw_sample_flush_epoch,
                             memblock_sync_pkg::dispatch_flush_epoch,
                             rob_value))
    end
    if (raw_sample_seq == 0) begin
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("ROB value=%0d kind=%0d has no MMIO sample provenance",
                             rob_value, expected_kind))
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    end
    if (raw_sample_seq > memblock_sync_pkg::peek_latest_dut_sample_seq()) begin
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("future MMIO sample sequence=%0d latest=%0d ROB value=%0d",
                             raw_sample_seq,
                             memblock_sync_pkg::peek_latest_dut_sample_seq(),
                             rob_value))
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    end

    if (expected_kind == MEMBLOCK_MMIO_KIND_LOAD) begin
        foreach (cancel_record_q[record_probe_idx]) begin
            if (!cancel_record_q[record_probe_idx].valid ||
                !cancel_record_q[record_probe_idx].redirect_anchor_valid ||
                (raw_sample_seq != cancel_record_q[record_probe_idx].redirect_sample_seq &&
                 raw_sample_seq != cancel_record_q[record_probe_idx].redirect_sample_seq + 1)) begin
                continue;
            end
            load_overlap_observed = 1'b1;
            overlap_redirect_match_count++;
            if (overlap_redirect_match_count > 1) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("LOAD MMIO sample=%0d overlaps multiple unfinished redirect records",
                                     raw_sample_seq))
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end
            overlap_redirect = cancel_record_q[record_probe_idx].redirect;
            overlap_redirect_epoch = cancel_record_q[record_probe_idx].redirect_epoch;
            overlap_redirect_sample_seq = cancel_record_q[record_probe_idx].redirect_sample_seq;
        end

        foreach (redirect_anchor_history_q[anchor_idx]) begin
            int record_idx_for_anchor;
            int unsigned unanchored_seen;

            if (!redirect_anchor_history_q[anchor_idx].valid ||
                redirect_anchor_history_q[anchor_idx].sample_seq == 0 ||
                (raw_sample_seq != redirect_anchor_history_q[anchor_idx].sample_seq &&
                 raw_sample_seq != redirect_anchor_history_q[anchor_idx].sample_seq + 1)) begin
                continue;
            end
            load_overlap_observed = 1'b1;
            overlap_redirect_match_count++;
            if (overlap_redirect_match_count > 1) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("LOAD MMIO sample=%0d overlaps multiple redirect anchors/records",
                                     raw_sample_seq))
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end

            record_idx_for_anchor = -1;
            unanchored_seen = 0;
            foreach (cancel_record_q[record_probe_idx2]) begin
                if (cancel_record_q[record_probe_idx2].valid &&
                    !cancel_record_q[record_probe_idx2].redirect_anchor_valid) begin
                    if (unanchored_seen == anchor_idx) begin
                        record_idx_for_anchor = record_probe_idx2;
                        break;
                    end
                    unanchored_seen++;
                end
            end
            if (record_idx_for_anchor < 0) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("LOAD MMIO sample=%0d has anchor without unfinished redirect record",
                                     raw_sample_seq))
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end
            if (redirect_anchor_history_q[anchor_idx].level !=
                    cancel_record_q[record_idx_for_anchor].redirect.level ||
                redirect_anchor_history_q[anchor_idx].rob_flag !=
                    cancel_record_q[record_idx_for_anchor].redirect.rob_key.flag ||
                redirect_anchor_history_q[anchor_idx].rob_value !=
                    cancel_record_q[record_idx_for_anchor].redirect.rob_key.value) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("LOAD MMIO anchor FIFO mismatch sample=%0d record=%0d",
                                     raw_sample_seq,
                                     cancel_record_q[record_idx_for_anchor].cancel_record_id))
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end
            overlap_redirect = cancel_record_q[record_idx_for_anchor].redirect;
            overlap_redirect_epoch = cancel_record_q[record_idx_for_anchor].redirect_epoch;
            overlap_redirect_sample_seq = redirect_anchor_history_q[anchor_idx].sample_seq;
        end

        if (load_overlap_observed && overlap_redirect_epoch == 0) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("LOAD MMIO sample=%0d overlap has invalid redirect epoch",
                                 raw_sample_seq))
            return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
        end
    end
```

中文伪代码：

1. 本段建立resolver输入合法性和LOAD overlap上下文，不查询主表或写tag。
2. 先清全部输出/计数，拒绝非法kind、future observation epoch、零sample provenance和超过monitor最新
   watermark的未来sample；这些条件均以`MMIO_RESOLVE` fatal结束。
3. 仅对LOAD扫描全部未完成且已绑定anchor的cancel record，寻找`raw_sample_seq=R/R+1`；再扫描尚未
   绑定的anchor FIFO，并按FIFO位置找到对应未绑定record。两个来源合计只能命中一个redirect。
4. anchor无record、多个重叠record、anchor payload与record不一致或redirect epoch无效都fatal。STORE
   完全跳过本段overlap扫描，后续只走普通active provenance解析。

```systemverilog
    for (int unsigned flag_idx = 0; flag_idx < 2; flag_idx++) begin
        memblock_rob_key_t key;
        memblock_uid_t candidate_uid;
        status_transaction status;
        memblock_op_behavior_t behavior;

        key.flag = flag_idx[0];
        key.value = rob_value;
        if (!lookup_active_uid_by_rob(key, candidate_uid)) begin
            continue;
        end
        active_candidate_count++;
        status = get_status(candidate_uid);
        if (!status.active_instance_flush_epoch_valid) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("active uid=%0d ROB=%0d/%0d lacks activation provenance",
                                 candidate_uid, key.flag, key.value))
        end
        if (status.active_instance_flush_epoch >
            memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("uid=%0d activation epoch=%0d is newer than current=%0d",
                                 candidate_uid, status.active_instance_flush_epoch,
                                 memblock_sync_pkg::dispatch_flush_epoch))
        end
        if (load_overlap_observed) begin
            behavior = memblock_op_behavior_util::derive_op_behavior(
                get_main_transaction(candidate_uid));
            if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD ||
                !behavior.commit_is_load || !status.load_dispatched) begin
                overlap_incompatible_count++;
                continue;
            end
            if (rob_order_util::rob_need_flush(key, overlap_redirect) &&
                status.active_instance_flush_epoch < overlap_redirect_epoch) begin
                overlap_old_covered_count++;
                overlap_old_key = key;
            end else if (status.active_instance_flush_epoch >= overlap_redirect_epoch) begin
                overlap_new_candidate_count++;
            end else begin
                overlap_uncovered_count++;
            end
            continue;
        end
        if (raw_sample_flush_epoch < status.active_instance_flush_epoch) begin
            newer_candidate_count++;
            continue;
        end

        behavior = memblock_op_behavior_util::derive_op_behavior(
            get_main_transaction(candidate_uid));
        if (expected_kind == MEMBLOCK_MMIO_KIND_LOAD) begin
            if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD ||
                !behavior.commit_is_load || !status.load_dispatched) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("LOAD raw ROB=%0d/%0d maps to incompatible uid=%0d behavior=%0d load_dispatched=%0d",
                                     key.flag, key.value, candidate_uid,
                                     behavior.kind, status.load_dispatched))
            end
        end else begin
            if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_STORE ||
                !behavior.commit_is_store ||
                !status.sta_dispatched || !status.std_dispatched) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("STORE raw ROB=%0d/%0d maps to incompatible uid=%0d behavior=%0d sta/std_dispatched=%0d/%0d",
                                     key.flag, key.value, candidate_uid,
                                     behavior.kind, status.sta_dispatched,
                                     status.std_dispatched))
            end
        end
        current_candidate_count++;
        uid = candidate_uid;
    end
```

中文伪代码：

1. 本段只probe同一ROB value的两个完整wrap key，并把每个active命中分类，不扫描uid主表。
2. 每个命中先要求activation provenance有效且不晚于当前环境epoch。若存在LOAD overlap，则先用行为
   util确认候选是普通scalar load、`commit_is_load=1`且已dispatch；不兼容候选单独计数。
3. 兼容overlap候选中，完整key被redirect覆盖且activation epoch早于redirect epoch的记为旧owner；同
   epoch或更晚记为新owner；旧epoch但未被覆盖记为uncovered。该分支完成后不进入普通candidate逻辑。
4. 非overlap路径先排除晚于raw observation epoch的新实例；LOAD要求普通load已dispatch，STORE要求
   普通store的STA/STD均已dispatch。兼容候选计入current并保存uid，任一kind/dispatch不符立即fatal。

```systemverilog
    if (load_overlap_observed) begin
        if (active_candidate_count == 1 &&
            overlap_old_covered_count == 1 &&
            overlap_new_candidate_count == 0 &&
            overlap_uncovered_count == 0 &&
            overlap_incompatible_count == 0) begin
            stale_reason = $sformatf(
                "loadMmio sample=%0d overlaps redirect sample=%0d and old active ROB=%0d/%0d is covered",
                raw_sample_seq, overlap_redirect_sample_seq,
                overlap_old_key.flag, overlap_old_key.value);
            return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
        end
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("cannot prove LOAD MMIO stale ownership sample=%0d redirect_sample=%0d active=%0d old_covered=%0d new=%0d uncovered=%0d incompatible=%0d",
                             raw_sample_seq, overlap_redirect_sample_seq,
                             active_candidate_count, overlap_old_covered_count,
                             overlap_new_candidate_count, overlap_uncovered_count,
                             overlap_incompatible_count))
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    end

    if (current_candidate_count > 1) begin
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("ROB value=%0d kind=%0d has multiple current active candidates",
                             rob_value, expected_kind))
    end
    if (current_candidate_count == 1) begin
        return MEMBLOCK_MMIO_RESOLVE_CURRENT;
    end
    if (raw_sample_flush_epoch < memblock_sync_pkg::dispatch_flush_epoch &&
        (active_candidate_count == 0 ||
         newer_candidate_count == active_candidate_count)) begin
        stale_reason = active_candidate_count == 0 ?
            "old raw has no active ROB-key owner" :
            "old raw predates every active ROB-key instance";
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    end

    `uvm_fatal("MMIO_RESOLVE",
               $sformatf("cannot prove MMIO raw ownership ROB value=%0d kind=%0d raw_epoch=%0d current_epoch=%0d active=%0d newer=%0d",
                         rob_value, expected_kind, raw_sample_flush_epoch,
                         memblock_sync_pkg::dispatch_flush_epoch,
                         active_candidate_count, newer_candidate_count))
    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
endfunction:resolve_mmio_uid_by_rob_value
```

中文伪代码：

1. 本段根据前两段的计数给出唯一返回结果，不再访问map或修改status。
2. LOAD overlap只有“active候选恰好1、旧且被覆盖恰好1、新/uncovered/incompatible均0”时写明原因并
   返回`STALE_DROP`。这要求唯一旧scalar load owner和完整key覆盖证据同时成立。
3. overlap的无owner、新owner、多owner、不兼容owner或无法覆盖都以固定ID `MMIO_RESOLVE`和
   `cannot prove LOAD MMIO stale ownership`消息fatal；fatal后的return只满足函数返回类型，不是静默drop。
4. 非overlap时，多个current fatal、唯一current返回`CURRENT`；只有旧observation epoch且无active owner，
   或所有owner都比raw更新时才stale drop。其余无法证明归属的LOAD/STORE统一fatal。

## 4. Adapter 与 LSQ Owner

### 4.1 `apply_raw_ctrl_mmio_tags()`

**抽象功能描述：** deferred raw consumer 在 active map 仍存在时归一化全部 MMIO port，确保同 raw
全成或全不成；它不释放 LSQ mapping，也不推进 modeled ROB head。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数：`apply_raw_ctrl_mmio_tags()`。

```systemverilog
function void apply_raw_ctrl_mmio_tags(
    input memblock_sync_pkg::dispatch_raw_ctrl_t raw
);
    memblock_mmio_tag_stage_t staged_tags[$];
    int unsigned port_order;

    ensure_handles();
    if (raw.load_mmio_valid == '0 && !raw.store_mmio_valid) begin
        return;
    end
    if (raw.mmio_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
        `uvm_fatal("MMIO_RAW",
                   $sformatf("future ctrl raw epoch=%0d current=%0d cycle=%0d",
                             raw.mmio_flush_epoch,
                             memblock_sync_pkg::dispatch_flush_epoch,
                             raw.cycle))
    end

    port_order = 0;
    foreach (raw.load_mmio_valid[port]) begin
        if (raw.load_mmio_valid[port]) begin
            memblock_uid_t uid;
            string stale_reason;
            memblock_mmio_resolve_result_e result;
            int existing_idx;

            result = data.resolve_mmio_uid_by_rob_value(
                raw.load_mmio_rob_value[port],
                MEMBLOCK_MMIO_KIND_LOAD,
                raw.mmio_flush_epoch,
                raw.mmio_sample_seq,
                uid,
                stale_reason);
            if (result == MEMBLOCK_MMIO_RESOLVE_STALE_DROP) begin
                `uvm_info("MMIO_RAW",
                          $sformatf("drop stale loadMmio port=%0d ROB value=%0d epoch=%0d sample=%0d reason=%s",
                                    port, raw.load_mmio_rob_value[port],
                                    raw.mmio_flush_epoch, raw.mmio_sample_seq,
                                    stale_reason),
                          UVM_LOW)
            end else begin
                existing_idx = -1;
                foreach (staged_tags[idx]) begin
                    if (staged_tags[idx].uid == uid) begin
                        existing_idx = idx;
                        break;
                    end
                end
                if (existing_idx >= 0 &&
                    staged_tags[existing_idx].kind != MEMBLOCK_MMIO_KIND_LOAD) begin
                    `uvm_fatal("MMIO_RAW",
                               $sformatf("same raw assigns load/store MMIO kinds to uid=%0d",
                                         uid))
                end
                if (existing_idx < 0) begin
                    memblock_mmio_tag_stage_t stage;

                    stage.uid = uid;
                    stage.kind = MEMBLOCK_MMIO_KIND_LOAD;
                    stage.first_port_order = port_order;
                    staged_tags.push_back(stage);
                end
            end
        end
        port_order++;
    end

    if (raw.store_mmio_valid) begin
        memblock_uid_t uid;
        string stale_reason;
        memblock_mmio_resolve_result_e result;
        int existing_idx;

        result = data.resolve_mmio_uid_by_rob_value(
            raw.store_mmio_rob_value,
            MEMBLOCK_MMIO_KIND_STORE,
            raw.mmio_flush_epoch,
            raw.mmio_sample_seq,
            uid,
            stale_reason);
        if (result == MEMBLOCK_MMIO_RESOLVE_STALE_DROP) begin
            `uvm_info("MMIO_RAW",
                      $sformatf("drop stale storeMmio ROB value=%0d epoch=%0d reason=%s",
                                raw.store_mmio_rob_value,
                                raw.mmio_flush_epoch, stale_reason),
                      UVM_LOW)
        end else begin
            existing_idx = -1;
            foreach (staged_tags[idx]) begin
                if (staged_tags[idx].uid == uid) begin
                    existing_idx = idx;
                    break;
                end
            end
            if (existing_idx >= 0 &&
                staged_tags[existing_idx].kind != MEMBLOCK_MMIO_KIND_STORE) begin
                `uvm_fatal("MMIO_RAW",
                           $sformatf("same raw assigns load/store MMIO kinds to uid=%0d",
                                     uid))
            end
            if (existing_idx < 0) begin
                memblock_mmio_tag_stage_t stage;

                stage.uid = uid;
                stage.kind = MEMBLOCK_MMIO_KIND_STORE;
                stage.first_port_order = port_order;
                staged_tags.push_back(stage);
            end
        end
    end

    foreach (staged_tags[idx]) begin
        data.set_uid_mmio_tag(staged_tags[idx].uid,
                              staged_tags[idx].kind,
                              MEMBLOCK_MMIO_TAG_MONITOR,
                              1'b0);
    end
    foreach (staged_tags[idx]) begin
        data.set_uid_mmio_tag(staged_tags[idx].uid,
                              staged_tags[idx].kind,
                              MEMBLOCK_MMIO_TAG_MONITOR,
                              1'b1);
    end
endfunction:apply_raw_ctrl_mmio_tags
```

中文伪代码：

1. 本函数在deferred ctrl raw的deq之前处理全部MMIO port，不释放mapping或推进head。
2. 先确保公共data和singleton handler句柄可用；raw没有MMIO时直接返回，future observation epoch在首次
   active-map probe前fatal。随后按物理load port顺序调用resolver，并同时传入observation epoch和同拍
   sample provenance；`STALE_DROP`只记录该port，`CURRENT`按uid去重进入staging。
3. load staging发现同uid已有store kind时fatal。store分支再调用同一resolver，但resolver内部不执行LOAD
   `R/R+1` overlap规则；store stale只丢该事实，current store按相同规则去重，load/store冲突fatal。
4. 全部port解析完成后，第一轮setter以`apply_update=0`预检每个unique uid；所有预检返回后第二轮才以
   `apply_update=1`提交monitor tag。因此任何resolver或preflight fatal都不会留下半条raw的部分tag。

同 UID 按首 port 顺序保留一次；同 raw load/store 冲突在 preflight 前 fatal。stale drop 只丢弃对应
port，不丢整条 raw 的 deq、memoryViolation 或 `sbIsEmpty` 事实。LOAD的fatal不能被当成stale返回继续；
源码中的fatal后return只满足SystemVerilog函数控制流，正常UVM执行会被fatal终止或由精确directed catcher捕获。

### 4.2 `apply_raw_ctrl_deq()`

**抽象功能描述：** adapter 先完成 MMIO normalization，再把完整 raw 交给唯一 LSQ owner并返回success；
上层持久FIFO只在success时pop。adapter不拆分count/pointer，也不自行维护free count或pointer。

```systemverilog
function bit apply_raw_ctrl_deq(input memblock_sync_pkg::dispatch_raw_ctrl_t raw);
    ensure_handles();
    apply_raw_ctrl_mmio_tags(raw);
    return monitor_commit_handler.apply_raw_ctrl_deq(raw);
endfunction:apply_raw_ctrl_deq
```

中文伪代码：

1. 本段是 deferred ctrl raw 的统一消费顺序，确保 MMIO tag在任何 deq删除 active map之前完成归一化。
2. adapter 先调用 `apply_raw_ctrl_mmio_tags()` 执行 resolve/preflight/commit；返回后把未拆分的
   `dispatch_raw_ctrl_t` 直接传给 `lsq_commit_handler::apply_raw_ctrl_deq()`，并返回owner success。
3. LSQ handler 使用同一 raw联合预检 LQ/SQ deq并更新 `sbIsEmpty`；adapter 不读取或写入 LSQ pointer、
   free count和terminal，因此 commit/deq/head生命周期仍只有 singleton handler一个 owner。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，
`apply_deferred_ctrl_updates_batch()`。

**抽象功能描述：** 该函数把已经完成memoryViolation semantic投影的本拍raw转入持久FIFO，按队首调用
上述wrapper；它不再次生成semantic event，也不允许后续raw越过失败队首。

```systemverilog
foreach (deferred_ctrl[idx]) begin
    memblock_sync_pkg::push_deferred_raw_ctrl(deferred_ctrl[idx]);
end
deferred_ctrl.delete();
while (memblock_sync_pkg::peek_deferred_raw_ctrl(raw)) begin
    if (!apply_raw_ctrl_deq(raw)) begin
        break;
    end
    if (!memblock_sync_pkg::pop_deferred_raw_ctrl(applied_raw)) begin
        `uvm_fatal("DISP_MON_BATCH", "deferred ctrl apply succeeded but queue pop failed")
    end
end
```

中文伪代码：

1. 先按本拍采集顺序把临时raw全部追加到`deferred_raw_ctrl_q`并清空临时列表，避免task返回时销毁失败项。
2. 查看持久FIFO队首；调用`apply_raw_ctrl_deq()`先完成MMIO normalization，再由singleton owner处理deq。
3. owner返回失败时停止本拍消费并保留队首；返回成功才pop。队列pop内部失败属于一致性错误，直接fatal。
4. 该队列计入`raw_monitor_queue_size()`；只要仍有待重试raw，global stop不会把本plan事实当成已drain。

`ensure_handles()` 使用 `lsq_commit_handler::get()`。这与 handler 已实现的 LQ/SQ 联合 preflight、
V2 count-only SQ deq 和 `sq_deq_ptr_valid` capability 检查一致。

## 5. 行为矩阵与 Directed 场景

### 5.1 `memblock_op_behavior_util`

**抽象功能描述：** util 纯派生 LOAD/PREFETCH/STORE/CBO/AMO 的 allocation、route 和 commit 属性，
不持有公共状态；common data resolver 与 LSQ owner共享同一矩阵。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_op_behavior_util.sv`，class：`memblock_op_behavior_util`。

```systemverilog
class memblock_op_behavior_util;

    static function bit is_vector_ls_futype(
        input bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fuType
    );
        return fuType == MEMBLOCK_FUTYPE_VLDU    ||
               fuType == MEMBLOCK_FUTYPE_VSTU    ||
               fuType == MEMBLOCK_FUTYPE_VSEGLDU ||
               fuType == MEMBLOCK_FUTYPE_VSEGSTU;
    endfunction:is_vector_ls_futype

    static function bit is_load_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_LB  ||
               fuOpType == MEMBLOCK_LSUOP_LH  ||
               fuOpType == MEMBLOCK_LSUOP_LW  ||
               fuOpType == MEMBLOCK_LSUOP_LD  ||
               fuOpType == MEMBLOCK_LSUOP_LBU ||
               fuOpType == MEMBLOCK_LSUOP_LHU ||
               fuOpType == MEMBLOCK_LSUOP_LWU;
    endfunction:is_load_fuoptype

    static function bit is_prefetch_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_PREFETCH_I ||
               fuOpType == MEMBLOCK_LSUOP_PREFETCH_R ||
               fuOpType == MEMBLOCK_LSUOP_PREFETCH_W;
    endfunction:is_prefetch_fuoptype

    static function bit is_store_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_SB ||
               fuOpType == MEMBLOCK_LSUOP_SH ||
               fuOpType == MEMBLOCK_LSUOP_SW ||
               fuOpType == MEMBLOCK_LSUOP_SD;
    endfunction:is_store_fuoptype

    static function bit is_cbo_fuoptype(input bit [8:0] fuOpType);
        bit [3:0] low4;

        low4 = fuOpType[3:0];
        return ((fuOpType[3:2] == 2'b11) && (fuOpType[6:4] == 3'b000)) ||
               (low4 == MEMBLOCK_LSUOP_CBO_ZERO[3:0]);
    endfunction:is_cbo_fuoptype

    static function bit is_amocas_q_fuoptype(input bit [8:0] fuOpType);
        return fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_Q_LO;
    endfunction:is_amocas_q_fuoptype

    static function bit is_amocas_wd_fuoptype(input bit [8:0] fuOpType);
        return fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_W_LO ||
               fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_D_LO;
    endfunction:is_amocas_wd_fuoptype

    static function bit is_amo_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_LR_W      ||
               fuOpType == MEMBLOCK_LSUOP_SC_W      ||
               fuOpType == MEMBLOCK_LSUOP_AMOSWAP_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOADD_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOXOR_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOAND_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOOR_W   ||
               fuOpType == MEMBLOCK_LSUOP_AMOMIN_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAX_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMINU_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAXU_W ||
               fuOpType == MEMBLOCK_LSUOP_LR_D      ||
               fuOpType == MEMBLOCK_LSUOP_SC_D      ||
               fuOpType == MEMBLOCK_LSUOP_AMOSWAP_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOADD_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOXOR_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOAND_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOOR_D   ||
               fuOpType == MEMBLOCK_LSUOP_AMOMIN_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAX_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMINU_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAXU_D ||
               (fuOpType[5:2] == 4'b1011);
    endfunction:is_amo_fuoptype

    static function memblock_op_behavior_t make_default_behavior();
        memblock_op_behavior_t behavior;

        behavior.kind                   = MEMBLOCK_OP_BEHAVIOR_UNKNOWN;
        behavior.need_alloc             = 2'b00;
        behavior.uses_lq                = 1'b0;
        behavior.uses_sq                = 1'b0;
        behavior.route_load             = 1'b0;
        behavior.route_sta              = 1'b0;
        behavior.route_std              = 1'b0;
        behavior.commit_is_load         = 1'b0;
        behavior.commit_is_store        = 1'b0;
        behavior.commit_is_normal       = 1'b1;
        behavior.is_prefetch            = 1'b0;
        behavior.is_cbo                 = 1'b0;
        behavior.is_atomic              = 1'b0;
        behavior.num_ls_elem            = memblock_num_ls_elem_t'(0);
        behavior.atomic_sta_uop_count   = 3'd0;
        behavior.atomic_data_uop_count  = 3'd0;
        return behavior;
    endfunction:make_default_behavior

    static function memblock_op_behavior_t derive_op_behavior(
        input main_control_transaction tr
    );
        memblock_op_behavior_t behavior;

        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "derive_op_behavior got null transaction")
        end
        if (is_vector_ls_futype(tr.fuType)) begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d vector LS is not supported by initial lsq_ctrl_model",
                                 tr.uid))
        end

        behavior = make_default_behavior();
        if (tr.fuType == MEMBLOCK_FUTYPE_LDU) begin
            behavior.need_alloc       = 2'b01;
            behavior.uses_lq          = 1'b1;
            behavior.route_load       = 1'b1;
            behavior.commit_is_load   = 1'b1;
            behavior.commit_is_normal = 1'b0;
            behavior.num_ls_elem      = memblock_num_ls_elem_t'(1);
            if (is_prefetch_fuoptype(tr.fuOpType)) begin
                behavior.kind        = MEMBLOCK_OP_BEHAVIOR_PREFETCH;
                behavior.is_prefetch = 1'b1;
            end else if (is_load_fuoptype(tr.fuOpType)) begin
                behavior.kind = MEMBLOCK_OP_BEHAVIOR_LOAD;
            end else begin
                `uvm_fatal("LSQ_CTRL",
                           $sformatf("uid=%0d has illegal LDU fuOpType=%0d",
                                     tr.uid, tr.fuOpType))
            end
        end else if (tr.fuType == MEMBLOCK_FUTYPE_STU) begin
            behavior.need_alloc       = 2'b10;
            behavior.uses_sq          = 1'b1;
            behavior.route_sta        = 1'b1;
            behavior.route_std        = 1'b1;
            behavior.commit_is_store  = 1'b1;
            behavior.commit_is_normal = 1'b0;
            behavior.num_ls_elem      = memblock_num_ls_elem_t'(1);
            if (is_cbo_fuoptype(tr.fuOpType)) begin
                behavior.kind   = MEMBLOCK_OP_BEHAVIOR_CBO;
                behavior.is_cbo = 1'b1;
            end else if (is_store_fuoptype(tr.fuOpType)) begin
                behavior.kind = MEMBLOCK_OP_BEHAVIOR_STORE;
            end else begin
                `uvm_fatal("LSQ_CTRL",
                           $sformatf("uid=%0d has illegal STU fuOpType=%0d",
                                     tr.uid, tr.fuOpType))
            end
        end else if (tr.fuType == MEMBLOCK_FUTYPE_MOU) begin
            if (!is_amo_fuoptype(tr.fuOpType)) begin
                `uvm_fatal("LSQ_CTRL",
                           $sformatf("uid=%0d has illegal MOU fuOpType=%0d",
                                     tr.uid, tr.fuOpType))
            end
            behavior.kind             = MEMBLOCK_OP_BEHAVIOR_ATOMIC;
            behavior.need_alloc       = 2'b00;
            behavior.route_sta        = 1'b1;
            behavior.route_std        = 1'b1;
            behavior.commit_is_normal = 1'b1;
            behavior.is_atomic        = 1'b1;
            behavior.num_ls_elem      = memblock_num_ls_elem_t'(0);
            if (is_amocas_q_fuoptype(tr.fuOpType)) begin
                behavior.atomic_sta_uop_count  = 3'd2;
                behavior.atomic_data_uop_count = 3'd4;
            end else if (is_amocas_wd_fuoptype(tr.fuOpType)) begin
                behavior.atomic_sta_uop_count  = 3'd1;
                behavior.atomic_data_uop_count = 3'd2;
            end else begin
                behavior.atomic_sta_uop_count  = 3'd1;
                behavior.atomic_data_uop_count = 3'd1;
            end
        end else begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d has unsupported fuType=0x%0h",
                                 tr.uid, tr.fuType))
        end
        return behavior;
    endfunction:derive_op_behavior

endclass:memblock_op_behavior_util
```

中文伪代码：

1. 该class是操作分类唯一真源，只从main transaction字段派生behavior，不读写status、queue、map或driver。
2. 前半部分用纯predicate定义vector LS、普通load、prefetch、普通store、CBO和各AMO编码；默认behavior
   把所有route/allocation/commit分类和uop数量初始化为中性值。
3. `derive_op_behavior()`先拒绝null transaction和本轮不支持的vector LS。LDU分配一个LQ element并只
   route LOAD，普通load置`kind=LOAD`，prefetch保持不同kind；因此MMIO LOAD setter/resolver会拒绝prefetch。
4. STU分配一个SQ element并route STA/STD；普通store置`kind=STORE`，CBO置不同kind，因此MMIO STORE
   setter/resolver不会把CBO静默当普通MMIO store。MOU按AMO编码设置原子route和uop数量，不属于普通
   load/store commit。非法fuType/fuOpType全部fatal，最后返回完整behavior。

`lsq_ctrl_model` 的旧具名 classifier 和 `derive_op_behavior()` 均保留签名，但函数体只转发到 util。
include 顺序固定为 types、main transaction、util、status、common data、LSQ model。

### 5.2 pending-MMIO directed sequence

**抽象功能描述：** software-only sequence 只调用 admission、issue、tag、fault、commit/deq 和 sideband
owner 的公开 API，验证跨 owner 合同与LOAD provenance边界；不直接写 status/head/map/pointer，也不推进
DUT sample watermark。

覆盖场景：

1. directed load/store canonical tag，并检查非 fault load head 的 `pendingMMIOld=1`。
2. normal monitor-like raw 经 adapter 落 tag，再检查 sideband。
3. `mmio_sample_seq=R`和`R+1`分别证明唯一旧load owner被redirect覆盖并stale drop。
4. overlap只命中新owner时精确捕获一次指定`MMIO_RESOLVE` fatal，且不写tag。
5. tagged fault head 抑制 `pendingMMIOld/pendingst/scommit`，真实 fault commit/deq rebase 后下一 load head
   重新发布 `pendingMMIOld=1`。

入口为 `basicTest + ts=memblock_pending_mmio_directed_vseq`；vseq 只要求 virtual sequencer，不启动业务
agent default sequence。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`，class：`memblock_pending_mmio_expected_fatal_catcher`。

**抽象功能描述：** catcher只在一个负向resolver调用窗口内吞掉预期的单个provenance fatal；任何ID或
消息不匹配的fatal继续交给UVM，避免测试把真实回归错误误判成expected failure。

```systemverilog
class memblock_pending_mmio_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;
    string expected_message_pattern;

    function new(
        string name = "memblock_pending_mmio_expected_fatal_catcher",
        string message_pattern = "*cannot prove LOAD MMIO stale ownership*"
    );
        super.new(name);
        caught_count = 0;
        expected_message_pattern = message_pattern;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL &&
            get_id() == "MMIO_RESOLVE" &&
            uvm_pkg::uvm_is_match(expected_message_pattern, get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_pending_mmio_expected_fatal_catcher
```

中文伪代码：

1. 本class为单个expected-fatal场景提供精确report过滤，不改变resolver或status。
2. 构造时把计数清零并保存完整消息匹配模式；`catch()`只在severity为fatal、ID为`MMIO_RESOLVE`且消息
   匹配“cannot prove LOAD MMIO stale ownership”时计数并返回`CAUGHT`。
3. 任何其它fatal返回`THROW`继续走UVM默认报告链；场景结束后还要求计数恰好1且tag未写入。

源码位置同上，task：`wait_for_dut_sample_watermark()`。

**抽象功能描述：** watermark helper只等待ctrl monitor已经真实推进到目标sample；它不调用写accessor、
不使用固定延迟，也不修改sample time或sequence业务状态。

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::wait_for_dut_sample_watermark(
    input longint unsigned target_sample_seq,
    input string scenario_name
);
    if (target_sample_seq == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s requested zero DUT sample watermark", scenario_name))
    end
    while (memblock_sync_pkg::peek_latest_dut_sample_seq() < target_sample_seq) begin
        @(memblock_sync_pkg::dut_sample_seq);
    end
endtask:wait_for_dut_sample_watermark
```

中文伪代码：

1. 本task只等待真实monitor watermark达到目标，不创造sample provenance。
2. 目标为0时立即fatal；否则反复只读`peek_latest_dut_sample_seq()`，未达到时等待共享
   `dut_sample_seq`发生变化，达到后返回。
3. helper不调用`get_dut_sample_seq()`、不使用`#1`，因此directed sequence不能自己推进watermark。

源码位置同上，task：`run_monitor_raw_scenario()`。

**抽象功能描述：** normal raw场景在无redirect overlap时验证value-only ROB fact可以唯一归属到当前
已dispatch load，并通过adapter写入monitor来源tag和LSQ sideband。

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::run_monitor_raw_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;

    reset_directed_owner_state();
    build_load_table(1);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);

    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = data.get_status(0).robIdx_value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    wait_for_dut_sample_watermark(1, "monitor-raw");
    raw.mmio_sample_seq = memblock_sync_pkg::peek_latest_dut_sample_seq();
    raw.cycle = $time;
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    if (!data.uid_is_mmio_load(0) ||
        data.get_status(0).mmio_tag_source != MEMBLOCK_MMIO_TAG_MONITOR) begin
        `uvm_fatal(get_type_name(), "monitor-like raw did not produce canonical load MMIO tag")
    end
    check_load_head_sideband(0, "monitor-raw");
endtask:run_monitor_raw_scenario
```

中文伪代码：

1. 本场景验证非overlap LOAD raw的正常CURRENT路径，不模拟DUT sample生产者。
2. reset后建立并真实fire一个load；构造lane0 valid/value raw，保存当前observation epoch，先等待monitor
   watermark至少为1，再只读该watermark作为raw sample provenance。
3. adapter解析并提交后，要求uid查询为MMIO load且source为monitor；最后调用LSQ owner sideband检查，
   确认`pendingPtr`指向该uid且`pendingMMIOld=1`。

源码位置同上，task：`run_stale_load_overlap_scenario()`。

**抽象功能描述：** stale场景复用同一task分别构造`R`和`R+1`迟到LOAD raw，验证只有唯一旧scalar
load owner被redirect覆盖时按port丢弃且不写tag。

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::run_stale_load_overlap_scenario(
    input bit one_cycle_late
);
    memblock_issue_q_item_t fired_items[$];
    memblock_redirect_payload_t redirect;
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_rob_key_t owner_key;

    reset_directed_owner_state();
    build_load_table(1);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    owner_key = data.get_status(0).get_rob_key();

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    data.request_redirect_flush(redirect);

    anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
    anchor.valid = 1'b1;
    anchor.level = redirect.level;
    anchor.rob_flag = redirect.rob_key.flag;
    anchor.rob_value = redirect.rob_key.value;
    wait_for_dut_sample_watermark(1, "stale-load-overlap-anchor");
    anchor.sample_seq = memblock_sync_pkg::peek_latest_dut_sample_seq();
    anchor.cycle = $time;
    memblock_sync_pkg::push_raw_redirect_anchor(anchor);
    monitor_adapter.drain_lsq_timing_sidebands();

    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = owner_key.value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    raw.mmio_sample_seq = one_cycle_late ? anchor.sample_seq + 1 : anchor.sample_seq;
    wait_for_dut_sample_watermark(raw.mmio_sample_seq,
                                  one_cycle_late ? "stale-load-overlap-R+1" :
                                                   "stale-load-overlap-R");
    raw.cycle = $time;
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    if (data.get_status(0).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("stale loadMmio overlap offset=%0d incorrectly tagged old uid",
                             one_cycle_late))
    end
endtask:run_stale_load_overlap_scenario
```

中文伪代码：

1. 本task用参数选择`R`或`R+1`，其余owner和redirect条件完全相同。
2. 建立并fire唯一scalar load，保存完整ROB key；创建flush-itself redirect覆盖该key并登记cancel record。
   等待真实watermark后构造同payload anchor，通过timing collector把anchor绑定到未完成record。
3. raw只携带ROB value，sample seq按参数取`R`或`R+1`；若取`R+1`，先继续等待monitor真实推进到该值。
   adapter调用resolver后应得到`STALE_DROP`，场景最后要求旧uid未出现MMIO tag。

源码位置同上，task：`run_new_owner_overlap_fatal_scenario()`。

**抽象功能描述：** 负向场景让overlap只能命中redirect epoch内的新实例，验证resolver不能把旧脉冲
静默归给新owner，且只能产生预期的一个provenance fatal。

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::run_new_owner_overlap_fatal_scenario();
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;
    memblock_redirect_payload_t redirect;
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_rob_key_t owner_key;
    memblock_pending_mmio_expected_fatal_catcher catcher;

    reset_directed_owner_state();
    build_load_table(1);
    main_tr = data.get_main_transaction(0);
    behavior = derive_op_behavior(main_tr);
    owner_key = data.get_status(0).get_rob_key();

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    data.request_redirect_flush(redirect);

    anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
    anchor.valid = 1'b1;
    anchor.level = redirect.level;
    anchor.rob_flag = redirect.rob_key.flag;
    anchor.rob_value = redirect.rob_key.value;
    wait_for_dut_sample_watermark(1, "new-owner-overlap-anchor");
    anchor.sample_seq = memblock_sync_pkg::peek_latest_dut_sample_seq();
    anchor.cycle = $time;
    memblock_sync_pkg::push_raw_redirect_anchor(anchor);
    monitor_adapter.drain_lsq_timing_sidebands();

    lsq_ctrl.commit_allocate(0, behavior, main_tr);
    data.set_status_field(0, MEMBLOCK_STATUS_LOAD_DISPATCHED, 1'b1);
    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = owner_key.value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    raw.mmio_sample_seq = anchor.sample_seq;
    raw.cycle = $time;

    catcher = new();
    uvm_report_cb::add(null, catcher);
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || data.get_status(0).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("new-owner overlap expected one fatal/no tag, got fatal=%0d tag=%0d",
                             catcher.caught_count, data.get_status(0).mmio_tag_valid))
    end
endtask:run_new_owner_overlap_fatal_scenario
```

中文伪代码：

1. 本task验证“只能命中新owner”是fatal而不是stale，不承担正常tag流程。
2. 先建立redirect record和真实sample anchor，再在redirect epoch内重新allocate同uid/ROB key并把load标为
   dispatched，使active候选只能分类为new owner；raw sample固定为anchor的`R`。
3. 仅在`apply_raw_ctrl_mmio_tags()`调用期间安装精确catcher，调用返回后立即删除。最后要求指定fatal恰好
   捕获一次且新实例没有tag；ID/消息不匹配的fatal不会被吞掉。

### 5.3 `body()` 的 directed helper 初始化

**功能特性：** directed sequence 在 UVM macro 跳过 inherited `pre_body()` 时仍建立与 base sequence
相同的 helper 合同，但不手工调用生命周期 callback。

**修改前逻辑：** `body()` 直接运行第一个场景；`reset_directed_owner_state()` 首行要求 `data`、
`lsq_ctrl` 和 `monitor_adapter` 非空，因此 `uvm_do_on` 路径可能立即 fatal。

**修改后逻辑：** `body()` 首行调用幂等 task `ensure_directed_helpers()`，等待其中的
`seq_csr_common::init()` 和 handle bind 完成后，再按源码顺序运行八个场景，其中包含normal raw、`R`、
`R+1`和精确expected-fatal。初版误用 function 导致 VCS `SV-DOSIF`，本次改为 task 是编译修正，
不改变 helper 状态语义。

**正确性检查：** helper 不 reset data、LSQ model、commit cursor、queue 或 status；pre_body 已运行时仅重绑
现有 handle，未运行时按 factory/singleton合同补齐，因此不会复制状态机。

源码位置：`soft_test_memblock_pending_mmio_directed_sequence::body()`。

**抽象功能描述：** `body()` 是 software-only directed 总入口，先保证 helper 可用，再依次执行八个互相
reset 的 owner-contract/provenance 场景；它不直接创建 transaction owner、修改status或推进sample watermark。

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::body();
    ensure_directed_helpers();
    run_inactive_head_pointer_scenario();
    run_directed_tag_scenario();
    run_monitor_raw_scenario();
    run_stale_load_overlap_scenario(1'b0);
    run_stale_load_overlap_scenario(1'b1);
    run_new_owner_overlap_fatal_scenario();
    run_fault_head_suppress_scenario();
    run_global_stop_raw_drain_scenario();
    `uvm_info(get_type_name(), "pending-MMIO directed owner-contract checks completed", UVM_LOW)
endtask:body
```

中文伪代码：

1. 本段承担 directed 场景总调度，首先修复可能缺失的 helper handle，不承担任何 owner 状态 reset。
2. `ensure_directed_helpers()` 成功返回后，依次运行inactive head、directed tag、normal raw、`R` stale、
   `R+1` stale、新owner expected-fatal、fault-head suppress和global-stop raw drain；任一非预期fatal都会
   阻止后续场景，全部返回后只打印完成日志。
3. 各`run_*` task通过admission、issue、raw、redirect、writeback、commit/deq owner API推进状态；
   `body()`本身不写status、active map、modeled head、LSQ pointer或DUT sample watermark。

源码位置：`soft_test_memblock_pending_mmio_directed_sequence::ensure_directed_helpers()`。

**抽象功能描述：** 该 helper 消费 inherited helper handles，补齐缺失对象并重建必要 bind；它不调用
`pre_body()`，不启动 sequence，不 reset运行期状态。

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::ensure_directed_helpers();
    seq_csr_common::init();
    data = common_data_transaction::get();
    if (lsq_ctrl == null) begin
        lsq_ctrl = lsq_ctrl_model::get();
    end
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    if (field_assigner == null) begin
        field_assigner = issue_field_assigner::type_id::create("field_assigner");
    end
    if (writeback_handler == null) begin
        writeback_handler = writeback_status_handler::type_id::create("writeback_handler");
    end
    if (monitor_batch_handler == null) begin
        monitor_batch_handler = dispatch_monitor_batch_handler::type_id::create(
            "monitor_batch_handler");
    end
    if (exception_handler == null) begin
        exception_handler = exception_redirect_replay_handler::type_id::create(
            "exception_handler");
    end
    monitor_commit_handler = lsq_commit_handler::get();
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    if (data == null || lsq_ctrl == null || issue_sched == null ||
        field_assigner == null || writeback_handler == null ||
        monitor_batch_handler == null || exception_handler == null ||
        monitor_commit_handler == null || monitor_adapter == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize pending-MMIO directed helpers")
    end
    monitor_batch_handler.bind_writeback_handler(writeback_handler);
    monitor_commit_handler.bind_lsq_ctrl(lsq_ctrl);
    monitor_adapter.bind_commit_handler(monitor_commit_handler);
    commit_handler = monitor_commit_handler;
endtask:ensure_directed_helpers
```

中文伪代码：

1. 本 helper 在 directed 正文开始前建立可用 handle和连接，不承担任何场景状态推进。
2. 先初始化公共参数快照并取得 data singleton；LSQ model为空时取得 singleton。随后逐个检查 issue
   scheduler、field assigner、writeback handler、batch handler、exception handler和 adapter，只有空 handle
   才通过对应 UVM factory创建。commit handler无条件取 `lsq_commit_handler::get()`，确保重入时也回到唯一 owner。
3. 全部 handle建立后统一做 null fail-fast；再把 batch handler绑定到 writeback handler、commit handler绑定
   LSQ model、adapter绑定 commit singleton，最后让 inherited `commit_handler` 指向同一 singleton。各 bind
   只更新对象连接，不 reset cursor、queue、map或 status。

调用关系：

| 调用顺序 | 函数或 task | 在本流程中的功能 |
|---|---|---|
| 1 | `memblock_pending_mmio_directed_vseq::body()` | 通过 `uvm_do_on` 在 virtual sequencer启动 soft sequence。 |
| 2 | directed `body()` | 首行调用 ensure，成功后才进入八个场景。 |
| 3 | `ensure_directed_helpers()` task | 调用 `seq_csr_common::init()`，取得 singleton、按需 factory create并完成三条 bind；返回后 `body()` 才进入场景。 |
| 4 | `reset_directed_owner_state()` | 每个场景开始时使用已确保的 handle执行 owner-defined reset。 |
| 5 | 八个 `run_*_scenario()` | 复用 admission/issue/raw/redirect/fault/commit/deq API验证合同和provenance边界。 |

## 6. LSQ Driver Active Idle Level Hold

### 6.1 `main_phase()` active idle 路由

**功能特性：** active no-item、pre-gap 和 post-gap 不再受 configured `drv_mode` 影响。

**修改前逻辑：** 三类 active 气泡都调用 `drive_idle(cfg.drv_mode)`；只有 `DRV_0` 保持 cache，X、RAND、
LST和全 1 模式可能清除、污染或遗漏 pending level。

**修改后逻辑：** 三类气泡统一调用 `drive_active_idle()`；reset phase继续调用原 `drive_idle()`。

**正确性检查：** active 路径不再进入任何 mode分支；有 item 的拍仍只调用 `send_pkt()`，item_done和 gap
数量均未改变。

源码位置：`lsqcommit_agent_agent_driver::main_phase()`。

**抽象功能描述：** `main_phase()` 在 active driver线程中选择有效 transaction发送或 level-hold气泡；
它不解释 pending head语义，也不修改软件 commit/deq状态。

```systemverilog
repeat(req.pre_pkt_gap) begin
    @this.vif.drv_mp.drv_cb;
    this.drive_active_idle();
end
@this.vif.drv_mp.drv_cb;
this.send_pkt(req);
repeat(req.post_pkt_gap) begin
    @this.vif.drv_mp.drv_cb;
    this.drive_active_idle();
end
seq_item_port.item_done();
```

中文伪代码：

1. 本段负责一个有效 item周围的 active 气泡和发送，不承担 configured/reset idle。
2. 先按 `pre_pkt_gap` 数量逐拍调用 `drive_active_idle()` 保持已有 level；到发送边界调用 `send_pkt()`
   驱动新 transaction并更新 cache；再按 `post_pkt_gap` 数量逐拍保持刚缓存的 level，最后通知 sequencer
   当前 item完成。no-item分支同样在一个 clock边界后调用 `drive_active_idle()`。
3. `send_pkt()` 是 cache唯一正常更新者，`item_done()` 只完成 UVM handshake；这些调用不改变 gap计数、
   sequencer仲裁或软件 LSQ owner状态。

### 6.2 `drive_active_idle()`

源码位置：`lsqcommit_agent_agent_driver::drive_active_idle()`。

**抽象功能描述：** 该 task 在 active 气泡拍重驱最近一次 pending level并清单拍 pulse；它不读取
`cfg.drv_mode`，不更新 cache，也不创建 transaction。

```systemverilog
task lsqcommit_agent_agent_driver::drive_active_idle();
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingMMIOld <=
        cached_sideband_valid ? cached_pending_mmio_ld : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingst <=
        cached_sideband_valid ? cached_pending_st : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <=
        cached_sideband_valid ? cached_pending_ptr_flag : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <=
        cached_sideband_valid ? cached_pending_ptr_value : '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_scommit <= '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= '0;
endtask:drive_active_idle
```

中文伪代码：

1. 本 task承担 active driver气泡的 level-hold，不承担 reset/configured idle模式生成。
2. cache有效时把最近一次 `pendingMMIOld`、`pendingst` 和完整 `pendingPtr` 重驱到 VIF；cache无效时把
   三项 level驱零。无论 cache是否有效，都把 `scommit` 和 `flushSb` 清零，避免把上一有效拍 pulse重复发送。
3. task不调用随机函数、不读取 `drv_mode`、不改 cache；因此 `DRV_X/DRV_RAND/DRV_LST/DRV_1` 只影响
   原 configured idle，不会污染 active pending level。

### 6.3 cache 更新与 reset

源码位置：`lsqcommit_agent_agent_driver::send_pkt()`。

**抽象功能描述：** `send_pkt()` 驱动当前 transaction，并把其中三项 level保存为后续 active idle
的唯一缓存来源；它不缓存 pulse。

```systemverilog
cached_sideband_valid = 1'b1;
cached_pending_ptr_flag = tr.io_ooo_to_mem_lsqio_pendingPtr_flag;
cached_pending_ptr_value = tr.io_ooo_to_mem_lsqio_pendingPtr_value;
cached_pending_st = tr.io_ooo_to_mem_lsqio_pendingst;
cached_pending_mmio_ld = tr.io_ooo_to_mem_lsqio_pendingMMIOld;
```

中文伪代码：

1. 本段在有效 transaction 已写入 VIF 后更新 level cache，不承担下一拍气泡驱动。
2. 先把 cache标记有效，再逐项复制完整 pending pointer、pending store和pending MMIO load；不保存
   `scommit` 或 `flushSb`，所以 pulse不会被 active idle复用。
3. 后续 `drive_active_idle()` 只读这些字段；当前赋值不推进 commit cursor、modeled head或 status。

源码位置：`lsqcommit_agent_agent_driver::reset_phase()`。

**抽象功能描述：** reset phase 使旧 transaction 的 cache立即失效并清 payload，然后继续使用原
configured idle模式驱动接口；它不调用 active idle。

```systemverilog
cached_sideband_valid = 1'b0;
cached_pending_ptr_flag = 1'b0;
cached_pending_ptr_value = '0;
cached_pending_st = 1'b0;
cached_pending_mmio_ld = 1'b0;
```

中文伪代码：

1. 本段在 reset phase开始时清除 active level cache，防止复位前 head在复位后被重新发布。
2. 先清 valid，再清 pointer flag/value、pending store和pending MMIO load payload；随后 reset phase仍按
   原循环调用 `drive_idle(cfg.drv_mode)`，保持 configured idle的既有 X/RAND/LST等行为。
3. cache清理只影响 driver本地字段，不修改公共状态或 LSQ handler；reset结束后的首个 active no-item拍
   因 cache无效而驱动安全零 level。

调用关系：

| 调用顺序 | 调用点 | 功能 |
|---|---|---|
| 1 | `reset_phase()` | 清 cache并继续调用原 `drive_idle(cfg.drv_mode)`。 |
| 2 | `main_phase()` no-item/pre-gap/post-gap | 调用 `drive_active_idle()`，不读取 mode。 |
| 3 | `main_phase()` 有效 item | 调用 `send_pkt()` 驱动 transaction并刷新 level cache。 |
| 4 | 下一 active 气泡 | 读取新 cache保持 level，只清两个 pulse。 |

## 7. Timing Sideband 兼容修复

### 7.1 collector 与 reconcile

**抽象功能描述：** collector 可在同一 service tick 前后各 drain 一次新 snapshot/anchor；reconcile
只在 exception/redirect 处理后执行一次，避免同拍重复推进 record/deadline。

```text
drain collector；
处理 semantic batch 和 exception/redirect；
再次 drain collector；
service_lsq_timing_reconcile 一次。
```

该修改不调用 LSQ release/cancel，也不改变 pass/fail/terminal。

## 8. 验证状态说明

2026-07-23 最终工作区已在独立 mode `v2_lsq_mmio_cbo_final_20260723` 完成真实VCS/KDB compile；最终 KDB
摘要为`0 error(s), 0 warning(s)`，完整 transcript 另有一条工具自身的`LCA_FEATURES_ENABLED` usage
warning。同一编译产物下，
`basicTest + memblock_pending_mmio_directed_vseq + default.cfg`出现目标`VSEQ_BODY` start/complete、normal raw、
`R/R+1` stale、directed completion和精确caught fatal=1，最终`TEST_PASS`且未捕获
`UVM_ERROR/UVM_FATAL`均为0。

相邻默认real smoke和real cancel reconcile也使用该最终产物通过，说明deferred FIFO与singleton owner修复
没有破坏真实head/deq/cancel链。此前未启动directed body的空`TEST_PASS`仍保留为无效历史证据，不用于
当前结论。

## 9. Plan 对齐检查

关联plan为
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md`。
原始plan覆盖raw/accessor/tag/resolver/adapter/util/directed主链，但没有区分observation epoch与producer
provenance；该差异已按执行规则写入plan的`IMPLEMENTATION_DELTA 9.5`，并在下节作为有意实现差异审查。
LSQ owner终审新增的deferred FIFO交接已写入`IMPLEMENTATION_DELTA 9.8`；持久队列与deq success由LSQ
status plan拥有，本plan只说明MMIO normalization的幂等与active-map顺序边界。

## 10. 非本次修改的逻辑分析

共享工作区包含多个并行专项。本review只对pending-MMIO链、其LSQ sideband交接、directed初始化、
active idle修复，以及LOAD provenance读取的cancel timing结构给出结论。其它修改不纳入本review：

| 类别 | 文件或目录 | 判断 | 原因 |
|---|---|---|---|
| 文档搬迁/整理 | `AI_DOC/plan/test_framework/review_doc/{undo,do}`、`AGENTS.md`、`AI_DOC/project_management` | 非本次逻辑 | 属于既有review归档与规则更新。 |
| cancel主体 | `common_data_transaction.sv`、sync package、ctrl/redirect monitor中的非MMIO cancel hunk | 只读依赖，另行review | 本专项只读取未完成record/anchor，不拥有cancel计算、apply或reconcile。 |
| LSQ owner主体 | `lsq_commit_handler.sv`、LSQ enqueue/commit sequence、fault smoke、redirect sequence | 关联依赖，另行review | 属于modeled head、fault convergence、commit/deq和cancel owner主体。 |
| monitor output其它字段 | vec-WB monitor、ctrl xaction及monitor-output review | 另行review | 属于scalar-only gate与其它接口字段适配。 |
| cancel preset | `seq/plus_cfg/tc_dispatch_real_cancel_reconcile_smoke.cfg` | 另行review | pending-MMIO专项没有新增cfg key。 |
| 其它Flow | `lsq_admission_flow.md`、`redirect_flow.md` | 另行review | 描述reservation/cancel主体，不由本轮重写。 |

## 11. 实现与 Plan 不一致项

### 11.1 LOAD producer provenance 是 coding 中新增的有意修正

| 必填项 | 说明 |
|---|---|
| Plan 原有逻辑 | 原始plan只使用`mmio_flush_epoch`与active instance activation epoch判断current/stale。 |
| 当前源码逻辑 | raw新增`mmio_sample_seq`；LOAD按该序号扫描未完成redirect的`R/R+1` overlap，STORE不套用。 |
| 不一致原因 | coding中的RTL provenance review证明`loadMmio`脉冲可能在request被redirect杀掉后仍到达；observation epoch不能证明producer。 |
| 源码位置 | `memblock_sync_pkg.sv`、ctrl monitor、`common_data_transaction::resolve_mmio_uid_by_rob_value()`、pending-MMIO directed sequence。 |
| Plan回写 | 已写入plan `IMPLEMENTATION_DELTA 9.5`，没有伪装成执行前原始方案。 |

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`，字段：`dispatch_raw_ctrl_t::mmio_sample_seq`。

**抽象功能描述：** 该字段保存MMIO valid所在DUT sample的producer provenance，供LOAD resolver与redirect
anchor对齐；它不替代observation epoch，也不直接决定tag。

```systemverilog
int unsigned      mmio_flush_epoch;
// 中文注释：MMIO output 由 LoadQueueUncache 的 s1 后一拍脉冲产生；该序号
// 是 ctrl monitor 采样该脉冲的 DUT sample provenance，不等同于 flush epoch。
// 只有 monitor 在 MMIO valid 时写入，adapter 不得用当前 sample 重新推导。
longint unsigned  mmio_sample_seq;
```

中文伪代码：

1. 这两个相邻字段分别保存observation epoch与producer sample provenance，不承担status写入。
2. `mmio_flush_epoch`记录monitor看到脉冲时环境处于哪个flush epoch；`mmio_sample_seq`记录脉冲所在DUT
   sample。monitor只在MMIO valid时写后者，empty raw把它清0。
3. resolver用epoch处理普通current/stale，用sample seq处理LOAD `R/R+1` overlap；两者不能互相推导。

除该项及已在plan中单列的执行期helper/timing修正外，raw/accessor/tag/adapter/util和LSQ sideband交接
与原始plan目标一致。

## 12. Plan 未说明但实现补充的细节

### 12.1 Directed helper 幂等初始化

| 必填项 | 说明 |
|---|---|
| 细节功能 | `body()` 首行通过 task 确保 inherited helper handle和 bind完整。 |
| 为什么 plan 未覆盖 | 原 plan只规定 vseq启动和 owner边界，未展开 `uvm_do_on` 可能跳过 `pre_body()` 的 UVM macro细节。 |
| 在本特性中的作用 | 消除 directed首场景的 null-handle fatal，不复制状态机。 |
| 源码位置 | `soft_test_memblock_pending_mmio_directed_sequence::body()/ensure_directed_helpers()` |
| 是否需要回写 plan | 已写入 pending-MMIO plan `IMPLEMENTATION_DELTA` 9.3；function-to-task编译修正写入9.4。 |

```systemverilog
task soft_test_memblock_pending_mmio_directed_sequence::body();
    ensure_directed_helpers();
    run_inactive_head_pointer_scenario();
    run_directed_tag_scenario();
    run_monitor_raw_scenario();
    run_stale_load_overlap_scenario(1'b0);
    run_stale_load_overlap_scenario(1'b1);
    run_new_owner_overlap_fatal_scenario();
    run_fault_head_suppress_scenario();
    run_global_stop_raw_drain_scenario();
endtask:body
```

中文伪代码：

1. 本段在 directed正文入口先补齐 helper合同，不手工调用 `pre_body()`。
2. ensure成功后才依次执行八类场景；ensure内部按需创建对象、统一绑定singleton，任何handle仍为空则
   fatal，且fatal发生在场景reset和状态修改之前。
3. 八个场景继续调用既有owner API；本细节只改变启动健壮性，不直接写status或推进sample watermark。

### 12.2 Adapter fallback singleton

| 必填项 | 说明 |
|---|---|
| 细节功能 | adapter缺 commit handle时取得全局 singleton。 |
| 为什么 plan 未覆盖 | plan规定不得复制 owner，但未展开 adapter fallback构造语句。 |
| 在本特性中的作用 | 防止独立 commit cursor、modeled head和 deq pointer。 |
| 源码位置 | `dispatch_monitor_event_adapter::ensure_handles()` |
| 是否需要回写 plan | 已写入 pending-MMIO plan `IMPLEMENTATION_DELTA` 9.2。 |

```systemverilog
if (monitor_commit_handler == null) begin
    monitor_commit_handler = lsq_commit_handler::get();
end
```

中文伪代码：

1. 本分支只在 adapter尚未绑定 LSQ handler时执行 owner fallback。
2. 它调用 `lsq_commit_handler::get()` 取得已有 singleton，不使用 factory创建第二对象；返回后 raw consumer
   将完整 raw交给该 owner。
3. getter只返回或首次建立全局 handler；实际 cursor/deq状态更新仍发生在 handler后续 API中。

### 12.3 Timing collector 与 reconcile 调度分离

| 必填项 | 说明 |
|---|---|
| 细节功能 | 同 tick允许两次 collector drain，但只执行一次 cancel reconcile。 |
| 为什么 plan 未覆盖 | pending-MMIO主 plan不拥有 cancel timing scheduler；并行 review在集成时发现重复 service。 |
| 在本特性中的作用 | 防止同拍两次推进 reconcile record/deadline，同时保留 redirect处理期间的新 sideband。 |
| 源码位置 | `memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` |
| 是否需要回写 plan | 已写入 pending-MMIO plan `IMPLEMENTATION_DELTA` 9.1。 |

```systemverilog
monitor_adapter.drain_lsq_timing_sidebands();
collect_monitor_event_batch();
exception_redirect_replay_task();
monitor_adapter.drain_lsq_timing_sidebands();
monitor_adapter.service_lsq_timing_reconcile();
```

中文伪代码：

1. 本段在一个 service tick内编排 timing sideband采集和唯一对账，不直接修改 LSQ free count。
2. 先收集 tick开始前已有 snapshot/anchor，再处理 semantic batch和 exception/redirect；处理期间可能新到的
   sideband由第二次 collector收集，最后只调用一次 reconcile比较 exact target snapshot。
3. 两次 drain只搬运 raw fact；`service_lsq_timing_reconcile()` 只更新 record观察进度，不执行
   release/cancel或 pass/fail/terminal更新。

### 12.4 `basicTest::main_phase()` 显式启动顶层 vseq

| 必填项 | 说明 |
|---|---|
| 细节功能 | 把`+VSEQ_MAIN`直接解析为已注册wrapper，并在`env.vsqr`上显式创建、类型检查和启动目标vseq。 |
| 为什么 plan 未覆盖 | 原plan沿用base wrapper加name override的phase default间接入口；实际日志证明该入口没有执行任何可观察body。 |
| 在本特性中的作用 | 防止pending-MMIO directed未启动却因零error/fatal被误报为通过。 |
| 源码位置 | `mem_ut/ver/ut/memblock/tc/src/basicTest.sv`的`build_phase()`和`main_phase()`。 |
| 是否需要回写 plan | 已写入pending-MMIO plan `IMPLEMENTATION_DELTA 9.6`。 |

**抽象功能描述：** `basicTest::main_phase()` 是命令行场景到virtual sequencer的唯一启动入口；它建立
factory对象、phase和`p_sequencer`上下文，不拥有具体pending-MMIO场景逻辑。

```systemverilog
created_obj = uvm_factory::get().create_object_by_type(
    main_vseq_wrapper, env.vsqr.get_full_name(), main_vseq_name);
if (created_obj == null) begin
    `uvm_fatal("BASIC_VSEQ_CREATE", ...)
end
if (!$cast(main_vseq, created_obj)) begin
    `uvm_fatal("BASIC_VSEQ_TYPE", ...)
end
phase.raise_objection(this, "starting main virtual sequence");
main_vseq.set_sequencer(env.vsqr);
main_vseq.reseed();
main_vseq.set_starting_phase(phase);
main_vseq.start(env.vsqr);
phase.drop_objection(this, "main virtual sequence completed");
```

中文伪代码：

1. `build_phase()`先按名称查真实wrapper，未注册时在runtime phase前fatal，不再写base wrapper的
   `default_sequence` resource或设置全局base type override。
2. `main_phase()`检查`env.vsqr`，由factory创建对象并cast到`virtual_base_sequence`；任一步失败都fatal，
   不允许静默回退为空base sequence。
3. testcase先持有启动保护objection，再按UVM 1.2 phase default的顺序设置`env.vsqr`、reseed、
   `starting_phase`并randomize；这样randomize阶段也能读取正确`p_sequencer`，随后
   `start(env.vsqr)`执行原`pre_body/body/post_body`合同。
4. sequence对象在body开始前和返回后输出`VSEQ_BODY`信息；completion日志只证明顶层body返回，最终
   directed通过仍需同时检查场景completion、expected-fatal计数和最终report summary。

### 12.5 Deferred FIFO 与 MMIO 幂等重试边界

| 必填项 | 说明 |
|---|---|
| 细节功能 | full raw在LSQ resync mismatch时保留队首，MMIO normalization可能在下一service tick重试。 |
| 为什么原 plan 未覆盖 | 原方案把deferred列表视为一次task内必然成功消费，没有区分warning与success。 |
| 在本特性中的作用 | 避免已经采到的MMIO/deq/SB raw因LSQ owner暂时不匹配而静默消失。 |
| 源码位置 | `memblock_sync_pkg.sv`、`dispatch_monitor_event_adapter::apply_deferred_ctrl_updates_batch()`、`lsq_commit_handler::apply_raw_ctrl_deq()`。 |
| 是否需要回写 plan | 已写入pending-MMIO plan `IMPLEMENTATION_DELTA 9.8`和LSQ status plan对应delta。 |

MMIO setter对同一dynamic instance、同kind的重复monitor fact保持canonical且幂等；若redirect已使raw stale，
resolver按sample provenance丢弃旧port，不能在新实例复活tag。该细节不把FIFO或deq状态所有权移入本plan。

## 13. 验证结果

| 验证项 | 当前结果 |
|---|---|
| 最终目标diff的`git diff --check` | 通过（2026-07-23，终审前再执行全范围检查） |
| `basicTest`显式VSEQ启动入口 | 日志出现目标`VSEQ_BODY` start/complete和directed completion |
| provenance/`stale_reason`修正后VCS compile | 最终 KDB 摘要为`0 error(s), 0 warning(s)`；完整 transcript 含一条工具`LCA_FEATURES_ENABLED` usage warning |
| `memblock_pending_mmio_directed_vseq`仿真 | `TEST_PASS`；normal raw、`R`、`R+1`和新owner expected-fatal均实际执行 |
| 最终severity | `UVM_ERROR=0`、未捕获`UVM_FATAL=0`；caught fatal=1是精确负向检查 |
| 最终日志路径 | `mem_ut/ver/ut/memblock/sim/v2_lsq_mmio_cbo_final_20260723/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl_.log` |

本表只使用provenance和`stale_reason`修正后的最终产物。expected-fatal被catcher精确捕获不等于允许最终
日志出现未捕获`UVM_FATAL`；report summary已确认未捕获fatal为0。

## 14. 最终结论

源码与文档已按producer provenance方案完成对齐：`mmio_flush_epoch`只作为observation epoch，
`mmio_sample_seq`负责LOAD `R/R+1`来源证明；只有唯一旧scalar load owner、已dispatch且完整key被redirect
覆盖时才stale drop，STORE不使用该特例。新owner、无owner、多record、不兼容owner或无法证明覆盖均
fail-fast。

coding、文档同步和最终验证已完成，当前未发现pending-MMIO功能blocker。最后一轮独立review已重新核对
当前工作区、源码、Plan、flow和最终日志并给出`FINAL PASS`；Plan已归档到`plan/do`并进入专项提交。
