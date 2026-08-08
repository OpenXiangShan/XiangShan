# mem_ut V2 普通 Scalar Fault Drain 最小修改 Plan

状态：待 coding  
版本：V2  
范围：测试框架运行期逻辑；real fault smoke 只记录外部验证边界  
不包含：RTL、RM、scoreboard、coverage、fault stimulus、real smoke 配置和完整 ROB exception redirect 建模

## 1. Plan 定位、专有名词与抽象功能说明

### 1.1 专有名词

| 名词 | 当前含义与代码落点 | 最小示例 |
|---|---|---|
| `fault head` | 当前 `commit_cursor_uid` 指向且已经收到异常 writeback 的普通 scalar load/store，落点为 `lsq_commit_handler` 和对应 `status_transaction` | uid 3 的 LDA writeback 带非零 `exceptionVec`，且 uid 3 已到 modeled ROB head |
| `fault token` | `fault_head_waiting/fault_head_uid/fault_head_dynamic_epoch` 组成的等待状态；它不是 normal commit，也不表示 LSQ 已释放 | fault head 发出 `pendingPtr` 后开始等待真实 `lqDeq` |
| `LDA/STA/STD` | int writeback 的 fault target：load address、store address、store data；落点为 writeback event target 和 `load_fault/sta_fault/std_fault` | `STA` exception 只进入本 plan 的普通 scalar STA 分支，`STD` 明确排除 |
| `LDU/STU` | 主表 `fuType` 的 scalar load/store unit 分类；落点为 `main_control_transaction.fuType` | `LDU + 普通 load fuOpType` 才能与 LDA fault 配对 |
| `real deq` | DUT 输出的 `lqDeq/sqDeq` 经 ctrl monitor、raw queue 和 adapter 到达 `apply_raw_ctrl_deq()` 的真实释放事件 | monitor 观察到 `lqDeq=1` 后，现有 helper 释放该 uid 的 LQ mapping |
| `natural drain` | 普通 scalar fault 依靠 DUT 已有 LQ/SQ 完成路径产生真实 deq；测试框架不伪造 deq | 普通 STA fault 保持 head `pendingPtr`，最终等待 DUT `sqDeq` |
| `watchdog` | fault token 建立后独立累计等待周期的超时保护，落点为 `lsq_commit_handler`；它只诊断，不生成 transaction 或释放资源 | 连续超过阈值仍无 deq 时打印 uid、异常类型和 ROB/LQ/SQ key 后 `uvm_fatal` |
| `dynamic instance` | 同一 uid 在 redirect/reissue 前后的不同运行期实例，以 `status.dynamic_epoch` 区分 | redirect 后 epoch 改变，旧 fault token 失效，watchdog 清零并由既有 reissue flow 接管 |
| `owner` | 对某类状态拥有唯一写权限的函数或 flow | LQ/SQ pointer 和 free count 仍只由真实 deq/cancel owner 更新 |
| `fault stimulus` | 让 DUT 产生真实异常 writeback 的输入或 responder 条件，落点在 TLB/PMP 与 responder flow，不由本 plan 生成 | TLB/PMP response 使普通 LDA 的真实 writeback 带出非零 `exceptionVec` |
| `positive smoke` | 不依赖软件伪造 release、能观察真实 DUT raw 的正向 testcase；落点在 real fault smoke | ctrl monitor 观察到 `lqDeq/sqDeq` 后 fault uid 自然收敛 |
| `readiness` | status 已具备进入 candidate 的既有 writeback/fault 前置条件；落点在 candidate helper，不等于完成 | fault head 已可 commit，但仍可能等待 LSQ deq |
| `raw` | monitor 采样后进入现有 FIFO 的 DUT 原始事件，落点为 `dispatch_raw_ctrl_t` 和 raw queue；尚未应用到状态表 | `raw.sq_deq=1` 只是采样事实，随后才由 `apply_raw_ctrl_deq()` 应用 |
| `sideband` | 随 LSQ commit xaction 发送的 level/pulse 辅助字段，落点为 `pendingPtr/pendingst/pendingMMIOld/scommit` | fault head 可保持 `pendingPtr`，但不因此产生 `scommit` |
| `normal commit` | 满足既有 writeback/pass 条件的连续 ROB commit batch，落点为 `select_rob_commit_batch()`/`mark_rob_commit_batch()`；不包含 fault token | uid 2、uid 3 都完成且无异常时组成一个 batch |
| `candidate` | 当前 cursor 可被选择的 uid，落点为 candidate helper；不表示已发送、ROB 已提交或 LSQ 已释放 | `select_fault_head_candidate()` 返回后仍要等 `finish_item()` 才建立 token |
| `commit cursor` | 按 ROB 年龄顺序寻找下一个可处理 uid 的软件游标，落点为 `commit_cursor_uid` | uid 2 未收口时，cursor 不能跳过它直接选择 uid 3 |
| `modeled ROB head` | 软件根据 `commit_cursor_uid` 对应 status ROB key 建立的当前队头，落点为 `modeled_rob_deq_ptr/modeled_head_valid` | fault token 保持该 head 的 `pendingPtr`，不使用 batch tail 推导新 key |
| `terminal` | status 的最终收口状态，落点为 `try_retire_committed_uid()` 和 `terminal_done`；本 plan fault terminal 为 `success=0、terminal_done=1` | fault uid 只有真实 deq 后才进入非成功 terminal |
| `global flush` | 既有 redirect/flush 对当前动态实例的全局阻塞或清理状态，落点为 `data.issue_blocked_by_global_flush()` 和既有 sync owner | blocked 时 driver 发 idle xaction，不将 watchdog 当作错误证据 |
| `reissue` | 旧实例失效后同一 uid 重新建立的实例，落点为既有 redirect/reissue flow | `dynamic_epoch` 改变后旧 token 由 sync 清理，cursor 留给新实例 |
| `producer` | 产生 fault event 的 DUT monitor 来源，落点为 LDA/STA writeback port；本 plan 只分类、不制造事件 | STA writeback monitor 提供 `exceptionVec` |
| `progress` | driver/sequence 的轻量 debug 推进计数，落点为 `has_progress/idle_count`；不代表 fault LSQ 已释放 | `flushSb` busy 可重置 `idle_count`，但不清 fault token |
| `expected-fatal` | 故意阻断已建立 fault token 的外部验证场景，预期由 watchdog 报 fatal；不是正常 testcase 结果 | fault writeback 已到达后阻断 ctrl deq |
| `active mapping` | status 与当前 ROB/LQ/SQ key 的活动关联，落点为 data active map | `lqIdx` 反查到 uid 3；watchdog 只能读取和打印 |
| `report catcher` | UVM `uvm_report_catcher` 的 test-only 过滤对象，落点为既有 software-only sequence；只吞掉明确匹配的 expected fatal | ID 和消息 pattern 都匹配时返回 `CAUGHT`，其它 fatal 返回 `THROW` |

### 1.2 本 plan 涉及函数的抽象职责

- `lsq_commit_handler::uid_is_supported_scalar_fault()`：读取主表操作类型和当前 status fault target，只判断该 fault 是否属于本 plan 支持的普通 scalar LDA 或 STA；不修改状态。
- `lsq_commit_handler::uid_is_fault_terminal_candidate()`：在现有 active、rob_commit、replay、redirect、flushed、issue_killed 和 writeback/fault readiness 门禁基础上，只允许受支持 fault 成为 fault head；epoch 一致性由既有 `sync_modeled_head_after_fault_terminal()` 调用点处理，不负责释放 LSQ。
- `lsq_commit_handler::service_fault_head_drain_watchdog()`：每个 LSQ commit driver 周期检查现有 fault token 是否已经由真实 deq 或既有 redirect 流程收敛；未收敛时计数，超时只报错。
- `memblock_lsqcommit_dispatch_base_sequence::drive_lsqcommit_loop()`：继续负责逐周期驱动 `pendingPtr/pendingst/pendingMMIOld/scommit`，本 plan 只在每轮末尾增加一次 watchdog 调用。
- `memblock_lsq_commit_expected_fatal_catcher`：在 software-only guard 中按 report ID 和消息 pattern 捕获指定 expected fatal；不改变生产 flow，也不吞掉其它报告。

## 2. 目标 Flow 与需要修改的问题

### 2.1 最小目标 Flow

```text
真实 LDA/STA writeback monitor event
  -> writeback_status_handler::handle_real_writeback_event()
  -> status.load_fault/sta_fault
  -> lsq_commit_handler::select_fault_head_candidate()
  -> send_lsqcommit_cycle() 发送当前 pendingPtr，pendingst/pendingMMIOld/scommit 保持 0
  -> mark_fault_rob_commit_uid() 建立 fault token
  -> ctrl monitor raw lqDeq/sqDeq（可在 token 前或后到达）
  -> apply_raw_ctrl_deq() 释放 active mapping
  -> try_retire_committed_uid() 形成 success=0/terminal_done=1
  -> sync_modeled_head_after_fault_terminal() 推进 cursor

若 token 仍存在且没有真实 release：
  -> service_fault_head_drain_watchdog()
  -> 未超时继续等待；超时打印上下文并 uvm_fatal

若期间发生 fault/redirect/reissue overlap：
  -> 保持既有 redirect/cancel owner 处理；本 plan 不新增或证明该场景
```

`send_lsqcommit_cycle()` 负责构造并发送一个 xaction；如果本轮选择 fault head，它在
`finish_item()` 返回后调用 `mark_fault_rob_commit_uid()`，因此 watchdog 不会把“准备发送”
误认为“已经建立 token”。

### 2.2 现有基础与需要修改的问题

现有测试框架已经具备以下完整基础，本 plan 不重复实现：

1. int writeback monitor 将 LDA/STA `exceptionVec` 转成 fault event。
2. `writeback_status_handler` 将 fault 写入 status。
3. 当前 fault head 由 `mark_fault_rob_commit_uid()` 建立 token。
4. token 期间继续保持 modeled head 的 `pendingPtr`，并保持 `pendingst/pendingMMIOld/scommit=0`。
5. ctrl raw 的 `lqDeq/sqDeq` 由 `apply_raw_ctrl_deq()` 释放 mapping。
6. `try_retire_committed_uid()` 在 fault token 和 LSQ release 都具备后形成 `success=0、terminal_done=1`。
7. redirect/cancel 已有独立 owner；本 plan 不修改，也不新增 fault 与 redirect/reissue overlap 语义。

当前需要 coding 的是两个最小运行期改动；real fault stimulus 只作为外部前置条件记录：

| 问题 | 原因 | 最小修改 |
|---|---|---|
| fault candidate 接受范围过宽 | 当前条件包含 `std_fault` 和泛化 fault，可能把未支持指令套入 scalar LQ/SQ drain | 增加普通 scalar LDA/STA 分类门禁 |
| fault token 可无限等待 | 缺少 fault 专用超时，普通 no-progress warning 只能提示，不能指出阻塞 uid | 在现有 handler 增加独立等待计数和 fatal 诊断 |
| 缺少确定性 real fault stimulus | software-only fault smoke 直接调用 release helper，只能检查 ledger；当前 PTE `v=0` 不会自动生成 `tlbPF` | 本 plan 不新增错误的 cfg；fault stimulus 另由 TLB/fault 输入专项负责 |

## 3. 支持边界

本 plan 只支持：

- `numLsElem=1` 的普通 scalar LDU load，fault target 必须是 `LOAD/LDA`。
- `numLsElem=1` 的普通 scalar STU store，fault target 必须是 `STA`。
- fault token 与真实 deq 先后顺序不限；两者齐备后复用现有 terminal 逻辑。

natural-drain 范围限定为“fault writeback 已由 DUT 真实产生，且当前实例不与
redirect/reissue 重叠”的普通 scalar fault。fault writeback 之前的 TLB/PMP/异常 stimulus
不由本 plan 生成；fault 与 redirect/reissue 同时发生的旧实例关联也不由本 plan 扩展。

STA 的真实异常地址选择还依赖
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_is_store_exception_agent_migration_coding_plan_20260727.md`。
LDA 的 watchdog/drain coding 可以独立进行；该 plan 完成前不得执行或宣称通过 STA real smoke。
本 plan 的 `uid_is_supported_scalar_fault()` 只负责 ordinary scalar LDA/STA fault producer
门禁，不调用、不替代其中的 `fault_uid_is_store_exception()`，因为后者的 ROB store 分类还
可能包含 CBO，不能作为本 plan 的支持范围判断。

本 plan 不支持：

- `STD` fault 作为独立 terminal producer。
- software/hardware prefetch fault。
- vector LS、segment LS、AMO/MOU、HLV/HLVX/HSV、CBO 和 early CBO fault。
- 用软件调用 `apply_dut_lq_deq()/apply_dut_sq_deq()` 证明真实 DUT fault drain。
- fault anchor 保留、完整 ROB exception redirect 或新增 cancel 语义。

不支持的 fault 若到达当前 modeled ROB head，必须立即 `uvm_fatal`，不能静默返回后永久卡住。

## 4. 修改一：限定普通 Scalar LDA/STA Fault

### 4.1 新增 `uid_is_supported_scalar_fault()`

**修改原因：** 当前 `uid_is_fault_terminal_candidate()` 仅按 status fault 位判断，无法排除 prefetch、CBO、STD 或未来加入的非 scalar 操作。

**抽象功能描述：** helper 只做主表静态类型与 status fault target 的交叉分类。它不调用可能对 vector/unsupported 类型直接 fatal 的 `derive_op_behavior()`，也不修改 status、queue、map 或 pointer。

**输入：**

- `uid`。
- `data.get_main_transaction(uid)`。
- `data.get_status(uid)`。

**输出/副作用：**

- 返回 1：唯一 fault target 是受支持的普通 scalar LDA 或 STA。
- 返回 0：不是本 plan 支持范围。
- 无状态副作用。
- `uid` 越界或主表/status lookup 失败属于框架内部不变量破坏，必须 `uvm_fatal`，不能当作
  “unsupported 操作”返回 0。

**源码级伪代码：**

```systemverilog
function bit uid_is_supported_scalar_fault(memblock_uid_t uid);
    if (uid >= data.main_trans_num)
        uvm_fatal("fault classification uid is out of range");

    main_tr = data.get_main_transaction(uid);
    status  = data.get_status(uid);

    if (main_tr.numLsElem != memblock_num_ls_elem_t'(1))
        return 0;

    if (status.load_fault && !status.sta_fault && !status.std_fault)
        return main_tr.fuType == MEMBLOCK_FUTYPE_LDU &&
               memblock_op_behavior_util::is_load_fuoptype(main_tr.fuOpType);

    if (status.sta_fault && !status.load_fault && !status.std_fault)
        return main_tr.fuType == MEMBLOCK_FUTYPE_STU &&
               memblock_op_behavior_util::is_store_fuoptype(main_tr.fuOpType);

    return 0;
endfunction
```

**中文文字伪代码：**

1. 先直接检查主表 `numLsElem`，避免 behavior helper 固定派生为 1 而漏掉主表错误。
2. LOAD 分支要求只有 `load_fault` 有效，且主表是 LDU 的普通 load `fuOpType`；prefetch 因不在普通 load 表中返回 0。
3. STA 分支要求只有 `sta_fault` 有效，且主表是 STU 的普通 store `fuOpType`；CBO 因不在普通 store 表中返回 0。
4. `std_fault`、多 target fault、vector、MOU 和其它组合统一返回 0，由 head candidate 入口给出明确 fatal。

### 4.2 修改 `uid_is_fault_terminal_candidate()`

**修改原因：** unsupported fault 如果只返回“不是 candidate”，commit cursor 会停在该 uid，但日志无法说明原因。

**抽象功能描述：** 函数保留源码已有的 active、rob_commit、replay、redirect、flushed、issue_killed 和 writeback/fault readiness 条件，只在确认 uid 已有 fault 后增加支持范围判断。epoch 一致性仍由 `sync_modeled_head_after_fault_terminal()` 处理，不在本函数中虚构新的 epoch 门禁。受支持 fault 返回 1；当前 head 的不支持 fault直接失败。

**源码级伪代码：**

```systemverilog
保留现有 active、rob_commit、replay、redirect、flushed、killed 门禁；
has_fault = status.fault || status.exception_pending ||
            status.load_fault || status.sta_fault || status.std_fault；
若 !has_fault，返回 0；

保留现有 fault readiness：真实 writeback 已落表，或至少一个 target fault 位已落表；
若 readiness 不满足但只有泛化 fault/exception_pending：
    当前 head 以 ID=LSQ_COMMIT_UNSUPPORTED_FAULT 立即 uvm_fatal，非 head 返回 0；

若 uid_is_supported_scalar_fault(uid)，返回 1；

若 uid == commit_cursor_uid：
    uvm_fatal(report_id="LSQ_COMMIT_UNSUPPORTED_FAULT",
              message="unsupported fault head uid=<uid> ...")，打印 uid、fuType、fuOpType、
              numLsElem、load_fault/sta_fault/std_fault 和 exception_vec；

返回 0；
```

**中文文字伪代码：**

该函数先判断“是否存在任何 fault”，再判断原有 writeback/target readiness，最后判断“是否属于
本 plan 支持的 producer”。只有泛化 `fault/exception_pending`、却没有 target fault 或真实
writeback 的状态也属于不完整 unsupported fault：年轻 uid 返回 0，当前 head 立即报错。这样既不
放宽原有 readiness，也不会因旧的提前返回而让 head 静默永久等待。

## 5. 修改二：增加最小 Fault-Head Drain Watchdog

### 5.1 状态与阈值

只在 `lsq_commit_handler` 新增私有等待状态和固定编译期阈值：

```systemverilog
localparam int unsigned FAULT_HEAD_DRAIN_TIMEOUT_CYCLES = 10000;
// 中文注释：fault_head_wait_cycles 只记录已建立 fault token 的等待周期；
// 写者是 token 建立、watchdog service 和 reset；旧实例失效由 watchdog 观察后清零。
// 它不改变 public status、ROB/LSQ/SQ pointer、free count 或 active mapping。
int unsigned fault_head_wait_cycles;
```

该阈值是 fault drain 的诊断保护，不是公共测试参数。不得使用
`MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 或 sequence 的
`no_progress_warn_cycles` 代替，因为后者已有“只输出 warning、允许置 0 关闭”的既有语义；
fault watchdog 必须始终有效，且本 plan 不新增 plusarg、cfg key 或 getter。

生命周期：

- constructor 和 `reset_lsqcommit_runtime_state()` 清零。
- `mark_fault_rob_commit_uid()` 建立新 token 时清零。
- token natural terminal、或既有 sync/redirect owner 已使旧 dynamic instance 失效时，
  watchdog service 清零。若失效只是在本轮 monitor/driver 交错期间被观察到，watchdog 只清计数
  并返回，等待下一轮
  `sync_modeled_head_after_fault_terminal()` 正常清 token。
- token 仍有效且未 terminal 时每个 LSQ commit driver 周期加 1。

### 5.1.1 修改现有 token 生命周期函数

**抽象功能描述：** `reset_lsqcommit_runtime_state()`、constructor 和
`mark_fault_rob_commit_uid()` 只负责维护 watchdog 私有计数与既有 fault token 的生命周期；
不改变 fault token 的建立条件、`rob_commit` 记录或 modeled head 语义。`fault_head_wait_cycles`
的唯一写者是这些初始化/建 token 路径和 watchdog service；token 收敛或旧实例失效后的清零
也统一由 watchdog service 完成。

**源码级伪代码：**

```systemverilog
constructor/reset_lsqcommit_runtime_state():
    // 中文注释必须说明：该计数只由 token 建立、watchdog service 和 reset/失效路径写入，
    // 不影响 public status、ROB/LSQ/SQ pointer、free count 或 active mapping。
    fault_head_wait_cycles = 0;

mark_fault_rob_commit_uid(uid):
    按现有 head、fault candidate 和 modeled ROB key 校验；
    按现有逻辑设置 status.rob_commit、fault_head_waiting、fault_head_uid、fault_head_dynamic_epoch；
    fault_head_wait_cycles = 0;
```

**中文文字伪代码：**

初始化只清 watchdog 的私有计数，不碰公共 status 或 LSQ pointer。fault token 仍由现有
`mark_fault_rob_commit_uid()` 在 xaction 已发送后建立；该函数在记录 token 的同一时刻把计数
归零，确保每个 dynamic instance 都从零开始等待。natural deq 或既有 redirect owner 后续
清 token 时，watchdog 下一次调用会清除残留计数。

### 5.2 新增 `service_fault_head_drain_watchdog()`

**修改原因：** fault head 会阻止更年轻 normal commit；若 DUT、responder 或 monitor 没有给出真实 deq，当前框架只能永久等待。

**抽象功能描述：** 每个 LSQ commit driver 周期调用一次。函数只读取既有 fault token/status 并维护等待计数；terminal status 由原有 `try_retire_committed_uid()` 设置，`sync_modeled_head_after_fault_terminal()` 只同步 token/cursor/modeled head。超时只打印上下文并 fatal，不直接修改 LSQ 状态。

**输入：**

- handler 现有 fault token。
- fault uid 的 status 和 main transaction。
- handler 私有常量 `FAULT_HEAD_DRAIN_TIMEOUT_CYCLES`。

**输出/副作用：**

- 正常等待：`fault_head_wait_cycles++`。
- `data.issue_blocked_by_global_flush()`：保持当前计数，不计入 flush idle 周期。
- natural terminal 或既有 redirect/reissue owner 已清理 token：计数清零。
- 超时：`uvm_fatal`。
- 不创建 xaction，不调用 deq/cancel helper，不直接修改 free count、pointer 或 map。
  terminal 状态由 `try_retire_committed_uid()` 负责；watchdog 不直接调用 terminal/rebase helper，
  只读取既有 token 和 status，维护等待计数并在超时报告。

**源码级伪代码：**

```systemverilog
function void service_fault_head_drain_watchdog();
    if (!fault_head_waiting) begin
        fault_head_wait_cycles = 0;
        return;
    end

    if (data.issue_blocked_by_global_flush()) begin
        // global flush 可能只阻塞年轻实例；既有 token 未失效时保留计数但本拍不递增。
        // 解除阻塞后再由既有 sync/redirect 状态决定继续等待还是清 token。
        return;
    end

    status = data.get_status(fault_head_uid);

    if (status.dynamic_epoch != fault_head_dynamic_epoch ||
        status.flushed || status.issue_killed || !status.rob_commit) begin
        // redirect/flush 或同拍 owner 更新可能先于本轮 sync 被观察到；
        // 只清 watchdog 计数，保留 token 清理和 cursor 处理给既有 sync owner。
        fault_head_wait_cycles = 0;
        return;
    end

    fault_head_wait_cycles++;

    if (fault_head_wait_cycles < FAULT_HEAD_DRAIN_TIMEOUT_CYCLES)
        return;

    main_tr = data.get_main_transaction(fault_head_uid);
    uvm_fatal(report_id="LSQ_COMMIT_FAULT_WATCHDOG",
              message="fault head drain timeout uid=<uid> ..."，uid、LOAD/STA fault、exception_vec、
              ROB/LQ/SQ key、modeled_rob_deq_ptr、active mapping、
              rob_commit、lsq_deq、dynamic_epoch、
              wait_cycles、FAULT_HEAD_DRAIN_TIMEOUT_CYCLES);
endfunction
```

**中文文字伪代码：**

1. 不调用 terminal/rebase helper，直接读取本轮已有 raw/commit owner 更新后的 `fault_head_waiting` 和 status；natural terminal 与 redirect 清理仍由原有调用点完成。
2. 如果 token 已收敛，清计数并返回。
3. 如果 `data.issue_blocked_by_global_flush()` 为真，冻结当前计数，不因 flush 期间的 idle driver 周期增加等待；这覆盖 flush 只影响年轻指令、fault head 仍存活的情况。
4. 如果发现 epoch 不一致、实例已 `flushed/killed` 或 `rob_commit` 已失效，说明旧实例正在由 redirect/flush flow 收口；清零等待计数并返回，不能把交错时序误报为 DUT drain 超时。
5. 只有仍属于同一 dynamic instance 且仍保持 `rob_commit` 的 token 才递增计数；该计数不受 `flushSb` 或其它无关 progress 重置。
6. 达到 handler 私有固定阈值后只输出诊断并以 `LSQ_COMMIT_FAULT_WATCHDOG` report ID fatal。
函数不得补造 `lqDeq/sqDeq`，因此不会掩盖 DUT 或环境缺失的释放事件。

本 plan 不为日志新增“最近 raw”缓存或 provenance 字段；既有 ctrl raw consumer 已记录真实
deq event。watchdog 只打印当前 handler/status/main transaction 中可直接读取的上下文，避免为
诊断引入第二套 raw 生命周期。

### 5.3 修改 `drive_lsqcommit_loop()`

**修改原因：** watchdog 必须有唯一且逐周期稳定的调用点；不能同时放在 monitor service 和多个 deq helper 中重复计数。

**抽象功能描述：** 现有 driver loop 每轮完成一次 LSQ commit xaction 后，再调用一次 watchdog。普通 commit、flushSb 和 stop 条件完全不变。

**源码级伪代码：**

```systemverilog
forever begin
    send_lsqcommit_cycle(...);  // 维持现有 sideband、normal commit 和 fault token 生成
    commit_handler.service_fault_head_drain_watchdog();
    维持现有 idle warning 和 global-stop 判断；
end
```

**中文文字伪代码：**

调用放在 `send_lsqcommit_cycle()` 之后，使本轮刚建立的 token 从该轮开始计时；若真实 deq
已经由 monitor owner 消费，既有 deq helper 会尝试 terminal，watchdog 会观察到
`fault_head_waiting=0` 并清零。该位置即使 global flush 分支发送 idle xaction 也会执行一次，
但 epoch/flush/killed/`!rob_commit` 分支只清计数并等待既有 sync，不把 redirect/reissue overlap
当成 natural-drain positive 覆盖。普通 `idle_count`、`no_progress_warn_cycles` warning 和
global-stop 判断保持原逻辑。

## 6. Fault stimulus 与 smoke 边界

本 plan 不新增 plusarg、cfg、testcase 或 sequence 来制造 fault。原因是当前
`tlb_map_builder` 只把 PTE `v` 写入 `entry.pte_v`，而 L2TLB response 的 `s1_pf` 来自独立的
`entry.tlbPF`；仅设置 `PTE_V=0` 不能确定产生 `load_fault/sta_fault`，更不能据此宣称 real
`lqDeq/sqDeq` 已验证。

执行时应使用已有的、能明确产生真实 LDA/STA writeback 的 fault stimulus；若当前分支没有该
入口，先在独立 TLB/fault stimulus plan 中补齐 `tlbPF/tlbAF/tlbGPF` 的来源链，再回归本 plan。
本 plan 只规定 fault 事件已经进入 monitor 后的 drain 行为，不把 invalid-PTE response
误判为 fault producer。

现有 `soft_test_tc_dispatch_fault_smoke` 保持 software-only ledger 单元测试。它可以验证
candidate/token/terminal 的状态转移，但其中直接调用 `apply_dut_lq_deq()` 或
`apply_dut_sq_deq()` 不属于 real DUT drain 证据。为闭环检查本 plan 新增的两条防御分支，
在该已有 sequence 中增加最小 guard 检查：不新增 testcase、plusarg、cfg 或 real fault stimulus。

### 6.1 既有 software-only smoke 的 guard 检查

**抽象功能描述：** 在已有 fault smoke 的软件状态表准备阶段，用精确 UVM report catcher
验证 unsupported head fatal 和 fault watchdog timeout fatal；每次 expected-fatal 后都完整清理
公共 data/LSQ 状态，再继续下一阶段或原有 synthetic deq ledger。该检查只验证框架防御分支，
不宣称 DUT 产生了真实 fault/deq。

#### 6.1.1 `memblock_lsq_commit_expected_fatal_catcher`

**抽象功能描述：** 该 test-only catcher 在一个 directed guard 期间接收 UVM report，只有
severity 为 `UVM_FATAL`、ID 和消息 pattern 同时匹配时才将报告标为 `CAUGHT` 并计数；其它
fatal、error 或消息不匹配的同 ID 报告都返回 `THROW`，继续暴露真实错误。

**输入/输出/副作用：** constructor 输入 expected ID 和 glob pattern，初始化 `caught_count=0`；
`catch()` 读取当前 UVM report，匹配成功时递增计数并返回 `CAUGHT`，否则返回 `THROW`。它不
修改 `common_data_transaction`、`lsq_ctrl_model` 或 handler 状态。

**源码级伪代码：**

```systemverilog
class memblock_lsq_commit_expected_fatal_catcher extends uvm_report_catcher;
    string expected_id;
    string expected_message_pattern;
    int unsigned caught_count;

    function new(string name,
                 string report_id,
                 string message_pattern);
        super.new(name);
        expected_id = report_id;
        expected_message_pattern = message_pattern;
        caught_count = 0;
    endfunction

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL &&
            get_id() == expected_id &&
            uvm_pkg::uvm_is_match(expected_message_pattern, get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction
endclass
```

#### 6.1.2 guard 状态隔离与调用顺序

**抽象功能描述：** guard 使用现有公共 reset API 在两个 expected-fatal 场景之间建立独立的
data/LSQ/handler 状态；`reset_lsqcommit_runtime_state()` 只清 handler 私有字段，不能单独用于
恢复 guard。所有 catcher 删除后才进入下一阶段。

**文字伪代码：**

```text
guard-1：建立一个普通 scalar 表项并设置 fault 状态为 unsupported STD（或泛化 fault/exception_pending），
使其位于 commit_cursor_uid；安装 report_id=LSQ_COMMIT_UNSUPPORTED_FAULT、pattern="*unsupported fault head*"
的 catcher；调用 select_fault_head_candidate；要求恰好捕获一次 fatal，非目标 fatal 继续抛出。
删除 catcher；调用 data.reset_all_tables(1)、lsq_ctrl.reset() 和
commit_handler.reset_lsqcommit_runtime_state()，清除 guard-1 的 status、active map、pointer 和 token。

guard-2：在全新表/LSQ 状态上建立受支持 scalar LDA fault token，但不调用任何 deq helper；确认
data.issue_blocked_by_global_flush()=0；安装 report_id=LSQ_COMMIT_FAULT_WATCHDOG、
pattern="*fault head drain timeout*" 的 catcher；循环调用 service_fault_head_drain_watchdog()
至 FAULT_HEAD_DRAIN_TIMEOUT_CYCLES；要求恰好捕获一次 fatal。

删除 catcher；再次调用 data.reset_all_tables(2)、lsq_ctrl.reset() 和
commit_handler.reset_lsqcommit_runtime_state()，然后重新执行原 body 的主表建立、issue、writeback
和 direct apply_dut_* deq ledger；最后仍按原 smoke 检查 terminal/status。上述 direct apply 只用于
软件状态单元测试，不作为 positive smoke。
```

catcher 必须在同一时刻只注册一个实例；`uvm_report_cb::add(null, catcher)` 后执行目标调用，
随后无论捕获成功与否都调用 `uvm_report_cb::delete(null, catcher)`。该 guard 检查不改变生产
flow 的 owner、状态字段或阈值。

## 7. 保持不变的逻辑

以下逻辑不得因本 plan 修改：

- ctrl monitor、`dispatch_raw_ctrl_t`、deferred raw FIFO 和 `apply_raw_ctrl_deq()`。
- LQ/SQ pointer、free count、active mapping 和 V2 SQ count-only 处理。
- `pendingPtr/pendingst/pendingMMIOld/scommit` 语义；fault head 仍只保持 `pendingPtr`。
- normal commit batch、`try_retire_committed_uid()` 和 `terminal_done_uid` prefix。
- redirect/cancel record、`lqCancelCnt/sqCancelCnt` 对账和 reissue。
- software-only fault smoke 中用于单元测试的直接 helper 调用。
- RM、scoreboard、coverage 和 DUT RTL。

明确禁止新增：

- `monitor_raw_provenance`、`ctrl_sample_seq`、release provenance 字段。
- fault archive、第二套 cancel record 或 reissue activation 状态机。
- 新 fault plusarg 或 compile 宏。
- watchdog 中任何软件 release 或 synthetic deq。

## 8. 修改文件与验收

### 8.1 Coding 文件

| 文件 | 修改内容 |
|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv` | scalar LDA/STA 分类门禁、私有固定阈值、`fault_head_wait_cycles`、watchdog helper 和 reset；声明处添加 ownership 中文注释 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv` | 每轮唯一调用无参 watchdog；保留既有 no-progress warning 参数及其 warning 语义 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv` | 增加 `memblock_lsq_commit_expected_fatal_catcher` 的 constructor/`catch()` 以及既有 body 的两段 guard；不新增 testcase，不改变 real fault stimulus |
| 无新增 cfg/testcase | fault stimulus 不在本 plan 生成，避免把 invalid PTE 当作确定 fault |

coding 后必须同步检查以下现有文档，使其不再把 fault release 描述成软件伪造 deq，也不把
watchdog 描述成 terminal/deq owner；若源码落点在这些文档中已有对应章节，应在原章节最小更新：

- `AI_DOC/mem_ut_flow_doc/fault_exception_flow.md`
- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/lsq_commit_handler.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lsqcommit_dispatch_sequence.md`

文档同步至少说明：fault token 建立点、真实 `lqDeq/sqDeq` 仍由 ctrl raw owner 应用、watchdog
失效分支只清私有计数并等待既有 sync，以及超时不修改 status/free count/pointer/map。

### 8.2 最小验收

1. 运行现有正常 load/store real smoke，确认 normal commit/deq/terminal 不变。
2. 在已有确定性 fault producer 可用后，分别运行 LDA 和 STA real fault smoke；检查同一 uid 的
   fault event、真实 raw `lqDeq/sqDeq`、最终 `success=0/terminal_done=1`，不要求日志固定先后，
   只要求按 uid/当前 dynamic instance 关联且最终收敛。LDA 可独立执行；STA 必须在
   `isStoreException` 迁移 plan 完成后执行。
3. positive smoke 的 sequence 不得调用 `apply_dut_lq_deq()`/`apply_dut_sq_deq()`；静态检查
   只允许 ctrl monitor raw consumer 推进 mapping。
4. 既有 software-only smoke 必须完成两条 handler guard：unsupported fault head 的精确 fatal，
   以及无 deq token 达到固定阈值后的精确 watchdog fatal；每条 guard 使用参数化 catcher，只
   捕获目标 ID/消息，且在下一条 guard/原 ledger 前调用 `data.reset_all_tables()`、`lsq_ctrl.reset()`
   和 handler reset，避免遗留 `status.rob_commit`、active mapping 或 LSQ pointer。其它 fatal 必须
   继续抛出。该项验证的是框架防御分支，不替代 real fault smoke。
5. real fault expected-fatal/positive smoke 仍需先确认 fault writeback 已到达、
   `mark_fault_rob_commit_uid()` 已建立 `fault_head_waiting`，再阻断 ctrl deq/release；不能在
   token 建立前阻断 responder 并把 startup timeout 当成 watchdog 结果。该外部场景不新增本 plan
   的 fault stimulus。
6. compile 和可执行的非 expected-fatal smoke 要求 `UVM_ERROR=0`、`UVM_FATAL=0`、`TEST CASE PASSED`。

## 与初步 plan 差异说明

初步草稿把本功能扩大为 raw provenance、release sample epoch、fault archive、redirect reissue activation 和四态 raw 分类。该方案会同时修改 monitor、raw typedef、adapter、status、cancel record 和 terminal/release owner，不符合本轮“普通 scalar natural drain + watchdog”的最小目标。

最终 plan 只保留以下差异：

| 修改类型 | 原测试框架逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|
| 功能范围收窄 | 任意 `load_fault/sta_fault/std_fault` 均可成为 fault candidate | 非 LDA/STA producer 没有本 plan 定义的释放闭环 | 只允许 ordinary scalar LDA/STA；unsupported fault 到 head 时 fatal |
| 新增诊断功能 | fault token 无专用 timeout，只依赖通用 warning/UVM timeout | 缺 deq 时更年轻请求永久阻塞且难定位；global flush 期间不能误计时 | 单 owner watchdog 独立计数；global flush 冻结计数；正常等待超时打印 uid、fault 和 key 后 fatal |
| smoke 边界收窄 | 只有 software-only synthetic fault smoke，且 PTE `v=0` 被误认为 fault stimulus | 当前 TLB builder 未把 `pte_v=0` 转成 `tlbPF` | 不在本 plan 添加伪确定 cfg；真实 fault producer 由独立 stimulus plan 提供 |
| 新增防御分支自检 | 既有 software-only smoke 只验证 synthetic token/terminal，未验证 unsupported/head 和 watchdog fatal | 新增 fatal 若没有精确 catcher 可能回归时漏检或吞掉其它 fatal | 在既有 fault smoke body 中复用 UVM catcher，各捕获一次目标 ID；不新增 testcase 或生产 owner |
| 保持原逻辑 | real deq、pointer/free count、terminal、redirect/cancel 已有唯一 owner | 重建 owner 会增加状态一致性风险；overlap 仍是本 plan 外部边界 | 全部复用，不新增 provenance、archive 或第二套状态机 |

修改前的关键逻辑：

```text
status 有任意 fault
  -> 可能成为 fault head
  -> 建立 token
  -> 等待真实 deq
  -> 无专用超时
```

修改后的关键逻辑：

```text
status 有 fault
  -> 主表和 target 必须匹配 ordinary scalar LDA/STA
  -> 建立既有 token并保持 pendingPtr
  -> 既有 raw owner消费真实 lqDeq/sqDeq
  -> `try_retire_committed_uid()` 形成 success=0/terminal_done=1
  -> 若同一 dynamic instance等待超时，只诊断并fatal，不伪造释放
```

`drive_lsqcommit_loop()` 的修改前后文字伪代码如下：

修改前：每轮调用 `send_lsqcommit_cycle()` 驱动 xaction；该 task 内部根据当前 head 构造
`pendingPtr` 和 normal/fault 选择，完成 driver item 后更新 commit/token；随后只更新通用
`idle_count`，达到阈值时 warning，最后检查 global stop。

修改后：每轮仍先调用同一个 `send_lsqcommit_cycle()`，因此 xaction 生成、driver 发送和
`mark_fault_rob_commit_uid()` 顺序不变；item 完成后增加一次
`service_fault_head_drain_watchdog()`，该 helper 只读取原有 terminal/raw owner 已更新的状态，
决定清零、继续计数或 fatal；最后继续执行原有 idle warning 和 stop
判断。watchdog 不替代 `send_lsqcommit_cycle()`，也不改变其返回的 progress 语义。

### 新增/修改 helper 的差异明细

| 函数 | 修改前 | 修改后、输入输出和副作用 |
|---|---|---|
| `uid_is_supported_scalar_fault()` | 无独立 producer 门禁，fault candidate 只看 status fault 位 | 新增；输入 uid、主表 transaction 和 status，输出 0/1；只接受 `numLsElem=1` 的 LDU 普通 load 或 STU 普通 store STA，零副作用，不调用 `derive_op_behavior()` 处理 unsupported 类型。 |
| `uid_is_fault_terminal_candidate()` | ready fault 可能包含 STD/泛化组合 | 保留源码已有 active、rob_commit、replay、redirect、flushed、issue_killed 和 writeback/fault readiness 条件，再调用上述纯分类 helper；当前 cursor 遇 unsupported fault fatal，非 head 返回 0；不改 map/pointer。 |
| `service_fault_head_drain_watchdog()` | 无 fault 专用等待计数 | 新增无参 helper；读取现有 `fault_head_waiting/fault_head_uid/fault_head_dynamic_epoch` 和 status，更新私有计数或 fatal；不调用 terminal/rebase/deq/cancel，不写 free count/map。terminal 仍由既有 `try_retire_committed_uid()` 和 raw owner 完成。 |
| `drive_lsqcommit_loop()` | 每轮只发送 xaction并维护通用 `idle_count` | 在 `send_lsqcommit_cycle()` 返回后唯一调用 watchdog；输入仍是现有 sequence 状态，输出/sideband/progress/stop 语义不变，只增加 fault wait 诊断。 |
| `memblock_lsq_commit_expected_fatal_catcher::new/catch()` | 无 LSQ fault 专项 catcher | 新增 test-only 参数化 ID/pattern 过滤；匹配的目标 fatal 返回 `CAUGHT` 并计数，其它报告返回 `THROW`，不写公共状态。 |
| 既有 `soft_test_memblock_dispatch_fault_smoke_sequence::body()` | 只执行 synthetic fault、直接 deq ledger 和 terminal 检查 | 在原 ledger 前加入精确 unsupported/watchdog expected-fatal catcher；每次 guard 后完整 reset data/LSQ/handler 再继续原流程，提供防御分支闭环证据但不把 synthetic deq 当 real DUT 证据，不改变 production flow |

上述 helper 均复用现有 raw queue、deq owner、status 生命周期和 redirect 边界；本次不新增 monitor 字段、archive、参数或第二个 owner。

### 修改前后详细文字伪代码

以下伪代码用于 coding 和 review 对照；它明确列出旧逻辑、变化原因以及新逻辑的状态副作用。

#### `uid_is_supported_scalar_fault()`

抽象功能描述：从主表 transaction 和 status fault target 计算“是否为本专项支持的普通 scalar
producer”，只返回分类结果，不建立 token 或修改任何公共状态。

修改原因与差异影响：旧逻辑没有独立 producer gate，unsupported fault 可能进入 scalar drain；
新增 gate 后只缩小 candidate 范围，不改变正常 commit/deq owner。

修改前：没有独立的 producer 分类 helper。`uid_is_fault_terminal_candidate()` 看到
`load_fault/sta_fault/std_fault` 或泛化 `fault/exception_pending` 即可返回真，所以普通 LDA/STA、
STD、CBO、prefetch 或未来非 scalar 类型可能共用同一 fault candidate 入口。

修改后：

```text
输入 uid；读取主表 transaction 和该 uid 的 status。
若 numLsElem != 1，返回 false。
若仅 load_fault 有效：要求 fuType=LDU 且 fuOpType 在普通 load 表，满足则返回 true。
若仅 sta_fault 有效：要求 fuType=STU 且 fuOpType 在普通 store 表，满足则返回 true。
若 std_fault、多 fault target、vector、prefetch、CBO、MOU 或其它组合出现，返回 false。
整个过程只返回分类结果，不写 status、mapping、pointer 或 free count。
```

#### `uid_is_fault_terminal_candidate()`

抽象功能描述：在既有 recovery 和 readiness 门禁之后，把当前 uid 分为可处理 fault、年轻
unsupported fault 或当前 head 的非法 fault；它只返回 candidate 或报告非法 head，不释放 LSQ。

修改原因与差异影响：旧逻辑把泛化 fault 直接视为 candidate；新增分类和 head fatal 后，非法
fault 不再静默阻塞 cursor，正常 candidate 的行为不变。

修改前：先检查 active、未 `rob_commit`、无 replay/redirect/flush/killed，再以 fault 位或
`exception_pending` 判定 candidate；没有普通 scalar producer 范围限制，unsupported fault 可能
被选入后续 drain。

修改后：

```text
先执行原有 active、rob_commit、replay、redirect、flushed 和 issue_killed 门禁；任一不满足，
返回 false，保持 recovery owner 优先。
再计算是否存在任意 fault/exception_pending/target fault；完全没有 fault 时返回 false。
保留原有 writeback/target readiness。只有泛化 fault、却没有真实 writeback 或 target fault 时，
将其视为不完整 unsupported fault，而不是普通“尚未 ready”。
再调用 uid_is_supported_scalar_fault(uid)。
若返回 true，允许该 uid 作为 fault candidate。
若分类不支持或 readiness 不完整，且 uid 不是 commit_cursor_uid，返回 false，让年轻 unsupported
uid 不抢占当前 head。
若返回 false 且 uid 正是 commit_cursor_uid，立即 uvm_fatal，并打印类型、target、numLsElem、
exceptionVec 和 key，report ID 固定为 `LSQ_COMMIT_UNSUPPORTED_FAULT`，避免 cursor 静默卡死。
不在该函数中释放 LSQ，不推进 cursor，也不建立 fault token。
```

#### `constructor`、`reset_lsqcommit_runtime_state()` 与 `mark_fault_rob_commit_uid()`

抽象功能描述：初始化、重置和 token 建立路径共同维护 fault watchdog 的私有计数，并保持已有
ROB token/head 状态生命周期；它们不拥有 deq、cancel 或 terminal。

修改原因与差异影响：旧路径没有等待计数；只增加私有字段的初始化/归零，不改变公共 status、
pointer、free count 或 token 建立条件。

修改前：constructor/reset 只初始化已有 cursor、modeled head、watermark 和 fault token；
`mark_fault_rob_commit_uid()` 在 xaction 完成后设置 `rob_commit` 和 token，但没有 fault 专用
等待计数。

修改后：

```text
constructor/reset：按原逻辑初始化或清除已有字段，并额外把
fault_head_wait_cycles 置 0；不清公共 status、LSQ free count 或 active map。

mark_fault_rob_commit_uid(uid)：先执行原有 global-flush、cursor、head key 和 fault candidate
校验；finish_item 已返回后按原逻辑写 status.rob_commit、fault_head_waiting、fault_head_uid、
fault_head_dynamic_epoch，并把 fault_head_wait_cycles 置 0；随后仍调用原有 try_retire 和
sync 尝试，不能在这里伪造 lqDeq/sqDeq。
```

#### `service_fault_head_drain_watchdog()`

抽象功能描述：在每个 LSQ commit driver 边界观察现有 fault token，避开 global flush，针对同一
dynamic instance 累计无真实 deq 的等待周期，并在固定阈值报告诊断；不替代任何状态 owner。

修改原因与差异影响：旧逻辑可能无限等待；新增私有诊断计数和 fatal，但不新增 raw provenance、
不伪造 deq，也不改变 redirect/terminal 收口。

修改前：没有 fault 专用计数；fault token 可能一直阻塞 commit loop，只能依赖公共 idle warning，
且公共 warning 参数置 0 时不会再提示。

修改后：

```text
每个 LSQ commit driver 周期调用一次无参 helper。
若没有 fault token，清 fault_head_wait_cycles 并返回。
若 data.issue_blocked_by_global_flush() 为真，保持当前计数不变并返回，不在 flush idle 周期计数。
读取 fault uid 的 status。
若 dynamic_epoch 不匹配，或 status.flushed、status.issue_killed、!status.rob_commit，
只清 fault_head_wait_cycles 并返回；让下一次既有 sync/redirect owner 清理旧 token，
不得在这里 fatal、rebase 或推进 cursor。
否则递增 fault_head_wait_cycles。
未达到 FAULT_HEAD_DRAIN_TIMEOUT_CYCLES 时返回。
达到阈值时读取 main transaction 和所有 ROB/LQ/SQ key，uvm_fatal；不调用任何 deq/cancel/
terminal helper，不写 free count、pointer、mapping 或 public status。
```

#### `drive_lsqcommit_loop()`

抽象功能描述：保持既有逐周期 LSQ commit xaction、idle warning 和 global-stop 调度，只在每轮
发送完成后提供一个唯一 watchdog service 边界。

修改原因与差异影响：watchdog 若放在多个 monitor/deq owner 会重复计数；把单次调用放在现有
driver loop 末尾，只增加 fault 诊断，不改变 `has_progress`、sideband 或 stop 语义。

修改前：每轮调用 `send_lsqcommit_cycle()`，根据返回的 `has_progress` 更新既有 `idle_count`，
达到 `no_progress_warn_cycles` 时输出 warning，最后按原有 flush/terminal 条件判断 global stop。

修改后：

```text
每轮先调用 send_lsqcommit_cycle()；该 task 的 item 构造、driver 发送、normal commit、fault
token 建立和 has_progress 计算保持不变。
send_lsqcommit_cycle() 返回后立即调用
commit_handler.service_fault_head_drain_watchdog() 一次。
随后完全按旧逻辑更新 idle_count、使用 no_progress_warn_cycles 输出 warning，并判断 global
stop。watchdog 的私有计数与 idle_count 分离，不能互相重置，也不能改变 stop 条件。
```

#### 既有 `soft_test_memblock_dispatch_fault_smoke_sequence::body()` guard block

抽象功能描述：在已有 software-only fault smoke 中验证两条新增防御 fatal 的 report ID 和
消息边界，随后恢复 handler 私有状态并继续原有 synthetic ledger；不产生真实 DUT fault/deq。

修改原因与差异影响：原 smoke 只验证状态转移，无法发现 unsupported head 分支或 watchdog
分支被错误删除、误报或吞掉其它 fatal。该修改只增加 test-only 自检，不改变生产 owner、
状态语义、阈值或 real fault stimulus。

修改前：body 直接建立主表、注入 software-only fault writeback、调用 direct deq helper，最后
检查 fault terminal。

修改后：

```text
在原 synthetic deq ledger 前建立 unsupported fault head；安装只匹配
LSQ_COMMIT_UNSUPPORTED_FAULT 的 catcher，调用 fault-head selector，要求恰好捕获一次目标 fatal。
删除 catcher；调用 data.reset_all_tables(1)、lsq_ctrl.reset() 和
commit_handler.reset_lsqcommit_runtime_state()，清除该 guard 的 status、active map、pointer 和 token。

建立受支持 scalar LDA fault token，确认没有 global flush；安装只匹配
LSQ_COMMIT_FAULT_WATCHDOG 的 catcher，循环调用无参 watchdog 至固定阈值，要求恰好捕获一次目标
fatal。删除 catcher；再次调用 data.reset_all_tables(2)、lsq_ctrl.reset() 和
commit_handler.reset_lsqcommit_runtime_state()，再重新建立原 smoke 的主表和 issue 状态。

继续原 body 的 direct apply_dut_lq_deq()/apply_dut_sq_deq() ledger 和 terminal/status 检查；
这些 direct helper 仍只属于 software-only 单元测试。
```

#### `memblock_lsq_commit_expected_fatal_catcher::new()` / `catch()`

抽象功能描述：该 test-only class 保存一个 expected report ID 和消息 pattern，在 guard 的
注册窗口内精确捕获一类 fatal；它不参与生产 fault candidate、watchdog 或 terminal flow。

修改原因与差异影响：原环境没有针对 `LSQ_COMMIT_*` 的专用过滤对象，无法验证 fatal 分支后
安全恢复，也可能用宽泛 catcher 吞掉真正错误。新增参数化 catcher 只影响 software-only 检查。

修改前：没有该 class；任何 fatal 都由默认 UVM report 流程处理。

修改后：

```text
new(name, report_id, message_pattern)：保存两个匹配条件并将 caught_count 清零。
catch()：读取 severity、ID 和 message；只有 severity=UVM_FATAL 且 ID/pattern 同时匹配时
    caught_count++，返回 CAUGHT；其它情况返回 THROW。
guard 调用前 add(null, catcher)，调用完成后 delete(null, catcher)，再执行公共 data/LSQ reset。
```

本节之后不再增加新的正文章节；上面的差异说明仍是本 plan 的最后章节。
