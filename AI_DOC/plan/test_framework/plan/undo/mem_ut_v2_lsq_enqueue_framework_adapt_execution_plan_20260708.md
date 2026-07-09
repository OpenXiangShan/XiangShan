# mem_ut V2 LSQ enqueue 测试框架适配执行 Plan

## 1. Plan 定位

本文是 V2 `enqLsq_req` 字段补齐和 LSQ admission 细节适配的执行 plan。目标是不改变 LSQ admission 主体 flow，只补齐 V2 真实存在字段的生成、默认和不支持策略。

主体 flow 保持：

```text
main table uid 顺序
  -> collect_lsq_candidates()
  -> assign_lsqenq_slot()
  -> driver 等待 DUT ready
  -> confirm_lsq_candidates()
  -> lsq_ctrl_model 分配 LQ/SQ key
  -> issue_queue_scheduler route
```

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv
mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv
```

`main_control_transaction` 是本 plan 会读取的类。执行前必须用 `rg -n "class main_control_transaction" mem_ut/ver/ut/memblock/seq mem_ut/ver/ut/memblock/common` 确认其当前实际定义文件；只有字段缺失时才修改该实际定义文件，不得按不存在路径新增文件。

依赖：

- 必须先执行或同步考虑 `mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md`。

不属于本 plan：

- 不新增 vector LS 完整支持。
- 不修改 issue queue 主算法。
- 不修改 RM/checker/coverage。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 会对照 RTL/agent 确认 `enqLsq_req` 字段和 slot 口径；该检查是执行前防误读，不代表本 plan 会直接修改 RTL。

## 3. 问题依据

V2 逐端口差异显示 `io_ooo_to_mem_enqLsq_req_*` 中保留以下 V2 真实字段：

```text
exceptionVec[0..23]
flushPipe
fuOpType[8:0]
lastUop
trigger[3:0]
fuType[34:0]
robIdx/lqIdx/sqIdx
numLsElem
```

当前 V2 实际 slot 口径按 slot 0..5 共 6 个执行。执行前必须用 `rg` 同时检查 RTL、`lsqenq_agent_connect.sv`、agent xaction/interface/driver，确认没有 req_6/7 残留被误当作当前口径。

当前 `memblock_lsqenq_dispatch_base_sequence::set_req_fields()` 只填：

```text
valid
fuType
uopIdx
robIdx
lqIdx
sqIdx
numLsElem
```

这会让 V2 `fuOpType/exceptionVec/flushPipe/lastUop/trigger` 依赖 driver idle 默认或 connect 默认，不满足“进入 DUT 的每个行为字段必须有来源说明”的要求。

## 4. 修改原因

LSQ enqueue 是 main table 进入 DUT 的第一条真实激励路径。V2 `enqLsq_req` 字段比当前 sequence 填充内容多，如果不明确补齐：

- `fuOpType` 缺失会让 DUT 无法按真实 load/store/preload/CBO/AMO 类型解释 uop。
- `exceptionVec/flushPipe/trigger` 若默认 0，必须说明当前 smoke 不构造 enqueue 前异常、flushPipe 和 trigger。
- `lastUop` 对多 uop/atomic/vector 有语义影响；当前只支持标量单 uop 时必须固定为 1 或由 `uop_count` 推导。

## 5. 修改后方案

### 5.1 新增字段上下文

新增轻量结构或参数传递，建议在 `memblock_lsqenq_dispatch_base_sequence.sv` 内部定义局部 helper 输入，不新增全局状态：

```text
memblock_lsqenq_req_meta_t:
  fuOpType
  exception_vec
  flush_pipe
  last_uop
  trigger
```

若项目不希望新增 typedef，可直接扩展 `set_req_fields()` 参数，但必须保持每个字段来源清晰。

字段来源：

| 字段 | 来源 | 默认策略 |
|---|---|---|
| `fuOpType` | `main_tr.fuOpType` | 必填；若 op 行为非法由 `lsq_ctrl_model::derive_op_behavior()` fatal |
| `exceptionVec` | 当前主表不构造 enqueue 前异常 | 默认 0；后续异常激励另建 plan |
| `flushPipe` | 当前主表不构造 enqueue 前 flushPipe | 默认 0 |
| `lastUop` | 普通标量 load/store 为 1；atomic 多 uop 由 `uop_index/uop_count` 推导 | 当前普通路径为 1 |
| `trigger` | 当前主表不构造 trigger | 默认 0 |

### 5.2 slot 范围

`clear_lsqenq_xaction()`、`set_need_alloc()`、`set_req_fields()` 的 slot 上限必须使用编译期 `MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG` 或 `seq_csr_common::get_real_enq_width()` clamp 后值。case 分支仍可按当前 V2 6 slot 展开，但 fatal 信息必须说明编译期真实 slot 上限。若执行前检查发现当前 RTL/agent 不是 0..5 共 6 slot，停止并先修订 compile param/interface plan。

### 5.3 vector LS 策略

当前 `lsq_ctrl_model::derive_op_behavior()` 遇到 vector LS 会 fatal。LSQ enqueue plan 必须保留该边界，不允许把 `VLDU/VSTU/VSEGLDU/VSEGSTU` 静默映射成 scalar LDU/STU。

### 5.4 `fuType` DUT 边界适配

内部 `main_tr.fuType` 保留 compile param plan 定义的内部规范宽度。V2 DUT `enqLsq_req_*_bits_fuType` 是 `[34:0]`，coding 时必须同步检查 agent xaction/interface/driver/connect 的字段宽度，并在写 DUT 字段前调用 compile param plan 中定义的 `fit_dut_futype()` 或等价 helper。禁止直接截断 `main_tr.fuType[34:0]`。若 xaction 当前仍是 `[35:0]`，执行者必须在本 plan coding 中同步修正 agent 边界或明确保留内部字段再在 driver/connect 边界 fit，不能让 sequence 方案和 agent 字段断开。

### 5.5 `lastUop` 执行前确认

普通 scalar LSQ enqueue 当前策略为 single-uop smoke 下 `lastUop=1`。执行前必须从 V2 Scala/生成 RTL 确认普通 scalar load/store enqueue 的 `lastUop` 语义。若无法确认，默认只在 scalar single-uop smoke 配置下启用该赋值，并在遇到多 uop/atomic/vector 时 fatal；不得把 `lastUop=1` 扩展为所有操作的无条件默认。

## 6. 函数/任务级伪代码

### 6.1 `build_lsqenq_req_meta()`

函数目的：集中生成 V2 `enqLsq_req` 额外字段，避免 `set_req_fields()` 内散落默认值。

输入：

- `uid`
- `main_tr`
- `behavior`
- `uop_index`
- `uop_count`

输出/副作用：

- 返回 enqueue request meta。
- 不修改公共状态表，不推进 LSQ pointer。
- 字段非法时 fatal。

源码级伪代码：

```text
function memblock_lsqenq_req_meta_t build_lsqenq_req_meta(uid, main_tr, behavior, uop_index, uop_count);
    meta = default_zero_meta();
    if (main_tr == null) fatal;
    if (lsq_ctrl_model::is_vector_ls_futype(main_tr.fuType)) fatal unsupported vector;

    meta.fuOpType = main_tr.fuOpType;
    meta.exception_vec = 24'h0;
    meta.flush_pipe = 1'b0;
    meta.trigger = 4'h0;

    if (uop_count == 0) fatal;
    meta.last_uop = (uop_index == uop_count - 1);
    if (!meta.last_uop && !behavior.is_atomic) fatal unexpected multi-uop scalar;
    return meta;
endfunction
```

中文文字伪代码：

该函数在每个 LSQ enqueue slot 填充前调用。它先检查主表 transaction 存在，并复用 `lsq_ctrl_model` 的 vector 检查，保证当前不支持的 vector LS 不会进入 DUT。随后把 `fuOpType` 从主表复制到 request meta；异常向量、flushPipe 和 trigger 在当前 smoke 中不构造，因此统一置 0，并把这个默认策略集中记录。最后根据 `uop_index/uop_count` 计算 `lastUop`。普通标量路径只有一个 uop，因此 `lastUop` 为 1；如果未来 atomic 多 uop 进入该路径，则按最后一个 uop 置 1，否则 fatal 暴露未定义组合。

### 6.2 `set_req_fields()`

函数目的：把主表和 meta 字段写入 V2 `enqLsq_req_<slot>` xaction 字段。

输入：

- `slot`
- `valid`
- `fuType`
- `uopIdx`
- ROB/LQ/SQ key
- `numLsElem`
- `meta`

输出/副作用：

- 修改 `lsqenq_agent_agent_xaction` 对应 slot 字段。
- 不更新公共状态。

源码级伪代码：

```text
function void set_req_fields(tr, slot, valid, fuType, uopIdx, rob_key, lq_key, sq_key, numLsElem, meta);
    if (slot >= seq_csr_common::get_compile_lsq_enq_slot_num()) fatal;
    case (slot)
      0: begin
        tr.req_0_valid = valid;
        tr.req_0_bits_fuType = fit_dut_futype(fuType, "lsqenq_req_0");
        tr.req_0_bits_fuOpType = meta.fuOpType;
        tr.req_0_bits_exceptionVec_* = meta.exception_vec[*];
        tr.req_0_bits_flushPipe = meta.flush_pipe;
        tr.req_0_bits_lastUop = meta.last_uop;
        tr.req_0_bits_trigger = meta.trigger;
        tr.req_0_bits_uopIdx = uopIdx;
        tr.req_0_bits_robIdx_* = rob_key;
        tr.req_0_bits_lqIdx_* = lq_key;
        tr.req_0_bits_sqIdx_* = sq_key;
        tr.req_0_bits_numLsElem = numLsElem;
      end
      ...
    endcase
endfunction
```

中文文字伪代码：

该函数只负责把已经确定的字段写入 xaction，不再自己推导业务语义。它先确认 slot 没有超过编译期真实 LSQ enqueue slot 数，然后按 slot 展开赋值。每个 slot 都写入 valid、FU 类型、FU op 类型、异常向量、flushPipe、lastUop、trigger、uopIdx、ROB/LQ/SQ key 和 numLsElem。FU type 通过 `fit_dut_futype()` 从内部规范宽度适配到 V2 DUT 端口宽度；该 helper 会检查高位和 vector unsupported，禁止无说明截断。函数不推进 LSQ 软件模型，真正的 admission 成功仍由 `confirm_lsq_candidates()` 调用 `lsq_ctrl.commit_allocate_with_resp()` 完成。

### 6.3 `assign_lsqenq_slot()`

函数目的：在现有 admission flow 中接入 meta 构造。

源码级伪代码：

```text
function void assign_lsqenq_slot(tr, slot, uid, main_tr, behavior, lq_key, sq_key);
    if (slot >= seq_csr_common::get_real_enq_width()) fatal;
    meta = build_lsqenq_req_meta(uid, main_tr, behavior, 0, 1);
    set_need_alloc(tr, slot, behavior.need_alloc);
    set_req_fields(tr, slot, 1'b1, main_tr.fuType, uid[6:0],
                   main_tr.get_rob_key(), lq_key, sq_key, behavior.num_ls_elem, meta);
endfunction
```

中文文字伪代码：

该函数保持原有调用位置不变。它先检查 slot 是否在 runtime 本次允许使用的入队宽度内，然后为当前 uid 构造 V2 request meta。随后写 `needAlloc`，再把主表字段、LSQ key 和 meta 一起交给 `set_req_fields()`。公共状态表仍然只在 driver 完成并确认未被 redirect 打断后推进，因此不会改变原有 admission 生命周期。

## 7. 验收标准

1. 每个 V2 `enqLsq_req` 行为字段都有来源说明：主表、明确默认值或不支持 fatal。
2. `fuOpType` 不再依赖默认 0，而是从 `main_tr.fuOpType` 写入。
3. `exceptionVec/flushPipe/trigger` 默认 0 的原因写入注释或 review：当前 testcase 不构造 enqueue 前异常/trigger/flushPipe。
4. `lastUop` 对普通标量为 1 的策略已完成 V2 Scala/RTL 语义确认；若无法确认，只允许 scalar single-uop smoke 使用，多 uop/atomic/vector fatal。
5. `collect_lsq_candidates()` 仍只扫描 active 前缀后最多本拍 slot 数，不新增每拍全表扫描。
6. vector LS 仍显式 fatal 或由参数禁止生成，不静默按 scalar 处理。
7. V2 DUT `fuType[34:0]` 写入点全部调用 `fit_dut_futype()` 或等价 helper，agent xaction/interface/driver/connect 宽度与该策略一致。
8. 执行前确认当前 slot 仍为 0..5 共 6 个，不被旧 req_6/7 分析误导。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/seq mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv AI_DOC
rg -n "exceptionVec|flushPipe|fuOpType|lastUop|trigger" mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv
rg -n "class main_control_transaction" mem_ut/ver/ut/memblock/seq mem_ut/ver/ut/memblock/common
rg -n "enqLsq_req_[0-9]+|enqLsq_needAlloc_[0-9]+|fit_dut_futype" build_memblock/rtl/MemBlock.sv build/rtl/MemBlock.sv mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src mem_ut/ver/ut/memblock/seq/base_seq
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

若修改影响真实 dispatch 主流程，增加：

```bash
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
```

## 9. 与原始/初步 plan 差异说明

初步总控 plan 只提出 `set_req_fields()` 不能只写旧字段。本文将该要求拆成可 coding 的三个函数：`build_lsqenq_req_meta()`、扩展后的 `set_req_fields()`、接入后的 `assign_lsqenq_slot()`。本文还明确了每个新增 V2 字段的默认原因和 vector LS 不支持边界。

## 10. 风险与非目标

风险：

- 若当前 `lsqenq_agent_agent_xaction.sv` 尚未包含 V2 字段，本 plan 执行时需要先按 DUT/interface 适配规则补齐字段链路。
- 如果 V2 `lastUop` 对普通单 uop 默认不是 1，需要回到 Scala/RTL 语义重新确认。

非目标：

- 不实现 enqueue 前异常激励。
- 不实现 trigger 激励。
- 不实现 vector LS。
- 不改变 LSQ admission 高水位、redirect 回退和 issue route 主逻辑。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`局部逻辑适配 + 仅字段/参数适配`。局部逻辑适配集中在 `set_req_fields()` 的 V2 字段补齐、`build_lsqenq_req_meta()` 的默认/不支持策略和 `fit_dut_futype()` 边界调用；字段/参数适配集中在 V2 request 字段、slot 数和 `fuType` DUT 边界；主体 LSQ admission flow 不改变。

原测试框架逻辑：

- `memblock_lsqenq_dispatch_base_sequence::body()` 初始化 `seq_csr_common`，按 `MEMBLOCK_LSQENQ_SEQ_EN` 决定是否运行。
- `drive_lsqenq_loop()` 每拍调用 `send_lsqenq_cycle()`，先 `apply_pending_lsq_cancels()` 处理 redirect 后的 LQ/SQ 回退，再尝试 `admit_non_lsq_if_ready()`，然后通过 `collect_lsq_candidates()` 从 `data.get_next_new_admit_uid()` 开始收集连续候选。
- `collect_lsq_candidates()` 只在当前连续 admission 前缀后扫描，扫描上限由 `seq_csr_common::get_enq_per_cycle()` 控制；它用 `lsq_ctrl_model::derive_op_behavior()`、LQ/SQ 临时指针和 free count 判断本拍候选，不扫描完整主表。
- `assign_lsqenq_slot()` 写 `needAlloc` 和旧字段；`confirm_lsq_candidates()` 在 driver 完成且未被 redirect 打断后调用 `lsq_ctrl.commit_allocate_with_resp()` 和 `complete_admission()`。`complete_admission()` 再触发 `issue_sched.prepare_issue_route_for_uid()`。

本 plan 修改后逻辑：

- `collect_lsq_candidates()`、`confirm_lsq_candidates()`、`complete_admission()` 的状态推进时机保持不变。
- `assign_lsqenq_slot()` 在原位置新增 meta 构造，仍只负责把已确定的 uid、ROB/LQ/SQ key、behavior 和 meta 写入 xaction。
- `set_req_fields()` 从只写旧字段扩展为写 V2 `fuOpType/exceptionVec/flushPipe/lastUop/trigger` 等真实存在字段。
- vector LS、多 uop/atomic 中无法证明 `lastUop` 语义的组合按 plan fatal 或由参数禁止，不静默当作 scalar single-uop。

逻辑改变项：

1. 新增 `build_lsqenq_req_meta()` 或等价局部 helper。原因是 V2 `enqLsq_req` 有更多行为字段，必须集中说明来源。该 helper 不读写公共状态，只返回本次 xaction 字段。
2. `set_req_fields()` 扩展参数和赋值字段。原因是不能继续依赖 driver idle 默认或未连接字段默认值。该改变只影响 DUT input 字段完整性，不改变 admission 成功条件。
3. `lastUop` 和 vector LS 检查加入 fatal/drop 策略。原因是当前 scalar 主 flow 不支持 vector LS，也不能无依据支持多 uop 语义。该改变只暴露未支持激励，不改变已支持 scalar flow。
4. V2 DUT `fuType` 写入前调用 `fit_dut_futype()`。原因是 V2 端口宽度与内部规范宽度不同，不能隐式截断。

字段/参数改变项：

- V2 enqueue xaction/interface/driver/connect 字段链路需覆盖 `fuOpType`、`exceptionVec`、`flushPipe`、`lastUop`、`trigger`。
- `exceptionVec/flushPipe/trigger` 默认 0，语义是当前 smoke 不构造 enqueue 前异常、flushPipe 或 trigger 激励。
- `lastUop` 对普通 scalar single-uop 默认 1；若执行前 Scala/RTL 复核不支持该结论，必须收窄到已确认场景或另建 plan。
- `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH` 和编译期 slot 数共同限制 slot 使用范围。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增每拍全表扫描；候选收集仍按连续 uid 和本拍 slot 上限推进。
- 不改变 LQ/SQ pointer、free count、active map、main table status、`max_enqueued_uid` 和 `terminal_done_uid` 的生命周期。
- 不改变 redirect 后 `pending_lq_cancel_count/pending_sq_cancel_count` 的回退策略。
- 不改变 pass/fail/terminal_done；enqueue 字段补齐只影响 DUT 接收的 request 语义。

覆盖性结论：

本 plan 覆盖 V2 LSQ enqueue 字段补齐和入队 slot 数适配。compile/static width 由参数 plan 覆盖；split issue、writeback、MMIO/status、L2TLB、monitor output 和 CSR/control 由各自 plan 覆盖。本 plan 的改变属于 V2 端口细节适配和局部字段构造，不影响测试框架主体 LSQ admission 逻辑。
