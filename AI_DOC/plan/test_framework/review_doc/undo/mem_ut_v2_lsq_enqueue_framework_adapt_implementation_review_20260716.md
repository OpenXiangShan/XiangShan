# mem_ut V2 LSQ Enqueue 适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_enqueue_framework_adapt_final_plan_20260714.md` |
| Review 日期 | 2026-07-17 |
| 目标版本 | V2，分支 `mem_ut_uvm_v2` |
| Review 结论 | 第12轮源码review和修复后的第13轮文档review均通过，无新发现、无必须修改项 |
| Review 范围 | LSQ enqueue agent、sequence、软件 allocation、参数/类型和同步文档 |

## 1. 结论与发现

前置 review 找到一项低严重问题：V2 已无 enqueue response，但 `seq_csr_common` 仍可能
warning/clamp LSQ ready timeout。实现已按 compile capability 修复：公共加载仍解析并检查参数非负，
只有 `MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP=1` 才对零值 warning/clamp；V2 sequence 不读取该 getter。

归档前独立 review 第 1 轮找到两项中严重问题，均已修复：

- `lsqenq_agent_agent_xaction::compare()` 的手工回退路径遗漏 V2 extra 字段，`psdisplay()` 也未展示这些
  字段。现已通过 `v2_extra_fields_t` 和 `get_v2_extra_fields()` 把六个 slot 的
  `exceptionVec/trigger/fuOpType/flushPipe/lastUop` 同时接入 display 和 compare。
- 本文原第 10/11 章没有为 Plan 差异提供完整源码证据和紧邻中文伪代码，且漏记 idle item 通过全零
  `send_pkt()` 而非 `drive_idle()` 的等价实现差异。本文已按源码 review 规则重写这些章节。

归档前独立 review 第 2 轮没有发现 LSQ enqueue 源码功能问题，但发现两项文档问题，均已修正：

- 公共 `load_from_plus()` 仍解析 `MEMBLOCK_LSQENQ_READY_TIMEOUT` 并检查非负；V2 只是不读取 getter、
  不等待 ready，且跳过零值 warning/clamp。本文和同步文档已统一为该真实边界。
- 当前工作区中的 CSR control/sfence 专项 Plan 原先未进入“非本次修改”分类，现已明确排除。

归档前独立 review 第 3 轮确认上述两项已闭环，并发现参数规则仍把
`MEMBLOCK_LSQENQ_SEQ_EN` 写成默认 0，与 `plus.sv/default.cfg` 的既有默认 1 相反。规则现已同步为
默认启用，并明确无主表时只等待且保持 idle、显式关闭时直接返回的边界；运行期源码和默认值未修改。

归档前独立 review 第 4 轮仍未发现源码功能问题，但发现三类文档阻断，均已修正：

- 当前 web/interface/source analysis 仍展示旧 8-slot、`canAccept/response` 和 response wrapper主链；现已
  更新为V2 6-slot、6/4 gate、clock-first launch、`commit_allocate()` reservation和下一边界completion。
- 多份已归档Plan/review仍保留旧正文却没有失效边界；现统一增加醒目历史注记并指向当前Plan/flow。
- 总控Plan同一文件中的CSR/sfence并发hunk原先未分类，且最终Plan第13章漏记enable默认值文档同步；
  两处均已补齐。

归档前独立 review 第 5 轮继续发现前述类别中的内部残留，源码仍无新问题。修正内容如下：

- Web Markdown/普通JS/enhanced JS/模板数据的其它LSQ节点统一删除旧idle/ready timeout、8-wide、
  DUT response和等待接收描述；三份JS均重新通过`node --check`。
- interface前部8路request/response表直接改为V2 6路compile宏合同；公共`lsq_state`改为
  launch/reservation/pending-sample。
- current flow/source analysis中的固定`5'd0/1`、已删除`get_real_lsq_enq_max()`、DUT response写key和
  旧sequence类名全部按当前源码刷新；自动主表地址窗口同步为`MEMBLOCK_MAIN_VADDR_*`。
- 第5轮点名的其余历史Plan/review均增加失效注记；总控Plan在真正归档前恢复`undo`路径和等待状态。

归档前独立review第6轮继续确认源码无新问题，并指出少量文档合同不精确。现已统一为：V2 key来自
软件preview；`complete_admission()`只设置`issue_ready`并route；Web显式关闭不fallback且helper使用源码
真名；未launch无取消原因和launch/abort同时为1均fatal；unused key可携带另一队列preview但不消费；
随机0只保证不读uid/pointer/free/table/map。归档门禁措辞和review artifact覆盖也已修正。

归档前独立review第7轮发现五项中严重问题：通用default sequence缺少4-store batch gate；最终smoke早于
最新compile；Web仍有旧调用签名/AMO可达描述；本文漏展开`send_idle_lsqenq_boundary()`及其两个调用边界；
interface主文档仍保留V3 FuType编码。现已补齐xaction batch约束和driver fail-fast复核，同步Web/interface，
并在第5、7、11章补齐源码证据。修复后20:53开始的clean compile在20:55成功结束，随后20:55:57
真实load smoke通过；第12/13章已更新最终日志和执行顺序。

归档前独立review第8轮发现两项源码合同缺口和四项文档问题：active `fuOpType`未限制为本轮支持的
load/prefetch/store集合；custom compare/display遗漏部分framework metadata；non-LSQ pending边界文档顺序
不准确；本文两组源码块混合多个逻辑对象；compile warning描述和Plan编号数量不准确。现已新增agent层
opcode宏值表并由constraint直接求解、driver helper复核，补齐五个metadata打印/比较，修正flow/web调用顺序并拆分review
源码块；验证结论只保留真实的工具warning边界。

归档前独立review第9轮发现一项源码合同缺口和三项文档问题：通用default random item继承基类
`post_pkt_gap=0..50`，会被V2 driver零gap合同拒绝；两份当前flow仍遗漏non-LSQ前的idle sample或使用
不存在的`epoch_mismatch`；本文关键task/helper源码展开不完整；第13.3节仍把第7轮日志称为最终证据。
现已由LSQ xaction硬约束pre/post gap均为0，并按真实源码补齐flow和本文控制链。default-random专项随后
还发现constraint调用自定义opcode helper无法让VCS反向求解随机`fuOpType`；实现已改为宏值表单一权威、
constraint直接`inside`、driver helper复核。第9轮最终clean compile、default-random专项和同一`simv`
真实scalar-load smoke均已通过，证据见第12章。

归档前独立review第10轮发现一项低严重setter合同缺口：`set_req_fields()`没有在入口自行拒绝空
xaction、越界slot或不完整idle调用，错误caller可能在字段写入前得不到受控fatal。现由setter检查
`tr != null`和slot compile范围；idle要求`main_tr == null`、behavior完整等于
`make_default_behavior()`且LQ/SQ key全零。修复后最终clean compile、`UVM_FULL` default-random专项和
真实scalar-load smoke均已通过，并进入第11轮独立review。

归档前独立review第11轮没有发现高/中严重源码功能问题，但发现两项低严重源码合同缺口：custom compare
手工回退遗漏pre/post gap；当前显式六slot字段链没有拒绝非6/6/4 compile tuple。两项均已按最小方案
修复：display/compare覆盖streaming gap，compile consistency在激励前要求当前V2 tuple为6/6/4。

同轮文档review发现：最终验证时间链不能绑定当前源码；current source/Web仍有旧类名或错误allocation
调用边；setter完整自检未同步；主flow三个源码块共用一份伪代码；epoch条件表述不精确；非本次修改清单
漏列L2 inner TileLink分析。以上问题均已逐项按当前源码修复。修复后冻结
`mem_ut/ver/ut/memblock` diff，冻结版clean compile、default-random专项和随后同一`simv`的真实load smoke
严格顺序通过；冻结哈希和日志见第12章。第12轮最终独立review通过前仍不得归档本Plan。

当前实现满足以下正确性条件：

- scalar request 不再把 uid 写入 `uopIdx`，V2 extra字段由唯一setter完整赋值/清零；通用random/direct
  item的`fuOpType`也只能落入load/prefetch/store支持集合。
- dispatch candidate单拍load不超过6、store不超过4且不超过实际free count；通用random/direct item也由
  xaction约束和driver复核限制6/4；所有路径都不保留无意义的6/4队尾空项。
- driver无 `canAccept/response` 等待，连续batch可每拍launch；当前batch不会在DUT sample机会之前进入issue。
- 通用random item由xaction保证pre/post gap均为0；driver保留同一检查拦截绕过约束的directed item。
- redirect launch前不预留，launch后通过active mapping和原cancel owner回退，不新增第二套recovery状态。
- 随机0只产生idle，不消费uid；zero-only配置在0ns fail-fast。
- 主表、issue scheduler内部算法、writeback、commit/deq、pass/fail和terminal owner未改。

## 2. Diff 覆盖矩阵

| 文件 | Review 覆盖 | 结论 |
|---|---|---|
| `cfg/memblock_compile_params.svh`、`seq_csr_common::check_compile_param_consistency()` | LSQ 6/4、uopIdx/numLsElem派生宏；当前显式字段链在激励前严格要求6/6/4 tuple | 通过 |
| `env/plus.sv`、`seq/plus_cfg/default.cfg` | 三类权重定义、加载和默认值 | 通过，见 5.2 |
| `seq/base_seq_help/memblock_dispatch_types.sv` | compile localparam 和 `memblock_num_ls_elem_t` | 通过，见 3.3 |
| `seq/base_seq_help/main_control_transaction.sv` | `numLsElem` 统一类型和范围 | 通过，见 3.3 |
| `memblock_dispatch_base_sequence.sv`、manual/soft producer | 0/1 typed cast | 通过，见 3.3 |
| `seq/base_seq_help/lsq_ctrl_model.sv` | 唯一 allocation owner、response wrapper | 通过，见第 8 章 |
| `seq/base_seq_help/seq_csr_common.sv` | compile 检查、权重、randomize、V2 timeout capability | 通过，见 3.4、5.2 |
| `lsqenq_agent_agent_cfg.sv` | V2 idle 固定 `DRV_0` | 通过，见 6.2 |
| `lsqenq_agent_agent_interface.sv` | `uopIdx/numLsElem` 消费 compile width | 通过，见 3.3 |
| `lsqenq_agent_agent_xaction.sv` | 宽度、scalar/opcode constraint、metadata、field automation，以及pre/post gap和全部本类功能字段的display/custom compare回退 | 通过，见第 4、11.5 章 |
| `lsqenq_agent_agent_driver.sv` | clock-first streaming、redirect launch gate、pre-drive scalar/opcode check | 通过，见第 4/6 章 |
| `lsqenq_agent_agent_monitor.sv` | local field 和 X/Z check 消费 compile width | 通过，见 3.3 |
| `memblock_lsqenq_dispatch_base_sequence.sv` | candidate、setter、pending-sample、redirect/drain | 通过 |
| flow/source analysis/参数规则/总控plan/专项plan | 当前实现同步 | 通过 |
| `dispatch_plan_v2_development_detail_20260614.md` | 早期 LSQ enqueue 章节增加失效边界，默认值条目标明历史/当前语义 | 通过 |
| `main_table_build_and_stimulus_flow.md`、interface/source analysis | 当前LSQ主链改为cancel -> non-LSQ idle boundary -> admission -> launch/reservation/pending-sample | 通过 |
| `AI_DOC/web` Markdown/普通与enhanced脚本数据 | 删除旧ready/response节点，按源码顺序展示pending cancel、non-LSQ idle边界、completion和唯一allocation owner | 通过 |
| 其它早期dispatch Plan/review | 保留审计正文，文首增加LSQ enqueue失效注记并指向当前权威 | 通过 |
| `issue_queue_scheduler.md`、`main_control_transaction.md`及关联source analysis | 删除旧runtime getter、固定`5'd`、DUT response写key和不存在的sequence类名 | 通过 |
| 总控Plan LSQ状态行 | 最终Plan移动前保持`undo`路径；CSR/sfence并发hunk单独分类 | 通过 |
| 本 implementation review 文档 | 记录全部review轮次、diff覆盖、非本次修改分类和最终门禁 | 通过，当前为未跟踪review artifact |

`tb/lsqenq_agent_connect.sv` 和 V2 `dut_inst.sv` 的 extra字段链在执行前已完整，本轮核对后没有制造diff。

## 3. 编译期结构与字段适配

### 3.1 修改前逻辑

LSQ agent和sequence仍存在固定7-bit `uopIdx`、5-bit `numLsElem`，没有独立的load/store enqueue width
编译期权威。固定值在当前V2恰好相等，但版本参数变化时会形成第二权威。

### 3.2 修改后逻辑

源码位置：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`。该字段组定义V2物理结构和派生宽度。

```systemverilog
`define MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH 6
`define MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH 4
`define MEMBLOCK_DUT_UOP_IDX_W ($clog2(`MEMBLOCK_DUT_MAX_UOP_SIZE + 1))
`define MEMBLOCK_DUT_MAX_LS_ELEM (`MEMBLOCK_DUT_VLEN / 8)
`define MEMBLOCK_DUT_NUM_LS_ELEM_W ($clog2(`MEMBLOCK_DUT_MAX_LS_ELEM) + 1)
```

中文伪代码：

```text
把load/store每拍端口能力固定为当前V2的6/4；
只保留MAX_UOP_SIZE和VLEN为主参数；
由表达式派生uopIdx、最大LS element和numLsElem宽度；
interface/xaction/monitor直接消费宏，seq package通过同值localparam和typedef消费；
runtime plus无权改变这些elaboration前结构。
```

### 3.3 Consumer 覆盖

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_interface.sv`。
interface 的职责是定义 driver 与 connect 共享的 V2 请求载体；它不保存运行期状态。

```systemverilog
logic [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_fuType;
logic [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_uopIdx;
logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value;
logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value;
logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value;
logic [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_numLsElem;
```

中文伪代码：

```text
interface 对每个slot声明同一组request字段；
FuType、uopIdx、ROB/LQ/SQ value和numLsElem都直接消费compile宏；
slot0到slot5采用相同声明，不保留固定7-bit/5-bit或第二套宽度权威；
这些声明只决定elaboration后的VIF尺寸，不修改queue、status或pointer。
```

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_monitor.sv`，
task：`mon_data()`。monitor 本轮仍保持既有“不发布 analysis item”边界，只同步 local field 和 X/Z 检查宽度。

```systemverilog
logic [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_uopIdx;
logic [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_numLsElem;
`TCNT_CHECK_SIG_XZ(io_ooo_to_mem_enqLsq_req_0_bits_uopIdx,
                   io_ooo_to_mem_enqLsq_req_0_bits_uopIdx,
                   `MEMBLOCK_DUT_UOP_IDX_W);
`TCNT_CHECK_SIG_XZ(io_ooo_to_mem_enqLsq_req_0_bits_numLsElem,
                   io_ooo_to_mem_enqLsq_req_0_bits_numLsElem,
                   `MEMBLOCK_DUT_NUM_LS_ELEM_W);
```

中文伪代码：

```text
monitor按interface同宽度声明采样临时变量；
每拍把VIF值读入临时变量后，X/Z checker使用同一compile宏检查真实字段宽度；
slot1到slot5执行相同检查；
本轮不创建monitor transaction、不调用analysis port write，也不改变RM/scoreboard生命周期。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，参数和类型定义。
该定义组把compile宏引入seq package，并建立`numLsElem`的唯一seq内类型。

```systemverilog
localparam int unsigned MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM = `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM;
localparam int unsigned MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH = `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH;
localparam int unsigned MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH = `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH;
typedef bit [MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] memblock_num_ls_elem_t;
```

中文伪代码：

```text
seq package把物理slot数和load/store宽度暴露为只读localparam；
numLsElem类型只由compile宽度派生；
后续main transaction、behavior和issue item均消费该类型，不建立第二套固定5-bit权威。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/main_control_transaction.sv`，字段：
`numLsElem`和constraint：`c_scalar_lsq_elem`。该字段保存每个uid的主表element数。

```systemverilog
rand memblock_num_ls_elem_t numLsElem;
constraint c_scalar_lsq_elem {
    numLsElem inside {[0:MEMBLOCK_DUT_MAX_LS_ELEM]};
}
```

中文伪代码：

```text
main transaction用公共类型保存主表element数；
随机化时把通用合法范围限制在0到硬件最大element数；
0继续表示初始化或非LSQ项，本轮scalar request setter进一步只接受1。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，
function：`apply_minimal_op_template()`/`apply_op_class_template()`。自动主表producer只做机械类型迁移。

```systemverilog
tr.numLsElem = memblock_num_ls_elem_t'(1); // scalar load/store/prefetch/CBO
tr.numLsElem = memblock_num_ls_elem_t'(0); // MOU/AMO或初始化语义
```

中文伪代码：

```text
自动主表producer保持原来的op class选择和0/1语义；
只把固定5-bit literal改成公共类型cast；
不改变主表生成概率、地址模板、FuType/fuOpType或validation控制流。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv`，
function：`make_real_mixed_transaction()`。手动主表的scalar load/store继续写element数1。

```systemverilog
tr.numLsElem = memblock_num_ls_elem_t'(1);
```

中文伪代码：

```text
手动主表根据既有op_class选择load或store；
两类scalar操作都把numLsElem写为公共类型的1；
该修改不改变手动transaction的FuType、fuOpType、地址或ROB key。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`，
function：`make_smoke_transaction()`。software-only smoke producer继续使用相同scalar element语义。

```systemverilog
tr.numLsElem = memblock_num_ls_elem_t'(1);
```

中文伪代码：

```text
smoke producer保持原来的load/store选择；
把numLsElem的固定5-bit literal替换为公共类型的1；
该测试构造路径的其它字段和控制流保持不变。
```

### 3.4 正确性检查

`seq_csr_common::check_compile_param_consistency()` 检查派生公式、非零值、6/4不超过总slot、当前V2物理
展开严格为6/6/4，以及FuType bit不越界/不重复。ROB/LQ/SQ key typedef仍从compile width派生，合法
value额外受真实resource size约束。

## 4. Scalar Request 合同

### 4.1 修改前逻辑

旧setter把 `uid[6:0]` 当成 `uopIdx`，caller分散传raw字段；active和idle没有共同的V2 extra字段写者。
xaction slot1..5部分约束为空，inactive slot也没有保证payload为0。

### 4.2 修改后逻辑

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`set_req_fields()`。该函数只构造当前
slot，不写公共状态。

```systemverilog
if (tr == null) begin
    `uvm_fatal(get_type_name(), "set_req_fields got null xaction")
end
if (slot >= MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
    `uvm_fatal(get_type_name(),
               $sformatf("set_req_fields slot=%0d exceeds compile-time slot count=%0d",
                         slot,
                         MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM))
end
default_behavior = lsq_ctrl_model::make_default_behavior();
if (valid) begin
    if (main_tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("active LSQ slot=%0d got null main transaction", slot))
    end
    if (behavior.num_ls_elem != memblock_num_ls_elem_t'(1) ||
        main_tr.numLsElem != memblock_num_ls_elem_t'(1) ||
        !(behavior.need_alloc inside {2'b01, 2'b10}) ||
        (behavior.need_alloc == 2'b01 && (!behavior.uses_lq || behavior.uses_sq)) ||
        (behavior.need_alloc == 2'b10 && (behavior.uses_lq || !behavior.uses_sq))) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("slot=%0d violates scalar LSQ behavior: needAlloc=%0b uses_lq/sq=%0b/%0b main/behavior numLsElem=%0d/%0d",
                             slot,
                             behavior.need_alloc,
                             behavior.uses_lq,
                             behavior.uses_sq,
                             main_tr.numLsElem,
                             behavior.num_ls_elem))
    end
    dut_futype = encode_and_fit_dut_futype(
        main_tr.fuType,
        $sformatf("%s::set_req_fields(slot=%0d)", get_type_name(), slot));
    rob_key = main_tr.get_rob_key();
    uop_idx = '0;
    num_ls_elem = behavior.num_ls_elem;
    fu_op_type = main_tr.fuOpType;
    last_uop = 1'b1;
end else begin
    if (main_tr != null || behavior != default_behavior ||
        lq_key.flag || lq_key.value != '0 ||
        sq_key.flag || sq_key.value != '0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("idle slot=%0d requires null main transaction, default behavior, and zero keys",
                             slot))
    end
    dut_futype = '0;
    rob_key = '{default:'0};
    uop_idx = '0;
    num_ls_elem = '0;
    fu_op_type = '0;
    last_uop = 1'b0;
end
```

中文伪代码：

```text
setter入口先检查xaction非空且slot位于compile slot范围内，失败时受控fatal；
获取完整默认behavior作为idle合同；
active分支检查main transaction存在，main/behavior的numLsElem都为1；
检查needAlloc=01只使用LQ、needAlloc=10只使用SQ；
调用FuType helper做V2无损one-hot编码；
ROB key直接从main transaction读取，LQ/SQ key使用candidate预览结果；
固定uopIdx=0、lastUop=1，复制fuOpType，并把exceptionVec/trigger/flushPipe清零；
idle分支要求main transaction为空、behavior完整等于默认值且LQ/SQ key全零，再清空全部slot字段；
本函数只写当前xaction slot，不修改主表、状态表、map、pointer或free count。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，function：
`validate_v2_scalar_item()`。该函数在任何VIF赋值前执行。

```systemverilog
if (!valid) begin
    if (need_alloc != 2'b00 || fu_type != '0 || uop_idx != '0 ||
        rob_idx_flag || rob_idx_value != '0 ||
        lq_idx_flag || lq_idx_value != '0 ||
        sq_idx_flag || sq_idx_value != '0 || num_ls_elem != '0 ||
        exception_vec != '0 || trigger != '0 || fu_op_type != '0 ||
        flush_pipe || last_uop) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("inactive slot=%0d must have zero qualifier and payload", slot))
    end
end
```

中文伪代码：

```text
逐slot读取flat xaction字段；
inactive slot只要任一qualifier或payload非0就在首次VIF写入前fatal；
active slot继续检查needAlloc/FuType匹配、key value范围和scalar固定字段；
全部slot通过后send_pkt才开始搬运信号，因此不存在部分驱动后再报错。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`。两组宏值表是本轮agent opcode
集合的唯一数值权威；它们属于协议语义，不是可配置硬件结构参数。

```systemverilog
`define MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES [9'd0:9'd6], 9'd8, 9'd9, 9'd10
`define MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES [9'd0:9'd3]
```

中文伪代码：

```text
第一组值表列出普通load 0到6和software prefetch 8、9、10；
第二组值表列出普通store 0到3；
constraint和driver checker都展开同一值表，避免维护两套opcode数字。
```

源码位置：同文件，static function：`is_supported_v2_load_or_prefetch_fuoptype()`。该无状态helper供
driver对directed item做运行期复核。

```systemverilog
static function bit is_supported_v2_load_or_prefetch_fuoptype(input bit [8:0] fu_op_type);
    return fu_op_type inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
endfunction:is_supported_v2_load_or_prefetch_fuoptype
```

中文伪代码：

```text
helper接收9-bit fuOpType；
普通load 0到6或software prefetch 8、9、10返回真；
CBO、AMO和其它值返回假；
函数只读取入参，无transaction或公共状态副作用。
```

源码位置：同文件，static function：`is_supported_v2_store_fuoptype()`。该无状态helper定义本轮SQ
支持的普通store opcode集合。

```systemverilog
static function bit is_supported_v2_store_fuoptype(input bit [8:0] fu_op_type);
    return fu_op_type inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
endfunction:is_supported_v2_store_fuoptype
```

中文伪代码：

```text
helper接收9-bit fuOpType；
普通store 0到3返回真；
CBO 7/12/13/14、AMO和其它值返回假；
函数只读取入参，无transaction或公共状态副作用。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，constraint：
`c_v2_scalar_request_contract`。constraint 为随机 item 建立与 directed setter 相同的 scalar 合同。

```systemverilog
if (!io_ooo_to_mem_enqLsq_req_0_valid) {
    io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b00;
    io_ooo_to_mem_enqLsq_req_0_bits_fuType == '0;
    io_ooo_to_mem_enqLsq_req_0_bits_uopIdx == '0;
    io_ooo_to_mem_enqLsq_req_0_bits_numLsElem == '0;
    io_ooo_to_mem_enqLsq_req_0_bits_fuOpType == '0;
    io_ooo_to_mem_enqLsq_req_0_bits_lastUop == '0;
} else {
    io_ooo_to_mem_enqLsq_needAlloc_0 inside {2'b01, 2'b10};
    (io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b01) ->
        io_ooo_to_mem_enqLsq_req_0_bits_fuOpType inside
            {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
    (io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) ->
        io_ooo_to_mem_enqLsq_req_0_bits_fuOpType inside
            {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
    io_ooo_to_mem_enqLsq_req_0_bits_uopIdx == '0;
    io_ooo_to_mem_enqLsq_req_0_bits_numLsElem == 1;
    io_ooo_to_mem_enqLsq_req_0_bits_lastUop == 1'b1;
}
```

中文伪代码：

```text
随机化slot0时先按valid选择合同；
inactive时要求needAlloc和全部payload为0；
active LQ直接约束fuOpType落在load/prefetch宏值表，active SQ直接约束落在store宏值表；
不在constraint中调用checker函数，因此VCS可以反向求解随机fuOpType；
active时固定uopIdx=0、numLsElem=1和lastUop=1；
同一constraint对slot1到slot5重复相同字段集合；
directed赋值仍由driver的validate_v2_scalar_item做最终兜底。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，function：
`validate_v2_scalar_item()`的active LQ opcode分支。该分支覆盖关闭constraint或手工赋值的producer。

```systemverilog
if (!lsqenq_agent_agent_xaction::is_supported_v2_load_or_prefetch_fuoptype(fu_op_type)) begin
    `uvm_fatal(get_type_name(),
               $sformatf("load/prefetch slot=%0d has unsupported fuOpType=0x%0h",
                         slot, fu_op_type))
end
```

中文伪代码：

```text
active LQ slot调用共享helper复核load/prefetch opcode；
不支持值在首次VIF赋值前fatal，directed item不能绕过；
合法值继续进入load batch计数、key范围和scalar固定字段检查。
```

源码位置：同文件同function，active SQ opcode分支。该分支对普通store执行同一时点的复核。

```systemverilog
if (!lsqenq_agent_agent_xaction::is_supported_v2_store_fuoptype(fu_op_type)) begin
    `uvm_fatal(get_type_name(),
               $sformatf("store slot=%0d has unsupported fuOpType=0x%0h",
                         slot, fu_op_type))
end
```

中文伪代码：

```text
active SQ slot调用共享helper复核普通store opcode；
不支持值在首次VIF赋值前fatal；
合法值继续进入store batch计数、key范围和scalar固定字段检查。
```

### 4.3 V2 Extra 字段与 Framework Metadata 打印和比较闭环

归档前独立 review 第 1 轮发现，extra 字段虽然已进入 UVM field automation，但旧自定义 compare 在
`super.compare()` 失败后把结果重新置 1，只手工复查旧公共字段，因此可能忽略仅 extra 字段不同的 item；
旧 `psdisplay()` 也没有展示 extra 字段。第8轮进一步发现手工回退只比较launch/abort，遗漏
`wait_can_accept/ready_timeout/flush_epoch`。修复不改变driver或DUT数据，只补齐transaction观察和比较语义。

源码位置：`lsqenq_agent_agent_xaction.sv`，类型与 helper 声明。packed struct 是单个 slot extra 字段的
只读快照，helper 不修改 transaction。

```systemverilog
typedef struct packed {
    bit [23:0] exception_vec;
    bit [3:0]  trigger;
    bit [8:0]  fu_op_type;
    bit        flush_pipe;
    bit        last_uop;
} v2_extra_fields_t;

extern function v2_extra_fields_t get_v2_extra_fields(input int unsigned slot);
```

中文伪代码：

```text
定义一个packed快照保存单个slot的五组V2 extra字段；
get_v2_extra_fields按slot0到slot5读取flat xaction成员并返回快照；
slot越界时fatal，避免display或compare静默跳过不存在的slot；
helper只读当前transaction，不改变随机字段、metadata或公共状态。
```

源码位置：同文件，function：`get_v2_extra_fields()`。以下完整函数证明helper实际覆盖slot 0至5，
每一路都把同slot的24-bit exception、trigger、`fuOpType`、`flushPipe`和`lastUop`聚合到只读快照；
slot越界在返回前fatal。

```systemverilog
function lsqenq_agent_agent_xaction::v2_extra_fields_t
lsqenq_agent_agent_xaction::get_v2_extra_fields(input int unsigned slot);
    v2_extra_fields_t fields;

    fields = '0;
    case (slot)
        0: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_0_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_0_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_0_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_0_bits_lastUop;
        end
        1: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_1_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_1_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_1_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_1_bits_lastUop;
        end
        2: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_2_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_2_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_2_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_2_bits_lastUop;
        end
        3: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_3_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_3_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_3_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_3_bits_lastUop;
        end
        4: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_4_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_4_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_4_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_4_bits_lastUop;
        end
        5: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_5_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_5_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_5_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_5_bits_lastUop;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported V2 LSQ enqueue slot=%0d", slot))
        end
    endcase
    return fields;
endfunction:get_v2_extra_fields
```

中文伪代码：

```text
本函数负责把flat展开的单slot V2 extra字段聚合成统一快照，供display和custom compare共同读取；
入口先把返回快照清零，再按slot值进入0、1、2、3、4、5六个明确分支；
每个分支只读取该slot自己的exceptionVec[23:0]、trigger、fuOpType、flushPipe和lastUop，不跨slot取值；
slot不在0至5时立即fatal，防止compile循环和显式字段面分叉后静默漏比较；
合法分支结束后返回快照，不修改xaction字段、driver、VIF或公共状态。
```

源码位置：同文件，function：`psdisplay()`的metadata摘要。该赋值打印全部五个framework控制字段。

```systemverilog
pkt_str = $sformatf(
    "%swait_can_accept=%0b ready_timeout=%0d request_launched=%0b aborted_by_redirect=%0b flush_epoch=%0d ",
    pkt_str,
    this.memblock_dispatch_wait_can_accept,
    this.memblock_dispatch_ready_timeout,
    this.memblock_dispatch_request_launched,
    this.memblock_dispatch_aborted_by_redirect,
    this.memblock_dispatch_flush_epoch);
```

中文伪代码：

```text
psdisplay先打印wait、timeout、launch、abort和flush epoch五个framework metadata；
这些字段不连接DUT，但决定V2 launch和stale epoch解释；
随后再打印每个slot的DUT payload，日志可以同时还原控制合同和接口值。
```

源码位置：同文件，function：`psdisplay()`的extra循环。该循环在原公共字段输出前增加六个 slot 的可读
extra摘要。

```systemverilog
for (int unsigned slot = 0; slot < `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; slot++) begin
    fields = get_v2_extra_fields(slot);
    pkt_str = $sformatf(
        "%sslot%0d_v2_extra={exceptionVec=0x%0h trigger=0x%0h fuOpType=0x%0h flushPipe=%0b lastUop=%0b} ",
        pkt_str,
        slot,
        fields.exception_vec,
        fields.trigger,
        fields.fu_op_type,
        fields.flush_pipe,
        fields.last_uop);
end
```

中文伪代码：

```text
psdisplay遍历当前compile profile的六个slot；
每个slot调用只读helper取得extra字段；
把exceptionVec、trigger、fuOpType、flushPipe和lastUop追加到packet字符串；
随后继续执行原公共needAlloc、valid、key和numLsElem打印。
```

源码位置：同文件，function：`compare()`的metadata分支。该分支位于旧手工compare回退路径内，保留
原先忽略base timestamp差异的行为，同时完整复查本类五个framework控制字段。

```systemverilog
if (this.memblock_dispatch_wait_can_accept != rhs_.memblock_dispatch_wait_can_accept ||
    this.memblock_dispatch_ready_timeout != rhs_.memblock_dispatch_ready_timeout ||
    this.memblock_dispatch_request_launched != rhs_.memblock_dispatch_request_launched ||
    this.memblock_dispatch_aborted_by_redirect != rhs_.memblock_dispatch_aborted_by_redirect ||
    this.memblock_dispatch_flush_epoch != rhs_.memblock_dispatch_flush_epoch) begin
    super_result = 0;
    `uvm_info(get_type_name(), "compare fail for dispatch metadata", UVM_NONE)
end
```

中文伪代码：

```text
UVM automation报告差异后，项目既有回退路径继续忽略base timestamp等非payload字段；
单个条件同时复查wait、timeout、launch、abort和flush epoch；
任一framework metadata不同都把结果保持为失败；
随后继续比较六个slot的extra和公共payload。
```

源码位置：同文件，function：`compare()`的extra循环。该循环逐slot比较V2 extra字段。

```systemverilog
for (int unsigned slot = 0; slot < `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; slot++) begin
    lhs_fields = this.get_v2_extra_fields(slot);
    rhs_fields = rhs_.get_v2_extra_fields(slot);
    if (lhs_fields != rhs_fields) begin
        super_result = 0;
        `uvm_info(get_type_name(),
                  $sformatf("compare fail for slot%0d V2 extra fields: this=0x%0h rhs=0x%0h",
                            slot,
                            lhs_fields,
                            rhs_fields),
                  UVM_NONE)
    end
end
```

中文伪代码：

```text
当UVM自动compare报告任一差异时，旧回退路径继续逐项复查有效payload；
新增循环分别读取左右transaction每个slot的extra快照；
任一extra字段不同就保持compare结果为失败，并打印slot及左右packed值；
循环后继续执行原公共字段手工比较，最终任一有效payload不同都会返回0。
```

### 4.4 正确性检查

xaction constraint、sequence setter和driver validation使用同一合同。直接赋值可绕过random constraint，但不能绕过
driver pre-drive检查；随机item和directed item都不能把不支持opcode或残留payload送入DUT。extra字段和
五个framework metadata同时进入UVM automation、自定义display和自定义compare回退，不再存在
“能驱动/影响launch解释但不能观察或比较”的断链。

## 5. Candidate Gate 和随机0

### 5.1 修改前逻辑

旧candidate只限制公共总slot和free count，可能形成5/6个store的packet。随机模式使用
`$urandom_range(MAX,1)`，不能产生主动idle，也不能控制0/中间/MAX类别概率。

### 5.2 修改后逻辑

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`collect_lsq_candidates()`。

```systemverilog
max_enq = seq_csr_common::get_enq_per_cycle();
if (max_enq == 0) return 1'b0;
tentative_load = load_elem_count + (behavior.uses_lq ? behavior.num_ls_elem : 0);
tentative_store = store_elem_count + (behavior.uses_sq ? behavior.num_ls_elem : 0);
if (tentative_load > MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH ||
    tentative_store > MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH ||
    tentative_load > lq_free_tmp || tentative_store > sq_free_tmp) break;
```

中文伪代码：

```text
每拍只采样一次总slot目标；
目标为0时在读取next uid和LSQ pointer/free前返回空candidate；
非零时从next-admit uid开始只扫描连续前缀，最大6项；
每项要求scalar numLsElem=1，并分别累计load/store element；
累计超过6/4或实际free count时停止，不消费当前uid；
只推进局部pointer，driver launch前不改公共状态。
```

源码位置：`seq_csr_common.sv`，function：`get_enq_per_cycle()`。

```systemverilog
if (!std::randomize(sample_class) with {
        sample_class dist {0 := zero_weight, 1 := middle_weight, 2 := max_weight};
    }) begin
    `uvm_fatal("SEQ_CSR_CFG", "failed to randomize LSQ enqueue ZERO/MIDDLE/MAX class")
end
case (sample_class)
    0: return 0;
    1: std::randomize(middle_value) with {
           middle_value inside {[1:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1]};
       };
    2: return MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM;
endcase
```

中文伪代码：

```text
固定模式直接返回配置的1..MAX值；
随机模式先按三类总权重选择ZERO/MIDDLE/MAX；
ZERO返回0，MAX返回物理slot数，MIDDLE再在1..MAX-1均匀采样；
任一randomize失败立即fatal，不混用第二套随机算法。
```

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`。三个新增 plus 只控制随机类别权重，不描述硬件结构。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT, int, -1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT, int, 1)

load_int("MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT", MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT);
load_int("MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT", MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT);
load_int("MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT", MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT);
```

中文伪代码：

```text
plus package为ZERO、MIDDLE和MAX三类分别建立signed int入口；
默认ZERO为0、MIDDLE为-1 AUTO、MAX为1；
load_all从命令行或cfg读取三个值；
这些值只进入seq_csr_common的行为随机化，不改变slot数、LQ/SQ深度或interface尺寸。
```

配置位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`。默认 preset 明确保存 raw 权重：

```text
+MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT=0
+MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT=-1
+MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT=1
```

`MIDDLE=-1` 在 `seq_csr_common` 内解析为 `SLOT_NUM-1`，V2 即 5；因此默认类别总权重为 0/5/1，
再把 MIDDLE 的 5 份均匀分给 1..5，最终仍保持旧 1..6 各占 1/6。

源码位置：`seq/base_seq_help/seq_csr_common.sv`，function：`apply_runtime_resource_limits()`。
该函数是 raw 权重到 effective 权重的唯一收敛点。

```systemverilog
if (enq_per_cycle_middle_weight == -1) begin
    enq_per_cycle_effective_middle_weight = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM - 1;
end else begin
    enq_per_cycle_effective_middle_weight = enq_per_cycle_middle_weight;
end
enq_weight_sum = enq_per_cycle_zero_weight;
enq_weight_sum += enq_per_cycle_effective_middle_weight;
enq_weight_sum += enq_per_cycle_max_weight;
if (enq_weight_sum == 0)
    `uvm_fatal("SEQ_CSR_CFG", "LSQ enqueue ZERO/MIDDLE/MAX weights must not all be zero")
if (enq_per_cycle_effective_middle_weight == 0 &&
    enq_per_cycle_max_weight == 0)
    `uvm_fatal("SEQ_CSR_CFG", "LSQ enqueue random weights cannot select ZERO forever")
```

中文伪代码：

```text
先拒绝小于-1的MIDDLE raw值；
raw值为-1时按物理slot数减1派生effective MIDDLE，否则使用显式非负值；
在64-bit unsigned变量中逐项累加三类权重，避免32-bit表达式先溢出；
三类全0时fatal，因为随机solver没有合法类别；
MIDDLE和MAX同时为0时fatal，因为主动flow会永久只发ZERO idle而无法完成。
```

### 5.3 Agent 通用路径的 6/4 Gate

归档前第7轮review确认dispatch candidate本身已正确限制6/4，但通用default sequence直接随机化xaction，
原实现仍可生成5/6个store。修复后，xaction负责标准随机路径，driver负责所有producer的最终协议兜底。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，
constraint：`c_v2_batch_enqueue_width`。该约束统计六个valid slot的资源类型，不读取主表或软件free count。

```systemverilog
constraint c_v2_batch_enqueue_width {
    int'(io_ooo_to_mem_enqLsq_req_0_valid && io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b01) +
    int'(io_ooo_to_mem_enqLsq_req_1_valid && io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b01) +
    int'(io_ooo_to_mem_enqLsq_req_2_valid && io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b01) +
    int'(io_ooo_to_mem_enqLsq_req_3_valid && io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b01) +
    int'(io_ooo_to_mem_enqLsq_req_4_valid && io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b01) +
    int'(io_ooo_to_mem_enqLsq_req_5_valid && io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b01)
        <= `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH;

    int'(io_ooo_to_mem_enqLsq_req_0_valid && io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) +
    int'(io_ooo_to_mem_enqLsq_req_1_valid && io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b10) +
    int'(io_ooo_to_mem_enqLsq_req_2_valid && io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b10) +
    int'(io_ooo_to_mem_enqLsq_req_3_valid && io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b10) +
    int'(io_ooo_to_mem_enqLsq_req_4_valid && io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b10) +
    int'(io_ooo_to_mem_enqLsq_req_5_valid && io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b10)
        <= `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH;
}
```

中文伪代码：

```text
该约束在default sequence随机化整个LSQ enqueue item时执行；
逐个判断六个slot是否同时满足valid且needAlloc为LQ，把命中数累加为load batch数量；
要求load数量不超过当前compile profile的load enqueue width；
再以相同方式统计needAlloc为SQ的valid slot，并要求store数量不超过store enqueue width；
inactive slot不计入任何类别，逐slot scalar约束仍负责要求其payload全零；
因此V2随机item最多形成6个load或4个store，不依赖dispatch candidate路径。
```

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，
function：`validate_v2_scalar_item()`。该函数在`send_pkt()`首次写VIF前调用，防止directed item或关闭约束
的producer绕过xaction合同。

```systemverilog
case (need_alloc)
    2'b01: begin
        if (fu_type != load_fu_type) begin
            `uvm_fatal(get_type_name(), $sformatf("load slot=%0d has FuType=0x%0h", slot, fu_type))
        end
        load_count++;
        if (load_count > `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("V2 LSQ batch load count=%0d exceeds width=%0d",
                                 load_count, `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH))
        end
    end
    2'b10: begin
        if (fu_type != store_fu_type) begin
            `uvm_fatal(get_type_name(), $sformatf("store slot=%0d has FuType=0x%0h", slot, fu_type))
        end
        store_count++;
        if (store_count > `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("V2 LSQ batch store count=%0d exceeds width=%0d",
                                 store_count, `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH))
        end
    end
    default: begin
        `uvm_fatal(get_type_name(),
                   $sformatf("active slot=%0d has illegal needAlloc=%0b", slot, need_alloc))
    end
endcase
```

中文伪代码：

```text
driver进入六slot循环前把load_count和store_count清零；
inactive slot先执行全零检查，然后continue，不计入batch；
active slot按needAlloc进入load或store分支，同时检查FuType与该资源类型一致；
每命中一个load或store就递增对应计数，第一次超过compile 6/4上限时立即fatal；
非法needAlloc也立即fatal；
只有batch计数、key范围和其余scalar字段全部通过后，send_pkt才开始写VIF。
```

### 5.4 正确性检查

默认 `0/5/1` 保持旧1..6均匀分布。随机模式用64-bit逐项累加并拒绝全0及`MIDDLE+MAX=0`；高ZERO
权重测试中issue延迟约1.7us后仍完成terminal，zero-only测试在0ns按预期fatal。dispatch candidate、
xaction约束和driver复核分别覆盖主流程、标准随机路径和任意directed producer；xaction还把继承的
pre/post gap都收紧为0，不再让default random item命中driver的streaming fatal。

## 6. Clock-First Streaming

### 6.1 修改前逻辑

旧driver可能进入不存在完成条件的ready等待并重复发送同一request。若每个item内部额外等待sample边界，
稳态又只能两拍一批。

### 6.2 修改后逻辑

源码位置：`lsqenq_agent_agent_driver.sv`，task：`main_phase()`。

```systemverilog
task lsqenq_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            req = null;
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                bit active_request;

                if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("V2 LSQ streaming requires pre/post gap 0, got %0d/%0d",
                                         req.pre_pkt_gap,
                                         req.post_pkt_gap))
                end
                req.memblock_dispatch_request_launched = 1'b0;
                req.memblock_dispatch_aborted_by_redirect = 1'b0;
                active_request = has_active_request(req);
                if (active_request &&
                    (memblock_sync_pkg::dispatch_flush_in_progress ||
                     memblock_sync_pkg::dispatch_flush_epoch != req.memblock_dispatch_flush_epoch)) begin
                    req.memblock_dispatch_aborted_by_redirect = 1'b1;
                    this.drive_idle(this.cfg.drv_mode);
                end else begin
                    this.send_pkt(req);
                    req.memblock_dispatch_request_launched = active_request;
                end
                seq_item_port.item_done();
            end
            else begin
                this.drive_idle(this.cfg.drv_mode);
            end
        end
    end
    else if (this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            `uvm_fatal(get_type_name(), $sformatf("sqr_sw==OFF & drv_sw==ON, please give a driver send task!"))
        end
    end
endtask:main_phase
```

中文伪代码：

```text
每轮先经过clocking边界，使DUT采样上一轮VIF值；
清旧req句柄后最多取得一个新item；
无item时驱动全零idle；
有item时先拒绝非零pre/post gap，再把本轮launch和abort metadata清零；
active item在launch前检查flush和epoch，失效时标记abort且不发送；
合法item只send一次，active时记录request_launched；
立即item_done，不在本item内等待下一边界，也不立即撤销active request。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，function：`build_phase()`。
该函数在driver进入reset/main phase前固定V2 idle驱动合同，不读取或修改运行期公共状态。

```systemverilog
function void lsqenq_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (cfg.drv_sw == tcnt_dec_base::ON && cfg.drv_mode != tcnt_dec_base::DRV_0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("V2 LSQ enqueue active driver requires DRV_0 idle mode, got %0d",
                             cfg.drv_mode))
    end
endfunction:build_phase
```

中文伪代码：

```text
driver build阶段先完成父类配置初始化；
如果driver启用，则要求最终drv_mode精确为DRV_0；
启用driver却选择其它idle模式时，在任何reset或main-phase VIF赋值前fatal并打印实际mode；
driver关闭时不限制该无消费者配置，本函数不改变sequence、transaction或公共状态。
```

源码位置：同文件，function：`has_active_request()`。该无状态helper把六个request valid归并为本item是否
包含真实active request，返回值只供`main_phase()`决定redirect gate和launch metadata。

```systemverilog
function bit lsqenq_agent_agent_driver::has_active_request(lsqenq_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "has_active_request got null transaction")
    end
    return tr.io_ooo_to_mem_enqLsq_req_0_valid ||
           tr.io_ooo_to_mem_enqLsq_req_1_valid ||
           tr.io_ooo_to_mem_enqLsq_req_2_valid ||
           tr.io_ooo_to_mem_enqLsq_req_3_valid ||
           tr.io_ooo_to_mem_enqLsq_req_4_valid ||
           tr.io_ooo_to_mem_enqLsq_req_5_valid;
endfunction:has_active_request
```

中文伪代码：

```text
本函数判断当前item是active request还是显式idle boundary；
空transaction表示driver内部合同损坏，立即fatal，不能把它误判为idle；
合法transaction依次对slot0至slot5的valid做逻辑或，任一路valid即返回1，全部为0才返回0；
main_phase用返回值决定是否检查redirect/epoch，并只对active item设置request_launched。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`。LSQ xaction必须把基类允许的
`post_pkt_gap=0..50`收紧为V2 streaming唯一合法值0。

```systemverilog
constraint v2_streaming_gap_cons {
    pre_pkt_gap == 0;
    post_pkt_gap == 0;
}
```

中文伪代码：

```text
LSQ xaction随机化时同时要求发送前和发送后gap为0；
因此通用default random sequence生成的每个item都满足单拍连续streaming合同；
driver仍在首次VIF赋值前检查实际值，拦截关闭约束或randomize后手工改写的directed item。
```

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_cfg.sv`。cfg 通过 soft constraint 给 V2
active driver 选择全零 idle 模式，仍允许上层在 randomize 前约束，但 driver 会拒绝非零模式。

```systemverilog
constraint c_v2_idle_drive_mode {
    soft drv_mode == tcnt_dec_base::DRV_0;
}
```

中文伪代码：

```text
随机化LSQ enqueue agent cfg时默认选择DRV_0；
DRV_0表示reset、无item和redirect abort都把全部request字段清零；
driver build_phase再次检查实际配置，非DRV_0时fatal；
该cfg不改变sequence item数量、DUT采样时序或公共状态。
```

源码位置：`seq/base_seq_help/seq_csr_common.sv`，function：`load_from_plus()`。公共参数加载层保留
兼容参数入口，并在所有 profile 下统一检查输入非负；这不等于 V2 LSQ sequence 消费该 timeout。

```systemverilog
lsqenq_ready_timeout = get_non_negative_int(
    "MEMBLOCK_LSQENQ_READY_TIMEOUT",
    plus::MEMBLOCK_LSQENQ_READY_TIMEOUT);
```

中文伪代码：

```text
公共参数快照从plus层读取MEMBLOCK_LSQENQ_READY_TIMEOUT；
调用get_non_negative_int检查该值不小于0，负值立即fatal；
合法值只保存到公共快照，是否用于ready等待由具体profile和sequence决定；
当前V2 sequence不调用timeout getter，因此该值不会进入driver时序。
```

源码位置：`seq/base_seq_help/seq_csr_common.sv`，function：`validate_and_clamp()`。V2 没有
accept-response capability，因此不修改无消费者的 ready timeout。

```systemverilog
if (MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP &&
    lsqenq_seq_en && lsqenq_ready_timeout == 0) begin
    `uvm_warning("SEQ_CSR_CFG",
                 "lsqenq_ready_timeout=0 while lsqenq sequence is enabled, clamp to 1")
    lsqenq_ready_timeout = 1;
end
```

中文伪代码：

```text
load_from_plus已经完成公共参数的非负检查；
validate只在compile capability声明存在accept-response时检查timeout是否为0；
该接口版本启用sequence且timeout为0时才warning并clamp到1；
当前V2 capability为0，因此公共层仍解析并检查非负，但不做零值warning/clamp；
V2 sequence不读取该getter；
V2 driver主循环也不调用wait_lsq_can_accept或sample_lsqenq_resp。
```

### 6.3 正确性检查

连续A/B/C可在相邻边界launch，单笔仍有launch到sample的一拍延迟。cfg soft默认`DRV_0`且driver build
阶段拒绝其它mode，reset/no-item/abort不会产生随机valid/X。

## 7. Reservation 与 Pending Sample

### 7.1 修改前逻辑

旧confirm把预测key伪装成response，并在driver返回后同时allocation和开放issue。V2当前request尚未经过
下一DUT sample边界，立即issue可能早一拍；若把allocation也延后，下一batch又会重复使用旧pointer。

### 7.2 修改后逻辑

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`confirm_lsq_candidates()`。

```systemverilog
function void memblock_lsqenq_dispatch_base_sequence::confirm_lsq_candidates(input lsqenq_agent_agent_xaction tr,
                                                                        input memblock_uid_t uids[$],
                                                                        input main_control_transaction trs[$],
                                                                        input memblock_op_behavior_t behaviors[$],
                                                                        input memblock_lq_key_t lq_keys[$],
                                                                        input memblock_sq_key_t sq_keys[$],
                                                                        inout bit has_progress);
    if (!tr.memblock_dispatch_request_launched) begin
        if (!tr.memblock_dispatch_aborted_by_redirect &&
            !admission_blocked_by_flush() &&
            tr.memblock_dispatch_flush_epoch == memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal(get_type_name(), "active LSQ candidate returned without launch or redirect abort")
        end
        return;
    end
    if (tr.memblock_dispatch_aborted_by_redirect) begin
        `uvm_fatal(get_type_name(), "LSQ transaction cannot be both launched and aborted before launch")
    end
    if (admission_blocked_by_flush() ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        `uvm_info(get_type_name(), "skip LSQ enqueue confirmation because redirect/flush is in progress", UVM_LOW)
        return;
    end
    if (pending_sample_valid) begin
        `uvm_fatal(get_type_name(), "cannot reserve current LSQ batch before completing previous sample")
    end
    if (uids.size() == 0 || uids.size() != trs.size() ||
        uids.size() != behaviors.size() || uids.size() != lq_keys.size() ||
        uids.size() != sq_keys.size()) begin
        `uvm_fatal(get_type_name(), "LSQ candidate queues are empty or have inconsistent sizes")
    end
    foreach (uids[idx]) begin
        memblock_lq_key_t expected_lq_key;
        memblock_sq_key_t expected_sq_key;

        lsq_ctrl.preview_allocate(behaviors[idx], expected_lq_key, expected_sq_key);
        if (behaviors[idx].uses_lq && expected_lq_key != lq_keys[idx]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d LQ preview drift: expected={%0d,%0d} candidate={%0d,%0d}",
                                 uids[idx], expected_lq_key.flag, expected_lq_key.value,
                                 lq_keys[idx].flag, lq_keys[idx].value))
        end
        if (behaviors[idx].uses_sq && expected_sq_key != sq_keys[idx]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d SQ preview drift: expected={%0d,%0d} candidate={%0d,%0d}",
                                 uids[idx], expected_sq_key.flag, expected_sq_key.value,
                                 sq_keys[idx].flag, sq_keys[idx].value))
        end
        lsq_ctrl.commit_allocate(uids[idx], behaviors[idx], trs[idx]);
        pending_sample_uids.push_back(uids[idx]);
        has_progress = 1'b1;
    end
    pending_sample_flush_epoch = tr.memblock_dispatch_flush_epoch;
    pending_sample_launch_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    pending_sample_valid = 1'b1;
endfunction:confirm_lsq_candidates
```

中文伪代码：

```text
未launch且没有redirect/flush/epoch失效原因时fatal；launched与abort同时为1也fatal；
只接受driver已launch、当前epoch有效且上一pending batch已完成的batch；
候选queue为空或长度不一致时在任何allocation前fatal；
逐uid重新preview当前真实pointer，并只比较behavior实际使用的key；
key漂移时在状态修改前fatal；
调用唯一commit_allocate建立active/enq/map并推进pointer/free count；
把整批uid、epoch和launch cycle保存为pending，此处不调用complete_admission。
```

源码位置：同文件，function：`complete_v2_pending_sample()`。

```systemverilog
function void memblock_lsqenq_dispatch_base_sequence::complete_v2_pending_sample(inout bit has_progress);
    if (!pending_sample_valid) begin
        return;
    end
    if (!admission_blocked_by_flush() &&
        pending_sample_flush_epoch == memblock_sync_pkg::dispatch_flush_epoch) begin
        foreach (pending_sample_uids[idx]) begin
            complete_admission(pending_sample_uids[idx]);
            has_progress = 1'b1;
        end
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("discard LSQ pending sample after redirect: launch_cycle=%0d saved_epoch=%0d current_epoch=%0d",
                            pending_sample_launch_cycle,
                            pending_sample_flush_epoch,
                            memblock_sync_pkg::dispatch_flush_epoch),
                  UVM_LOW)
    end
    clear_v2_pending_sample();
endfunction:complete_v2_pending_sample
```

中文伪代码：

```text
没有pending batch时立即返回；
epoch有效且未处于flush时逐uid调用complete_admission，由原issue scheduler设置issue_ready并写issue queue；
每完成一个uid都把本拍progress置1；
epoch失效时只记录丢弃原因，不开放issue，也不在本helper释放LSQ资源；
两条路径最后都调用clear_v2_pending_sample清单深度pending状态，防止重复route。
```

源码位置：同文件，function：`clear_v2_pending_sample()`。该helper是pending状态的唯一清理入口。

```systemverilog
function void memblock_lsqenq_dispatch_base_sequence::clear_v2_pending_sample();
    pending_sample_uids.delete();
    pending_sample_valid = 1'b0;
    pending_sample_flush_epoch = 0;
    pending_sample_launch_cycle = 0;
endfunction:clear_v2_pending_sample
```

中文伪代码：

```text
清空上一批uid队列并撤销pending有效位；
同时清零保存的epoch和launch cycle，避免下一批误用旧metadata。
```

源码位置：同文件，function：`complete_admission()`。该helper复用原有CSR drain和issue route owner。

```systemverilog
function void memblock_lsqenq_dispatch_base_sequence::complete_admission(input memblock_uid_t uid);
    drain_csr_runtime_events();
    issue_sched.prepare_issue_route_for_uid(uid);
endfunction:complete_admission
```

中文伪代码：

```text
先消费最新CSR runtime event，保持后续issue使用当前上下文；
再调用既有issue scheduler为该uid设置issue_ready并尝试写入对应issue queue；
本helper不修改LSQ pointer/free count，也不新建第二个route owner。
```

源码位置：同文件，task：`send_lsqenq_cycle()`。这是每拍LSQ admission的完整主入口。

```systemverilog
task memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle(input int unsigned cycle_idx,
                                                          output bit has_progress);
    lsqenq_agent_agent_xaction tr;
    memblock_uid_t            uids[$];
    main_control_transaction  trs[$];
    memblock_op_behavior_t    behaviors[$];
    memblock_lq_key_t         lq_keys[$];
    memblock_sq_key_t         sq_keys[$];
    bit                       admission_progress;

    has_progress = 1'b0;
    apply_pending_lsq_cancels();
    if (pending_sample_valid) begin
        memblock_uid_t probe_uid;
        main_control_transaction probe_tr;
        memblock_op_behavior_t probe_behavior;

        if (next_uid_needs_lsq_admission(probe_uid, probe_tr, probe_behavior) &&
            probe_behavior.need_alloc == 2'b00) begin
            send_idle_lsqenq_boundary(cycle_idx, "non-LSQ sample boundary", has_progress);
        end
    end
    admission_progress = 1'b0;
    if (admit_non_lsq_if_ready(admission_progress)) begin
        has_progress |= admission_progress;
        return;
    end
    if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
        send_idle_lsqenq_boundary(cycle_idx, "no LSQ candidate", has_progress);
        return;
    end

    tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b0;
    tr.memblock_dispatch_ready_timeout = 0;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_request_launched = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    foreach (uids[idx]) begin
        assign_lsqenq_slot(tr, idx, trs[idx], behaviors[idx], lq_keys[idx], sq_keys[idx]);
    end

    start_item(tr);
    finish_item(tr);
    complete_v2_pending_sample(has_progress);
    confirm_lsq_candidates(tr, uids, trs, behaviors, lq_keys, sq_keys, has_progress);
endtask:send_lsqenq_cycle
```

中文伪代码：

```text
每轮先消费redirect登记的LQ/SQ cancel；
若上一批仍pending且下一uid不分配LSQ，先发送全零idle经过driver边界，完成上一批sample；
随后才调用原non-LSQ admission helper，命中时完成该uid并返回；
否则收集普通LSQ候选；无候选时发送idle边界并返回；
有候选时创建并清空xaction，写入当前epoch、零gap和全部slot payload；
finish_item返回后先完成上一批pending sample，再根据当前item的launch/epoch结果预留当前批。
```

源码位置：同文件，function：`apply_pending_lsq_cancels()`。该helper在任何新admission前消费redirect回退量。

```systemverilog
function void memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    if (data.pending_lq_cancel_count != 0) begin
        lsq_ctrl.cancel_lq(data.pending_lq_cancel_count);
        data.pending_lq_cancel_count = 0;
    end
    if (data.pending_sq_cancel_count != 0) begin
        lsq_ctrl.cancel_sq(data.pending_sq_cancel_count);
        data.pending_sq_cancel_count = 0;
    end
endfunction:apply_pending_lsq_cancels
```

中文伪代码：

```text
先确保公共data和LSQ model已建立；
LQ cancel计数非零时按该数量回退软件LQ pointer/free count，然后把计数清零，避免重复回退；
SQ cancel计数非零时执行同样的SQ资源回退和计数清理；
本helper不创建新的redirect owner，只消费原global redirect handler登记的数量。
```

源码位置：同文件，function：`admit_non_lsq_if_ready()`。该helper保留原non-LSQ零分配兼容入口。

```systemverilog
function bit memblock_lsqenq_dispatch_base_sequence::admit_non_lsq_if_ready(output bit has_progress);
    memblock_uid_t uid;
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;

    has_progress = 1'b0;
    if (!next_uid_needs_lsq_admission(uid, main_tr, behavior)) begin
        return 1'b0;
    end
    if (behavior.need_alloc != 2'b00) begin
        return 1'b0;
    end
    lsq_ctrl.commit_non_lsq_admission(uid, behavior, main_tr);
    complete_admission(uid);
    has_progress = 1'b1;
    return 1'b1;
endfunction:admit_non_lsq_if_ready
```

中文伪代码：

```text
从公共连续admission起点读取下一uid并推导behavior；没有合法next uid时返回未处理；
下一uid需要LQ或SQ时返回未处理，让普通candidate路径继续；
仅当need_alloc为0时调用既有non-LSQ commit owner，再复用complete_admission开放issue；
最后置progress并返回已处理；当前scalar-only validation使该入口在本轮支持集合内不可达。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`，
task：`send_idle_lsqenq_boundary()`。该task的输入是cycle编号、debug原因和本拍progress引用；副作用是向
driver发送一个全零item，并在该driver边界完成上一pending batch。

```systemverilog
task memblock_lsqenq_dispatch_base_sequence::send_idle_lsqenq_boundary(
    input int unsigned cycle_idx,
    input string reason,
    inout bit has_progress);
    lsqenq_agent_agent_xaction tr;

    tr = lsqenq_agent_agent_xaction::type_id::create(
        $sformatf("lsqenq_dispatch_idle_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create idle lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b0;
    tr.memblock_dispatch_ready_timeout = 0;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_request_launched = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    start_item(tr);
    finish_item(tr);
    complete_v2_pending_sample(has_progress);
endtask:send_idle_lsqenq_boundary
```

中文伪代码：

```text
该task用于主动制造一个没有新request的driver采样边界；
先创建新xaction，创建失败立即fatal；
调用clear_lsqenq_xaction把六个slot的valid、qualifier和payload全部清零；
再明确关闭ready等待和launch metadata，并保存当前flush epoch，pre/post gap固定为0；
通过start_item/finish_item把全零item交给clock-first driver；
finish_item返回表示driver已跨过一个clocking边界并处理该idle；
最后调用complete_v2_pending_sample，根据epoch开放上一批issue或丢弃completion，并把结果OR到has_progress。
```

源码位置：同文件，task：`drive_lsqenq_loop()`的global-stop分支。该分支保证sequence退出前不会留下
一批已reservation但未跨sample边界的uid。

```systemverilog
if (data.is_global_stop_requested()) begin
    has_progress = 1'b0;
    if (pending_sample_valid) begin
        send_idle_lsqenq_boundary(cycle_idx, "global_stop trailing sample", has_progress);
    end
    `uvm_info(get_type_name(),
              $sformatf("stop LSQ enqueue loop by global_stop_requested at cycle=%0d",
                        cycle_idx),
              UVM_LOW)
    break;
end
```

中文伪代码：

```text
主循环每拍首先查询公共global-stop；
命中后先把本地has_progress清零；
如果仍有pending batch，调用send_idle_lsqenq_boundary再跨一个driver边界并完成或丢弃该batch；
随后打印退出原因并break，不再收集或launch新candidate；
如果没有pending batch则直接退出，不制造无意义的额外item。
```

三个调用点的职责如下：

| 调用者 | 触发条件 | idle边界后的主流程影响 |
|---|---|---|
| `send_lsqenq_cycle()`无candidate分支 | 本拍没有可launch的LSQ candidate，包括随机目标为0 | 完成上一pending batch后返回，本拍不建立新reservation |
| `send_lsqenq_cycle()`non-LSQ切换分支 | pending有效且下一uid为`need_alloc=0` | 先完成上一batch，再进入兼容non-LSQ admission |
| `drive_lsqenq_loop()` | global stop且pending有效 | 完成/丢弃最后batch后退出sequence |

### 7.3 正确性检查

launch后立即预留保证下一packet key连续；issue-ready延后一边界保证DUT有sample机会。pending遇non-LSQ或
global stop时由全零idle提供边界，最后一批不会悬空。该局部状态不改变issue scheduler算法。

## 8. Allocation Owner 收敛

源码位置：`lsq_ctrl_model.sv`，function：`commit_allocate_with_resp()`。

```systemverilog
preview_allocate(behavior, expected_lq_key, expected_sq_key);
if ((behavior.uses_lq && dut_lq_key != expected_lq_key) ||
    (behavior.uses_sq && dut_sq_key != expected_sq_key)) begin
    `uvm_fatal("LSQ_CTRL",
               $sformatf("uid=%0d LSQ enq resp mismatch: expected lq={%0d,%0d} sq={%0d,%0d}, got lq={%0d,%0d} sq={%0d,%0d}",
                         uid,
                         expected_lq_key.flag,
                         expected_lq_key.value,
                         expected_sq_key.flag,
                         expected_sq_key.value,
                         dut_lq_key.flag,
                         dut_lq_key.value,
                         dut_sq_key.flag,
                         dut_sq_key.value))
end
commit_allocate(uid, behavior, tr);
```

中文伪代码：

```text
有response版本先只读计算expected key；
load只比较LQ、store只比较SQ，unused key不影响结果；
匹配后调用唯一commit_allocate写主表、active/enq/map、pointer和free count；
wrapper不再复制公共状态更新公式；V2 sequence不调用该wrapper。
```

这消除了两个allocation owner。non-LSQ路径也复用`commit_allocate()`，但因不使用LQ/SQ，不推进资源。

## 9. 主体逻辑影响判断

| 原主体逻辑 | 本轮是否改变 | 说明 |
|---|---|---|
| 主表生成/validation | 否 | 只把现有`numLsElem` 0/1改为统一类型cast |
| 连续uid admission | 否 | 仍从`get_next_new_admit_uid()`取连续前缀，不跳过阻塞uid |
| issue scheduler | 否 | 仅把调用时点延后到sample边界，内部route/fired-mask不变 |
| redirect/reissue owner | 否 | 复用现有flush epoch、active map和pending cancel |
| writeback/commit/deq | 否 | 本轮未修改相关handler或sequence |
| pass/fail/terminal | 否 | admission不直接置这些状态 |

新增功能仅包括三类入队数量权重和pending-sample时序；agent通用路径的6/4约束/复核是对既定V2协议
上限的完整覆盖，不改变dispatch仲裁。其余是V2字段/位宽/协议适配或公共owner收敛。

## 10. 实现与 Plan 不一致项

### 10.1 未新增 V2/V3 Profile Selector

Plan 原有逻辑：原始问题一要求 `tb.f` 选择 `MEMBLOCK_DUT_PROFILE_V2/V3`，再由排他 profile 分支定义
LSQ tuple。

当前源码逻辑与原因：当前 V2 worktree 已有可编译的 V2 baseline，但没有 profile selector。本专项复用
该 baseline，只补齐缺少的 LSQ 派生宏，避免在一个 V2 专项中建立第二套版本权威。该调整已写入 Plan 的
`IMPLEMENTATION_DELTA`。

源码位置：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`。当前宏允许外部 compile-time 覆盖，
默认值直接描述本 worktree 的 V2 结构。

```systemverilog
`ifndef MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM
    `define MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM 6
`endif
`ifndef MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH
    `define MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH 6
`endif
`ifndef MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH
    `define MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH 4
`endif
```

中文伪代码：

```text
编译当前V2 worktree时读取compile参数文件；
若外层没有覆盖LSQ结构宏，就采用V2的总slot 6、load 6和store 4；
本文件不读取runtime plus，也不根据运行期值切换版本；
V3继续由其独立worktree/profile专项维护，本次不宣称跨版本编译验证。
```

处理结论：保持当前实现；这是已审计的 implementation delta，不是遗漏的 selector。

### 10.2 不实现 Vector Chunk，也不保留 6/4 个空项

Plan 原有逻辑：原始问题四曾要求 vector element 跨拍 chunk，并要求 LQ/SQ base free 先保留 6/4 个空项。

当前源码逻辑与原因：用户已明确本轮不支持 vector LS。当前 candidate 只接受
`num_ls_elem==1`，并把 6/4 当作本拍 load/store element 端口上限；实际 free count只需容纳当前 batch，
不复制 RTL registered credit 的提前余量。否则软件模型会永久浪费 LQ/SQ 尾部资源。

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`collect_lsq_candidates()`。

```systemverilog
if (behavior.num_ls_elem != memblock_num_ls_elem_t'(1)) begin
    `uvm_fatal(get_type_name(),
               $sformatf("uid=%0d scalar-only LSQ enqueue requires num_ls_elem=1, got %0d",
                         uid,
                         behavior.num_ls_elem))
end
tentative_load = load_elem_count + (behavior.uses_lq ? behavior.num_ls_elem : 0);
tentative_store = store_elem_count + (behavior.uses_sq ? behavior.num_ls_elem : 0);
if (tentative_load > MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH ||
    tentative_store > MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH ||
    tentative_load > lq_free_tmp ||
    tentative_store > sq_free_tmp) begin
    break;
end
```

中文伪代码：

```text
对连续candidate逐条推导behavior；
只要element数不是1就fatal，不建立vector chunk/progress隐状态；
分别累计本拍load和store element数；
超过compile 6/4或超过当前实际free count时停止收集；
不执行base_free大于等于6/4或free大于等于batch加reserve的额外门限。
```

处理结论：保持 scalar-only 和实际 free gate；vector 分片归后续专项。

### 10.3 ROB Key 不作为 Setter 的重复入参

Plan 原有逻辑：问题三伪代码把 ROB/LQ/SQ key 都列成 `set_req_fields()` 的 caller 入参。

当前源码逻辑与原因：LQ/SQ key 是本拍 candidate 的局部预览结果，仍由 caller传入；ROB key 已由主表
transaction 保存，setter 直接调用 `main_tr.get_rob_key()`，避免 caller 再维护第二份 ROB 值。

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`set_req_fields()`。

```systemverilog
function void set_req_fields(input lsqenq_agent_agent_xaction tr,
                             input int unsigned slot,
                             input bit valid,
                             input main_control_transaction main_tr,
                             input memblock_op_behavior_t behavior,
                             input memblock_lq_key_t lq_key,
                             input memblock_sq_key_t sq_key);
    if (valid) begin
        rob_key = main_tr.get_rob_key();
    end
endfunction:set_req_fields
```

中文伪代码：

```text
caller只传当前slot、main transaction、behavior和局部预测LQ/SQ key；
active分支从main transaction唯一读取ROB key；
setter把该ROB key和caller提供的LQ/SQ key写入同一slot；
idle分支不读取main transaction，并把三类key全部清零。
```

处理结论：保持当前更简单实现；Plan 执行结果和 source analysis 已同步该签名。

### 10.4 `tb.f` 和 Connect 在执行前已完整

Plan 原有逻辑：问题一/二把 `tb.f` profile selector和 `lsqenq_agent_connect.sv` extra字段连接列为 coding
落点。

当前源码逻辑与原因：按 10.1 不新增 selector；执行前 V2 connect 已包含 slot0..5 的完整 extra 字段，
本轮只核对 connection 与 RTL 端口数量一致，不制造无行为变化 diff。

源码位置：`mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv`。以下是 active drive 方向的 slot0代表片段；
slot1..5 和 passive mirror 方向采用相同字段集合。

```systemverilog
force RTL_PATH.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0 =
    U_IF_NAME.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0;
force RTL_PATH.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe =
    U_IF_NAME.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe;
force RTL_PATH.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType =
    U_IF_NAME.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType;
force RTL_PATH.io_ooo_to_mem_enqLsq_req_0_bits_lastUop =
    U_IF_NAME.io_ooo_to_mem_enqLsq_req_0_bits_lastUop;
force RTL_PATH.io_ooo_to_mem_enqLsq_req_0_bits_trigger =
    U_IF_NAME.io_ooo_to_mem_enqLsq_req_0_bits_trigger;
```

中文伪代码：

```text
LSQ agent处于active drive方向时，把VIF的extra字段逐项force到同名RTL input；
exceptionVec的24个flat bit、trigger、fuOpType、flushPipe和lastUop均有连接；
slot0到slot5执行相同映射；
连接只搬运字段，不修改sequence、status、pointer或driver时序。
```

基线核对命令：

```text
git diff -- mem_ut/ver/ut/memblock/cfg/tb.f \
            mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv
<无输出>
```

处理结论：保持无 diff；Plan 的 `IMPLEMENTATION_DELTA` 已说明复用现有 baseline。

### 10.5 Idle Item 使用全零 `send_pkt()`

Plan 原有逻辑：问题五伪代码要求取得 idle item 后先验证 payload，再调用 `drive_idle(DRV_0)`。

当前源码逻辑与原因：driver 对没有 redirect abort 的 item 统一调用 `send_pkt(req)`；idle item 的 valid
全为0，`send_pkt()` 先调用 `validate_v2_scalar_item()` 确认全部 qualifier/payload 为0，再把这些0值搬到
VIF。接口结果与 `drive_idle()` 相同，同时不会让 malformed idle item 绕过检查。

源码位置：`lsqenq_agent_agent_driver.sv`，task：`main_phase()`。

```systemverilog
active_request = has_active_request(req);
if (active_request &&
    (memblock_sync_pkg::dispatch_flush_in_progress ||
     memblock_sync_pkg::dispatch_flush_epoch != req.memblock_dispatch_flush_epoch)) begin
    req.memblock_dispatch_aborted_by_redirect = 1'b1;
    this.drive_idle(this.cfg.drv_mode);
end else begin
    this.send_pkt(req);
    req.memblock_dispatch_request_launched = active_request;
end
```

中文伪代码：

```text
先对六个valid做OR判断当前item是否active；
active且flush/epoch失效时不发送item，改为drive_idle并标记abort；
其余情况统一调用send_pkt；
active item被标记launched=1，idle item保持launched=0；
send_pkt在首次VIF赋值前验证idle全部为0，所以最终VIF效果等价于drive_idle且保留错误检查。
```

处理结论：保持当前实现，并把该功能等价但调用点不同的行为明确记录为 Plan 不一致项。

## 11. Plan 未说明但 Coding 落实的细节

### 11.1 通用 Default/Directed Item 的 6/4 防绕过

细节功能：原始Plan只在dispatch candidate中定义6/4 gate；第7轮review发现通用default sequence和关闭
约束的directed item不经过candidate。Coding因此把同一compile合同补到xaction约束和driver运行期检查。

为什么Plan未覆盖：原Plan聚焦真实dispatch sequence，遗漏了agent自身仍可由default sequence或其它
producer直接生成xaction。该细节已回写Plan的`IMPLEMENTATION_DELTA`，但仍按执行规则记录为原始Plan
未说明的coding补充。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，
constraint：`c_v2_batch_enqueue_width`。以下store表达式证明随机solver不能选择超过4个store。

```systemverilog
int'(io_ooo_to_mem_enqLsq_req_0_valid && io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) +
int'(io_ooo_to_mem_enqLsq_req_1_valid && io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b10) +
int'(io_ooo_to_mem_enqLsq_req_2_valid && io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b10) +
int'(io_ooo_to_mem_enqLsq_req_3_valid && io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b10) +
int'(io_ooo_to_mem_enqLsq_req_4_valid && io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b10) +
int'(io_ooo_to_mem_enqLsq_req_5_valid && io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b10)
    <= `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH;
```

中文伪代码：

```text
随机solver只统计valid且needAlloc为SQ的slot；
六个判断结果相加得到本item的store数量；
该数量必须不超过compile profile的store enqueue width，V2即4；
load使用同构表达式并受load width约束；
因此标准default sequence无法随机出5个或6个store。
```

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，
function：`validate_v2_scalar_item()`。以下分支证明driver不信任producer约束状态。

```systemverilog
store_count++;
if (store_count > `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH) begin
    `uvm_fatal(get_type_name(),
               $sformatf("V2 LSQ batch store count=%0d exceeds width=%0d",
                         store_count, `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH))
end
```

中文伪代码：

```text
driver扫描active store slot时递增本item的store计数；
第一次超过V2 compile上限4就fatal；
该检查发生在send_pkt写任何VIF字段之前，所以directed item即使关闭xaction约束也不能绕过；
正常dispatch candidate已提前过滤，运行期复核只承担协议兜底，不改变其candidate选择结果。
```

在本特性中的作用：补齐完整agent行为面，防止随机fallback、directed sequence或未来producer把非法batch
送入DUT。是否需要回写Plan：已作为第7轮`IMPLEMENTATION_DELTA`回写。

### 11.2 通用 Default/Directed Item 的 `fuOpType` 防绕过

细节功能：原始Plan定义本轮只支持scalar load、software prefetch和普通store，但没有让agent自身的
default/direct路径共享明确opcode合同。第8轮review发现任意9-bit值仍可绕过主表validation送入DUT。

为什么Plan未覆盖：原Plan假设真实dispatch setter总是从已校验main transaction复制`fuOpType`，没有覆盖
直接随机或手工赋值xaction的producer。第9轮default-random专项进一步证明自定义函数调用不能用于
反向求解随机opcode，因此当前用宏值表统一constraint和checker数值。该细节已回写`IMPLEMENTATION_DELTA`。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，constraint：
`c_v2_scalar_request_contract`的active opcode约束。该分支让random solver只选择支持集合。

```systemverilog
(io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b01) ->
    io_ooo_to_mem_enqLsq_req_0_bits_fuOpType inside
        {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
(io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) ->
    io_ooo_to_mem_enqLsq_req_0_bits_fuOpType inside
        {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
```

中文伪代码：

```text
slot0为active LQ时，fuOpType必须落入普通load/prefetch宏值表；
slot0为active SQ时，fuOpType必须落入普通store宏值表；
slot1到slot5应用同一约束；
constraint直接对随机字段使用inside，VCS可以求解合法值；
因此default sequence不能随机出CBO、AMO或其它不支持opcode。
```

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，function：
`validate_v2_scalar_item()`的LQ opcode复核。该分支覆盖directed item和关闭constraint的路径。

```systemverilog
if (!lsqenq_agent_agent_xaction::is_supported_v2_load_or_prefetch_fuoptype(fu_op_type)) begin
    `uvm_fatal(get_type_name(),
               $sformatf("load/prefetch slot=%0d has unsupported fuOpType=0x%0h",
                         slot, fu_op_type))
end
```

中文伪代码：

```text
driver在首次VIF赋值前读取active LQ slot的fuOpType；
调用读取同一宏值表的load/prefetch helper；
不支持值立即fatal，不继续执行send_pkt；
合法值才进入load batch计数、key和其它scalar字段复核。
```

源码位置：同文件同function，SQ opcode复核。该分支覆盖普通store directed item。

```systemverilog
if (!lsqenq_agent_agent_xaction::is_supported_v2_store_fuoptype(fu_op_type)) begin
    `uvm_fatal(get_type_name(),
               $sformatf("store slot=%0d has unsupported fuOpType=0x%0h",
                         slot, fu_op_type))
end
```

中文伪代码：

```text
driver在首次VIF赋值前读取active SQ slot的fuOpType；
调用读取同一宏值表的store helper；
不支持值立即fatal；
合法值才进入store batch计数、key和其它scalar字段复核。
```

在本特性中的作用：让main-table producer、default sequence和directed item遵守同一个scalar opcode边界。
是否需要回写Plan：已作为第8轮`IMPLEMENTATION_DELTA`回写。

### 11.3 Framework Metadata 的手工 Compare 回退闭环

细节功能：项目既有custom compare在`super.compare()`失败后重新执行payload手工比较，以忽略base
timestamp等差异。第8轮review发现该回退遗漏wait、timeout和flush epoch，会吞掉stale metadata差异。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，function：
`compare()`的metadata分支。该单一分支一次覆盖本类全部五个控制字段。

```systemverilog
if (this.memblock_dispatch_wait_can_accept != rhs_.memblock_dispatch_wait_can_accept ||
    this.memblock_dispatch_ready_timeout != rhs_.memblock_dispatch_ready_timeout ||
    this.memblock_dispatch_request_launched != rhs_.memblock_dispatch_request_launched ||
    this.memblock_dispatch_aborted_by_redirect != rhs_.memblock_dispatch_aborted_by_redirect ||
    this.memblock_dispatch_flush_epoch != rhs_.memblock_dispatch_flush_epoch) begin
    super_result = 0;
end
```

中文伪代码：

```text
进入手工回退后同时比较wait、timeout、launch、abort和flush epoch；
任一字段不同都把结果保持为失败；
随后继续比较extra和普通request payload；
因此旧timestamp忽略语义保留，但功能metadata差异不再被吞掉。
```

在本特性中的作用：保证不同launch/flush解释的xaction不会被误判相等。是否需要回写Plan：已作为第8轮
`IMPLEMENTATION_DELTA`回写；`psdisplay()`同步打印同一五字段集合。

### 11.4 通用 Default/Directed Item 的零 Gap 合同

细节功能：V2 driver使用clock-first逐拍streaming，所有producer都必须提供`pre_pkt_gap=0`和
`post_pkt_gap=0`。第9轮review发现LSQ xaction继承基类`post_pkt_gap=0..50`，通用default random
sequence因此可能生成driver必然拒绝的item。

为什么Plan未覆盖：原Plan只要求dispatch sequence显式写零gap并让driver拒绝非零gap，没有检查xaction
继承的random约束。该细节已回写第9轮`IMPLEMENTATION_DELTA`。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，constraint：
`v2_streaming_gap_cons`。

```systemverilog
constraint v2_streaming_gap_cons {
    pre_pkt_gap == 0;
    post_pkt_gap == 0;
}
```

中文伪代码：

```text
LSQ xaction随机化时把发送前和发送后gap都固定为0；
通用default sequence的十个随机item因此天然满足V2连续streaming合同；
driver仍在首次VIF赋值前复核实际gap，关闭约束或randomize后手工改值时立即fatal。
```

在本特性中的作用：补齐default random producer的合法性，同时不改变dispatch candidate、DUT采样时序或
主框架状态。是否需要回写Plan：已作为第9轮`IMPLEMENTATION_DELTA`回写。

### 11.5 Streaming Gap 的 Display/Compare 回退闭环

细节功能：第11轮review发现，driver虽然会在首次VIF写入前拒绝非零gap，但xaction的custom compare在
`super.compare()`失败后重置结果并手工比较时，没有重新比较`pre_pkt_gap/post_pkt_gap`。两个仅gap不同的
item因此可能被误判相等，`psdisplay()`也无法直接定位差异。

为什么Plan未覆盖：原Plan定义了V2零gap驱动合同，没有分析项目基类compare失败后，本类手工回退会忽略
哪些基类字段。该细节已回写第11轮`IMPLEMENTATION_DELTA`。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`，function：
`psdisplay()`。该打印分支让streaming gap进入transaction调试文本。

```systemverilog
pkt_str = $sformatf("%spre_pkt_gap=%0d post_pkt_gap=%0d ",
                    pkt_str,
                    this.pre_pkt_gap,
                    this.post_pkt_gap);
```

中文伪代码：

```text
psdisplay构造transaction调试文本时读取pre和post gap；
把两个值追加到packet、channel和timestamp之后；
该函数只读transaction，不改变随机字段、driver或公共状态。
```

源码位置：同文件，function：`compare()`。该分支在手工回退中重新比较两个功能gap，同时继续忽略
`start/finish`时间戳。

```systemverilog
if (this.pre_pkt_gap != rhs_.pre_pkt_gap ||
    this.post_pkt_gap != rhs_.post_pkt_gap) begin
    super_result = 0;
    `uvm_info(get_type_name(),
              $sformatf("compare fail for streaming gap: this={pre=%0d post=%0d} rhs={pre=%0d post=%0d}",
                        this.pre_pkt_gap,
                        this.post_pkt_gap,
                        rhs_.pre_pkt_gap,
                        rhs_.post_pkt_gap),
              UVM_NONE)
end
```

中文伪代码：

```text
只有super.compare失败进入项目既有手工回退后才执行本分支；
比较左右transaction的pre gap和post gap；
任一值不同就把回退结果保持为失败，并打印双方值；
随后仍继续比较framework metadata、V2 extra和普通payload，不改变timestamp忽略边界。
```

在本特性中的作用：transaction compare与driver零gap合同一致，未来scoreboard或debug不会漏掉仅gap不同的
item。是否需要回写Plan：已作为第11轮`IMPLEMENTATION_DELTA`回写。

### 11.6 当前 V2 6/6/4 Tuple 的激励前 Fail-Fast

细节功能：三个LSQ compile宏仍可被外部覆盖，但当前interface、xaction、driver和setter物理展开固定六个
slot。第11轮review发现，原compile consistency只检查非零和width不超过slot，较小tuple会让循环跳过仍被
驱动的高slot，较大tuple则延迟到case default fatal。

为什么Plan未覆盖：原Plan要求用compile宏方便版本切换，未区分“consumer引用宏”与“当前显式字段链已经
支持任意tuple”。当前没有V2/V3 profile selector或生成式字段consumer，该细节已回写第11轮
`IMPLEMENTATION_DELTA`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，static function：
`check_compile_param_consistency()`。该检查在公共配置初始化阶段、任何sequence产生激励前执行。

```systemverilog
if (MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM != 6 ||
    MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH != 6 ||
    MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH != 4) begin
    `uvm_fatal("SEQ_COMPILE_CFG",
               $sformatf("current V2 LSQ field expansion requires slot/load/store tuple 6/6/4, got %0d/%0d/%0d",
                         MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM,
                         MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH,
                         MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH))
end
```

中文伪代码：

```text
公共配置初始化读取三个LSQ compile宏；
要求当前slot/load/store值依次精确为6、6、4；
任一值不匹配就在sequence或driver运行前fatal，并打印实际tuple；
匹配时不修改宏或runtime参数，继续执行其它compile结构一致性检查。
```

在本特性中的作用：保留宏作为所有consumer的唯一命名入口，同时拒绝尚未完成全链路参数化的伪profile，
默认V2行为不变。是否需要回写Plan：已作为第11轮`IMPLEMENTATION_DELTA`回写。

`get_v2_extra_fields()`仍只是完成原Plan已明确要求的extra字段`psdisplay()/compare()`链路，不是额外功能。
验证入口和工具环境差异单独记录在第13章，不归类为产品coding。

## 12. 验证结果

| 验证 | 结果 | 关键证据 |
|---|---|---|
| `git diff --check` | PASS | 本专项源码和文档无 whitespace error |
| 第7轮修复后远端VCS clean compile | PASS | 2026-07-16 20:53开始，20:55完成全部partition compile和link；无源码error，含1条无害`LCA_FEATURES_ENABLED`工具提示；最终KDB阶段0 error/0 warning |
| 第7轮clean compile后的真实scalar load | PASS | 2026-07-16 20:55:57，372.8ns完成issue、DCache response、WB、ROB commit、LQ deq和terminal；0 warning/error/fatal |
| 第8轮源码修复后远端VCS clean compile | PASS | 2026-07-17 10:36:36开始；全部partition compile/link完成，无源码error，含1条无害`LCA_FEATURES_ENABLED`工具提示；最终KDB阶段0 error/0 warning |
| 第8轮clean compile后的真实scalar load | PASS | 2026-07-17 10:40:00，372.8ns完成issue、DCache response、WB、ROB commit、LQ deq和terminal；`TEST CASE PASSED`，0 warning/error/fatal |
| 第9轮及opcode宏值表修复后远端VCS clean compile | PASS | 2026-07-17 11:37开始；174个RTL module、全部partition、stitch和link完成，无源码error；最终KDB阶段0 error/0 warning |
| default-random gap/opcode专项 | PASS | type override启动10-item `lsqenq_agent_agent_default_sequence`；LSQ enqueue在main结束时无需kill，且无`CNST-CIF`、`RNDFLD`、gap fatal或UVM warning/error/fatal |
| 第9轮最终真实scalar load | PASS | 使用同一`simv`，372.8ns完成LDA issue、DCache response、WB、ROB commit、LQ deq和terminal；`TEST CASE PASSED`，0 warning/error/fatal |
| 第11轮修复后源码冻结 | PASS | 编译前后`git diff --binary -- mem_ut/ver/ut/memblock` SHA-256均为`99fdd0c69f99f7dd3e08eed289ea9ace2df11c344b62563c8f147873f3f3b8f0`；所有相关SV时间早于最终`simv` |
| 冻结版最终远端VCS clean compile | PASS | 2026-07-17 13:33开始，13:35完成新partition、174个RTL module、全部UVM package、stitch和link；compile log仅有LCA工具warning，无源码error；最终KDB 0 error/0 warning |
| 冻结版default-random专项 | PASS | 13:35生成的`simv`以`UVM_FULL`明确启动10-item `lsqenq_agent_agent_default_sequence`，main结束时LSQ enqueue无需kill；13:38:15 PASS，0 warning/error/fatal |
| 冻结版最终真实scalar load | PASS | 在default-random之后使用同一`simv`，13:38:28于372.8ns完成LDA issue、DCache response、WB、ROB commit、LQ deq和terminal；PASS，0 warning/error/fatal |
| 高ZERO权重 `100/0/1` | PASS | issue延迟到约1950ns后完整terminal；0 error/fatal |
| zero-only `1/0/0` | EXPECTED FATAL | 0ns命中`LSQ enqueue random weights cannot select ZERO forever` |
| software-only基础序列 | PASS | 0 error/fatal，但不计真实LSQ覆盖 |
| scalar store admission | 部分通过 | 已到STA/STD issue、WB和ROB commit；后续既有SQ deq pointer mismatch |
| 6-store压力尝试 | 下游失败 | 旧尝试出现既有`WB_UID_MISMATCH`；当前4-store合同由candidate、xaction约束和driver复核三层静态确认 |

最终日志：

- compile：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log`
- default-random专项：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_sanity_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_lsqenq_round12_frozen_default_random_20260717.log`
- 冻结版最终真实load：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=tc_dispatch_real_smoke_seed=666666_rtl_lsqenq_round12_frozen_real_load_20260717.log`

store失败落在未修改的writeback/SQ deq专项，不证明LSQ enqueue失败，也不能被本轮静默放宽。完整store
terminal仍是总控Plan中int-WB和ROB/LSQ commit/deq专项的验证依赖。

## 13. 验证入口与工具环境差异

### 13.1 `basicTest` vseq 入口无有效时序覆盖

执行规则建议的 `basicTest + memblock_dispatch_real_smoke_vseq` 当前在 vseq 启动同拍结束，日志没有真实
LSQ issue/terminal。为避免空跑 PASS，本轮使用仓库既有 `tc_dispatch_real_smoke`，它通过 `tc_base` 在
agent sequencer 启动真实 LSQ/issue/commit/L2TLB sequence。该差异只影响验证入口，不改产品代码；
virtual-sequence phase 生命周期问题应单独处理。

### 13.2 多 Plusarg Wrapper 转义

`eda_batch_run` wrapper 会把含空格的 `plus_arg` 转义成一个参数。随机权重专项因此直接在 eda01 执行
同一 Makefile `batch_run`，并把整组 plusarg作为单个 make assignment 传入。日志确认所有参数分别加载。
该差异不影响 LSQ 实现，只影响专项验证命令传参。

### 13.3 第7轮历史 VCS/NFS 增量缓存异常

第7轮归档检查期间，增量编译先命中生成文件`work.lib++/tdc.sdb` corrupted；`eda_run`的重复partcomp又在
未启动仿真前命中VCS`SIGSEGV`并挂起。当时只清理`base_fun/partitionlib`和`base_fun/exec`下VCS生成物，
随后于2026-07-16 20:55完成clean compile，并在20:55:57用同一`simv`完成真实load smoke。

本轮在clean-build smoke后再次调用会隐式重复partcomp的`eda_run`，同样在仿真启动前命中增量
`SIGSEGV`；终止该无效调用后，真实load改由不重复编译的`batch_run`执行。随后再次删除生成的
`partitionlib/exec`并完成最终clean compile，保留两份smoke日志。

这些现象属于VCS/NFS增量数据库稳定性，不是SystemVerilog compile error，也不作为当前最终归档依据。
当前归档证据采用第11轮修复后冻结diff哈希、13:33之后的最终clean compile、随后同一`simv`的
default-random专项和真实scalar-load smoke；结果在第12章更新，不把失败wrapper调用记成产品测试通过。

### 13.4 Default-random 专项隔离无关 Commit Sequence

`tc_sanity`不创建主表，而默认启用的`memblock_lsqcommit_dispatch_base_sequence`会在
`wait_for_main_table()`中永久等待；这与LSQ enqueue xaction随机化和driver streaming合同无关。最终专项
显式设置`MEMBLOCK_LSQCOMMIT_SEQ_EN=0`，只关闭该无关commit producer，同时保留LSQ enqueue type
override、agent driver、VIF和DUT采样路径。日志确认被测default sequence启动并在main phase结束前自然
完成；该隔离不放宽LSQ enqueue约束，也不作为commit/deq功能验证。

## 14. 非本次修改的逻辑分析

当前工作区另有以下并发改动，未纳入本review，也不会进入本轮commit：

- `AI_DOC/analysis/rtl/v2/flows/memory_flush_pipe_flow.md`：RTL flushPipe长期分析方向。
- `AI_DOC/analysis/rtl/v2/index.md`：RTL知识库索引同步。
- `AI_DOC/analysis/rtl/v2/flows/memory_pmp_pma_permission_flow.md`：未跟踪的PMP/PMA权限分析文档。
- `AI_DOC/analysis/rtl/v2/flows/dcache_l2_refill_hint_and_flush_done_flow.md`：未跟踪的DCache L2
  refill hint/flush-done RTL分析文档，属于DCache sideband专项。
- `AI_DOC/analysis/rtl/v2/flows/l2_inner_tilelink_request_response_flow.md`：未跟踪的L2 inner TileLink
  request/response RTL分析文档，属于DCache/L2接口专项。
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_csr_control_runtime_semantic_review_execution_plan_20260708.md`：
  CSR control/sfence `flushPipe` 接口透传专项 Plan，属于其他功能逻辑，需要独立 coding/review。
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_adapt_coding_plan_20260708.md`：本专项只覆盖
  该总控Plan中的LSQ enqueue状态/路径同步；同文件当前diff中的CSR/sfence `flushPipe` 方案属于并发专项，
  不纳入本次LSQ功能正确性结论，也不进入本次commit。

这些文件不影响本轮SystemVerilog编译和LSQ enqueue运行期逻辑，必须由各自任务单独review。

## 15. Review 轮次与当前结论

- 前置 review：发现 V2 timeout warning/clamp 未受 capability 控制，已修复并重编译。
- 归档前独立 review 第 1 轮：发现 xaction extra 字段 display/compare 缺口和本文 Plan 差异证据不足，
  两项均已修复，最新 compile 与真实 load smoke 已通过。
- 归档前独立 review 第 2 轮：未发现 LSQ enqueue 源码功能问题，发现 ready-timeout 文档边界不准确和
  并发 CSR Plan 分类遗漏，均已修正。
- 归档前独立 review 第 3 轮：确认第 2 轮问题已闭环，发现参数规则中的 LSQ enqueue sequence 默认值
  与源码相反；规则已同步为既有默认 1。
- 归档前独立 review 第 4 轮：确认源码无新问题，发现当前web/interface/source analysis旧主链、历史文档
  失效边界、总控Plan并发hunk分类和最终Plan第13章遗漏；均已修正。
- 归档前独立 review 第 5 轮：确认源码无新问题，继续发现Web/interface/source analysis内部旧段落、
  其余历史文档缺少失效注记和总控Plan提前归档；已通过全量分类扫描修正。
- 归档前独立 review 第 6 轮：确认源码无新问题，发现key来源、issue-ready/TLB gate、Web helper/关闭
  路径、未launch fatal、unused key和归档措辞等少量文档偏差；均已修正。
- 归档前独立review第7轮：发现default/direct路径缺少4-store gate、验证日志执行顺序、Web调用链、
  review idle边界覆盖和interface FuType编码五项问题；源码与文档已修正，clean compile和其后的真实load
  smoke均通过。
- 归档前独立review第8轮：发现active `fuOpType`合同、framework metadata compare/display、non-LSQ
  pending边界文档顺序、review源码块结构、compile warning措辞和Plan编号六项问题；均已修复。第8轮
  修复后的clean compile和同一`simv`真实scalar-load smoke均已通过。
- 归档前独立review第9轮：发现default random item零gap合同、两份flow残留、本文关键task/helper展开和
  第7轮历史验证措辞四项问题；源码和文档已修复。第9轮修复后进一步发现constraint helper不可由VCS
  反向求解随机opcode，已改为constraint和driver共享宏值表；最终clean compile、default-random专项和
  同一`simv`真实load smoke均已通过。
- 归档前独立review第10轮：发现`set_req_fields()`缺少空xaction、越界slot和完整idle合同的入口自检；
  已补齐受控fatal。修复后的最终clean compile、`UVM_FULL` default-random专项和clean-build `simv`
  真实load smoke均已通过。
- 归档前独立review第11轮：源码review发现custom compare手工回退遗漏pre/post gap，以及当前显式六slot
  字段链未拒绝非6/6/4 compile tuple；文档review发现旧类名、错误allocation调用边、setter合同、源码块
  伪代码、epoch条件、非本次修改分类和验证时间链问题。源码已补齐gap display/compare和激励前6/6/4
  fail-fast，文档已逐项同步。随后冻结`mem_ut/ver/ut/memblock` diff，并严格顺序完成clean compile、
  default-random专项和同一`simv`真实scalar-load smoke，三项均通过且冻结哈希保持不变。

### 15.1 原逻辑、变更原因与变更后逻辑集中总结

下表是本review的最终功能分类。它集中覆盖本专项全部字段适配、新增功能和功能逻辑修改；前文第3至11章
提供每项对应的源码证据、中文伪代码和正确性检查。

| 修改项 | 修改类型 | 原逻辑 | 变更原因 | 变更后逻辑 |
|---|---|---|---|---|
| LSQ物理结构与派生宽度 | 字段/参数适配 | slot/load/store上限、`uopIdx`和`numLsElem`存在固定值或缺少统一权威 | V2接口为6 slot、6 load、4 store、7-bit `uopIdx`和5-bit `numLsElem` | compile宏及typed localparam成为唯一入口；当前显式字段链在激励前严格要求6/6/4，非默认tuple受控fatal |
| interface/xaction/monitor字段链 | 字段适配 | `uopIdx/numLsElem`沿用固定packed宽度，extra字段观察链不完整 | transaction、VIF和monitor必须与V2顶层同宽且可追踪 | 三层统一消费compile宽度；六slot extra字段进入automation、采样、display和compare |
| `numLsElem`公共类型 | 字段适配 | main transaction与behavior使用不同固定声明或`5'd`字面量 | 版本宽度变化会形成第二权威 | 统一使用`memblock_num_ls_elem_t`；非LSQ初始化可为0，当前scalar producer和setter只接受1 |
| Scalar request setter | 功能逻辑修改 | `uopIdx`可能来自uid低位，active/idle字段由多处零散赋值，错误caller缺少入口检查 | uid不是DUT uop序号，复用item可能残留payload | 唯一setter固定`uopIdx=0/lastUop=1/numLsElem=1`并完整写V2字段；null、越界、非法behavior和不完整idle均fatal |
| FuType/fuOpType合同 | 字段合法性修改 | V3 FuType可被裁剪，active `fuOpType`可取任意9-bit值 | 本轮只支持scalar load、software prefetch和普通store | FuType通过无损编码检查；LQ仅允许0..6、8..10，SQ仅允许0..3；constraint和driver复用同一值表 |
| Agent通用item合同 | 功能逻辑修改 | inactive slot可能残留qualifier/payload，default/direct item可生成5/6个store或非零gap | 任意producer都必须满足V2物理和streaming合同 | xaction约束inactive全零、load/store 6/4及pre/post gap 0；driver首次VIF赋值前再次fail-fast复核 |
| Transaction debug/compare | 功能逻辑修改 | custom compare回退可能吞掉extra、metadata或gap差异，display也不完整 | driver合法性检查不能替代transaction等价判断 | `get_v2_extra_fields()`聚合六slot；display和手工compare覆盖extra、五个framework metadata及pre/post gap |
| 每拍入队数量随机 | 新增runtime功能 | 随机模式只在1..物理最大值内均匀采样，不能主动产生idle | 需要分别控制0、中间和最大边界概率 | 新增ZERO/MIDDLE/MAX权重；两阶段`std::randomize`返回0、中间值或物理最大值，默认0/5/1保持旧1..6均匀分布 |
| 权重与runtime资源检查 | 新增配置检查 | 没有三类权重合法性和集中物理上限收敛 | 全0或zero-only会让主动flow无法推进 | `apply_runtime_resource_limits()`使用64-bit求和，拒绝非法组合并集中限制runtime资源；物理结构不建立runtime镜像 |
| Candidate gate | 功能逻辑修改 | 只限制总slot和free count，没有分别累计load/store element | 合法总slot仍可能超过V2 4-store能力 | 每拍只采样一次总目标，按连续uid累计6/4并限制实际LQ/SQ free；不额外保留6/4空项，超限uid留到下一拍 |
| Driver发送时序 | 功能逻辑修改 | 可能等待不存在的`canAccept/response`或在item内重复发送 | V2仅有request input，且需要保持每拍一批吞吐 | clock-first循环每边界先形成上一批sample机会，再最多launch一个当前item并立即`item_done()`；无item驱全零idle |
| Launch/reservation/pending sample | 新增局部状态并修改开放issue时点 | driver返回后allocation和`issue_ready`同拍发生 | 当前request尚未跨过下一DUT sample边界，但延后pointer又会重复分配 | launch后立即由唯一`commit_allocate()`预留；单深度pending batch在下一driver边界才`complete_admission()`开放issue |
| non-LSQ、末批与redirect边界 | 功能逻辑收敛 | non-LSQ零时间路径或global stop可能绕过最后sample，launch/abort/sample边界不清 | pending批不能悬空，也不能让旧epoch开放issue | pending后遇non-LSQ、无candidate或global stop时发送idle边界；epoch失效只丢弃pending completion，资源回退复用既有redirect/cancel owner |
| Allocation owner | 共享实现重构 | response wrapper复制main/status/map、pointer和free-count更新 | 两个状态owner容易分叉，且V2没有response key | `commit_allocate()`是唯一写状态owner；wrapper只比较实际使用key后转调，V2主链不调用wrapper |
| ready timeout与idle mode | 配置细节修改 | V2可能对无消费者timeout做warning/clamp，generic idle mode可产生valid/X | V2无accept-response且idle必须确定全零 | timeout仍解析并检查非负，但V2不读取、不等待；cfg soft默认`DRV_0`，driver build阶段拒绝其它active idle mode |

新增功能完整列表：ZERO/MIDDLE/MAX随机类别与权重检查、`request_launched` metadata、单深度
pending-sample batch。功能逻辑修改完整列表：唯一scalar setter、agent通用合同、6/4 candidate gate、
clock-first driver、allocation与issue-ready分层、non-LSQ/global-stop sample边界、redirect epoch过滤、
allocation owner收敛和transaction display/compare闭环。主表生成算法、issue scheduler内部仲裁、writeback、
ROB/LSQ commit/deq、pass/fail和terminal owner均未改变。

### 15.2 最终独立 Review 与归档结论

- 第12轮独立源码/V2语义review通过：无新发现、无必须修改项。
- 第12轮文档review发现`build_phase()`、`has_active_request()`、`get_v2_extra_fields()`的函数级证据不完整，
  且最后一章缺少集中式四要素总结；本文已补齐三个源码展开和第15.1节。
- 修复后的第13轮独立文档review通过：60个SystemVerilog块均在5行内一一对应中文伪代码，第15.1节完整
  覆盖修改类型、原逻辑、变更原因、变更后逻辑及新增/修改功能；无新发现、无必须修改项。
- 本agent最终自查未发现其它功能blocker。剩余store terminal失败属于尚未完成的SQ deq/int-WB专项，
  不能通过放宽本轮LSQ enqueue检查修复，也不阻塞本plan归档。

最终结论：本专项源码、文档、冻结验证和独立review均已闭环；关联Plan已移动到`plan/do`。
