# mem_ut V2 LSQ Enqueue 测试框架适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `do`，coding、文档同步、冻结验证和最终独立review均已完成 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle()` |
| 适配原则 | 只修改 V2 字段、参数和必要的入队完成细节，不改变主表、issue、commit/deq、pass/fail、terminal 主体逻辑 |
| 创建/修订日期 | 2026-07-17 |

## 1. 范围与边界

本 plan 只整理 LSQ enqueue 适配 V2 时需要解决的问题。每个问题均说明修改原因、最终方案、修改的
原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- scalar `LDU`、现有 software prefetch 和普通 scalar `STU`。
- V2 6 个 enqueue slot、单拍最多 6 个 load element 和 4 个 store element。
- LSQ enqueue 随机模式支持受权重控制的零入队空拍、中间数量和物理最大数量。
- V2 request 完整字段驱动、无 `canAccept/response` 的发送时序、redirect 后重试和软件 LSQ 分配确认。

本轮不支持：

- vector LS、`issueVldu`、segment 和 fixVl。
- MOU、AMO、CBO 的 enqueue、issue、writeback 和 commit 闭环。
- enqueue 前 directed `exceptionVec/trigger/flushPipe` 激励。
- issue hold、压力模式、boundary vseq、专项统计、RM、scoreboard、checker 和 coverage。

随机返回 0 只表示 LSQ enqueue sequence 主动发送一拍全零 idle，不等价于 DUT backpressure、issue hold
或压力模式；这些不支持项的边界保持不变。

unsupported op 继续由现有 `validate_main_table_entry()` 唯一拒绝。LSQ candidate 和 request setter
不得再增加一套 CBO、AMO 或 vector classifier。本 plan 不修改 issue loop、global redirect handler、
writeback、ROB commit、LSQ deq、pass/fail 或 terminal owner。

例外说明：问题四中的 `collect_lsq_candidates()` 只做 element 计数和 candidate 预览，本轮把该逻辑写成
multi-element/vector-ready 形式。当前 main table validation、`derive_op_behavior()` 和 request setter
仍不开放 vector LS；未来如果上游正式支持 vector LS，candidate 逻辑按 `behavior.num_ls_elem` 做
element 级分片。单个 vector uid 的 element 数超过当前 profile enqueue width 时，不把它视为 fatal，
而是按硬件 width 拆成多个 chunk；非 vector 或未声明可分片的 request 超过 width 仍应 fatal，避免同一
uid 永久重试。

V2/V3 的硬件结构差异只使用 compile profile 和 presence macro 隔离，不增加硬件结构 runtime plus。
所有新增 V2 运行期分支都放在“accept-response presence 不存在”的编译分支。V3 只保留当前字段和
方法入口的条件编译边界；本 plan 不新增或宣称补齐 V3 `wait/sample/response` 运行期功能。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] 本轮保持 scalar-only

- 来源：coding 前对照本 plan 范围声明、总控 plan 和用户已确认边界时发现，原问题四仍写有 vector
  chunk 与同 UID 跨拍进度，但本轮明确不支持 vector LS / `issueVldu`。
- 原 plan：Candidate 写成 vector-ready，并要求本轮实现 vector element 分片状态。
- 实现调整：本轮只接受 `behavior.num_ls_elem == 1` 的 scalar LDU/STU；Candidate 仍按
  `num_ls_elem` 类型累计，但不新增 vector chunk/progress 状态。vector 继续由现有 validation、
  `derive_op_behavior()` 和 FuType helper fail-fast，分片功能转入后续 vector 专项。
- 原因：vector 分片需要额外定义状态 owner、redirect/replay 恢复和终态合同，不能隐藏混入 scalar
  V2 接口适配。
- 影响范围：`memblock_lsqenq_dispatch_base_sequence.sv`、类型参数化和实现 review；不修改 issue loop。

### [IMPLEMENTATION_DELTA] 复用已完成的 V2 compile baseline

- 来源：总控 plan 已将 compile/width 基线归属到已完成的
  `plan/do/mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md`，当前源码没有
  `MEMBLOCK_DUT_PROFILE_V2/V3` selector。
- 原 plan：本专项重新引入排他的 V2/V3 profile selector，并描述 V3 8-slot/accept-response tuple。
- 实现调整：不修改 `tb.f`，不新增 profile selector。本轮只在现有 V2
  `memblock_compile_params.svh` 补齐 LSQ 专项缺失的 load/store enqueue width、uopIdx 和
  numLsElem 派生宏；现有 `MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP=0` 继续作为 V2 capability。
- 原因：本专项目标是 V2 可执行适配；重新建立 V3 编译基线会扩大范围并形成第二套版本权威。
- 影响范围：`memblock_compile_params.svh`、LSQ agent/sequence consumer 和实现 review；V3 不宣称验证。

### [IMPLEMENTATION_DELTA] Candidate 不保留 6/4 个空项

- 来源：原问题四仍要求 base LQ/SQ free count 先达到 6/4；这与此前确认的串行 UVM 软件分配模型冲突。
- 原 plan：只有 LQ 至少空 6 项、SQ 至少空 4 项时才允许形成 Candidate。
- 实现调整：只要求本拍累计 `load_elem_count <= MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH`、
  `store_elem_count <= MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH`，并分别不超过该拍入口快照的实际 LQ/SQ
  free count。不得额外执行 `base_lq_free >= 6` 或 `base_sq_free >= 4` gate。
- 原因：6/4 是单拍端口结构上限；当前 UVM 在 launch confirm 时立即预留资源，不复制 RTL
  registered credit 的提前余量。如果强制保留 6/4 个空项，LQ/SQ 尾部资源永远不可使用。
- 影响范围：`collect_lsq_candidates()`、LSQ admission flow 文档和实现 review；已有 pointer、free-count
  advance/cancel/release 公式不变。

### [IMPLEMENTATION_DELTA] 6/4 Gate 覆盖整个 LSQ Agent 行为面

- 来源：归档前独立review第7轮发现，原方案只在dispatch sequence的candidate路径累计6/4，但通用
  `lsqenq_agent_agent_default_sequence`会直接随机化xaction，仍可能生成5个或6个store。
- 原 plan：问题四只要求`collect_lsq_candidates()`限制load/store batch，没有定义随机default sequence
  和外部directed item进入driver时的同一合同。
- 实现调整：`lsqenq_agent_agent_xaction`新增batch约束，对6个slot中valid load/store分别计数并限制为
  compile-profile 6/4；`validate_v2_scalar_item()`在首次VIF写入前再次统计并fail-fast。dispatch candidate
  继续执行原有6/4和实际free-count过滤。
- 原因：xaction约束保证标准随机路径不产生非法item，driver检查保证关闭约束、手工赋值或其它sequence
  也不能绕过硬件端口能力；二者共同覆盖完整agent行为面。
- 影响范围：`lsqenq_agent_agent_xaction.sv`、`lsqenq_agent_agent_driver.sv`、Plan/review和接口/flow文档；
  不改变主表、allocation、issue、commit/deq或terminal owner。

源码级伪代码：

```text
xaction batch constraint：
  load_count = 六个slot中(valid && needAlloc==LQ)的数量；
  store_count = 六个slot中(valid && needAlloc==SQ)的数量；
  约束load_count <= MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH；
  约束store_count <= MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH；

driver validate_v2_scalar_item()：
  load_count=0；store_count=0；
  逐slot读取valid、needAlloc和payload；
  inactive slot继续执行全零合同检查；
  active load使load_count递增，超过compile load width则uvm_fatal；
  active store使store_count递增，超过compile store width则uvm_fatal；
  然后继续执行FuType、key范围和scalar字段检查；
  所有检查通过后send_pkt才写VIF。
```

中文文字伪代码：先由xaction约束保证标准default sequence随机化时不会生成超过6/4的batch；driver不信任
producer是否保持约束开启，所以在任何字段写入VIF前重新扫描六个slot。inactive slot不计数且必须全零，
active slot按`needAlloc`归入load或store；计数第一次越过compile上限就fatal并阻止非法item继续成为有效激励。
dispatch sequence仍在candidate阶段提前过滤，因此正常主路径不会依赖driver fatal来做仲裁。

### [IMPLEMENTATION_DELTA] `fuOpType` 与 Framework Metadata 合同闭环

- 来源：归档前独立review第8轮发现，xaction/driver虽然限制了`needAlloc/FuType`，但没有限制active
  slot的`fuOpType`；自定义compare回退路径也遗漏`wait_can_accept/ready_timeout/flush_epoch`。
- 原 plan：要求本轮只支持scalar load、software prefetch和普通store，并要求framework metadata进入
  automation/打印/compare，但没有列出agent层可复用的`fuOpType`合法集合和手工compare完整字段。
- 实现调整：xaction用两组宏值表唯一维护合法集合，LQ只接受普通load `0..6`和software prefetch
  `8/9/10`，SQ只接受普通store `0..3`；逐slot constraint直接`inside`值表，driver static helper读取
  同一值表完成fail-fast。
  `psdisplay()`和自定义compare回退路径覆盖全部五个framework metadata字段。
- 原因：通用default sequence和关闭constraint的directed item都不能绕过本轮scalar-only边界；仅凭
  `FuType`无法排除CBO等不支持opcode。手工compare覆盖UVM automation失败结果时，也必须重新比较全部
  本类功能metadata，否则不同flush epoch可能被误判为相等。
- 影响范围：`lsqenq_agent_agent_xaction.sv`、`lsqenq_agent_agent_driver.sv`、Plan/review和字段分析文档；
  不改变主表producer、dispatch setter、allocation、issue、commit/deq或terminal owner。

文字伪代码：

```text
xaction随机约束：
  active LQ slot要求fuOpType属于普通load 0..6或prefetch 8/9/10；
  active SQ slot要求fuOpType属于普通store 0..3；
  CBO 7/12/13/14、AMO和其它9-bit值均不可随机生成；

driver运行期复核：
  load/prefetch slot调用同一个LQ helper，不合法立即fatal；
  store slot调用同一个SQ helper，不合法立即fatal；
  复核发生在首次VIF赋值前，directed item不能绕过；

display/compare：
  打印wait_can_accept、ready_timeout、request_launched、aborted_by_redirect和flush_epoch；
  super.compare失败进入项目既有手工回退时，逐项重新比较上述五个metadata；
  任一字段不同都保持compare失败，不吞掉stale epoch或launch合同差异。
```

### [IMPLEMENTATION_DELTA] 当前 V2 物理展开拒绝非 6/6/4 Tuple

- 来源：归档前独立review第11轮发现，`MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`、
  `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH`和`MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH`虽是compile宏，但当前interface、
  xaction、driver和setter仍显式展开六个slot。只检查非零及width不超过slot，无法阻止宏值与物理字段面分叉。
- 原 plan：要求物理结构通过compile宏管理，未说明当前V2尚未具备任意tuple的全链路参数化能力。
- 实现调整：保留三个宏作为consumer的唯一引用入口；`check_compile_param_consistency()`进一步要求当前V2
  tuple严格等于6/6/4。非默认覆盖在任何sequence或driver激励前`uvm_fatal`；未来只有其它profile同步
  参数化全部显式consumer后，才能修改或放开该检查。
- 原因：较小slot值会让循环跳过仍被driver物理驱动的高slot，较大值会在case default处延迟fatal；两者
  都不能作为合法版本切换。当前最小安全方案是明确拒绝不完整profile，而不是假装已经通用参数化。
- 影响范围：`memblock_compile_params.svh`注释、`seq_csr_common::check_compile_param_consistency()`和
  implementation review；默认6/6/4行为、candidate、driver时序和公共状态不变。

文字伪代码：

```text
公共配置初始化时调用check_compile_param_consistency；
先执行既有非零和width不超过slot检查；
再比较当前LSQ slot/load/store compile tuple是否精确为6/6/4；
任一值不同就在产生激励前uvm_fatal，并打印实际tuple；
全部匹配才继续其它宽度、FuType和issue port一致性检查。
```

### [IMPLEMENTATION_DELTA] Streaming Gap 的 Display/Compare 回退闭环

- 来源：归档前独立review第11轮发现，LSQ xaction已约束并由driver复核`pre_pkt_gap/post_pkt_gap=0`，
  但custom compare在`super.compare()`失败后重置结果并手工比较时遗漏两个gap；`psdisplay()`也未打印。
- 原 plan：要求V2 streaming使用零gap，没有覆盖项目基类compare返回失败后的本类手工回退细节。
- 实现调整：`psdisplay()`打印pre/post gap；手工compare回退在framework metadata和payload之前显式比较
  两个gap，任一不同保持compare失败并打印双方值。`start/finish`时间戳仍按既有回退语义忽略。
- 原因：driver fatal只能阻止非法DUT激励，不能保证transaction compare正确；仅gap不同的item也必须被
  debug/未来scoreboard识别为不同。
- 影响范围：`lsqenq_agent_agent_xaction.sv`和implementation review；不改变random约束、driver、VIF或
  公共状态。

文字伪代码：

```text
psdisplay输出pre_pkt_gap和post_pkt_gap；
compare先调用super.compare；
如果进入项目既有手工回退：
  重新把本类比较结果初始化为相等；
  比较双方pre/post gap，任一不同则保持失败并打印差异；
  再按既有顺序比较framework metadata、V2 extra字段和普通payload；
不重新比较start/finish时间戳，保留原回退边界。
```

## 2. 问题一：LSQ 硬件结构仍有 V3 固定值

### V2 问题

当前公共宏仍以散落 `ifndef` 默认值为主，LSQ consumer 中还可能出现 V3 的 8-slot、36-bit FuType、
9-bit ROB value 和固定 `[4:0]` 假设。仅修改一个 interface 位宽不能保证 sequence、xaction 和 driver
使用同一版本结构。

### 修改原因

slot 数量、端口 presence 和 key/FuType 位宽在 elaboration 前已经确定，不能由 runtime plus 改变。
V3 编码直接裁剪到 V2 宽度还可能把有效 one-hot 位丢失后继续驱动。

### 修改方案与修改逻辑

`tb.f` 只选择一个 compile profile：`MEMBLOCK_DUT_PROFILE_V2` 或
`MEMBLOCK_DUT_PROFILE_V3`。`memblock_compile_params.svh` 在排他的 profile 分支中统一定义：

| 宏 | V2 | V3 | 主要 consumer |
|---|---:|---:|---|
| `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM` | 6 | 8 | candidate、xaction、setter、driver、monitor |
| `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH` | 6 | 6 | load element gate、LQ 接收门限 |
| `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH` | 4 | 4 | store element gate、SQ 接收门限 |
| `MEMBLOCK_DUT_FUTYPE_W` | 35 | 36 | xaction、setter、driver、monitor |
| `MEMBLOCK_DUT_ROB_VALUE_W` | 8 | 9 | key typedef、transaction、interface |
| `MEMBLOCK_DUT_LQ_VALUE_W` | 7 | 7 | key typedef、transaction、interface |
| `MEMBLOCK_DUT_SQ_VALUE_W` | 6 | 6 | key typedef、transaction、interface |
| `MEMBLOCK_DUT_MAX_UOP_SIZE` | 65 | 65 | `uopIdx` 合法上限的主参数 |
| `MEMBLOCK_DUT_UOP_IDX_W` | 7 | 7 | 由 `$clog2(MAX_UOP_SIZE+1)` 派生的 `uopIdx` 宽度 |
| `MEMBLOCK_DUT_VLEN` | 128 | 128 | `numLsElem` 上限的主参数 |
| `MEMBLOCK_DUT_MAX_LS_ELEM` | 16 | 16 | 由 `VLEN/8` 派生的 `numLsElem` 合法上限 |
| `MEMBLOCK_DUT_NUM_LS_ELEM_W` | 5 | 5 | 由 `$clog2(MAX_LS_ELEM)+1` 派生的字段宽度 |
| `MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP` | 0 | 1 | capability 一致性 |

`MEMBLOCK_DUT_LSQ_ENQ_ACCEPT_RESP_PORTS_PRESENT` 只在 V3 profile 定义。只有 presence macro 可以
包围当前 profile 不存在的成员，不能使用 runtime `if` 引用不存在的端口。

`MEMBLOCK_DUT_UOP_IDX_W`、`MEMBLOCK_DUT_MAX_LS_ELEM` 和 `MEMBLOCK_DUT_NUM_LS_ELEM_W` 必须直接从
上述主参数表达式派生，不能再作为可独立覆盖的第二主参数。

`memblock_dispatch_types.sv` 定义 `memblock_num_ls_elem_t`，供 seq package 内的 main transaction、
behavior 和 sequence 使用。先于 seq package 编译的 LSQ agent interface/xaction 直接使用
`bit [MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0]`，避免引用尚未编译的 typedef。

### 文字伪代码

```text
编译当前 worktree：
  tb.f 只定义一个版本 profile selector；
  如果同时定义 V2 和 V3，或两个都未定义，则停止编译；

展开 memblock_compile_params.svh：
  如果是 V2，定义 6-slot、35-bit FuType、8/7/6-bit key 和无 accept-response；
  如果是 V3，定义 8-slot、36-bit FuType、9/7/6-bit key 和有 accept-response；
  不接受同名 runtime plus 覆盖这些硬件结构；

调用 seq_csr_common::check_compile_param_consistency()：
  检查 slot、load/store width、key width、FuType width、uopIdx width 和 numLsElem width 均非零；
  检查 load/store width 不大于 slot 数；
  检查 UOP_IDX_W 等于 clog2(MAX_UOP_SIZE+1)；
  检查 MAX_LS_ELEM 等于 VLEN/8，NUM_LS_ELEM_W 等于 clog2(MAX_LS_ELEM)+1；
  检查 V2/V3 参数 tuple 与所选 profile 一致；
  检查 capability 数值与 presence macro 一致；
  任一基础结构不一致时 uvm_fatal，函数不修改 transaction、queue 或公共状态；

处理 FuType：
  request setter 调用现有 encode_and_fit_dut_futype()；
  helper 按当前 profile 的语义位生成 DUT FuType；
  如果内部编码不能无损表示到 V2 35 bit，则在驱动前 uvm_fatal；
  禁止先截断 V3 36-bit 编码再发送。
```

## 3. 问题二：V2 Request 字段链不完整

### V2 问题

V2 slot0..5 除公共字段外还有 `exceptionVec[23:0]`、`trigger[3:0]`、`fuOpType`、`flushPipe` 和
`lastUop`。这些字段虽然已出现在部分生成文件中，但 sequence setter、clear、xaction、driver、monitor
和 connect 尚未形成同一条完整字段链。

### 修改原因

只在 interface 或 driver 中补字段会让 active request 使用随机残值，idle/abort 清理也不能保证把全部
DUT input 清零。扁平展开的 `exceptionVec_*` 不能假定为一个可整体赋值的 packed 成员。

### 修改方案与修改逻辑

以下文件采用完全相同的 profile 分支和字段集合：

```text
lsqenq_agent_agent_interface.sv
lsqenq_agent_agent_xaction.sv
lsqenq_agent_agent_driver.sv
lsqenq_agent_agent_monitor.sv
tb/lsqenq_agent_connect.sv
memblock_lsqenq_dispatch_base_sequence.sv
```

V2 active scalar slot 的最终语义为：

| 字段 | valid slot | unused slot |
|---|---|---|
| `valid` | 1 | 0 |
| `needAlloc` | load=`01`，store=`10` | 0 |
| `fuType` | V2 无损编码 | 0 |
| `fuOpType` | `main_tr.fuOpType` | 0 |
| `exceptionVec[23:0]` | 0 | 0 |
| `flushPipe` | 0 | 0 |
| `trigger[3:0]` | 0 | 0 |
| `uopIdx` | 0 | 0 |
| `lastUop` | 1 | 0 |
| `robIdx` | `main_tr.get_rob_key()` | 0 |
| `lqIdx/sqIdx` | 本拍 candidate 预测 key | 0 |
| `numLsElem` | 1 | 0 |

`dut_inst.sv` 仍是当前版本生成的具体 wrapper：V2 worktree 只保留 V2 端口，V3 worktree 只保留 V3
端口，不把它改成同时包含两套 DUT 端口的条件文件。

### 文字伪代码

```text
构造 V2 interface 和 xaction：
  声明 slot0..5 的全部公共字段和 V2 extra 字段；
  ROB/LQ/SQ/FuType/uopIdx/numLsElem 位宽只消费 compile macro；
  ROB/LQ/SQ value 的合法范围分别使用 ROB_SIZE/LQ_SIZE/SQ_SIZE，不把整个 packed 范围当成合法队列项；
  uopIdx 的通用合法范围使用 0..MAX_UOP_SIZE-1，numLsElem 使用 0..MAX_LS_ELEM；
  每个 slot 的约束按 valid 条件化：
    valid=0 时 needAlloc和全部payload为0；
    valid=1 时 needAlloc只允许load/store，FuType只使用当前profile的LDU/STU one-hot，
      并固定uopIdx=0、lastUop=1、numLsElem=1、exceptionVec/flushPipe/trigger=0；
  排除固定 V3 FuType literal 和全部 vector one-hot；
  把全部字段加入项目现有 UVM field automation、约束、打印和 compare；

连接和驱动：
  connect 将每个 xaction/interface 字段连接到同名 V2 DUT input；
  send_pkt 搬运 valid slot 的全部字段；
  drive_idle 清零 slot0..5 的 needAlloc、valid 和全部 payload；

采样：
  monitor 的 mon_data() 只把当前 profile 真实存在的字段采到既有局部变量；
  X/Z 检查中的 uopIdx、numLsElem、FuType和key宽度改为消费同一 compile macro；
  保持现有 analysis-port 发布代码不启用，不创建 mon_tr，不调用 mon_item_port.write()；
  保留现有 analysis FIFO/RM consumer 配置，本轮不恢复 producer 发布，也不新增 consumer；
  unused slot 不依赖 randomize 默认值，由 sequence clear 显式写零。
```

该问题只改变字段采样和位宽覆盖，不改变 monitor/transaction 生命周期、driver ownership 或公共状态。

## 4. 问题三：Scalar Request 构造语义错误

### V2 问题

当前 `assign_lsqenq_slot()` 把 `uid[6:0]` 写入 `uopIdx`，`set_req_fields()` 由 caller 传入散落 raw
字段，并且 `numLsElem` 仍使用固定 `[4:0]`/`5'd0/1`。这会把框架内部 uid 错当成 DUT 微操作序号，
也无法统一填充 V2 extra 字段。

### 修改原因

本轮 scalar load/store 每条 request 只有一个 LS element 和一个 uop，所以必须使用
`uopIdx=0`、`lastUop=1`、`numLsElem=1`。uid 只用于框架定位 transaction，不是 DUT `uopIdx`。

### 修改方案与修改逻辑

`main_control_transaction::numLsElem` 和 `memblock_op_behavior_t::num_ls_elem` 改用
`memblock_num_ls_elem_t`；现有 producer 使用该类型的 0/1 cast。main transaction 可用 0 表示初始化
或非 LSQ 项，非零值不得超过 `MEMBLOCK_DUT_MAX_LS_ELEM`；本轮 scalar LSQ setter 进一步只接受 1。

`set_req_fields()` 继续作为唯一 payload setter，但改为接收 `main_tr + behavior + ROB/LQ/SQ key`。
`clear_lsqenq_xaction()` 和 `assign_lsqenq_slot()` 都通过该 setter 写字段，不再自行拼装第二份 payload。

### 文字伪代码

```text
set_req_fields(tr, slot, valid, main_tr, behavior, rob_key, lq_key, sq_key)：
  输入为一个 xaction、slot、valid、主表 transaction、已有 behavior 和 candidate key；
  只修改 xaction 当前 slot，不写主表、状态表、map、pointer 或 free count；
  如果 tr 为空或 slot 超过 compile slot 数，uvm_fatal；
  先把当前 slot 的局部 payload 全部初始化为零；
  如果 valid=0：
    要求 main_tr 为空、behavior 等于 make_default_behavior() 的无分配语义且传入 key 全零；
    把当前 profile 该 slot 的 qualifier 和 payload 全部写零后返回；
  如果 valid=1：
    检查 main_tr 非空；
    检查 behavior 只使用 LQ 或 SQ 之一，needAlloc 与 uses_lq/uses_sq 一致；
    检查 main_tr.numLsElem 和 behavior.num_ls_elem 都等于 1；
    检查 rob_key 等于 main_tr.get_rob_key()；
    调用 encode_and_fit_dut_futype() 生成无损 V2 FuType，失败时在写 vif 前 fatal；
    写入 uopIdx=0、lastUop=1、numLsElem=1；
    复制 main_tr.fuOpType 和 ROB/LQ/SQ key；
    写入 exceptionVec=0、flushPipe=0、trigger=0；
    按 slot 写入全部 V2 字段；
  函数不重新判断 CBO、AMO 或 vector 类型；

clear_lsqenq_xaction(tr)：
  输入为待复用 xaction，输出是全部 slot 已清零的同一对象；
  tr 为空时 uvm_fatal；
  调用 make_default_behavior() 得到 need_alloc=0、uses_lq/sq=0、num_ls_elem=0 的 idle behavior；
  循环 0 到 compile slot 数减 1；
  每个 slot 先把 needAlloc 写零，再调用
    set_req_fields(tr, slot, valid=0, main_tr=null, idle_behavior, zero_rob_key, zero_lq_key, zero_sq_key)
    清全部 payload；
  V2 把本地 wait/timeout 控制字段写零；
  不修改任何公共状态；

assign_lsqenq_slot(tr, slot, main_tr, behavior, lq_key, sq_key)：
  检查对象、slot 和 scalar behavior 合法；
  用 behavior.need_alloc 写当前 slot 的 needAlloc；
  调用 set_req_fields(tr, slot, valid=1, main_tr, behavior, main_tr.get_rob_key(), lq_key, sq_key)
    构造完整 request；
  删除 uid 入参，uid 不再成为 uopIdx 数据源。
```

## 5. 问题四：Candidate Gate 与每拍入队数量随机控制不完整

### V2 问题

当前 `collect_lsq_candidates()` 只用 runtime `enq_per_cycle` 限制公共 slot 总数，并按当前
LQ/SQ free count 逐项扣减，没有分别累计本拍 load/store element 数。V2 总 slot 为 6 时，现有逻辑
可能把 5 个或 6 个 scalar store 放进同一 packet，而 V2 单拍最多只能接收 4 个 store element。

现有 `get_enq_per_cycle()` 在随机模式下使用 `$urandom_range(MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM, 1)`，只能
均匀返回 `1..MAX`，不能主动生成零入队空拍，也不能分别控制 0、中间数量和最大数量的命中概率。

### 修改原因

`MEMBLOCK_ENQ_PER_CYCLE` 及 `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN` 只控制本拍公共 enqueue slot 总数，
不能表达 load/store 两类独立的硬件结构上限。load/store 最大 allocation width 属于版本编译期事实，
必须分别使用 `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH` 和 `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH`，不能再新增
同义 runtime plus。

零入队是测试框架主动插入的合法 idle 拍，不改变 DUT 物理结构。0、中间值和最大值的概率属于 testcase
运行期行为，应由三个 runtime 权重控制。权重必须表达三个类别的总权重，不能直接对 range 使用 `:=`
导致中间类别总权重随 `MAX-1` 再次放大。

V2 的 LQ/SQ 实际 `canAccept` 仍要求两侧分别保留至少一个物理 enqueue window，因此 Candidate 开始前
必须检查当前 LQ/SQ free count 分别不小于 6/4。测试框架在当前 item launch 后完成软件资源预留，
再构造下一批 Candidate，
不存在 RTL `RegNext(canAccept)` 对下一批 request 的提前授权，所以不能把 RTL controller 的下一拍
credit 公式复制成 `tentative + 6/4 reserve`。本批只需同时满足：

```text
公共 candidate slot 数不超过现有 runtime 总量限制；
load/store element 累计数不超过当前 profile 的编译期 6/4 上限；
当前 LQ/SQ free count 先满足物理 6/4 接收窗口；
本批 load/store element 累计数不超过对应队列的实际 free count。
```

这既避免发送超过版本结构能力的 packet，也不会因为额外 reserve 让软件 LQ/SQ mirror 永远无法使用
最后 6/4 个 entry。

### 修改方案与修改逻辑

不新增 load/store enqueue runtime limit 或 per-type 随机 plus，不删除或复用 issue 阶段的
`MEMBLOCK_*_PIP_NUM_LIMIT`。`MEMBLOCK_ENQ_PER_CYCLE` 继续只表示固定模式的本拍总 slot 数，合法范围
保持 `[1:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM]`；固定模式不允许配置 0。

`MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=1` 时，`get_enq_per_cycle()` 的返回范围扩展为
`[0:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM]`，并新增三个公共测试框架 runtime plus：

| 参数 | 默认配置 | 有效语义 |
|---|---:|---|
| `MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT` | `0` | 选择本拍返回 0、主动发送 idle 的类别总权重 |
| `MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT` | `-1`（AUTO） | `-1` 时有效权重派生为 `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1`；显式非负值直接作为 `1..MAX-1` 类别总权重 |
| `MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT` | `1` | 选择本拍返回物理最大 slot 数的类别总权重 |

`MIDDLE_WEIGHT=-1` 只作为 default cfg 无法书写 compile 表达式时的 AUTO sentinel；进入随机 solver 前
必须解析成非负的 effective weight。V2 默认 effective 权重为 `ZERO/MIDDLE/MAX=0/5/1`，因此随机模式
默认仍使 `1..6` 每个数值各占 `1/6`，不改变原 `$urandom_range(6,1)` 的分布；后续 profile 改变 slot
数时由 compile localparam 自动派生，不能在 `seq_csr_common` 中写死 5。
MIDDLE raw snapshot 必须保留 signed `int`，AUTO 解析前不得调用 `get_non_negative_int()` 或转换为
unsigned；解析后的 effective weight 再保存为 unsigned 供随机 solver 使用。

随机模式采用 SystemVerilog 内建 `std::randomize(... with { ... dist ...; })`，不新增自定义随机数生成器，
也不把 `$urandom_range()` 与手工累计权重混用。先按三个总权重选择 `ZERO/MIDDLE/MAX` 类别；命中
`MIDDLE` 后再在 `[1:MAX-1]` 内均匀随机一个数值。这样三个 plus 控制的是类别总概率，而不是给每个
中间数值重复分配一次完整权重。

例如 V2 显式配置 `ZERO/MIDDLE/MAX=1/8/1` 时，返回 0 的概率为 10%，返回 1..5 的类别总概率为
80%且每个中间值为 16%，返回 6 的概率为 10%。该实现使用 UVM sequence 可直接调用的 SystemVerilog
约束随机化能力；UVM 本身不再增加一套权重选择 API。

三个权重控制的是 `total_slot_limit` 目标上限，不承诺最终 packet 一定达到该数量。`MAX` 命中只表示
本拍允许最多扫描物理 slot 数；连续 uid 前缀、load/store 6/4 gate、LQ/SQ free count、主表尾部和
non-LSQ 边界仍可使实际 candidate 数小于目标。`ZERO` 命中则明确禁止本拍建立新 LSQ candidate。

权重加载和合法性集中在 `seq_csr_common::validate_and_clamp()`：`ZERO/MAX` 不得为负，`MIDDLE` 只允许
`-1` 或非负值；随机模式只要求解析后的三类总权重大于0，因此`ZERO/MIDDLE/MAX=1/0/0`是合法的
idle-only配置。`MAX<=1` 时必须禁止非零 `MIDDLE`。任一约束随机化失败均`uvm_fatal`，不得静默回退
成均匀随机。总权重使用`longint unsigned`逐项求和，避免三个`int`表达式先溢出。

每次调用 `collect_lsq_candidates()` 时仍只通过 `get_enq_per_cycle()` 采样一次本拍总量上限。若返回 0，
立即返回空 candidate，不读取 next uid，不检查或推进 LQ/SQ pointer/free，不修改主表、状态表或 map；
`send_lsqenq_cycle()` 复用现有空 candidate 路径发送显式全零 idle item。若上一批仍在 pending-sample，
该 idle 边界仍可完成上一批 sample；本拍是否计为 progress 继续由现有 sample/launch progress OR 决定。
若没有 pending sample，随机 0 产生的纯 idle 不算 admission progress，继续累计现有 no-progress 计数；
权重配置不得绕过 watchdog 或修改 global stop/terminal 条件。

`collect_lsq_candidates()` 使用当前 compile profile 提供的 load/store width 宏执行共享过滤，V2
展开为 6/4，后续版本只切换 compile profile，不修改 Candidate 算法。候选仍从
`get_next_new_admit_uid()` 开始，只预览连续 uid 前缀，并复用 `lsq_ctrl_model` 现有 pointer advance
helper。

Candidate 的 load/store 统计以 `behavior.num_ls_elem` 为单位，不假设每个 request 永远只有 1 个
element。当前 scalar LDU/STU 仍由 `derive_op_behavior()` 产生 `num_ls_elem=1`；若未来 vector 或其它
multi-element request 在上游被允许，本函数按 element 数累计。遇到第一个“累计后”超过 load/store
width 的 uid 时截断本批；该 uid 不入本批、不被 drop，也不能跳过后继续选择更年轻 uid，下一拍仍从该
uid 重试。

向量模式下，若单个 vector uid 的 `num_ls_elem` 大于对应 enqueue width，`collect_lsq_candidates()`
不 fatal，而是生成一个不超过本拍剩余 width/free count 的 chunk。chunk 使用同一个 uid 和同一个
main transaction，`behavior.num_ls_elem` 在 candidate 输出中改写为本 chunk 的 element 数，并记录
chunk 起始 LQ/SQ key。后续 confirm 只有在 driver launch 成功后才提交该 chunk 的 allocation，并更新
该 uid 的 vector enqueue progress。该 uid 还有剩余 element 时，`next-admit uid` 不能越过它；下一拍
继续从同一 uid 的剩余 element 构造 chunk。非 vector 或未声明可分片的 multi-element request 若单笔
delta 超过硬件 width，仍 `uvm_fatal`，因为 collect 无权猜测如何拆分该操作。Candidate 阶段本身不修改
主表、状态表、map、真实 pointer 或 free count。

### 文字伪代码

```text
seq_csr_common 参数加载与检查：
  读取MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT；要求值大于等于0；
  读取MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT；只允许-1或大于等于0；
  读取MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT；要求值大于等于0；
  如果MIDDLE_WEIGHT为-1：
    effective_middle_weight = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM - 1；
  否则：
    effective_middle_weight = MIDDLE_WEIGHT；
  如果RAND_EN=1：
    检查ZERO + effective_middle + MAX大于0；
    如果MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM小于等于1且effective_middle不为0，则uvm_fatal；

seq_csr_common::get_enq_per_cycle()：
  如果RAND_EN=0：
    返回固定MEMBLOCK_ENQ_PER_CYCLE；该值仍必须在[1:MAX]；
  max_enq = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM；
  bucket类型只包含ENQ_COUNT_ZERO、ENQ_COUNT_MIDDLE和ENQ_COUNT_MAX；
  使用std::randomize(bucket)和dist：
    ENQ_COUNT_ZERO   := zero_weight；
    ENQ_COUNT_MIDDLE := effective_middle_weight；
    ENQ_COUNT_MAX    := max_weight；
  如果randomize失败，则uvm_fatal；
  如果bucket为ENQ_COUNT_ZERO，返回0；
  如果bucket为ENQ_COUNT_MAX，返回max_enq；
  如果bucket为ENQ_COUNT_MIDDLE：
    使用std::randomize(result)约束result inside {[1:max_enq-1]}；
    如果randomize失败，则uvm_fatal；
    返回result；

collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)：
  清空五组输出 queue，保证不存在上一拍残留；
  如果原 global flush gate 有效，返回无 candidate；
  调用get_enq_per_cycle()一次，取得本拍total_slot_limit；
  如果total_slot_limit为0，立即返回无candidate，不读取uid/pointer/free且不产生公共状态副作用；
  zero路径后total_slot_limit必定位于1到MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM之间；
  V2 把当前 LQ/SQ free count 保存为本拍不变的 base_lq_free/base_sq_free；
  如果 base_lq_free 少于 MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH，返回无 candidate；
  如果 base_sq_free 少于 MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH，返回无 candidate；
  复制 LQ/SQ enqueue pointer 到局部变量 lq_ptr_tmp/sq_ptr_tmp；
  load_elem_count=0，store_elem_count=0；
  从 next-admit uid 开始按顺序预览，最多收集total_slot_limit项：
    遇到主表末尾、已有状态、global flush 或 non-LSQ 项时停止当前连续前缀；
    调用 derive_op_behavior() 读取现有分类结果，不增加第二套 unsupported-op classifier；
    full_load_delta = behavior.uses_lq ? behavior.num_ls_elem : 0；
    full_store_delta = behavior.uses_sq ? behavior.num_ls_elem : 0；
    该计算对 scalar、vector 和其它 future multi-element request 共用；
    当前源码仍在validate_main_table_entry()和derive_op_behavior()拒绝vector LS；
    如果是vector/multi-element可分片request：
      remaining_elem = total_elem - committed_elem_for_this_uid；
      本拍剩余load宽度 = MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH - load_elem_count；
      本拍剩余store宽度 = MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH - store_elem_count；
      本拍剩余LQ资源 = base_lq_free - load_elem_count；
      本拍剩余SQ资源 = base_sq_free - store_elem_count；
      chunk_elem = min(remaining_elem, 对应本拍剩余width, 对应本拍剩余free)；
      如果chunk_elem为0，则停止收集；
      load_delta = behavior.uses_lq ? chunk_elem : 0；
      store_delta = behavior.uses_sq ? chunk_elem : 0；
      chunk_behavior = behavior，并把chunk_behavior.num_ls_elem改为chunk_elem；
    否则：
      load_delta = full_load_delta；
      store_delta = full_store_delta；
      chunk_behavior = behavior；
      如果单笔load_delta超过MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH，则uvm_fatal，避免同一uid永久重试；
      如果单笔store_delta超过MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH，则uvm_fatal，避免同一uid永久重试；
    tentative_load = load_elem_count + load_delta；
    tentative_store = store_elem_count + store_delta；
    如果 tentative_load 超过 MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH，则停止收集；
    如果 tentative_store 超过 MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH，则停止收集；
    如果 tentative_load 超过 base_lq_free，则停止收集；
    如果 tentative_store 超过 base_sq_free，则停止收集；
    lq_key = lq_ptr_tmp，sq_key = sq_ptr_tmp；
    保存 uid、transaction、chunk_behavior 和当前预测 key 到五组等长 queue；
    如果behavior.uses_lq，则lq_ptr_tmp = advance_lq_key(lq_ptr_tmp, load_delta)；
    如果behavior.uses_sq，则sq_ptr_tmp = advance_sq_key(sq_ptr_tmp, store_delta)；
    load_elem_count = tentative_load；
    store_elem_count = tentative_store；
    只推进局部 pointer，不推进真实 lsq_ctrl pointer；
  返回 queue 是否非空；

  多笔累计导致的load/store上限命中只截断当前连续前缀；
  vector/multi-element可分片request的单笔delta超过width时按chunk拆分；
  非vector或不可分片request的单笔delta超过对应硬件宽度时直接uvm_fatal；
  对累计超限uid不跳过、不清其状态、不推进公共next-admit uid；
  下一拍仍从该uid开始重新构造candidate。
```

修改后增加 compile-profile admission 合法性 gate，并把随机模式从均匀 `1..MAX` 扩展为权重控制的
`0/MIDDLE/MAX`；固定模式、连续 uid 顺序、pointer 公式、non-LSQ admission、issue pipe 参数和公共
状态 owner 保持不变。

## 6. 问题五：V2 不存在 `canAccept/response`

### V2 问题

V2 DUT 顶层只有 LSQ enqueue request input，没有 `canAccept/response`。当前 sequence 仍把 active
xaction 设置为 wait，driver 会进入 `wait_lsq_can_accept()` 并重复发送同一 request，直到不存在的
ready 条件满足或 timeout。若把每个 active item 自身改成“drive 一拍、再等待一拍 sample 后
`item_done()`”，虽然能去掉 response 等待，但稳态仍只能两拍发送一个 batch。

### 修改原因

V2 request 从一个 driver clocking 边界 drive 后，到下一个边界才被 DUT 采样；这是单笔 request
固有的一拍延迟，不代表 driver 必须空等一拍。driver 可以在 DUT 采样上一批的同一边界立即覆盖为
下一批，从而保持每拍一个 batch。sequence 的 `finish_item()` 只能表示“本批已 launch 到 VIF”，
不能表示 DUT 已采样，更不能表示收到 response。

### 修改方案与修改逻辑

V2 `configure_from_plus()` 保留 sequence enable 和 no-progress 参数，直接把本地 `ready_timeout` 置零，
不读取 timeout getter。公共 `seq_csr_common::load_from_plus()` 仍解析该兼容参数并执行非负合法性检查，
因此显式负值仍会 fatal；`validate_and_clamp()` 则在 V2 跳过 LSQ ready timeout 的零值 warning/clamp。
V3 仍保留原 timeout 字段及 wait/response 方法入口，其功能扩展不属于本 plan。

V2 xaction 增加 framework-only metadata `memblock_dispatch_request_launched`，并把现有
`memblock_dispatch_aborted_by_redirect` 的 V2 语义收窄为“launch 前因 flush/epoch 失效而未发送”。
这两个字段进入 constructor 默认值、UVM automation、`psdisplay()` 和自定义 `compare()`，但不连接
DUT。V2 dispatch sequence 创建的
idle/active item 均设置 `wait_can_accept=0`、`ready_timeout=0`、`pre_pkt_gap=0`、`post_pkt_gap=0`。

V2 driver 使用 clock-first streaming：每轮先等待一个 driver clocking 边界，使 DUT 采样上一拍接口值；
随后 `try_next_item()` 取得本拍要覆盖到 VIF 的 item。active item 成功 launch 后立即 `item_done()`，
不在本 item 内等待下一边界，也不立即撤销 request；下一拍由新 active item覆盖，无 item或 idle item时
才调用 `drive_idle(DRV_0)`。因此连续 A/B/C 三批可以分别在相邻三个边界 launch。
该时序复用 interface 现有 `drv_cb` output hold skew，不修改 clocking block 或 DUT 采样定义。

### 文字伪代码

```text
lsqenq_agent_agent_driver::main_phase() 的 V2 item 分支：
  每轮首先等待 @vif.drv_mp.drv_cb；该边界采样的是上一轮已放到 VIF 的值；
  先把req置null，再调用 seq_item_port.try_next_item(req)，禁止复用上一轮item句柄；
  如果没有 item：
    调用 drive_idle(DRV_0)，结束本轮；
  如果取得 item：
    若 pre_pkt_gap或post_pkt_gap非零则uvm_fatal；V2 dispatch item合同固定为0；
    request_launched=0，aborted_by_redirect=0；
    对当前 profile 的 request valid 位做 OR，判断 item 是否 active；
    如果是 idle item：
      检查全部needAlloc、valid、qualifier和payload均为0，否则uvm_fatal；
      调用 drive_idle(DRV_0)；
    否则如果 flush 正在进行或 item.flush_epoch不等于当前epoch：
      aborted_by_redirect=1；
      调用 drive_idle(DRV_0)，不发送 request；
    否则：
      调用 send_pkt(req) 一次，把完整 request 覆盖到 VIF；
      request_launched=1；
      不在此处等待下一 clocking 边界，也不立即 drive_idle；
    调用 item_done()；
  V2 全程不调用 wait_lsq_can_accept() 或 sample_lsqenq_resp()；

连续 batch 时序：
  C0边界：DUT采样旧idle；driver launch A并item_done(A)；sequence预留A资源；
  C1边界：DUT采样A；driver launch B并item_done(B)；sequence先开放A issue，再预留B资源；
  C2边界：DUT采样B；driver launch C并item_done(C)；sequence先开放B issue，再预留C资源；
  单批仍有launch到sample的一拍延迟，但稳态吞吐为每拍一个batch；

send_pkt(tr)：
  在首次 vif 赋值前逐 slot 检查 scalar request 合同；
  valid=0 时检查 needAlloc和全部payload为零；
  valid=1 时检查 needAlloc与LDU/STU FuType匹配，并检查
    uopIdx=0、lastUop=1、numLsElem=1、exceptionVec/flushPipe/trigger=0；
  任一 scalar 合同不满足时 uvm_fatal，不允许先部分驱动再报错；
  检查通过后只搬运当前 profile 的全部 request 字段，不等待时钟、不改状态；

drive_idle(cfg.drv_mode)：
  V2 build 合同保证传入 mode 等于 DRV_0；
  清零当前 profile 的全部 needAlloc、valid、qualifier 和 payload；
  不保留上一笔 request 的任何 extra 字段。
```

## 7. 问题六：V2 Confirm 错用 Response Helper，且 Launch 与 Sample 未分层

### V2 问题

当前 `confirm_lsq_candidates()` 把 candidate 预测 key 传给 `commit_allocate_with_resp()`，等价于把软件
预测伪装成 DUT response。与此同时，`commit_allocate_with_resp()` 又复制了一套 main/status/map、
pointer 和 free-count 更新代码，形成两个 allocation owner。

改为无 response 后还有一个时序问题：若 driver launch 返回后立即执行完整 `complete_admission()`，
issue 可能早于 DUT 下一边界采样 LSQ request；若把 allocation 也全部推迟到下一边界，sequence 又无法
用更新后的 pointer/free count 构造下一拍 batch，破坏每拍 streaming。

### 修改原因

V2 没有真实 response，软件只能依据 driver 的 launch 结果维护镜像。资源预留必须在 launch 后完成，
使下一批看到正确 pointer/free count；`issue_ready` 必须等到下一 driver 边界，确保 DUT 已有机会采样。
公共状态仍只能由一个 allocation owner 更新，避免 key、状态和资源计数分叉。

### 修改方案与修改逻辑

在 `memblock_lsqenq_dispatch_base_sequence` 增加一组单深度 pending-sample 状态：

```text
bit              v2_pending_sample_valid
memblock_uid_t   v2_pending_sample_uids[$]
int unsigned     v2_pending_sample_epoch
longint unsigned v2_pending_sample_launch_cycle
```

现有 `confirm_lsq_candidates()` 保留函数名以缩小改动，但职责改为“确认当前 item 已 launch并预留
allocation”，不再调用 `complete_admission()`。每次当前 item 的 `finish_item()` 返回时，先调用新增
`complete_v2_pending_sample()` 处理上一批，再调用 `confirm_lsq_candidates()` 预留当前批；这一顺序
保证 C1 先完成 A 的 sample，再登记 B 为下一批 pending。

launch 成功后，`confirm_lsq_candidates()` 重新 `preview_allocate()` 并比较使用中的预测 key，随后逐项
调用唯一 `commit_allocate()`，立即更新 active/enq、key map、pointer 和 free count，但不设置
`issue_ready`。整批 uid、dispatch/flush epoch 和 dispatch service cycle 保存到 pending-sample。这里的
`v2_pending_sample_epoch` 是 `tr.flush_epoch`，不是后续专项新增的 reservation launch epoch。下一 driver
边界返回后，若 epoch 仍有效且无 flush，再逐 uid 调用原 `complete_admission()` 开放 issue route；
若 epoch 已失效，则不开放 issue，由全局 redirect handler 和原 pending cancel 流程回退 reservation。
`v2_pending_sample_launch_cycle` 只用于日志和时序诊断，不作为 sample 完成条件；sample 完成的唯一边界
是下一次 driver `item_done()` 使 `finish_item()` 返回。

共享的 `commit_allocate_with_resp()` 保留为有真实 response 时的比较入口，但改成“response 比较 wrapper”：只比较
behavior 实际分配的 key，匹配后调用 `commit_allocate()`，不再直接写公共状态或 pointer。V3 成功路径
的 uid、key 和状态结果不变，只消除重复状态更新实现和 unused key 的无意义比较。

non-LSQ admission 当前可零时间完成，不能把它误当作 driver sample 边界。若存在 pending LSQ batch且
下一 uid 是 non-LSQ，sequence 必须先发送一个全零 idle item；该 `finish_item()` 返回后先完成上一批
sample，再执行原 non-LSQ admission。连续 LSQ batch 直接由下一 active item 提供该边界，不插 idle。
主循环退出前若仍有 pending batch，也发送一个 trailing idle item完成或按最新 epoch 作废该 batch，
防止最后一批永远不进入 issue route。

“下一 uid 是 non-LSQ”只通过现有 `next_uid_needs_lsq_admission()` 只读取得 uid/main/behavior，再判断
`behavior.need_alloc==0`；不得调用有状态副作用的 admission helper提前探测，也不得新增第二套 op
classifier。

### 文字伪代码

```text
send_lsqenq_cycle(cycle_idx, has_progress)：
  has_progress=0；
  调用 apply_pending_lsq_cancels()；
  如果 pending_sample_valid且下一uid是non-LSQ：
    构造并发送一拍全零idle item；
    finish_item返回后调用complete_v2_pending_sample(has_progress)；
    使用独立non_lsq_progress调用原admit_non_lsq_if_ready()；
    has_progress = has_progress或non_lsq_progress，然后返回；
  如果没有pending sample：
    使用独立non_lsq_progress调用admit_non_lsq_if_ready()；
    若成功则把non_lsq_progress OR进has_progress，保持原零时间non-LSQ路径并返回；
  调用collect_lsq_candidates()并构造active或idle item；
  active/idle item均设置wait_can_accept=0、ready_timeout=0、pre/post gap=0；
  通过start_item/finish_item交给driver；
  finish_item返回表示本边界已采样上一批、当前item已处理launch；
  先调用complete_v2_pending_sample(has_progress)结算上一批；
  若当前candidate非空，再调用confirm_lsq_candidates()处理当前active item的launch reservation；
  当前为idle item时不调用confirm，不为idle建立reservation；
  两个helper都把has_progress作为inout，只能置1不能清0，最终值为sample_progress或launch_progress；

complete_v2_pending_sample(inout has_progress)：
  如果pending_sample_valid=0，直接返回；
  如果pending epoch等于当前epoch且global flush无效：
    对pending uids逐项检查active/enq reservation仍存在；
    调用complete_admission(uid)，保持原CSR runtime event drain，并只在此边界设置issue_ready、进入
      LOAD/STA/STD issue queue；
    has_progress=1；
  否则：
    不调用complete_admission，不开放issue；
    不在本helper手工释放reservation；全局redirect handler已识别或随后识别active mapping，
      后续由pending_lq/sq_cancel_count回退；
  清空pending uids并清pending_sample_valid；

confirm_lsq_candidates(tr, uids, trs, behaviors, lq_keys, sq_keys, inout has_progress)：
  输入为 driver 返回的 xaction 和五组等长 candidate queue；
  inout has_progress，成功时只置1并产生launch reservation，不产生issue-route副作用；
  检查 tr 非空且五组 queue 长度一致，基础合同失败时 uvm_fatal；
  如果 request_launched=0：
    若active item未标记aborted_by_redirect，则uvm_fatal；
    整批返回，不调用 allocation owner，不修改 main/status/map/pointer/free count；
    本函数不新增post-release guard，也不写全局flush状态；
  如果 request_launched=1且aborted_by_redirect=1，uvm_fatal；
  如果 request_launched=1但上一批pending尚未清空，uvm_fatal；
  按 candidate 顺序处理：
    调用 preview_allocate() 只读当前 pointer/free count，得到 expected LQ/SQ key；
    如果 behavior 使用 LQ，只比较 predicted LQ key；
    如果 behavior 使用 SQ，只比较 predicted SQ key；
    使用中的 key 漂移时在 commit 前 uvm_fatal；
    调用 commit_allocate()：由唯一 owner 写 main transaction、active/enq status、key map、pointer 和 free count；
  保存整批uids、tr.flush_epoch和当前dispatch service cycle到pending_sample；
  pending_sample_valid=1，has_progress=1；
  V2 不调用 commit_allocate_with_resp()，也不构造常量 DUT response；

drive_lsqenq_loop()退出：
  global_stop检查命中时先判断pending_sample_valid，不直接break；
  如果pending_sample_valid=1，构造wait/timeout/gap和全部DUT字段均为0的trailing idle item；
  通过start_item/finish_item发送该item；
  finish_item返回后调用complete_v2_pending_sample()；
  然后才允许sequence退出；

lsq_ctrl_model::commit_allocate_with_resp(uid, behavior, tr, dut_lq_key, dut_sq_key)：
  检查 transaction、uid 和 LSQ behavior 基础合同；
  调用 preview_allocate() 只读预期 key，不更新状态；
  behavior 使用 LQ 时只比较 DUT LQ key与预期 LQ key；
  behavior 使用 SQ 时只比较 DUT SQ key与预期 SQ key；
  使用中的 response key 不匹配时 uvm_fatal；
  匹配后调用唯一 commit_allocate() 完成全部公共状态和 pointer/free-count 更新；
  本 wrapper 不再直接赋值 main/status/map/pointer/free count。
```

## 8. 问题七：Redirect/Flush 期间 LSQ Request 需要统一受控

### V2 问题

redirect 可能发生在 request launch 前、launch 后但 DUT sample 前，或 sample/issue-ready 之后。只在
`confirm_lsq_candidates()` 根据当前 request 的 abort 结果处理 redirect，会漏掉 flush 已经出现但 request
尚未构造、或 request 构造后 driver 才看到 epoch 失效的情况。

### 修改原因

当前测试框架已经有全局 flush/redirect 标志、`dispatch_flush_epoch` 和 driver 侧
`aborted_by_redirect` 机制。代码结构上，`apply_redirect_flush()` 在清全局标志前已经执行 redirect range
flush、`rollback_max_enqueued_uid()` 和 pending LQ/SQ cancel 累计；LSQ sequence 每拍入口先
`apply_pending_lsq_cancels()`，再构造新的 LSQ candidate。因此本 plan 不把“flush 解除后固定等待 N 个
cycle”作为必需修复，也不新增不可证明拍数的 retry guard。

真正需要保证的是 LSQ enqueue 的三段逻辑使用同一套现有保护：

```text
collect 阶段：
  flush/redirect 有效时不读取或推进 LSQ candidate；

driver launch 阶段：
  flush 正在进行或 transaction epoch 已过期时，不把 request launch 到 VIF；

confirm 阶段：
  driver 未 launch时不建立reservation；driver已经launch时不再按当前flush/epoch二次否决，始终建立reservation；
  保存的transaction epoch只在下一sample边界决定是否开放issue。
```

若后续日志证明 `apply_redirect_flush()` 清标志后仍存在旧 epoch sample/cancel/mirror 收尾影响新 LSQ
admission，再追加 post-release guard 或调整 owner 顺序；该保护作为后续可选方案，不进入本轮必改逻辑。

### 修改方案与修改逻辑

不新增任何本地 release-cycle/retry 字段，也不新增 retry guard 编译宏。继续复用现有：

```text
common_data_transaction::issue_blocked_by_global_flush()
memblock_sync_pkg::dispatch_flush_epoch
memblock_sync_pkg::dispatch_flush_in_progress
lsqenq xaction 的 memblock_dispatch_flush_epoch
lsqenq xaction 的 memblock_dispatch_aborted_by_redirect
新增 V2 framework-only memblock_dispatch_request_launched
```

`collect_lsq_candidates()` 在读取 `get_next_new_admit_uid()`、LQ/SQ pointer 和 free count 前先调用
`admission_blocked_by_flush()`。blocked 时直接返回空 candidate，不创建 launch reservation，也不修改
pointer、free count、主表、状态表或 map。

V2 driver 在每个 clock-first launch 边界检查 `dispatch_flush_in_progress` 和 item 保存的
`memblock_dispatch_flush_epoch`。如果 flush 正在进行或 epoch 已变化，则设置
`memblock_dispatch_aborted_by_redirect=1`、`memblock_dispatch_request_launched=0` 并 drive idle；否则只发送一次
request，设置 `request_launched=1`。

`confirm_lsq_candidates()` 只读取 driver 返回的 `request_launched`、`aborted_by_redirect` 和 epoch。
只有 `request_launched=0` 的 launch 前 abort 才整批不预留资源；一旦 `request_launched=1`，即使
confirm 时 global flush 已开始或 batch epoch 已失效，也必须调用唯一 allocation owner 建立 reservation并
登记 pending sample。epoch 只在下一边界控制是否开放 issue，不能反向取消已经放到 VIF 的 request。
该归档后合同的 token/sample 增量由 MMIO/status `undo` plan 唯一 coding；confirm 不写 retry guard，也不清
全局 flush 状态。

三类 redirect 时点按以下方式处理：

```text
launch前redirect：
  driver检测flush/epoch失效，request_launched=0、aborted_by_redirect=1；
  sequence不创建reservation，也不登记pending sample；

launch后、sample前redirect：
  launch reservation已经存在；
  下一边界complete_v2_pending_sample()因epoch失效而不开放issue；
  prepare_uid_for_redirect_reissue()依据active LQ/SQ mapping累计pending cancel；
  LSQ sequence后续apply_pending_lsq_cancels()回退pointer/free count；

sample或issue-ready后redirect：
  保持原全局redirect handler、active mapping和cancel/reissue生命周期；
  本plan不新增第二套清理路径。
```

### 文字伪代码

```text
collect_lsq_candidates()：
  在读取next uid、pointer或free count前调用admission_blocked_by_flush()；
  如果global flush/redirect仍有效，返回空candidate；
  blocked路径不修改主表、状态表、queue、map、pointer或free count；
  flush标志清除后不额外等待固定cycle，下一次LSQ cycle先apply_pending_lsq_cancels()再重新收集candidate；

driver V2 launch：
  每轮先等待driver clocking边界；
  取得item后检查dispatch_flush_in_progress和item.flush_epoch；
  如果flush有效或epoch mismatch：
    request_launched=0；
    aborted_by_redirect=1；
    drive_idle(DRV_0)，不发送request；
  否则：
    send_pkt(item)一次；
    request_launched=1；
    aborted_by_redirect=0；
  item_done()；

confirm_lsq_candidates()：
  只读取driver给出的request_launched/aborted_by_redirect和transaction epoch；
  如果request_launched=0：
    要求aborted_by_redirect=1或当前global flush有效或transaction epoch已失效；
    返回，不建立reservation；
  如果aborted_by_redirect=1且request_launched=1，uvm_fatal；
  不再因当前global flush有效或transaction epoch失效而返回；
  按问题六的launch reservation流程调用唯一allocation owner并登记pending sample；
  由MMIO/status专项紧接着建立(uid,reservation_launch_epoch) token；
  独立保存transaction dispatch/flush epoch，供下一边界issue gate使用；
  confirm不新增post-release等待，不写全局flush状态；

complete_v2_pending_sample()：
  epoch mismatch时只拒绝本批开放issue；
  不直接回退pointer/free count，也不写waiting/release字段；
  redirect handler和apply_pending_lsq_cancels()仍是cancel及资源回退owner。
```

### 8-A. 归档后跨专项依赖边界

本 plan 已完成的 `request_launched`、单深度 pending-sample 和 `commit_allocate()` 单一 allocation
owner，是后续 redirect cancel 对账可复用的基础。尚未实现的 reservation 动态实例 token、统一
DUT sample sequence、per-redirect record、DUT cancel snapshot 和 global-stop 收敛 gate，全部由
`AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md`
唯一描述和执行。该专项把本 plan 的 UID-only pending queue升级为reservation token queue；现有
`v2_pending_sample_epoch`仍是独立batch dispatch/flush epoch并继续控制issue gate；同时删除当前源码中
`request_launched=1`后按confirm时flush/epoch提前返回的旧分支，确保每个真实launch都有allocation/token。
本 `do` plan 不复制其它待实现字段或record算法，具体coding仍由该`undo` plan唯一拥有。

## 9. 问题八：LSQ Idle 驱动模式不受约束

### V2 问题

LSQ driver 的 reset、no-item、idle item 和 launch 前 abort 都调用通用 `drive_idle(cfg.drv_mode)`。如果
`drv_mode` 被配置为非 `DRV_0`，idle 拍可能驱动 valid 或 X，形成非 sequence 所有的 enqueue。

### 修改原因

LSQ enqueue 是明确的主动 sequence flow，idle 的唯一合法语义是全部 DUT input 为零。运行到 reset、
无 item 或 launch abort 后才发现错误配置会污染 DUT 状态，必须在 driver 启动前拒绝。

### 修改方案与修改逻辑

`lsqenq_agent_agent_cfg` 增加 `soft drv_mode == DRV_0`，允许上层显式配置但不给出随机非零默认值。
driver `build_phase()` 取得最终 cfg 后，在 V2 active-driver 分支拒绝任何非 `DRV_0` 配置；不自动覆写
用户值。V3 保留原 cfg 行为。

### 文字伪代码

```text
lsqenq_agent_agent_cfg 随机化：
  如果上层没有显式覆盖，drv_mode 取 DRV_0；

lsqenq_agent_agent_driver::build_phase()：
  先调用 super.build_phase() 获取最终 cfg 和 vif；
  在 V2 且 driver 已开启时检查 cfg.drv_mode；
  如果不是 DRV_0，uvm_fatal 并停止进入 reset/main phase；
  不在 build_phase 中静默把非法配置改成 DRV_0；

reset、no-item、idle item 和launch前abort：
  保持调用现有 drive_idle(cfg.drv_mode)，build 合同保证实际 mode 为 DRV_0；
  drive_idle 清当前 V2 profile 的全部 request input；
  正常 active request 仍只由 sequence item 和 send_pkt() 驱动，launch 后不立即 revoke。
```

## 10. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/cfg/tb.f` | 问题一：选择唯一 V2/V3 compile profile |
| `mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh` | 问题一、四：LSQ slot/width/presence 宏 |
| `mem_ut/ver/ut/memblock/env/plus.sv` | 问题四：新增 ZERO/MIDDLE/MAX 三类 enqueue 数量权重定义、中文注释和 plus 加载 |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` | 问题四：增加三个同名默认项，分别为 `0/-1/1`；`-1` 表示 MIDDLE AUTO |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv` | 问题一、三：compile localparam、key/FuType 和 `memblock_num_ls_elem_t` |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 问题一、四、五：compile tuple、三类权重快照/AUTO解析/合法性、两阶段 `std::randomize` getter，以及 timeout 公共解析与 V2 sequence 不读取的边界 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/main_control_transaction.sv` | 问题三：`numLsElem` 统一类型和范围 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv` | 问题三、六：behavior 类型；response wrapper 调用唯一 allocation owner |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 问题三：现有 `numLsElem` producer 使用统一类型 cast |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv` | 问题三至七：request/candidate、随机 0 的无副作用短路与显式 idle、streaming launch reservation、pending-sample completion、non-LSQ/trailing idle 边界和现有 flush/epoch gate |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_cfg.sv` | 问题八：V2 `DRV_0` soft 默认 |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_interface.sv` | 问题一、二：profile 字段和统一宽度 |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv` | 问题一至五：字段、类型、逐slot scalar约束、整个batch的6/4约束、automation/打印/compare，以及 framework-only `request_launched` metadata |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv` | 问题二、四、五、八：V2 clock-first streaming、launch前epoch检查、整个batch的6/4运行期复核、build合同、send和idle |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_monitor.sv` | 问题一、二：当前 profile 字段局部采样和 X/Z 宽度；不恢复 analysis-port 发布 |
| `mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv` | 问题一、二：V2 extra 字段连接和 V3 presence 隔离 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 问题四：同步新增 plus 名称、默认配置和使用示例 |
| `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md` | 问题四：同步 enqueue 随机返回范围、三类权重和 AUTO sentinel 规则 |
| `AI_DOC/project_management/mem_ut_parameter_management.md` | 问题四：同步 runtime 行为参数与 compile slot 上限的归属关系 |

明确不修改：

```text
memblock_issue_dispatch_base_sequence.sv
issue_queue_scheduler.sv
exception_redirect_replay_handler.sv
common_data_transaction 的全局 redirect 生命周期
任何 virtual sequence 或 testcase cfg
commit/deq、pass/fail、terminal owner
```

## 11. 修改类型与原逻辑对比总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| compile profile、slot、位宽和 presence | 字段/参数适配 | 散落默认值，混有 V3 8-slot、36-bit FuType、9-bit ROB 和固定 uop/LS-element 宽度 | V2 结构在编译期固定 | 一个 profile 定义全部 LSQ 结构；V2=6-slot、35-bit、8/7/6-bit key；uopIdx/numLsElem 从 MAX_UOP_SIZE/VLEN 派生；硬件结构不走 plus |
| V2 extra request 字段链 | 字段适配 | 字段只存在于部分 interface/xaction/driver，setter/clear/connect 不完整 | active 和 idle 都可能残留字段 | slot0..5 的 interface、xaction、setter、driver、monitor-local/XZ、connect 使用同一字段集合；monitor 不新增 transaction 发布 |
| `numLsElem` 类型 | 字段/参数适配 | 固定 `[4:0]` 和散落 `5'd0/1` | 类型存在多个权威 | seq 内统一使用 `memblock_num_ls_elem_t`；agent 使用同宽度 compile macro packed 类型 |
| xaction 合法范围 | 字段/约束适配 | FuType 保留 V3 literal，uop/LQ/SQ/numLsElem 使用固定上限，部分slot约束为空，通用随机路径可生成5/6个store，基类post gap可随机为0..50 | 位宽参数化后仍可能随机出错误profile、越过真实resource size、超过V2单拍store width或违反streaming零gap合同 | 所有slot使用当前profile FuType、MAX_UOP_SIZE、ROB/LQ/SQ SIZE和MAX_LS_ELEM；scalar setter再收紧到uopIdx=0、numLsElem=1；batch约束分别限制load/store为compile 6/4；LSQ xaction硬约束pre/post gap均为0 |
| scalar request 构造 | 功能逻辑修改 | `uid[6:0]` 被写入 `uopIdx`，caller 分散拼 payload | uid 与 DUT uop 序号语义不同，V2 extra 字段缺少唯一 setter | scalar 固定 `uopIdx=0/lastUop=1/numLsElem=1`；setter 从 main/behavior/key 一次构造完整 request |
| `clear_lsqenq_xaction()` | 功能逻辑修改 | 只清公共字段，extra 字段可能保留 | idle/reuse 必须无残留 | 遍历 compile slot，通过唯一 setter 清全部 qualifier 和 payload，不改公共状态 |
| `assign_lsqenq_slot()` | 功能逻辑修改 | 接收 uid 并自行拼 raw 字段 | 产生错误 uopIdx 且重复 setter 职责 | 删除 uid 入参，只写 needAlloc 并调用唯一 request setter |
| 每拍入队数量加权随机 | 新增 runtime 配置与功能 | `RAND_EN=1` 时用 `$urandom_range(MAX,1)` 均匀返回 `1..MAX`，不能生成主动空拍或控制边界概率 | 需要独立控制 0、中间数量和最大数量，同时保持硬件 slot 上限只有 compile profile 一个权威 | 新增 ZERO/MIDDLE/MAX 三类总权重；使用两阶段 `std::randomize/dist`；随机目标范围扩展为 `0..MAX`，固定模式仍为 `1..MAX`；MIDDLE 默认 AUTO 派生 `MAX-1`，V2 effective 默认 `0/5/1` 保持旧目标分布；权重只控制目标上限，实际 candidate 仍受 prefix、6/4 gate和free count过滤；非法权重或随机失败均 fatal |
| `collect_lsq_candidates()` | 功能逻辑修改 | 只受 runtime 总 slot 数和逐项 free-count 约束，没有分别累计 load/store element；总量 getter 不会返回 0 | 总 slot 合法仍可能形成超过 V2 6-load/4-store 结构能力的 packet；RTL registered credit 不应复制到串行 UVM Candidate；随机 0 必须形成无公共状态副作用的主动 idle | 每拍只采样一次总量；返回 0 时在读取 uid/pointer/free 前返回空 candidate并走显式 idle；非零时按当前scalar `num_ls_elem=1`累计，并要求tentative不超过6/4和对应实际free count；不要求base free预留6/4，不实现vector chunk；累计超限uid不消费、下一拍重试；不增加per-type plus |
| runtime timeout 消费 | 配置细节修改 | V2 sequence 仍读取并等待 ready timeout | V2 没有 ready/response | 公共参数层仍解析并检查该兼容参数非负；V2 sequence 本地 timeout 固定 0且不调用 getter、不等待 ready；零值 warning/clamp 只在 accept-response capability 存在时执行 |
| driver item 时序 | 功能逻辑修改 | active item 进入不存在完成条件的 accept 等待并可能重复发送；若单 item 自行等待 drive/sample 两个边界则只能两拍一批；通用random item继承非零post gap | V2 无 response 完成出口，但连续 request 可以在相邻边界覆盖，任意producer都必须满足零gap | 每轮先过 clocking 边界再取 item；active launch 后立即 `item_done()`且不撤销，下一拍由active/idle覆盖；LSQ xaction把pre/post gap硬约束为0，driver在首次VIF赋值前继续fatal复核directed item |
| xaction launch metadata | 新增框架字段 | 只有 `aborted_by_redirect`，无法区分“已放到 VIF、尚未 sample”和“launch 前被取消” | sequence 需要以 driver 实际 launch 结果决定是否预留资源 | 新增 framework-only `request_launched`；V2 `aborted_by_redirect` 只表示 launch 前取消；二者不连接 DUT |
| `send_pkt()` | 字段驱动细节修改 | 只搬运已有字段，可能部分驱动后才暴露非法值，directed item可绕过random constraint形成5/6个store | 防止错误scalar request落入VIF，并让driver成为所有producer的协议兜底 | 首次VIF写入前检查每个slot的scalar固定字段、key范围和整个batch的load/store 6/4上限，再一次搬运全部V2 input |
| `drive_idle()` | 字段驱动细节修改 | extra 字段和非零 mode 可能残留 valid/X | idle 必须是明确全零协议状态 | `DRV_0` 清当前 profile 的全部 request input |
| `confirm_lsq_candidates()` | 功能逻辑修改 | 用预测 key 调 response helper，并在 driver 返回后同时 allocation 与开放 issue | V2 不存在 response；launch 后立即 issue 早于 DUT sample，全部延迟 allocation 又会让下一批 pointer 过期 | 只在 `request_launched=1` 时 preview 使用中的 key并调用唯一 `commit_allocate()`预留资源；不调用 `complete_admission()` |
| pending-sample completion | 新增局部功能 | 没有“已 launch/已预留但尚未过 sample 边界”的状态 | streaming driver 必须把资源预留和 issue-ready 分开一拍 | sequence 保存单批 pending uid/epoch/launch cycle；每次 `finish_item()` 返回先完成上一批，再预留当前批；epoch有效才调用 `complete_admission()`执行原CSR drain并开放issue；sample/launch progress用inout OR汇总，不互相覆盖 |
| non-LSQ 与末批 sample 边界 | 功能逻辑修改 | non-LSQ admission 可零时间返回，主循环退出也不保证再经过 driver 边界 | 零时间路径不能证明上一 LSQ request 已被采样 | pending batch 遇到 non-LSQ 时先发 idle 再走原 non-LSQ admission；退出前 pending batch 用 trailing idle drain；连续 LSQ 不插空拍 |
| `commit_allocate_with_resp()` | 共享实现重构 | 自己复制 main/status/map/pointer/free-count 更新并比较两个 key | 两个 allocation owner 容易分叉，unused key 比较无语义 | 只比较 behavior 实际使用的 response key，匹配后调用唯一 `commit_allocate()` |
| redirect sample/cancel 分工 | 功能逻辑修改 | abort/confirm 没有区分 launch 前和 launch 后 sample 前，可能不建 mapping或过早开放 issue | launch 后 request 可能被 DUT采样，redirect 必须能按 active mapping回退 | launch 前 abort不预留；launch 后先保留reservation，epoch失效时不开放issue；原redirect handler累计cancel，原LSQ cancel路径回退资源 |
| redirect/flush gate 统一 | 功能逻辑收敛 | collect、driver 和 confirm 对 redirect/flush 的观察点不统一，容易把 abort 结果当成唯一保护入口 | V2 redirect 可发生在 request 构造前、launch 前后和 sample 后；必须用已有全局 flag、epoch 和 driver launch 结果共同决定是否 admission/confirm | 不新增固定延迟 retry guard；collect 前和driver launch前继续检查flush/epoch以形成launch前abort；一旦`request_launched=1`，confirm无条件建立reservation/token，batch epoch只在下一边界禁止issue；flush清0后先应用finalized cancel再收集新candidate |
| LSQ `drv_mode` 合同 | 配置逻辑修改 | 通用 mode 可让 idle 随机出 valid/X | idle request 不属于 sequence | V2 soft 默认 `DRV_0`，active driver 在 build 阶段拒绝非零 mode |

保持不变的主体逻辑：主表生成和 validation、连续 uid admission、已有 pointer advance/cancel/release
公式、non-LSQ admission 的判定与提交、issue scheduler 和 fired-mask、writeback、ROB commit、LSQ deq、pass/fail、
terminal 和 global stop。

## 12. 执行结果（2026-07-16）

本 plan 已完成 coding。执行时以本文件的 `IMPLEMENTATION_DELTA` 为最终口径；原正文中以下内容只保留
审计记录，不代表当前实现：

- 不实现 V2/V3 profile selector，复用已归档的 V2 compile baseline。
- 不要求 LQ/SQ base free 先达到 6/4，也不要求 `tentative + 6/4 reserve`。
- 不实现 vector/multi-element chunk；本轮 scalar setter 只接受 `numLsElem=1`。
- 不增加 redirect 后固定 5-cycle retry guard。

执行中进一步采用十七个最小实现细节：

1. `set_req_fields()` 不接收冗余 ROB key 参数，直接调用 `main_tr.get_rob_key()`；LQ/SQ key仍来自
   candidate 预览。这样 ROB key只有主表一个权威，避免 caller 传入不一致副本。
2. `tb/lsqenq_agent_connect.sv` 的 V2 extra字段连接和 `dut_inst.sv` 具体端口已在执行前基线完整，
   本轮静态核对后不制造无意义diff；实际改动集中在字段宽度consumer、xaction/driver/monitor和sequence。
3. driver 对合法 idle item 调用全零 `send_pkt()`，而不是直接调用 `drive_idle()`；`send_pkt()` 会先验证
   inactive qualifier/payload全部为0，再搬运到VIF，因此接口效果相同且不会让 malformed idle绕过检查。
4. 归档前独立review发现 xaction 自定义 compare 的旧手工回退路径会覆盖 UVM automation 对extra字段的
   失败结果，`psdisplay()`也未显示这些字段。现用只读 `get_v2_extra_fields()` 同时补齐六个slot的
   `exceptionVec/trigger/fuOpType/flushPipe/lastUop` 打印和比较，不改变driver或DUT数据路径。
5. 独立review第2轮确认LSQ enqueue源码无新功能问题，但指出timeout公共参数解析边界和并发CSR Plan
   分类描述不准确。现已统一为“公共层解析并检查非负，V2 sequence不读取、不等待ready”，并在review
   文档中明确排除CSR control/sfence专项Plan。
6. 独立review第3轮确认前述问题已闭环，并发现参数规则仍把
   `MEMBLOCK_LSQENQ_SEQ_EN` 写成默认0。当前 `plus.sv/default.cfg` 的既有权威值均为1，已同步规则为
   默认启用；无主表场景只等待并保持idle，显式关闭时直接返回且不回退随机sequence。
7. 独立review第4轮确认源码无新问题，并发现当前web/interface/source analysis仍残留旧response主链，
   多份历史Plan缺少失效注记，总控Plan内CSR并发hunk未分类，且第13章漏记enable默认值文档同步。
   现已把当前文档改为V2无response的clock-first/pending-sample主链，为历史文档增加醒目边界，并补齐
   review分类和本章修改类型总结。
8. 独立review第5轮继续发现同类文档内部残留：Web其它节点仍有旧timeout/response描述，interface前部
   仍列8路response，source analysis保留固定`5'd`和已删getter，另有8份历史Plan/review缺少失效注记，
   总控Plan也提前写成已归档。现已按类别全量扫描并清理，三份JS重新通过`node --check`，总控Plan在
   真正移动前恢复`undo`路径。
9. 独立review第6轮确认源码无新问题，并发现少量key来源、`issue_ready`/`tlb_mapped`、Web helper名、
   未launch fatal、unused key和归档门禁描述仍不精确。现已逐项按源码修正，并把implementation review
   自身纳入diff覆盖矩阵。
10. 独立review第7轮发现通用default sequence没有4-store batch gate、最终smoke早于最新compile、Web仍有
    旧调用签名/AMO可达描述、implementation review漏展开idle边界，以及interface主文档保留V3 FuType
    编码。现已补齐xaction约束和driver复核、刷新Web/interface、展开review源码证据；20:53开始的干净
    compile在20:55成功结束，随后20:55:57的真实load smoke通过，结果已写入第14章。
11. 独立review第8轮发现agent通用随机/direct路径没有限制active `fuOpType`，手工compare遗漏三个
    framework metadata，non-LSQ pending边界文档顺序、review源码块结构和compile warning描述也不准确。
    现已补齐agent层opcode helper和driver复核、全部metadata打印/比较，并同步当前flow/web/review证据。
12. 独立review第9轮发现通用default random item继承基类`post_pkt_gap=0..50`，会被V2 driver零gap合同
    拒绝。现由LSQ xaction硬约束pre/post gap均为0，driver保留首次VIF赋值前fatal兜底；本轮同时按真实
    源码补齐non-LSQ sample顺序和implementation review关键task/helper展开。
13. 第9轮修复后的default-random专项进一步发现，constraint调用自定义opcode helper时VCS会先求值函数
    入参，不能反向求解随机`fuOpType`。现把load/prefetch和store合法集合分别定义为xaction文件内的宏值表；
    constraint直接`inside`值表，driver checker helper也读取同一值表，既保留单一编码权威又可被solver求解。
    宏值表修复后的clean compile、default-random专项和同一`simv`真实scalar-load smoke均已通过。
14. 第10轮源码review发现`set_req_fields()`没有完整落实原Plan已要求的setter自校验。现由setter入口自行
    拒绝空xaction和越界slot；idle分支要求`main_tr==null`、behavior完整等于
    `make_default_behavior()`且LQ/SQ key全零。该修复只补齐错误caller的受控fatal，不改变当前唯一
    clear caller、active payload、公共状态或DUT主路径。
15. 第11轮源码review发现当前显式六slot字段面仍允许compile宏覆盖成非6/6/4。现由
    `check_compile_param_consistency()`在激励前要求当前V2 tuple严格等于6/6/4；默认行为不变，未完成
    全链路参数化的其它tuple受控fatal。
16. 第11轮源码review发现custom compare手工回退遗漏pre/post gap。现由`psdisplay()`打印两个gap，
    compare回退显式比较并报告差异；该修改只影响transaction debug/compare，不改变随机或驱动逻辑。
17. 第11轮文档review发现当前flow/source/Web中仍有旧类名、错误allocation调用边、setter合同和epoch条件
    表述遗漏，且原验证时间链无法证明覆盖最新源码。现已逐项按真实调用链同步，并在修复后冻结
    `mem_ut/ver/ut/memblock` diff；冻结版clean compile、default-random专项和随后同一`simv`的真实
    scalar-load smoke均通过。最终归档只使用第14章记录的冻结哈希和这组三段严格顺序证据。

## 13. 执行后实际修改类型总结

本章是执行后的当前实现总结，替代第11章中被 `IMPLEMENTATION_DELTA` 覆盖的旧描述。

| 修改项 | 修改类型 | 原有逻辑 | 变更原因 | 当前实现 |
|---|---|---|---|---|
| V2 LSQ 派生宏和类型 | 字段/参数适配 | uopIdx、numLsElem和load/store enqueue width仍有固定值 | interface、xaction、monitor和sequence需要同一编译期权威；当前显式六slot consumer不能安全接受任意宏覆盖 | 新增6/4、MAX_UOP_SIZE/VLEN派生宏；seq内统一`memblock_num_ls_elem_t`；compile consistency在激励前要求当前V2 slot/load/store tuple为6/6/4，未来完整profile参数化后再放开 |
| interface/xaction/monitor宽度 | 字段适配 | uopIdx/numLsElem保留固定packed宽度 | 防止版本切换后consumer分叉 | 全部消费`MEMBLOCK_DUT_*`宏；connect端已有字段链保持不动 |
| V2 request字段构造 | 功能修改 | uid低位写uopIdx，extra字段未由唯一setter完整控制 | uid不是DUT uop序号，active/idle可能残留payload | scalar固定uopIdx=0、lastUop=1、numLsElem=1；extra异常字段本轮为0；ROB从main直接取得；setter自行拒绝空xaction/越界slot，idle要求null main、完整默认behavior和全零key |
| xaction/driver合同 | 功能修改 | slot1..5约束不完整，inactive只检查needAlloc，active `fuOpType`可取任意9-bit值，通用随机/direct item可形成5/6个store或非零post gap，custom compare/display遗漏extra、gap和部分metadata | 任一producer都可能把不支持opcode、非法payload、超过V2端口能力的batch或非零gap送入driver；constraint调用checker函数还会让VCS无法反向求解随机opcode；debug/compare也可能漏掉V2字段、streaming gap或stale epoch差异 | 六个slot按valid条件约束；inactive qualifier/payload全0；宏值表唯一维护LQ load/prefetch 0..6/8..10和SQ store 0..3，constraint直接`inside`值表且driver helper读取同一值表；xaction batch约束限制load/store 6/4并硬约束pre/post gap为0；driver首次赋值前复查gap、opcode、scalar合同、key范围和batch计数；extra字段、pre/post gap及全部framework metadata进入display和手工compare回退路径 |
| FuType | 字段/合法性适配 | xaction保留V3 literal | V3编码不能裁剪后送V2 | 使用当前profile LDU/STU one-hot和`encode_and_fit_dut_futype()`无损检查 |
| 每拍总量随机 | 新增runtime功能 | 随机模式只能均匀返回1..MAX | 需要可配置主动idle和边界概率 | ZERO/MIDDLE/MAX两阶段`std::randomize`；默认0/5/1保持旧1..6均匀分布 |
| 权重合法性 | 新增配置检查 | 无三类权重 | 需要支持ZERO及zero-only，同时拒绝随机solver没有合法类别的全0配置 | 随机模式用64-bit逐项计算三类总权重；只拒绝全0，`1/0/0`合法并稳定返回0；不新增zero-only自动退出或terminal逻辑 |
| load/store 6/4 gate | 功能修改 | 只限制总slot和free count，dispatch或default/direct路径都可能让6个store进入同一packet | V2 store每拍最多4 element，gate必须覆盖完整agent行为面 | dispatch candidate按`num_ls_elem`累计并限制6/4及实际free；xaction随机约束限制6/4；driver在写VIF前再次统计fail-fast；不额外保留6/4空项 |
| driver时序 | 功能修改 | 等待不存在的canAccept/response或重复发送item | V2没有完成响应，但接口可以每拍streaming | clock-first：每边界先sample上一批，再launch当前批并立即item_done；pre/post gap固定0 |
| launch metadata | 新增框架字段 | 只有abort字段，无法区分未launch和待sample | sequence必须根据driver实际行为决定是否预留 | 新增`request_launched`并纳入automation/打印/custom compare；不连接DUT |
| allocation与issue-ready | 功能修改/新增局部状态 | driver返回后同拍allocation和开放issue | 当前request尚未经过下一DUT sample边界 | launch后立即`commit_allocate()`预留；单深度pending batch在下一driver边界才`complete_admission()` |
| non-LSQ/末批边界 | 功能修改 | non-LSQ零时间路径和global stop可能绕过最后sample | 最后一批可能永远不进入issue | pending后遇non-LSQ或global stop时发送全零idle完成sample，再继续原流程/退出 |
| response helper | 共享实现重构 | `commit_allocate_with_resp()`复制状态更新并比较unused key | 两个allocation owner易分叉 | 只比较实际使用key，随后复用唯一`commit_allocate()`；V2不调用该wrapper |
| redirect分工 | 功能收敛 | launch/confirm/sample边界不清 | redirect可能出现在三个阶段 | launch前abort不预留；launch后即使epoch失效也建立并保留reservation，下一边界只禁止issue；原redirect/cancel owner回退资源 |
| ready timeout | 配置细节适配 | V2 sequence仍可能读取并等待LSQ ready timeout | V2接口无ready/response | 公共参数加载仍解析并检查该兼容参数非负；V2 sequence不读getter、不等待ready，只有`MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP=1`才执行零值warning/clamp |
| sequence enable/default文档语义 | 文档语义同步，不改运行期逻辑 | 部分参数规则和历史文档仍写`MEMBLOCK_LSQENQ_SEQ_EN=0`，或声称real smoke必须单独打开 | `plus.sv`、`seq_csr_common`和`default.cfg`的既有单一权威均为1 | 当前规则明确默认1；无主表时sequence只idle等待，显式0时直接返回且不fallback；历史Plan保留正文但增加失效注记 |
| idle drive模式 | 配置逻辑修改 | 通用drv_mode可能让idle含valid/X | no-item/reset/abort必须安全 | cfg soft默认DRV_0，driver build阶段拒绝非DRV_0，idle清全部字段 |

保持不变：主表生成/validation、连续uid admission原则、issue scheduler内部算法、writeback、ROB commit、
LSQ deq、pass/fail、terminal和global stop owner。

## 14. 验证记录

### 14.1 通过项

- 第11轮修复后的`mem_ut/ver/ut/memblock`冻结diff（SHA-256
  `99fdd0c69f99f7dd3e08eed289ea9ace2df11c344b62563c8f147873f3f3b8f0`）是ZERO语义补充前的历史基线，
  不覆盖本轮17:36之后的权重检查修改。
- 历史冻结版远端VCS clean compile：删除`base_fun/partitionlib`和`base_fun/exec`后于2026-07-17 13:33
  重新开始，13:35完成全量parsing、全部174个RTL module、全部UVM package、partition compile、stitch
  和link；compile log只有`LCA_FEATURES_ENABLED`工具特性warning，没有源码编译错误，最终KDB报告
  0 error/0 warning。
- 冻结版default-random专项：在上述13:35生成的`simv`上以`UVM_FULL`和type override启动
  10-item `lsqenq_agent_agent_default_sequence`；显式设置`MEMBLOCK_LSQCOMMIT_SEQ_EN=0`隔离空主表下的
  无关commit sequence。日志明确显示main phase启动该sequence，结束时LSQ enqueue sequencer报告
  `No default sequence to kill`；没有`CNST-CIF`、`RNDFLD`、gap/setter fatal或其它
  `UVM_WARNING/ERROR/FATAL`，13:38:15最终`TEST CASE PASSED`。
- 冻结版最终`tc_dispatch_real_smoke`：在default-random结束后使用同一`simv`，真实load从LSQ admission
  进入LDA issue、DCache request/response、writeback、ROB commit、LQ deq和terminal，372.8ns结束，
  13:38:28最终`TEST CASE PASSED`且`UVM_WARNING/ERROR/FATAL=0`。
- 第9轮及opcode宏值表修复后远端VCS clean compile：2026-07-17 11:37开始，完成parsing、elaboration、
  全部174个RTL module、全部partition compile、stitch和link；没有源码编译错误，日志包含1条无害的
  `LCA_FEATURES_ENABLED`工具特性提示，最终KDB阶段报告0 error/0 warning。
- default-random专项：通过type override在LSQ enqueue sequencer启动
  `lsqenq_agent_agent_default_sequence`并随机发送10个item；为隔离空主表下会永久等待的无关LSQ commit
  sequence，命令显式设置`MEMBLOCK_LSQCOMMIT_SEQ_EN=0`。main phase结束时LSQ enqueue sequencer报告
  `No default sequence to kill`，证明sequence自然完成；没有`CNST-CIF`、`RNDFLD`、gap fatal或其它
  `UVM_WARNING/ERROR/FATAL`，最终`TEST CASE PASSED`。
- 第9轮最终`tc_dispatch_real_smoke`：default-random专项后通过`eda_batch_run`运行同一`simv`；真实load
  从LSQ admission进入LDA issue、DCache request/response、writeback、ROB commit、LQ deq和terminal，
  372.8ns结束，`TEST CASE PASSED`且`UVM_WARNING/ERROR/FATAL=0`。
- ZERO语义补充后的最终源码/规则diff：当前`git diff --binary -- mem_ut/ver/ut/memblock` SHA-256为
  `39910ad5aa5155627e11072ef19b158f7d138656d971f9d09779b14646239377`；`seq_csr_common.sv`和`plus.sv`
  均于17:36:42修改，随后于17:39重新开始远端VCS编译，最终KDB报告0 error/0 warning。该哈希覆盖
  本轮代码/规则修改，不把之后仅有的plan/review文档编辑混入源码证据。
- 高ZERO权重随机场景：`ZERO/MIDDLE/MAX=100/0/1`，seed=1；issue从约255ns延迟到1950ns后正常完成
  terminal，证明随机0只插idle且未消费uid，最终`UVM_ERROR=0`、`UVM_FATAL=0`。
- 用户语义澄清后的合法ZERO配置：`ZERO/MIDDLE/MAX=1/0/1`，远端VCS重新编译后运行
  `tc_dispatch_real_smoke`，参数在0ns正确加载，372.8ns完成真实load admission、issue、writeback、
  ROB commit、LQ deq和terminal，`TEST CASE PASSED`，`UVM_WARNING/ERROR/FATAL=0`。日志为
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=tc_dispatch_real_smoke_seed=666666_rtl_lsqenq_zero_mixed_final_20260717.log`。
- zero-only支持场景：`ZERO/MIDDLE/MAX=1/0/0`在重新编译后的同一`simv`上运行`tc_smoke`，关闭LSQ
  enqueue/commit/issue/L2TLB主sequence，由原有smoke phase结束；参数初始化检查无fatal，`TEST CASE PASSED`且
  `UVM_WARNING/ERROR/FATAL=0`。该场景只证明zero-only配置合法和不伪造progress，不把非空主表推进到terminal。
- 全零负向场景：`ZERO/MIDDLE/MAX=0/0/0`在`0ns`命中
  `LSQ enqueue ZERO/MIDDLE/MAX weights must not all be zero`预期fatal。
- software-only `virtual_base_sequence`基础运行：通过；该场景不作为真实LSQ时序覆盖证据。

### 14.2 已识别但不归属本专项的失败

- `tc_dispatch_real_store_smoke` 已证明store经过LSQ admission、STA/STD issue、writeback和ROB commit；随后
  在既有V2 SQ deq适配边界出现`DUT sqDeq start ... mismatches software SQ head`。
- 6-store/总slot=6压力尝试在下游出现既有`WB_UID_MISMATCH`。日志已显示STA/STD连续发射，本轮
  `collect_lsq_candidates()`的4-store上限由源码静态review确认；该下游writeback/deq问题属于int-WB和
  LSQ commit/deq专项，不回写LSQ enqueue owner，也不阻塞本plan归档。
- `basicTest + memblock_dispatch_real_smoke_vseq` 当前会在vseq启动同拍结束，不能作为有效真实flow smoke；
  本轮改用现有`tc_dispatch_real_smoke`的agent default-sequence入口验证。virtual-sequence phase生命周期
  问题不在本plan修改范围。
- 第7轮和本轮归档检查重复调用`eda_run`时，VCS/NFS增量数据库先后出现`tdc.sdb corrupted`或partcomp
  `SIGSEGV`，均在仿真启动前。清理`base_fun/partitionlib`和`base_fun/exec`下VCS生成缓存后，clean
  compile均恢复；真实场景改用不重复编译的`batch_run`执行。这些工具异常不计为产品测试通过，也不
  归因于SystemVerilog源码；当前归档依据使用ZERO补充后的`39910ad5...39377`源码diff哈希、17:39之后
  的最终clean compile、default-random专项和随后同一`simv`的真实scalar-load/权重边界场景。

最终日志：

- compile：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log`
- default-random：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_sanity_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_lsqenq_round12_frozen_default_random_20260717.log`
- smoke：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=tc_dispatch_real_smoke_seed=666666_rtl_lsqenq_round12_frozen_real_load_20260717.log`
- 合法zero-only：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_smoke_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_lsqenq_zero_only_supported_20260717.log`
- 全零负向：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=tc_dispatch_real_smoke_seed=666666_rtl_lsqenq_zero_all_0ns_expected_fatal_20260717.log`

## 15. 用户后续语义澄清（IMPLEMENTATION_DELTA，2026-07-17）

用户后续明确要求支持ZERO权重和`1/0/0` zero-only，并要求在0ns参数初始化完成后只检查三类中
至少一个权重非零。本次补充不改变随机器、candidate或driver逻辑，只放开原zero-only禁用条件：

```text
ZERO_WEIGHT可以大于0，用于让get_enq_per_cycle()随机返回0；
ZERO/MIDDLE/MAX全0继续fatal；
1/0/0合法且每次采样都返回0；
zero-only只发送idle，不消费uid，也不产生terminal或global stop。
```

实现删除正入队类别必须非零的fatal，只保留64-bit三类总权重全0检查。本补充不新增参数，不改变
默认`0/-1/1`，也不改变pass/fail、terminal或no-progress owner。非空主表使用zero-only且没有外部
结束条件时会按既有主动flow语义持续idle，最终由no-progress诊断/UVM timeout暴露，不伪造完成状态。

## 16. 最终 Review 结论

- 第12轮独立源码/V2语义review通过：无新发现、无必须修改项；明确排除CSR/sfence、DCache/L2、PMP/PMA
  和其它非LSQ enqueue专项。
- 第12轮文档review发现implementation review缺少三个helper/function的源码展开，以及最后一章缺少集中式
  四要素总结；两项均已修复。
- 修复后的第13轮独立文档review通过：60个SystemVerilog源码块均在5行内紧邻对应中文伪代码，最后一章
  已完整覆盖修改类型、原逻辑、变更原因、变更后逻辑和新增/修改功能列表；无新发现、无必须修改项。
- 用户后续ZERO/zero-only语义补充已完成源码、参数规则、flow、源码分析、plan和review同步；重新编译、
  合法`1/0/1`真实load、合法`1/0/0` smoke场景和非法`0/0/0`负向场景均得到预期结果。
- 最后一轮独立subagent review已通过：确认历史哈希与最终哈希的时间线无矛盾，源码只拒绝全零权重，
  zero-only无progress验证边界已明确，无新增遗漏或逻辑问题。
- 本agent复核最终源码diff、同步文档、最终哈希和日志后未发现除“zero-only启用完整LSQ flow不产生
  admission progress”这一已记录边界外的blocker。本plan满足归档条件并移动到
  `AI_DOC/plan/test_framework/plan/do`。

## 与初步 plan 差异说明

本章只总结本 `do` plan 已完成的 LSQ enqueue 功能，不包含后续 cancel reconcile 的待实现字段或
函数；后者由 MMIO/status `undo` plan 唯一拥有。

| 修改项 | 修改类型 | 修改前逻辑 | 变更原因 | 最终逻辑与影响 |
|---|---|---|---|---|
| V2 request字段 | 接口字段适配 | slot/宽度和extra字段仍混有V3固定值，uid低位被写入uopIdx | V2是6-slot scalar request，uopIdx不是UID | compile宏统一字段宽度；唯一setter固定`uopIdx=0/lastUop=1/numLsElem=1`并填写完整payload，idle清全部字段 |
| candidate 6/4 gate | candidate功能逻辑修改 | 只限制总slot/free count，可能构造6个store | V2单拍最多6 load/4 store element | `collect_lsq_candidates()`按scalar element累计并以6/4及实际free count过滤；超限UID留到下一拍，不复制RTL额外credit reserve |
| enqueue数量随机 | runtime配置与随机功能新增 | 随机模式只返回1..MAX | 需要显式idle和边界概率 | ZERO/MIDDLE/MAX两阶段`dist`返回0/中间/MAX；`1/0/0`合法idle-only，仅三类全0fatal；返回0不消费UID或资源 |
| driver握手 | driver时序逻辑修改 | 等不存在的canAccept/response，或每item等待两拍 | V2只有request input但支持相邻拍streaming | driver clock-first：每边界sample上一批、覆盖当前批并立即`item_done()`；pre/post gap固定0，active payload保持到下一覆盖 |
| allocation与sample | 状态生命周期功能新增/修改 | driver返回后同拍allocation和issue-ready | 当前request尚未经过下一DUT sample边界 | `commit_allocate()`立即预留pointer/free count，单深度pending batch保存UID和独立dispatch/flush epoch，在下一`finish_item()`返回后才`complete_admission()`；后续MMIO/status专项升级UID queue为reservation token queue，不替代flush epoch gate；连续LSQ无空拍，non-LSQ/末批用idle补sample边界 |
| redirect边界 | redirect生命周期逻辑修改 | launch前abort和launch后epoch失效未分层 | redirect可能落在构造、launch、sample三个阶段 | launch前abort不预留；一旦launch即建立reservation/token，confirm时epoch失效也不得跳过；下一sample边界只禁止issue，由既有redirect/cancel owner回退 |
| idle驱动 | driver配置与失败策略修改 | 通用driver mode可能让idle含valid/X | 无item/reset/abort必须保持协议安全 | V2 active driver只接受`DRV_0`，非法mode在build阶段fatal；idle完整驱0，不改变pass/fail/terminal |

关键 helper 差异：`set_req_fields()` 输入slot、main/behavior和预测key，输出完整V2 request且不修改公共
状态；`collect_lsq_candidates()` 输入当前连续UID、compile 6/4和free count，输出本拍有序candidate；
`get_enq_per_cycle()` 输出0..物理slot目标值；`confirm_lsq_candidates()` 只确认真实launch并调用唯一
allocation owner；`complete_v2_pending_sample()` 只结算上一批并开放issue；
`commit_allocate_with_resp()` 已收敛为比较wrapper。各函数完整分支和错误策略见第2至第9章，执行后
差异与验证见第12至第16章。

### 审稿用四要素伪代码

```text
修改目的：
  让V2六slot、6-load/4-store、无response接口支持逐拍streaming和可配置idle，同时保持单一allocation owner。
修改前逻辑行为：
  candidate只按总slot限制；随机目标最小为1；driver等待不存在的accept/response；allocation与issue-ready同拍。
修改后逻辑行为：
  setter完整构造V2 scalar payload；candidate按load/store element和free count过滤；ZERO/MIDDLE/MAX可返回0；
  driver在每个clock-first边界采样上一批并覆盖当前批；commit_allocate立即预留，下一边界才开放issue。
差异影响：
  改变V2 enqueue payload、随机idle、driver时序和issue-ready时点；不改变合法主表顺序、pass/fail或terminal owner。
```

### 新增/修改 Helper 详细伪代码

```text
set_req_fields(slot,main,behavior,predicted_key)：
  添加原因：六个slot不能分散遗漏V2字段。
  写单slot完整scalar request；校验target/opcode/key，inactive slot全零；不修改主表、pointer或free count。

collect_lsq_candidates(target_count)：
  添加原因：总slot限制不能阻止单拍超过4个store。
  顺序读取连续UID；按num_ls_elem累计load/store并检查6/4、实际free count；超限UID留到下一拍，不消费状态。

get_enq_per_cycle()：
  添加原因：需要ZERO/MIDDLE/MAX可配置分布和合法zero-only。
  检查三类总权重非零；用SV dist选择0、中间或MAX，再在中间区间随机；返回目标数，不写UID或LSQ状态。

confirm_lsq_candidates()：
  修改前把软件预测key伪装成response；修改后仅在request_launched时复核preview并调用唯一commit_allocate；
  request_launched后不再受confirm时flush/epoch二次否决，保存UID batch和dispatch/flush epoch且不设置issue_ready；
  只有launch前abort无allocation副作用，launch后epoch失效由下一sample/redirect路径收敛。

complete_v2_pending_sample()：
  添加原因：allocation预留与DUT sample/issue-ready必须分层。
  下一finish_item返回后处理唯一pending batch；flush epoch有效才逐UID complete_admission，否则等待redirect owner回退；
  清pending状态，不直接release pointer/free count。

commit_allocate_with_resp()：
  修改前复制状态/map/pointer更新；修改后只比较真实response key并调用commit_allocate，保持单一写者。
```
