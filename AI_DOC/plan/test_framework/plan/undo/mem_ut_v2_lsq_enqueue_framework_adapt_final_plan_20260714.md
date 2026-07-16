# mem_ut V2 LSQ Enqueue 测试框架适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle()` |
| 适配原则 | 只修改 V2 字段、参数和必要的入队完成细节，不改变主表、issue、commit/deq、pass/fail、terminal 主体逻辑 |
| 创建/修订日期 | 2026-07-16 |

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
`-1` 或非负值；随机模式下解析后的三类总权重必须大于 0，并且 `MIDDLE+MAX` 必须大于 0，禁止配置成
永远只返回 0 而使主动 flow 永不推进。`MAX<=1` 时必须禁止非零 `MIDDLE`。任一约束随机化失败均
`uvm_fatal`，不得静默回退成均匀随机。权重求和使用 `longint unsigned`，避免三个 `int` 相加时溢出。

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
    检查effective_middle + MAX大于0，禁止永远只生成idle；
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
不读取 timeout getter。`seq_csr_common::validate_and_clamp()` 在 V2 跳过 LSQ ready timeout 的
warning/clamp；V3 仍保留原 timeout 字段及 wait/response 方法入口，其功能扩展不属于本 plan。

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
`issue_ready`。整批 uid、launch epoch 和 dispatch service cycle 保存到 pending-sample。下一 driver
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
  driver 未 launch、当前仍处于 flush、或 transaction epoch 已过期时，不建立 launch reservation。
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

`confirm_lsq_candidates()` 只读取 driver 返回的 `request_launched`、`aborted_by_redirect` 和 epoch。如果
request 未 launch 或 epoch 失效，则整批不预留资源；如果 request 已 launch 且 epoch 仍有效，才调用唯一
allocation owner 建立 launch reservation，并登记 pending sample。confirm 不写任何 retry guard 字段，也不清
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
  如果当前global flush有效或transaction epoch已失效：
    返回，不建立reservation；
  否则按问题六的launch reservation流程调用唯一allocation owner并登记pending sample；
  confirm不新增post-release等待，不写全局flush状态；

complete_v2_pending_sample()：
  epoch mismatch时只拒绝本批开放issue；
  不直接回退pointer/free count，也不写waiting/release字段；
  redirect handler和apply_pending_lsq_cancels()仍是cancel及资源回退owner。
```

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
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 问题一、四、五：compile tuple、三类权重快照/AUTO解析/合法性、两阶段 `std::randomize` getter 和 V2 timeout 不消费 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/main_control_transaction.sv` | 问题三：`numLsElem` 统一类型和范围 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv` | 问题三、六：behavior 类型；response wrapper 调用唯一 allocation owner |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 问题三：现有 `numLsElem` producer 使用统一类型 cast |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv` | 问题三至七：request/candidate、随机 0 的无副作用短路与显式 idle、streaming launch reservation、pending-sample completion、non-LSQ/trailing idle 边界和现有 flush/epoch gate |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_cfg.sv` | 问题八：V2 `DRV_0` soft 默认 |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_interface.sv` | 问题一、二：profile 字段和统一宽度 |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv` | 问题一至三、五：字段、类型、约束、automation/打印/compare，以及 framework-only `request_launched` metadata |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv` | 问题二、五、八：V2 clock-first streaming、launch 前 epoch 检查、build 合同、send 和 idle |
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
| xaction 合法范围 | 字段/约束适配 | FuType 保留 V3 literal，uop/LQ/SQ/numLsElem 使用固定上限，且部分 slot 约束为空 | 位宽参数化后仍可能随机出错误 profile 或越过真实资源 size | 所有 slot 使用当前 profile FuType、MAX_UOP_SIZE、ROB/LQ/SQ SIZE 和 MAX_LS_ELEM；scalar setter再收紧到 uopIdx=0、numLsElem=1 |
| scalar request 构造 | 功能逻辑修改 | `uid[6:0]` 被写入 `uopIdx`，caller 分散拼 payload | uid 与 DUT uop 序号语义不同，V2 extra 字段缺少唯一 setter | scalar 固定 `uopIdx=0/lastUop=1/numLsElem=1`；setter 从 main/behavior/key 一次构造完整 request |
| `clear_lsqenq_xaction()` | 功能逻辑修改 | 只清公共字段，extra 字段可能保留 | idle/reuse 必须无残留 | 遍历 compile slot，通过唯一 setter 清全部 qualifier 和 payload，不改公共状态 |
| `assign_lsqenq_slot()` | 功能逻辑修改 | 接收 uid 并自行拼 raw 字段 | 产生错误 uopIdx 且重复 setter 职责 | 删除 uid 入参，只写 needAlloc 并调用唯一 request setter |
| 每拍入队数量加权随机 | 新增 runtime 配置与功能 | `RAND_EN=1` 时用 `$urandom_range(MAX,1)` 均匀返回 `1..MAX`，不能生成主动空拍或控制边界概率 | 需要独立控制 0、中间数量和最大数量，同时保持硬件 slot 上限只有 compile profile 一个权威 | 新增 ZERO/MIDDLE/MAX 三类总权重；使用两阶段 `std::randomize/dist`；随机目标范围扩展为 `0..MAX`，固定模式仍为 `1..MAX`；MIDDLE 默认 AUTO 派生 `MAX-1`，V2 effective 默认 `0/5/1` 保持旧目标分布；权重只控制目标上限，实际 candidate 仍受 prefix、6/4 gate和free count过滤；非法权重或随机失败均 fatal |
| `collect_lsq_candidates()` | 功能逻辑修改 | 只受 runtime 总 slot 数和逐项 free-count 约束，没有分别累计 load/store element；总量 getter 不会返回 0 | 总 slot 合法仍可能形成超过 V2 6-load/4-store 结构能力的 packet；RTL registered credit 不应复制到串行 UVM Candidate；随机 0 必须形成无公共状态副作用的主动 idle | 每拍只采样一次总量；返回 0 时在读取 uid/pointer/free 前返回空 candidate并走显式 idle；非零时先要求 base LQ/SQ free 满足 compile-profile 6/4 floor，再按 `behavior.num_ls_elem` 累计并要求 tentative 不超过 6/4 和对应 base free；该逻辑对 future vector/multi-element request 可复用；vector可分片request单笔超过硬件width时按chunk拆分，非vector或不可分片request才fatal；累计超限 uid 不消费、下一拍重试；不增加 per-type plus，不使用 `tentative + 6/4 reserve` |
| runtime timeout 消费 | 配置细节修改 | V2 仍读取、clamp ready timeout | V2 没有 ready/response | V2 本地 timeout 固定 0且不调用 getter；V3 只保留现有 timeout 参数和方法入口，本 plan 不扩展其 runtime 功能 |
| driver item 时序 | 功能逻辑修改 | active item 进入不存在完成条件的 accept 等待并可能重复发送；若单 item 自行等待 drive/sample 两个边界则只能两拍一批 | V2 无 response 完成出口，但连续 request 可以在相邻边界覆盖 | 每轮先过 clocking 边界再取 item；active launch 后立即 `item_done()`且不撤销，下一拍由 active/idle 覆盖；单笔延迟一拍、稳态每拍一批；V2 pre/post gap固定为0 |
| xaction launch metadata | 新增框架字段 | 只有 `aborted_by_redirect`，无法区分“已放到 VIF、尚未 sample”和“launch 前被取消” | sequence 需要以 driver 实际 launch 结果决定是否预留资源 | 新增 framework-only `request_launched`；V2 `aborted_by_redirect` 只表示 launch 前取消；二者不连接 DUT |
| `send_pkt()` | 字段驱动细节修改 | 只搬运已有字段，可能部分驱动后才暴露非法值 | 防止错误 scalar request 落入 vif | 首次 vif 写入前检查 scalar 固定字段，再一次搬运全部 V2 input |
| `drive_idle()` | 字段驱动细节修改 | extra 字段和非零 mode 可能残留 valid/X | idle 必须是明确全零协议状态 | `DRV_0` 清当前 profile 的全部 request input |
| `confirm_lsq_candidates()` | 功能逻辑修改 | 用预测 key 调 response helper，并在 driver 返回后同时 allocation 与开放 issue | V2 不存在 response；launch 后立即 issue 早于 DUT sample，全部延迟 allocation 又会让下一批 pointer 过期 | 只在 `request_launched=1` 时 preview 使用中的 key并调用唯一 `commit_allocate()`预留资源；不调用 `complete_admission()` |
| pending-sample completion | 新增局部功能 | 没有“已 launch/已预留但尚未过 sample 边界”的状态 | streaming driver 必须把资源预留和 issue-ready 分开一拍 | sequence 保存单批 pending uid/epoch/launch cycle；每次 `finish_item()` 返回先完成上一批，再预留当前批；epoch有效才调用 `complete_admission()`执行原CSR drain并开放issue；sample/launch progress用inout OR汇总，不互相覆盖 |
| non-LSQ 与末批 sample 边界 | 功能逻辑修改 | non-LSQ admission 可零时间返回，主循环退出也不保证再经过 driver 边界 | 零时间路径不能证明上一 LSQ request 已被采样 | pending batch 遇到 non-LSQ 时先发 idle 再走原 non-LSQ admission；退出前 pending batch 用 trailing idle drain；连续 LSQ 不插空拍 |
| `commit_allocate_with_resp()` | 共享实现重构 | 自己复制 main/status/map/pointer/free-count 更新并比较两个 key | 两个 allocation owner 容易分叉，unused key 比较无语义 | 只比较 behavior 实际使用的 response key，匹配后调用唯一 `commit_allocate()` |
| redirect sample/cancel 分工 | 功能逻辑修改 | abort/confirm 没有区分 launch 前和 launch 后 sample 前，可能不建 mapping或过早开放 issue | launch 后 request 可能被 DUT采样，redirect 必须能按 active mapping回退 | launch 前 abort不预留；launch 后先保留reservation，epoch失效时不开放issue；原redirect handler累计cancel，原LSQ cancel路径回退资源 |
| redirect/flush gate 统一 | 功能逻辑收敛 | collect、driver 和 confirm 对 redirect/flush 的观察点不统一，容易把 abort 结果当成唯一保护入口 | V2 redirect 可发生在 request 构造前、launch 前后和 sample 后；必须用已有全局 flag、epoch 和 driver launch 结果共同决定是否 admission/confirm | 不新增固定延迟 retry guard；collect 前检查 `admission_blocked_by_flush()`；driver launch 前检查 `dispatch_flush_in_progress` 和 epoch；confirm 只在 `request_launched=1`、未 abort、epoch 未失效且 global flush 无效时建立 reservation；flush 清 0 后下一轮先 apply pending LSQ cancel 再收集新 candidate |
| LSQ `drv_mode` 合同 | 配置逻辑修改 | 通用 mode 可让 idle 随机出 valid/X | idle request 不属于 sequence | V2 soft 默认 `DRV_0`，active driver 在 build 阶段拒绝非零 mode |

保持不变的主体逻辑：主表生成和 validation、连续 uid admission、已有 pointer advance/cancel/release
公式、non-LSQ admission 的判定与提交、issue scheduler 和 fired-mask、writeback、ROB commit、LSQ deq、pass/fail、
terminal 和 global stop。
