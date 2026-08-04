# mem_ut V2 编译期参数与宽度适配 Implementation Review

状态：review通过

日期：2026-07-13

对应Plan：`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md`

## 1. Review结论

本专项已经完成。V2静态结构由`memblock_compile_params.svh`和
`memblock_dispatch_types.sv`形成单一参数链，runtime plus只控制本testcase使用量；
ROB/LQ/SQ、FTQ和FuType字段链已按V2宽度编译通过。directed ROB值不再静默截断，
software smoke已从load/store/AMO三笔收敛为load/store两笔。

Review中发现并修复一项逻辑错误：AMO与普通load/store复用部分LSUOp数值，不能脱离
`op_class/fuType/lsq_flow`仅按`fuOpType`判定AMO。修正后normal、fault、replay三项
software smoke均通过，无已知blocker。

## 2. 范围和术语

| 术语 | 当前测试框架中的含义 | 代码对象 |
|---|---|---|
| compile参数 | 编译后不能由plusarg改变的接口宽度、物理端口数量和能力开关 | `memblock_compile_params.svh` |
| runtime limit | 本testcase本拍最多使用多少已有slot或pipe，不代表DUT物理数量 | `MEMBLOCK_ENQ_PER_CYCLE`、三类`*_PIP_NUM_LIMIT` |
| internal FuType | 公共主表和helper使用的36-bit one-hot容器 | `MEMBLOCK_INTERNAL_FUTYPE_W` |
| DUT-facing FuType | 写入当前V2 DUT端口的35-bit编码 | `MEMBLOCK_DUT_FUTYPE_W` |
| directed ROB fit | 保留完整输入做范围检查，合法后才转换到DUT ROB value宽度 | `fit_directed_rob_value_or_fatal()` |

本review覆盖本专项对compile宏、公共类型、agent字段宽度、runtime资源参数、主表
unsupported gate和software smoke的修改。LSQ enqueue握手、完整split issue ready/fire、
int-WB generation闭环、MMIO和L2TLB业务逻辑仍由各自专项负责。

## 3. 编译期参数链

### 3.1 功能特性、修改前和修改后

修改前，ROB/LQ/SQ、FTQ、FuType和3/2/2端口布局散落为固定宽度或runtime物理参数。
这会让V2的8-bit ROB、35-bit FuType和6/4 FTQ字段继续消费V3数值，或者在赋值时被
静默裁剪。

修改后，`.svh`只保存默认宏和compile override入口；`seq_pkg`通过同名localparam读取，
agent package和`memblock_sync_pkg`因编译顺序直接读取同源宏。没有新增compile getter或
runtime镜像。

源码位置：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`，参数定义组。

函数功能简析：该定义组提供V2默认结构事实，不读写任何运行期状态。

```systemverilog
`define MEMBLOCK_DUT_ROB_VALUE_W 8
`define MEMBLOCK_DUT_LQ_VALUE_W 7
`define MEMBLOCK_DUT_SQ_VALUE_W 6
`define MEMBLOCK_INTERNAL_FUTYPE_W 36
`define MEMBLOCK_DUT_FUTYPE_W 35
`define MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM 6
`define MEMBLOCK_DUT_LOAD_PIPE_NUM 3
`define MEMBLOCK_DUT_STA_PIPE_NUM 2
`define MEMBLOCK_DUT_STD_PIPE_NUM 2
```

中文伪代码：

该定义组在当前feature中提供编译期唯一默认值。预处理器先检查调用方是否已经覆盖同名
宏；没有覆盖时写入V2默认值。后续interface、transaction、raw类型和sequence只读取最终
宏，不允许plusarg在仿真开始后改变packed类型或物理端口数量。本段没有子函数，也不修改
queue、map、状态表或runtime参数。

### 3.2 纯一致性检查

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，函数：
`check_compile_param_consistency()`。

函数功能简析：初始化时检查width/count、port区间和FuType bit是否可编码；只fatal或返回。

```systemverilog
if (MEMBLOCK_DUT_FUTYPE_W > MEMBLOCK_INTERNAL_FUTYPE_W) begin
    `uvm_fatal("SEQ_COMPILE_CFG", "DUT FuType width exceeds internal width")
end
if (MEMBLOCK_DUT_STA_PORT_BASE !=
    MEMBLOCK_DUT_LOAD_PORT_BASE + MEMBLOCK_DUT_LOAD_PIPE_NUM ||
    MEMBLOCK_DUT_STD_PORT_BASE !=
    MEMBLOCK_DUT_STA_PORT_BASE + MEMBLOCK_DUT_STA_PIPE_NUM) begin
    `uvm_fatal("SEQ_COMPILE_CFG", "scalar issue port constants are inconsistent")
end
```

中文伪代码：

该函数在当前feature中阻止非法compile profile进入runtime。它先确认关键width/count和
MMIO load port数非0，再确认DUT FuType容器不宽于内部容器；随后检查LOAD、STA、STD三个
半开区间连续，并逐port确认唯一归属；最后检查全部FuType bit在容器内且互不重复。任一步
失败立即fatal，不调用`clamp_int()`也不写runtime字段；全部通过后返回
`validate_and_clamp()`继续处理runtime参数。

正确性检查：函数体没有`ref`参数、runtime赋值、版本标签或精确V2/V3值比较；静态扫描通过。

## 4. Runtime资源参数

### 4.1 旧对象和替代关系

| 项目 | 修改前 | 修改后 |
|---|---|---|
| 物理LSQ宽度 | 五个`MEMBLOCK_REAL_*`plus/runtime字段重复保存硬件数量 | 只读compile slot/pipe localparam |
| 固定enqueue数量 | 固定上限或runtime物理镜像参与限制 | 超过compile slot数直接fatal |
| pipe limit | 分散clamp | `apply_runtime_resource_limits()`集中warning+clamp |
| 随机enqueue | 读取runtime物理宽度 | 在完整compile slot范围内采样 |

五个旧字段原来由`plus.sv`加载、`seq_csr_common`保存并被sequence读取。它们的问题是
runtime配置可以伪装成DUT物理结构。替代后，compile localparam是物理真源；runtime字段只
表达本testcase使用量，二者不会重复维护。

### 4.2 集中收敛helper

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，函数：
`apply_runtime_resource_limits()`。

函数功能简析：读取已经加载的runtime值，按compile资源完成唯一一次fatal或clamp写回。

```systemverilog
if (enq_per_cycle == 0 ||
    enq_per_cycle > MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
    `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_ENQ_PER_CYCLE is outside compile slots")
end
clamp_int("load_pip_num_limit", load_pip_num_limit, 1,
          MEMBLOCK_DUT_LOAD_PIPE_NUM);
clamp_int("sta_pip_num_limit", sta_pip_num_limit, 1,
          MEMBLOCK_DUT_STA_PIPE_NUM);
clamp_int("std_pip_num_limit", std_pip_num_limit, 1,
          MEMBLOCK_DUT_STD_PIPE_NUM);
```

中文伪代码：

该helper在当前feature中是runtime资源写回的唯一owner。它先确认四个compile资源数非0；
固定enqueue数量为0或超过物理slot时直接fatal，不修改值继续运行。随后调用
`clamp_int()`分别把LOAD、STA、STD limit收敛到对应物理pipe范围；该子函数在越界时打印
warning并写回边界值。完成后返回总入口，后续地址、权重和timeout检查继续执行。

### 4.3 总入口和随机getter

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，函数：
`validate_and_clamp()`。

函数功能简析：编排compile纯检查、runtime资源收敛及AMO/CBO配置边界。

```systemverilog
check_compile_param_consistency();
apply_runtime_resource_limits();
if (op_class_amo_wt != 0) `uvm_fatal("SEQ_CSR_CFG", "AMO is unsupported")
if (op_class_cbo_wt != 0) `uvm_fatal("SEQ_CSR_CFG", "CBO is unsupported")
```

中文伪代码：

该总入口先调用纯检查确认compile profile可用，再调用资源helper收敛runtime使用量。随后
分别检查AMO和CBO op-class权重；任一显式非0请求立即fatal，不静默改成0。通过后继续原有
地址窗口、权重组和timeout检查。本函数只修改runtime快照，不修改compile类型、主表或公共
状态。

源码位置：同文件，函数：`get_enq_per_cycle()`。

函数功能简析：返回本拍行为使用量，不返回或修改DUT物理结构。

```systemverilog
if (enq_per_cycle_rand_en) begin
    return $urandom_range(MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM, 1);
end
return enq_per_cycle;
```

中文伪代码：

该getter先确认参数初始化完成。随机开关打开时，在1到compile slot总数之间采样并返回；
关闭时返回已经由资源helper验证的固定值。它不保存随机结果，也不改变下一拍行为。其唯一
子调用`check_initialized()`负责防止未初始化读取，失败时fatal。

### 4.4 Plus加载表

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，函数：`reload_from_cmdline()`。

函数功能简析：重新读取公共runtime plusarg；不再定义或加载硬件物理数量镜像。

```systemverilog
load_int("MEMBLOCK_ENQ_PER_CYCLE", MEMBLOCK_ENQ_PER_CYCLE);
load_int("MEMBLOCK_LOAD_PIP_NUM_LIMIT", MEMBLOCK_LOAD_PIP_NUM_LIMIT);
load_int("MEMBLOCK_STA_PIP_NUM_LIMIT", MEMBLOCK_STA_PIP_NUM_LIMIT);
load_int("MEMBLOCK_STD_PIP_NUM_LIMIT", MEMBLOCK_STD_PIP_NUM_LIMIT);
load_int("MEMBLOCK_OP_CLASS_AMO_WT", MEMBLOCK_OP_CLASS_AMO_WT);
load_int("MEMBLOCK_OP_CLASS_CBO_WT", MEMBLOCK_OP_CLASS_CBO_WT);
```

中文伪代码：

该函数在当前feature中只加载runtime行为数量和op权重。它按既有顺序调用`load_int()`；该
子函数读取同名plusarg，合法时写入对应static字段，非法格式时fatal。旧
`MEMBLOCK_REAL_LSQ_ENQ_MAX/ENQ_WIDTH/LOAD_PIPE_NUM/STA_PIPE_NUM/STD_PIPE_NUM`不再定义、
加载或打印，因此调用结束后不存在runtime物理结构快照。后续`seq_csr_common`只复制保留的
行为参数并交给集中资源helper检查。

## 5. FuType和字段宽度边界

### 5.1 FuType转换helper

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，函数：
`encode_and_fit_dut_futype()`。

函数功能简析：检查internal FuType是否属于当前scalar集合，并在确认高位为0后返回DUT宽度值。

```systemverilog
case (internal_fuType)
    MEMBLOCK_FUTYPE_LDU,
    MEMBLOCK_FUTYPE_STU,
    MEMBLOCK_FUTYPE_MOU: begin end
    default: `uvm_fatal("MEMBLOCK_FUTYPE", "unsupported FuType")
endcase
if ((internal_fuType >> MEMBLOCK_DUT_FUTYPE_W) != '0)
    `uvm_fatal("MEMBLOCK_FUTYPE", "FuType exceeds DUT width")
return internal_fuType[MEMBLOCK_DUT_FUTYPE_W-1:0];
```

中文伪代码：

该helper在internal transaction写DUT端口前建立检查边界。它先按当前profile one-hot常量
识别LDU、STU、MOU；vector或未知值立即fatal。随后右移检查DUT宽度以上是否有非0 bit，
有则fatal；只有全部检查通过才返回低`MEMBLOCK_DUT_FUTYPE_W`位。调用者在fatal前不会写
半个payload，本函数不修改transaction或公共状态。

### 5.2 机械字段链

interface/xaction/monitor/XZ和raw类型中的ROB/LQ/SQ、FTQ及FuType声明均改读同源宏。
代表性链路如下：

```text
memblock_compile_params.svh
  -> agent interface/xaction/monitor XZ
  -> *_agent_connect.sv同宽直连
  -> memblock_sync_pkg raw字段
  -> adapter/main/status transaction的package localparam字段
```

静态审计确认未把同宽但不同语义的`pdest`、`uopIdx`、count或普通数据字段替换成index宏。
`tb/dut_inst.sv`保留RTL展开后的固定wire宽度，它是DUT边界事实，不是测试框架第二权威。

issue consumer中的`is_valid_pipe_idx()`改读三类compile count，fired-mask索引改读三类port
base；候选选择、target语义和fire状态更新不变。完整物理ready映射仍属于split issue专项。

## 6. Directed ROB fit

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，函数：
`fit_directed_rob_value_or_fatal()`。

函数功能简析：检查完整`int unsigned`输入能否由当前ROB value宽度表示，合法后显式转换。

```systemverilog
exclusive_limit = 64'd1 << MEMBLOCK_ROB_VALUE_W;
promoted_value = value;
if (promoted_value >= exclusive_limit) begin
    `uvm_fatal("MEMBLOCK_ROB_FIT", "directed ROB value exceeds width")
end
return MEMBLOCK_ROB_VALUE_W'(value);
```

中文伪代码：

该helper先确认ROB width非0且小于64，非法width立即fatal，避免无效移位。然后用64-bit 1
左移形成第一个不可表示值，把原始32-bit无符号输入无损提升到64 bit后比较；越界时fatal并
报告调用位置。只有合法时才执行当前ROB宽度的sized cast并返回。它不创建transaction、
不保存static状态，也没有clamp或失败返回值。

调用关系：

| 调用者 | 功能 | helper返回后的行为 |
|---|---|---|
| real manual `make_directed_transaction()` | 构造真实mixed load/store主表 | 把fitted值写入新transaction |
| software `make_directed_transaction()` | 构造软件闭环load/store主表 | 把fitted值写入新transaction |

两个builder中均无`rob_value[...]`低位slice或第二套范围检查。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv`，
函数：`make_directed_transaction()`。

函数功能简析：real mixed builder复用共享ROB fit，再保持原load/store模板构造顺序。

```systemverilog
fitted_rob_value = fit_directed_rob_value_or_fatal(
    rob_value, $sformatf("%s::%s", get_type_name(), tr_name));
tr = main_control_transaction::type_id::create(tr_name);
tr.robIdx_value = fitted_rob_value;
```

中文伪代码：

该builder先把未截断ROB输入和调用上下文传给共享helper；helper负责范围检查和显式转换，
fatal时builder不创建可继续使用的transaction。成功后创建对象，null时fatal，再把fitted值
写入ROB字段；随后按原顺序填load或store模板并更新vaddr。本函数不复制width检查，不写公共
主表或状态，返回对象后由调用者决定是否导入。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`，
函数：`make_directed_transaction()`。

函数功能简析：software builder使用同一个ROB fit，并只接受当前闭环的load/store op class。

```systemverilog
fitted_rob_value = fit_directed_rob_value_or_fatal(
    rob_value, $sformatf("%s::%s", get_type_name(), tr_name));
tr = main_control_transaction::type_id::create(tr_name);
tr.robIdx_value = fitted_rob_value;
```

中文伪代码：

该builder同样先调用共享helper，非法ROB输入立即fatal且不产生半成品对象。成功创建对象后
写入fitted值和公共字段，再按op class选择LDU load或STU store模板；AMO、CBO和其它类型进入
default fatal。最后只更新当前对象vaddr并返回，不直接修改main table、queue、map或status。

## 7. AMO/CBO运行期边界

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，
函数：`validate_main_table_entry()`。

函数功能简析：在主表落表和admission前拒绝当前V2未闭环的AMO/CBO语义，不写公共状态。

```systemverilog
if (tr.op_class == MEMBLOCK_OP_CLASS_AMO ||
    tr.fuType == MEMBLOCK_FUTYPE_MOU ||
    tr.lsq_flow == MEMBLOCK_LSQ_FLOW_ATOMIC) begin
    `uvm_fatal(get_type_name(), "unsupported scalar atomic")
end
if (tr.op_class == MEMBLOCK_OP_CLASS_CBO ||
    tr.lsq_flow == MEMBLOCK_LSQ_FLOW_CBO ||
    lsq_ctrl_model::is_cbo_fuoptype(tr.fuOpType)) begin
    `uvm_fatal(get_type_name(), "unsupported scalar CBO")
end
```

中文伪代码：

该函数先拒绝null transaction。随后检查AMO的三个无歧义语义字段；任一命中就fatal，
主表ready、admission和pointer均不推进。它不独立按AMO fuOpType判定，因为普通`LD/SD`
与AMO复用部分编码。接着检查CBO op class、flow和当前不与合法store模板重叠的CBO编码；
命中同样fatal。两组前置检查通过后才执行原有地址、ROB、vector、numLsElem和模板校验。
既有子函数`derive_op_behavior()`只在前置gate通过后推导合法scalar行为，不会把unsupported
entry写入queue或状态表。

正确性检查：第一次smoke曾把普通`LD`误判为AMO；移除歧义判断后normal/fault/replay均通过。
显式AMO仍由op class/MOU/ATOMIC三项覆盖，显式CBO仍由独立分支覆盖。

## 8. Software smoke两笔收敛

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`，
函数：`new()`和task：`build_directed_main_table()`。

函数功能简析：构造函数保存唯一transaction数量；build task只导入uid 0 load和uid 1 store。

```systemverilog
dispatch_smoke_trans_num = 2;
set_manual_main_transaction(0, make_directed_transaction(
    "dispatch_smoke_load", MEMBLOCK_OP_CLASS_INT_LOAD, 0, 64'h1000));
set_manual_main_transaction(1, make_directed_transaction(
    "dispatch_smoke_store", MEMBLOCK_OP_CLASS_STORE, 1, 64'h2000));
```

中文伪代码：

构造函数在基类初始化后把唯一数量字段设为2，不创建公共状态。build task先清空manual表，
再调用software builder构造load和store并写入索引0、1；builder内部先执行共享ROB fit，随后
分别填LDU/LOAD/LD和STU/STORE/SD模板。第三笔AMO、MOU和ATOMIC分支已删除，其它op class
进入default fatal。最后导入两笔主表；后续admission、issue、commit/deq和final helper继续
按`data.main_trans_num`参数化遍历，没有改写主体算法。

结果是uid 0产生LOAD target，uid 1产生STA和STD target；normal smoke形成两笔ROB commit、
LQ/SQ各出队一笔。fault和replay smoke继承同一两笔主表并正常收敛。

## 9. 验证结果

| 验证 | 结果 |
|---|---|
| 本专项旧参数、固定宽度、固定port offset、ROB slice静态扫描 | 通过 |
| `git diff --check` | 通过 |
| `make eda_compile tc=tc_sanity mode=base_fun` | 通过，0 error |
| `tc_sanity` | 通过，`UVM_ERROR/FATAL=0` |
| `tc_dispatch_smoke` | 通过，两笔commit、LQ/SQ各deq 1 |
| `tc_dispatch_fault_smoke` | 通过，uid 0 fault、uid 1正常完成 |
| `tc_dispatch_replay_smoke` | 通过，uid 1 STA replay后完成 |

`tc_sanity`本身不建立主表，验证时关闭了会等待主表的LSQ enqueue/commit常驻sequence。
三项dispatch smoke保持默认完整软件闭环配置。`make eda_run`会重复compile；最终使用
`eda_compile`一次生成的`simv`配合`eda_batch_run`执行各testcase。

## 10. Plan对齐检查

### 10.1 实现与Plan不一致项

| 原Plan | 当前实现 | 原因 | 处理结论 |
|---|---|---|---|
| AMO gate独立检查`is_amo_fuoptype()` | 只用op class/MOU/ATOMIC无歧义字段前置拒绝 | LSUOp编码复用会把普通LD误判为AMO | 源码和plan均已修正 |
| `soft_test_and_mixed_directed_flow.md`只读 | 同步两笔场景和共享ROB fit | 用户要求执行完成同步对应文档 | 已写入`IMPLEMENTATION_DELTA` |

以上不一致均已在专项plan第12.2节明确记录，不存在未处理差异。

### 10.2 Plan未说明但Coding落实的细节

| 细节 | 作用 | 是否回写Plan |
|---|---|---|
| ROB fit用`promoted_value`替代复合类型cast | 兼容VCS Q-2020语法，保持无符号64-bit比较 | 已回写 |
| `is_valid_pipe_idx()`和`mark_fired_items()`消费compile count/base | 删除3/2/2和`+3/+5`第二权威，不改业务算法 | 已回写 |
| `lsq_ctrl_model::is_vector_ls_futype()`输入参数化 | 避免内部FuType容器保留固定36第二权威 | 已回写 |
| MMIO load port count非0、DUT FuType不宽于internal检查 | 防止非法compile profile通过初始化 | 已回写 |

## 11. 非本次修改的逻辑分析

### 11.1 git status对比结论

本次review覆盖compile/width相关源码、参数文档、三份对应flow文档、专项plan、总控plan和
本review文档。当前工作区还存在大量其它未提交修改，未回滚也未纳入本专项正确性结论：

| 类别 | 代表路径 | 归属判断 |
|---|---|---|
| int-WB V3到V2命名和generation逻辑 | `io_mem_to_ooo_int_wb_agent_agent/**`、对应connect | 本review只覆盖其中ROB/LQ/SQ宽度宏；其它逻辑归int-WB专项 |
| DCache L2 sideband | `dcache_agent_agent/**`、`dcache_agent_connect.sv` | 归DCache sideband专项 |
| CSR、L2TLB、MMIO、IQ等plan和分析 | `AI_DOC/plan/test_framework/plan/undo/**`、`AI_DOC/analysis/**` | 其它V2适配专项 |
| 其它flow状态/generation说明 | `AI_DOC/mem_ut_flow_doc/**` | 归对应运行期flow review |
| version profile和项目入口 | `rule/version/v2/**`、`AGENTS.md` | 已有项目管理修改，不由本专项评审 |
| 生成产物 | `sim/.compileDeletedAssertionPC`、`.humanize/` | 非源码review对象 |

同一文件同时含本专项宽度修改和其它专项修改时，本review只确认宏化声明、X/Z宽度和同宽
直连；其它新增字段、重命名、adapter状态语义必须由对应专项单独review。

## 12. 最终结论

源码实现、同步文档、静态检查和指定回归与修正后的plan一致。compile/width专项可以归档
到`plan/do`。剩余LSQ enqueue、split issue、int-WB、MMIO等专项仍未因本项完成而自动完成，
但不构成本专项blocker。
