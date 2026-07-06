# 主表异常与属性字段权重化方案

状态：未实现。

创建日期：2026-06-25

关联源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

## 1. 背景

`main_control_transaction` 中已经存在以下主表控制字段：

```systemverilog
rand bit                 tlbAF;
rand bit                 tlbPF;
rand bit                 tlbGPF;
rand bit [1:0]           PBMT;
rand bit                 pmaAF;
rand bit                 corrupt;
rand bit                 denied;
```

中文文字伪代码：

这组字段属于主表激励 payload。`tlbAF/tlbPF/tlbGPF/pmaAF/PBMT` 用于描述翻译、PMA、PBMT 相关返回属性；`corrupt/denied` 用于描述 DCache/SBuffer/TL 返回异常属性。字段已经在 transaction 中声明，但声明本身不代表随机主表已经可以按权重生成这些场景，也不代表后续 TLB 或 memory responder 已经完整消费这些字段。

当前 `randomize_main_transaction()` 在主表随机生成后固定清零：

```systemverilog
tr.tlbAF        = 1'b0;
tr.tlbPF        = 1'b0;
tr.tlbGPF       = 1'b0;
tr.PBMT         = '0;
tr.pmaAF        = 1'b0;
tr.corrupt      = 1'b0;
tr.denied       = 1'b0;
```

中文文字伪代码：

当前随机主表生成每条 transaction 后，直接把 TLB fault、PBMT、PMA fault、DCache/TL corrupt 和 denied 字段全部置为正常值。这样可以保证基础 smoke 默认走正常路径，但无法通过 plus/cfg 控制异常或属性场景比例；即使字段声明为 `rand`，这里的固定赋值也会覆盖随机结果。

## 2. 修改目标

把上述固定赋值替换成由 plus 权重控制的随机赋值。

目标要求：

- 不再在 `randomize_main_transaction()` 中固定把这些字段写死为 0。
- `tlbAF/tlbPF/tlbGPF/pmaAF/corrupt/denied` 使用一个 `*_EN_1_WT` 控制置 1 权重，置 0 权重为 `100 - *_EN_1_WT`。
- `PBMT` 支持 4 个取值 `0/1/2/3` 的独立权重。
- 默认配置必须保持现有行为：所有 fault/error 位默认生成 0，`PBMT` 默认生成 0。
- 本计划第一阶段只改变主表字段生成策略；`tlbAF/tlbPF/tlbGPF/PBMT/pmaAF/corrupt/denied` 从主表到 TLB entry、memory responder、writeback/checker 的完整消费闭环必须作为后续专项继续实现或复查。
- 为了能验证参数确实生效，需要同步扩展主表 report/debug 输出，至少在 real smoke 或主表构建日志中打印这些字段。
- 新增参数必须走公共参数链路：`plus.sv -> seq_csr_common.sv -> default.cfg -> randomize_main_transaction()`。

## 3. 新增参数

### 3.1 bit 类字段权重

新增 plus 参数：

```text
MEMBLOCK_MAIN_TLBAF_EN_1_WT
MEMBLOCK_MAIN_TLBPF_EN_1_WT
MEMBLOCK_MAIN_TLBGPF_EN_1_WT
MEMBLOCK_MAIN_PMAAF_EN_1_WT
MEMBLOCK_MAIN_CORRUPT_EN_1_WT
MEMBLOCK_MAIN_DENIED_EN_1_WT
```

默认值全部为 0。

语义：

- 取值范围 `[0:100]`。
- `*_EN_1_WT = N` 表示该字段每条随机主表生成时约 `N%` 概率为 1。
- 字段为 0 的权重不单独配置，由 `100 - N` 自动得到。

中文文字伪代码：

读取每个 `*_EN_1_WT` 参数后先限制到 `[0:100]`。随机主表生成某个 bit 字段时，把 `EN_1_WT` 当作置 1 权重，把 `100 - EN_1_WT` 当作置 0 权重。默认 `EN_1_WT=0` 时，置 1 权重为 0，置 0 权重为 100，因此结果保持固定为 0。

### 3.2 PBMT 取值权重

新增 plus 参数：

```text
MEMBLOCK_MAIN_PBMT_PMA_WT
MEMBLOCK_MAIN_PBMT_NC_WT
MEMBLOCK_MAIN_PBMT_IO_WT
MEMBLOCK_MAIN_PBMT_RESERVED_WT
```

默认值：

```text
MEMBLOCK_MAIN_PBMT_PMA_WT=1
MEMBLOCK_MAIN_PBMT_NC_WT=0
MEMBLOCK_MAIN_PBMT_IO_WT=0
MEMBLOCK_MAIN_PBMT_RESERVED_WT=0
```

语义：

RTL 依据：`build_memblock/rtl/LoadUnitS2.sv` 中 `pbmt == 2'h1` 进入 NC，`pbmt == 2'h2` 进入 MMIO/IO，`pbmt == 2'h0` 结合 PMP/PMA 的 MMIO 判断；`2'h3` 按 Svpbmt 语义属于 reserved 编码。

- `MEMBLOCK_MAIN_PBMT_PMA_WT` 对应 `PBMT=2'b00`，表示按 PMA/默认内存类型判断；在当前 RTL 中 `PBMT=0` 仍可能结合 PMP/PMA 的 MMIO 判断走 IO。
- `MEMBLOCK_MAIN_PBMT_NC_WT` 对应 `PBMT=2'b01`，表示 Non-cacheable/NC 类型。
- `MEMBLOCK_MAIN_PBMT_IO_WT` 对应 `PBMT=2'b10`，表示 IO/MMIO 类型。
- `MEMBLOCK_MAIN_PBMT_RESERVED_WT` 对应 `PBMT=2'b11`，属于 Svpbmt reserved 取值，默认关闭，只建议后续非法/保留编码专项显式打开。
- 4 个权重不能同时为 0。
- 随机结果直接写入 `tr.PBMT[1:0]`。
- 默认只选择 `PBMT=2'b00`，保持当前 smoke 行为。

中文文字伪代码：

读取 PBMT 四个语义权重后，检查不能全为 0。生成主表时计算总权重并采样一次随机数；随机数落在 PMA 权重区间时选择 `PBMT=2'b00`，落在 NC 权重区间时选择 `PBMT=2'b01`，落在 IO 权重区间时选择 `PBMT=2'b10`，剩余区间选择 reserved 的 `PBMT=2'b11`。默认只有 `PBMT_PMA_WT` 非零，因此每次都生成 `PBMT=2'b00`。

## 4. 代码修改点

### 4.1 `plus.sv`

新增字段定义：

```systemverilog
// 主表异常/PMA/PBMT/TL返回属性随机控制。默认保持正常路径。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TLBAF_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TLBPF_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TLBGPF_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_PMAAF_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_CORRUPT_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_DENIED_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_PBMT_PMA_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_PBMT_NC_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_PBMT_IO_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_PBMT_RESERVED_WT, int, 0)
```

中文文字伪代码：

在 plus 参数类中声明 10 个新的运行期参数。前 6 个控制主表 bit 字段置 1 的概率，后 4 个控制 PBMT 四种取值的概率。默认值选择正常路径，避免新增参数后改变 `tc_sanity` 和现有 smoke 的默认随机分布。

新增命令行读取：

```systemverilog
load_int("MEMBLOCK_MAIN_TLBAF_EN_1_WT", MEMBLOCK_MAIN_TLBAF_EN_1_WT);
load_int("MEMBLOCK_MAIN_TLBPF_EN_1_WT", MEMBLOCK_MAIN_TLBPF_EN_1_WT);
load_int("MEMBLOCK_MAIN_TLBGPF_EN_1_WT", MEMBLOCK_MAIN_TLBGPF_EN_1_WT);
load_int("MEMBLOCK_MAIN_PMAAF_EN_1_WT", MEMBLOCK_MAIN_PMAAF_EN_1_WT);
load_int("MEMBLOCK_MAIN_CORRUPT_EN_1_WT", MEMBLOCK_MAIN_CORRUPT_EN_1_WT);
load_int("MEMBLOCK_MAIN_DENIED_EN_1_WT", MEMBLOCK_MAIN_DENIED_EN_1_WT);
load_int("MEMBLOCK_MAIN_PBMT_PMA_WT", MEMBLOCK_MAIN_PBMT_PMA_WT);
load_int("MEMBLOCK_MAIN_PBMT_NC_WT", MEMBLOCK_MAIN_PBMT_NC_WT);
load_int("MEMBLOCK_MAIN_PBMT_IO_WT", MEMBLOCK_MAIN_PBMT_IO_WT);
load_int("MEMBLOCK_MAIN_PBMT_RESERVED_WT", MEMBLOCK_MAIN_PBMT_RESERVED_WT);
```

中文文字伪代码：

`reload_from_cmdline()` 从命令行或 cfg 展开的 plusarg 中读取新参数。如果用户没有传入对应 plusarg，则保留字段定义处的默认值。如果用户传入非法整数格式，沿用 `load_int()` 的 warning/忽略逻辑。

### 4.2 `seq_csr_common.sv`

新增静态字段：

```systemverilog
static int unsigned main_tlbaf_en_1_wt = 0;
static int unsigned main_tlbpf_en_1_wt = 0;
static int unsigned main_tlbgpf_en_1_wt = 0;
static int unsigned main_pmaaf_en_1_wt = 0;
static int unsigned main_corrupt_en_1_wt = 0;
static int unsigned main_denied_en_1_wt = 0;
static int unsigned main_pbmt_pma_wt = 1;
static int unsigned main_pbmt_nc_wt = 0;
static int unsigned main_pbmt_io_wt = 0;
static int unsigned main_pbmt_reserved_wt = 0;
```

中文文字伪代码：

`seq_csr_common` 保存参数快照，避免 sequence/helper 直接长期读取 `plus::MEMBLOCK_*`。这些字段在初始化或 reload 时由 plus 值赋值，后续主表生成只通过 getter/sample 函数读取。

新增加载逻辑：

```systemverilog
main_tlbaf_en_1_wt   = get_non_negative_int("MEMBLOCK_MAIN_TLBAF_EN_1_WT", plus::MEMBLOCK_MAIN_TLBAF_EN_1_WT);
main_tlbpf_en_1_wt   = get_non_negative_int("MEMBLOCK_MAIN_TLBPF_EN_1_WT", plus::MEMBLOCK_MAIN_TLBPF_EN_1_WT);
main_tlbgpf_en_1_wt  = get_non_negative_int("MEMBLOCK_MAIN_TLBGPF_EN_1_WT", plus::MEMBLOCK_MAIN_TLBGPF_EN_1_WT);
main_pmaaf_en_1_wt   = get_non_negative_int("MEMBLOCK_MAIN_PMAAF_EN_1_WT", plus::MEMBLOCK_MAIN_PMAAF_EN_1_WT);
main_corrupt_en_1_wt = get_non_negative_int("MEMBLOCK_MAIN_CORRUPT_EN_1_WT", plus::MEMBLOCK_MAIN_CORRUPT_EN_1_WT);
main_denied_en_1_wt  = get_non_negative_int("MEMBLOCK_MAIN_DENIED_EN_1_WT", plus::MEMBLOCK_MAIN_DENIED_EN_1_WT);
main_pbmt_pma_wt       = get_non_negative_int("MEMBLOCK_MAIN_PBMT_PMA_WT", plus::MEMBLOCK_MAIN_PBMT_PMA_WT);
main_pbmt_nc_wt       = get_non_negative_int("MEMBLOCK_MAIN_PBMT_NC_WT", plus::MEMBLOCK_MAIN_PBMT_NC_WT);
main_pbmt_io_wt       = get_non_negative_int("MEMBLOCK_MAIN_PBMT_IO_WT", plus::MEMBLOCK_MAIN_PBMT_IO_WT);
main_pbmt_reserved_wt       = get_non_negative_int("MEMBLOCK_MAIN_PBMT_RESERVED_WT", plus::MEMBLOCK_MAIN_PBMT_RESERVED_WT);
```

中文文字伪代码：

初始化公共参数时，把 plus 中的新字段读入 `seq_csr_common`。每个参数必须是非负整数；如果用户传入负数，配置初始化直接 fatal，避免后续权重计算出现无意义结果。

新增合法性检查：

```systemverilog
clamp_int("main_tlbaf_en_1_wt", main_tlbaf_en_1_wt, 0, 100);
clamp_int("main_tlbpf_en_1_wt", main_tlbpf_en_1_wt, 0, 100);
clamp_int("main_tlbgpf_en_1_wt", main_tlbgpf_en_1_wt, 0, 100);
clamp_int("main_pmaaf_en_1_wt", main_pmaaf_en_1_wt, 0, 100);
clamp_int("main_corrupt_en_1_wt", main_corrupt_en_1_wt, 0, 100);
clamp_int("main_denied_en_1_wt", main_denied_en_1_wt, 0, 100);
fatal_if_all_zero4("main PBMT weights", main_pbmt_pma_wt, main_pbmt_nc_wt, main_pbmt_io_wt, main_pbmt_reserved_wt);
```

中文文字伪代码：

对 bit 字段置 1 权重做 `[0:100]` 限制，超过范围时按现有 `clamp_int()` 规则 warning 后截断。PBMT 四个权重允许任意非负组合，但不能全为 0；如果全为 0，主表生成无法选择合法 PBMT 值，因此配置初始化 fatal。

新增采样函数：

```systemverilog
static function bit sample_main_binary_field(input string name, input int unsigned one_wt);
    bit          value;
    int unsigned zero_wt;

    check_initialized(name);
    zero_wt = 100 - one_wt;
    if (!std::randomize(value) with {
        value dist {
            1'b1 := one_wt,
            1'b0 := zero_wt
        };
    }) begin
        `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s randomize failed", name))
    end
    return value;
endfunction:sample_main_binary_field
```

中文文字伪代码：

该函数用于按百分比权重生成主表 bit 字段。函数先确认 `seq_csr_common` 已初始化，再用 `one_wt` 作为置 1 权重，并计算 `zero_wt = 100 - one_wt` 作为置 0 权重。随后调用 SystemVerilog `std::randomize(value) with dist` 进行约束随机；随机成功后返回 `value`，随机失败则 fatal。`one_wt` 的范围必须已经在 `validate_and_clamp()` 中被限制到 `[0:100]`，因此这里不会出现负权重或超过 100 的权重。默认 `one_wt=0` 时只会生成 0，`one_wt=100` 时只会生成 1。

新增专用 wrapper：

```systemverilog
static function bit sample_main_tlbaf();
    return sample_main_binary_field("sample_main_tlbaf", main_tlbaf_en_1_wt);
endfunction

static function bit sample_main_tlbpf();
    return sample_main_binary_field("sample_main_tlbpf", main_tlbpf_en_1_wt);
endfunction

static function bit sample_main_tlbgpf();
    return sample_main_binary_field("sample_main_tlbgpf", main_tlbgpf_en_1_wt);
endfunction

static function bit sample_main_pmaaf();
    return sample_main_binary_field("sample_main_pmaaf", main_pmaaf_en_1_wt);
endfunction

static function bit sample_main_corrupt();
    return sample_main_binary_field("sample_main_corrupt", main_corrupt_en_1_wt);
endfunction

static function bit sample_main_denied();
    return sample_main_binary_field("sample_main_denied", main_denied_en_1_wt);
endfunction
```

中文文字伪代码：

这些 wrapper 给主表生成函数提供语义明确的读取入口。每个 wrapper 只负责选择对应字段的权重并调用公共 bit 采样函数；主表生成代码不需要知道权重字段名，也不直接读 plus。

新增 PBMT 采样函数：

```systemverilog
static function bit [1:0] sample_main_pbmt();
    bit [1:0] pbmt;

    check_initialized("sample_main_pbmt");
    if (!std::randomize(pbmt) with {
        pbmt dist {
            2'b00 := main_pbmt_pma_wt,
            2'b01 := main_pbmt_nc_wt,
            2'b10 := main_pbmt_io_wt,
            2'b11 := main_pbmt_reserved_wt
        };
    }) begin
        `uvm_fatal("SEQ_CSR_CFG", "sample_main_pbmt randomize failed")
    end
    return pbmt;
endfunction:sample_main_pbmt
```

中文文字伪代码：

该函数按 PBMT 四个语义权重采样。函数先确认 `seq_csr_common` 已初始化，然后调用 SystemVerilog `std::randomize(pbmt) with dist`。`PBMT=2'b00` 使用 PMA/默认内存类型权重，`PBMT=2'b01` 使用 NC 权重，`PBMT=2'b10` 使用 IO/MMIO 权重，`PBMT=2'b11` 使用 reserved 权重。四个权重不能全为 0 的检查已经在 `validate_and_clamp()` 中完成；如果随机化失败则 fatal，否则返回随机得到的 `pbmt`。默认配置只有 PMA 权重非零，因此返回值保持为 `PBMT=2'b00`。

### 4.3 `randomize_main_transaction()`

旧逻辑：

```systemverilog
tr.tlbAF        = 1'b0;
tr.tlbPF        = 1'b0;
tr.tlbGPF       = 1'b0;
tr.PBMT         = '0;
tr.pmaAF        = 1'b0;
tr.corrupt      = 1'b0;
tr.denied       = 1'b0;
```

中文文字伪代码：

旧逻辑无条件生成正常翻译、正常 PMA/PBMT 和正常 DCache/TL 返回属性。该逻辑不会看 testcase cfg，也不会看 plusarg，因此无法对异常或属性字段做比例控制。

新逻辑：

```systemverilog
tr.tlbAF        = seq_csr_common::sample_main_tlbaf();
tr.tlbPF        = seq_csr_common::sample_main_tlbpf();
tr.tlbGPF       = seq_csr_common::sample_main_tlbgpf();
tr.PBMT         = seq_csr_common::sample_main_pbmt();
tr.pmaAF        = seq_csr_common::sample_main_pmaaf();
tr.corrupt      = seq_csr_common::sample_main_corrupt();
tr.denied       = seq_csr_common::sample_main_denied();
```

中文文字伪代码：

新逻辑在每条随机主表 transaction 生成时调用 `seq_csr_common` 的采样函数。TLB/PMA/TL 返回类 bit 字段按各自 `EN_1_WT` 决定是否置 1；PBMT 按 4 个取值权重选择 0 到 3。由于默认权重等价于全 0 和 PBMT 0，现有 smoke 默认行为保持不变；当 testcase cfg 显式打开权重后，随机主表可以按比例生成对应异常或属性场景。

### 4.4 主表 report/debug 输出

当前 `memblock_dispatch_real_smoke_sequence::report_main_transaction()` 主要打印 op、地址、ROB/LQ/SQ 和 send priority，没有打印本计划新增权重控制的字段。为了让参数可验证，coding 时需要同步补充输出字段：

```systemverilog
`uvm_info(get_type_name(),
          $sformatf("main uid=%0d ... tlbAF=%0d tlbPF=%0d tlbGPF=%0d PBMT=%0d pmaAF=%0d corrupt=%0d denied=%0d",
                    uid,
                    tr.tlbAF,
                    tr.tlbPF,
                    tr.tlbGPF,
                    tr.PBMT,
                    tr.pmaAF,
                    tr.corrupt,
                    tr.denied),
          UVM_LOW)
```

中文文字伪代码：

主表 report 需要把当前 uid 对应 transaction 的异常和属性字段一起打印出来。这样在默认配置下可以确认这些字段仍为 0/PBMT 0；在显式 plusarg 打开权重后，可以直接从日志确认字段是否被采样到。该修改只增加可观测性，不改变主表内容、队列状态或 DUT 驱动行为。

## 5. default.cfg 同步

`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` 必须新增：

```text
+MEMBLOCK_MAIN_TLBAF_EN_1_WT=0
+MEMBLOCK_MAIN_TLBPF_EN_1_WT=0
+MEMBLOCK_MAIN_TLBGPF_EN_1_WT=0
+MEMBLOCK_MAIN_PMAAF_EN_1_WT=0
+MEMBLOCK_MAIN_CORRUPT_EN_1_WT=0
+MEMBLOCK_MAIN_DENIED_EN_1_WT=0
+MEMBLOCK_MAIN_PBMT_PMA_WT=1
+MEMBLOCK_MAIN_PBMT_NC_WT=0
+MEMBLOCK_MAIN_PBMT_IO_WT=0
+MEMBLOCK_MAIN_PBMT_RESERVED_WT=0
```

中文文字伪代码：

默认 cfg 显式列出所有新增参数，保证 Makefile runtime cfg 路径可见。默认值必须和 `plus.sv` 中字段定义一致，使不传 cfg 和使用 default.cfg 的行为一致。

## 6. 重要边界和不在本计划内的内容

### 6.0 `tlbAF/tlbPF/tlbGPF/PBMT`

本计划把 `tlbAF/tlbPF/tlbGPF/PBMT` 从固定清零改成主表可配置随机字段，但不自动证明这些字段已经完整驱动 L2TLB response。

中文文字伪代码：

主表生成阶段会根据 plus 权重生成字段值，并保存在 `main_control_transaction` 中。后续是否影响 DUT，要看 uid 对应的 TLB record/TLB entry 建表逻辑是否读取这些主表字段，并最终写入 L2TLB response。coding 本计划时至少需要确认字段生成和 report 生效；若发现 TLB entry builder 没有消费主表字段，应记录为后续闭环任务或扩展本计划实现范围。

后续 coding 时必须额外检查：

- `main_control_transaction.tlbAF/tlbPF/tlbGPF/PBMT` 是否被拷贝到 uid 对应 TLB record 或 TLB entry。
- L2TLB response 中 `s1_pf/s1_af/s2_entry_pbmt` 是否来自对应 uid 的主表控制字段。
- fault/writeback/commit checker 是否能区分 TLB fault、PMA fault 和普通 pass。

### 6.1 `pmaAF`

本计划只让 `main_control_transaction.pmaAF` 能按权重生成。当前 L2TLB response 末端存在：

```systemverilog
resp.io_ptw_resp_bits_s1_af = entry.tlbAF || entry.pmaAF;
```

中文文字伪代码：

L2TLB response 会把 TLB entry 中的 `tlbAF` 或 `pmaAF` 合并成 access fault 返回给 DUT。但当前随机主表字段是否完整传递到 `memblock_tlb_entry` 仍需要后续专项检查和实现。本计划不把 `pmaAF` 后端消费闭环算作完成条件。

后续 coding 时必须额外检查：

- `main_control_transaction.pmaAF` 是否被拷贝到 uid 对应 TLB record 或 TLB entry。
- 如果未拷贝，需要新增主表字段到 TLB entry 的映射逻辑。
- 对应 fault/writeback/commit checker 是否能正确处理 PMA access fault。

### 6.2 `corrupt/denied`

本计划只让主表 `corrupt/denied` 能按权重生成。当前 DCache/SBuffer responder 的 D channel 已有 `denied/corrupt` 输出，但来源主要是 memory model：

```systemverilog
paddr_to_error(byte_addr, addr_corrupt, addr_denied);
corrupt |= addr_corrupt;
denied  |= addr_denied;
```

中文文字伪代码：

Memory responder 当前按物理地址和 byte mask 计算返回错误属性，再把结果写入 D channel。主表 `corrupt/denied` 并没有直接控制这条返回路径。因此即使本计划让主表字段可以随机为 1，也不代表 DUT 一定会收到对应 corrupt/denied response。

后续 coding 时必须额外检查：

- 是否存在稳定的 DCache/SBuffer request 到 uid 的反查方式。
- 是否应该按 uid 控制 `corrupt/denied`，还是按物理地址范围控制。
- `corrupt/denied` 与 store memory update、load data、writeback fault、commit 的 checker 语义。

## 7. 验证建议

基础检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC
```

编译检查：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

默认行为回归：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

参数可见性检查建议：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun plus_arg="+MEMBLOCK_MAIN_TLBAF_EN_1_WT=100 +MEMBLOCK_MAIN_PBMT_RESERVED_WT=1 +MEMBLOCK_MAIN_PBMT_PMA_WT=0"
```

中文文字伪代码：

先用 `git diff --check` 检查格式。再跑 `tc_sanity` 编译和默认仿真，确认新增参数默认值没有改变现有 smoke 行为。最后用显式 plusarg 打开一个 bit 字段和一个 PBMT 非 0 值，检查仿真日志中 plus 参数被读取，并在主表 report 或 debug 输出中能看到对应字段生效。如果当前没有主表字段 report，需要在 debug 日志中补最小可观测输出，避免参数存在但无法确认是否生效。

## 8. 类似未参数化输入激励建议（本次不实现）

本次检查聚焦 `randomize_main_transaction()` 中固定写死的 TLB/PMA/PBMT/TL 返回属性字段。本章只记录后续建议和边界说明，本次 coding 不实现本章内容；如果后续要做，需要另起专项 plan 或在本 plan 进入 coding 前明确扩展范围。

### 8.1 `lqIdx/sqIdx` 主表初值

当前随机主表中：

```systemverilog
tr.lqIdx_flag   = 1'b0;
tr.lqIdx_value  = '0;
tr.sqIdx_flag   = 1'b0;
tr.sqIdx_value  = '0;
```

中文文字伪代码：

随机主表阶段不直接随机 LQ/SQ index，而是在 LSQ admission 后由测试框架根据 DUT response 和 LSQ 模型回填。这类字段不建议简单加 plus 权重随机，因为它们属于运行期资源分配结果，不是普通输入激励属性。后续只需要确保 directed/manual 场景不会误用这些初值。

### 8.2 `numLsElem`

当前标量随机主表固定：

```systemverilog
tr.numLsElem = 5'd1;
```

中文文字伪代码：

标量 load/store 在 LSQ admission 中按 1 个元素处理，因此固定为 1 符合当前框架边界。后续如果支持 vector LS，`numLsElem` 才需要通过 vector op、VL/LMUL/segment 配置派生，而不是简单加一个独立随机权重。

### 8.3 `PC/FTQ/PDEST/RVC/MDP` 等 issue payload 字段

这些字段已经完成参数化，本次不需要实现。参数链路为 `env/plus.sv -> seq_csr_common.sv -> issue_field_assigner.sv -> lintsissue transaction`。

| 参数 | 默认值 | 当前用途 | 是否影响 DUT 功能 | 本次处理 |
|---|---:|---|---|---|
| `MEMBLOCK_MDP_LOAD_WAIT_WT` | `0` | 控制 load 是否设置 `loadWaitBit/waitForRobIdx` | 会影响 MDP/load wait 行为 | 不改 |
| `MEMBLOCK_MDP_STORESET_HIT_WT` | `0` | 控制 `storeSetHit` 和 store set 相关字段 | 会影响 StoreSet/LFST 相关行为 | 不改 |
| `MEMBLOCK_LOAD_WAIT_STRICT_WT` | `0` | 在 load wait 命中后控制 `loadWaitStrict` | 会影响 strict wait 语义 | 不改 |
| `MEMBLOCK_RVC_WT` | `0` | 控制 issue uop 的 `isRVC` | 主要影响指令长度元数据/debug，普通 MemBlock 数据路径影响弱 | 不改 |
| `MEMBLOCK_PC_BASE` | `80000000` | issue uop PC 起始值 | 影响 prefetch/MDP train/debug 等 PC 相关路径 | 不改 |
| `MEMBLOCK_PC_STRIDE` | `4` | 每个 uid 的 PC 步长 | 影响 PC 分布，进而影响 prefetch/MDP train/debug | 不改 |
| `MEMBLOCK_FTQ_IDX_BASE` | `0` | `ftqIdx_value` 起始值 | 主要是 FTQ/PC 元数据，普通 MemBlock 数据路径影响弱 | 不改 |
| `MEMBLOCK_PDEST_BASE` | `1` | 有写回 uop 的 `pdest` 起始值 | load/AMO 写回目标会使用，非纯透传 | 不改 |
| `MEMBLOCK_PDEST_RANGE` | `128` | `pdest = base + uid % range` 的循环范围 | load/AMO 写回目标会使用，非纯透传 | 不改 |

中文文字伪代码：

这些 issue payload 字段已经通过公共参数系统控制，不属于本计划新增参数范围。MDP 三个权重默认全为 0，因此默认不会主动制造 load wait、store set 或 strict wait。`RVC` 默认关闭，所有 uop 按非压缩指令元数据处理。`PC` 默认从 `0x8000_0000` 开始按 uid 步进 4；`FTQ` 默认从 0 开始；`PDEST` 默认从 1 开始并在 128 范围内循环。后续如果要调整这些字段，应作为 MDP/prefetch/backend metadata 专项，而不是混入本次 TLB/PMA/PBMT/TL 返回属性权重化任务。

### 8.4 `tlbAF/tlbPF/tlbGPF/PBMT/pmaAF` 与 TLB entry 的一致性

主表字段生成参数化后，还必须继续检查 TLB entry builder 是否使用主表字段。否则会出现主表字段已随机，但 L2TLB response 仍由 `tlb_map_builder` 自己随机或默认生成的情况。

中文文字伪代码：

后续专项应从 uid 发射记录、TLB request 建表、uid record 激活和 L2TLB response 生成几个阶段检查字段传递。目标是同一个 uid 的主表异常属性能够稳定影响它对应的 TLB response，而不是只停留在主表字段中。

### 8.5 `corrupt/denied` 与 memory responder 的一致性

主表字段生成参数化后，还必须继续检查 DCache/SBuffer request 是否能稳定反查 uid。若无法反查 uid，更合理的方案可能是按 paddr/range 建错误注入表，而不是按主表字段直接驱动 D channel。

中文文字伪代码：

后续专项应先确定 request 到 uid 的映射来源。如果能从 active map 或 transaction source 稳定拿到 uid，则可以按 uid 查询主表 `corrupt/denied` 并驱动 responder；如果不能稳定拿到 uid，则应设计地址范围错误模型，通过 `paddr_to_error()` 控制 corrupt/denied，避免把错误响应打到错误 transaction 上。
