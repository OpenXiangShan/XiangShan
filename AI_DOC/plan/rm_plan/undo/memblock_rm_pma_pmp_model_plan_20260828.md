# memblock RM PMA/PMP 独立参考模型修改方案（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 待 coding，未实施 |
| 方案类型 | RM plan |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 适用范围 | memblock UVM responder、RM、CSR runtime mirror |
| 关联分析 | [`memblock_mmu_sv39_rm_exception_mismatch_analysis_20260827.md`](../../../analysis/framework_design/memblock_mmu_sv39_rm_exception_mismatch_analysis_20260827.md) |
| 关联方案 | [`memblock_rm_l2_dcache_sticky_error_ledger_plan_20260828.md`](./memblock_rm_l2_dcache_sticky_error_ledger_plan_20260828.md) |
| 代码修改授权 | 当前未获得；本文只描述方案，不修改 RTL、RM、sequence、cfg 或 interface |
| 创建日期 | 2026-08-28 |

> 本文将用户提示中的“pma/pma”按上下文解释为 **PMA/PMP**。PMA 是物理内存属性检查，
> PMP 是物理内存保护检查；两者在 DUT 中由同一个 `PMPChecker` 合并输出，但在参考模型中
> 必须保留独立状态和独立诊断来源。

## 1. 术语和职责

| 术语 | 含义 | 本方案中的职责 |
| --- | --- | --- |
| PMA | Physical Memory Attributes，物理地址的读写执行、cacheable、atomic 属性 | 由 V2 Scala PMA profile 初始化，并响应 PMA CSR 写入 |
| PMP | Physical Memory Protection，按权限和地址范围保护物理访问 | 由 reset 状态初始化，并响应 PMP CSR 写入 |
| post-TLB 检查 | 已完成虚拟地址翻译、拿到真实 PA 后的数据路径检查 | 本次修复的第一集成目标 |
| PTW/TLB 检查 | 页表遍历或 TLB 阶段的物理访问检查 | 与 post-TLB 结果分开，避免重复计入 |
| CSR context | 一次访问使用的 privilege、debug、KEYID、CMODE 和表项 generation 快照 | 在 PA 生成时冻结，供 RM 延迟查询 |
| entry priority | 多个范围同时匹配时的硬件优先级 | 按 RTL 的 entry 0 到 entry N-1 选择首个命中项 |
| `mmio` | PMA `C=0` 导出的非 cacheable/内存映射属性 | 仅保留在模型内部诊断；RM 不读取该分类，只有实际到达 DCache 时才将 `C=0` 归约为 cache-path AF |
| `atomic_allowed` | PMA entry 是否允许原子访问 | 不作为独立比较字段；对 LR/SC/AMO 为命令相关的 AF 输入，值为 0 时直接产生对应 AF |
| raw response | `ld/st/instr/mmio/atomic` 及命中 entry 等原始结果 | 模型内部诊断和 RTL 对照；RM 不直接消费属性分类 |
| valid/eligible | 结果是否可以应用到当前数据访问 | 翻译故障、无 PA 时为假，不读取当前 PMA/PMP 表 |

## 2. 结论和总体架构

建立独立的 `pma_pmp_model`，作为 PMA/PMP 状态和 AF 计算的运行期 owner。RM 只通过
AF-only 只读 API 使用它；既有 memory responder/observer 只负责发布“本访问是否实际到达
DCache”的事实，不复用或修改 PMA/PMP 表。模型与以下对象完全解耦：

- `main_view`/主 transaction 表中的静态异常字段；
- TLB entry 表和 TLB PF/AF/GPF 推导逻辑；
- L2 DCache sticky `denied/corrupt` 账本；
- RTL XMR 路径。

模型分为四层：

```text
V2 profile/reset + CSR write monitor
            |
            v
  ordered PMA table / ordered PMP table
            |
            v
  request context snapshot (PA, size, cmd, privilege, debug, keyid, generation)
            |
            v
  PMA/PMP raw result + AF-only RM view
            |
            +--> RM Access Fault expectation
            +--> cache-access fact（由独立 observer 提供）
            +--> DCache sticky ledger gate
```

核心原则：

1. PMA 与 PMP 状态分表维护，但匹配算法、地址编码和 CSR WARL 规则与 RTL 对齐。
2. CSR 更新只影响之后建立的 request context；已发出的访问使用自己的快照。
3. PMA/PMP 结果在“翻译成功且 PA 有效”后计算一次，RM 不在 writeback/commit 时读取可变表。
4. RM 先无条件归约 PMP/PMA `R/W/X`、keyid 和当前 LR/SC/AMO atomic 的基础 AF；一旦基础 AF
   已确定，绝不等待或读取 `dcache_fact`。
5. RM 只消费 PMA/PMP 是否产生 AF 及其来源；`mmio/cacheable` 不是 RM 的比较字段，
   `atomic_allowed` 也不作为独立字段比较，但对 LR/SC/AMO 必须作为命令相关的 AF 输入。
6. 对普通 scalar Load/Store，只有基础 AF 为 0 且实际确认到达 DCache 时，PMA `C=0` 才产生
   cache-path AF；没有到达 DCache 时，`C=0` 不单独产生 AF。LR/SC/AMO 还必须遵守原子消费者
   的 non-cache path AF 规则。
7. `mmio`、permission fault、D-channel `denied/corrupt` 是不同来源，不能互相替代；模型不能
   为了让回归通过而修改 RTL，无法解释的差异必须保留日志和波形证据。

### 2.1 方案比较和选型

| 方案 | 主要问题 | 结论 |
| --- | --- | --- |
| 继续读取 `main_view` | 建表时静态值无法反映 CSR 写、privilege 和实际 PA | 不采用 |
| 每次 RM 通过 XMR 读取 RTL entry | 层次名随 RTL 生成变化，且难以冻结 outstanding request 的历史上下文 | 只保留为可选校验 |
| 只写一个地址范围近似表 | 丢失 TOR/NAPOT、lock/WARL、entry priority、debug/keyid 和跨界语义 | 不采用 |
| 独立 ordered PMA/PMP 表 + CSR 镜像 + request snapshot | 状态边界清晰，可复刻硬件算法并支持动态 CSR | **采用** |

长期最优落地是“Scala profile 自动生成 + SV 独立运行期模型”；在自动生成链路完成前，先用
版本化 V2 profile 配合启动校验，避免阻塞当前 RM 修复。

## 3. DUT 事实和必须复刻的边界

### 3.1 参数和实例

当前 V2 配置的关键参数为：

| 参数 | 当前值 | 来源 |
| --- | ---: | --- |
| `PAddrBits` | 48 | `src/main/scala/system/SoC.scala` |
| `PlatformGrain` | 4096 bytes | `src/main/scala/xiangshan/PMParameters.scala` |
| `NumPMPReal` | 32 | `src/main/scala/xiangshan/PMParameters.scala` |
| `NumPMAReal` | 32 | `src/main/scala/xiangshan/PMParameters.scala` |
| `PMPKeyIDBits` | 0（当前 V2 默认） | `CVMParamsKey` / `PMParameters.scala` |

`MemBlock.scala` 为各 DTLB/Load/Store/Atomic 端口实例化 `PMPChecker`。普通 Load/Store 的
checker 使用 `leaveHitMux=true`，因此 DUT 会在 request valid 时锁存匹配结果和 command；
模型必须以同一类 request sample 建立 context，而不能等到 commit 再查表。

### 3.2 表项初始化和优先级

- PMP 初始 `cfg`、`addr`、`mask` 全为零。
- PMA 初始值来自 `SoC.scala` 的 `PMAConfigs`，不是测试 cfg 中的地址窗口近似。
- `PMA.scala::pma_init()` 会先补足到 `NumPMAReal`，再对 `cfg/addr/mask` 做 reverse；
  因此 `PMAConfigs` 源码列表的顺序不是最终硬件 entry index。profile loader 必须复刻
  “补零 + reverse + 每八项合并 CSR”这三个步骤。
- 匹配时使用 `ParallelPriorityMux` 的 entry 顺序。实现时固定为 entry 0 优先，遇到首个
  `is_match` 即停止；不能按地址范围大小或 PMA 源码书写顺序自行排序。

当前 PMA profile 中同时存在大范围 NAPOT、RAM/设备 TOR 区间和零配置占位项。特别是
`PMAConfigEntry(0x80000000L, a = 1, w = true, r = true)` 的 `c` 默认为 0，不能假设
`0x80000000` 物理窗口天然是 cacheable；必须让模型按 profile 计算 `C`。该属性只用于模型内部
诊断以及“实际 DCache 访问是否违反 `C=0`”的 AF 归约，RM 不输出或比较 `mmio/cacheable` 分类。
这也是后续 10000 笔回归选择 PA 窗口时需要先确认的前置条件。

### 3.3 CSR 地址和写通道

模型只镜像 DUT 实际观察到的通用 CSR 写总线：

```text
io_ooo_to_mem_csrCtrl_distribute_csr_w_valid
io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr
io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data
```

解码时使用与 RTL 相同的符号常量和 XLEN 规则：

| 表 | 地址基址（V2/RV64） | 备注 |
| --- | --- | --- |
| PMP config | `CSRs.pmpcfg0`，标准实现为 `0x3a0`，每 8 项占一个 64-bit CSR 槽 | 不能按字节地址递增 |
| PMP address | `CSRs.pmpaddr0`，标准实现为 `0x3b0`，每项递增 1 | 存储的是 PA 去掉 `PMPOffBits=2` 的值 |
| PMA config | `PmacfgBase=0x7c0`，每 8 项占一个 64-bit CSR 槽 | 自定义 machine CSR |
| PMA address | `PmaaddrBase=0x7c8`，每项递增 1 | 与 PMP 地址 WARL 规则相同 |

实现时应从当前 V2 RTL/生成 CSR 常量复核标准地址；表中的数值只作为文档基线，不能让
测试 cfg 直接覆盖结构常量。

## 4. 数据结构设计

建议新增 `memblock_pma_pmp_model.sv`，提供以下值型结构。具体位宽以 V2 compile parameter
为单一权威，不在 runtime plus 中复制硬件结构。

### 4.1 entry 记录

```systemverilog
typedef enum bit [1:0] {
    PMA_PMP_A_OFF  = 2'd0,
    PMA_PMP_A_TOR  = 2'd1,
    PMA_PMP_A_NA4  = 2'd2,
    PMA_PMP_A_NAPOT = 2'd3
} pma_pmp_a_mode_e;

typedef struct {
    bit                  valid;
    int unsigned         index;
    bit                  lock;
    bit                  c;
    bit                  atomic;
    pma_pmp_a_mode_e     a;
    bit                  x;
    bit                  w;
    bit                  r;
    bit [45:0]           addr_raw;       // PAddrBits - PMPOffBits, V2
    bit [47:0]           match_mask;
    bit [47:0]           compare_addr;
    longint unsigned     update_generation;
    bit [1:0]            origin;         // reset/profile/CSR
} pma_pmp_entry_t;
```

PMA/PMP 各自维护固定长度 32 项的数组；`A=OFF` 表示不匹配，不能把整项删除或压缩，
否则后一个 TOR entry 的 lower bound 会改变。

### 4.2 request context 和评估结果

```systemverilog
typedef struct {
    bit                  valid;
    bit                  translation_success;
    bit [47:0]           paddr;
    bit [6:0]            size_bytes;
    bit [2:0]            lg_size;
    bit [2:0]            cmd;
    bit [1:0]            priv_mode;
    bit                  debug;
    bit                  keyid_enable;
    bit                  cmode;
    bit [7:0]            keyid;
    int unsigned         csr_update_seq;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_request_context_t;

typedef struct {
    bit                  valid;
    bit                  translation_eligible;
    bit                  pmp_hit;
    bit                  pma_hit;
    int signed           pmp_hit_index;
    int signed           pma_hit_index;
    bit                  pmp_ld_fault;
    bit                  pmp_st_fault;
    bit                  pmp_instr_fault;
    bit                  pma_ld_fault;
    bit                  pma_st_fault;
    bit                  pma_instr_fault;
    bit                  keyid_fault;
    bit                  ld_fault;
    bit                  st_fault;
    bit                  instr_fault;
    bit                  mmio;
    bit                  atomic_allowed;
    bit                  cacheable;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_eval_t;
```

`pma_pmp_eval_t` 是模型内部的完整评估结果，便于和 DUT `PMPChecker` 做诊断对照；其中
`mmio`、`cacheable` 和原始 `atomic_allowed` 不进入 RM 的最小读取接口。RM 使用下面的 AF-only
视图；原子命令对 `atomic_allowed=0` 的判断已经折叠为 `pma_atomic_fault`，不能因此被忽略：

```systemverilog
typedef enum bit [1:0] {
    PMA_PMP_DCACHE_FACT_UNKNOWN = 2'd0,
    PMA_PMP_DCACHE_FACT_YES     = 2'd1,
    PMA_PMP_DCACHE_FACT_NO      = 2'd2
} pma_pmp_dcache_fact_e;

typedef struct {
    bit                  valid;
    bit                  translation_eligible;
    bit                  base_ld_access_fault;
    bit                  base_st_access_fault;
    bit                  base_instr_access_fault;
    bit                  dcache_fact_needed_for_c;
    bit                  af_decided;
    bit                  pmp_ld_fault;
    bit                  pmp_st_fault;
    bit                  pmp_instr_fault;
    bit                  pma_ld_fault;
    bit                  pma_st_fault;
    bit                  pma_instr_fault;
    bit                  keyid_fault;
    bit                  pma_atomic_fault;
    bit                  atomic_noncache_path_fault;
    bit                  pma_cache_path_fault;
    bit                  ld_access_fault;
    bit                  st_access_fault;
    bit                  instr_access_fault;
    int signed           pmp_hit_index;
    int signed           pma_hit_index;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_af_view_t;
```

`base_*_access_fault` 只由 PMP/PMA `R/W/X`、keyid 和当前 LR/SC/AMO 的 atomic 检查归约，
不依赖 `dcache_fact`。`dcache_fact_needed_for_c=1` 只表示“本次是基础 AF 为 0 的普通 scalar
Load/Store 数据访问，且 PMA `C=0`，需要 DCache 访问事实才能完成 C 属性的 AF 判定”。
`af_decided=0` 只允许出现在这一种等待 C 属性事实的情形；基础 AF 已存在、原子消费者已给出
non-cache AF、PMA `C=1` 或非 DCache 数据访问时均为 1。

`pma_pmp_dcache_fact_e` 不是 PMA 模型推导出的属性，而是由独立 observer 按
`{uid,dynamic_epoch,access_seq}` 发布的三态访问事实：`YES` 表示本次标量访问已实际到达
DCache 数据路径，`NO` 表示没有到达，`UNKNOWN` 表示事实尚未收齐。RM 不读取
`mmio/cacheable`，只把 `dcache_fact=YES` 作为判断是否应用 `pma_cache_path_fault` 的输入。
`pma_atomic_fault` 是“当前 command 为 LR/SC/AMO 且 PMA 不允许 atomic”得到的 AF 来源，
不是单独的 `atomic_allowed` 属性。`atomic_noncache_path_fault` 是当前原子消费者（V2
`AtomicsUnit`）确认 non-cache 路径非法时的 AF 来源；它同样是命令相关的结果，不是对
`mmio/cacheable` 字段的独立比较。

## 5. CSR 镜像和 WARL 规则

### 5.1 初始化

**抽象功能：** `pma_pmp_model::reset_and_init_v2_profile()` 在 testcase/reset 边界建立
确定的 PMP reset 表和 PMA profile 表，并把 generation 置为 0。

实现约束：

1. PMP 32 项全部 `cfg=0/addr=0/mask=0`。
2. PMA 由版本化 profile 生成；profile 先记录 `PMAConfigs` 源项，再执行 RTL 的补零和
   reverse，最后计算每项 `compare_addr/match_mask`。
   NAPOT 源项必须使用 `PMA.scala::get_napot(base, range)` 的
   `(base + (range / 2 - 1)) >> PMPOffBits` 公式，不能用常规区间起始地址代替。
3. 初始化完成前 `is_ready()` 为假，RM 不得把“表还没准备好”当作“没有 fault”。
4. 可选地通过 XMR 读取生成 RTL 的 `_inner_pmp_io_pmp_*`、`_inner_pmp_io_pma_*` 做一次
   debug 校验，但 XMR 只用于校验，不能作为模型主数据源，以免 RTL 层次变化破坏回归。

### 5.2 config 写入

**抽象功能：** `apply_cfg_csr_write(addr, data)` 将一个实际 CSR 写按 8-bit entry 拆分，
应用锁定位、`W` 依赖 `R` 和 grain 归一化规则后更新对应表。

必须复刻 `PMPReadWriteMethodBare::write_cfg_vec()`：

- 旧 `L=1` 时整项保持不变；
- 新值 `W` 强制变为 `write_bit && read_bit`；
- `PlatformGrain > PMPOffBits` 时，A 值按 RTL 的 `Cat(a(1), a.orR)` 归一化，V2 不可用
  独立 NA4 语义；
- A 变为 NA4/NAPOT 时重新计算 `match_mask`；
- 保留 `C`、`atomic` 作为 PMA 属性，PMP 中它们仍是保留位，不得影响 PMP 权限。

### 5.3 address 写入

**抽象功能：** `apply_addr_csr_write(table, index, value)` 按当前项和下一项的锁定状态
更新地址及匹配 mask。

必须遵守 `addr_locked(next)`：

- 当前项 `L=1` 时地址不可写；
- 下一项 `L=1` 且下一项为 TOR 时，当前项也不可写；
- 未锁定时保存 PA 去掉低两位的 raw value，并按 `match_mask()` 计算 mask；
- 对低位读回归一化只影响可见 canonical value，不得改变硬件匹配边界。

PMA 与 PMP 共用这些 WARL 规则，但 generation 分开递增。无效 CSR 地址、未对齐地址或
不支持的写入只记录诊断，不得修改表。

### 5.4 写入时序

CSR monitor 在 `w.valid` 采样点调用 `apply_csr_write()`；不读取 sequence 预期值，也不等
commit 回写。若 CSR 写与 memory request 在同一采样边沿出现，先按 DUT register timing
建立一个明确的事件顺序，并增加定向测试；默认采用“该边沿 request 使用旧表，下一边沿
request 使用新表”的保守规则，若波形证明 V2 为相反顺序再调整模型和文档。

## 6. 地址匹配和权限算法

### 6.1 匹配模式

模型应逐项实现与 `PMPMatchMethod` 等价的 helper，而不是用简单 `[base, base+range)`
字符串判断：

1. **OFF**：不命中。
2. **TOR**：使用前一项 `compare_addr` 作为下界，当前项作为上界，范围为下界包含、上界
   不包含；entry 0 的下界为零。
3. **NA4**：仅在 grain 允许时有效；V2 `PlatformGrain=4KiB`，写入会按 RTL 归一化，
   不能假设 NA4 可用。
4. **NAPOT**：用 `compare_addr` 和 `match_mask` 做 unmasked-equal 比较。
5. **size/alignment**：复制 `boundMatch()` 和 `aligned()` 的跨界判断。访问跨 TOR/NAPOT
   边界或不满足对齐条件时，命中项权限应被清零；不能把一次访问拆成多个“都合法”的
   子访问来绕过硬件判断。
6. 临时计算使用至少 `PAddrBits+1` 位，避免 `paddr + size` 溢出导致与 RTL 不同。

命中选择固定为 entry 0 优先。没有任何 entry 命中时，分别使用 PMA default 和 PMP default，
不能共用一个默认配置。

### 6.2 PMP 权限

复制 `PMPCheckMethod::pmp_check()`：

| command | fault 条件 |
| --- | --- |
| read/read-exec（非 AMO） | `!R` |
| write/AMO | `!W` |
| exec/read-exec | `!X` |

PMP default 的 `R/W/X` 为 `mode > 1`（当前 RTL 的 `passThrough` 表达式）。因此：

- M 态（以及 RTL 表达式覆盖的 mode 2）对未锁定 PMP 项具有 bypass 语义；
- S/U 态未命中项默认拒绝；
- 已锁定且命中的项即使在 M 态也不能被 bypass；
- 不得把“有 PMP 表项”简化成“所有非 M 态都允许”或“所有 M 态都允许”。

### 6.3 PMA 属性和权限

复制 `PMACheckMethod::pma_check()`：

| command | fault/属性条件 |
| --- | --- |
| read/read-exec | `!R`；`atom_read` 还要求 `atomic=1` |
| write | `!W` |
| AMO | `!W || !atomic` |
| exec/read-exec | `!X` |
| 任意访问 | 保留 `C` 作为模型内部属性；它不直接生成 `mmio/cacheable` RM 比较结果 |

PMA 永远检查，不使用 PMP 的 M 态 bypass。无 PMA 命中时使用全零 default，因此会先产生
基础权限 AF，同时内部 `C=0`；基础 AF 已经足以结束本次 PMA/PMP 判定，RM 不应再为了该 `C=0`
等待 DCache 事实。`C` 只在基础 AF 为 0 的实际 DCache 数据访问上参与 cache-path AF 归约，
并且必须与 DCache `denied/corrupt` 分开。

原子命令的 `atomic_allowed` 处理必须按命令区分。V2 `TlbCmd.atom_read`（LR）与
`TlbCmd.atom_write`（SC/AMO）分别映射到 Load、Store 方向；因此：

| 命令 | PMA 原子权限条件 | RM 基础 AF 映射 |
| --- | --- | --- |
| LR (`atom_read`) | `atomic_allowed=0` | `pma_atomic_fault=1`，产生 Load Access Fault（LAF） |
| SC (`atom_write`) | `atomic_allowed=0` | `pma_atomic_fault=1`，产生 Store Access Fault（SAF） |
| AMO (`atom_write`) | `atomic_allowed=0` | `pma_atomic_fault=1`，产生 Store Access Fault（SAF） |
| 普通 Load/Store | `atomic_allowed=0` | 不因 atomic 属性单独产生 AF，仍只检查 `R/W` 及适用的 C 路径规则 |

`atomic_allowed` 为 0 时属于基础 AF，RM 立即结束该访问的 PMA/PMP 判定，不等待
`dcache_fact`。它可以与 `!R/!W` 同时出现，但最终对应方向的异常向量 bit 只置一次。

### 6.4 keyid、debug 和 CMODE

当 `KEYIDBits>0` 且 `keyIDEN=1` 时：

- PMP/PMA 地址匹配先去掉 PA 高位 KEYID；
- `keyid != 0`、`CMODE=0`、`priv_mode < M` 时，`ld/st/instr` 均产生 keyid fault；
- `CMODE=1` 或 M 态按 RTL 条件不产生该 fault。

地址落在 `debugStart..debugEnd` 时，只有 `debug=1` 才允许表项匹配；否则按无命中处理。
即使 V2 当前 `KeyIDBits=0`，结果结构仍保留这些字段，避免后续 profile 变更时静默缺失。

### 6.5 合并结果

`PMPChecker` 的最终结果等价于：

```text
combined = pmp_response | pma_response | keyid_response
```

模型应分别输出 `pmp_*`、`pma_*`、`keyid_fault`，再生成 `ld_fault/st_fault/instr_fault`。
这些字段构成不依赖 DCache 事实的基础 AF。`atomic_allowed` 来自 PMA 命中项的 `atomic`，
不能把它误当作“AMO 没有 fault”。

### 6.6 RM 的 AF-only 归约：PMA C=0 与 DCache 访问事实

PMA `C` 是硬件属性，模型内部应继续保留它以复刻 `PMPChecker`；但 **RM 不据此判断
MMIO 或 cacheable，也不校验这些分类**。RM 调用 PMA/PMP 模型的唯一目的，是得到本次是否应
产生 Access Fault 及其来源。归约必须严格分为两个阶段，避免已确定的基础 AF 因为没有 DCache
token 而被无谓阻塞。

1. **基础 AF 阶段：** `R/W/X`、keyid 和当前 LR/SC/AMO atomic 检查仅使用冻结的
   PMA/PMP request context 完成。若对应 command 的基础 AF 为 1，立即写入 expected raw vector，
   `af_decided=1`，不得读取或等待 `dcache_fact`。
2. **普通数据访问的 C 属性补充阶段：** 仅当基础 AF 为 0、当前为本期覆盖的普通 scalar
   Load/Store 数据访问、且评估结果为 `C=0` 时，读取 `dcache_fact`。`YES` 表示该访问实际走到了
   DCache 数据路径，因而设置 `pma_cache_path_fault=1` 并映射为对应 AF；`NO` 表示没有 DCache
   访问，`C=0` 不单独产生 AF；`UNKNOWN` 时才返回“C 属性事实未就绪”。
3. **原子消费者阶段：** LR/SC/AMO 的原子权限失败已经在基础 AF 阶段处理。当前 V2
   `AtomicsUnit` 还规定原子操作不能走 non-cache 路径：PMA `C=0` 导出的 non-cache 属性，或
   PBMT `NC/IO`，会在没有 DCache 访问时直接映射为对应 LAF/SAF。该结果记为
   `atomic_noncache_path_fault`，不把 `mmio` 当作 RM 比较字段，也不等待 `dcache_fact`。
   当前默认 V2 cfg 仍将 `MEMBLOCK_OP_CLASS_AMO_WT` 设为 0；只要后续 sequence 放开 LR/SC/AMO
   权重，就必须同时启用该 consumer contract 和对应事实校验，不能把它当作可忽略属性。

| 条件 | `pma_pmp_af_view_t` 的处理 |
| --- | --- |
| 翻译失败或 PA 无效 | `valid=0`，不读取 PMA/PMP，也不产生 cache-path AF |
| 对应 command 的 PMP/PMA `R/W/X`、keyid 或 LR/SC/AMO atomic 检查失败 | 设置对应 `base_*_access_fault` 和最终 `ld/st/instr_access_fault`，`af_decided=1`，不读取 `dcache_fact` |
| 基础 AF 为 0，普通 scalar 数据访问的 PMA `C=1` | 不增加 C 属性 AF，`af_decided=1`；是否查询 DCache ledger 由后续 `dcache_fact` 决定 |
| 基础 AF 为 0，普通 scalar 数据访问的 PMA `C=0`，且 `dcache_fact=YES` | 设置 `pma_cache_path_fault=1`，按 command OR 到 Access Fault，`af_decided=1` |
| 基础 AF 为 0，普通 scalar 数据访问的 PMA `C=0`，且 `dcache_fact=NO` | 不因 C 位单独设置 AF，`af_decided=1`；不查询 DCache ledger |
| 基础 AF 为 0，普通 scalar 数据访问的 PMA `C=0`，且 `dcache_fact=UNKNOWN` | `dcache_fact_needed_for_c=1`、`af_decided=0`；RM 只为完成 C 属性 AF 判定等待事实，不能猜测成 DCache/非 DCache |
| LR/SC/AMO 且原子消费者确认 non-cache 路径非法 | 设置 `atomic_noncache_path_fault=1`，按 LR/SC/AMO 方向产生 LAF/SAF，`af_decided=1`；不读取 `dcache_fact` |
| 指令访问或不属于本期 scalar DCache 数据路径的访问 | 不使用 `dcache_fact` 推导 C 属性 AF；I-Cache/其他 consumer 的 C 语义必须由独立模型覆盖 |

这里的 `dcache_fact=YES/NO` 必须来自稳定 observer token，不能由 PMA `mmio`、PBMT 编码、
主表字段、外部 L2 `A.fire` 单独事件或测试 cfg 反推。外部 L2 DCache `A.fire` 只覆盖 miss/refill，
cache hit 不会产生该事件，因此不能作为“本访问到达 DCache”的唯一判据。这样满足以下边界：

```text
PMA/PMP model:  计算基础 AF、内部 C 属性和命中 entry
RM:             只读取 AF-only view；不读取 mmio/cacheable 分类
cache observer: 以 UID/epoch/access sequence 提供 dcache_fact=YES/NO
```

建议 `cache observer` 在 Load/Store 实际进入 DCache 的内部监控点发布 `YES`；已通过可靠
non-DCache completion/token 确认未进入 DCache 时发布 `NO`。没有任一事实时保持 `UNKNOWN`。
对基础 AF 为 0、PMA `C=0` 且 `dcache_fact=YES` 的请求，`pma_cache_path_fault` 是 RM 对
“cache 访问违反 PMA C 属性”的 AF 预测；它不是把 `mmio=1` 直接映射成 AF，也不要求 RM 证明
DUT 应该走 MMIO。PBMT 仍由 TLB/Load/Store 的既有路径处理，但不成为 RM PMA/PMP 模型的比较输入。

## 7. request 快照和 RM 查询时序

### 7.1 快照建立点

**抽象功能：** `capture_request_context()` 在 TLB 已返回有效 PA、且 request 即将进入
Load/Store/Atomic 数据路径时，把当前 PMA/PMP 表和控制 CSR 的 generation 绑定到该访问。

快照至少包含：

- PA、访问 size/`lgSize`、`TlbCmd`；
- Load/Store/Instr/AMO 类型；
- `priv_imode`/`priv_dmode`（按实际 consumer 选择）；
- `debug`、`keyIDEN`、`CMODE`、KEYID；
- translation stage；PBMT/MMIO/cacheable 分类不进入 PMA/PMP AF-only RM contract；
- `pmp_generation`、`pma_generation`、`csr_update_seq`、capture sample。

之后的 CSR 写不能回写该 context。RM 在 LDA writeback、实际 Store fault/WB 或 AMO 观察点
只消费这个值型快照和已计算结果，不重新读当前表。

### 7.2 翻译故障门控

LoadUnit/StoreUnit 源码明确说明：翻译已经产生 PF/AF/GPF 后，后续 PMP/PMA response
不可信。因此：

```text
translation_success && PA_valid
    -> 允许 PMA/PMP evaluate
translation_fault || !PA_valid
    -> evaluate.valid=0，不查询 PMA/PMP 表
```

TLB entry 中的 `pma_af` 只表示 TLB/PTW 阶段事实；不得与 post-TLB PMA fault 重复 OR。
对于 DUT raw vector 在 PF 后仍保留的额外 AF（当前已观察到 `0x2020/0x8080`），RM 保留
原始诊断，但在 `translation_eligible=0` 时按既有 priority/架构 cause 比较，不把未定义的
后续 PMP/PMA response 判为模型失配。

### 7.3 Load、Store、Atomic 和跨范围

1. Load 使用 LDA writeback observer 的实际 sample；Store 使用 STA/STD fault 或真实 Store
   writeback sample，不能使用更晚的 ROB commit 时间；Atomic 使用对应 AMO writeback sample。
2. PMA/PMP 一次评估完整访问的 PA 和 size，复制硬件跨界/aligned 语义；不能像 DCache
   sticky ledger 那样按 64B line 拆开再 OR。
3. `pma_pmp_model` 先返回内部完整 result，再建立不依赖 `dcache_fact` 的基础 AF-only view；
   RM 不读取 `mmio/cacheable` 分类，也不直接比较原始 `atomic_allowed`；LR/SC/AMO 的
   `atomic_allowed=0` 必须先归约为 `pma_atomic_fault`。基础 AF 为 1 时立即结束 PMA/PMP
   判定，不能等待尚未产生的 DCache token。
4. 仅当基础 AF 为 0、普通 scalar Load/Store 的 PMA `C=0` 时，才读取 `dcache_fact` 完成
   cache-path AF 归约：`YES` 产生 AF，`NO` 不产生 C 属性 AF，`UNKNOWN` 才返回
   `FACT_NOT_READY`。LR/SC/AMO 另外遵循原子消费者的 non-cache path AF 规则。翻译成功但最终
   AF-only view 已有任一 Access Fault 时，不进入 DCache ledger 查询。
5. 只有 `translation_eligible=1`、最终 AF-only view 无 AF、且 cache observer 已确认
   `dcache_fact=YES` 的访问，才调用
   [`query_l2_d_error_at_sample`](./memblock_rm_l2_dcache_sticky_error_ledger_plan_20260828.md)
   查询 `denied/corrupt`。对基础 AF 为 0、PMA `C=1` 的数据访问，`dcache_fact=UNKNOWN` 不影响
   PMA/PMP AF 结论，但 RM 仍须等待它来决定是否查询账本；`NO/UNKNOWN` 均不得查询该账本。

## 8. RM 接入边界

### 8.1 异常唯一来源

`observer_build_commit_item()` 的期望构造应改成以下分层：

| 异常/属性 | 唯一来源 |
| --- | --- |
| S1/S2 PF、GPF、TLB AF、translation-side PMA AF | 冻结 TLB context + canonical `tlb_entry_by_key` |
| post-TLB PMP AF | `pma_pmp_model` 的 `pmp_*_fault` |
| post-TLB PMA AF | `pma_pmp_model` 的 `pma_*_fault` |
| post-TLB PMA `C=0` 的 cache-path AF | `pma_pmp_model` AF-only view + cache observer 的 `dcache_fact=YES` |
| LR/SC/AMO non-cache path AF | 原子消费者 contract（V2 `AtomicsUnit` 的 PMA/PBMT 路径事实） |
| L2 D-channel `denied/corrupt` | DCache sticky ledger |
| 主表 | 只提供 opcode、VA、ROB、源寄存器和立即数，不提供异常事实 |

必须删除/禁止以下读取路径：`main_view.tlb_af`、`main_view.tlb_pf`、`main_view.tlb_gpf`、
`main_view.pma_af`、`main_view.denied`、`main_view.corrupt`。

### 8.2 raw vector 和架构 cause 分离

模型结果先写入 RM item 的独立字段：

```text
expected_pmp_fault
expected_pma_fault
expected_keyid_fault
expected_pma_atomic_fault
expected_atomic_noncache_path_fault
expected_pma_cache_path_fault
expected_dcache_denied
expected_dcache_corrupt
expected_exception_vec_raw
expected_exception_cause_priority
```

`mmio/cacheable` 和原始 `atomic_allowed` 可以保留在模型内部日志或可选 debug dump 中，但不得
作为独立字段写入 RM expected item 或成为 pass/fail 条件；由 `atomic_allowed=0` 推导出的
`expected_pma_atomic_fault` 以及原子 non-cache path fault 必须保留为 AF 来源。raw vector 用于
调试和在 translation-success 场景下的精确比较；架构 cause 用于 PF/AF 同时存在时的优先级比较。
这样既不会丢掉 DUT 原始信息，也不会把属性分类或未定义 response 当成第二个架构 trap。

### 8.3 PMA/PMP 与 sticky ledger 的调用顺序

```text
TLB entry/context lookup
  -> PA 和 translation status
  -> PMA/PMP evaluate（只在 translation_success）
  -> 基础 AF reduce：R/W/X/keyid/current-atomic
  -> 基础 AF? 记录 PMA/PMP 来源，结束；不读取 dcache_fact
  -> [普通 scalar Load/Store && C=0] ?
       -> 读取 dcache_fact=YES/NO/UNKNOWN
       -> UNKNOWN? 等待 C 属性事实
       -> YES? 追加 cache-path AF，结束
       -> NO? 不产生 C 属性 AF，结束；不查 DCache ledger
  -> [LR/SC/AMO && 原子消费者 non-cache 路径非法] ?
       -> 追加 atomic_noncache_path_fault，结束；不读取 dcache_fact
  -> 无 AF 后读取/复用 dcache_fact，决定是否可查 ledger
  -> dcache_fact != YES? UNKNOWN 等待、NO 结束；均不查 DCache ledger
  -> 按 LDA/Store sample 查询 sticky denied/corrupt
  -> 生成 RM expected raw/priority result
```

两套状态表只通过值型 API 连接。PMA/PMP 模型不读取或修改 DCache ledger，DCache ledger 也
不读取 PMA/PMP entry 数组。

### 8.4 `observer_build_commit_item()` 的 AF-only 伪代码

**抽象功能：** 该 RM helper 先从 TLB 表确定翻译异常和 PA，再用 PMA/PMP AF-only view 增补
data-side AF；它只在已确认 DCache 访问且无更早 AF 时查询 sticky ledger。它不读取
`main_view` 异常字段，也不比较 MMIO/cacheable 分类。

```text
build expected item(uid):
  tlb = read frozen TLB context and canonical entries
  derive TLB PF/GPF/AF and PA geometry
  if translation fault or PA invalid:
      record only translation exception
      finish item; do not call PMA/PMP or DCache ledger

  pma_pmp_result = evaluate(frozen PA/size/cmd/privilege/CSR generation)
  base_af_view = make_base_af_view(pma_pmp_result)
  OR base_af_view.base_ld/st/instr_access_fault into expected raw exception vector
  save only base AF source bits to RM diagnostic item
  if base_af_view contains AF:
      finish item; do not read dcache_fact or query DCache ledger

  af_view = base_af_view
  dcache_fact_read = 0
  if access is LR/SC/AMO and atomic_noncache_path_is_illegal:
      af_view.atomic_noncache_path_fault = 1
      OR it through the LR/SC/AMO direction-specific AF mapper
      finish item; do not read dcache_fact or query DCache ledger

  if base_af_view.dcache_fact_needed_for_c:
      dcache_fact = read_dcache_access_fact(uid, dynamic_epoch, access_seq)
      dcache_fact_read = 1
      if dcache_fact is UNKNOWN:
          return FACT_NOT_READY; do not treat it as NO
      af_view = finalize_cache_path_af(pma_pmp_result, base_af_view, dcache_fact)
      OR af_view.pma_cache_path_fault through the command-specific AF mapper
      save cache-path AF source bit to RM diagnostic item
      if af_view contains AF:
          finish item; do not query DCache ledger

  if access is not a scalar DCache data candidate:
      finish item; do not query DCache ledger

  if !dcache_fact_read:
      dcache_fact = read_dcache_access_fact(uid, dynamic_epoch, access_seq)
  if dcache_fact is UNKNOWN:
      return FACT_NOT_READY; final PMA/PMP AF is known, but ledger eligibility is not
  if dcache_fact is not YES:
      finish item; do not query DCache ledger

  l2_error = query_l2_d_error_at_sample(PA lines, actual access sample)
  OR l2_error through the existing consumer-specific error mapper
  finish item
```

`FACT_NOT_READY` 是等待条件，不是 RM error，也不能通过重读当前 PMA/PMP CSR 表绕过。它只允许
出现在“基础 AF 为 0、普通 scalar Load/Store 的 `C=0` cache-path AF 尚未可判”或“最终无 AF、
但 DCache ledger 资格尚未可判”两种情况；基础 AF 或原子 non-cache AF 已确定时禁止进入该等待
分支。若访问已 terminal、超过关联窗口仍找不到唯一的 `dcache_fact`，才报告 observer contract
错误，并输出 UID、dynamic epoch、ROB、PA、PMA/PMP generation 和等待 sample 范围。

### 8.5 DCache 访问事实发布合同

**抽象功能：** `publish_dcache_access_fact()` 由独立 cache observer 为一次 scalar access 发布
`YES/NO`；它不判断 PMA/PMP 权限，不修改 ledger，也不把 L2 response 当成 cache-path 唯一来源。

实现前必须先在当前 V2 DUT 确认一个 DCache 内部接入点，满足以下条件：

1. `YES` 对 cache hit 和 cache miss 都会出现，且一次 access 至多发布一次。
2. 事件可关联到 `{uid,dynamic_epoch,access_seq}`，至少携带或可稳定反查 ROB/LQ/SQ 身份及 PA。
3. Load 和 Store 分别从真实 DCache 数据路径观察；不能用 commit、L2 `A.fire`、GrantData 或
   DCache response 是否到达来反推前序 access。
4. `NO` 只在已有可靠 non-DCache completion/token 已经关闭该动态 access 时发布；PF/AF、flush、
   replay 和 redirect 必须先以 UID/epoch 清除 pending token，不能误写 `NO` 给重发实例。
5. 无法获得上述事实时保持 `UNKNOWN`。不得为了让回归继续而把 `UNKNOWN` 降级为 `NO`。

第一阶段可以把该 observer 限定在 scalar Load/Store。若当前顶层交接 interface 没有足够身份字段，
应新增只读 monitor/XMR 采样而不是改动 RTL；若仍无法建立唯一映射，应把该项列为测试框架
observer 缺口，停止该分支实现并记录波形路径。

### 8.6 原子消费者 AF 事实合同

**抽象功能：** `publish_atomic_noncache_path_fact()` 为 LR/SC/AMO 发布“原子操作是否因
non-cache 路径而被消费者拒绝”的值型事实，使 RM 能复刻原子单元的 AF 归约，而不读取或比较
`mmio/cacheable` 分类。

当前 V2 `AtomicsUnit` 在 `s_pm` 阶段把 PMA `C=0` 导出的 non-cache 属性以及 PBMT `NC/IO`
合并到 `exception_pa_mmio_nc`；该条件对 LR 映射为 LAF，对 SC/AMO 映射为 SAF，并在发起 DCache
请求前结束。因而：

1. `atomic_allowed=0` 由 PMA 模型直接产生 `pma_atomic_fault`，不依赖该事实 token。
2. `atomic_allowed=1` 但原子消费者确认 non-cache 路径非法时，产生
   `atomic_noncache_path_fault`，同样不等待 `dcache_fact`。
3. 事实至少关联 `{uid,dynamic_epoch,access_seq}`，并携带 LR/SC/AMO command 和 sample；
   PF/AF、flush、replay、redirect 的旧 token 必须失效，不能污染重发实例。
4. 若本期只运行普通 scalar Load/Store，可不发布该 token；一旦 sequence 生成 LR/SC/AMO，
   缺失该事实必须报告 observer contract 缺口，而不是把 `dcache_fact=NO` 当作“无 AF”。

## 9. 建议的文件和接口落点

### 9.1 新增模型文件

建议文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_pma_pmp_model.sv
```

建议 API（名称可按现有命名规范微调）：

```systemverilog
extern function void reset_and_init_v2_profile();
extern function bit is_ready();
extern function void apply_csr_write(
    input bit [11:0] addr,
    input bit [63:0] data,
    input longint unsigned sample);
extern function bit capture_request_context(
    input pma_pmp_request_context_t request,
    output pma_pmp_request_context_t snapshot);
extern function bit evaluate(
    input pma_pmp_request_context_t snapshot,
    output pma_pmp_eval_t result);
extern function void make_base_af_view(
    input pma_pmp_eval_t result,
    output pma_pmp_af_view_t view);
extern function void finalize_cache_path_af(
    input pma_pmp_eval_t result,
    input pma_pmp_af_view_t base_view,
    input pma_pmp_dcache_fact_e dcache_fact,
    output pma_pmp_af_view_t final_view);
extern function bit read_entry(
    input bit is_pma,
    input int unsigned index,
    output pma_pmp_entry_t entry);
extern function bit check_invariants(output string message);
```

`evaluate()`、`make_base_af_view()` 和 `finalize_cache_path_af()` 都必须是纯查询，不修改表；
`apply_csr_write()` 是唯一的表修改入口。`make_base_af_view()` 不接收也不得读取 `dcache_fact`，
只归约基础 AF；`finalize_cache_path_af()` 只允许在 `base_view.dcache_fact_needed_for_c=1` 且事实
已知为 `YES/NO` 时调用，负责补充 `pma_cache_path_fault`。两个 API 都不得自行查询或向 RM
暴露 MMIO/cacheable 路径。

### 9.2 共享 owner 和只读 façade

- 模型对象由 dispatch/shared runtime state 持有，生命周期覆盖整个 testcase。
- CSR monitor 或现有 CSR service loop 在看到实际 `w.valid` 后调用 `apply_csr_write()`。
- `memblock_rm_readonly_api.sv` 增加值型 `pma_pmp_view_t`、`dcache_access_fact_view_t` 和只读
  读取函数，不向 RM 暴露
  associative array、class handle 或修改方法。该对外 view 应为 `pma_pmp_af_view_t`，不包含
  `mmio/cacheable/atomic_allowed`。
- `dcache_access_fact_view_t` 必须至少保存 `{valid, uid, dynamic_epoch, access_seq, fact, sample}`。
  它由 cache observer 唯一发布：Load 以可关联的 DCache 数据路径 monitor token 为 `YES`，Store
  以实际 store DCache 路径 token 为 `YES`；只有已被可靠 non-DCache token 关闭的访问可写 `NO`。
  不允许把 L2 `A.fire` 当作唯一 `YES`，也不允许把“尚未见 A.fire”直接写成 `NO`。
- `mmu_csr_runtime_state.sv` 只增加 `pma_generation/pmp_generation` 和 context token 等
  标量快照；不把 32 项表复制进 `dispatch_raw_csr_t`，避免每拍传输大对象。
- `memblock_rm_dut_writeback_observer.sv` 提供 Load/Store/AMO 的实际 sample，RM 通过 UID
  查找对应 context。

### 9.3 编译接入

新增文件后按 sequence 规则同步：

```text
mem_ut/ver/ut/memblock/seq/seq.f
mem_ut/ver/ut/memblock/seq/seq_pkg.sv
```

模型文件应在 `memblock_rm_readonly_api.sv`、`mem_base_sequence.sv` 和 RM consumer 之前
include。若新增 profile 文件，也要在同一 package/filelist 中显式管理，不能依赖 simulator
自动搜文件。

## 10. 参数、profile 和 cfg 方案

### 10.1 参数分类

PMA/PMP entry 表、generation、CSR 快照和匹配结果都是运行期状态，不放入 `seq_csr_common`
或 testcase cfg。建议只增加以下行为参数：

| 参数 | 默认值 | 作用 |
| --- | ---: | --- |
| `MEMBLOCK_PMA_PMP_MODEL_EN` | `0` | 是否启用 RM PMA/PMP 模型；关闭时保持旧回归兼容 |
| `MEMBLOCK_PMA_PMP_RAW_COMPARE_EN` | `1` | 翻译成功时是否对 AF-only raw fault 做精确比较，不比较路由属性 |
| `MEMBLOCK_PMA_PMP_XMR_CHECK_EN` | `0` | 是否在启动时用 XMR 做 profile 一致性检查 |
| `MEMBLOCK_PMA_PMP_UNDEFINED_AFTER_TLB_FAULT` | `1` | PF/AF 后把 PMP/PMA response 标为不可用，仅比较有效 cause |

参数链路应为 `env/plus.sv -> seq_csr_common -> getter -> sequence/RM`，并同步
`seq/plus_cfg/default.cfg`。专用 Sv39/U 态 cfg 显式打开模型；默认 cfg 不改变既有 testcase。

### 10.2 PMA profile 的单一权威

短期 V2 落地建议：

1. 新增一个版本化 `memblock_pma_pmp_profile_v2.svh`，只保存从 `SoC.scala` 读取并复核的
   PMA 源项；代码中集中执行 reverse/padding/掩码计算。
2. 启动时打印每项 canonical `cfg/addr/mask/priority`，可选 XMR 检查生成 RTL 的输出。
3. 长期将 profile 生成脚本接到 Scala elaboration/RTL 生成流程，由 Scala `PMAConfigs` 自动
   导出 SV/JSON，消除手工重复维护；生成物仍按 V2 profile 固定，不跨版本复用。

禁止在 testcase cfg 中手写“RAM 一定允许”“所有地址 cacheable”之类的近似规则；RM 也不得用
testcase cfg 推导 `dcache_fact`。

### 10.3 与当前 Sv39/U 场景的关系

当前 CSR sequence 把 privilege 切到 U 态且 PMP reset 为全零时，模型应预测 U/S 未匹配 PMP
的 Access Fault；这是硬件语义，不是模型默认值错误。若目标是 10000 笔正常 DCache 请求，
应另设一个显式的“PMP allow RAM”preset，并确认 cache observer 实际发布了 `dcache_fact=YES`；
PMA `C=0` 的 cache 请求将被 AF-only view 拦截。若保留全零 PMP，则回归目标应接受 AF，
不能同时要求所有请求进入正常 DCache 比较路径。

## 11. 验证计划

### 11.1 模型单元场景

| 场景 | 验收点 |
| --- | --- |
| PMP reset、U/S 未命中 | `ld/st/instr` 按 RTL default 拒绝 |
| M 态未锁定 bypass | 未锁定项不产生 PMP 权限 fault |
| M 态锁定项 | 锁定项仍执行权限检查 |
| OFF/TOR/NA4/NAPOT | A 编码、边界和 entry 0 lower bound 正确 |
| TOR 前后项锁定 | 前一项地址锁定规则与 RTL 一致 |
| `W=1,R=0` 写入 | 读回/内部状态归一化为 `W=0` |
| V2 grain | NA4 禁用/归一化行为正确 |
| 多项重叠 | entry 0 优先，不按范围大小重排 |
| PMA default | 未命中同时得到权限拒绝和 `mmio=1` |
| PMA C/atomic/X/R/W | `R/W/X` 与当前 LR/SC/AMO atomic 先转为基础 AF；仅基础 AF 为 0 时，`C` 才在实际 DCache 路径转为 cache-path AF |
| LR + `atomic_allowed=0` | `pma_atomic_fault=1`，只产生 LAF，且不等待 `dcache_fact` |
| SC/AMO + `atomic_allowed=0` | `pma_atomic_fault=1`，只产生 SAF，且不等待 `dcache_fact` |
| 普通 Load/Store + `atomic_allowed=0` | 不因 atomic 属性单独产生 AF |
| LR/SC/AMO + `atomic_allowed=1` 但 non-cache 路径非法 | `atomic_noncache_path_fault=1`，按原子方向产生 LAF/SAF |
| keyid/debug/CMODE | 条件与地址剥离规则正确 |
| 跨区域/未对齐访问 | 与 `aligned/boundMatch` 一致，不拆分绕过 |

### 11.2 CSR 和快照时序

| 场景 | 预期 |
| --- | --- |
| `pmpcfg` 单项写 | 只更新未锁定项，generation 增加一次 |
| `pmpaddr` 写入 | 当前/下一项 lock/TOR 规则正确 |
| PMA CSR 写入 | PMA 表变化，不影响 PMP generation |
| CSR 写后新请求 | 使用新 generation |
| CSR 写前已 outstanding 请求 | 仍使用旧 snapshot |
| CSR 与 request 同边沿 | 按已确认 DUT 时序固定 old/new 规则 |
| 首份 profile 未 ready | RM 等待，不默认无 fault |
| 基础 AF + `dcache_fact=UNKNOWN` | 立即预测基础 AF；禁止读取或等待 `dcache_fact`，不查 DCache ledger |
| 基础 AF 为 0，PMA `C=0` + `dcache_fact=YES` | `pma_cache_path_fault=1`，RM 预测对应 Access Fault，不查 DCache ledger |
| 基础 AF 为 0，PMA `C=0` + `dcache_fact=NO` | 不因 C 位单独产生 AF，不查 DCache ledger |
| 基础 AF 为 0，PMA `C=0` + `dcache_fact=UNKNOWN` | 仅 C 属性 AF 未决定，RM 等待，不猜测 cacheable/MMIO |
| 基础 AF 为 0，PMA `C=1` + `dcache_fact=UNKNOWN` | PMA/PMP AF 已确定为无额外 AF；RM 仅为决定 DCache ledger 资格等待事实 |

### 11.3 集成和回归

至少执行以下组合：

1. Sv39 + U 态 + PMP 全零，确认模型能解释 `0x2020/0x8080` 中的 raw AF 来源，并按
   translation-fault policy 不把不可信的后置 AF 当成第二个架构 trap。
2. Sv39 + U 态 + CSR 配置 PMP TOR/NAPOT 允许 RAM，确认有效翻译访问的 PMP AF 消失。
3. PMA 不可读/不可写/不可执行、LR/SC/AMO atomic 和 `C=0 + dcache_fact=YES` 场景，确认均按
   适用 command 预测 AF；基础 AF 在 `dcache_fact=UNKNOWN` 时仍立即完成；`C=0 + dcache_fact=NO`
   场景不产生额外 C-attribute AF。
4. PMA/PMP fault 与 DCache sticky `denied/corrupt` 组合，确认 fault/ledger 调用顺序正确，
   基础 PMA/PMP AF 不读取 DCache fact，cache-path AF、`dcache_fact=NO` 或 `dcache_fact=UNKNOWN`
   均不查询 DCache ledger。
5. 运行目标 10000 笔回归；若仍出现无法由模型和已有 contract 解释的 mismatch，保存最小
   seed、日志和 FSDB，并按用户规则将其归类为 RTL 问题后结束，不修改 RTL。

## 12. 实施顺序和完成条件

### 12.1 实施顺序

1. 取得明确的 coding 授权，确认工作区现有修改不被覆盖。
2. 建立 V2 PMA profile 和 PMP reset 表，先完成纯 SV 单元测试。
3. 实现 CSR decode、WARL、lock、generation 和 invariant 检查。
4. 接入 CSR monitor，确认动态写入与波形中的 distributed CSR bus 一致。
5. 接入 request context snapshot 和 PMA/PMP evaluate，先覆盖 Load/Store，再扩展 AMO/Instr。
6. 扩展 RM readonly API、writeback sample 关联和异常来源分层；删除 `main_view` 异常读取。
7. 接入 cache observer 的 `dcache_fact` token 和两阶段 AF-only reducer：基础 AF 直接结束；
   仅 `C=0` 且基础 AF 为 0 时用 token 归约 cache-path AF；只让最终无 AF 且
   `dcache_fact=YES` 的访问进入 DCache sticky ledger gate。
8. 增加默认/专用 cfg、编译检查、定向回归，再启动 10000 笔测试。

### 12.2 完成条件

- PMA/PMP 表初始化与 V2 RTL profile 一致，reverse/padding/priority 有可审计日志。
- CSR 写入的 WARL、lock 和 generation 通过定向场景。
- RM 不再从 `main_view` 读取任何异常字段。
- 翻译成功访问的基础 PMA/PMP AF、LR/SC/AMO atomic AF、PMA `C=0` cache-path AF 与 DCache
  ledger 调用边界正确；基础 AF 不等待 DCache fact，RM 不比较 `mmio/cacheable/atomic_allowed`。
- 翻译故障后的不可信 PMP/PMA response 只做诊断，不造成伪造的第二架构 trap。
- 10000 笔目标回归通过且 `UVM_ERROR/UVM_FATAL` 为 0，或已定位 RTL 问题并记录日志、
  出错点和波形路径。
- 全程不修改 Scala/Chisel RTL；如需 RTL 修复，只在独立问题文档中记录建议。

## 13. 风险和未决事项

1. **PMA profile 漂移**：Scala 配置变化会使手工 SV 表过期。短期用 XMR checksum 检查，
   长期接入自动生成。
2. **PMA 地址优先级误读**：`pma_init()` 的 reverse 很容易被遗漏；实现必须打印最终 entry
   index，而不是只打印源列表。
3. **PMA C 属性和 DCache 访问事实不一致**：`PADDR_BASE` 不代表 cacheable，且 RM 不得据此
   推导路径；启动 10000 笔前应确认 cache observer 能为每个基础 AF 为 0 的 DCache 候选发布唯一的
   `dcache_fact=YES/NO` token。基础 AF 不得等待 token；`C=0` 却有 `YES` token 时应预测 AF，
   而不是回退为 MMIO 分类比较。
4. **TLB fault 与 post-TLB fault 重叠**：必须用 `translation_eligible` 门控，不能把
   `tlb_entry.pma_af` 和 data-side PMA fault 重复 OR。
5. **Store/AMO sample 不完整**：若 observer 不能唯一定位实际 fault/WB 时刻，先扩展值型
   sample 事实，禁止用 commit 时间猜测。
6. **XMR 层次变化**：XMR 只能是可选校验，不能成为主模型依赖。
7. **PMP/PMA 与 vector/ITLB/PTW**：第一阶段覆盖 scalar Load/Store/AMO；其它端口需在有
   明确 request sample 和消费 contract 后扩展，不能复制 scalar 假设。

本文保持在 `undo`，因为尚未取得 coding 授权，也未实施或验证任何代码修改。
