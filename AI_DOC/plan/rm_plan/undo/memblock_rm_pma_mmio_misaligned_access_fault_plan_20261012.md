# memblock RM 非对齐 PMA/MMIO Access Fault 预测修复方案（2026-10-12）

| 项目 | 内容 |
| --- | --- |
| 状态 | 第一轮 review 未通过后已按 blocker 收窄；第二轮独立 review 已通过，已获得 coding 授权并完成实现与本轮验证。 |
| 方案类型 | V2 RM plan。 |
| 目标问题 | scalar Load/Store 非对齐访问满足 DUT PMA/MMIO 条件时，RM 漏预测 Access Fault。 |
| 首个复现 seed | `37617171`。 |
| 工作分支 | `codex/fault1-pma-mmio-20261012`。 |
| 工作目录 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault1_pma_mmio_20261012`。 |
| review/coding 门禁 | 第一轮 review 指出范围过宽；修订后第二轮独立 review 通过，父任务已明确授权 coding，当前实现已完成。 |

## 1. 术语和职责边界

| 术语 | 含义 | 本方案中的职责 |
| --- | --- | --- |
| PMA memory type | `PMPChecker` 选中 PMA entry 后由 `cfg.c` 得到的内存类型；`c=0` 时 `resp.mmio=1`。 | 只在 PMA/PMP model 已开启且 readonly view 有效时使用。 |
| PBMT-PMA | effective PBMT=`00`，内存类型由 PMA 决定。 | 允许读取 PMA model 的 `normal_cacheable` 窄事实。 |
| PBMT-NC | effective PBMT=`01`。 | 保持当前 RM 行为，不属于本方案新增逻辑。 |
| PBMT-IO | effective PBMT=`10`。 | RTL 语义保留为背景事实，但本方案不新增或修改该路径。 |
| `s2_pmp.st` | `PMPChecker` 对当前 command 产生的 store-fault response。 | 是 command-dependent 结果，不是无条件写权限事实。 |
| backing query | RM 为无异常 Load 读取 initialized backing memory，构造期望数据。 | 只在 RM 和 DUT 都进入正常数据比较路径时执行。 |

本方案只修复 RM 期望和二次误报保护，不修改 Scala/Chisel、生成 RTL、DUT interface、激励、PMA/PMP
表内容或 cfg。问题 2 的 MMIO raw owner fatal 不属于本文范围。

### 1.1 第一轮 review blocker 响应

第一版方案错误扩展到了逐 byte 首个 memory-type cause、混合 PBMT-NC/MMIO 和跨页/PMA 区域，
但对应的 MisalignBuffer 分段顺序和跨边界 AF 聚合又被列为待决风险，导致方案不可执行。本版按 review
要求做以下收窄：

1. 只处理整笔有效 byte 都能证明为 PBMT-PMA 且 PMA C=0/MMIO 的场景。
2. 不实现“任一后续 byte MMIO 即整笔 AF”。
3. 不实现“首 byte/首个 memory-type cause 决定异常”。
4. 混合 PBMT、混合 PMA C 属性、跨页和跨 PMA 区域全部排除为后续专项工作。
5. backing 路径改为 exception compare 在前，只有 expected、actual、status 都无异常时才读取数据。

## 2. 复现证据与首发根因

seed `37617171` 在 841.9 ns 的关键记录为：

```text
uid=0 kind=LOAD fu_op=0x5
VA=0x8d34a1cf
PA=0xd34a1cf..0xd34a1d0
size=2 hdMisalign=1
allPbmtPma=1 allNormalCacheable=0
RM expected_exception=0x0
DUT exception=0x20
```

`fu_op=0x5` 是 `LHU`，两字节访问从奇地址开始，因此地址非对齐。DUT bit 5 为
`loadAccessFault`。RM 当前在 `hd_misalign_ld_enable && all_bytes_pma_pbmt` 时抑制
`loadAddrMisaligned`，但没有继续判断该访问是否属于 DUT 的实际 MMIO 路径，最终保留
`expected_exception=0`。

随后 `observer_capture_load_backing()` 把该访问当作正常 Load，查询未初始化 PA
`0xd34a1cf`，产生 `initialized_backing query miss`。该 query miss 是 RM 漏预测异常后的二次误报，
不是独立的 memory 初始化问题。

## 3. 当前 RTL 的精确语义

### 3.1 LoadUnit

V2 `LoadUnit.scala` 的条件为：

```text
s2_actually_all_mmio =
    !s2_in.tlbMiss &&
    !s2_un_access_exception &&
    ((Pbmt.isPMA(s2_pbmt) && !s2_pmp.st && s2_pmp.mmio) ||
     s2_in.mmio)

misaligned && s2_actually_all_mmio
    -> loadAccessFault
```

其中 `s2_in.mmio` 是 PBMT-IO；`s2_pmp.mmio` 来自 PMA `C=0`。PBMT-NC 使用
`s2_actually_pbmt_nc` 产生 `loadAddrMisaligned`，与 MMIO Access Fault 是不同分支。

### 3.2 StoreUnit

V2 `StoreUnit.scala` 使用对称条件：

```text
s2_actually_all_mmio =
    s2_tlb_hit &&
    !s2_un_access_exception &&
    ((Pbmt.isPMA(s2_pbmt) && !s2_pmp.st && s2_pmp.mmio) ||
     s2_in.mmio)

misaligned && s2_actually_all_mmio
    -> storeAccessFault
```

PBMT-NC 则产生 `storeAddrMisaligned`。

### 3.3 `!s2_pmp.st` 不能解释成额外写权限要求

`PMP.scala::pmp_check()` 和 `PMA.scala::pma_check()` 都按当前 `TlbCmd` 生成 response：

```text
READ command:
    resp.ld 根据 R/atomic 产生
    resp.st 固定为 0

WRITE/AMO command:
    resp.st 根据 W/atomic 产生
```

因此：

1. Load 使用 READ command 时，`!s2_pmp.st` 天然成立，不要求额外证明 PMA/PMP 的 W 权限。
2. Store 使用 WRITE command 时，`s2_pmp.st` 才反映当前 Store 的 W/atomic fault。
3. 当前 RM view 中的 `pmp_st_fault`、`pma_st_fault`、`st_access_fault` 同样是 command-dependent；
   禁止把它们描述或实现成“无条件写权限”。
4. 最小实现不为 Load 额外发起 WRITE command 查询，也不新增独立 W-permission 字段。
5. 当前 V2 `PMPKeyIDBits=0`，不会由 keyid response 破坏上述 READ 下 `st=0` 的结论；未来启用
   KeyID 后需重新 review。

## 4. 当前 RM view 能证明什么

现有 `pma_pmp_af_view_t` 已提供最小所需字段：

| 字段 | 证明内容 |
| --- | --- |
| `valid` | 本次 PMA/PMP evaluation 有有效结果。 |
| `translation_eligible` | 物理地址可用于 PMA/PMP 判断。 |
| `af_decided` | 当前 view 已完成 AF 判定，不是未知状态。 |
| `normal_cacheable` | 当前 PMA 匹配结果的 `C` 值；有效 view 下为 0 表示 PMA MMIO。 |
| `ld_access_fault` / `st_access_fault` | 当前 command 对应方向的基础 AF。 |

不新增 raw `mmio` 字段。PMA model 开启、view 有效且 `normal_cacheable=0` 时，可以用其反值表达
PMA `resp.mmio=1`；但必须同时受 PBMT-PMA、有效性和 command-dependent AF 条件保护。模型关闭时
readonly API 返回的默认 `normal_cacheable=0` 只表示没有该事实，绝不能据此推断 MMIO。

## 5. 方案设计

### 5.1 整笔 PMA MMIO 证明

**抽象功能：** 复用当前完整尺寸 PMA/PMP query 和已有逐 byte readonly query，仅在整笔访问的所有
有效 byte 都能证明为 PBMT-PMA、PMA C=0/MMIO 且当前方向无基础 AF 时，形成
`all_bytes_pma_mmio=1`。该事实只用于本方案的非对齐 Access Fault overlay。

建议在 `observer_build_commit_item()` 增加一个局部聚合值 `all_bytes_pma_mmio`，默认值为 0。只有
`MEMBLOCK_PMA_PMP_MODEL_EN=1`、翻译成功、PA mask 完整且完整尺寸 view 有效时，才开始聚合：

```text
all_bytes_pma_mmio = all_bytes_pma_pbmt

若完整尺寸 view 无效、translation_eligible=0 或 af_decided=0：
    all_bytes_pma_mmio = 0

若完整尺寸 view 的当前方向 access_fault=1：
    all_bytes_pma_mmio = 0

对每一个有效 byte 的现有 PMA/PMP readonly view：
    若 view 无效、translation_eligible=0 或 af_decided=0：置 0
    若 normal_cacheable=1：置 0
    若当前方向 access_fault=1：置 0
    若 command-dependent st_access_fault=1：置 0
```

“当前方向 access fault”定义为：Load 使用 `ld_access_fault`，Store 使用 `st_access_fault`。额外检查
`st_access_fault` 是为了与 RTL `!s2_pmp.st` 形式保持一致；Load 的 query command 为 READ，因此其
`st_access_fault` 按当前 V2 源码固定为 0，不构成额外 W 权限要求。Store 的 WRITE command 下，
`st_access_fault` 与当前方向基础 AF 一致。

本方案不新增 PBMT 数组，不按 byte 排序异常原因，也不新增 WRITE command query。现有
`all_bytes_pma_pbmt` 已经证明整笔有效 byte 的 effective PBMT 都为 PMA。

### 5.2 最小 Access Fault overlay

**抽象功能：** 在既有 TLB/PMA/PMP 异常完成后，只对已经完全证明的整笔 PMA MMIO 非对齐访问补充
DUT 对应的 Access Fault bit。

精确 gate：

```text
PMA/PMP model 已开启
&& misaligned == 1
&& item.expected_exception == 0
&& all_bytes_pma_pbmt == 1
&& all_bytes_pma_mmio == 1
&& 完整尺寸 view 当前方向无基础 AF
```

动作：

```text
Load  -> expected_exception[5] = 1
Store -> expected_exception[7] = 1
```

该 overlay 放在现有 `observer_should_expect_addr_misaligned()` 之前。命中后不再置 bit 4/6。若 gate
不完整成立，则不进入本方案逻辑，继续执行当前 RM 行为。

由此保证：

- 已有 page/guest-page/access fault 时不追加新 bit。
- 对齐的 PMA C=0 访问不因本方案产生 AF。
- 任一 byte 不是 PBMT-PMA、不是 C=0、事实无效或存在方向 AF 时，不使用本 overlay。
- PBMT-NC、PBMT-IO 和其他既有路径保持当前行为，本方案不修改。

### 5.3 PMA model 关闭时的行为

`MEMBLOCK_PMA_PMP_MODEL_EN=0` 时，`all_bytes_pma_mmio` 保持 0。不得根据 readonly API 默认返回的
`normal_cacheable=0` 推断 MMIO；PBMT-PMA 非对齐访问完全保持当前 fallback 行为。

### 5.4 明确排除的跨边界场景

以下场景不属于本方案，既不新增预测，也不声明已覆盖：

1. 同一 scalar 访问包含混合 PBMT-PMA、PBMT-NC 或 PBMT-IO。
2. 同一访问跨页后，两侧 PMA entry 或 `C` 属性不同。
3. 只有部分 byte 为 PMA C=0/MMIO。
4. 根据 MisalignBuffer 后续子访问推导“任一后续 byte MMIO 即整笔 AF”。
5. 根据 byte 地址顺序选择首个 memory-type cause。
6. 跨 PMA/PMP 边界的后续方向 AF 聚合。

这些场景需要先独立核对 Load/Store MisalignBuffer 的真实分段、请求顺序和异常合并规则，再建立后续
专项方案，不能在本次 coding 中顺带实现。

## 6. backing query 二次误报保护

### 6.1 目标

当 RM 仍错误预测无异常、但 DUT LDA/status 已明确携带异常时，应优先报告 exception mismatch，不能先因
backing 未初始化而报告 `RM_LS_ERR_ORIGINAL_L2_NOT_READY`。保护逻辑不得把 DUT exception 写回
`item.expected_exception`。

### 6.2 最小控制流调整

调整 Load compare 顺序为：

```text
1. 捕获 RM expected_exception、LDA actual_exception 和 terminal/status exception。
2. 使用同一个 compare_arch_exception 语义先比较三者 exception。
3. 若 exception 不匹配：直接报告 RM_LS_ERR_EXCEPTION_MISMATCH，不读取 backing。
4. 若 exception 匹配且任一方存在异常：完成异常路径比较，不读取 backing。
5. 仅当 expected、actual、status 三者都无异常时：
   读取 initialized backing，构造 expected data，并比较 data/data_valid。
```

实现时把 exception compare 提取为可复用的单一 helper，供 `observer_process_current_commit()` 在
`observer_capture_load_backing()` 之前调用，并由 `rm_ls_core::compare_load()` 复用，避免两套异常比较
语义分叉。`observer_capture_commit_actual()` 只额外返回已经捕获的 LDA exception vector，不改变它。

该调整只改变错误报告优先级：

- RM expectation 始终来自 TLB/PMA/PMP/reference-model 事实。
- DUT exception 只用于判断是否还有必要构造正常数据期望。
- DUT 与 RM 不一致时仍返回 `RM_LS_ERR_EXCEPTION_MISMATCH`，不吞掉错误。

## 7. 实际修改文件

| 顺序 | 文件 | 实际修改 |
| --- | --- | --- |
| 1 | `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv` | 聚合整笔 `all_bytes_pma_mmio`；在既有 misaligned gate 前补 bit 5/7；在 backing query 前调用统一 exception precheck。 |
| 2 | `mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv` | 提取统一 Load exception compare helper；仅在 expected/actual/status 都无异常后构造 backing expected data。 |

本次未修改 `memblock_pma_pmp_model.sv` 和 `memblock_rm_readonly_api.sv` 的数据结构；现有字段足以完成
保守证明，也没有新增 raw `mmio`、无条件权限字段或 DUT route observer。

## 8. 明确不做

1. 不把所有 `normal_cacheable=0` 或所有 PMA `C=0` 无条件改成 Access Fault。
2. 不为 Load 发起额外 WRITE PMA/PMP query，不把 command-dependent `st_fault` 当静态 W 权限。
3. 不在 PMA model 关闭时用默认零值推断 MMIO。
4. 不改变 PBMT-NC 的 Address Misaligned 行为。
5. 不用 DUT exception、status exception、`debug.isMMIO` 或 `debug.isNCIO` 反推 RM expectation。
6. 不修改 RTL，也不在本文处理 `MMIO_RESOLVE` 的同 epoch 无主 raw。
7. 不因 backing query miss 而自动初始化 memory line。
8. 不处理混合 PBMT、跨页、跨 PMA entry、部分 byte C=0 或 MisalignBuffer 后续子访问归因。
9. 不实现“任一后续 byte MMIO 即整笔 AF”或“首 byte cause 决定异常”。

## 9. 验证计划

### 9.1 静态检查

```bash
git diff --check
rg -n "actually_all_mmio|normal_cacheable|st_access_fault|observer_capture_load_backing|compare_load" \
  src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala \
  src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala \
  mem_ut/ver/ut/memblock/env/src/memblock_rm \
  mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_pma_pmp_model.sv
```

人工确认 Load 的 READ path 没有新增 WRITE 权限要求，PMA model 关闭路径没有使用
`normal_cacheable=0` 推断 MMIO。

### 9.2 首个失败 seed

复跑：

```text
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_l2tlb_fault_2pct_no_trigger
seed=37617171
MEMBLOCK_PMA_PMP_MODEL_EN=1
```

在问题 2 的独立 `MMIO_RESOLVE` fatal 发生前，必须观察到：

- uid 0 `expected_exception=0x20`。
- DUT LDA/status 仍为 `0x20`，exception compare PASS。
- uid 0 不再查询 `0xd34a1cf` backing，不再出现对应 query miss。
- 不能把后续 `MMIO_RESOLVE` fatal 误算成问题 1 验证失败。

### 9.3 directed 语义矩阵

| 场景 | Load 期望 | Store 期望 |
| --- | --- | --- |
| 非对齐、PBMT-PMA、PMA C=0、当前方向权限允许 | bit 5 | bit 7 |
| 对齐、PBMT-PMA、PMA C=0、当前方向权限允许 | 不由本方案新增异常 | 不由本方案新增异常 |
| 非对齐、已有 PF/GPF/AF | 保持既有异常，不叠加 | 保持既有异常，不叠加 |
| 非对齐、PBMT-PMA、model=0 | 保持既有 fallback，不推断 PMA MMIO | 保持既有 fallback，不推断 PMA MMIO |
| 非对齐、PBMT-PMA、任一 byte C=1/未知 | 不由本方案新增异常 | 不由本方案新增异常 |
| PBMT-NC、PBMT-IO、混合 PBMT 或跨 PMA 边界 | 不属于本方案；确认行为未被修改 | 不属于本方案；确认行为未被修改 |

### 9.4 正常 seed 回归

至少复跑历史 PASS seed `1461454841`，但必须使用当前 PMA model=1 的 cfg 重新运行，不能直接引用旧
批次的 PMA model=0 结论。再选择至少一个包含同一 PBMT-PMA/PMA 区域内 Store 的 PASS seed，确认：

- aligned cacheable Load/Store expectation 不变。
- 正常 backing data compare 仍执行。
- `UVM_ERROR=0`、`UVM_FATAL=0`，或仅出现已单独登记且与本问题无关的框架错误。

## 10. 验收标准

1. seed `37617171` 的 uid 0 从 RM `expected_exception=0` 修正为 `0x20`。
2. 同一 uid 不再产生 backing query miss，首发结果为 exception compare PASS。
3. 非对齐 Store 的对称实现已完成，但本轮没有执行专用 directed Store 测试，列为残余验证项。
4. model off、aligned C=0、任一 byte 非 C=0 和已有 PF/GPF/AF 时不触发本 overlay。
5. PBMT-NC、PBMT-IO、混合 PBMT、跨页和跨 PMA 边界行为没有被本方案修改。
6. exception mismatch 优先于 backing query miss；只有 expected/actual/status 都无异常时读取 backing。
7. Load READ path 未新增 W 权限要求；review 能从 command-dependent view 证明这一点。
8. RTL 和原工作区保持不修改。

## 11. Review 结论与残余风险

1. **两轮 review：** 第一轮因方案扩展到混合 PBMT/PMA、跨边界和首 byte cause 而未通过；收窄为
   “整笔有效 byte 均可证明为 PBMT-PMA 且 PMA C=0”后，第二轮独立 review 通过。coding 后父级 review
   确认 `all_bytes_pma_mmio` 的 model/query gate、逐 byte C=0 合取、方向 AF gate 和只读 exception
   precheck 均无 blocker。
2. **Store directed 未覆盖：** 非对齐 PMA/MMIO Store 的 bit 7 对称路径已实现并通过编译，但本轮没有
   执行专用 directed Store 测试，不能声明动态验证通过，必须作为后续残余验证项。
3. **正常 Load data 覆盖：** seed `1461454841` 的 5554 个 Load 均携带 `0x2000` 异常，因此该 10K
   回归能证明异常路径和 UID 连续消费未退化，但不能单独证明三方零异常时的 backing/data compare 覆盖。
4. **exception mismatch directed 未覆盖：** mismatch 仍由 `compare_load()`/`commit()` 写 error/history
   并消费 UID，已由代码路径和 review 核对；本轮未通过 fault injection 制造 exception mismatch 做动态验证。
5. **范围外场景：** 混合 PBMT、混合 PMA C 属性、跨页/跨 PMA 边界、部分 byte MMIO 和首 byte cause
   仍明确排除，需在理解 MisalignBuffer 分段及异常聚合后另立专项。
6. **未来 KeyID：** 当前结论依赖 V2 `PMPKeyIDBits=0`；若目标配置变化，READ 下合并后的
   `st_access_fault` 需要重新核验。

## 12. 实现与验证结果

### 12.1 VCS 编译

复用 `fault1_pma_mmio_20261012` 仿真模式完成 RTL/VCS 编译。编译日志：

```text
mem_ut/ver/ut/memblock/sim/fault1_pma_mmio_20261012/log/vcs_compile_rtl.log
```

结果为 `0 error(s), 0 warning(s)`，simv 和 Verdi KDB 均成功生成。

### 12.2 专项 seed `37617171`

运行参数：

```text
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_l2tlb_fault_2pct_no_trigger
seed=37617171
MEMBLOCK_PMA_PMP_MODEL_EN=1
UVM_VERBOSITY=UVM_HIGH
```

日志：

```text
mem_ut/ver/ut/memblock/sim/fault1_pma_mmio_20261012/log/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl.log
```

问题 1 验证 PASS：uid 0 显示 `allPmaMmio=1`、RM `expected_exception=0x20`，LDA/status 均为
`0x20`，`RM_LS_TRACE_COMPARE result=PASS`；未再出现 PA `0xd34a1cf` 的 initialized-backing query
miss。随后在 868.4 ns 命中已独立登记的问题 2 `MMIO_RESOLVE` fatal，最终统计为
`UVM_ERROR=0`、`UVM_FATAL=1`，该 fatal 不属于问题 1。

### 12.3 正常 seed `1461454841`

运行使用同一 `tc/ts/cfg`、`MEMBLOCK_PMA_PMP_MODEL_EN=1`，seed 改为 `1461454841`。日志：

```text
mem_ut/ver/ut/memblock/sim/fault1_pma_mmio_20261012/log/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=1461454841_rtl_normal_seed.log
```

10,000 笔 RM compare 全部完成，日志包含 `TEST_PASS`、`RM_LS_COMPARE=10000`、
`UVM_ERROR=0`、`UVM_FATAL=0`，并在 2455737.100 ns 正常 `$finish`。该 seed 的 Load 均走异常路径，
因此不把它表述为零异常 backing/data compare 的动态覆盖。

### 12.4 静态检查与未覆盖项

- `git diff --check`：PASS。
- 专用非对齐 PMA/MMIO Store directed：未执行，bit 7 动态行为仍待验证。
- exception mismatch fault-injection：未执行，UID 推进结论来自实现路径和 code review。
- 混合 PBMT/PMA、跨页/跨 PMA 边界：不在本次实现和验证范围内。
