# V2 L2TLB Response 随机 Payload 扩展 Plan

## 目标与边界

本 plan 扩展 V2 `L2TLB_agent` 的 response payload 随机能力：首次 lookup miss 创建某个 key 的 entry 时
随机生成 S1/S2 fault、level、PPN、PTE permission、PTE.N 与 PBMT；再按 `s2xlate` 修正为可驱动且符合
两阶段翻译语义的 payload。同一 key 后续命中时复用既有 entry，不重新随机。

本 plan 不修改既有 request fire 记账、pending queue、response latency、顺序/乱序调度、driver hold、
reset/flush 或 lifecycle owner 逻辑。首次建 entry 的随机结果写入 live entry；每次 accepted request 仅把
该 entry 复制为 pending snapshot，后续 pending/driver 重试不得重新随机。

## 一、公共 plus 权重参数

所有新参数走 `env/plus.sv -> seq_csr_common.sv -> getter -> L2TLB sequence`；默认 preset 保留现有
稳定 response 行为，testcase 通过 `seq/plus_cfg/*.cfg` 选择特定随机 profile。

### 1. Fault 为 1 的权重

为以下四个字段分别提供“随机为 1”的 plus 权重：

| 字段 | 参数名 |
|---|---|
| `s1_pf` | `MEMBLOCK_L2TLB_S1_PF_1_WT` |
| `s1_af` | `MEMBLOCK_L2TLB_S1_AF_1_WT` |
| `s2_gpf` | `MEMBLOCK_L2TLB_S2_GPF_1_WT` |
| `s2_gaf` | `MEMBLOCK_L2TLB_S2_GAF_1_WT` |

每个 lookup miss 的新 entry 均先按四组权重独立取得四个原始随机结果，再进入 `s2xlate` 修正；不因某个
stage 当前不生效而跳过其随机取值。lookup hit 直接复用该 entry 已保存的最终字段。

### 2. Level、PTE.N、permission 与 PBMT 权重

| 功能 | 参数命名规则 | 数量 |
|---|---|---:|
| S1/S2 level | `MEMBLOCK_L2TLB_S{1,2}_LEVEL_{0,1,2,3}_WT` | 8 组 |
| S1/S2 PTE.N 为 1 | `MEMBLOCK_L2TLB_S{1,2}_PTE_N_1_WT` | 2 组 |
| S1/S2 `R/W/X/U/G/A/D/V` 为 1 | `MEMBLOCK_L2TLB_S{1,2}_PTE_{R,W,X,U,G,A,D,V}_1_WT` | 16 组 |
| S1/S2 PBMT 三种编码 | `MEMBLOCK_L2TLB_S{1,2}_PBMT_{0,1,2}_WT` | 6 组 |

布尔字段的 `_1_WT` 合法范围为 `0..100`，其随机为 0 的权重固定为 `100 - _1_WT`；因此 `_1_WT=0`
表示永不随机为 1，`_1_WT=100` 表示必定随机为 1。level/PBMT 等多值字段的有效候选权重不能全为 0。
参数非法时在 sequence 配置阶段报 `uvm_fatal`，不允许静默回退成固定值。

### 3. 旧共享 PTE 参数迁移为 S1，并新增 S2 对称参数

现有 `MEMBLOCK_TLB_PTE_<FIELD>_1_WT` 参数不再表示共享 PTE；迁移并重命名为
`MEMBLOCK_L2TLB_S1_PTE_<FIELD>_1_WT`，其中 `<FIELD>` 覆盖 `R/W/X/U/G/A/D/N/V`。新增完全对称的
`MEMBLOCK_L2TLB_S2_PTE_<FIELD>_1_WT`，只控制 S2 对应字段的原始随机。

旧 `MEMBLOCK_TLB_PTE_<FIELD>_0_WT` 不迁移，也不保留兼容扫描；新规则统一由
`100 - MEMBLOCK_L2TLB_S{1,2}_PTE_<FIELD>_1_WT` 得到随机为 0 的权重。所有旧名称的定义、加载、
`seq_csr_common` 字段/getter、preset 和 consumer 必须在同一变更中删除或改为新名称，避免存在两套参数
权威。

原 `MEMBLOCK_TLB_PTE_MODE` 删除并拆分为 `MEMBLOCK_L2TLB_S1_PTE_MODE` 与
`MEMBLOCK_L2TLB_S2_PTE_MODE`，不保留旧名称兼容。每一套 mode 独立取
`LEGAL/MIXED/EXCEPTION_BIASED`，只影响本 stage 的 permission 合法化，不得把 S1 mode 或修正结果
传播到 S2。需要两阶段使用同一合法化策略时，由 testcase preset 将两个新参数设成相同值。

旧共享 level 控制 `MEMBLOCK_TLB_LEVEL_MODE`、`MEMBLOCK_TLB_LEVEL_FIXED_VALUE`、
`MEMBLOCK_TLB_LEVEL_RANDOM_LOW/HIGH` 不迁移为共享兼容参数；统一由本 plan 的
`MEMBLOCK_L2TLB_S1_LEVEL_{0,1,2,3}_WT` 与 `MEMBLOCK_L2TLB_S2_LEVEL_{0,1,2,3}_WT` 替代。旧 PBMT
控制参数/consumer 同样分别迁移为 S1/S2 三组 PBMT 权重及其对应 getter，不保留旧共享入口。

默认值固定如下，以保持基础 response 为 4 KiB、normal PBMT 的稳定行为：

| 参数组 | S1 默认值 | S2 默认值 |
|---|---|---|
| PTE mode | `MEMBLOCK_L2TLB_S1_PTE_MODE=0`（`LEGAL`） | `MEMBLOCK_L2TLB_S2_PTE_MODE=0`（`LEGAL`） |
| level 权重 | `LEVEL_0_WT=100`，`LEVEL_1/2/3_WT=0` | `LEVEL_0_WT=100`，`LEVEL_1/2/3_WT=0` |
| PBMT 权重 | `PBMT_0_WT=100`，`PBMT_1/2_WT=0` | `PBMT_0_WT=100`，`PBMT_1/2_WT=0` |
| fault 为 1 权重 | `S1_PF_1_WT=0`，`S1_AF_1_WT=0` | `S2_GPF_1_WT=0`，`S2_GAF_1_WT=0` |
| PTE.N 为 1 权重 | `S1_PTE_N_1_WT=0` | `S2_PTE_N_1_WT=0` |

S1/S2 的 `R/W/X/U/G/A/D/V` 默认 `_1_WT` 保持旧共享 PTE 默认概率，并折算到本 plan 的 0–100
权重规则：`R=89`、`W=86`、`X=80`、`U=11`、`G=11`、`A=89`、`D=89`、`V=90`。对应随机为 0 的权重
继续由 `100 - _1_WT` 派生；S1、S2 默认值相同但后续可由各自 plus 独立覆盖。

## 二、Fault 随机与 `s2xlate` 优先级修正

### 功能

每个新建 entry 先独立随机 `s1_pf/s1_af/s2_gpf/s2_gaf` 四个原始结果，再按当前 request 的 `s2xlate`
过滤非生效字段。entry 的最终 payload 始终只允许一个 fault 字段为 1；同 key hit 直接复用该结果。

本 plan 采用以下固定默认优先级，仅作为“多个原始随机字段同时为 1 且权重相同”时的确定性 tie-break：

| 默认优先级 | 字段 | 对应 plus 权重 |
|---:|---|---|
| 4 | `s2_gaf` | `MEMBLOCK_L2TLB_S2_GAF_1_WT` |
| 3 | `s1_af` | `MEMBLOCK_L2TLB_S1_AF_1_WT` |
| 2 | `s1_pf` | `MEMBLOCK_L2TLB_S1_PF_1_WT` |
| 1 | `s2_gpf` | `MEMBLOCK_L2TLB_S2_GPF_1_WT` |

`s2xlate` 对应的有效候选字段如下：

| `s2xlate` | 有效候选字段 | 非生效字段处理 |
|---|---|---|
| `noS2xlate` / `onlyStage1` | `s1_pf`、`s1_af` | `s2_gpf/s2_gaf` 强制为 0 |
| `onlyStage2` | `s2_gpf`、`s2_gaf` | `s1_pf/s1_af` 强制为 0 |
| `allStage` | 四个字段 | 统一参与唯一 fault 选择 |

唯一 fault 选择规则：

1. 对每个 lookup miss 的新 entry，四个字段都先按各自 `_1_WT` 完成原始随机。
2. 按 `s2xlate` 去掉非生效字段；若剩余候选没有任何字段为 1，则最终四字段均为 0。
3. 若只剩一个为 1 的候选，直接选中该字段。
4. 若有多个为 1 的候选，先比较这些候选对应的 plus 权重，权重最大的字段胜出。
5. 若最高权重仍有多个候选，按上表默认优先级选择唯一字段。
6. 将所有未选中的字段清零，保证最终 response 至多一个 fault 为 1。

这是 testbench 的统一随机收敛规则，不试图用一个扁平四字段表复刻所有 PTW/LLPTW/HPTW 内部 producer
分支；debug 必须同时记录原始结果、有效候选、各字段权重、最终选中字段和 `s2xlate`。

## 三、独立 S1/S2 Level 与 PPN 构造

### 功能

S1 与 S2 各自按 level 权重随机 `0/1/2/3`，不再从同一 entry level/PPN 复制。随后根据 `s2xlate`
和 PTE.N 状态修正 level，并构造与该 level 对应的 PPN 编码。

- 普通 page：每个 stage 的 level 决定该 stage PPN 中由输入 VPN/GVPN 补齐的低位语义。
- `s1_pte_n=1` 或 `s2_pte_n=1`：将对应 stage level 修正为 `0`，并把对应 PPN 修正为合法 NAPOT
  编码。
- `allStage`：保留独立 S1/S2 level，按 S1 PPN 生成 GPA，再以 GPA page number 作为 S2 的输入语义；
  不把两个 level 或两个 PPN 强行复制为同一个值。
- `noS2xlate/onlyStage1`：以 S1 PPN 为有效翻译结果；`onlyStage2/allStage`：以 S2 PPN 为有效翻译结果。

无 fault 的成功翻译 response 必须满足 DCache 可访问物理地址窗口约束：`onlyStage2/allStage` 约束
`s2_ppn`，其余模式约束 `s1_ppn`。选中 fault 后不建立 DCache PPN owner，也不要求该 PPN 落入
DCache 窗口；其中 `s2_gaf` 是最高默认优先级字段，选中后优先按 access-fault response 处理。所有
fault（包括 `s2_gaf`）只决定 fault 位和 DCache owner，不清零或覆写任一 S1/S2 的随机 PPN、permission、
PTE.N 或 PBMT payload；由 DUT 的 exception 路径处理这些伴随字段。

`allStage` 除约束最终 `s2_ppn` 外，还必须把经过 S1 level/NAPOT 补齐的完整 `s1_ppn` 限制在可编码为
GPA/GVPN、且可作为 S2 lookup 输入的地址范围。该约束不要求 S1 PPN 落入 DCache 窗口，但防止 S2
使用截断、越界或与请求 VPN 无法组合的 GPA page number。

allStage 的 S1 GPA/GVPN 范围不新增 plus 参数，直接由 request-time CSR snapshot 的 S1 虚拟地址模式
确定：Sv39 使用 `39 - 12 = 27` 位 VPN/GVPN，Sv48 使用 `48 - 12 = 36` 位 VPN/GVPN。S1 PPN/level
合成得到的 GPA page number 超出该模式有效 VPN 位宽时必须在建 entry 阶段重新约束；合规的结果同时作为
S2 lookup 输入和 `s2_tag` 的派生来源。

### S1 sector PPN 派生字段

S1 response 是 sector payload，不只传输 `s1_entry_ppn`。plan 要求先生成一份完整、经过 level/NAPOT
修正的 S1 translation PPN，再由同一份结果一致性派生 `s1_entry_ppn`、`s1_ppn_low[]`、`s1_valididx[]`
和 `s1_pteidx[]`。其中 `pteidx` 对应当前请求的 sector 位置，`valididx` 与 page size/sector 有效范围
一致；不得分别随机这些字段，避免 L1 TLB 在同一 response 内看到互相矛盾的 S1 PPN/level/sector 信息。

payload 建立顺序固定为：原始字段随机 → `s2xlate` 有效字段过滤 → 唯一 fault 选择 → S1/S2 level 与
PTE.N 修正 → S1 PPN 合成、allStage 的 S1 GPA/GVPN 输入约束和 S2 PPN 合成 → 无 fault 时的最终 DCache
PPN 约束 → PBMT effective 值派生 → S1 sector 字段派生 → snapshot。选中任何 fault 后保留已经生成的
PPN/PTE/PBMT 字段，仅跳过 DCache owner 建立。

## 四、独立 PTE Permission 随机

### 功能

S1/S2 各自独立随机 `R/W/X/U/G/A/D/V` 八个 permission 位，每一位都使用对应的“为 1”权重参数。
生成器不得把 S1 permission 镜像到 S2，或把任意 S2 permission 常量化。默认允许 `W=1,R=0` 等
非规范组合，不做自动合法化。`MEMBLOCK_L2TLB_S1_PTE_MODE` 与
`MEMBLOCK_L2TLB_S2_PTE_MODE` 分别控制本 stage 的 PTE profile：`0=LEGAL`、`1=MIXED`、
`2=EXCEPTION_BIASED`。当某一 stage 为 `LEGAL` 时，只对该 stage 调用可复用的
`fixup_pte_legal()` 规则，规避 `W=1,R=0`、无 `R/W/X` 的有效 PTE 及不一致 A/D 组合；另一个 stage
继续保留自己的 mode 和随机结果。

当前 L2TLB request 只有 `vpn/s2xlate`，不携带 load/store access kind。因此本阶段 S1、S2 均固定以
`MEMBLOCK_TLB_ACCESS_UNKNOWN` 调用各自的 PTE profile/fixup helper，不新增 access-kind sideband。
在该策略下，`EXCEPTION_BIASED` 不承诺产生 load/store 定向的 A/D exception bias；当前 UNKNOWN 规则对
有效 PTE 的最终 A/D 处理保持既有 UNKNOWN 语义。后续若接口能够提供可靠的 request-time access kind，
再单独扩展该 mode 的 load/store 定向行为。

随机 permission 与 fault、level、PTE.N 修正后的 response 一起写入 response snapshot；`fill_dtlb_resp_from_entry()`
负责把 S1 字段写入 S1 payload、把 S2 字段写入 S2 payload，driver 只驱动已经冻结的结果。

## 五、独立 PBMT 随机与阶段修正

### 功能

`s1_entry_pbmt` 与 `s2_entry_pbmt` 分别使用三组 PBMT 权重随机赋值，共六组参数。生成后按
`s2xlate` 修正有效 PBMT 来源：

| `s2xlate` | 有效 PBMT 语义 |
|---|---|
| `noS2xlate` / `onlyStage1` | 使用 S1 PBMT |
| `onlyStage2` | 使用 S2 PBMT |
| `allStage` | S1 PBMT 非零时使用 S1；S1 为零时使用 S2 |

S1/S2 PBMT 始终使用各自字段和各自三组 plus 权重随机，不能为了获得 effective PBMT 而覆写、复制或
合并 raw `s1_entry_pbmt/s2_entry_pbmt`。上述 `s2xlate` 规则只派生 `effective_pbmt`，用于 debug 和
后续预期检查；interface 继续驱动两个独立 raw PBMT 字段。

## 六、数据模型、响应建立与可观测性

### 功能

旧 `memblock_tlb_entry.pte_*` 不保留、不兼容：统一重命名为 `s1_pte_*`，并新增完全独立的
`s2_pte_*`。所有旧 consumer 同步改读 `s1_pte_*`；不得留下含义模糊的共享 `pte_*` 字段。旧共享
`paddr/ppn/level/pte_n/pbmt` 及 S1 sector 相关 `addr_low/ppn_low/valididx/pteidx` 也不保留：分别迁移为
`s1_*` 字段，并新增对应 `s2_paddr/s2_ppn/s2_level/s2_pte_n/s2_pbmt` 字段。所有旧 consumer 必须按原 S1
语义改读 `s1_*`，不得继续读取共享字段。pending response snapshot 必须保存两套完整 S1/S2 fault、PAddr、
level、PPN、PTE.N、permission 与 PBMT。

S1/S2 tag 与上下文字段必须作为明确的 payload 数据字段保存：

```text
s1_tag / s1_asid / s1_vmid
s2_tag / s2_vmid
```

这些字段在 lookup miss 创建 live entry 时从 request-time CSR snapshot 派生，并由 `copy_from()` 逐字段
冻结到 pending snapshot；UID record 同样保存两套 tag/context 用于 debug 和后续两阶段分析。response driver
不得在发包时重新读取当前 CSR。

S1 tag 从 request VPN/S1 sector 语义派生；S2 tag 从 only-S2 的 request VPN，或 allStage 的 S1
GPA/GVPN 输入语义派生。S1 ASID/VMID 与 S2 VMID 分别使用 request-time 对应 CSR context。tag、context、
PAddr 和 PPN 都必须与各自 stage level/NAPOT 修正后的 payload 一致，禁止再由同一个 `lookup_key.vpn`
直接扇出到两边。

L2TLB responder 在 lookup miss 创建 live entry 时完成“随机 -> `s2xlate` 修正 -> PPN/DCACHE 窗口约束”
流程。lookup hit 不改写该 entry；每次 accepted request 都从命中的 live entry 构造独立 pending snapshot，
pending queue 和 driver 只传递 snapshot。

### Lookup key 与 CSR 翻译上下文失效

同一 live entry 的复用 key 固定为 request-time 的 `vpn + asid + vmid + s2xlate`。该 key 不包含
`satp/vsatp/hgatp` 的 mode 或 root PPN，故不得把“key 相同”单独视为 CSR 翻译上下文仍相同。

凡 runtime CSR 变化改变任一翻译根或阶段配置，包括 `satp/vsatp/hgatp` 的 mode、root PPN、或触发对应
地址空间切换的上下文变化，CSR testcase 必须按既有框架规则构造对应的 SFENCE/flush。既有 L2TLB
lifecycle 负责 entry 失效、pending cancel 和 flush hold；本 plan 不新增 CSR 变化检测、entry 删除、
flush sideband 或额外 debug 逻辑。

ASID、VMID 或 `s2xlate` 改变本身会形成不同 key；mode/root PPN 改变即使 key 位相同，也依赖既有
SFENCE/flush 规则切断复用。本 plan 只把此事实作为随机 entry 复用的 testcase 前提。

独立 permission 的数据链必须显式实现：

```text
S1/S2 独立 plus/getter
  -> S1/S2 独立随机与 mode/fixup
  -> lookup miss 创建的 live memblock_tlb_entry.s1_pte_* / s2_pte_*
  -> lookup hit 复用 live entry，不重新随机
  -> 每次 request 的 copy_from()/clone/debug 逐字段冻结到 pending snapshot
  -> fill_dtlb_resp_from_entry()
  -> s1_entry_perm_* / s2_entry_perm_* 各自 interface 字段
```

`fill_dtlb_resp_from_entry()` 必须只以 `s1_pte_*` 驱动 S1 permission、只以 `s2_pte_*` 驱动 S2
permission，并分别使用 `s1_tag/s1_asid/s1_vmid` 与 `s2_tag/s2_vmid` 驱动对应 response context 字段。
`fixup_pte_legal()` 必须改为可对指定 stage PTE 字段组和指定 stage mode 运行的 helper，不能先修正 S1
再复制给 S2。

### UID record 双阶段字段迁移

`memblock_uid_tlb_record` 同步删除旧共享 `ppn/pte_*/level/pbmt/pte_n` 记录字段，迁移为独立的
`s1_ppn/s1_pte_*/s1_level/s1_pbmt/s1_pte_n` 与
`s2_ppn/s2_pte_*/s2_level/s2_pbmt/s2_pte_n` 字段；必要的 S1 sector 派生字段也按 S1 语义保存。
UID record 同时保存 `s1_paddr/s2_paddr`、`s1_tag/s1_asid/s1_vmid/s2_tag/s2_vmid`，及四个 fault：
`s1_pf/s1_af/s2_gpf/s2_gaf`。
`copy_entry_fields()` 必须逐字段复制两套 S1/S2 payload，不能从其中一套覆盖另一套。

后续需要 UID 对应的最终物理地址或最终 translation 属性时，统一按该 UID record 的 `s2xlate` 选择：

| `s2xlate` | 最终 translation 字段来源 |
|---|---|
| `noS2xlate` / `onlyStage1` | `uid_record.s1_*` |
| `onlyStage2` / `allStage` | `uid_record.s2_*` |

该选择只用于四个 fault 均为 0 时的最终 PA/属性 consumer，并派生 `final_paddr/final_paddr_valid`；只要
任一 fault 为 1，`final_paddr_valid=0`，不得把 S1/S2 PAddr 作为正常翻译结果消费。UID record 的 debug、
检查与两阶段行为分析始终保留并输出 S1/S2 两套原始字段，不折叠成共享记录。

debug/基础统计至少记录：lookup miss/hit、`s2xlate`、四个 fault 的原始值和修正值、S1/S2 level、PTE.N、
有效 DCache PPN owner、S1/S2 permission、S1/S2 PBMT 以及各权重 profile。该记录只验证激励生成是否符合
本 plan，不承担 DUT checker 或 scoreboard 功能。

## 七、验收范围

- 四种 `s2xlate` 均能生成 response；非生效字段为 0，最终四字段至多一个为 1。
- 多个原始随机候选同时为 1 时，先按对应 plus 权重选择；最高权重相同时按默认优先级选择，且 debug
  可追溯原始候选、权重与最终结果。
- 每个 lookup miss 的新 entry 都完成四个 fault 的原始随机采样；同 key lookup hit 不重新随机，且每次
  request 的 snapshot 都稳定保留命中 entry 的 payload。
- S1/S2 level 可分别命中 `0/1/2/3`；PTE.N 命中时对应 level 为 0、PPN 为合法 NAPOT 编码。
- 有效 DCache PPN owner 随 `s2xlate` 正确切换为 S1 或 S2，且落在配置的物理地址窗口内。
- S1/S2 八个 permission 位和三种 PBMT 均可分别由对应 plus 权重驱动，S1/S2 不发生意外镜像。
- 同一 lookup key 在未被 reset/flush/显式失效前重复请求时，response payload 保持相同；key miss 或
  entry 被删除后重建时才允许重新随机。
- CSR 翻译根或 mode 变化场景由 testcase 按既有规则同时构造 SFENCE/flush；本 plan 不实现对应
  invalidation 逻辑。
- 既有 L2TLB request token、latency、reorder、flush/reset、driver hold 与 idle-stop 行为保持不变。

## 后续实现落点

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` 及新增 testcase preset cfg
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`

本 plan 只定义测试框架 response 激励生成；不实现 Scala/RTL 正确性 checker、scoreboard 或功能覆盖率。
