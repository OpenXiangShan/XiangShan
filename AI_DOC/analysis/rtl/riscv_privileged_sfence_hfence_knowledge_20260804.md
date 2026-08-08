# RISC-V 特权架构中的 SFENCE.VMA、HFENCE.VVMA 与 HFENCE.GVMA

## 1. 文档定位、术语与权威来源

本文是 RISC-V 特权架构知识文档，说明 `SFENCE.VMA`、`HFENCE.VVMA` 和 `HFENCE.GVMA` 的架构语义、操作数范围、
失效对象、权限限制以及两阶段地址翻译下的使用关系。文中“失效”指地址翻译缓存或等价的内存管理缓存条目
不再被后续隐式地址翻译使用；不等同于删除普通数据缓存、主存数据或测试框架中的历史 transaction。

本文以 RISC-V Ratified Specifications Library `v20260120` 为依据，对应模块为 Supervisor ISA 1.13 和
Hypervisor Extension 1.0：

- [Supervisor Memory-Management Fence Instruction — `SFENCE.VMA`](https://docs.riscv.org/reference/isa/v20260120/priv/supervisor.html#sfence.vma)
- [Hypervisor Memory-Management Fence Instructions — `HFENCE.VVMA/HFENCE.GVMA`](https://docs.riscv.org/reference/isa/v20260120/priv/hypervisor.html#hfence.vma)
- [Machine `mstatus.TVM` virtualization control](https://docs.riscv.org/reference/isa/v20260120/priv/machine.html#virt-control)

| 术语 | 含义 | 本文中的关键点 |
|---|---|---|
| `implicit read` | 硬件地址翻译算法对页表或翻译元数据的隐式读取 | 普通 load/store 的显式内存访问之外，必须由 fence 建立顺序 |
| `translation cache` | TLB 或其它缓存翻译结果的结构 | fence 可使匹配条目失效，不只针对名为 TLB 的结构 |
| HS/S-stage | 当前 supervisor 地址翻译阶段 | `satp` 控制；`V=0` 时为当前 hart 的 S/HS 翻译 |
| VS-stage | 虚拟 supervisor 地址翻译阶段 | `vsatp` 控制，GVA 翻译为 GPA |
| G-stage | guest-physical 地址翻译阶段 | `hgatp` 控制，GPA 翻译为 supervisor physical address |
| ASID | 地址空间标识符 | `SFENCE.VMA` 和 `HFENCE.VVMA` 的 `rs2` 语义 |
| VMID | 虚拟机标识符 | `HFENCE.GVMA` 的 `rs2` 语义；`HFENCE.VVMA` 隐式使用执行时的 `hgatp.VMID` |
| global mapping | PTE `G=1` 的全局映射 | `rs2` 指定非零 ASID 时，普通 S/VS fence 不要求失效 global 条目 |
| subsuming fence | 覆盖某条旧翻译的 fence | 没有覆盖旧翻译的 fence，硬件仍可使用 fence 之前有效的旧翻译 |
| over-fence | 失效范围大于最小架构要求 | 架构允许；测试模型不能少失效，但可以保守地多失效 |
| `rs1=x0` / `rs2=x0` | 操作数寄存器编号为零 | 表示“所有地址/所有 ID”，不是“匹配数值为 0 的地址/ID” |

## 2. 两阶段地址翻译模型

在没有虚拟化时，当前 hart 通常执行单阶段翻译：

```text
VA -- satp --> supervisor physical address
```

在虚拟化模式 `V=1` 下，guest 的地址翻译由两个阶段组成：

```text
GVA -- vsatp --> GPA -- hgatp --> supervisor physical address
```

因此三个 fence 的目标不是同一张“总 TLB”：

| 指令 | 目标翻译阶段 | 控制 CSR | 直观含义 |
|---|---|---|---|
| `SFENCE.VMA`，`V=0` | HS/S-stage | 当前 `satp` | 刷新当前 supervisor 地址空间 |
| `SFENCE.VMA`，`V=1` | VS-stage | `vsatp` | guest 自己执行的 VS 地址空间 fence |
| `HFENCE.VVMA` | VS-stage | `vsatp` | HS/M 代表 hypervisor 为指定 VM 执行 VS fence |
| `HFENCE.GVMA` | G-stage | `hgatp` | 刷新 GPA 到 supervisor physical address 的翻译 |

`SFENCE.VMA` 在 `V=0` 时不会替代 `HFENCE.VVMA`；`HFENCE.VVMA` 也不会替代 `HFENCE.GVMA`。当两阶段的页表都
发生变化时，必须分别对相应阶段建立 fence。

## 3. 三条指令的共同架构语义

### 3.1 顺序保证与翻译缓存失效

执行 fence 后，当前 hart 在 fence 之前已经可见的显式 store，会先于 fence 之后相关指令触发的、对内存管理
数据结构的隐式读取。指令同时用于使匹配的地址翻译缓存条目失效。

这里有两个容易混淆的边界：

1. fence 不是普通数据 `FENCE` 的替代品，也不把所有显式 load/store 放入全局内存顺序。
2. fence 只约束当前 hart。其它 hart 的翻译缓存不会自动失效；多 hart 系统需要单独的数据可见性、IPI/软件
   shootdown、远端 hart 执行 fence 和完成确认。

### 3.2 没有足够 fence 时的允许行为

如果修改页表后没有执行覆盖该翻译的 fence，硬件可以使用自最近一次覆盖性 fence 以来任一时刻有效的旧翻译。
硬件可以选择旧翻译或新翻译，且连续访问不保证每次选择相同结果；这不是“必定立即看到新 PTE”，也不是
“必定一直使用旧 PTE”。

### 3.3 over-fence 与无效地址

实现可以忽略部分地址/ID 位，甚至把所有 fence 都实现成全局 fence，只要不比架构要求失效得更少。
`SFENCE.VMA` 的 `rs1` 若为非零但不是当前实现支持的有效虚拟地址，指令无效果且不产生异常；这条“无效
地址不报错”规则只对 `SFENCE.VMA` 明确规定，不应无条件推广到所有自定义 interface。

### 3.4 规范允许缓存无效 PTE

地址翻译缓存可以缓存 `V=0` 的 PTE。软件不能假设“无效 PTE 永远不在 TLB”；修改 PTE 有效位或重新建立映射
时，仍须按相应阶段和地址范围执行 fence（除非实现了适用的其它标准扩展语义）。

## 4. `SFENCE.VMA` 语义

### 4.1 `rs1/rs2` 四种范围

`rs1` 是虚拟地址选择，`rs2` 是 ASID 选择。非零 `rs1` 指向包含该地址的 leaf PTE/翻译范围；它不是要求
按字节地址精确相等。superpage 或 NAPOT 映射必须按其覆盖范围判断。

| `rs1` | `rs2` | 顺序范围 | 翻译缓存失效范围 |
|---|---|---|---|
| `x0` | `x0` | 所有地址空间的所有页表层级 | 所有地址空间的所有翻译条目 |
| `x0` | 非 `x0` | 指定 ASID 的所有页表层级 | 指定 ASID 的条目；不要求失效 global 条目 |
| 非 `x0` | `x0` | 所有 ASID 中对应 VA 的 leaf PTE | 所有 ASID 中覆盖该 VA 的 leaf 翻译，含 global |
| 非 `x0` | 非 `x0` | 指定 ASID 中对应 VA 的 leaf PTE | 指定 ASID 中覆盖该 VA 的 leaf 翻译，不含 global |

当软件只把一个 ASID 的根页表换成新地址时，常用 `rs1=x0`、`rs2=该 ASID`；如果希望只选择数值为 0 的
ASID，必须把 0 放入一个非零编号的寄存器，不能把 `rs2` 编码为 `x0`。

### 4.2 global mapping 规则

页表遍历路径中只要存在 global 语义，相关映射可以在所有地址空间复用。因而 `rs2` 为非零 ASID 时，软件
不能依靠该 fence 清除 global 映射；需要使用 `rs2=x0` 或更宽范围的 fence。

当 `rs2` 为非零寄存器时，软件应把 `XLEN-1:ASIDMAX` 的保留位清零；实现必须忽略超出自己 `ASIDLEN` 的
ASID 位。该规则同样适用于 `HFENCE.VVMA` 的 VS-ASID。

### 4.3 常见页表更新场景

- 修改 leaf PTE：`rs1` 使用该页/映射内的虚拟地址；`rs2` 使用目标 ASID，若路径含 global 映射则使用 `x0`。
- 修改 non-leaf PTE：通常使用 `rs1=x0`，因为一个上层 PTE 可能影响大量下级翻译。
- 回收并复用 ASID：先把新页表与 ASID 建立，再执行 `SFENCE.VMA rs1=x0, rs2=该 ASID`。
- 只把 `satp.MODE` 在 Bare 与分页模式之间切换，架构规定切换立即生效，不要求额外 `SFENCE.VMA`；但页表根
  或 ASID 复用仍需按上面场景同步。

### 4.4 执行权限与异常

| 执行上下文 | 条件 |
|---|---|
| M-mode | 通常可执行；若实现把 `satp.MODE` 固定为 Bare，`SFENCE.VMA` 可能为非法指令 |
| S/HS-mode | `mstatus.TVM=0` 可执行；`TVM=1` 产生非法指令异常 |
| VS-mode | `hstatus.VTVM=0` 可执行；`VTVM=1` 产生虚拟指令异常 |
| U/VU-mode | U-mode 为非法指令；VU-mode 为虚拟指令异常 |

`mstatus.TVM` 只拦截 S/HS 对 `satp`、`SFENCE.VMA` 等操作；VS-mode 的相应拦截由 `hstatus.VTVM` 控制。

## 5. `HFENCE.VVMA` 语义

### 5.1 作用对象

`HFENCE.VVMA` 等价于 hypervisor 暂时进入 VS-mode 后执行一次 `SFENCE.VMA`，但由 M/HS-mode 直接执行。
它只作用于 VS-stage 的内存管理结构（`vsatp`），并且只保证对执行时 `hgatp.VMID` 对应的一个虚拟机生效。

当 `hgatp.VMID` 在 fence 后发生改变时，后续指令在新 VMID 下的 VS-stage 隐式读取不由这条旧 fence 保证顺序。

### 5.2 操作数

- `rs1 != x0`：一个 guest virtual address，按所在页/超级页覆盖范围选择。
- `rs1 == x0`：所有 guest virtual address。
- `rs2 != x0`：一个 VS-level ASID。
- `rs2 == x0`：所有 VS-level ASID。
- 作用域始终再叠加执行瞬间的 `hgatp.VMID`；不能用 `HFENCE.VVMA` 一次精确清理其它 VMID 的 VS 条目。

其 `rs1/rs2` 的四种组合可按 `SFENCE.VMA` 的地址/ASID矩阵理解；实现可以保守地扩大到全部 VM 或全部翻译
结构，但不能少于当前 VMID 下的架构要求。

### 5.3 权限与异常

- 仅 M-mode 和 HS-mode 合法。
- `mstatus.TVM`、`hstatus.VTVM` 不会使 HS/M 执行的 `HFENCE.VVMA` 产生陷阱。
- V=1 时执行 HFENCE 指令会产生虚拟指令异常；U-mode 执行会产生非法指令异常。

## 6. `HFENCE.GVMA` 语义

### 6.1 作用对象

`HFENCE.GVMA` 作用于 `hgatp` 控制的 G-stage 内存管理结构，即 GPA 到 supervisor physical address 的翻译。
它不是 VS-stage fence，也不要求清理仅保存 GVA→GPA 的前级缓存。

如果实现把 GVA 直接缓存为 supervisor physical address，则必须清除所有其 GVA 所映射 GPA 与 fence 地址/VMID
匹配的合并条目；实现也可以为降低硬件复杂度而清除指定 VMID 的全部 G-stage 条目。

### 6.2 操作数与 `GPA >> 2` 编码

- `rs1 != x0`：一个 guest physical address，编码为该 GPA 右移 2 位。
- `rs1 == x0`：所有 GPA。
- `rs2 != x0`：一个 VMID。
- `rs2 == x0`：所有 VMID。

当 `rs2` 指定 VMID 时，软件应清零 `XLEN-1:VMIDMAX` 的保留位；实现必须忽略超过自己 `VMIDLEN` 的 VMID 位。

`rs1` 的右移 2 位不是普通页号右移 12 位。该编码用于容纳比当前 XLEN 更宽的 GPA，并与 PTE/PMP 中的物理
地址编码保持一致。测试框架不能把 HFENCE.GVMA 的 `rs1` 直接当成普通 VA VPN 使用。

### 6.3 `hgatp.MODE` 变化

如果某个 VMID 的 `hgatp.MODE` 发生变化，即使旧 mode 或新 mode 是 Bare，也必须执行：

```text
HFENCE.GVMA rs1=x0, rs2=x0 或 rs2=该 VMID
```

仅写 `hgatp` 不会自动建立页表更新与后续 G-stage 隐式读取之间的顺序。

### 6.4 权限与异常

- M-mode 始终可执行。
- HS-mode 只有 `mstatus.TVM=0` 时可执行；`TVM=1` 为非法指令异常。
- V=1 时执行为虚拟指令异常；U-mode 执行为非法指令异常。

## 7. 三条指令的对比速查

| 项目 | `SFENCE.VMA` | `HFENCE.VVMA` | `HFENCE.GVMA` |
|---|---|---|---|
| 主要阶段 | `V=0` 时 HS；`V=1` 时 VS | VS | G |
| 控制 CSR | `satp` 或 `vsatp`（由 V 决定） | `vsatp` | `hgatp` |
| `rs1` | VA | GVA | GPA >> 2 |
| `rs2` | ASID | VS-ASID | VMID |
| 隐含 VM 作用域 | `V=1` 时当前 `hgatp.VMID` | 执行时当前 `hgatp.VMID` | 由 `rs2` 选择 VMID |
| global mapping | ASID 非零时不要求清 global | 继承 VS `SFENCE.VMA` 的 global 语义 | 无 ASID/global 语义，按 GPA/VMID 匹配 |
| 典型执行者 | S/HS 或 M；VS guest 可执行 | HS 或 M | HS 或 M |
| 能否替代其它阶段 fence | 不能 | 不能替代 GVMA | 不能替代 VVMA |

最重要的判断是“哪一级页表被修改”：

```text
HS/satp 页表改变      -> V=0 下的 SFENCE.VMA
VS/vsatp 页表改变     -> VS 中的 SFENCE.VMA，或 HS/M 的 HFENCE.VVMA
G-stage/hgatp 页表改变 -> HFENCE.GVMA
两级页表都改变        -> 分别执行 VVMA 和 GVMA
```

## 8. 典型同步流程

### 8.1 修改单阶段页表

```text
显式 store 修改 PTE
  -> 确保 store 对当前 hart 可见
  -> SFENCE.VMA 按 VA/ASID 范围执行
  -> 后续访问使用不早于该 fence 语义允许的翻译
```

### 8.2 修改 VS-stage 页表

```text
修改 GVA -> GPA 的 VS PTE
  -> guest 在 VS-mode 执行 SFENCE.VMA，或 hypervisor 执行 HFENCE.VVMA
  -> rs1 选择 GVA，rs2 选择 VS-ASID
  -> 作用域限定为执行时的 hgatp.VMID
```

### 8.3 修改 G-stage 页表

```text
修改 GPA -> SPA 的 G-stage PTE
  -> HFENCE.GVMA
  -> rs1 选择 GPA>>2，rs2 选择 VMID
```

### 8.4 两阶段同时修改

VS-stage 和 G-stage 是独立的内存管理数据结构。修改 GVA→GPA 与 GPA→SPA 两级映射时，不能只执行一条
fence；应对每个被修改的阶段分别执行覆盖性 `HFENCE.VVMA`/`SFENCE.VMA` 和 `HFENCE.GVMA`。

### 8.5 VM/CSR 上下文切换

架构没有提供原子地同时切换 `vsatp` 和 `hgatp` 的机制。为避免投机翻译把旧 guest 的 VS 翻译缓存到新 VMID，
推荐顺序是：

```text
vsatp = 0
切换 hgatp
写入新的 vsatp
```

如果修改的是某个 VMID 的 `hgatp.MODE`，必须补 `HFENCE.GVMA rs1=x0`。如果只是 `satp.MODE` 在 Bare 与分页
模式间切换，架构规定立即生效，但测试软件仍需根据根页表/ASID复用场景安排相应 fence。

### 8.6 跨 hart shootdown

三条指令都只作用于当前 hart。典型远端失效流程为：

```text
本 hart 更新页表
  -> 数据 fence，使更新对其它 hart 可见
  -> IPI 通知目标 hart
  -> 目标 hart 执行覆盖性 SFENCE/HFENCE
  -> 目标 hart 回 ACK
```

### 8.7 PBMT、A/D 与 PMP 相关上下文

根据特权手册的 hypervisor 章节：

| 变化对象 | 推荐同步动作 |
|---|---|
| `menvcfg.PBMTE` 或 `menvcfg.ADUE` | `HFENCE.GVMA rs1=x0, rs2=x0` |
| `henvcfg.PBMTE` 或 `henvcfg.ADUE` | 当前 VMID 的 `HFENCE.VVMA rs1=x0, rs2=x0` |
| 影响 HS 页表或最终物理地址的 PMP 设置 | M-mode `SFENCE.VMA x0,x0`；G/VS 相关缓存用 `HFENCE.GVMA x0,x0` 同步，手册指出不需要额外 `HFENCE.VVMA` |

这些是架构同步要求，不应与测试框架中的随机 PBMT/A/D payload 生成混为一谈。

## 9. 对当前 mem_ut / L2TLB 模型的落地约束

本节不是 RISC-V 新语义，而是把架构语义映射到仓库现有的 `sfence_flow.md` 和双阶段 TLB 数据模型。

### 9.1 stage-aware 匹配字段

| 事件 | 架构目标 | 应使用的匹配字段 |
|---|---|---|
| 普通 `SFENCE.VMA`，`V=0` | HS/S-stage | `s1_tag`、`s1_level`、`s1_asid`、`s1_pte_g` |
| guest `SFENCE.VMA` 或 `HFENCE.VVMA` | VS-stage | `s1_tag`、`s1_level`、VS-ASID、执行时 VMID |
| `HFENCE.GVMA` | G-stage | `s2_tag`、`s2_level`、`s2_vmid`，以及 GPA 语义的地址字段 |

因此，`allStage` entry 不能用一个共享 `lookup_key.vpn/level` 同时匹配 VVMA 和 GVMA：

```text
VS fence  -> 匹配 GVA 覆盖范围和 VS-ASID/VMID
G fence   -> 匹配 GPA 覆盖范围和 VMID
```

`hv`、`hg` 等 raw event 标志是当前测试框架的解码字段，不是 RISC-V 指令编码本身；adapter 必须先把它们
解码成上述架构事件，再选择 stage 字段。

### 9.2 地址和 level 匹配

非零 `rs1` 不是对 `tag` 做简单等值比较。匹配 helper 必须考虑：

1. level 对应的 page/superpage 覆盖范围；
2. NAPOT 或 sector 派生的有效范围；
3. `SFENCE.VMA/HFENCE.VVMA` 的 GVA 与 `HFENCE.GVMA` 的 GPA 不可混用；
4. `rs1=x0` 的全地址语义。

### 9.3 ASID、VMID 与 global

- `SFENCE.VMA/HFENCE.VVMA` 的非零 `rs2` 是 ASID，不是 VMID。
- `HFENCE.GVMA` 的非零 `rs2` 是 VMID，不是 ASID。
- VVMA 必须使用事件执行时的 `hgatp.VMID`；延迟消费事件时不能只读取后来的当前 CSR。
- S/VS stage 的 `pte_g=1` 条目在指定非零 ASID 时不应被错误删除；若实现选择 over-fence，必须明确记录为保守失效。

### 9.4 live entry 与历史记录的边界

架构 fence 的直接对象是地址翻译缓存/内存管理缓存。测试框架可以删除 `tlb_entry_by_key` 中命中的 live entry，
使后续相同 request 重新生成或重新查表；不应因为 fence 自动删除：

- `main_table_by_uid` 的原始 transaction；
- `uid_tlb_record_by_uid` 的历史/调试记录；
- 与当前 fence 无关的 pending response token。

pending response 是否取消、如何记账属于 responder lifecycle 合同，不能由“TLB entry 被失效”这一架构事实自动推导。

### 9.5 与现有 flow 的关系

仓库已有 [sfence/hfence flow](../../mem_ut_flow_doc/sfence_flow.md)，
它描述 monitor、raw FIFO、CSR latest snapshot 和 live entry 删除的测试框架实现。本文作为其上层架构依据：

```text
RISC-V 手册的 stage/operand/ordering 语义
  -> raw fence event 解码
  -> stage-aware live entry 匹配
  -> 只删除架构作用域内的 live translation entry
```

如果实现为了简单执行 over-fence，可以满足架构要求，但不得把当前 `s1_*` 字段误用于所有 HFENCE.GVMA，
也不得用当前 CSR 替换 fence 采样时的 VMID/ASID 上下文。

## 10. 常见误区检查表

| 误区 | 正确理解 |
|---|---|
| 把 `rs2=x0` 当成 ASID/VMID 数值 0 | `x0` 是“所有 ID”；数值 0 应放在非零寄存器中 |
| `SFENCE.VMA` 等于清空整个 TLB | 它按 VA/ASID 范围建立顺序并使匹配翻译缓存失效；实现可以 over-fence，但架构语义不是普通全局数据 flush |
| `HFENCE.VVMA` 可以替代 `HFENCE.GVMA` | VVMA 只管 VS-stage，GVMA 才管 G-stage |
| `HFENCE.GVMA rs1` 直接传普通 VPN | `rs1` 编码的是 GPA>>2，且地址匹配属于 G-stage |
| 只比较 VPN/tag，不考虑 level | superpage/NAPOT 条目覆盖范围必须参与匹配 |
| fence 自动同步所有 hart | 三条指令都是 current-hart local，需要 IPI/shootdown |
| 写 `satp/hgatp` 就自动完成所有失效和顺序 | `satp.MODE/ASID` 有立即生效规则；`hgatp.MODE` 变化明确要求 `HFENCE.GVMA`，页表内容更新仍需相应 fence |
| fence 后删除 UID/main-table 历史记录 | 架构只约束翻译缓存；历史记录和 responder token 是测试框架状态 |
| 迟到 fence 用当前 CSR 重新解释 | 应使用 fence 执行/采样时的 ASID、VMID、stage 上下文 |

## 11. 快速结论

```text
SFENCE.VMA  = 当前 satp/vsatp 控制的 S/VS-stage fence
HFENCE.VVMA  = hypervisor 代表指定 VM 执行 VS-stage fence
HFENCE.GVMA  = G-stage GPA -> SPA 翻译 fence

VA/GVA 走 rs1；ASID 走 rs2；GPA>>2 走 HFENCE.GVMA.rs1；VMID 走 HFENCE.GVMA.rs2。
三者都只作用当前 hart；两阶段修改必须分别 fence；实现可以 over-fence，但不能 under-fence。
```
