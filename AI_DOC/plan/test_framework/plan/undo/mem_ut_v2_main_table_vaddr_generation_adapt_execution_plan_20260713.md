# mem_ut V2 主表虚拟地址窗口解耦最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 测试框架入口 | `memblock_dispatch_base_sequence::apply_legal_addr_template()` |
| 参数归属 | 公共测试框架参数，路径为 `plus.sv -> seq_csr_common.sv -> getter` |
| 适配原则 | normal 主表自动地址生成只使用 VADDR 窗口，TLB 物理映射继续只使用 PADDR 窗口 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只处理 V2 自动主表 normal transaction 的虚拟地址生成问题。目标是让
`apply_legal_addr_template()` 不再把 `MEMBLOCK_PADDR_BASE/RANGE` 当作虚拟地址窗口使用。

本轮修改范围：

| 文件 | 修改内容 |
|---|---|
| `mem_ut/ver/ut/memblock/env/plus.sv` | 新增 `MEMBLOCK_MAIN_VADDR_BASE/RANGE` 输入解析与默认值 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 保存参数快照、合法性检查、getter |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | `apply_legal_addr_template()` 改读主表 VADDR getter |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` | 新增同名默认项 |
| `AI_DOC/project_management/mem_ut_parameter_management.md` | 同步说明 VADDR/PADDR 参数职责边界，如当前已存在则核对不重复改写 |

本轮不修改：

- `tlb_map_builder::choose_paddr()` 和 `MEMBLOCK_PADDR_BASE/RANGE` 的物理 PPN 映射职责。
- 地址复用、boundary profile、manual/software directed builder。
- non-canonical fault、Sv48 高地址、negative canonical、MMIO 地址 directed 生成。
- LSQ admission、issue、writeback、redirect/replay、commit/deq、pass/fail、terminal。
- interface、agent、driver、monitor、RTL、Scala 和 flow 文档。

## 2. 问题一：normal 主表虚拟地址错误复用 PADDR 参数

### V2 问题

修改前 `apply_legal_addr_template()` 从 `get_paddr_base()/get_paddr_range()` 读取窗口，选择地址后写入
`tr.src_0`，再设置 `tr.imm=0` 并调用 `tr.update_vaddr()`。由于
`main_control_transaction::update_vaddr()` 的权威公式是：

```text
vaddr = src_0 + sign_extend_imm12(imm)
```

normal 主表自动生成的 issue 输入虚拟地址实际上被限制在 PADDR 窗口内。

### 修改原因

issue 输入表达的是虚拟地址；TLB builder 才拥有 VA 到 PA 的映射职责。继续共用 PADDR 参数会把虚拟
地址激励空间和物理映射窗口绑死，translated testcase 也无法配置不同 VA/PA 窗口来验证翻译链路。

### 修改方案与修改逻辑

新增公共测试框架参数：

```text
MEMBLOCK_MAIN_VADDR_BASE  = 64'h0000_0000_8000_0000
MEMBLOCK_MAIN_VADDR_RANGE = 64'h0000_0000_1000_0000
```

参数链：

```text
env/plus.sv
  -> seq_csr_common::load_from_plus()
  -> seq_csr_common::validate_and_clamp()
  -> seq_csr_common::get_main_vaddr_base()
  -> seq_csr_common::get_main_vaddr_range()
  -> apply_legal_addr_template()
```

`MEMBLOCK_PADDR_BASE/RANGE` 继续只由 TLB/物理映射相关逻辑读取，例如
`tlb_map_builder::choose_paddr(vaddr)`。两组参数默认值可以相同以保持 Bare smoke 兼容，但语义和
consumer 必须独立。

### 文字伪代码

```text
env/plus.sv：
  定义 MEMBLOCK_MAIN_VADDR_BASE 默认 0x80000000；
  定义 MEMBLOCK_MAIN_VADDR_RANGE 默认 0x10000000；
  使用与其它 hex64 公共参数一致的 plus 解析方式；

seq_csr_common::load_from_plus()：
  读取 plus::MEMBLOCK_MAIN_VADDR_BASE；
  读取 plus::MEMBLOCK_MAIN_VADDR_RANGE；
  保存到 main_vaddr_base/main_vaddr_range 快照；
  不读取或修改 MEMBLOCK_PADDR_BASE/RANGE；

seq_csr_common getter：
  get_main_vaddr_base() 只返回 main_vaddr_base；
  get_main_vaddr_range() 只返回 main_vaddr_range；
  公共 sequence 不长期直接读 plus::MEMBLOCK_MAIN_VADDR_*；

tlb_map_builder::choose_paddr(vaddr)：
  保持读取 MEMBLOCK_PADDR_BASE/RANGE 的既有逻辑；
  继续根据 VPN modulo 物理页数量选择 PPN；
  不读取 MAIN_VADDR 参数。
```

## 3. 问题二：VADDR 参数缺少 fail-fast 合法性检查

### V2 问题

如果新增 VADDR 参数只提供默认值，但不检查 range、溢出和 canonical 边界，非法配置可能在主表生成
后才表现为难定位的异常地址、wrap 地址或与 normal legal 标签不一致的 transaction。

### 修改原因

本 plan 只面向自动 normal 地址生成，不扩展 directed fault。normal 模式应生成保守合法的 Sv39
positive-canonical 地址，并在参数非法时 fail-fast，而不是 clamp、wrap、退回 PADDR 窗口或静默改用
固定地址。

### 修改方案与修改逻辑

`seq_csr_common::validate_and_clamp()` 是参数合法性唯一 owner，新增检查：

```text
main_vaddr_range != 0
upper = main_vaddr_base + main_vaddr_range - 1
upper >= main_vaddr_base
main_vaddr_base[63:38] == 0
upper[63:38] == 0
```

检查失败统一 `uvm_fatal`。本轮不做 clamp，不自动改用 `MEMBLOCK_PADDR_*`，也不允许 normal 路径生成
non-canonical fault。

### 文字伪代码

```text
seq_csr_common::validate_and_clamp()：
  保留原公共参数检查顺序；
  如果 main_vaddr_range == 0：
    uvm_fatal，说明 MAIN_VADDR_RANGE 不能为 0；
  upper = main_vaddr_base + main_vaddr_range - 1；
  如果 upper < main_vaddr_base：
    uvm_fatal，说明 VADDR 窗口发生 64-bit wrap；
  如果 main_vaddr_base[63:38] != 0：
    uvm_fatal，说明 normal VADDR base 必须是 Sv39 positive-canonical；
  如果 upper[63:38] != 0：
    uvm_fatal，说明 normal VADDR upper 必须是 Sv39 positive-canonical；
  检查通过后保留参数原值；
  不修改 PADDR 参数；
  不生成替代默认值；
```

## 4. 问题三：`apply_legal_addr_template()` 的地址选择仍按 PADDR 窗口和 fallback 工作

### V2 问题

修改前 `apply_legal_addr_template()` 在 PADDR 窗口中选择 64B 对齐地址，窗口无对齐槽时可能退回
未对齐 base。这会让函数在非法配置下生成与“legal aligned normal address”标签不一致的地址。

### 修改原因

normal 自动主表地址需要满足两个条件：起始地址来自 VADDR 窗口，并且完整访问跨度落在窗口内。
如果窗口不足以容纳一个合法 64B 对齐槽，应当立即报错，而不是生成 fallback 地址。

### 修改方案与修改逻辑

`apply_legal_addr_template()` 改为：

1. 从 `seq_csr_common::get_main_vaddr_base/range()` 读取 VADDR 窗口。
2. 根据 `tr.op_class/fuOpType` 推导完整访问字节数。
3. 在 VADDR 窗口中计算 64B 对齐的首个合法起始地址和最后一个合法起始地址。
4. 随机选择一个 64B slot。
5. 写 `tr.src_0=selected_vaddr`、`tr.imm=0`，调用 `tr.update_vaddr()`。
6. 对生成结果执行自洽检查，失败时 fatal。

地址复用仍在 `apply_addr_reuse_window()` 中按原逻辑执行；manual/boundary 地址生成不受全局
`MEMBLOCK_MAIN_VADDR_*` 拦截。

### 文字伪代码

```text
apply_legal_addr_template(tr)：
  如果 tr 为空：
    uvm_fatal；

  base = seq_csr_common::get_main_vaddr_base()；
  range = seq_csr_common::get_main_vaddr_range()；
  upper = base + range - 1；

  调用 derive_size_bytes：
    根据 tr.op_class 和 tr.fuOpType 推导当前 transaction 的完整访问字节数；
    如果返回 0，说明当前 op 不适合 normal legal 地址模板，uvm_fatal；

  如果 range < size_bytes：
    uvm_fatal，说明窗口容纳不下完整访问；

  aligned_first = align_up(base, 64)；
  latest_start = upper - (size_bytes - 1)；
  如果 latest_start < base：
    uvm_fatal，说明访问跨度计算下溢；
  aligned_last = align_down(latest_start, 64)；

  如果 aligned_first > aligned_last：
    uvm_fatal，说明窗口内没有完整落入范围的 64B 对齐起始槽；

  slot_count = ((aligned_last - aligned_first) / 64) + 1；
  随机选择 slot_pick，范围为 [0, slot_count-1]；
  selected_vaddr = aligned_first + slot_pick * 64；
  access_end = selected_vaddr + size_bytes - 1；

  如果 selected_vaddr < base：
    uvm_fatal；
  如果 access_end > upper 或 access_end < selected_vaddr：
    uvm_fatal；
  如果 selected_vaddr 不是 64B 对齐：
    uvm_fatal；

  tr.src_0 = selected_vaddr；
  tr.imm = 0；
  tr.update_vaddr()：按 src_0 + sign_extend_imm12(imm) 更新 tr.vaddr；
  如果 tr.vaddr != selected_vaddr：
    uvm_fatal，说明生成公式和模板假设不一致；

  不写主表、状态表、TLB 表、queue、cursor、map 或 runtime CSR snapshot；
```

## 5. 修改方案总结

本 plan 修改的是 normal 自动主表地址生成的参数来源和合法槽选择逻辑。

修改前：

```text
apply_legal_addr_template()
  -> 读取 get_paddr_base()/get_paddr_range()
  -> 在 PADDR 窗口内选地址
  -> 无 64B 对齐槽时可能 fallback 到 base
  -> src_0=address, imm=0, update_vaddr()

tlb_map_builder::choose_paddr()
  -> 同样读取 PADDR 窗口生成物理 PPN
```

修改后：

```text
apply_legal_addr_template()
  -> 读取 get_main_vaddr_base()/get_main_vaddr_range()
  -> 在 VADDR 窗口内选择完整访问跨度合法的 64B 对齐槽
  -> 配置非法或无槽时 fail-fast
  -> src_0=selected_vaddr, imm=0, update_vaddr()

tlb_map_builder::choose_paddr()
  -> 继续只读取 PADDR 窗口生成物理 PPN
```

新增参数使 translated testcase 可以配置不同 VA/PA 窗口。默认 VADDR 数值保持旧地址分布，主要用于
避免 Bare smoke 默认行为突变，不表示 VADDR 与 PADDR 仍存在语义耦合。

## 6. 验证方案

静态检查：

```bash
rg -n "MEMBLOCK_MAIN_VADDR_(BASE|RANGE)|get_main_vaddr_(base|range)" \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv \
  mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg \
  AI_DOC/project_management/mem_ut_parameter_management.md

sed -n '/function void memblock_dispatch_base_sequence::apply_legal_addr_template/,/endfunction:apply_legal_addr_template/p' \
  mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv \
  | rg "get_paddr_base|get_paddr_range"
```

第二条命令必须无输出。PADDR getter 在 `tlb_map_builder.sv` 中继续存在。

coding 完成后的远端验证入口：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

VA/PA 解耦定向验收建议：

```text
MEMBLOCK_MAIN_VADDR_BASE  = 0x0000001000000000
MEMBLOCK_MAIN_VADDR_RANGE = 0x0000000010000000
MEMBLOCK_PADDR_BASE       = 0x0000000080000000
MEMBLOCK_PADDR_RANGE      = 0x0000000010000000
```

验收要求：

- issue `src_0/vaddr` 位于 MAIN_VADDR 窗口。
- DTLB/L2TLB entry 的 PPN 位于 PADDR 窗口。
- `tc_sanity/base_fun` 通过，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 非法 VADDR range、溢出或非 Sv39 positive-canonical 配置能在参数检查阶段 fail-fast。

## 7. 风险与未解决项

- 本 plan 不扩展 Sv48 高地址、negative canonical 或 non-canonical fault；这些需要后续 directed 地址专项。
- manual directed 和 boundary profile 不受 `MEMBLOCK_MAIN_VADDR_*` 全局拦截，后续如果要统一边界地址策略，需要单独 plan。
- 本 plan 只整理执行方案，不在本文档阶段执行编译或仿真。
