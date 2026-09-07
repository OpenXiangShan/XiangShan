# V2 boundary 地址窗口与 Sv39 canonical 修正 Implementation Review

| 项目 | 内容 |
| --- | --- |
| Review 状态 | 已完成实现与独立复核；boundary plan 可归档。real-DUT 压力回归因独立确认的 V2 RTL blocker 停止。 |
| 对应 Plan | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_boundary_vaddr_window_canonical_fix_plan_20260904.md` |
| 源码范围 | `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，以及无 reference fallback 的 software-only sequence/testcase/cfg。 |
| 文档范围 | boundary flow、参数 consumer 说明、历史 plan 注记、source-SV analysis |
| DUT/RTL 修改 | 无 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| boundary profile | 自动主表为普通访存选择的地址边界类别。 | `main_control_transaction.boundary_profile` | `CROSS_16B_SAME_LINE` 保证访问跨 16B、不跨 64B。 |
| anchor | 添加 profile 固定偏移前的对齐 VA 基点。 | `random_aligned_vaddr()` 返回值 | cross-16B 以 64B 对齐 anchor 加 `bank/k` 偏移。 |
| tail offset | anchor 到本次访问最后一个字节的偏移。 | `gen_final_vaddr_by_profile()` 局部值 | 它同时覆盖起始偏移和 `size-1`。 |
| full VA | `src_0 + sign_extend(imm)` 得到、DUT 实际检查的有效虚拟地址。 | `main_control_transaction.vaddr` | 模板先构造 `final_vaddr`，回填后再以 `tr.vaddr` 验证。 |
| MAIN_VADDR window | normal 自动主表和自动 boundary 模板允许使用的 VA 闭区间。 | `seq_csr_common::get_main_vaddr_base/range()` | PBMT0 cfg 使用 `0x8000_0000..0x8fff_ffff`。 |
| Sv39 positive canonical | Sv39 低半区地址，bit38 及所有更高位均为 0。 | `is_sv39_positive_canonical()` | `0x0000_004e_...` 的 bit38=1，因此不能作为自动 normal VA。 |
| manual directed | testcase 显式指定的主表输入，不是自动 boundary 随机路径。 | `manual_main_table_by_rob` | 有意构造 page fault/non-canonical 地址时保持可用。 |
| slot | anchor 选择器中两个相邻对齐地址之间的离散候选。 | `aligned_first/aligned_last/slot_pick` | 64B 对齐且窗口足够大时，从合法 slot 均匀取一个。 |

抽象功能描述：本功能只修复自动 boundary 地址构造。`random_aligned_vaddr()` 在当前
`MAIN_VADDR` 窗口内选择能够容纳完整访问尾部的对齐 anchor；
`gen_final_vaddr_by_profile()` 保持既有 profile 语义并叠加偏移；
`apply_boundary_addr_template()` 在回填 `src_0/imm` 后复查 full VA。它们都不修改 RM、TLB、PBMT、
PMA/PMP、DUT 或 manual directed 输入。

## 2. Review 范围与问题重现

原始严格 PBMT0/non-NC Sv39 运行的 UID0 自动地址为：

```text
VA = 0x0000_004e_0e34_fb2f
```

它满足旧实现的 `vaddr[63:39] == 0`，但 bit38 为 1；因此不是 Sv39 canonical 地址。DUT full-VA
precheck 正确产生 Store Page Fault，RM 仍只预期非对齐异常，形成初始 mismatch。独立 RTL review
已确认该 DUT 行为和 PBMT 无关，不应修改 RTL。

本次 review 覆盖下列功能改动：

| 文件 | 修改性质 | Review 结论 |
| --- | --- | --- |
| `memblock_dispatch_base_sequence.sv` | 自动 boundary VA 构造 | 已按 plan 修复 canonical、窗口、尾部和宽位溢出检查。 |
| `soft_test_boundary_addr_reuse_gate_sequence.sv` / `soft_test_tc_boundary_addr_reuse_gate.sv` / `tc_boundary_addr_reuse_store_cross8_gate.cfg` | 无 reference fallback 定向检查 | 已固定覆盖 Load 改 Store 后仍须遵守 `STORE x CROSS_8B` gate 的分支。 |
| `seq_pkg.sv` / `tc_pkg.sv` | software-only testcase 注册 | 已注册专用 sequence 与 testcase，编译和运行入口闭合。 |
| `main_table_boundary_profile_generation_flow.md` | 实际调用链和地址生成说明 | 已同步为 window anchor/tail flow。 |
| `memblock_dispatch_base_sequence.md` | source-SV analysis 路径与 helper 说明 | 已修正源码路径并补充新 helper 语义。 |
| `mem_ut_parameter_management.md` | `MAIN_VADDR` consumer 边界 | 已明确自动 boundary 是 consumer，manual directed 不受全局过滤。 |
| `main_table_boundary_candidate_addr_generation_plan_20260701.md` | 历史实现注记 | 已标记 `2^39`/`[63:39]` 描述过时。 |
| `plus_demo_migration_plan.md` | 参数 consumer 说明 | 已补充自动 boundary 消费窗口的规则。 |

## 3. 修改前后行为

### 3.1 修改前

```text
profile 模板
  -> random_aligned_vaddr(align)
  -> 在 [0, 2^39) 选择 anchor
  -> 添加 profile offset
  -> 仅以 [63:39] 判断 canonical
  -> 回填 src_0/imm
```

问题在于 `[0, 2^39)` 覆盖 Sv39 bit38=1 的非法洞，且该路径没有读取 `MAIN_VADDR_BASE/RANGE`。
profile 访问尾部也没有约束到配置窗口内。

### 3.2 修改后

```text
profile / size
  -> 固定 offset、tail offset
  -> random_aligned_vaddr(align, tail)
  -> 在 MAIN_VADDR 合法 slot 中选择 anchor
  -> 添加 offset 并检查回绕
  -> 回填 src_0/imm，update_vaddr
  -> 检查 full VA、访问末字节、窗口和 [63:38] canonical
  -> 复查 profile 分类
```

新流程保持一次随机选择、无 retry、无主表扫描。对每笔 transaction 的开销是固定数量的位宽算术和一次
取模，不会随着 `MAIN_TRANS_NUM` 增长而扫描历史 main table、TLB map 或 RM 表。

## 4. 重点函数复核

### 4.1 `is_sv39_positive_canonical()`

抽象功能描述：该纯 helper 只判断自动 boundary 模板使用的 Sv39 低半区 canonical 规则，不读取配置、
不修改 transaction，也不拦截 manual directed 地址。

```systemverilog
function bit memblock_dispatch_base_sequence::is_sv39_positive_canonical(input bit [63:0] vaddr);
    return vaddr[63:38] == '0;
endfunction:is_sv39_positive_canonical
```

中文伪代码：读取 bit63 到 bit38；只要任意一位为 1 就返回 false。bit38 是 Sv39 的符号位，因此该
检查修复了旧 `[63:39]` 漏检。

正确性判断：`0x0000_004e_0e34_fb2f` 现在会被拒绝；配置中的 `0x8000_0000..0x8fff_ffff` 全部保持
低半区 canonical。

### 4.2 `random_aligned_vaddr(align_bytes, tail_offset)`

抽象功能描述：该 helper 是自动 boundary 路径唯一的 anchor 选择器。它根据调用者给出的对齐要求和
完整访问尾部偏移，在 `MAIN_VADDR` 中选择合法对齐地址；它不修改 profile、transaction、队列或 map。

```systemverilog
main_vaddr_limit = {1'b0, main_vaddr_base} + {1'b0, main_vaddr_range};
main_vaddr_upper = main_vaddr_limit[63:0] - 64'd1;
latest_anchor = main_vaddr_upper - tail_offset;
aligned_first = ({1'b0, main_vaddr_base} + {1'b0, align_mask}) & ~align_mask;
aligned_last  = latest_anchor & ~align_mask;
slot_pick = random64() % slot_count;
slot_product = {66'd0, slot_pick} * {66'd0, align_bytes};
candidate = ({1'b0, aligned_first} + slot_product[64:0])[63:0];
```

中文伪代码：先以 65-bit 计算 `base + range`，拒绝空窗口和 64-bit 回绕。再由 `upper-tail` 得到最后
允许的 anchor，将窗口首尾分别向上/向下对齐。若没有合法 slot，则报告 cfg 或模板错误。随机 slot
乘以对齐粒度时，两个操作数显式扩宽为 130-bit，检查高位后再形成候选地址；候选 anchor 和
`candidate+tail` 均要二次确认未回绕且不越过窗口。

调用关系：

| 顺序 | 调用者/子对象 | 当前职责 |
| --- | --- | --- |
| 1 | `gen_final_vaddr_by_profile()` | 先选择 profile 离散变量，传入对齐粒度和 tail。 |
| 2 | `seq_csr_common::get_main_vaddr_base/range()` | 读取既有的单一参数快照。 |
| 3 | `random64()` | 只提供一次 64-bit 随机数，决定 slot。 |
| 4 | 返回 anchor | 调用者再叠加固定 offset 并复查。 |

正确性判断：访问末字节由 tail 本身纳入 slot 上界；因此任何 profile 不会仅因其起始 VA 在窗口内而让
末字节跨出窗口。

### 4.3 `gen_final_vaddr_by_profile()`

抽象功能描述：该函数保留已有 boundary profile 的大小矩阵、bank/line/k 分布和分类语义，只将
anchor 来源收敛为 `MAIN_VADDR` 内能容纳 tail 的候选。

```systemverilog
final_offset = bank * 16 + 16 - k;
tail_offset  = final_offset + size_bytes - 1;
anchor       = random_aligned_vaddr(64'd64, tail_offset);
final_vaddr  = anchor + final_offset;
if (final_vaddr < anchor) `uvm_fatal(...);
return final_vaddr;
```

中文伪代码：每个 profile 先决定其离散参数，例如 cross-16B 的 `bank/k`。随后将从 anchor 到访问最后
字节的距离传给 helper，因此 helper 不会改变 profile 的几何关系。`CROSS_4K` 删除旧 `2^39` 页数
计算，改用同一个 4KB 对齐 helper；所有 profile 因而使用同一窗口和回绕规则。

正确性判断：`ALIGNED`、within-8B、cross-8B、cross-16B、cross-cacheline 与 cross-4K 的既有
分类仍由 `classify_boundary_profile()` 最后复查；任何计算错误会 fail-fast，不能 fallback 为 ALIGNED。

### 4.4 `apply_boundary_addr_template()`

抽象功能描述：该函数将生成的 final VA 以既有负 `imm12` 形式拆回可驱动 transaction，并在
`update_vaddr()` 后验证 DUT 实际看到的完整访问范围；它不改变 manual directed 或全局主表校验。

```systemverilog
final_vaddr = gen_final_vaddr_by_profile(profile, size_bytes, tr.op_class);
end_vaddr = final_vaddr + size_minus_one;
tr.src_0 = final_vaddr - sign_extend_imm12(imm12);
tr.imm = imm12;
tr.update_vaddr();
full_end_vaddr = tr.vaddr + size_minus_one;
if (tr.vaddr < main_vaddr_base || full_end_vaddr > main_vaddr_upper ||
    !is_sv39_positive_canonical(tr.vaddr) ||
    !is_sv39_positive_canonical(full_end_vaddr)) `uvm_fatal(...);
```

中文伪代码：先检查 `final_vaddr` 与末字节的回绕和 canonical，再用既有负立即数拆分，使非零 imm
路径仍被覆盖。回填后调用 `tr.update_vaddr()`，确保不依赖拆分前局部变量。最后验证 full VA 的起始、
末字节、窗口与 canonical，并比较实际 profile；此检查只在自动 boundary 函数内发生，不会把手工
fault 场景拦住。

正确性判断：对齐和窗口上界都在回填前后双重验证，避免 `src_0/imm` 重构产生意外回绕或越界。

## 5. 文档同步复核

`main_table_boundary_profile_generation_flow.md` 已把流程图和伪代码改为 offset/tail 后选择
`MAIN_VADDR` anchor，并明确 `[63:38]` 的 canonical 规则。参数管理和 plus migration 文档都区分：
自动 normal/boundary 是 window consumer，manual directed 不是全局 consumer。历史 plan 顶部已加注记，
不再把旧 `2^39` 描述误认为当前行为。

source-SV analysis 原先指向 `seq/base_seq/`，本次同步修正到真实的
`seq/base_seq_help/memblock_dispatch_base_sequence.sv`，并增加新 helper 的职责说明。

## 6. 验证结果

### 6.1 静态与编译

已执行：

```bash
git diff --check -- <本功能源码和文档范围>
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=basicTest \
  ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq \
  mode=pbmt0_non_nc_vaddr_fix \
  cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
```

VCS 编译完成，未出现 `Error-[...]`。日志保留 1 条工具许可功能提示和 48 条既有
`Warning-[KUAI]`；它们并非本次源码的新增编译错误，因此不将本次结果表述为“0 warning”。

### 6.2 PBMT0/non-NC 1k smoke

已执行：

```bash
make eda_run tc=basicTest \
  ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq \
  mode=pbmt0_non_nc_vaddr_fix \
  cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k \
  plus_arg='+MEMBLOCK_MAIN_TRANS_NUM=1000'
```

日志确认实际运行参数为：

- `mPBMTE=hPBMTE=0`；S1/S2 PBMT 为 `00`；
- `MAIN_VADDR` 与 PA 均为 `0x8000_0000..0x8fff_ffff`；
- 主表已构造 1000 笔，说明地址生成没有因窗口或 canonical 校验 fatal；
- 原本的 non-canonical UID0 pre-fault 未再出现。

运行在 `727.8ns` 由于另一个测试框架 fatal 停止：

```text
INT_WB_STA0_TRIGGER_PROVENANCE
STA0 trigger=0 without breakpoint needs uncache/CBO provenance
```

该事件与 boundary 地址、PBMT 和 RM 无关，但独立 Scala、生成 RTL 与 FSDB 复核已确认它是 V2
`StoreMisalignBuffer` 的普通跨 16B store `trigger` metadata 缺陷，而非 checker 误判。最终 STA0
writeback 取的是在 MAB admission 时保存的早期 `s1_in.uop.trigger=0`，并非无 trigger 时应有的
`TriggerAction.None=4'hf`。详见
`AI_DOC/analysis/rtl/v2/flows/store_misalign_trigger_metadata_propagation.md`。

因此本 review 将 1k 结果记为“boundary 输入修复已生效，但被独立确认的 DUT blocker 中止”；根据用户的
停止规则，不删除或放宽 `INT_WB_STA0_TRIGGER_PROVENANCE`，也不继续运行 100k。RTL 修复并重新接入后，
才应重新执行完整 1k/100k。

## 7. Plan 对齐检查

关联 plan：
`AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_boundary_vaddr_window_canonical_fix_plan_20260904.md`。

| Plan 项 | 实现状态 | 复核结论 |
| --- | --- | --- |
| `[63:38]` canonical | 已完成 | 与 plan 一致。 |
| `MAIN_VADDR` anchor/tail slot 选择 | 已完成 | 与 plan 一致。 |
| 所有 profile 先 offset/tail 后 anchor | 已完成 | 与 plan 一致，`CROSS_4K` 已删除 `2^39` 硬编码。 |
| final/full VA 窗口与回绕检查 | 已完成 | 与 plan 一致。 |
| 不改 `validate_main_table_entry()` | 已完成 | 与 plan 一致，manual directed 行为保留。 |
| 文档同步 | 已完成 | 与 plan 一致。 |
| PBMT0 1k/100k 验证 | 1k 在 `727.8ns` 命中已确认的 V2 MAB metadata blocker；100k 未启动 | 地址修复已覆盖到主表构造，100k 按用户 RTL-blocker 停止规则暂停。 |

### 7.1 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；当前 coding 行为与对应 plan 保持一致。

### 7.2 Plan 未说明但 Coding 落实的细节

| 细节功能 | 原因与作用 | 源码位置 | 是否回写 Plan |
| --- | --- | --- | --- |
| 显式 130-bit slot 乘法 | SystemVerilog 乘法结果宽度可能不随接收变量扩展；将两个乘数扩到 130-bit 后再检查高位，避免溢出检测失效。 | `random_aligned_vaddr()` | 已以 `IMPLEMENTATION_DELTA` 回写。 |
| 地址复用后的最终 span 收敛 | 初始模板之后仍可能复制 reference 地址并改变访问大小；boundary 路径需要对最终 full VA 再检查窗口和 canonical。 | `ensure_reused_addr_span()`、`sync_boundary_profile_after_addr_reuse()` | 已以 `IMPLEMENTATION_DELTA` 回写。 |
| 无 reference Store cross-8B gate | `LOAD_AFTER_STORE` fallback 可把初始 Load 改成 Store，必须再次检查 Store 专用 gate，不能只依赖初始 candidate cache。 | `apply_addr_reuse_window()` 与 software-only testcase | 已以 `IMPLEMENTATION_DELTA` 回写并通过定向仿真。 |

中文伪代码：将 `slot_pick` 和 `align_bytes` 先零扩展到 130-bit 后相乘；若乘积 bit129:64 任何一位
非零则 fatal。只有乘积可完全放入候选地址加法的有效范围时，才用其低位构造 anchor。

中文伪代码：地址复用命中 reference 时，先复制地址再检查最终 size 的完整访问范围；若无法容纳，保留
reference 地址关系并将当前访问收敛到 reference size。没有 reference 时，不伪造关系，而是按 fallback
后的最终 op/size 重建 boundary 地址；若该 op 是 Store 且 Store cross-8B gate 关闭，就把 profile
降为 `ALIGNED` 后再生成和检查。

## 8. 非本次修改的逻辑分析

本次 review 只覆盖上表列出的 boundary 功能文件。当前工作区还包含以下非本次修改，未纳入本功能
正确性判断：

| 类别 | 文件或目录 | 判断 | 原因 |
| --- | --- | --- | --- |
| V2 DUT metadata blocker | `StoreMisalignBuffer`、`dispatch_monitor_event_adapter.sv` | 不修改测试框架 checker | 普通跨 16B MAB parent 丢失 trigger metadata；独立 RTL review 已确认，原 checker 删除 plan 已废止。 |
| PBMT 压力功能 | 已提交的 PBMT CSR/VSEQ/cfg 改动 | 已有独立提交 | 不是本 boundary 地址实现。 |
| 其它 AI/RTL 分析文档 | `AI_DOC/analysis/**` 的既有脏改动 | 非本次逻辑 | 用户工作区原有分析资料。 |
| 仿真产物 | `mem_ut/ver/ut/memblock/sim/**` | 非源码 review | 远端编译、FSDB、Verdi 临时文件和日志。 |

## 9. 结论与剩余风险

地址生成修复本身与 plan 一致；编译未出现 `Error-[...]`，且 software-only 定向测试已通过。PBMT0/non-NC
1k 证明自动主表能够建立 1000 笔合法窗口内地址，原 Sv39 non-canonical pre-fault 未再出现。完整 1k/100k
completion 尚未完成的唯一原因是独立确认的 V2 `StoreMisalignBuffer` RTL metadata blocker；按用户规则，
不关闭 checker、不修改 RTL，停止后续压力测试并记录波形路径。
