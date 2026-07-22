# mem_ut V2 主表虚拟地址窗口解耦 Implementation Review

## 1. Review 元数据

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_main_table_vaddr_generation_adapt_execution_plan_20260713.md` |
| 目标版本 | V2，`mem_ut_uvm_v2` |
| 执行日期 | 2026-07-21/22 |
| 源码实现来源 | `22732ba476`，`mem_ut: adapt v2 dispatch framework` |
| 本执行单元 | 复核已有源码、修正文档残留、补齐地址复用跨度收口、执行验证、归档并独立提交 |
| Review 状态 | 通过；第二轮独立 review 无强制修改项 |

## 2. Review 结论摘要

Plan 要求的参数链和基础地址生成逻辑已经在当前基线中完整存在；首轮 review 后本执行单元补充了
地址复用后的最终跨度收口：

1. `MEMBLOCK_MAIN_VADDR_BASE/RANGE` 已形成 `plus.sv -> seq_csr_common -> getter` 参数链。
2. 参数初始化阶段已拒绝零 range、64 bit 回绕和超出 Sv39 正规范正地址空间的窗口。
3. `apply_legal_addr_template()` 已按完整访问字节数在 VADDR 窗口内选择 64B 对齐起始槽。
4. `tlb_map_builder::choose_paddr()` 继续只消费 `MEMBLOCK_PADDR_BASE/RANGE`，VA 与 PA consumer 已解耦。

本轮实际 diff 修正五个当前有效 flow/analysis/web 文档中的旧 PADDR 描述，并在地址复用共同收口点增加
一个 normal-only 合法性适配 helper。所有差异记录在 Plan 的 `IMPLEMENTATION_DELTA`，不改变
boundary/manual 语义，也不改变测试框架主表控制顺序。

## 3. 术语与对象

| 术语 | 通俗解释 | 代码对象 |
|---|---|---|
| 主表 VADDR 窗口 | 自动 normal transaction 可选择的虚拟起始地址范围。 | `main_vaddr_base/main_vaddr_range` |
| PADDR 窗口 | TLB builder 把虚拟页映射到物理页时使用的物理地址范围。 | `paddr_base/paddr_range` |
| 完整访问跨度 | 从起始虚拟地址到本次 load/store 最后一个字节的闭区间。 | `selected_vaddr..access_end` |
| 64B 对齐槽 | 低 6 bit 为 0、且完整访问不会越过 VADDR 窗口的候选起始地址。 | `aligned_base/aligned_upper/slot_count` |

## 4. 功能一：独立 VADDR 参数链

### 4.1 修改前逻辑

旧 `apply_legal_addr_template()` 与 TLB builder 都读取 PADDR 参数，导致 issue 输入虚拟地址与物理映射
范围共用一个配置真源。translated testcase 无法让 VA、PA 落入不同窗口。

### 4.2 修改后逻辑

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，参数定义与解析。

该定义组为自动主表地址增加独立输入；默认数值与 PADDR 相同只用于保持既有 Bare smoke 分布。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_VADDR_BASE, bit [63:0], 64'h8000_0000)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_VADDR_RANGE, bit [63:0], 64'h1000_0000)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PADDR_BASE, bit [63:0], 64'h8000_0000)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PADDR_RANGE, bit [63:0], 64'h1000_0000)
```

中文伪代码：解析层分别保存 MAIN_VADDR 和 PADDR 两组 64 bit 参数；默认值相同不代表语义合并，后续
consumer 必须通过各自 getter 读取。该定义不直接修改 transaction、TLB entry 或公共状态表。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，参数快照与 getter。

该对象是公共 sequence 的唯一参数读取入口，避免业务 helper 长期直接访问 `plus::` 全局变量。

```systemverilog
main_vaddr_base  = plus::MEMBLOCK_MAIN_VADDR_BASE;
main_vaddr_range = plus::MEMBLOCK_MAIN_VADDR_RANGE;
paddr_base       = plus::MEMBLOCK_PADDR_BASE;
paddr_range      = plus::MEMBLOCK_PADDR_RANGE;
```

中文伪代码：初始化时分别复制两组 plus 参数，VADDR 与 PADDR 保存到不同静态快照。该赋值不做
fallback、不修改 transaction，也不把任一窗口覆盖到另一窗口。

源码位置：同一文件，函数 `get_main_vaddr_base()`。

该 getter 返回 VADDR 窗口起点，是 `apply_legal_addr_template()` 的参数来源。

```systemverilog
static function bit [63:0] get_main_vaddr_base();
    check_initialized("get_main_vaddr_base");
    return main_vaddr_base;
endfunction:get_main_vaddr_base
```

中文伪代码：先调用 `check_initialized()` 确认公共配置完成；检查通过后返回 VADDR base。函数只读
快照，不访问 PADDR，也不修改任何运行期状态。

源码位置：同一文件，函数 `get_main_vaddr_range()`。

该 getter 返回 VADDR 窗口大小，是窗口末地址和候选槽计算的输入。

```systemverilog
static function bit [63:0] get_main_vaddr_range();
    check_initialized("get_main_vaddr_range");
    return main_vaddr_range;
endfunction:get_main_vaddr_range
```

中文伪代码：先确认公共配置已初始化，再返回独立 VADDR range。函数不 clamp、不回退到 PADDR，也不
产生 transaction、queue 或状态表副作用。

### 4.3 正确性检查

- `default.cfg` 同时存在 MAIN_VADDR 与 PADDR 四个 key，不需要隐式 fallback。
- `apply_legal_addr_template()` 只出现 VADDR getter。
- `tlb_map_builder::choose_paddr()` 只出现 PADDR getter。
- 参数管理文档已明确两组 consumer，可以配置不同 VA/PA 窗口。

## 5. 功能二：VADDR 窗口 fail-fast

### 5.1 修改前逻辑

共用 PADDR 参数时没有针对 normal 虚拟地址的 canonical 检查。零 range、加法回绕或超出当前 Sv39
positive-canonical 范围的配置可能到主表生成阶段才暴露。

### 5.2 修改后逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，函数
`validate_and_clamp()`。

该函数在 sequence 消费参数前统一拒绝非法窗口；虽然函数名保留 `clamp`，本特性对 VADDR 不做截断。

```systemverilog
fatal_if_zero("main_vaddr_range", main_vaddr_range);
main_vaddr_upper = main_vaddr_base + main_vaddr_range - 1;
if (main_vaddr_upper < main_vaddr_base) begin
    `uvm_fatal("SEQ_CSR_CFG", "main_vaddr_base + main_vaddr_range - 1 overflows")
end
if (main_vaddr_base[63:38] != '0 || main_vaddr_upper[63:38] != '0) begin
    `uvm_fatal("SEQ_CSR_CFG",
               $sformatf("main vaddr window must stay in Sv39 positive-canonical space: base=0x%0h upper=0x%0h",
                         main_vaddr_base, main_vaddr_upper))
end
```

中文伪代码：先拒绝零 range，再计算包含末地址的 `upper`；如果无符号加法回绕，立即 fatal；如果 base
或 upper 的高位不满足当前 normal Sv39 正规范范围，同样 fatal。检查通过时保留原配置，不 clamp、
不替换成 PADDR 默认值，也不生成 directed fault。

### 5.3 正确性检查

- `range=0` 在地址减一前失败，避免下溢。
- `upper < base` 明确识别 64 bit 回绕。
- base 与 upper 同时检查，窗口中间不会跨出正规范区域。
- manual、boundary、non-canonical directed 生成不经过该 normal 参数窗口，Plan 边界保持不变。

## 6. 功能三：完整访问跨度内的 64B 对齐 VADDR

### 6.1 修改前逻辑

旧路径按 PADDR 窗口选起始地址，并可能在没有对齐槽时 fallback 到 base。它没有把操作字节数纳入最后
合法起始地址，可能让“legal address”标签与完整访问范围不一致。

### 6.2 修改后逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，函数
`apply_legal_addr_template()`。

该函数为一笔 normal transaction 选择虚拟起始地址；输入是已经完成 op template 的 `tr`，副作用只写
`src_0/imm/vaddr`。

```systemverilog
base       = seq_csr_common::get_main_vaddr_base();
range      = seq_csr_common::get_main_vaddr_range();
upper      = base + range - 1;
align_mask = 64'd63;
size_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);

if (size_bytes == 0) begin
    `uvm_fatal(get_type_name(),
               $sformatf("apply_legal_addr_template uid=%0d cannot derive access size op_class=%0d fuOpType=0x%0h",
                         tr.uid, tr.op_class, tr.fuOpType))
end
if (range < size_bytes) begin
    `uvm_fatal(get_type_name(),
               $sformatf("main vaddr range=0x%0h is smaller than access size=%0d uid=%0d",
                         range, size_bytes, tr.uid))
end

aligned_base  = (base + align_mask) & ~align_mask;
latest_start  = upper - (size_bytes - 1);
aligned_upper = latest_start & ~align_mask;
if (aligned_base > aligned_upper) begin
    `uvm_fatal(get_type_name(),
               $sformatf("main vaddr window has no 64B-aligned slot for uid=%0d base=0x%0h upper=0x%0h size=%0d",
                         tr.uid, base, upper, size_bytes))
end
slot_count = ((aligned_upper - aligned_base) >> 6) + 1;
```

中文伪代码：从独立 VADDR getter 读取闭区间窗口；`derive_size_bytes()` 根据操作类型和 `fuOpType`
返回完整访问字节数。窗口容不下访问或操作大小无法识别时 fatal。随后向上对齐第一个起始槽，并从
窗口末地址减去访问跨度得到最后可用起点，再向下对齐。若首槽晚于末槽，说明没有合法候选并 fatal；
否则计算候选槽数量。该过程不访问 PADDR、TLB 表或状态表。

源码位置：同一函数，候选选择与结果核对分支。

```systemverilog
if (slot_count <= 1) begin
    slot_pick = 64'd0;
end else begin
    slot_pick = {$urandom(), $urandom()} % slot_count;
end
selected_vaddr = aligned_base + (slot_pick << 6);
access_end = selected_vaddr + size_bytes - 1;
if (selected_vaddr < base || access_end < selected_vaddr || access_end > upper ||
    (selected_vaddr & align_mask) != 0) begin
    `uvm_fatal(get_type_name(),
               $sformatf("main vaddr generation bug uid=%0d selected=0x%0h end=0x%0h window=[0x%0h,0x%0h] size=%0d",
                         tr.uid, selected_vaddr, access_end, base, upper, size_bytes))
end
tr.src_0 = selected_vaddr;
tr.imm   = 64'h0;
tr.update_vaddr();
if (tr.vaddr != selected_vaddr) begin
    `uvm_fatal(get_type_name(),
               $sformatf("main vaddr update mismatch uid=%0d expected=0x%0h actual=0x%0h",
                         tr.uid, selected_vaddr, tr.vaddr))
end
```

中文伪代码：只有一个槽时固定选择它；多个槽时随机选择合法索引。根据槽索引生成 64B 对齐地址并
计算访问末字节，随后再次检查起点、终点、加法回绕和对齐。检查通过后写入 `src_0`、把立即数清零，
再由 `update_vaddr()` 按正式地址公式更新 transaction；结果与选中地址不同则 fatal。函数不写主表
容器、状态表、queue、map、cursor 或 terminal 字段。

### 6.4 地址复用后的最终跨度收口

#### 6.4.1 修改前逻辑

`build_random_main_table()` 先通过 `randomize_main_transaction()` 生成一个合法地址，之后才调用
`apply_addr_reuse_window()`。地址复用可能切换最终 load/store 类型和 `fuOpType`，但旧的无参考 fallback
只切换类型而保留旧地址，复制参考地址的分支也只调用 `update_vaddr()` 与主表字段校验。窄窗口末端因此
可能出现“原访问合法、最终访问跨度越界”的情况。

#### 6.4.2 修改后逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，
函数 `fixup_after_addr_reuse()` 和 `apply_addr_reuse_window()` 的 fallback 分支。

这段逻辑负责在地址复用完成后决定最终 transaction 是否仍可作为 normal 主表项使用；它只修改当前
transaction 的地址/操作模板，不写状态表或 LSQ 状态。

```systemverilog
if (!got_ref) begin
    fallback_caller = $sformatf("%s fallback uid=%0d", caller_prefix, cur_uid);
    tr.op_class = fallback_op_class;
    apply_minimal_op_template(tr);
    if (seq_csr_common::get_boundary_profile_gen_en()) begin
        fixup_after_addr_reuse(tr, null, 1'b0, fallback_caller);
    end else begin
        apply_legal_addr_template(tr);
        validate_main_table_entry(tr, fallback_caller);
    end
    return;
end
```

中文伪代码：地址复用没有找到参考 uid 时，先按 fallback 操作类别重新生成合法的 load/store 模板。
如果当前是 boundary profile，继续沿用 boundary 地址并只做原有 fixup；如果是 normal 自动主表，则在最终
操作类型确定后重新调用虚拟地址合法化函数，避免旧地址只对原操作大小合法。最后调用主表字段校验，随后
返回主表生成循环；该分支不分配 LSQ index，也不更新状态表或 terminal。

源码位置：同一文件，函数 `ensure_normal_reused_addr_span()`，由 `fixup_after_addr_reuse()` 在复制
参考 `src_0/imm` 后调用。

该 helper 的输入是当前 transaction、参考 transaction 和调用者字符串；输出没有独立返回值，副作用是
必要时把目标 load/store `fuOpType` 收敛到参考访问大小。它只在 normal 路径工作，boundary profile 直接
返回。

```systemverilog
tr.update_vaddr();
size_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
access_end = tr.vaddr + size_bytes - 1;
if (tr.vaddr >= base && access_end >= tr.vaddr && access_end <= upper) begin
    return;
end

ref_tr.update_vaddr();
ref_size_bytes = derive_size_bytes(ref_tr.op_class, ref_tr.fuOpType);
ref_end = ref_tr.vaddr + ref_size_bytes - 1;
if (ref_tr.vaddr < base || ref_end < ref_tr.vaddr || ref_end > upper) begin
    `uvm_fatal(get_type_name(),
               $sformatf("%s ref uid=%0d address span is outside MAIN_VADDR", caller, ref_tr.uid))
end
fitted_fuOpType = default_fuop_by_op_class_and_size(tr.op_class, ref_size_bytes);
apply_op_class_template(tr, fitted_fuOpType);
tr.update_vaddr();
size_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
access_end = tr.vaddr + size_bytes - 1;
if (tr.vaddr < base || access_end < tr.vaddr || access_end > upper) begin
    `uvm_fatal(get_type_name(),
               $sformatf("%s uid=%0d could not fit reused address span", caller, tr.uid))
end
```

中文伪代码：先用 `derive_size_bytes()` 计算复制地址后最终 opcode 的访问字节数，并检查起点、终点、
回绕和 MAIN_VADDR 上界；已经合法时直接返回。若越界，读取参考 transaction 的地址和访问大小，确认参考
本身仍是合法 normal 地址；再用 `default_fuop_by_op_class_and_size()` 为当前目标 load/store 选择相同
访问大小的默认 opcode，并由 `apply_op_class_template()` 重建匹配的 `fuType/lsq_flow/numLsElem`。
重新计算 `vaddr` 和访问末字节，第二次检查失败则 fatal。该 helper 保留复制地址，因此 RAW/地址复用关系
不变；它不调用 admission、issue、writeback、commit 或 deq helper。

#### 6.4.3 正确性检查

- 无参考 fallback 的 normal 路径最终重新选择地址，因此不会把旧类型的地址合法性带入新类型。
- 有参考地址且最终尺寸合法时，transaction 完全保持原随机结果；只有越界时才采用参考尺寸的默认 opcode。
- 参考地址自身越界、访问尺寸不可识别、算术回绕或修正后仍越界均 fatal，不能静默把非法地址送入主表。
- `boundary_profile_gen_en=1` 和 manual table 不消费 `MAIN_VADDR`，保持原有 boundary/manual 地址语义。

## 7. 功能四：PADDR consumer 保持隔离

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`，函数 `choose_paddr()`。

该函数只在构造 TLB entry 时把 VPN 映射到物理页，不能反向约束 issue 输入虚拟地址。

```systemverilog
base       = seq_csr_common::get_paddr_base();
range      = seq_csr_common::get_paddr_range();
page_count = range >> 12;
if (page_count == 0) begin
    page_count = 1;
end
vpn_mix    = (vaddr >> 12) % page_count;
page_offset = vaddr[11:0];
return (base & 64'hffff_ffff_ffff_f000) + (vpn_mix << 12) + page_offset;
```

中文伪代码：读取独立 PADDR 窗口并按 4KB 页计算页数；不足一页时把页数保护为 1，避免除零。随后
根据物理页数量对输入 VPN 取模，再把页内偏移原样拼回。返回值只用于 TLB entry 的物理地址字段；
函数不读取 MAIN_VADDR 参数，也不改 transaction 的 `src_0/imm/vaddr`。

## 8. 调用关系

| 顺序 | 对象 | 本流程职责 | 主要副作用 |
|---:|---|---|---|
| 1 | `plus::load_plus_args()` | 解析 MAIN_VADDR/PADDR 四个 cfg key。 | 更新 plus 输入层变量。 |
| 2 | `seq_csr_common::load_from_plus()` | 复制为公共参数快照。 | 更新静态配置快照。 |
| 3 | `validate_and_clamp()` | 对 normal VADDR 窗口 fail-fast。 | 非法配置报 fatal；合法配置不改值。 |
| 4 | `apply_legal_addr_template()` | 选择合法虚拟起始地址。 | 写当前 transaction 的 `src_0/imm/vaddr`。 |
| 5 | `tlb_map_builder::choose_paddr()` | 后续按独立物理窗口建立映射。 | 返回 PADDR，不回写 VADDR 参数。 |

## 9. 本执行单元 Diff 覆盖

| 文件 | 变更 | Review 覆盖 |
|---|---|---|
| `AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md` | 把旧 PADDR 虚拟地址伪代码改为 MAIN_VADDR + 完整跨度。 | 第 2、6 节 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_base_sequence.md` | 更新 helper 表中的参数来源和 fail-fast 语义。 | 第 2、6 节 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 地址复用复制地址后补最终 MAIN_VADDR 访问跨度收口；无参考 fallback 按最终类型重新合法化地址。 | 第 4、6、10 节 |
| `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md` | 更新调用表中的 VADDR 参数来源与完整访问跨度。 | 第 2、6 节 |
| `AI_DOC/web/memblock_dispatch_control_flow_callgraph/assets/app.js` | 更新基础 callgraph 的 helper 输出、职责和 VA/PA 边界。 | 第 2、6 节 |
| `AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js` | 同步增强版 callgraph 的相同语义。 | 第 2、6 节 |
| 执行 Plan | 增加 `IMPLEMENTATION_DELTA`，记录已有源码、文档同步和首轮 review 修复原因。 | 第 10、11 节 |
| 本 review | 新增完整源码审计记录。 | 全文 |

## 10. 实现与 Plan 不一致项

参数链和基础 `apply_legal_addr_template()` 已由 `22732ba476` 提前落地；本执行单元没有重复制造
这部分源码 diff。首轮 review 发现地址复用阶段可能在最终类型变化后越过窗口，因此新增
`ensure_normal_reused_addr_span()`，该差异已写入 Plan 的 `IMPLEMENTATION_DELTA`。

Plan 第 1 章原本排除 flow 文档修改，但当前有效 flow/analysis 仍含旧 PADDR 描述。依据 plan execution
规则，本轮同步文档并补齐 normal 地址复用的最终合法性；该同步差异和行为边界也已写入
`IMPLEMENTATION_DELTA`。

## 11. Plan 未说明但 Coding 落实的细节

源码中 `slot_count<=1` 时直接选择槽 0，避免对 1 取模和无意义随机；这只是 Plan“随机选择合法槽”在
单候选场景的确定化实现，不改变地址集合。首轮 review 修复的 helper 只在 normal 复制地址后的
访问尺寸越界时选择参考尺寸的合法 load/store opcode；无参考 fallback 重新选 legal address，
不新增状态、queue、map、pass/fail 或 terminal 行为。

## 12. 验证记录

| 检查 | 结果 |
|---|---|
| VADDR 参数/getter静态搜索 | 通过；定义、snapshot、getter、consumer 和 default cfg 链路完整。 |
| `apply_legal_addr_template()` 无 PADDR getter | 通过；函数内只消费 MAIN_VADDR getter。 |
| PADDR consumer仍存在于 `tlb_map_builder` | 通过；`choose_paddr()` 继续消费 PADDR getter。 |
| 当前有效文档无旧 PADDR 主表生成描述 | 通过；flow、source analysis 和两版 web callgraph 已同步。 |
| `git diff --check` | 通过。 |
| 远端干净 `make eda_compile tc=tc_sanity mode=base_fun` | 通过；VCS/KDB 0 error。 |
| 远端 `make eda_batch_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` | 通过；`TEST CASE PASSED`，`UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |
| VA/PA 不同窗口定向 smoke | 通过；临时 preset 配置 `MAIN_VADDR_BASE=0x1000000000`、`PADDR_BASE=0x80000000`，主表到 terminal 完整闭环且 0 warning/error/fatal；临时 preset 验证后已删除。 |
| 首轮 review 指出的窄窗口复用场景 | helper 已触发并打印 `fitted reused access to ref_size=1`；随后仿真在既有 `DUT sqDeq ... mismatches software SQ head` 处 fatal，属于后续 SQ deq/store owner，不是 VADDR helper 失败。临时 preset 已删除。 |
| 修复后远端干净 `make eda_compile tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` | 通过；VCS/KDB 0 error。 |
| 修复后默认 `tc_dispatch_real_smoke` | 通过；`TEST CASE PASSED`，`UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |

第一次编译后直接调用 `eda_run` 时，VCS 增量阶段报告 `tdc.sdb` 损坏。删除本地生成目录后重新执行
干净编译成功，确认该现象属于生成缓存损坏。由于 `eda_run` 会再次编译，本轮在成功的干净产物上调用
项目自带 `eda_batch_run` 完成仿真。`tc_sanity/default` 的 virtual sequence 为空，不生成主表，因此不作为
本专项动态验收；主表 flow 使用现有 `tc_dispatch_real_smoke` 验收。不同窗口定向运行确认
`MEMBLOCK_MAIN_VADDR_BASE=0x1000000000` 生效，但当前 testcase 处于 Bare 场景，没有产生 translated
L2TLB lookup，因此该结果不用于宣称 PPN 动态落入 PADDR 窗口。

通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=tc_dispatch_real_smoke_seed=666666_rtl_.log`。

不同窗口定向日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=tc_dispatch_real_smoke_vaddr_decouple_tmp_seed=666666_rtl_.log`。

## 13. 剩余风险

- 本 Plan 不覆盖 Sv48、negative canonical、non-canonical fault、manual 或 boundary 地址策略。
- 本轮已覆盖 VA/PA 不同窗口下的 Bare load 闭环；启用地址翻译后验证 PPN 落入 PADDR 窗口，仍需在
  后续 translated 地址专项扩大覆盖。
- 本 review 不承担 RM/checker/coverage 正确性比较职责。
- translated PPN 的动态 PADDR 窗口验证仍需后续 translated 地址专项；本轮 Bare smoke 只验证 VA 参数解耦。

## 14. 第二轮独立 Review 结论

第二轮 subagent 按当前源码、Plan `IMPLEMENTATION_DELTA`、review 第 6.4 节和 git diff 独立复核，结论为：

> 最终 review 通过，无强制修改项。

复核确认：

- `ensure_normal_reused_addr_span()` 的调用路径覆盖复制地址后的最终跨度检查。
- normal fallback 在最终类型确定后重新执行 `apply_legal_addr_template()`。
- 越界时按参考访问尺寸选择默认 `fuOpType`，boundary profile 入口直接返回。
- review 伪代码、源码路径和实际实现一致；`git diff --check` 通过。
