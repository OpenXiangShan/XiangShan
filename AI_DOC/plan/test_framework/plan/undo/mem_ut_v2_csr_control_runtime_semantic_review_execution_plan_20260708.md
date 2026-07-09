# mem_ut V2 CSR/control runtime 语义复查无代码优先执行 Plan

## 1. Plan 定位

本文是 V2 CSR/control 字段对测试框架 runtime state 影响的无代码优先 review execution plan。若复查确认当前 raw CSR/runtime snapshot 已满足 TLB lookup、权限和 sfence/hfence 需求，本 plan 不要求代码修改；若发现必要字段缺失，必须停止本 plan 并新建专项 coding plan，不得在本 plan 中直接修改 SV/源码。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/tb/csr_ctrl_agent_connect.sv
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
```

只复查：

- 哪些 CSR/control 字段进入 runtime CSR snapshot。
- 哪些字段影响 TLB lookup、权限、sfence/hfence、异常激励合法性。
- 哪些 V2 branch predictor enable/control 字段只属于旁路观察。

不允许：

- 把 branch predictor enable 字段混入 TLB lookup key。
- 用 `seq_csr_common` plus 参数替代 runtime CSR 真值。
- 把 monitor output 分类问题混入本 plan 主流程。
- 在本 review plan 中偷偷新增 raw CSR 字段、monitor 采样或 runtime getter。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 默认只复查 CSR/control runtime 语义，也需要以真实 RTL 和 connect 字段为准；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

V2 CSR/control 字段包括：

```text
btb_enable
ras_enable
sc_enable
tage_enable
ubtb_enable
hd_misalign_ld_enable
hd_misalign_st_enable
tlbCsr_priv_debug
```

V3 中存在不同字段集合，例如 `abtb/mbtb/ittage` 等。当前 V2 connect 层可能已经做同名或近义映射，但测试框架 runtime CSR snapshot 只应保存当前 TLB lookup 和权限相关真值。

`memblock_sync_pkg::dispatch_raw_csr_t` 当前保存：

```text
satp/vsatp/hgatp
priv_mxr/sum/vmxr/vsum/virt/spvp/imode/dmode
m_pbmt_en/h_pbmt_en
```

## 4. 修改原因

CSR/control 字段分三类：

1. TLB/runtime 必需字段：影响 L2TLB lookup key、PTE 权限解释、sfence/hfence。
2. 异常激励合法性字段：可能影响 misalign 或 debug privilege 行为。
3. 旁路控制字段：例如 branch predictor enable，不影响当前 mem_ut LSQ/TLB 状态推进。

如果不分层，容易把近义控制信号直接映射进 runtime CSR，导致 lookup key 或异常模型错误。

## 5. 修改后方案

### 5.1 review 决策表

执行者必须产出或在 implementation review 中记录：

| 字段 | V2 来源 | 当前 raw CSR 是否需要 | 决策 |
|---|---|---|---|
| `satp/vsatp/hgatp` | CSR monitor | 是 | 保留 runtime snapshot |
| `mxr/sum/vmxr/vsum/virt/spvp` | CSR monitor | 是 | 保留 runtime snapshot |
| `m_pbmt_en/h_pbmt_en` | CSR monitor | 是 | 保留 runtime snapshot |
| `btb/ras/sc/tage/ubtb_enable` | CSR control | 否 | 不进入 TLB lookup |
| `hd_misalign_ld/st_enable` | CSR control | 待确认 | 若影响 misalign 激励合法性，另建 plan |
| `tlbCsr_priv_debug` | CSR/TLB control | 待确认 | 若影响权限/异常，另建 plan |

### 5.2 无代码修改路径

若确认 `dispatch_raw_csr_t` 已覆盖 L2TLB lookup 必需字段：

- 不修改 SV。
- 在 review 中记录 V2 branch predictor enable 不进入 runtime CSR。
- 对 `misalign/debug` 写为后续专项风险。

### 5.3 需要代码修改路径

若发现 `hd_misalign_ld/st_enable` 或 `priv_debug` 当前 testcase 必须使用：

- 停止执行本 plan，并新建独立 CSR runtime snapshot coding plan。
- 新增 raw CSR 字段、monitor 采样、`apply_raw_csr_runtime()` 更新和 getter。
- 不在本 review plan 里直接 coding。

## 6. 函数/任务级伪代码

### 6.1 `review_csr_runtime_field_need()`

函数目的：作为执行复查流程，判断 V2 CSR/control 字段是否需要进入 runtime state。

输入：字段名、V2 RTL/monitor 来源、当前公共状态使用点。

输出/副作用：分类结论；不修改代码。

源码级伪代码：

```text
for each csr_control_field:
    users = rg field in common_data_transaction dispatch_monitor_event_adapter l2tlb sequence
    if field participates in tlb lookup key or permission:
        classify REQUIRED_RUNTIME_CSR
    else if field affects generated stimulus legality:
        classify NEED_SPECIAL_PLAN
    else:
        classify OBSERVATION_ONLY
```

中文文字伪代码：

执行者逐个字段查找公共状态和 sequence 使用点。如果字段参与 TLB lookup key、PTE 权限或 CSR runtime snapshot，就归为必需 runtime CSR。如果字段不参与 lookup，但会影响测试框架生成某类异常激励是否合法，例如 misalign 或 debug privilege，则不能直接混入现有 flow，而要另建专项 plan。若字段只是 branch predictor 或旁路控制，就归为观察项，不进入 TLB lookup。

### 6.2 `check_raw_csr_snapshot_sufficient()`

函数目的：确认当前 `dispatch_raw_csr_t` 对 L2TLB responder 足够。

源码级伪代码：

```text
required = {satp_asid, vsatp_asid, hgatp_vmid, priv_virt, priv_mxr, priv_sum, priv_vmxr, priv_vsum, m_pbmt_en, h_pbmt_en}
for each required field:
    require field in dispatch_raw_csr_t
    require monitor writes field
    require common_data_transaction.apply_raw_csr_runtime consumes field
if all present: no code change
else: create CSR runtime coding plan
```

中文文字伪代码：

该检查先列出 L2TLB lookup 和权限解释需要的 runtime CSR 字段。然后逐项确认 raw struct 有字段、monitor 会写字段、公共状态会消费字段。三段链路都存在时，本 plan 不需要代码修改；任何一段缺失都表示 runtime CSR snapshot 不完整，必须新建专项 coding plan，而不是在其他 flow 中临时读取 plus 参数。

## 7. 验收标准

1. branch predictor enable 类字段没有进入 TLB lookup key。
2. `seq_csr_common` 仍只提供 plus 配置，不提供 CSR runtime 真值。
3. L2TLB lookup 使用 runtime CSR snapshot，而不是静态初始配置。
4. `hd_misalign_ld/st_enable`、`tlbCsr_priv_debug` 有明确结论：不影响当前 smoke、后续专项或需要代码修改。
5. 若需要代码修改，必须停止本 plan 并先新建专项 coding plan；本 plan 验收不允许出现 SV/源码修改。

## 8. 验证命令或静态检查

```bash
git diff --check -- AI_DOC mem_ut/ver/ut/memblock
rg -n "btb_enable|ras_enable|sc_enable|tage_enable|ubtb_enable|hd_misalign|priv_debug|dispatch_raw_csr|apply_raw_csr_runtime|seq_csr_common" mem_ut/ver/ut/memblock AI_DOC
```

本 plan 是无代码优先 review plan，默认不执行仿真。若后续专项 plan 实际新增 CSR runtime 字段，再在该专项中执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

## 9. 与原始/初步 plan 差异说明

初步 plan 将 CSR/control 作为需复查项。本文明确它默认是无代码优先 review execution plan，不直接 coding；只有发现 runtime CSR 必需字段缺失时才停止并升级为专项 coding plan。

## 10. 风险与非目标

风险：

- `misalign` 和 `priv_debug` 语义可能影响未来异常 directed testcase，需要后续专项确认。

非目标：

- 不修改 branch predictor control。
- 不实现 CSR checker。
- 不修改 L2TLB lookup key 结构，除非专项 plan 证明需要。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：本 plan 是“无代码优先检查/复查”。默认不修改 SV/源码，不改变 runtime 主逻辑；若发现 runtime CSR snapshot 必需字段缺失，必须停止并新建专项 coding plan。

原测试框架逻辑：

- `csr_ctrl_agent_agent_monitor::mon_data()` 每拍采样 CSR/control 字段，构造 `memblock_sync_pkg::dispatch_raw_csr_t`，只在 payload 变化时更新 latest raw CSR。
- `dispatch_monitor_event_adapter::drain_csr_events()` 读取 latest raw CSR，并调用 `common_data_transaction::apply_raw_csr_runtime()`。
- `mmu_csr_runtime_state::update_from_raw_csr()` 保存 `satp/vsatp/hgatp`、privilege、mxr/sum/vmxr/vsum、virt/spvp 等 runtime 真值。
- L2TLB lookup 使用 runtime CSR snapshot 生成 `asid/vmid` 和 lookup key；`seq_csr_common` 只提供 plus 参数，不提供 CSR runtime 真值。

本 plan 修改后逻辑：

- 默认只复查 V2 CSR/control 字段分类：TLB/runtime 必需、异常激励合法性相关、旁路观察。
- branch predictor enable 类字段不进入 TLB lookup key。
- `hd_misalign_ld/st_enable`、`priv_debug` 等若影响异常 directed testcase，只记录为后续专项，不在本 plan 中混入现有 flow。
- 若 raw CSR 三段链路已经覆盖 L2TLB lookup 必需字段，则本 plan 无代码修改。

逻辑改变项：

- 默认无运行期逻辑改变。
- 若复查发现缺字段，本 plan 不直接实现，而是要求新增 CSR runtime 专项 plan。原因是 CSR runtime 字段会影响 TLB lookup、权限解释或异常激励合法性，必须单独定义 raw struct、monitor 写者、consumer 和验证。

字段/参数改变项：

- 复查字段包括 `satp/vsatp/hgatp`、`priv_mxr/sum/vmxr/vsum/virt/spvp/imode/dmode`、`hd_misalign_ld/st_enable`、`tlbCsr_priv_debug`、V2 branch predictor enable 字段。
- 不新增 plus/cfg；不允许用 `seq_csr_common` 替代 runtime CSR。
- 不修改 L2TLB lookup key，除非后续专项证明缺字段。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增扫描、raw queue 或状态表生命周期。
- 不改变 latest raw CSR 的更新策略和 `raw_csr_seq` 语义。
- 不改变 L2TLB request 的 CSR drain 时机、TLB entry 生命周期、sfence/hfence 失效策略。
- 不改变 terminal/pass/fail。

覆盖性结论：

本 plan 覆盖 CSR/control runtime 语义复查 flow。它确认哪些 V2 control 字段不能混入当前 TLB/LSQ 主状态，哪些字段若缺失必须另建专项。结论是：默认无代码、无主体逻辑改变；该 flow 不遗漏当前 V2 测试框架 runtime CSR 适配的检查入口。
