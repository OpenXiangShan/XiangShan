# mem_ut V2 L2TLB response permission 适配执行 Plan

## 1. Plan 定位

本文是 V2 L2TLB responder response 权限字段的执行 plan。目标是在保持 `DTLB -> L2TLB_agent -> DTLB` responder 模型不变的前提下，确保 V2 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 等权限字段由 TLB entry 真实驱动。

本 plan 默认先执行静态检查和 profile 同步：若当前源码已经满足完整链路，只记录检查结论，不改代码；只有发现任一链路断裂、active 接管路径常量化或 profile 描述不一致时，才按本 plan 做最小 coding 修复。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv
mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv
mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
```

不允许：

- 不把 `L2TLB_agent` 改成 L2Cache/PTW/memory 下游模型。
- 不把顶层 `io_l2_tlb_req_*` 当成当前 responder 接管点。
- 不用 paddr 查表替代 req `vpn/s2xlate` 和 runtime CSR。
- 不混淆 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` active 接管分支和 `=0` inactive 默认分支语义。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 需要对照实际 RTL 内部 `_inner_dtlbRepeater_*` 和 `_inner_ptw_io_tlb_1_*` wire；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

V2 profile 明确当前接管点：

```text
request:
  _inner_dtlbRepeater_io_ptw_req_0_valid
  _inner_dtlbRepeater_io_ptw_req_0_bits_vpn
  _inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate

response:
  _inner_ptw_io_tlb_1_req_0_ready
  _inner_ptw_io_tlb_1_resp_*
```

当前 `memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry()` 已将：

```text
entry.pte_g -> s1_entry_perm_g / s2_entry_perm_g
entry.pte_u -> s1_entry_perm_u / s2_entry_perm_u
```

当前 `L2tlb_agent_connect.sv` active 分支也已 force `s2_entry_perm_g/u`。本 plan 先把该链路固化成可执行检查和后续 coding 标准，防止后续更新 RTL 或 agent 时回退为常量 0。

已确认的新问题：

- 当前 `memblock_tlb_entry` 只有一套 `pte_r/w/x/u/g/a/d/n/v` 字段。
- `fill_dtlb_resp_from_entry()` 用同一套 `entry.pte_*` 同时填充 s1 和 s2 entry 权限字段。
- V2 L2TLB response 中 s1/s2 entry 权限语义需要分开建模，不能长期保持同源字段。
- 因此本 plan 需要新增 `L2TLB_to_do` 项，记录后续将 s1/s2 PTE 权限拆分为两套建模字段和填充链路。

## 4. 修改原因

V2 二阶段权限字段 `g/u` 影响 TLB response 语义。如果 active 接管路径中固定 0：

- L2TLB responder 返回的权限与 `memblock_tlb_entry` 不一致。
- 后续权限/fault 激励会被错误过滤或误判。
- 文档和代码可能再次把当前 agent 误写成下游 PTW/L2Cache 模型。

## 5. 修改后方案

### 5.1 保持请求查表模型

`send_l2tlb_cycle()` 保持：

```text
sample vpn/s2xlate
drain CSR runtime
data.get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)
fill_dtlb_resp_from_entry(entry, resp)
send response
```

### 5.2 权限字段链路检查

执行 coding/review 时必须确认完整链路：

```text
memblock_tlb_entry.pte_g/pte_u
  -> fill_dtlb_resp_from_entry()
  -> L2tlb_agent_agent_xaction.io_ptw_resp_bits_s2_entry_perm_g/u
  -> L2tlb_agent_agent_driver
  -> L2tlb_agent_agent_interface
  -> L2tlb_agent_connect.sv active branch
  -> RTL _inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u
```

若任一环节缺失，按本 plan 补齐。

若完整链路已经存在，执行者只需在 review 或执行记录中写明：

- `fill_dtlb_resp_from_entry()` 当前已由 `entry.pte_g/pte_u` 填充 s1/s2 `perm_g/u`，这只能证明 active 链路不是常量 0，不能证明 s1/s2 权限语义已经分开建模。
- driver/interface/connect active 分支已把 xaction 权限字段传到 RTL internal response。
- 本轮无 SV 代码修改。
- 后续必须按 `L2TLB_to_do` 将 s1/s2 权限来源拆开。

### 5.3 L2TLB_to_do：s1/s2 PTE 权限分开建模

当前最小检查只解决 `s2_entry_perm_g/u` 不应在 active 接管路径常量化的问题。后续需要单独执行 L2TLB 建模增强，目标是让 s1 和 s2 response entry 使用不同的 PTE 权限来源。

待办项：

1. 在 `memblock_tlb_entry` 中将当前单套 `pte_*` 扩展为 s1/s2 两套字段，例如：

   ```text
   s1_pte_r/w/x/u/g/a/d/n/v
   s2_pte_r/w/x/u/g/a/d/n/v
   ```

   或采用等价 packed struct，但字段命名必须能清楚表达 s1/s2 来源。

2. 保留必要的兼容 helper，避免一次性破坏现有 lookup、uid record 回填和 debug dump。若保留旧 `pte_*` 字段，必须明确它只是兼容别名或默认模板，不再作为 s1/s2 权限的唯一真实来源。

3. 更新 TLB entry 构造逻辑，例如 `tlb_map_builder::randomize_pte_bits()` 和 `common_data_transaction::build_tlb_entry_for_key()`，分别生成或派生 s1/s2 PTE 权限。s1/s2 的随机权重可以先复用现有 `MEMBLOCK_TLB_PTE_*_WT`，但后续若需要独立权重，必须另走参数管理规则新增 cfg key。

4. 更新 `fill_dtlb_resp_from_entry()`：

   ```text
   resp.s1_entry_perm_g = entry.s1_pte_g;
   resp.s1_entry_perm_u = entry.s1_pte_u;
   ...
   resp.s2_entry_perm_g = entry.s2_pte_g;
   resp.s2_entry_perm_u = entry.s2_pte_u;
   ...
   ```

5. 更新 `memblock_uid_tlb_record::copy_entry_fields()` 和所有 PTE debug/compare/dump 逻辑，确保 uid record 中保存的权限与 response 实际驱动字段一致。

6. 添加静态检查，禁止 `fill_dtlb_resp_from_entry()` 继续把同一个 `entry.pte_g/u` 同时写到 s1 和 s2 `perm_g/u`。如果需要临时兼容，应在代码注释和 implementation review 中写明过渡原因。

文字伪代码：

```text
build_tlb_entry_for_key():
    entry.s1_pte_* = generate_stage1_pte_bits(key, csr_snapshot, weights)
    entry.s2_pte_* = generate_stage2_pte_bits(key, csr_snapshot, weights)
    entry.legacy_pte_* = derive_debug_default(entry.s1_pte_*, entry.s2_pte_*) // 可选

fill_dtlb_resp_from_entry(entry, resp):
    resp.s1_entry_perm_* = entry.s1_pte_*
    resp.s2_entry_perm_* = entry.s2_pte_*
```

该待办会改变 L2TLB response 建模精度，但不改变 responder 的 request 消费、lookup key、latency、idle-stop 或 connect takeover 方向。它应作为后续 L2TLB coding plan 或本 plan 的后续阶段执行，不应和“active path 是否常量 0”的最小修复混在一次无边界修改中。

### 5.4 inactive 分支语义

`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0` 时保持 inactive 默认 0。该模式不是 passive monitor，不要求观察 DUT 原始 PTW/L2TLB response。

执行时必须同时检查两种分支：

- `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1`：active 接管路径，RTL `_inner_ptw_io_tlb_1_resp_*` 必须由 agent/interface/xaction 字段 force 或 assign，`s2_entry_perm_g/u` 不得常量 0。
- `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`：inactive 路径，只表示不接管并保持默认驱动策略，不代表 passive 捕获真实 PTW response，也不要求把 default 0 当成 active 行为。

## 6. 函数/任务级伪代码

### 6.1 `fill_dtlb_resp_from_entry()`

函数目的：从公共 TLB entry 生成 DTLB response xaction，包含 V2 s1/s2 权限字段。

输入：`memblock_tlb_entry entry`、response xaction。

输出/副作用：

- 写 response xaction 的 valid、s1/s2 entry、pf/af/gpf/gaf。
- 不修改 TLB 表；表项创建由调用者完成。

源码级伪代码：

```text
function void fill_dtlb_resp_from_entry(entry, ref resp);
    if (entry == null || resp == null) fatal;
    resp.io_ptw_resp_valid = 1;
    resp.io_ptw_resp_bits_s2xlate = entry.s2xlate;

    // 当前最小检查阶段：
    resp.s1_entry_perm_d = entry.pte_d;
    resp.s1_entry_perm_a = entry.pte_a;
    resp.s1_entry_perm_g = entry.pte_g;
    resp.s1_entry_perm_u = entry.pte_u;
    resp.s1_entry_perm_x = entry.pte_x;
    resp.s1_entry_perm_w = entry.pte_w;
    resp.s1_entry_perm_r = entry.pte_r;

    resp.s2_entry_perm_d = entry.pte_d;
    resp.s2_entry_perm_a = entry.pte_a;
    resp.s2_entry_perm_g = entry.pte_g;
    resp.s2_entry_perm_u = entry.pte_u;
    resp.s2_entry_perm_x = entry.pte_x;
    resp.s2_entry_perm_w = entry.pte_w;
    resp.s2_entry_perm_r = entry.pte_r;

    // L2TLB_to_do 后续阶段：
    resp.s1_entry_perm_* = entry.s1_pte_*;
    resp.s2_entry_perm_* = entry.s2_pte_*;
endfunction
```

中文文字伪代码：

该函数只负责把已经查到的 TLB entry 搬运成 response。它先检查输入非空，再置 response valid。当前最小检查阶段用 `entry.pte_*` 同时填充 s1/s2 权限字段，只能保证 active path 不再常量化。后续 `L2TLB_to_do` 必须改为 s1 response 使用 `entry.s1_pte_*`，s2 response 使用 `entry.s2_pte_*`。函数仍不重新查表，也不根据 paddr 推导权限，避免改变当前 DTLB/L2TLB responder 模型。

### 6.2 `check_l2tlb_response_permission_chain()`

函数目的：作为 coding/review 静态检查项，确认字段链路没有断。

源码级伪代码：

```text
check:
    rg "io_ptw_resp_bits_s2_entry_perm_g|io_ptw_resp_bits_s2_entry_perm_u" agent tb seq
    require assignment from entry.pte_g/pte_u in sequence
    require driver assignment from xaction to vif
    require connect force from vif to RTL active path
    reject active path constant 0
    require inactive branch constant/default 0 is documented as non-takeover path
```

中文文字伪代码：

该检查不是运行时函数，而是执行本 plan 的固定 review 步骤。执行者用 `rg` 查找 `s2_entry_perm_g/u`，确认 sequence 由 `entry.pte_g/u` 填充，driver 从 xaction 写 interface，connect active 分支从 interface force 到 RTL internal wire。若 active 分支发现常量 0，必须修正；inactive 分支常量 0 可以保留，但必须在检查结论中说明它只对应 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0` 非接管路径。

## 7. 验收标准

1. `L2TLB_agent` 语义仍是 DTLB/L2TLB responder。
2. `s2_entry_perm_g/u` active 接管路径不为常量 0。
3. `fill_dtlb_resp_from_entry()` 当前从 `entry.pte_g/pte_u` 填充 s1/s2 权限字段的事实已在 review 中记录，并明确该实现只是现状，不是最终 s1/s2 分开建模方案。
4. `send_l2tlb_cycle()` 仍使用 req `vpn/s2xlate` 和 runtime CSR 查表。
5. 顶层 `io_l2_tlb_req_*` 未被接成当前 `L2TLB_agent` 接管点。
6. 若当前源码已满足以上链路，本 plan 只执行静态检查和 profile 同步，不改代码。
7. `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` active 分支和 `=0` inactive 分支语义在 review 结论中分别说明。
8. `L2TLB_to_do` 已记录 s1/s2 PTE 权限分开建模的后续 coding 范围、字段拆分方向和风险边界。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent AI_DOC
rg -n "s2_entry_perm_g|s2_entry_perm_u|pte_g|pte_u|io_l2_tlb_req|_inner_dtlbRepeater|_inner_ptw_io_tlb_1" mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

## 9. 与原始/初步 plan 差异说明

初步 plan 将 `s2_entry_perm_g/u` 记录为风险。当前源码看起来已经补齐链路，本文将其转成可执行检查 plan：后续 coding/review 必须验证 entry 到 RTL internal wire 的完整链路，并防止 active 接管路径回退为常量 0。

本轮新增差异：确认当前 s1/s2 权限都来自同一套 `memblock_tlb_entry.pte_*`，这不满足后续精细建模要求。因此本文新增 `L2TLB_to_do`，把 s1/s2 PTE 权限分开建模记录为后续必做项。

## 10. 风险与非目标

风险：

- V2 RTL 重新生成后 `_inner_dtlbRepeater_*` 或 `_inner_ptw_io_tlb_1_*` 名称可能变化，需要先复查 profile。
- 当前 `memblock_tlb_entry.pte_*` 同时驱动 s1/s2 权限，后续权限/fault 定向激励若依赖 s1/s2 差异，会受到建模精度限制。

非目标：

- 不建模 L2Cache/PTW 下游。
- 不观察顶层 `io_l2_tlb_req_*` response。
- 不实现权限 checker。
- 本 plan 当前阶段不直接实现 s1/s2 PTE 权限拆分；拆分已记录为 `L2TLB_to_do`。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`无代码优先检查/复查 + 仅字段/参数适配`，必要时包含 `局部逻辑适配`。默认不改变 L2TLB responder 主逻辑。若源码已保持 `entry.pte_g/pte_u -> xaction -> interface -> connect active path -> RTL internal response` 完整链路，则本 plan 当前阶段只做静态检查和 profile 同步；若链路断开，修复也只属于 response 权限字段适配。新增的 `L2TLB_to_do` 是后续建模增强项，会改变 s1/s2 权限字段来源，但不改变 responder 主 flow。

原测试框架逻辑：

- `memblock_l2tlb_base_sequence::body()` 初始化 runtime 参数，确认 `MEMBLOCK_L2TLB_SEQ_EN` 和 `memblock_sync_pkg::l2tlb_responder_active` 后进入 `drive_l2tlb_loop()`。
- `drive_l2tlb_loop()` 是被动 responder loop，有 request progress 时清 idle count，无 progress 到 `idle_stop_cycle` 退出。
- `send_l2tlb_cycle()` 在 DTLB request valid 时采样 `vpn/s2xlate`，先发送 ready，再 drain CSR runtime，调用 `data.get_or_create_tlb_entry_by_req()` 通过 req `vpn/s2xlate` 和 runtime CSR 查表，最后 `fill_dtlb_resp_from_entry()` 构造 response。
- `L2tlb_agent_connect.sv` 在 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` active 分支接管内部 `dtlbRepeater` 与 `inner_ptw` response 路径。
- 当前 `memblock_tlb_entry` 只有一套 `pte_*` 权限字段，`fill_dtlb_resp_from_entry()` 用同一套 `entry.pte_*` 同时填充 s1 和 s2 entry 权限。

本 plan 修改后逻辑：

- responder 仍是 DTLB -> L2TLB_agent -> DTLB 模型，不接顶层 `io_l2_tlb_req_*`，不建模 L2Cache/PTW 下游。
- request 查表仍使用 req `vpn/s2xlate` 和 runtime CSR snapshot，不使用 paddr。
- 只确认或修复 response 权限字段链路，特别是 `s2_entry_perm_g/u` 不能在 active 分支固定为 0。
- inactive 分支保持默认 0，但仅表示关闭 takeover 后 agent 非激活，不是 passive observation。
- 新增 `L2TLB_to_do`，要求后续将 s1/s2 PTE 权限字段拆分建模，避免长期使用同一套 `entry.pte_*` 同时驱动 s1/s2 response。

逻辑改变项：

- 默认无 responder 主逻辑改变。
- 如果 active 链路缺 `s2_entry_perm_g/u`，需要补齐 `fill_dtlb_resp_from_entry()`、driver/interface/connect 字段赋值。原因是 V2 二阶段权限字段真实存在，active 接管路径应由 TLB entry 驱动。该改变只补 response 字段，不改变查表、latency、idle-stop 或 request 消费。
- 后续执行 `L2TLB_to_do` 时，会有字段来源级的局部逻辑改变：`fill_dtlb_resp_from_entry()` 从单套 `entry.pte_*` 改为分别读取 `entry.s1_pte_*` 和 `entry.s2_pte_*`。该改变提升 response 建模精度，但仍不改变 DTLB request 消费、lookup key 或 responder loop。

字段/参数改变项：

- 字段链路必须覆盖 `io_ptw_resp_bits_s2_entry_perm_g`、`io_ptw_resp_bits_s2_entry_perm_u`，并记录当前 s1/s2 同源 `pte_g/pte_u` 只是现状。
- 后续 `L2TLB_to_do` 需要新增或重命名 s1/s2 两套 PTE 权限字段，例如 `s1_pte_g/u` 和 `s2_pte_g/u`，并同步 uid record、debug dump 和 response 填充逻辑。
- `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 仍是编译期 connect 开关；`MEMBLOCK_L2TLB_SEQ_EN` 仍是 runtime sequence 开关。
- 当前阶段不新增 plus/cfg，不改变 TLB lookup key 字段；后续如需独立 s1/s2 权重，必须另按参数管理规则新增 cfg key。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增扫描；`get_or_create_tlb_entry_by_req()` 和 `update_uid_tlb_records_by_entry()` 的既有行为保持。
- 不改变 L2TLB request/response queue、idle-stop、latency、runtime CSR drain 时机。
- 不改变 TLB entry 生命周期、uid record 回填策略、sfence/hfence 失效策略。
- 不改变 terminal/pass/fail；权限字段只是 DUT response 内容完整性。

覆盖性结论：

本 plan 覆盖 V2 L2TLB permission 字段适配，且明确不覆盖顶层 L2 TLB/PMP response 观察。顶层 output 分类由 monitor output plan 覆盖。结论是：该 flow 是 V2 权限字段链路细节适配，不影响测试框架 L2TLB responder 主体逻辑。

补充结论：当前 plan 已记录 `L2TLB_to_do`，后续必须把 s1/s2 PTE 权限分开建模。该待办不是新增接管点，也不是 L2Cache/PTW 下游模型，而是当前 DTLB/L2TLB responder response 内容的建模精度增强。
