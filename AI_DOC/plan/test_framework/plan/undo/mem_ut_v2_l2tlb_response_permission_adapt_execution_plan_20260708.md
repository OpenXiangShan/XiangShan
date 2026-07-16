# mem_ut V2 L2TLB Response Permission 适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding / 静态核对 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` 与 `mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md` |
| 测试框架入口 | `memblock_l2tlb_base_sequence::send_l2tlb_cycle()` |
| 适配原则 | 保持 `DTLB -> L2TLB_agent -> DTLB` responder 模型，只检查或修复 V2 response permission 字段链 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只处理 V2 L2TLB responder response 权限字段链路，重点是确保 active takeover 路径中的
`_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 由 TLB entry 真实驱动，而不是常量 0 或断链。

涉及文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv
mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv
```

本轮允许的结果只有两类：

1. 如果当前链路完整，输出静态核对结论，不修改 SV 或外部 TODO/flow 文档。
2. 如果 active path 中 `s2_entry_perm_g/u` 缺字段、缺搬运或被常量化，只补齐该字段链路。

本轮不实现：

- L2Cache、PTW 或 memory 下游模型。
- 顶层 `io_l2_tlb_req_*` 的观察或接管。
- 用 paddr 查表替代 req `vpn/s2xlate` 与 runtime CSR。
- s1/s2 两套 PTE 权限字段拆分。
- stage2 legal leaf、GAF/GPF 派生或 directed fault 策略。
- acceptance gate、strict count、driver ready、response cadence、stopping/reset 策略的新增 owner；这些若与其它 CSR runtime semantic plan 合并，只能按对应 plan 保留，不归本 permission plan 所有。

执行前必须确认：

```bash
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

V2 权威 RTL 以 `build_memblock/rtl` 为准，不能沿用旧 worktree 或 `MemBlockTop.sv`。

## 2. 问题一：`s2_entry_perm_g/u` active 字段链可能断裂或回退常量

### V2 问题

V2 profile 已确认当前 `L2TLB_agent` 接管点是 `dtlbRepeater` 与 `inner_ptw` 之间的内部 request/response
路径。response 中 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 是真实权限字段。如果 active takeover
路径中这些字段被固定为 0，或者只在某一层声明但没有从 entry 搬运到 RTL wire，DUT 收到的 response
权限会与公共 TLB entry 不一致。

### 修改原因

`s2_entry_perm_g/u` 属于 response payload，不是 inactive 默认值。active 接管时必须来自
`memblock_tlb_entry.pte_g/pte_u`，否则权限/fault 激励会被错误过滤或误判，也容易把当前 agent 误写成
无权限语义的占位驱动。

### 修改方案与修改逻辑

需要核对或修复以下完整链路：

```text
memblock_tlb_entry.pte_g/pte_u
  -> memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry()
  -> L2tlb_agent_agent_xaction.io_ptw_resp_bits_s2_entry_perm_g/u
  -> L2tlb_agent_agent_driver::send_pkt()
  -> L2tlb_agent_agent_interface
  -> L2tlb_agent_connect.sv active branch
  -> RTL _inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u
```

如果链路已经完整，本 plan 不产生源码 diff。如果链路断裂，只在断裂层补齐 `g/u` 字段声明、赋值或
active connect 搬运。inactive branch 继续保持默认 0，但必须明确它表示
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0` 的非接管路径，不是 passive observation。

### 文字伪代码

```text
执行静态链路检查：
  读取 V2 profile，确认接管点是 dtlbRepeater 与 inner_ptw 的内部路径；
  读取 build_memblock/rtl/MemBlock.sv，确认 _inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u 存在；
  检查 fill_dtlb_resp_from_entry：
    必须把 entry.pte_g 写入 response 的 s2_entry_perm_g；
    必须把 entry.pte_u 写入 response 的 s2_entry_perm_u；
  检查 xaction/interface：
    必须声明 s2_entry_perm_g/u 字段，位宽为 1；
    clear 或 idle 默认可以清 0，但不能替代有效 response payload；
  检查 driver：
    send_pkt 必须从 xaction 搬运 s2_entry_perm_g/u 到 vif；
  检查 connect：
    takeover=1 active branch 必须从 interface force/assign 到 RTL internal wire；
    active branch 禁止把 g/u 写成常量 0；
    takeover=0 inactive branch 可保持默认 0，并在 review 中说明不是被动观察模式；
  如果任一 active 层断链：
    只修复该层的 g/u 搬运；
  如果链路完整：
    不修改 SV。
```

## 3. 问题二：L2TLB responder 语义容易被误接到顶层 `io_l2_tlb_req_*`

### V2 问题

V2 `build_memblock/rtl/MemBlock.sv` 暴露了顶层 `io_l2_tlb_req_*` 和 `io_l2_pmp_resp_*`，但这些顶层端口
不是当前 mem_ut `L2TLB_agent` 的接管点。若执行 permission 修复时误把 agent 接到这些顶层端口，就会
把 DTLB/L2TLB responder 写成 L2/L2Cache 侧 requestor 或下游 PTW/L2Cache 模型。

### 修改原因

项目规则要求 `L2TLB_agent` 建模的是上游 DTLB 发往 L2TLB 的 request，以及 L2TLB 返回 DTLB 的
response。request 查表必须使用 DTLB request 的 `vpn/s2xlate` 和 request 采样时刻的 runtime CSR
snapshot，不使用 paddr，也不观察 L2Cache/PTW 下游路径。

### 修改方案与修改逻辑

本 plan 固定以下语义边界：

- request 来源：`_inner_dtlbRepeater_io_ptw_req_0_valid`、
  `_inner_dtlbRepeater_io_ptw_req_0_bits_vpn`、
  `_inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate`。
- response 目标：`_inner_ptw_io_tlb_1_req_0_ready` 和 `_inner_ptw_io_tlb_1_resp_*`。
- lookup key：req `vpn/s2xlate` 加 runtime CSR 中的 `asid/vmid` 等上下文。
- response payload：由 `data.get_or_create_tlb_entry_by_req()` 返回的 TLB entry 填充。
- 顶层 `io_l2_tlb_req_*` 只作为当前 V2 顶层端口存在性背景，不作为本 agent 接管点。

### 文字伪代码

```text
send_l2tlb_cycle()：
  如果没有真实 DTLB -> L2TLB request fire：
    不查表，不构造 response，不修改 permission 字段；
    返回；

  如果 request fire：
    采样 req.vpn 和 req.s2xlate；
    采样 runtime CSR snapshot；
    根据 req.s2xlate 选择有效 asid/vmid 字段；
    调用 data.get_or_create_tlb_entry_by_req：
      该 helper 使用 vpn/s2xlate 和 runtime CSR 构造 lookup key；
      命中时返回已有 tlb_entry_by_key；
      未命中时创建新映射并返回 entry；
    调用 fill_dtlb_resp_from_entry：
      把 entry 的 PPN、pf/af/gpf 和权限字段搬运到 response xaction；
      其中 s2_entry_perm_g/u 来自 entry.pte_g/u；
    通过 L2TLB agent driver 发送 response 到 DTLB；

  整个流程不读取顶层 io_l2_tlb_req_* 作为 responder request；
  不使用 paddr 查表；
  不建模 L2Cache/PTW/memory 下游访问。
```

## 4. 问题三：s1/s2 PTE 权限尚未分开，不能用局部 fixup 伪装完整 stage2 建模

### V2 问题

当前 `memblock_tlb_entry` 只有一套 `pte_r/w/x/u/g/a/d/n/v` 字段，
`fill_dtlb_resp_from_entry()` 使用同一套 `entry.pte_*` 同时填充 s1 和 s2 entry 权限。这足以保证
active path 的 `s2_entry_perm_g/u` 不是常量 0，但不能证明 s1/s2 权限语义已经独立建模。

### 修改原因

如果为了让当前 smoke 或某个 directed 场景看起来更自洽，就在本 plan 中强制改写共享
`pte_u/a/v/r/w`、清 `tlbGPF` 或伪造 GAF/GPF，会同时改变 s1 和 s2 权限来源，反而破坏当前最小
permission 字段适配边界。

### 修改方案与修改逻辑

本轮保持当前共享 PTE 现状：

- `s1_entry_perm_*` 继续来自 `entry.pte_*`。
- `s2_entry_perm_*` 继续来自 `entry.pte_*`。
- `s2_gpf` 继续来自 `entry.tlbGPF`。
- `s2_gaf` 继续明确为 0，表示当前不覆盖 GAF，不表示已完成 GAF 派生。

同时明确禁止本轮新增：

- `fixup_legal_stage2_smoke_profile()` 或等价 helper。
- 为 stage2 合法性强制赋值共享 `pte_u/a/v/r/w` 的逻辑。
- 清除或重派生 `entry.tlbGPF` 的逻辑。
- directed GPF/GAF 的临时 fatal 分支。
- 外部 TODO、flow 文档或规则文档修改。

后续若要支持 s1/s2 权限差异，必须另建专项，把 `memblock_tlb_entry` 扩展为 s1/s2 两套字段，并同步
builder、uid record、sfence/hfence global 判断、debug dump 和 response fill 链路。

### 文字伪代码

```text
fill_dtlb_resp_from_entry(entry, resp) 当前阶段：
  如果 entry 或 resp 为空，uvm_fatal；
  设置 resp valid 和基础 response 字段；
  用 entry.pte_d/a/g/u/x/w/r 填充 s1_entry_perm_*；
  用同一套 entry.pte_d/a/g/u/x/w/r 填充 s2_entry_perm_*；
  设置 resp.s2_gpf = entry.tlbGPF；
  设置 resp.s2_gaf = 0；
  不修改 entry；
  不修改 TLB table；
  不创建 stage2 legal-leaf fixup；

deferred s1/s2 PTE 拆分专项：
  在 memblock_tlb_entry 中新增 s1_pte_* 和 s2_pte_*；
  build_tlb_entry_for_key 分别生成或派生两套权限；
  fill_dtlb_resp_from_entry 从 s1_pte_* 填 s1 response；
  fill_dtlb_resp_from_entry 从 s2_pte_* 填 s2 response；
  sfence_match_entry 的 global 判断读取 s1_pte_g；
  uid record 和 debug dump 保存实际 response 使用的字段；
  GPF/GAF 派生与 legal-leaf 构造一起实现；
  该专项不属于本 plan 当前 coding。
```

## 5. 修改方案总结

本 plan 修改或核对的是 V2 L2TLB response permission 字段链，不改变 responder 主流程。

修改前风险：

```text
只检查某一层字段时，可能漏掉 sequence -> xaction -> driver -> interface -> active connect 的断链；
active branch 如果回退为常量 0，会让 s2_entry_perm_g/u 与 TLB entry 不一致；
顶层 io_l2_tlb_req_* 容易被误认为当前 L2TLB_agent 接管点；
共享 pte_* 同时填 s1/s2 的限制容易被误当成已完成两阶段权限建模。
```

修改后方案：

```text
先执行 V2 profile 和 RTL internal wire 核对；
逐层检查 s2_entry_perm_g/u 从 entry.pte_g/u 到 RTL active wire 的搬运；
链路完整时不改 SV；
断链时只修复断链层的 g/u 字段；
request/response 语义仍固定为 DTLB <-> L2TLB responder；
共享 pte_* 的 s1/s2 建模限制保留为后续专项，不在本轮局部 fixup。
```

## 6. 验证方案

静态检查：

```bash
rg -n "s2_entry_perm_g|s2_entry_perm_u|pte_g|pte_u|io_l2_tlb_req|_inner_dtlbRepeater|_inner_ptw_io_tlb_1" \
  mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv \
  mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv \
  mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent \
  mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md \
  build_memblock/rtl/MemBlock.sv

git diff --check -- \
  mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv \
  mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv \
  mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent \
  AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md
```

coding 后如产生 SV diff，再执行远端验证：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

验收要求：

- `L2TLB_agent` 仍是 DTLB/L2TLB responder。
- active takeover 路径中 `s2_entry_perm_g/u` 不为常量 0。
- `send_l2tlb_cycle()` 仍使用 req `vpn/s2xlate` 与 runtime CSR 查表。
- 顶层 `io_l2_tlb_req_*` 未被接成当前 agent 接管点。
- 本轮 diff 不包含 stage2 PTE fixup、`tlbGPF` 改写或外部 TODO/flow 文档修改。

## 7. 风险与未解决项

- V2 RTL 重新生成后，`_inner_dtlbRepeater_*` 或 `_inner_ptw_io_tlb_1_*` 名称可能变化，必须先复查
  `build_memblock/rtl/MemBlock.sv` 和 V2 profile。
- 当前 `entry.pte_*` 同时驱动 s1/s2 权限，无法覆盖 s1/s2 权限差异；该建模精度问题需要后续专项。
- 若本 permission patch 与 CSR runtime semantic plan 修改同一 sequence/driver 文件，必须在合并 review
  中确认 gate、ready、count、cadence、stopping 等 CSR contract 没被覆盖；这些状态不属于本 plan 所有。
