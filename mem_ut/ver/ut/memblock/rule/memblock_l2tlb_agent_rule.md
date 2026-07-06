# memblock L2TLB agent 规则

## 触发条件

当任务涉及 `mem_ut` 中任何 L2TLB 相关内容时，必须先阅读本规则。范围包括但不限于：

- 新增、修改或重新连接 `L2TLB_agent` / `L2tlb_agent`。
- 修改 L2TLB 相关 `interface`、`connect.sv`、`top_tb.sv` 连接或 virtual interface 配置。
- 修改 L2TLB agent 内部 `driver`、`monitor`、`sequencer`、`transaction/xaction`、`default_sequence` 或 cfg。
- 新增或修改 L2TLB responder sequence，例如 `memblock_l2tlb_base_sequence.sv`。
- 修改 L2TLB 相关 plus 参数、TLB lookup API、DTLB request 采集、L2TLB response 回填逻辑。
- 修改与 L2TLB 相关的 plan、设计文档或规则文档。

如果任务同时涉及 agent 结构、sequence、DUT 接口适配或 cfg，还必须继续阅读对应规则：

- `mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md`

不同 XiangShan 版本的 L2TLB/DTLB 连接层级和字段可能不同。修改前必须按当前分支或用户指定版本读取对应 profile：

```text
mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
mem_ut/ver/ut/memblock/rule/version/v3/l2tlb_interface_profile.md
```

## 核心语义边界

`L2TLB_agent` 在当前 mem_ut 环境中代替的是 **L2TLB 对上游 DTLB 的功能**，连接点是 **DTLB 与 L2TLB 的 request/response 交互处**。

因此，`L2TLB_agent` 的职责是：

- 接收或采集上游 DTLB 发往 L2TLB 的翻译请求。
- 从 DTLB request 中获取 `vpn`、`s2xlate` 等请求侧字段。
- 从运行时 CSR 镜像获取当前请求需要的 `asid`、`vmid` 等翻译上下文。
- 查询 `common_data_transaction.sv` 中的 TLB 表或 lookup 索引。
- 构造 L2TLB 返回给 DTLB 的 response transaction。
- 通过 driver 驱动 **L2TLB -> DTLB** 的 response 接口。

`L2TLB_agent` 不是用来建模 L2TLB 下游访问路径的组件，禁止把它写成以下语义：

- 代替 L2Cache。
- 建模 L2TLB 与 L2Cache 的交互。
- 建模 PTW/page table walk 到 memory/cache 的完整访问过程。
- 根据 L2Cache 侧 `paddr` 请求去返回 DTLB response。

如果需要建模 L2TLB 到 PTW、L2Cache 或 memory 的下游路径，必须另立方案和 agent/interface 语义，不能混入当前 `L2TLB_agent`。

## 接口连接规则

修改 L2TLB interface 或 connect 时，必须先确认当前连接点是否是 DTLB 与 L2TLB 之间的交互接口。

正确方向：

- request 方向：DTLB -> L2TLB_agent。
- response 方向：L2TLB_agent -> DTLB。

错误方向：

- L2TLB_agent -> L2Cache。
- L2Cache/PTW -> L2TLB_agent。
- 把 L2TLB_agent 连接到 dcache error、memory info、L2Cache refill 等无关端口。

如果现有 `L2tlb_agent_connect.sv` 是占位、误连或连接语义无法确认，不允许直接启用。必须先追 RTL 层级、interface 信号方向和上游 DTLB 端口，再修改 connect。

当前 mem_ut 的 connect 接管由编译期宏
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，统一定义在：

```text
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
```

该宏默认值为 1，表示默认由 `L2TLB_agent` 接管 DTLB/L2TLB response 通路。
runtime `MEMBLOCK_L2TLB_SEQ_EN` 只表示 responder sequence 是否运行，不再控制
connect 是否接管，也不应在 connect 文件中解析。若某个调试场景需要保留 DUT
原始 PTW/L2TLB response，只观察不接管，应在编译期覆盖
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`。

## sequence 规则

L2TLB responder sequence 的职责是消费 DTLB request 并生成 L2TLB response。

`MEMBLOCK_L2TLB_SEQ_EN=1` 时，sequence 启动前必须确认
`memblock_sync_pkg::l2tlb_responder_active=1`。如果编译期关闭了
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`，但 runtime 又打开了
`MEMBLOCK_L2TLB_SEQ_EN`，该组合应 fatal，因为 responder sequence 无法把 response
合法送回 DUT。

推荐流程：

1. monitor 或 sequence 采集 DTLB -> L2TLB request。
2. 从 request 中保存 `vpn/s2xlate`，并在 request 采样时刻保存 runtime CSR snapshot，例如 `satp.asid/vsatp.asid/hgatp.vmid`。
3. 通过 req 的 `s2xlate` 选择有效 `asid/vmid` 字段，并构造 TLB lookup key。
4. 查询 `common_data_transaction.sv` 中的 TLB 表。
5. 构造 response item。
6. 通过 L2TLB agent driver 返回给 DTLB。

注意：

- 不应假设 DTLB -> L2TLB request 一定携带 `paddr`。
- 若文档或实现中出现“根据采集到的 paddr 查 L2TLB 表”的说法，必须重新确认来源。当前规则下优先使用 DTLB request 的 `vpn/s2xlate` 与 runtime CSR 的 `asid/vmid` 查表。
- 所有基于 CSR 的 L2TLB lookup 必须使用运行时 CSR 镜像，不允许直接使用静态初始配置或 plus/参数快照。
- `seq_csr_common.sv` 只提供 plus 配置和权重，不提供 CSR 运行时真值。

## driver / monitor 规则

monitor 应关注 DTLB 发往 L2TLB 的 request fire 和请求字段，至少包括：

- request valid/ready 或等价握手。
- `vpn` 或可推导 VPN 的虚拟地址字段。
- `s2xlate` 或等价翻译阶段字段。
- request 采样时刻的 runtime CSR snapshot 关联信息。

driver 应驱动 L2TLB 返回 DTLB 的 response，不应驱动 L2Cache/PTW 下游接口。

driver/monitor 字段命名、方向和 valid/ready 语义必须以实际 Verilog interface 和 Scala/RTL 接口为准。无法确认时先查 RTL，不允许按 agent 名称猜测端口方向。

## common data / TLB lookup 规则

L2TLB response 查表必须与公共 TLB 表方案保持一致：

- lookup key 使用 req 的 `vpn/s2xlate` 与 request 采样时刻的 runtime CSR 生成。
- `s2xlate` 直接来自 DTLB -> L2TLB req，不在建表路径中从 opcode 重新推导。
- `asid/vmid` 是否进入 key 由 req 的 `s2xlate` 决定，无效字段必须归零。
- TLB 主存储使用 `tlb_entry_by_key[key]`，不再按 `uid` 建 `tlb_table_by_uid[]`。
- `uid` 只用于 `uid_tlb_record_by_uid[]` 追踪发射时上下文和后续 PTE 回填，不建立 `key -> uid` 强绑定。
- uid 发射后预登记 record；L2TLB req 建表或命中 `tlb_entry_by_key` 后，按 key 回填所有 `pte_valid=0` 的匹配 uid record。
- CSR runtime 变化不能通过 `csr_update_seq` 粗暴清表或拒绝命中；失效必须由 `sfence/hfence` entry 级逻辑完成。
- 查不到表项时，按当前最终方案由 `get_or_create_tlb_entry_by_req()` 自动创建新映射；禁止回退到初始 CSR 值。

## 文档同步规则

修改任何 L2TLB agent、interface、sequence、driver、monitor 或 lookup 行为后，必须同步检查并更新相关文档：

- `AI_DOC/plan/test_framework/plan/do/l2tlb_base_seq_plan_20260614.md`
- `AI_DOC/plan/test_framework/review_doc/undo/dispatch_plan_v2_review_annotated.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_framework_design_20260614.md`
- `AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md`
- `AI_DOC/project_management/mem_ut_parameter_management.md`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`
- 本规则文件

不得出现代码语义已经改为 DTLB/L2TLB 上游响应模型，但文档仍描述为 L2TLB/L2Cache 或 paddr 下游访问模型的情况。

## 完成标准

完成 L2TLB 相关修改后，至少执行以下静态检查：

```bash
rg -n "L2TLB|L2tlb|l2tlb|DTLB|dtlb" mem_ut/ver/ut/memblock AI_DOC mem_ut/ver/ut/memblock/rule
```

检查结果必须确认：

- L2TLB_agent 语义仍是 L2TLB 对上游 DTLB 的 responder。
- interface 连接方向未被误写为 L2TLB/L2Cache 下游交互。
- sequence 不按 L2Cache paddr 请求模型工作。
- driver/monitor 字段与真实 DTLB/L2TLB 接口一致。

如修改影响编译，应按项目远端编译仿真规则从 `mem_ut/ver/ut/memblock/sim` 使用 `eda01` flow 验证。
