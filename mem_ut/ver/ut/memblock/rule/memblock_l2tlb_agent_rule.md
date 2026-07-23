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
connect 是否接管，也不应在 connect 文件中解析。若编译期覆盖
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`，当前实现表示 `L2TLB_agent` 完全不接管，
agent interface 保持非激活默认值；该模式不是 passive observation 连接，不能依赖它观察
DUT 原始 PTW/L2TLB response。

## sequence 规则

L2TLB responder sequence 的职责是消费 DTLB request 并生成 L2TLB response。

当前 V2 runtime 调度参数为 `MEMBLOCK_L2TLB_MAX_OUTSTANDING`、`MEMBLOCK_L2TLB_RESP_REORDER_EN`、`MEMBLOCK_L2TLB_RESP_MID_LATENCY`、`MEMBLOCK_L2TLB_RESP_LONG_LATENCY`、三个 `MEMBLOCK_L2TLB_RESP_*_WT` 和 `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE`。结构上限和 flush hold 由 compile-time `MEMBLOCK_DUT_L2TLB_DFILTER_SIZE`、`MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES` 提供。旧 `MEMBLOCK_L2TLB_MIN_LATENCY/MAX_LATENCY` 已删除，不得恢复为第二延迟权威。

`MEMBLOCK_L2TLB_SEQ_EN=1` 时，sequence 启动前必须确认
`memblock_sync_pkg::l2tlb_responder_active=1`。如果编译期关闭了
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`，但 runtime 又打开了
`MEMBLOCK_L2TLB_SEQ_EN`，该组合应 fatal，因为 responder sequence 无法把 response
合法送回 DUT。

同一时刻只允许一个 L2TLB lifecycle sequence 实例运行。legacy testcase 的 agent default sequence与
`basicTest + VSEQ_MAIN` 的显式 virtual sequence是两种分别合法的启动拓扑；同一 testcase 不得混用，
也不能各建一份 pending queue 后依赖 sequencer item arbitration 混跑。active sequence 必须在ready
生效前 try-claim package 级 owner；公共 package helper只返回成功状态和当前owner名，UVM fatal由
sequence报告。最终inactive item完成并自然退出后再release。

正常owner交接只允许自然release后由后续实例claim。对持有owner的sequence执行`kill()`、
`stop_sequences()`或phase jump后再在同一仿真启动新owner，不属于当前支持范围；强制终止只能用于
仿真整体结束，不能依赖残留ready/owner自动恢复。

推荐流程：

1. monitor 或 sequence 只在 DTLB -> L2TLB 的 `valid && ready` request fire 边界采集请求。
2. 从 request 中保存 `vpn/s2xlate`，并在该 fire 边界保存 runtime CSR snapshot，例如
   `satp.asid/vsatp.asid/hgatp.vmid`；延迟 response 时不得重新读取 current CSR 替换该 snapshot。
3. 通过 req 的 `s2xlate` 选择有效 `asid/vmid` 字段，并构造 TLB lookup key。
4. 每笔已握手 request 必须有独立 lifecycle record；如果版本 profile 允许多 outstanding，使用有
   compile 上界的 queue 保存全部 request，不能在 driver gap 期间继续 ready 却不采样。
   相同 lookup key 的多次真实 request fire 也必须分别建 record，除非对应版本 profile 明确证明
   该接口允许把已接受 request 合并；当前 V2 明确禁止按 key 合并 token。
   reset/flush 可以按版本合同取消 token 并不返回 response，但必须进入 canceled 记账，不能 silent drop。
5. 查询 `common_data_transaction.sv` 中的 TLB 表并冻结 response item。
   若 TLB entry class 没有完整 UVM field automation，不得把 live table handle 或默认 `copy()` 冒充
   request-time snapshot；必须使用显式逐字段 copy helper，并让 response payload与完成时UID回填同源。
6. 按对应版本 profile 允许的 ordered/reorder 合同选择已到期 request，每拍最多返回一笔。
7. 通过 L2TLB agent driver 返回给 DTLB；只有 response 的真实 sample 边界后才登记完成并更新
   依赖 response 完成的公共记录。

注意：

- 不应假设 DTLB -> L2TLB request 一定携带 `paddr`。
- 若文档或实现中出现“根据采集到的 paddr 查 L2TLB 表”的说法，必须重新确认来源。当前规则下优先使用 DTLB request 的 `vpn/s2xlate` 与 runtime CSR 的 `asid/vmid` 查表。
- 所有基于 CSR 的 L2TLB lookup 必须使用运行时 CSR 镜像，不允许直接使用静态初始配置或 plus/参数快照。
- `seq_csr_common.sv` 只提供 plus 配置和权重，不提供 CSR 运行时真值。
- CSR monitor必须在post-reset sample独立发布non-destructive runtime CSR latest snapshot，不得让该
  发布依赖dispatch semantic raw capture gate；semantic raw路径仍保留原gate。两条latest视图必须共享
  同一snapshot sequence并幂等写入同一runtime CSR state，不能形成两套CSR模型。逐拍payload baseline
  由monitor唯一持有并在每个post-reset sample更新；semantic latest被clear后，统一seq mismatch或
  semantic valid=0必须使下一gate sample重新发布。
- responder取得并应用首份有效runtime CSR snapshot前必须保持request ready为0，且该启动等待不能累计
  idle-stop；不得用未初始化CSR构造lookup key。CSR未就绪路径仍必须处理flush event和global stop，
  不能提前continue导致owner无法退出。
- response 延迟应在 responder queue 中表示为 `due sample/cycle`，不得用会阻塞整个 driver 的
  `pre_pkt_gap/post_pkt_gap` 实现多 outstanding latency。
- `due sample/cycle` 只表示最早可响应边界；ordered head blocking 或单 response 端口竞争允许实际
  completion 更晚，必须检查 completion 不早于 due，不能把 latency 档误写成拥塞下保证完成周期。
- queue 满时通过 request ready 合法反压；ready 是 queue 容量和 reset/flush/stop 状态的派生值，
  不建立同义 runtime ready plus。
- reset、sfence 和 translation CSR changed 必须定义 pending request 的删除或排空规则；不得让 DUT
  已 flush 的旧 request 在长延迟后收到无 owner response。
- flush ready hold 必须以 event 的实际 monitor 观测点到 DTLB filter 清空点的总 pipeline 延迟为准；
  若版本 profile 在 filter 外还有寄存级，不能只使用 filter 内部 `fenceDelay`。
- 多 consumer 观察 flush 时应使用 non-destructive snapshot/event sequence；不得让 L2TLB lifecycle
  owner pop 掉 dispatch/CSR flow 仍需消费的 raw sfence queue。
- ready已经开放后，sequence首次观察到的新flush event必须来自当前sample；迟到event必须在修改
  queue/driving/counter前fatal，不能从当前拍重新锚定并错误取消flush后新request。只有reset/startup且
  ready从未开放时，才允许把较早latest event作为baseline并保守hold完整pipeline延迟。
- idle-stop必须在构造下一cycle item前决定，退出路径必须发送最终`ready=0/resp_valid=0` item；禁止
  发送ready=1后立即退出。
- reset释放或flush ready hold解除后，idle-stop重新计数前必须至少生成一拍合法`ready=1`机会。
  “本次lifecycle block后是否提供过ready机会”必须使用独立状态维护，不能复用
  `acceptance_opened_since_reset`：后者还承担active flush event时间新鲜度判断，flush时不能清零。

## driver / monitor 规则

monitor 应关注 DTLB 发往 L2TLB 的 request fire 和请求字段，至少包括：

- request valid/ready 或等价握手。
- `vpn` 或可推导 VPN 的虚拟地址字段。
- `s2xlate` 或等价翻译阶段字段。
- request 采样时刻的 runtime CSR snapshot 关联信息。

driver 应驱动 L2TLB 返回 DTLB 的 response，不应驱动 L2Cache/PTW 下游接口。

driver/monitor 字段命名、方向和 valid/ready 语义必须以实际 Verilog interface 和 Scala/RTL 接口为准。无法确认时先查 RTL，不允许按 agent 名称猜测端口方向。

当前 active responder 只允许 `DRV_0` idle 基线。`DRV_1` 等 generic pattern mode 会在没有合法 request
lifecycle 时制造 ready/response，driver 必须 fail-fast，不能把它当作 L2TLB responder 压力模式。

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
