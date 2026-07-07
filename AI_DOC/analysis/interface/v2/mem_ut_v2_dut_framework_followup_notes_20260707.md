# mem_ut V2 DUT 接口适配后续测试框架记录

## 1. 记录目的

本文记录本轮 V2 DUT coding 适配中发现但未修改测试激励框架的事项。用户本轮要求只适配 V2 RTL、`dut_inst`、DUT 连接和必要 agent 接口，不改测试激励框架主逻辑。因此本文只作为后续新建测试框架同步 plan 的输入，不代表本轮已经完成行为级适配。

## 2. 本轮已完成的结构闭合

- RTL Verilog 来源已从 `build_memblock/rtl/filelist.f` 切换到整核 `build/rtl/filelist.f`。
- `dut_inst.sv` 已按 `build/rtl/MemBlock.sv` 重新生成，V2 顶层 1334 个端口全部实例连接。
- 除 L2TLB 需要接管内部 DTLB/L2TLB wire 外，`tb/*_agent_connect.sv` 中其他 `RTL_PATH.*` 引用已保证存在于 V2 `MemBlock` 顶层；L2TLB 的 `_inner_dtlbRepeater_*` 和 `_inner_ptw_io_tlb_1_*` 已按当前 `build/rtl/MemBlock.sv` 确认存在。
- 上一轮把 V2 顶层 L2TLB `io_l2_tlb_req_*` 映射到 `L2TLB_agent` responder 的语义错误已纠正；当前默认开启 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 后，接管的是内部 `_inner_dtlbRepeater_*` request 和 `_inner_ptw_io_tlb_1_*` response 交接信号。
- L2TLB driver idle ready 已修正为 active responder 默认可接收 request。
- 未修改 `tc/`、`env/src/memblock_rm.sv`、公共状态表或 testcase 主激励逻辑。

## 3. 后续必须单独分析的测试框架事项

| 类别 | 本轮结构处理 | 后续需要分析的问题 |
|---|---|---|
| `intIssue` 到 `issueLda/Sta/Std` | connect 层把可对应字段映射到 V2 拆分 issue 端口，缺少等价字段的 DUT input 保持 `dut_inst` 默认 0 | 当前 issue sequence 和 transaction 仍按 V3 聚合 `intIssue` 语义生成，需要确认拆分 lane、字段宽度和 `fuType/fuOpType` 语义是否一致。 |
| `vecIssue` 到 `issueVldu` | connect 层把可对应字段映射到 V2 `issueVldu`，非等价字段保持 0 | V2 只有 vector load issue 拆分形态，后续需要确认 vector store/segment 相关激励是否还能通过现有 sequence 表达。 |
| `intWriteback` 到 `writebackLda/Sta/Std` | monitor 侧旧接口保留，connect 层采样 V2 split writeback 的可对应字段，缺失字段置 0 | RM/scoreboard 若依赖 V3 `toRob/toIntRf/toFpRf` 聚合字段，需要建立 V2 writeback adapter，而不是只靠字段置 0。 |
| `vecWriteback` 到 `writebackVldu` | connect 层采样 `writebackVldu` 的可对应字段，缺失字段置 0 | vector writeback transaction 需要重新确认 `vdIdx`、`vls_*`、异常字段和 V2 `uop_*` 字段的含义差异。 |
| L2TLB/PTW | 上一轮顶层 `io_l2_tlb_req_*` 映射语义错误，已改为接 V2 内部 `_inner_dtlbRepeater_io_ptw_req_0_valid`、`_inner_dtlbRepeater_io_ptw_req_0_bits_vpn`、`_inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate` request，并由 agent response 驱动 `_inner_ptw_io_tlb_1_req_0_ready` 和 `_inner_ptw_io_tlb_1_resp_*` | 顶层 `io_l2_tlb_req_*` 是 L2/L2Cache 侧 requestor，不作为 `L2TLB_agent` 接管点。当前接入点是生成后 Verilog internal wire，随 RTL 生成可能变化，后续更新 V2 RTL 时必须复查层级名；此外 V2 内部 response 有 `s2_entry_perm_g/u`，当前 agent 无对应字段，本轮固定为 0。 |
| CSR control | V2 命名为 `*_enable`、`btb_enable`、`ras_enable` 等，connect 层做了明确同名或近义映射，缺失字段置 0 | 需要确认 V3 `abtb/mbtb/ittage` 等字段在 V2 中是否合并或删除，避免测试框架仍认为这些开关可独立控制。 |
| `other_ctrl` | `cpuWfi` 映射到 V2 `cpuHalted`/`io_outer_cpu_halt`，`inner_hartId` 映射到 `topToBackendBypass_hartId` | 后续 flow 文档和检查逻辑需要统一 V2 命名，避免把 halt/WFI 混为同一行为。 |
| TileLink/auto 与 memory model | 本轮只保证 ext memory model filelist 闭合，若未由 agent 驱动的输入默认 0 | 整核带入更多 SoC/TileLink 边界，后续若要跑真实事务，需要专门确认外部总线 ready/valid 默认值是否足够。 |

## 4. 后续 plan 建议

1. 新建 V2 测试框架行为适配 plan，先从 `issue`、`writeback`、`L2TLB` 三条主链路分析。
2. 对每个 agent 建立 V2 adapter 表：旧 transaction 字段、V2 RTL 字段、是否可直接映射、是否需要默认值、是否影响 RM。
3. 对所有置 0 字段按功能分类：删除字段、V2 无等价字段、后续应新增 transaction 字段、后续应新增 sequence 驱动字段。
4. 后续更新 V2 RTL 时复查 L2TLB internal 层级：当前接入 `_inner_dtlbRepeater_*` 和 `_inner_ptw_io_tlb_1_*`，若生成名变化，需要同步更新 connect、profile 和 review 文档。
5. 优先跑最小 `tc_sanity` 仿真，若 runtime 因 handshake、response 字段语义或 sequence 等待卡住，再针对对应 agent 做行为级适配。
