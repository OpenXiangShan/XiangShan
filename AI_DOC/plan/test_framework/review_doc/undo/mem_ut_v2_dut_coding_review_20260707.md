# mem_ut V2 DUT coding review

## 1. Review 范围

本文复查本轮 V2 DUT coding 适配。适配基准为用户指定的整核 V2 Verilog：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/build/rtl/MemBlock.sv
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/build/rtl/filelist.f
```

本轮修改范围集中在：

```text
mem_ut/ver/ut/memblock/cfg/rtl.f
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv
AI_DOC/analysis/interface/v2/mem_ut_v2_dut_framework_followup_notes_20260707.md
```

本轮未修改 `tc/`、`env/src/memblock_rm.sv`、公共状态表、testcase 主激励逻辑。

## 2. 修改前逻辑

修改前 `rtl.f` 使用：

```text
-F $MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f
```

这会继续读取独立 memblock RTL，而不是用户指定的整核 `build/rtl`。修改前 `dut_inst.sv` 是 V3 形态，实例端口约 1393 个，其中 648 个端口不存在于 V2 整核 `MemBlock`，直接切到 V2 整核 RTL 会出现 DUT 端口不存在错误。

修改前 `memblock_connect.sv` 展开的多个 connect 文件仍引用 V3 聚合接口或旧 internal 层级，例如 `io_ooo_to_mem_intIssue_*`、`io_ooo_to_mem_vecIssue_*`、`io_mem_to_ooo_intWriteback_*`、`io_mem_to_ooo_vecWriteback_*`、旧 L2TLB/PTW `_inner_*` 路径。这些引用在 V2 整核顶层不存在。

## 3. 修改后逻辑

### 3.1 RTL 来源切换

`rtl.f` 已切换到：

```text
-F $MEMBLOCK_XS_HOME/build/rtl/filelist.f
```

同时补入 `build/rtl/*_ext.v` 生成 memory model。原因是整核 `build/rtl/filelist.f` 中包含 `array_*.sv` wrapper，但第一轮 VCS 编译报 `array_7_ext` 到 `array_12_ext` 等 cell 找不到；这些 `*_ext.v` 文件实际存在于 `build/rtl`，需要显式加入 UVM 编译 filelist。

### 3.2 `dut_inst.sv` 全量重建

`dut_inst.sv` 已按 V2 `build/rtl/MemBlock.sv` 模块头重新生成：

- V2 顶层端口数：1334。
- `dut_inst.sv` 实例连接端口数：1334。
- 缺失端口：0。
- 多余端口：0。
- `clock` 接 `clk`。
- `reset` 接 `~tc_if.rst_n`。
- 其他 DUT input 在 `initial` 中默认置 0，agent connect 宏按需 force 关键驱动端口。
- `io_dft_reset_lgc_rst_n` 默认置 1，避免 DFT reset 默认 0 把整核长期压在逻辑 reset 状态。
- 两条整核 TileLink A ready 边界默认置 1，避免完全无 agent 驱动时输出侧长期 backpressure。

### 3.3 connect 层 V2 映射

除 L2TLB 需要接管内部 DTLB/L2TLB wire 外，其他 `tb/*_agent_connect.sv` 已清理到只引用 V2 `MemBlock` 顶层存在的端口。主要映射如下：

| 旧连接类别 | V2 连接策略 |
|---|---|
| `io_ooo_to_mem_intIssue_0/1/2` | 映射到 `io_ooo_to_mem_issueLda_0/1/2` 的可对应字段。 |
| `io_ooo_to_mem_intIssue_3/4` | 映射到 `io_ooo_to_mem_issueSta_0/1` 的可对应字段。 |
| `io_ooo_to_mem_intIssue_5/6` | 映射到 `io_ooo_to_mem_issueStd_0/1` 的可对应字段。 |
| `io_ooo_to_mem_vecIssue_0/1` | 映射到 `io_ooo_to_mem_issueVldu_0/1` 的可对应字段。 |
| `io_mem_to_ooo_intWriteback_0/1/2` | 从 `io_mem_to_ooo_writebackLda_0/1/2` 采样可对应字段。 |
| `io_mem_to_ooo_intWriteback_3/4` | 从 `io_mem_to_ooo_writebackSta_0/1` 采样可对应字段。 |
| `io_mem_to_ooo_intWriteback_5/6` | 从 `io_mem_to_ooo_writebackStd_0/1` 采样可对应字段。 |
| `io_mem_to_ooo_vecWriteback_0/1` | 从 `io_mem_to_ooo_writebackVldu_0/1` 采样可对应字段。 |
| `cpuWfi` / `io_outer_cpu_wfi` | 映射到 V2 `cpuHalted` / `io_outer_cpu_halt`。 |
| CSR `*Enable` 字段 | 映射到 V2 `*_enable` 风格字段；V2 无等价字段置 0。 |
| 旧 L2TLB/PTW internal 层级 | 上一轮改到 V2 顶层 `io_l2_tlb_req_*` 的映射语义错误；本轮已改为接 V2 内部 `_inner_dtlbRepeater_*` request 和 `_inner_ptw_io_tlb_1_*` response 交接信号。 |

### 3.4 L2TLB 内部 responder 映射纠正

上一轮把 `L2TLB_agent` 接到 V2 顶层 `io_l2_tlb_req_*` 是错误语义。该顶层端口族连接到 V2 `MemBlock` 内部 TLB/L2TLB 的 `io_requestor_1_*`，真实含义是 L2/L2Cache 侧向 MemBlock 内部 TLB/L2TLB 做地址查询的 requestor 口，不是内部 DTLB 发到 L2TLB/PTW 的交接通路。

本轮已把 `L2TLB_agent` 接回 V2 RTL 内部 DTLB/PTW filter/repeater 与 L2TLB 的交接信号：

- `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 默认值仍为 1，表示 V2 默认由 `L2TLB_agent` 接管内部 dtlbRepeater <-> inner_ptw/L2TLB 通路。
- `l2tlb_active` 由 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，并同步写入 `memblock_sync_pkg::l2tlb_responder_active`，供 runtime responder sequence 判断当前 connect 是否真实接管。
- request 方向从 `RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_valid`、`RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_vpn`、`RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate` 采样到 agent interface。
- response 方向由 agent driver/interface 驱动 `RTL_PATH._inner_ptw_io_tlb_1_req_0_ready` 和 `RTL_PATH._inner_ptw_io_tlb_1_resp_*` 完整 response bundle。
- 当前 agent interface 没有 V2 内部 response 中的 `s2_entry_perm_g`、`s2_entry_perm_u` 字段，本轮按现有 agent 能力在 connect 层固定为 0；若后续需要覆盖这两个权限位，应专项扩展 interface、xaction、driver 和 sequence。
- active=0 时，agent request/response interface 保持非激活置 0，不驱动 DUT 内部 response，避免误声明接管。

### 3.5 L2TLB driver ready 修正

修改前 `L2tlb_agent_agent_driver::drive_idle()` 在 `memblock_sync_pkg::l2tlb_responder_active=1` 时把 `io_ptw_req_0_ready` 置 0。这会导致默认接管后，agent interface 一直不表示可接收 request，`request_fire()` 语义也无法成立。

修改后 `DRV_0` idle 分支在 responder active 时保持 `io_ptw_req_0_ready=1`，未接管时置 0。该修改只影响 L2TLB agent driver，不改变通用测试激励框架主逻辑。

### 3.6 L2TLB 映射源码落点

源码位置：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`，参数定义：`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`。
该参数是 L2TLB connect-time takeover 开关，也就是编译展开 connect 宏时决定是否由 `L2TLB_agent` 接管 V2 内部 dtlbRepeater <-> inner_ptw/L2TLB 通路。

```systemverilog
// L2TLB connect-time takeover switch.
// 1: mem_ut L2TLB_agent owns the DTLB <-> L2TLB response path.
//    V2 takes over the internal dtlbRepeater <-> inner_ptw/L2TLB path by default.
// 0: keep L2TLB_agent inactive; this mode is not a passive observation connection.
`ifndef MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN
    `define MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN 1
`endif
```

中文伪代码：该逻辑在当前 V2 DUT 适配中承担“默认开启内部 DTLB/L2TLB responder 接管”的功能。编译时如果外部没有覆盖 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`，就把它定义为 1；后续 `L2tlb_agent_connect.sv` 读取该宏，并据此设置 `l2tlb_active` 和 `memblock_sync_pkg::l2tlb_responder_active`。如果调试场景显式覆盖为 0，connect 层进入非接管分支，L2TLB sequence 启动时会根据 active 状态判断是否 fatal。

源码位置：`mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`，宏：`MEMBLOCK__L2TLB_AGENT_CONNECT`。
该宏是 testbench 和 V2 DUT 内部 DTLB/L2TLB 通路的实际交接点，负责把 V2 内部 dtlbRepeater request 采样到 agent interface，并把 agent response force 回内部 inner_ptw/L2TLB response wire。

```systemverilog
U_IF_NAME``_l2tlb_active = (`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN != 0);
memblock_sync_pkg::l2tlb_responder_active = U_IF_NAME``_l2tlb_active;
if(U_IF_NAME``_l2tlb_active) begin
    force U_IF_NAME.io_ptw_req_0_valid =
        RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_valid;
    force U_IF_NAME.io_ptw_req_0_bits_vpn =
        RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_vpn;
    force U_IF_NAME.io_ptw_req_0_bits_s2xlate =
        RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate;
    force RTL_PATH._inner_ptw_io_tlb_1_req_0_ready =
        U_IF_NAME.io_ptw_req_0_ready;
    force RTL_PATH._inner_ptw_io_tlb_1_resp_valid =
        U_IF_NAME.io_ptw_resp_valid;
    force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2xlate =
        U_IF_NAME.io_ptw_resp_bits_s2xlate;
    force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_tag =
        U_IF_NAME.io_ptw_resp_bits_s1_entry_tag;
    force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_ppn =
        U_IF_NAME.io_ptw_resp_bits_s1_entry_ppn;
    force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pf =
        U_IF_NAME.io_ptw_resp_bits_s1_pf;
    force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_gpf =
        U_IF_NAME.io_ptw_resp_bits_s2_gpf;
end
```

中文伪代码：该逻辑首先根据编译期宏判断当前是否接管 V2 内部 DTLB/L2TLB responder，并把判断结果同步给 `memblock_sync_pkg::l2tlb_responder_active`，供运行期 sequence 判断 response 是否能合法送回 DUT。active 为 1 时，connect 层从内部 `_inner_dtlbRepeater_io_ptw_req_0_*` 采样 request 的 valid、VPN 和 `s2xlate`，这三个字段是 DTLB/PTW filter/repeater 发往 inner_ptw/L2TLB 的请求侧真源。response 方向由 agent interface 的 ready、valid 和 PTE response 字段驱动内部 `_inner_ptw_io_tlb_1_*` wire，等价于由 `L2TLB_agent` 替代 inner_ptw/L2TLB 对 DTLB request 作响应。active 为 0 时不驱动 DUT 内部 response wire，agent interface 只保持非激活默认值，避免 debug-only 场景被误认为已经接管。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，task：`L2tlb_agent_agent_driver::drive_idle()`。
该 task 是 L2TLB driver 空闲周期默认驱动逻辑，本轮只修正 active responder 下的 ready 默认值。

```systemverilog
if(drv_mode==tcnt_dec_base::DRV_0) begin
    vif.drv_mp.drv_cb.io_ptw_req_0_ready <=
        memblock_sync_pkg::l2tlb_responder_active ? '1 : '0;
    vif.drv_mp.drv_cb.io_ptw_resp_valid <= '0;
end
```

中文伪代码：该逻辑在 driver 空闲周期维持 L2TLB agent interface 的默认握手状态。若 connect 层已经接管 V2 内部 dtlbRepeater/inner_ptw responder，就把 request ready 保持为 1，让 sequence 侧看到 agent 可以接收内部 DTLB request；同时 response valid 保持 0，避免空闲周期误发 response。若 connect 层没有接管，则 ready 保持 0，使运行期 sequence 的 active 检查能够暴露错误组合，而不是静默消费 request。

## 4. 正确性检查

### 4.1 端口闭合

本轮使用脚本对比 `build/rtl/MemBlock.sv` 与 `tb/dut_inst.sv`：

```text
dut missing 0
extra 0
inst 1334
ports 1334
```

这说明 `dut_inst.sv` 对 V2 整核顶层没有遗漏端口，也没有 V3 残留实例端口。

### 4.2 connect 层级闭合

本轮使用脚本扫描 `L2tlb_agent_connect.sv` 中所有 `RTL_PATH.*` 引用，并与 V2 `build/rtl/MemBlock.sv` 全文比对内部 wire 是否存在：

```text
checked RTL_PATH references: 63
missing: 0
```

这说明 L2TLB connect 宏引用的内部 `RTL_PATH.*` wire 均存在于当前 V2 `build/rtl/MemBlock.sv`，没有悬空层级名。

### 4.3 编译验证

执行命令：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

验证结果：

```text
Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)
```

第一轮编译曾失败于 `array_*_ext` cell 找不到；补齐 `build/rtl/*_ext.v` 后第二轮编译通过。

本轮纠正为 V2 内部 `_inner_dtlbRepeater_*` 到 `_inner_ptw_io_tlb_1_*` 的真实映射并默认开启后，需要再次执行同一远端编译命令，确认内部 force 路径可被 VCS elaboration 接受：

```text
Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)
```

### 4.4 Subagent review 问题闭环

此前 review 曾指出两项阻塞问题：

- `L2tlb_agent_connect.sv` 仍可能让 L2TLB responder 处于 active，但上一轮接到了语义错误的 V2 顶层 `io_l2_tlb_req_*`。
- `auto_inner_buffers_out_a_ready` 和 `auto_inner_ptw_to_l2_buffer_out_a_ready` 曾存在先置 1 后被默认 0 覆盖的风险。

当前闭环结果：

- L2TLB takeover 已默认开启，`l2tlb_active` 由 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，V2 内部 `_inner_dtlbRepeater_io_ptw_req_0_*` 和 `_inner_ptw_io_tlb_1_*` 已映射到 `L2TLB_agent`。
- 两条 TileLink A ready 最终默认值保持 1。
- 额外检查发现 `io_dft_reset_lgc_rst_n` 也存在默认 1 被后续清零覆盖的风险，已修正为最终默认 1。

此前一轮 subagent 只读 review 曾确认 V2 DUT 结构适配无阻塞问题。DFT reset 默认值修正后，已重新执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

最终仍通过：

```text
Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)
```

本轮 V2 L2TLB 内部映射已复查代码和文档同步，并重新确认端口闭合、connect 内部层级闭合、`git diff --check` 和远端编译结果。

## 5. 风险边界和未完成项

本轮完成的是 V2 整核 RTL 编译结构适配，不代表测试框架行为已经完全适配 V2。以下事项已记录到后续分析文档：

```text
AI_DOC/analysis/interface/v2/mem_ut_v2_dut_framework_followup_notes_20260707.md
```

主要剩余风险：

- V3 聚合 issue/writeback transaction 仍然保留在测试框架中，本轮只在 connect 层做 V2 split 接口映射。
- L2TLB 内部 responder 映射已默认开启，request 的 `s2xlate` 直接来自 `_inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate`，不再由顶层端口推导或固定为 0。
- V2 内部 response bundle 中存在 `s2_entry_perm_g/u`，但当前 agent interface/xaction 没有对应字段，本轮为了避免扩大接口修改，在 connect 层固定为 0；若后续用例需要覆盖这两个权限位，应专项补齐 agent 字段链路。
- 本轮接入点是生成后 Verilog 内部 wire，后续 V2 RTL 重新生成时这些 `_inner_*` 层级名可能变化，需要按 profile 重新复查。
- 对 V2 无等价字段，本轮为了结构编译闭合采用置 0 或默认值，需要后续按功能分类判断是否应新增 V2 transaction 字段或修改 RM。
- `tc_sanity` 编译已通过，但本轮尚未执行 `make eda_run`，runtime 行为仍需后续验证。

## 6. Plan 对齐检查

本轮对应用户直接指令执行，未找到单一已完成 coding plan 文件完全覆盖本次“整核 `build/rtl` V2 DUT 接入 + `dut_inst` 全量重建 + connect V2-only 映射”的全部范围。已参考的接口差异文档为：

```text
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_memblock_interface_delta_20260707.md
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_port_diff_detail_20260707.md
```

已检查的 plan 目录包括：

```text
AI_DOC/plan/test_framework/plan/undo
AI_DOC/plan/test_framework/plan/do
```

本轮未把任何 plan 从 `undo` 移动到 `do`，因为用户要求的是本次 coding 适配和 review 文档，不是关闭既有 plan。

## 7. Review 结论

当前代码已经满足 V2 整核 `MemBlock` 顶层端口闭合、L2TLB internal wire 存在性检查和远端 VCS 编译通过三个结构性标准。后续需要单独建立测试框架行为适配 plan，继续处理 issue/writeback/L2TLB 的语义级适配和 runtime 仿真问题。
