# mem_ut V2 agent interface 信号归属矩阵

## 1. 文档目标

本文按当前 `mem_ut_uvm_v2` 分支的 agent 划分，汇总 `mem_ut/ver/ut/memblock/agent/*/src/*_interface.sv` 中每个 interface 信号，并逐项检查：是否有对应 `xaction` 字段、是否有 `tb/*_agent_connect.sv` 连接、是否被 monitor 采集、是否被 driver 驱动。

本文只做接口结构归属分析，不判断 raw queue、RM、scoreboard、sequence 主循环等测试框架语义是否已经完成适配。

## 2. 检查方法

- RTL 基准：当前 V2 profile权威文件`build_memblock/rtl/MemBlock.sv`及`build_memblock/rtl/filelist.f`；`build/rtl`只作辅助比对。
- connect 基准：`tb/*_agent_connect.sv` 的 `MEMBLOCK_UT` 活动分支；`connect方向/对象（解析）` 从 `force U_IF_NAME.<signal> = ...`、`force ... = U_IF_NAME.<signal>` 或直接赋值中提取。
- interface 基准：`agent/*/src/*_interface.sv` 中的 `logic` 字段。
- transaction 基准：同名 `*_xaction.sv` 中声明的字段。
- monitor 采集判定：monitor 源码中出现 `mon_cb.<field>`。
- driver 驱动判定：driver 源码中出现 `drv_cb.<field> <=` 或 `drv_cb.<field> =`。

说明：本文中的“没有 driver 驱动”是静态覆盖结果，不自动等同于接口缺陷。对于 DUT output、被动 monitor 型信号、ready/valid 中由 RTL 侧产生的握手信号，没有 driver 驱动是合理状态；只有 DUT input 方向且需要测试环境主动建模的信号，才需要在后续适配中进一步判断是否缺少 driver。

## 3. agent 汇总

| agent | interface信号数 | 有xaction | 有connect | 有monitor采集 | 有driver驱动 |
|---|---:|---:|---:|---:|---:|
| `L2tlb_agent_agent` | 63 | 63 | 63 | 63 | 60 |
| `backendToTopBypass_agent_agent` | 2 | 2 | 2 | 2 | 2 |
| `csr_ctrl_agent_agent` | 93 | 93 | 93 | 93 | 93 |
| `dcache_agent_agent` | 57 | 57 | 57 | 57 | 26 |
| `fence_agent_agent` | 8 | 8 | 8 | 8 | 8 |
| `int_sink_agent_agent` | 7 | 7 | 7 | 7 | 7 |
| `io_mem_to_ooo_ctrl_agent_agent` | 42 | 42 | 42 | 42 | 0 |
| `io_mem_to_ooo_int_wb_agent_agent` | 109 | 109 | 109 | 109 | 0 |
| `io_mem_to_ooo_iq_feedback_agent_agent` | 26 | 26 | 26 | 26 | 0 |
| `io_mem_to_ooo_vec_wb_agent_agent` | 73 | 73 | 73 | 73 | 0 |
| `io_mem_to_ooo_wakeup_agent_agent` | 12 | 12 | 12 | 12 | 0 |
| `itlb_agent_agent` | 64 | 64 | 64 | 64 | 4 |
| `lintsissue_agent_agent` | 112 | 112 | 112 | 112 | 105 |
| `lsqcommit_agent_agent` | 6 | 6 | 6 | 6 | 6 |
| `lsqenq_agent_agent` | 234 | 234 | 234 | 234 | 234 |
| `other_ctrl_agent_agent` | 16 | 16 | 16 | 16 | 4 |
| `prefetch_agent_agent` | 11 | 11 | 11 | 11 | 0 |
| `redirect_agent_agent` | 4 | 4 | 4 | 4 | 4 |
| `sbuffer_agent_agent` | 20 | 20 | 20 | 20 | 10 |
| `vecissue_agent_agent` | 76 | 76 | 76 | 76 | 74 |

## 4. 逐 agent interface 信号列表

### 4.1 `L2tlb_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ptw_req_0_ready` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_req_0_ready` | 有 | 有 |
| `io_ptw_req_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `_inner_dtlbRepeater_io_ptw_req_0_valid`<br>常量->IF: `'0` | 有 | 没有 |
| `io_ptw_req_0_bits_vpn` | `[37:0]` | `input` | `input` | 有 | 有 | RTL->IF: `_inner_dtlbRepeater_io_ptw_req_0_bits_vpn`<br>常量->IF: `'0` | 有 | 没有 |
| `io_ptw_req_0_bits_s2xlate` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `_inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate`<br>常量->IF: `'0` | 有 | 没有 |
| `io_ptw_resp_valid` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_valid` | 有 | 有 |
| `io_ptw_resp_bits_s2xlate` | `[1:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2xlate` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_tag` | `[34:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_tag` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_asid` | `[15:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_asid` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_vmid` | `[13:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_vmid` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_n` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_n` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_pbmt` | `[1:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_pbmt` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_d` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_d` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_a` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_a` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_g` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_g` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_u` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_u` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_x` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_x` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_w` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_w` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_perm_r` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_r` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_level` | `[1:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_level` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_v` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_v` | 有 | 有 |
| `io_ptw_resp_bits_s1_entry_ppn` | `[40:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_entry_ppn` | 有 | 有 |
| `io_ptw_resp_bits_s1_addr_low` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_addr_low` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_0` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_0` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_1` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_1` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_2` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_2` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_3` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_3` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_4` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_4` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_5` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_5` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_6` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_6` | 有 | 有 |
| `io_ptw_resp_bits_s1_ppn_low_7` | `[2:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_7` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_0` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_0` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_1` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_1` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_2` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_2` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_3` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_3` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_4` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_4` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_5` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_5` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_6` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_6` | 有 | 有 |
| `io_ptw_resp_bits_s1_valididx_7` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_valididx_7` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_0` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_0` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_1` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_1` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_2` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_2` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_3` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_3` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_4` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_4` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_5` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_5` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_6` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_6` | 有 | 有 |
| `io_ptw_resp_bits_s1_pteidx_7` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pteidx_7` | 有 | 有 |
| `io_ptw_resp_bits_s1_pf` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_pf` | 有 | 有 |
| `io_ptw_resp_bits_s1_af` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s1_af` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_tag` | `[37:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_tag` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_vmid` | `[13:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_vmid` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_n` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_n` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_pbmt` | `[1:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_pbmt` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_ppn` | `[37:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_ppn` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_d` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_d` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_a` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_a` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_g` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_u` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_u` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_x` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_x` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_w` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_w` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_perm_r` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_r` | 有 | 有 |
| `io_ptw_resp_bits_s2_entry_level` | `[1:0]` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_entry_level` | 有 | 有 |
| `io_ptw_resp_bits_s2_gpf` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_gpf` | 有 | 有 |
| `io_ptw_resp_bits_s2_gaf` | `` | `output` | `input` | 有 | 有 | 常量->IF: `'0`<br>IF->RTL: `_inner_ptw_io_tlb_1_resp_bits_s2_gaf` | 有 | 有 |

### 4.2 `backendToTopBypass_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/backendToTopBypass_agent_agent/src/backendToTopBypass_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_backendToTopBypass_cpuCriticalError` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_backendToTopBypass_cpuCriticalError`<br>RTL->IF: `io_ooo_to_mem_backendToTopBypass_cpuCriticalError` | 有 | 有 |
| `io_ooo_to_mem_backendToTopBypass_cpuHalted` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_backendToTopBypass_cpuHalted`<br>RTL->IF: `io_ooo_to_mem_backendToTopBypass_cpuHalted` | 有 | 有 |

### 4.3 `csr_ctrl_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_tlbCsr_satp_mode` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_satp_mode`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_satp_mode` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_satp_asid` | `[15:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_satp_asid`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_satp_asid` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_satp_ppn` | `[43:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_satp_ppn`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_satp_ppn` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_satp_changed` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_satp_changed`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_satp_changed` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_vsatp_mode` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_vsatp_mode`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_vsatp_mode` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_vsatp_asid` | `[15:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_vsatp_asid`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_vsatp_asid` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_vsatp_ppn` | `[43:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_vsatp_ppn`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_vsatp_ppn` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_vsatp_changed` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_vsatp_changed`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_vsatp_changed` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_hgatp_mode` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_hgatp_mode`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_hgatp_mode` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_hgatp_vmid` | `[15:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_hgatp_vmid`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_hgatp_vmid` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_hgatp_ppn` | `[43:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_hgatp_ppn`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_hgatp_ppn` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_hgatp_changed` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_hgatp_changed`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_hgatp_changed` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_mbmc_BME` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_mbmc_BME`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_mbmc_BME` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_mbmc_CMODE` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_mbmc_CMODE`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_mbmc_CMODE` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_mbmc_BCLEAR` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_mbmc_BCLEAR`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_mbmc_BCLEAR` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_mbmc_BMA` | `[57:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_mbmc_BMA`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_mbmc_BMA` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_mxr` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_mxr`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_mxr` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_sum` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_sum`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_sum` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_vmxr` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_vmxr`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_vmxr` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_vsum` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_vsum`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_vsum` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_virt` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_virt`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_virt` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_virt_changed` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_virt_changed`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_virt_changed` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_spvp` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_spvp`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_spvp` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_imode` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_imode`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_imode` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_dmode` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_dmode`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_dmode` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_mPBMTE` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_mPBMTE`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_mPBMTE` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_hPBMTE` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_hPBMTE`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_hPBMTE` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_pmm_mseccfg` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_pmm_mseccfg`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_pmm_mseccfg` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_pmm_menvcfg` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_pmm_menvcfg`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_pmm_menvcfg` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_pmm_henvcfg` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_pmm_henvcfg`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_pmm_henvcfg` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_pmm_hstatus` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_pmm_hstatus`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_pmm_hstatus` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_pmm_senvcfg` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_pmm_senvcfg`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_pmm_senvcfg` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency` | `[9:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_sbuffer_timeout` | `[21:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_sbuffer_timeout`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_sbuffer_timeout` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_ldld_vio_check_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_ldld_vio_check_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_ldld_vio_check_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_cache_error_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_cache_error_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_cache_error_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_power_down_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_power_down_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_power_down_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_flush_l2_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_flush_l2_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_flush_l2_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_distribute_csr_w_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_distribute_csr_w_valid`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_distribute_csr_w_valid` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr` | `[11:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_fsIsOff` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_fsIsOff`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_fsIsOff` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_hd_misalign_st_enable` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_hd_misalign_st_enable`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_hd_misalign_st_enable` | 有 | 有 |
| `io_ooo_to_mem_csrCtrl_mem_trigger_debugMode` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_csrCtrl_mem_trigger_debugMode`<br>RTL->IF: `io_ooo_to_mem_csrCtrl_mem_trigger_debugMode` | 有 | 有 |
| `io_ooo_to_mem_tlbCsr_priv_debug` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_tlbCsr_priv_debug`<br>RTL->IF: `io_ooo_to_mem_tlbCsr_priv_debug` | 有 | 有 |

### 4.4 `dcache_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `auto_inner_dcache_client_out_a_ready` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_a_ready`<br>RTL->IF: `auto_inner_dcache_client_out_a_ready` | 有 | 有 |
| `auto_inner_dcache_client_out_a_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_valid` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_opcode` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_opcode` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_param` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_param` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_size` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_size` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_source` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_source` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_address` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_address` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_user_alias` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_user_alias` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_user_vaddr` | `[43:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_user_vaddr` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_user_reqSource` | `[4:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_user_reqSource` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_user_needHint` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_user_needHint` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_echo_isKeyword` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_echo_isKeyword` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_mask` | `[31:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_mask` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_data` | `[255:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_data` | 有 | 没有 |
| `auto_inner_dcache_client_out_a_bits_corrupt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_a_bits_corrupt` | 有 | 没有 |
| `auto_inner_dcache_client_out_b_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_b_ready` | 有 | 没有 |
| `auto_inner_dcache_client_out_b_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_valid`<br>RTL->IF: `auto_inner_dcache_client_out_b_valid` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_opcode` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_opcode`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_opcode` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_param` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_param`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_param` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_size` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_size`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_size` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_source` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_source`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_source` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_address` | `[47:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_address`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_address` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_mask` | `[31:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_mask`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_mask` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_data` | `[255:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_data`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_data` | 有 | 有 |
| `auto_inner_dcache_client_out_b_bits_corrupt` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_b_bits_corrupt`<br>RTL->IF: `auto_inner_dcache_client_out_b_bits_corrupt` | 有 | 有 |
| `auto_inner_dcache_client_out_c_ready` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_c_ready`<br>RTL->IF: `auto_inner_dcache_client_out_c_ready` | 有 | 有 |
| `auto_inner_dcache_client_out_c_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_valid` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_opcode` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_opcode` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_param` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_param` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_size` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_size` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_source` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_source` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_address` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_address` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_user_alias` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_user_alias` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_user_vaddr` | `[43:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_user_vaddr` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_user_reqSource` | `[4:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_user_reqSource` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_user_needHint` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_user_needHint` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_echo_isKeyword` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_echo_isKeyword` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_data` | `[255:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_data` | 有 | 没有 |
| `auto_inner_dcache_client_out_c_bits_corrupt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_c_bits_corrupt` | 有 | 没有 |
| `auto_inner_dcache_client_out_d_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_d_ready` | 有 | 没有 |
| `auto_inner_dcache_client_out_d_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_valid`<br>RTL->IF: `auto_inner_dcache_client_out_d_valid` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_opcode` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_opcode`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_opcode` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_param` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_param`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_param` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_size` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_size`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_size` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_source` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_source`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_source` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_sink` | `[9:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_sink`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_sink` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_denied` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_denied`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_denied` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_echo_isKeyword` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_echo_isKeyword`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_echo_isKeyword` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_data` | `[255:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_data`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_data` | 有 | 有 |
| `auto_inner_dcache_client_out_d_bits_corrupt` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_d_bits_corrupt`<br>RTL->IF: `auto_inner_dcache_client_out_d_bits_corrupt` | 有 | 有 |
| `auto_inner_dcache_client_out_e_ready` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_dcache_client_out_e_ready`<br>RTL->IF: `auto_inner_dcache_client_out_e_ready` | 有 | 有 |
| `auto_inner_dcache_client_out_e_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_e_valid` | 有 | 没有 |
| `auto_inner_dcache_client_out_e_bits_sink` | `[9:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_dcache_client_out_e_bits_sink` | 有 | 没有 |
| `io_l2_hint_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_l2_hint_valid` | 有 | 有 |
| `io_l2_hint_bits_sourceId` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_l2_hint_bits_sourceId` | 有 | 有 |
| `io_l2_hint_bits_isKeyword` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_l2_hint_bits_isKeyword` | 有 | 有 |
| `io_l2_flush_done` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_l2_flush_done` | 有 | 有 |

补充的运行期 owner 约束：上述四个字段虽然在 interface、xaction、connect、monitor/driver 矩阵中
均已接入，但不是 generic random 的自由激励。`io_l2_hint_valid/sourceId/isKeyword` 只有在
DCache responder 真实接受 `AcquireBlock -> GrantData` 后才能由专用 builder 产生一拍非零 Hint；
generic xaction、generic idle 和 driver idle 必须保持 0。`io_l2_flush_done` 当前没有功能 producer，
全程保持已知 0。driver 在首次 VIF 赋值前使用四态检查，未知 Hint valid/payload、非已知 0 的
flush 和 valid=0 时非零 payload 均 fail-fast；四个 sideband xaction 字段保留四态，generic
E.ready 固定为 0，只有 GrantAck owner item 可以打开。字段链路和 A/C/D/E 完整生命周期见
`AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md`。

### 4.5 `fence_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_sfence_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_valid`<br>RTL->IF: `io_ooo_to_mem_sfence_valid` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_rs1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_rs1`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_rs1` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_rs2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_rs2`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_rs2` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_addr` | `[49:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_addr`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_addr` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_id` | `[15:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_id`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_id` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_hv` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_hv`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_hv` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_hg` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_hg`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_hg` | 有 | 有 |
| `io_ooo_to_mem_sfence_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_sfence_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_sfence_bits_flushPipe` | 有 | 有 |

### 4.6 `int_sink_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/int_sink_agent_agent/src/int_sink_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `auto_inner_beu_local_int_sink_in_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_beu_local_int_sink_in_0`<br>RTL->IF: `auto_inner_beu_local_int_sink_in_0` | 有 | 有 |
| `auto_inner_nmi_int_sink_in_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_nmi_int_sink_in_0`<br>RTL->IF: `auto_inner_nmi_int_sink_in_0` | 有 | 有 |
| `auto_inner_nmi_int_sink_in_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_nmi_int_sink_in_1`<br>RTL->IF: `auto_inner_nmi_int_sink_in_1` | 有 | 有 |
| `auto_inner_plic_int_sink_in_1_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_plic_int_sink_in_1_0`<br>RTL->IF: `auto_inner_plic_int_sink_in_1_0` | 有 | 有 |
| `auto_inner_plic_int_sink_in_0_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_plic_int_sink_in_0_0`<br>RTL->IF: `auto_inner_plic_int_sink_in_0_0` | 有 | 有 |
| `auto_inner_clint_int_sink_in_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_clint_int_sink_in_0`<br>RTL->IF: `auto_inner_clint_int_sink_in_0` | 有 | 有 |
| `auto_inner_clint_int_sink_in_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_clint_int_sink_in_1`<br>RTL->IF: `auto_inner_clint_int_sink_in_1` | 有 | 有 |

### 4.7 `io_mem_to_ooo_ctrl_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_mem_to_ooo_topToBackendBypass_hartId` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_hartId` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_msiInfo_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_msiInfo_valid` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_msiInfo_bits` | `[12:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_msiInfo_bits` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_clintTime_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_clintTime_valid` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_clintTime_bits` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_clintTime_bits` | 有 | 没有 |
| `io_mem_to_ooo_topToBackendBypass_l2FlushDone` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_topToBackendBypass_l2FlushDone` | 有 | 没有 |
| `io_mem_to_ooo_lqCancelCnt` | `[6:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lqCancelCnt` | 有 | 没有 |
| `io_mem_to_ooo_sqCancelCnt` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_sqCancelCnt` | 有 | 没有 |
| `io_mem_to_ooo_sqDeq` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_sqDeq` | 有 | 没有 |
| `io_mem_to_ooo_lqDeq` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lqDeq` | 有 | 没有 |
| `io_mem_to_ooo_lqDeqPtr_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lqDeqPtr_flag` | 有 | 没有 |
| `io_mem_to_ooo_lqDeqPtr_value` | `[6:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lqDeqPtr_value` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_valid` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_ftqIdx_value` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_ftqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_ftqOffset` | `[4:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_ftqOffset` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_isRVC` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_isRVC` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_level` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_level` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_memoryViolation_bits_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_memoryViolation_bits_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_sbIsEmpty` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_sbIsEmpty` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_vaddr` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_vaddr` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_gpaddr` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_gpaddr` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_isForVSnonLeafPTE` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_isForVSnonLeafPTE` | 有 | 没有 |
| `io_mem_to_ooo_ldCancel_0_ld2Cancel` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_ldCancel_0_ld2Cancel` | 有 | 没有 |
| `io_mem_to_ooo_ldCancel_1_ld2Cancel` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_ldCancel_1_ld2Cancel` | 有 | 没有 |
| `io_mem_to_ooo_ldCancel_2_ld2Cancel` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_ldCancel_2_ld2Cancel` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_loadMmio_0` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_loadMmio_0` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_loadMmio_1` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_loadMmio_1` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_loadMmio_2` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_loadMmio_2` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_storeMmio` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_storeMmio` | 有 | 没有 |
| `io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value` | 有 | 没有 |

### 4.8 `io_mem_to_ooo_int_wb_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv`

本 agent 当前只保留 V2 `MemBlock` 顶层真实 `writebackLda/Sta/Std` output 端口命名；旧 V3 整数写回聚合别名已从 interface、xaction、monitor 和 connect 中删除，不再作为当前字段列出。

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_mem_to_ooo_writebackLda_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_rfWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_rfWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_fpWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_fpWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_pdest` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_uop_replayInst` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_uop_replayInst` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_data` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_data` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_rfWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_rfWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_fpWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_fpWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_pdest` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_uop_replayInst` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_uop_replayInst` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_data` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_data` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_rfWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_rfWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_fpWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_fpWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_pdest` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_uop_replayInst` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_uop_replayInst` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_data` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_data` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackStd_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackStd_0_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackStd_1_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackStd_1_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value` | 有 | 没有 |
### 4.9 `io_mem_to_ooo_iq_feedback_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value` | `[6:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value` | `[5:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value` | `[6:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask` | `[15:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask` | `[15:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask` | 有 | 没有 |
| `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx` | 有 | 没有 |

V2 scalar STA IQ feedback 运行期结论：

1. 上表证明每路`staIqFeedback`只有`valid/hit/sqIdx_flag/sqIdx_value`，没有ROB、LQ、
   `issue_epoch`或`replay_seq`字段。动态实例信息不是缺失的interface字段，不能通过给
   interface/xaction增加DUT不存在的payload解决。
2. 当前 monitor 已按真实接口构造 SQ-only raw：只置`sq_valid=1`并复制真实SQ key，
   `rob_valid/lq_valid=0`，不再把 empty raw 的0值伪装成ROB0/LQ0。
3. adapter 以 active SQ map 唯一反查 uid，再从该 uid 的 current status 校验
   `active_sq_mapped/sta_dispatched/canonical SQ`并补齐ROB、`issue_epoch/replay_seq`。
   查无owner、owner不一致或snapshot不完整均fatal；本轮不建立generation token、claim map
   或tombstone。
4. STA IQ hit只调用`mark_issue_feedback_success()`，严格模式下继续等待真实STA real-WB；
   miss进入现有replay recovery，清旧STA状态、递增`replay_seq`并允许重新issue。
   同一采样batch先处理IQ raw再处理int-WB raw；ctrl deq在semantic batch之后应用。
5. `vstuIqFeedback`属于vector replay接口。本轮scalar-only不支持vector LS，任一VSTU
   valid固定`uvm_fatal`，不能静默当作STA，也不能info/drop后继续。完整vector partial
   replay留给vector专项。

对应唯一执行落点：

```text
AI_DOC/plan/test_framework/plan/do/mem_ut_v2_iq_feedback_replay_framework_adapt_execution_plan_20260711.md
```

### 4.10 `io_mem_to_ooo_vec_wb_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_mem_to_ooo_writebackVldu_0_bits_data` | `[127:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_data` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType` | `[8:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_pdest` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask` | `[127:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx` | `[6:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_vdIdx` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_vdIdx` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_0_valid` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_data` | `[127:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_data` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType` | `[8:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_pdest` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_trigger` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_trigger` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask` | `[127:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx` | `[6:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_vdIdx` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_vdIdx` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField` | 有 | 没有 |
| `io_mem_to_ooo_writebackVldu_1_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_writebackVldu_1_valid` | 有 | 没有 |

### 4.11 `io_mem_to_ooo_wakeup_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_wakeup_agent_agent/src/io_mem_to_ooo_wakeup_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_mem_to_ooo_wakeup_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_0_valid` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_0_bits_rfWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_0_bits_rfWen` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_0_bits_fpWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_0_bits_fpWen` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_0_bits_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_0_bits_pdest` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_1_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_1_valid` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_1_bits_rfWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_1_bits_rfWen` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_1_bits_fpWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_1_bits_fpWen` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_1_bits_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_1_bits_pdest` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_2_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_2_valid` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_2_bits_rfWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_2_bits_rfWen` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_2_bits_fpWen` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_2_bits_fpWen` | 有 | 没有 |
| `io_mem_to_ooo_wakeup_2_bits_pdest` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_mem_to_ooo_wakeup_2_bits_pdest` | 有 | 没有 |

### 4.12 `itlb_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/itlb_agent_agent/src/itlb_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_fetch_to_mem_itlb_req_0_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_req_0_ready` | 有 | 没有 |
| `io_fetch_to_mem_itlb_req_0_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_fetch_to_mem_itlb_req_0_valid`<br>RTL->IF: `io_fetch_to_mem_itlb_req_0_valid` | 有 | 有 |
| `io_fetch_to_mem_itlb_req_0_bits_vpn` | `[37:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_fetch_to_mem_itlb_req_0_bits_vpn`<br>RTL->IF: `io_fetch_to_mem_itlb_req_0_bits_vpn` | 有 | 有 |
| `io_fetch_to_mem_itlb_req_0_bits_s2xlate` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_fetch_to_mem_itlb_req_0_bits_s2xlate`<br>RTL->IF: `io_fetch_to_mem_itlb_req_0_bits_s2xlate` | 有 | 有 |
| `io_fetch_to_mem_itlb_resp_ready` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_fetch_to_mem_itlb_resp_ready`<br>RTL->IF: `io_fetch_to_mem_itlb_resp_ready` | 有 | 有 |
| `io_fetch_to_mem_itlb_resp_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_valid` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2xlate` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2xlate` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_tag` | `[34:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_tag` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_asid` | `[15:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_asid` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid` | `[13:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_n` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_n` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_level` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_level` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_v` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_v` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn` | `[40:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_addr_low` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_addr_low` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_0` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_0` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_1` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_1` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_2` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_2` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_3` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_4` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_5` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_6` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_valididx_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_valididx_7` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_pf` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_pf` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s1_af` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s1_af` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_tag` | `[37:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_tag` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_n` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_n` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn` | `[37:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_entry_level` | `[1:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_entry_level` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_gpf` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_gpf` | 有 | 没有 |
| `io_fetch_to_mem_itlb_resp_bits_s2_gaf` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_fetch_to_mem_itlb_resp_bits_s2_gaf` | 有 | 没有 |

### 4.13 `lintsissue_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_issueLda_0_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_fpWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_fpWen`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_fpWen` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_imm` | `[31:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_imm`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_imm` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_pc` | `[49:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_pc`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_pc` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_rfWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_rfWen`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_rfWen` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_0_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueLda_0_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueLda_0_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_0_valid`<br>RTL->IF: `io_ooo_to_mem_issueLda_0_valid` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_fpWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_fpWen`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_fpWen` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_imm` | `[31:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_imm`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_imm` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_pc` | `[49:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_pc`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_pc` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_rfWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_rfWen`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_rfWen` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_1_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueLda_1_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueLda_1_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_1_valid`<br>RTL->IF: `io_ooo_to_mem_issueLda_1_valid` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_fpWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_fpWen`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_fpWen` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_imm` | `[31:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_imm`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_imm` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_pc` | `[49:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_pc`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_pc` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_rfWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_rfWen`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_rfWen` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueLda_2_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueLda_2_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueLda_2_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueLda_2_valid`<br>RTL->IF: `io_ooo_to_mem_issueLda_2_valid` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_fuType` | `[34:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_fuType`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_fuType` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_imm` | `[31:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_imm`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_imm` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_rfWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_rfWen`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_rfWen` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueSta_0_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueSta_0_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueSta_0_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_0_valid`<br>RTL->IF: `io_ooo_to_mem_issueSta_0_valid` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_fuType` | `[34:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_fuType`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_fuType` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_imm` | `[31:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_imm`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_imm` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_rfWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_rfWen`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_rfWen` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueSta_1_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueSta_1_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueSta_1_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueSta_1_valid`<br>RTL->IF: `io_ooo_to_mem_issueSta_1_valid` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_bits_uop_fuType` | `[34:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_bits_uop_fuType`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_bits_uop_fuType` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueStd_0_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueStd_0_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueStd_0_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_0_valid`<br>RTL->IF: `io_ooo_to_mem_issueStd_0_valid` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_bits_src_0` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_bits_uop_fuType` | `[34:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_bits_uop_fuType`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_bits_uop_fuType` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueStd_1_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueStd_1_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueStd_1_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueStd_1_valid`<br>RTL->IF: `io_ooo_to_mem_issueStd_1_valid` | 有 | 有 |

### 4.14 `lsqcommit_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_lsqio_pendingPtr_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_lsqio_pendingPtr_flag`<br>RTL->IF: `io_ooo_to_mem_lsqio_pendingPtr_flag` | 有 | 有 |
| `io_ooo_to_mem_lsqio_pendingPtr_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_lsqio_pendingPtr_value`<br>RTL->IF: `io_ooo_to_mem_lsqio_pendingPtr_value` | 有 | 有 |
| `io_ooo_to_mem_flushSb` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_flushSb`<br>RTL->IF: `io_ooo_to_mem_flushSb` | 有 | 有 |
| `io_ooo_to_mem_lsqio_pendingMMIOld` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_lsqio_pendingMMIOld`<br>RTL->IF: `io_ooo_to_mem_lsqio_pendingMMIOld` | 有 | 有 |
| `io_ooo_to_mem_lsqio_pendingst` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_lsqio_pendingst`<br>RTL->IF: `io_ooo_to_mem_lsqio_pendingst` | 有 | 有 |
| `io_ooo_to_mem_lsqio_scommit` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_lsqio_scommit`<br>RTL->IF: `io_ooo_to_mem_lsqio_scommit` | 有 | 有 |

### 4.15 `lsqenq_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_enqLsq_needAlloc_0` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_needAlloc_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_needAlloc_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_needAlloc_1` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_needAlloc_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_needAlloc_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_needAlloc_2` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_needAlloc_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_needAlloc_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_needAlloc_3` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_needAlloc_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_needAlloc_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_needAlloc_4` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_needAlloc_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_needAlloc_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_needAlloc_5` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_needAlloc_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_needAlloc_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_valid`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_valid` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_fuType` | `[35:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_fuType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_fuType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_uopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_uopIdx`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_uopIdx` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_numLsElem` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_numLsElem`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_numLsElem` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_valid`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_valid` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_fuType` | `[35:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_fuType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_fuType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_uopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_uopIdx`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_uopIdx` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_numLsElem` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_numLsElem`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_numLsElem` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_valid`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_valid` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_fuType` | `[35:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_fuType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_fuType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_uopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_uopIdx`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_uopIdx` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_numLsElem` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_numLsElem`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_numLsElem` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_valid`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_valid` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_fuType` | `[35:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_fuType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_fuType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_uopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_uopIdx`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_uopIdx` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_numLsElem` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_numLsElem`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_numLsElem` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_valid`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_valid` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_fuType` | `[35:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_fuType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_fuType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_uopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_uopIdx`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_uopIdx` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_numLsElem` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_numLsElem`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_numLsElem` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_valid`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_valid` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_fuType` | `[35:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_fuType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_fuType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_uopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_uopIdx`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_uopIdx` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_numLsElem` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_numLsElem`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_numLsElem` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_flushPipe` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_fuOpType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_lastUop`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_lastUop` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_0_bits_trigger` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_0_bits_trigger`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_0_bits_trigger` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_flushPipe` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_fuOpType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_lastUop`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_lastUop` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_1_bits_trigger` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_1_bits_trigger`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_1_bits_trigger` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_flushPipe` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_fuOpType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_lastUop`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_lastUop` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_2_bits_trigger` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_2_bits_trigger`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_2_bits_trigger` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_flushPipe` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_fuOpType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_lastUop`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_lastUop` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_3_bits_trigger` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_3_bits_trigger`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_3_bits_trigger` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_flushPipe` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_fuOpType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_lastUop`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_lastUop` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_4_bits_trigger` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_4_bits_trigger`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_4_bits_trigger` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_flushPipe` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_flushPipe`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_flushPipe` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_fuOpType`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_lastUop`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_lastUop` | 有 | 有 |
| `io_ooo_to_mem_enqLsq_req_5_bits_trigger` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_enqLsq_req_5_bits_trigger`<br>RTL->IF: `io_ooo_to_mem_enqLsq_req_5_bits_trigger` | 有 | 有 |

### 4.16 `other_ctrl_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_hartId` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_hartId`<br>RTL->IF: `io_hartId` | 有 | 有 |
| `io_dcacheError_ecc_error_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_dcacheError_ecc_error_valid` | 有 | 没有 |
| `io_dcacheError_ecc_error_bits` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_dcacheError_ecc_error_bits` | 有 | 没有 |
| `io_uncacheError_ecc_error_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_uncacheError_ecc_error_valid` | 有 | 没有 |
| `io_uncacheError_ecc_error_bits` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_uncacheError_ecc_error_bits` | 有 | 没有 |
| `io_inner_reset_vector` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_inner_reset_vector` | 有 | 没有 |
| `io_outer_reset_vector` | `[47:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_outer_reset_vector`<br>RTL->IF: `io_outer_reset_vector` | 有 | 有 |
| `io_outer_l2_flush_en` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_outer_l2_flush_en` | 有 | 没有 |
| `io_outer_power_down_en` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_outer_power_down_en` | 有 | 没有 |
| `io_outer_cpu_critical_error` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_outer_cpu_critical_error` | 有 | 没有 |
| `io_inner_beu_errors_icache_ecc_error_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_inner_beu_errors_icache_ecc_error_valid`<br>RTL->IF: `io_inner_beu_errors_icache_ecc_error_valid` | 有 | 有 |
| `io_inner_beu_errors_icache_ecc_error_bits` | `[47:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_inner_beu_errors_icache_ecc_error_bits`<br>RTL->IF: `io_inner_beu_errors_icache_ecc_error_bits` | 有 | 有 |
| `io_outer_beu_errors_icache_ecc_error_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_outer_beu_errors_icache_ecc_error_valid` | 有 | 没有 |
| `io_outer_beu_errors_icache_ecc_error_bits` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_outer_beu_errors_icache_ecc_error_bits` | 有 | 没有 |
| `io_reset_backend` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_reset_backend` | 有 | 没有 |
| `io_outer_cpu_halt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_outer_cpu_halt` | 有 | 没有 |

### 4.17 `prefetch_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/prefetch_agent_agent/src/prefetch_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `auto_inner_l3_pf_sender_out_addr` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_l3_pf_sender_out_addr` | 有 | 没有 |
| `auto_inner_l3_pf_sender_out_addr_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_l3_pf_sender_out_addr_valid` | 有 | 没有 |
| `auto_inner_l2_pf_sender_out_addr` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_l2_pf_sender_out_addr` | 有 | 没有 |
| `auto_inner_l2_pf_sender_out_pf_source` | `[4:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_l2_pf_sender_out_pf_source` | 有 | 没有 |
| `auto_inner_l2_pf_sender_out_addr_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_l2_pf_sender_out_addr_valid` | 有 | 没有 |
| `io_ifetchPrefetch_0_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ifetchPrefetch_0_valid` | 有 | 没有 |
| `io_ifetchPrefetch_0_bits_vaddr` | `[49:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_ifetchPrefetch_0_bits_vaddr` | 有 | 没有 |
| `io_ifetchPrefetch_1_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ifetchPrefetch_1_valid` | 有 | 没有 |
| `io_ifetchPrefetch_1_bits_vaddr` | `[49:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_ifetchPrefetch_1_bits_vaddr` | 有 | 没有 |
| `io_ifetchPrefetch_2_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ifetchPrefetch_2_valid` | 有 | 没有 |
| `io_ifetchPrefetch_2_bits_vaddr` | `[49:0]` | `input` | `input` | 有 | 有 | RTL->IF: `io_ifetchPrefetch_2_bits_vaddr` | 有 | 没有 |

### 4.18 `redirect_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_redirect_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_redirect_valid`<br>RTL->IF: `io_redirect_valid` | 有 | 有 |
| `io_redirect_bits_level` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_redirect_bits_level`<br>RTL->IF: `io_redirect_bits_level` | 有 | 有 |
| `io_redirect_bits_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_redirect_bits_robIdx_flag`<br>RTL->IF: `io_redirect_bits_robIdx_flag` | 有 | 有 |
| `io_redirect_bits_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_redirect_bits_robIdx_value`<br>RTL->IF: `io_redirect_bits_robIdx_value` | 有 | 有 |

### 4.19 `sbuffer_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/sbuffer_agent_agent/src/sbuffer_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `auto_inner_buffers_out_a_ready` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_a_ready`<br>RTL->IF: `auto_inner_buffers_out_a_ready` | 有 | 有 |
| `auto_inner_buffers_out_a_valid` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_valid` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_opcode` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_opcode` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_param` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_param` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_size` | `[2:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_size` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_source` | `[3:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_source` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_address` | `[47:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_address` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_mask` | `[7:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_mask` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_data` | `[63:0]` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_data` | 有 | 没有 |
| `auto_inner_buffers_out_a_bits_corrupt` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_a_bits_corrupt` | 有 | 没有 |
| `auto_inner_buffers_out_d_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `auto_inner_buffers_out_d_ready` | 有 | 没有 |
| `auto_inner_buffers_out_d_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_valid`<br>RTL->IF: `auto_inner_buffers_out_d_valid` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_opcode` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_opcode`<br>RTL->IF: `auto_inner_buffers_out_d_bits_opcode` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_param` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_param`<br>RTL->IF: `auto_inner_buffers_out_d_bits_param` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_size` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_size`<br>RTL->IF: `auto_inner_buffers_out_d_bits_size` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_source` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_source`<br>RTL->IF: `auto_inner_buffers_out_d_bits_source` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_sink` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_sink`<br>RTL->IF: `auto_inner_buffers_out_d_bits_sink` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_denied` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_denied`<br>RTL->IF: `auto_inner_buffers_out_d_bits_denied` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_data` | `[63:0]` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_data`<br>RTL->IF: `auto_inner_buffers_out_d_bits_data` | 有 | 有 |
| `auto_inner_buffers_out_d_bits_corrupt` | `` | `output` | `input` | 有 | 有 | IF->RTL: `auto_inner_buffers_out_d_bits_corrupt`<br>RTL->IF: `auto_inner_buffers_out_d_bits_corrupt` | 有 | 有 |

### 4.20 `vecissue_agent_agent`

interface 文件：`mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_interface.sv`

| 信号 | 位宽 | drv_cb方向 | mon_cb方向 | xaction字段 | connect | connect方向/对象（解析） | monitor采集 | driver驱动 |
|---|---|---|---|---|---|---|---|---|
| `io_ooo_to_mem_isStoreException` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_isStoreException`<br>RTL->IF: `io_ooo_to_mem_isStoreException` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_flowNum` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_flowNum`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_flowNum` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_src_0` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_src_1` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_src_1`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_src_1` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_src_2` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_src_2`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_src_2` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_src_3` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_src_3`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_src_3` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_src_4` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_src_4`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_src_4` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_fuType` | `[34:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_fuType`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_fuType` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vecWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vecWen`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vecWen` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vlWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vlWen`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vlWen` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_vecReplayMask` | `[15:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_vecReplayMask`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_vecReplayMask` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_0_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueVldu_0_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueVldu_0_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_0_valid`<br>RTL->IF: `io_ooo_to_mem_issueVldu_0_valid` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_flowNum` | `[4:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_flowNum`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_flowNum` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_src_0` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_src_0`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_src_0` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_src_1` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_src_1`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_src_1` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_src_2` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_src_2`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_src_2` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_src_3` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_src_3`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_src_3` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_src_4` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_src_4`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_src_4` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType` | `[8:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_pdest` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_pdest`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_pdest` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value` | `[5:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vecWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vecWen`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vecWen` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vlWen` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vlWen`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vlWen` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul` | `[2:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask` | `[127:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew` | `[1:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart` | `[7:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx` | `[6:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_vecReplayMask` | `[15:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_vecReplayMask`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_vecReplayMask` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx` | `[3:0]` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx` | 有 | 有 |
| `io_ooo_to_mem_issueVldu_1_ready` | `` | `input` | `input` | 有 | 有 | RTL->IF: `io_ooo_to_mem_issueVldu_1_ready` | 有 | 没有 |
| `io_ooo_to_mem_issueVldu_1_valid` | `` | `output` | `input` | 有 | 有 | IF->RTL: `io_ooo_to_mem_issueVldu_1_valid`<br>RTL->IF: `io_ooo_to_mem_issueVldu_1_valid` | 有 | 有 |

## 5. 缺失项汇总

### 5.1 缺少 connect 连接

数量：0

本轮已将 `io_mem_to_ooo_int_wb_agent_agent` 统一收敛到 V2 顶层真实 `writebackLda/Sta/Std` 命名，interface、xaction、monitor 和 connect 均为 109 个字段，且 connect 活动分支与非活动分支均为同名 `RTL->IF` 连接。

当前静态扫描结果：`interface=109`、`xaction=109`、`monitor=109`、`connect=109`、`missing=0`。

旧 V3 整数写回聚合别名不再保留，也不再常量化占位；如后续测试框架仍需要旧聚合语义，应在测试框架适配 plan 中重新定义对应 V2 真实语义来源，而不是在 DUT interface 层恢复旧字段名。

## 6. dut_inst 顶层端口未归属 agent 列表

当前 V2 `MemBlock` 顶层端口除 `clock/reset` 外，未被 `tb/*_agent_connect.sv` 活动分支直接归属到 agent 的端口数量为：360。这些端口仍在 `dut_inst.sv` 中实例化，但没有进入现有 agent。

### 6.1 按建议分类汇总

| 分类 | 数量 | 新建/扩展 agent 建议 |
|---|---:|---|
| Fetch/ITLB到Mem端口 | 1 | 建议新建 fetch_itlb_mem_monitor_agent 或扩展 itlb_agent。 |
| Frontend reset bypass端口 | 2 | 建议并入 reset/frontend_ctrl_agent 或扩展 other_ctrl_agent。 |
| L2侧PMP response 顶层端口 | 2 | 建议新建 l2_pmp_resp_monitor_agent 或并入 l2_tlb_requestor_agent 的 response checker。 |
| L2侧TLB request/response 顶层端口 | 13 | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| TileLink/总线自动生成端口 | 109 | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| Debug interrupt sink端口 | 1 | 建议扩展 int_sink_agent 或新建 debug_interrupt_sink_agent，不归入 TileLink/bus agent。 |
| Top->Backend旁路控制端口 | 4 | 建议扩展 backendToTopBypass/other_ctrl 类 agent，按 MSI/CLINT time 分组。 |
| Trace/Encoder旁路端口 | 45 | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| WFI/低功耗状态端口 | 2 | 建议新建 wfi_power_monitor_agent 或并入 other_ctrl_agent。 |
| other_ctrl/外部控制边界 | 24 | 建议扩展 other_ctrl_agent。 |
| 其他未分类 | 8 | 建议先按 RTL 语义二次分类后再建 agent。 |
| 外部L2 prefetch控制端口 | 6 | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |
| 硬件性能事件输出端口 | 76 | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| 硬件性能事件输入端口 | 68 | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |

### 6.2 逐端口列表

#### Fetch/ITLB到Mem端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|

#### Frontend reset bypass端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_resetInFrontendBypass_fromFrontend` | `input` | `` | `io_resetInFrontendBypass_fromFrontend` | 建议并入 reset/frontend_ctrl_agent 或扩展 other_ctrl_agent。 |
| `io_resetInFrontendBypass_toL2Top` | `output` | `` | `io_resetInFrontendBypass_toL2Top` | 建议并入 reset/frontend_ctrl_agent 或扩展 other_ctrl_agent。 |

#### L2侧PMP response 顶层端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_l2_pmp_resp_ld` | `output` | `` | `io_l2_pmp_resp_ld` | 建议新建 l2_pmp_resp_monitor_agent 或并入 l2_tlb_requestor_agent 的 response checker。 |
| `io_l2_pmp_resp_mmio` | `output` | `` | `io_l2_pmp_resp_mmio` | 建议新建 l2_pmp_resp_monitor_agent 或并入 l2_tlb_requestor_agent 的 response checker。 |

#### L2侧TLB request/response 顶层端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_l2_tlb_req_req_valid` | `input` | `` | `io_l2_tlb_req_req_valid` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_req_bits_vaddr` | `input` | `[49:0]` | `io_l2_tlb_req_req_bits_vaddr` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_req_bits_cmd` | `input` | `[2:0]` | `io_l2_tlb_req_req_bits_cmd` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_req_bits_kill` | `input` | `` | `io_l2_tlb_req_req_bits_kill` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_req_bits_isPrefetch` | `input` | `` | `io_l2_tlb_req_req_bits_isPrefetch` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_req_bits_no_translate` | `input` | `` | `io_l2_tlb_req_req_bits_no_translate` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_valid` | `output` | `` | `io_l2_tlb_req_resp_valid` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_bits_paddr_0` | `output` | `[47:0]` | `io_l2_tlb_req_resp_bits_paddr_0` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_bits_pbmt_0` | `output` | `[1:0]` | `io_l2_tlb_req_resp_bits_pbmt_0` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_bits_miss` | `output` | `` | `io_l2_tlb_req_resp_bits_miss` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_bits_excp_0_gpf_ld` | `output` | `` | `io_l2_tlb_req_resp_bits_excp_0_gpf_ld` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_bits_excp_0_pf_ld` | `output` | `` | `io_l2_tlb_req_resp_bits_excp_0_pf_ld` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |
| `io_l2_tlb_req_resp_bits_excp_0_af_ld` | `output` | `` | `io_l2_tlb_req_resp_bits_excp_0_af_ld` | 建议新建 l2_tlb_requestor_agent；不要接到当前内部 L2TLB responder agent。 |

#### TileLink/总线自动生成端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `auto_inner_frontendBridge_instr_uncache_in_a_ready` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_in_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_in_a_valid` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_in_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_in_a_bits_address` | `input` | `[47:0]` | `auto_inner_frontendBridge_instr_uncache_in_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_in_d_valid` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_in_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_in_d_bits_source` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_in_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_in_d_bits_data` | `output` | `[63:0]` | `auto_inner_frontendBridge_instr_uncache_in_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_in_d_bits_corrupt` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_in_d_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_a_ready` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_out_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_a_valid` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_out_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_a_bits_param` | `output` | `[2:0]` | `auto_inner_frontendBridge_instr_uncache_out_a_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_a_bits_address` | `output` | `[47:0]` | `auto_inner_frontendBridge_instr_uncache_out_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_a_bits_corrupt` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_out_a_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_ready` | `output` | `` | `auto_inner_frontendBridge_instr_uncache_out_d_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_valid` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_out_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_opcode` | `input` | `[3:0]` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_param` | `input` | `[1:0]` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_size` | `input` | `[2:0]` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_source` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_sink` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_sink` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_denied` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_denied` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_data` | `input` | `[63:0]` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_instr_uncache_out_d_bits_corrupt` | `input` | `` | `auto_inner_frontendBridge_instr_uncache_out_d_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_ready` | `output` | `` | `auto_inner_frontendBridge_icachectrl_in_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_valid` | `input` | `` | `auto_inner_frontendBridge_icachectrl_in_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_opcode` | `input` | `[3:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_param` | `input` | `[2:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_size` | `input` | `[1:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_source` | `input` | `[4:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_address` | `input` | `[29:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_mask` | `input` | `[7:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_mask` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_data` | `input` | `[63:0]` | `auto_inner_frontendBridge_icachectrl_in_a_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_a_bits_corrupt` | `input` | `` | `auto_inner_frontendBridge_icachectrl_in_a_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_ready` | `input` | `` | `auto_inner_frontendBridge_icachectrl_in_d_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_valid` | `output` | `` | `auto_inner_frontendBridge_icachectrl_in_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_opcode` | `output` | `[3:0]` | `auto_inner_frontendBridge_icachectrl_in_d_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_param` | `output` | `[1:0]` | `auto_inner_frontendBridge_icachectrl_in_d_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_size` | `output` | `[1:0]` | `auto_inner_frontendBridge_icachectrl_in_d_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_source` | `output` | `[4:0]` | `auto_inner_frontendBridge_icachectrl_in_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_sink` | `output` | `` | `auto_inner_frontendBridge_icachectrl_in_d_bits_sink` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_denied` | `output` | `` | `auto_inner_frontendBridge_icachectrl_in_d_bits_denied` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_data` | `output` | `[63:0]` | `auto_inner_frontendBridge_icachectrl_in_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_in_d_bits_corrupt` | `output` | `` | `auto_inner_frontendBridge_icachectrl_in_d_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_ready` | `input` | `` | `auto_inner_frontendBridge_icachectrl_out_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_valid` | `output` | `` | `auto_inner_frontendBridge_icachectrl_out_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_opcode` | `output` | `[3:0]` | `auto_inner_frontendBridge_icachectrl_out_a_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_size` | `output` | `[1:0]` | `auto_inner_frontendBridge_icachectrl_out_a_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_source` | `output` | `[4:0]` | `auto_inner_frontendBridge_icachectrl_out_a_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_address` | `output` | `[29:0]` | `auto_inner_frontendBridge_icachectrl_out_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_mask` | `output` | `[7:0]` | `auto_inner_frontendBridge_icachectrl_out_a_bits_mask` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_data` | `output` | `[63:0]` | `auto_inner_frontendBridge_icachectrl_out_a_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_d_ready` | `output` | `` | `auto_inner_frontendBridge_icachectrl_out_d_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_d_valid` | `input` | `` | `auto_inner_frontendBridge_icachectrl_out_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_d_bits_opcode` | `input` | `[3:0]` | `auto_inner_frontendBridge_icachectrl_out_d_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_d_bits_size` | `input` | `[1:0]` | `auto_inner_frontendBridge_icachectrl_out_d_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_d_bits_source` | `input` | `[4:0]` | `auto_inner_frontendBridge_icachectrl_out_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icachectrl_out_d_bits_data` | `input` | `[63:0]` | `auto_inner_frontendBridge_icachectrl_out_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_a_ready` | `output` | `` | `auto_inner_frontendBridge_icache_in_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_a_valid` | `input` | `` | `auto_inner_frontendBridge_icache_in_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_a_bits_source` | `input` | `[3:0]` | `auto_inner_frontendBridge_icache_in_a_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_a_bits_address` | `input` | `[47:0]` | `auto_inner_frontendBridge_icache_in_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_d_valid` | `output` | `` | `auto_inner_frontendBridge_icache_in_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_d_bits_opcode` | `output` | `[3:0]` | `auto_inner_frontendBridge_icache_in_d_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_d_bits_source` | `output` | `[3:0]` | `auto_inner_frontendBridge_icache_in_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_d_bits_data` | `output` | `[255:0]` | `auto_inner_frontendBridge_icache_in_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_in_d_bits_corrupt` | `output` | `` | `auto_inner_frontendBridge_icache_in_d_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_ready` | `input` | `` | `auto_inner_frontendBridge_icache_out_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_valid` | `output` | `` | `auto_inner_frontendBridge_icache_out_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_opcode` | `output` | `[3:0]` | `auto_inner_frontendBridge_icache_out_a_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_param` | `output` | `[2:0]` | `auto_inner_frontendBridge_icache_out_a_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_size` | `output` | `[2:0]` | `auto_inner_frontendBridge_icache_out_a_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_source` | `output` | `[3:0]` | `auto_inner_frontendBridge_icache_out_a_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_address` | `output` | `[47:0]` | `auto_inner_frontendBridge_icache_out_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_user_alias` | `output` | `[1:0]` | `auto_inner_frontendBridge_icache_out_a_bits_user_alias` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_user_reqSource` | `output` | `[4:0]` | `auto_inner_frontendBridge_icache_out_a_bits_user_reqSource` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_user_needHint` | `output` | `` | `auto_inner_frontendBridge_icache_out_a_bits_user_needHint` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_mask` | `output` | `[31:0]` | `auto_inner_frontendBridge_icache_out_a_bits_mask` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_data` | `output` | `[255:0]` | `auto_inner_frontendBridge_icache_out_a_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_a_bits_corrupt` | `output` | `` | `auto_inner_frontendBridge_icache_out_a_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_ready` | `output` | `` | `auto_inner_frontendBridge_icache_out_d_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_valid` | `input` | `` | `auto_inner_frontendBridge_icache_out_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_opcode` | `input` | `[3:0]` | `auto_inner_frontendBridge_icache_out_d_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_param` | `input` | `[1:0]` | `auto_inner_frontendBridge_icache_out_d_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_size` | `input` | `[2:0]` | `auto_inner_frontendBridge_icache_out_d_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_source` | `input` | `[3:0]` | `auto_inner_frontendBridge_icache_out_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_sink` | `input` | `[9:0]` | `auto_inner_frontendBridge_icache_out_d_bits_sink` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_denied` | `input` | `` | `auto_inner_frontendBridge_icache_out_d_bits_denied` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_data` | `input` | `[255:0]` | `auto_inner_frontendBridge_icache_out_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_frontendBridge_icache_out_d_bits_corrupt` | `input` | `` | `auto_inner_frontendBridge_icache_out_d_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_ready` | `input` | `` | `auto_inner_ptw_to_l2_buffer_out_a_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_valid` | `output` | `` | `auto_inner_ptw_to_l2_buffer_out_a_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_opcode` | `output` | `[3:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_param` | `output` | `[2:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_size` | `output` | `[2:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_source` | `output` | `[3:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_address` | `output` | `[47:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_address` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_user_reqSource` | `output` | `[4:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_user_reqSource` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_mask` | `output` | `[31:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_mask` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_data` | `output` | `[255:0]` | `auto_inner_ptw_to_l2_buffer_out_a_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_a_bits_corrupt` | `output` | `` | `auto_inner_ptw_to_l2_buffer_out_a_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_ready` | `output` | `` | `auto_inner_ptw_to_l2_buffer_out_d_ready` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_valid` | `input` | `` | `auto_inner_ptw_to_l2_buffer_out_d_valid` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_opcode` | `input` | `[3:0]` | `auto_inner_ptw_to_l2_buffer_out_d_bits_opcode` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_param` | `input` | `[1:0]` | `auto_inner_ptw_to_l2_buffer_out_d_bits_param` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_size` | `input` | `[2:0]` | `auto_inner_ptw_to_l2_buffer_out_d_bits_size` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_source` | `input` | `[3:0]` | `auto_inner_ptw_to_l2_buffer_out_d_bits_source` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_sink` | `input` | `[9:0]` | `auto_inner_ptw_to_l2_buffer_out_d_bits_sink` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_denied` | `input` | `` | `auto_inner_ptw_to_l2_buffer_out_d_bits_denied` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_data` | `input` | `[255:0]` | `auto_inner_ptw_to_l2_buffer_out_d_bits_data` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
| `auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt` | `input` | `` | `auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt` | 建议新建或复用 TileLink/bus 边界 agent，按 uncache、icachectrl、icache、dcache 等通道再拆子 agent。 |
#### Top->Backend旁路控制端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_fromTopToBackend_msiInfo_valid` | `input` | `` | `io_fromTopToBackend_msiInfo_valid` | 建议扩展 backendToTopBypass/other_ctrl 类 agent，按 MSI/CLINT time 分组。 |
| `io_fromTopToBackend_msiInfo_bits` | `input` | `[12:0]` | `io_fromTopToBackend_msiInfo_bits` | 建议扩展 backendToTopBypass/other_ctrl 类 agent，按 MSI/CLINT time 分组。 |
| `io_fromTopToBackend_clintTime_valid` | `input` | `` | `io_fromTopToBackend_clintTime_valid` | 建议扩展 backendToTopBypass/other_ctrl 类 agent，按 MSI/CLINT time 分组。 |
| `io_fromTopToBackend_clintTime_bits` | `input` | `[63:0]` | `io_fromTopToBackend_clintTime_bits` | 建议扩展 backendToTopBypass/other_ctrl 类 agent，按 MSI/CLINT time 分组。 |

#### Debug interrupt sink端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `auto_inner_debug_int_sink_in_0` | `input` | `` | `auto_inner_debug_int_sink_in_0` | 建议扩展 int_sink_agent 或新建 debug_interrupt_sink_agent，不归入 TileLink/bus agent。 |

#### Trace/Encoder旁路端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_traceCoreInterfaceBypass_fromBackend_fromEncoder_enable` | `output` | `` | `io_traceCoreInterfaceBypass_fromBackend_fromEncoder_enable` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_fromEncoder_stall` | `output` | `` | `io_traceCoreInterfaceBypass_fromBackend_fromEncoder_stall` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_priv` | `input` | `[2:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_priv` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_mstatus` | `input` | `[63:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_mstatus` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_cause` | `input` | `[63:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_cause` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_tval` | `input` | `[49:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_tval` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_valid` | `input` | `` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_valid` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iaddr` | `input` | `[49:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iaddr` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset` | `input` | `[3:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_itype` | `input` | `[3:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_itype` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire` | `input` | `[6:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ilastsize` | `input` | `` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ilastsize` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_valid` | `input` | `` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_valid` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iaddr` | `input` | `[49:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iaddr` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset` | `input` | `[3:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_itype` | `input` | `[3:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_itype` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire` | `input` | `[6:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ilastsize` | `input` | `` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ilastsize` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_valid` | `input` | `` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_valid` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iaddr` | `input` | `[49:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iaddr` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset` | `input` | `[3:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_itype` | `input` | `[3:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_itype` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire` | `input` | `[6:0]` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ilastsize` | `input` | `` | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ilastsize` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_fromEncoder_enable` | `input` | `` | `io_traceCoreInterfaceBypass_toL2Top_fromEncoder_enable` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_fromEncoder_stall` | `input` | `` | `io_traceCoreInterfaceBypass_toL2Top_fromEncoder_stall` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_priv` | `output` | `[2:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_priv` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_mstatus` | `output` | `[63:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_mstatus` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_cause` | `output` | `[63:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_cause` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_tval` | `output` | `[49:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_tval` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_valid` | `output` | `` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_valid` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iaddr` | `output` | `[49:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iaddr` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_itype` | `output` | `[3:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_itype` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iretire` | `output` | `[6:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iretire` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_ilastsize` | `output` | `` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_ilastsize` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_valid` | `output` | `` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_valid` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iaddr` | `output` | `[49:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iaddr` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_itype` | `output` | `[3:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_itype` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iretire` | `output` | `[6:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iretire` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_ilastsize` | `output` | `` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_ilastsize` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_valid` | `output` | `` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_valid` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iaddr` | `output` | `[49:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iaddr` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_itype` | `output` | `[3:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_itype` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iretire` | `output` | `[6:0]` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iretire` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |
| `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_ilastsize` | `output` | `` | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_ilastsize` | 建议新建 trace_encoder_bypass_agent，后续按 trace/retire 场景决定是否进 RM。 |

#### WFI/低功耗状态端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_wfi_wfiReq` | `input` | `` | `io_wfi_wfiReq` | 建议新建 wfi_power_monitor_agent 或并入 other_ctrl_agent。 |
| `io_wfi_wfiSafe` | `output` | `` | `io_wfi_wfiSafe` | 建议新建 wfi_power_monitor_agent 或并入 other_ctrl_agent。 |

#### other_ctrl/外部控制边界

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_dft_ram_hold` | `input` | `` | `io_dft_ram_hold` | 建议扩展 other_ctrl_agent。 |
| `io_dft_ram_bypass` | `input` | `` | `io_dft_ram_bypass` | 建议扩展 other_ctrl_agent。 |
| `io_dft_ram_bp_clken` | `input` | `` | `io_dft_ram_bp_clken` | 建议扩展 other_ctrl_agent。 |
| `io_dft_ram_aux_clk` | `input` | `` | `io_dft_ram_aux_clk` | 建议扩展 other_ctrl_agent。 |
| `io_dft_ram_aux_ckbp` | `input` | `` | `io_dft_ram_aux_ckbp` | 建议扩展 other_ctrl_agent。 |
| `io_dft_ram_mcp_hold` | `input` | `` | `io_dft_ram_mcp_hold` | 建议扩展 other_ctrl_agent。 |
| `io_dft_cgen` | `input` | `` | `io_dft_cgen` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_lgc_rst_n` | `input` | `` | `io_dft_reset_lgc_rst_n` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_mode` | `input` | `` | `io_dft_reset_mode` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_scan_mode` | `input` | `` | `io_dft_reset_scan_mode` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_ram_hold` | `output` | `` | `io_dft_frnt_ram_hold` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_ram_bypass` | `output` | `` | `io_dft_frnt_ram_bypass` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_ram_bp_clken` | `output` | `` | `io_dft_frnt_ram_bp_clken` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_ram_aux_clk` | `output` | `` | `io_dft_frnt_ram_aux_clk` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_ram_aux_ckbp` | `output` | `` | `io_dft_frnt_ram_aux_ckbp` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_ram_mcp_hold` | `output` | `` | `io_dft_frnt_ram_mcp_hold` | 建议扩展 other_ctrl_agent。 |
| `io_dft_frnt_cgen` | `output` | `` | `io_dft_frnt_cgen` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_frnt_lgc_rst_n` | `output` | `` | `io_dft_reset_frnt_lgc_rst_n` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_frnt_mode` | `output` | `` | `io_dft_reset_frnt_mode` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_frnt_scan_mode` | `output` | `` | `io_dft_reset_frnt_scan_mode` | 建议扩展 other_ctrl_agent。 |
| `io_dft_bcknd_cgen` | `output` | `` | `io_dft_bcknd_cgen` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_bcknd_lgc_rst_n` | `output` | `` | `io_dft_reset_bcknd_lgc_rst_n` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_bcknd_mode` | `output` | `` | `io_dft_reset_bcknd_mode` | 建议扩展 other_ctrl_agent。 |
| `io_dft_reset_bcknd_scan_mode` | `output` | `` | `io_dft_reset_bcknd_scan_mode` | 建议扩展 other_ctrl_agent。 |

#### 其他未分类

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_topDownInfo_fromL2Top_l2Miss` | `input` | `` | `io_topDownInfo_fromL2Top_l2Miss` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_fromL2Top_l3Miss` | `input` | `` | `io_topDownInfo_fromL2Top_l3Miss` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_toBackend_lqEmpty` | `output` | `` | `io_topDownInfo_toBackend_lqEmpty` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_toBackend_sqEmpty` | `output` | `` | `io_topDownInfo_toBackend_sqEmpty` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_toBackend_l1Miss` | `output` | `` | `io_topDownInfo_toBackend_l1Miss` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_toBackend_noUopsIssued` | `input` | `` | `io_topDownInfo_toBackend_noUopsIssued` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_toBackend_l2TopMiss_l2Miss` | `output` | `` | `io_topDownInfo_toBackend_l2TopMiss_l2Miss` | 建议先按 RTL 语义二次分类后再建 agent。 |
| `io_topDownInfo_toBackend_l2TopMiss_l3Miss` | `output` | `` | `io_topDownInfo_toBackend_l2TopMiss_l3Miss` | 建议先按 RTL 语义二次分类后再建 agent。 |

#### 外部L2 prefetch控制端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_outer_l2PfCtrl_l2_pf_master_en` | `output` | `` | `io_outer_l2PfCtrl_l2_pf_master_en` | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |
| `io_outer_l2PfCtrl_l2_pf_recv_en` | `output` | `` | `io_outer_l2PfCtrl_l2_pf_recv_en` | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |
| `io_outer_l2PfCtrl_l2_pbop_en` | `output` | `` | `io_outer_l2PfCtrl_l2_pbop_en` | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |
| `io_outer_l2PfCtrl_l2_vbop_en` | `output` | `` | `io_outer_l2PfCtrl_l2_vbop_en` | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |
| `io_outer_l2PfCtrl_l2_tp_en` | `output` | `` | `io_outer_l2PfCtrl_l2_tp_en` | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |
| `io_outer_l2PfCtrl_l2_pf_delay_latency` | `output` | `[9:0]` | `io_outer_l2PfCtrl_l2_pf_delay_latency` | 建议新建 l2_prefetch_ctrl_monitor_agent 或并入 other_ctrl_agent 的只读监控扩展。 |

#### 硬件性能事件输出端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_perf_0_value` | `output` | `[5:0]` | `io_perf_0_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_1_value` | `output` | `[5:0]` | `io_perf_1_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_2_value` | `output` | `[5:0]` | `io_perf_2_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_3_value` | `output` | `[5:0]` | `io_perf_3_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_4_value` | `output` | `[5:0]` | `io_perf_4_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_5_value` | `output` | `[5:0]` | `io_perf_5_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_6_value` | `output` | `[5:0]` | `io_perf_6_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_perf_7_value` | `output` | `[5:0]` | `io_perf_7_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_0_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_0_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_1_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_1_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_2_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_2_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_3_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_3_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_4_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_4_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_5_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_5_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_6_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_6_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_7_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_7_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_8_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_8_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_9_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_9_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_10_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_10_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_11_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_11_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_12_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_12_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_13_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_13_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_14_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_14_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_15_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_15_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_16_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_16_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_17_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_17_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_18_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_18_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_19_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_19_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_20_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_20_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_21_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_21_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_22_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_22_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_23_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_23_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_24_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_24_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_25_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_25_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_26_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_26_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_27_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_27_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_28_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_28_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_29_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_29_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_30_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_30_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_31_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_31_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_32_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_32_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_33_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_33_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_34_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_34_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_35_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_35_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_36_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_36_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_37_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_37_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_38_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_38_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_39_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_39_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_40_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_40_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_41_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_41_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_42_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_42_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_43_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_43_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_44_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_44_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_45_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_45_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_46_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_46_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_47_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_47_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_48_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_48_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_49_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_49_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_50_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_50_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_51_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_51_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_52_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_52_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_53_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_53_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_54_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_54_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_55_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_55_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_56_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_56_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_57_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_57_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_58_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_58_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_59_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_59_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_60_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_60_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_61_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_61_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_62_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_62_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_63_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_63_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_64_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_64_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_65_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_65_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_66_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_66_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
| `io_inner_hc_perfEvents_67_value` | `output` | `[5:0]` | `io_inner_hc_perfEvents_67_value` | 建议新建 perf_event_monitor_agent；DUT output 方向多数场景只需 monitor/coverage，不建议 driver 驱动。 |
#### 硬件性能事件输入端口

| DUT端口 | 方向 | 位宽 | dut_inst连接信号 | 建议 |
|---|---|---|---|---|
| `io_outer_hc_perfEvents_1_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_1_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_2_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_2_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_3_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_3_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_4_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_4_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_5_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_5_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_6_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_6_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_7_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_7_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_8_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_8_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_9_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_9_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_10_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_10_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_11_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_11_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_12_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_12_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_13_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_13_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_14_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_14_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_15_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_15_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_16_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_16_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_17_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_17_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_18_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_18_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_19_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_19_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_20_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_20_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_21_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_21_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_22_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_22_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_23_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_23_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_24_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_24_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_25_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_25_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_26_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_26_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_27_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_27_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_28_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_28_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_29_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_29_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_30_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_30_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_31_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_31_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_32_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_32_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_33_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_33_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_34_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_34_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_35_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_35_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_36_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_36_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_37_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_37_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_38_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_38_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_39_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_39_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_40_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_40_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_41_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_41_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_42_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_42_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_43_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_43_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_44_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_44_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_45_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_45_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_46_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_46_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_47_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_47_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_48_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_48_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_49_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_49_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_50_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_50_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_51_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_51_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_52_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_52_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_53_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_53_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_54_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_54_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_55_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_55_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_56_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_56_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_57_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_57_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_58_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_58_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_59_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_59_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_60_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_60_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_61_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_61_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_62_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_62_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_63_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_63_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_64_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_64_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_65_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_65_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_66_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_66_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_67_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_67_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |
| `io_outer_hc_perfEvents_68_value` | `input` | `[5:0]` | `io_outer_hc_perfEvents_68_value` | 建议新建 perf_event_input_agent 或在 perf_event_agent 中区分 input stimulus/常量源；DUT input 方向不能按纯 monitor 处理。 |

## 7. 结论

- 当前 agent interface 与 xaction、monitor 采集整体闭合；本次静态检查未发现缺 xaction 字段。规则上纯握手 `ready` 字段后续可按语义例外处理，但本轮不依赖该例外。

- 本文统计的“没有 driver 驱动”仅表示当前 driver 源码没有主动赋值该 interface 字段。对于 DUT output 或被动采样类接口，这是预期状态；后续只需要重点关注 DUT input 且测试目标要求主动驱动的未驱动字段。

- 当前 interface 字段的 connect 覆盖已闭合，`io_mem_to_ooo_int_wb_agent_agent` 静态扫描结果为 `interface=109`、`xaction=109`、`monitor=109`、`connect=109`、`missing=0`。该 agent 已全部使用 V2 `writebackLda/Sta/Std` 顶层原生命名，旧 V3 整数写回聚合别名已从 interface、xaction、monitor 和 connect 中删除，不再常量化保留。

- `io_mem_to_ooo_iq_feedback_agent_agent` 的静态字段链完整，scalar STA接口只含SQ key。
  IQ feedback/replay专项已把monitor改为SQ-only raw，并由adapter通过active SQ map/current
  status补齐当前动态实例；VSTU valid在scalar-only范围内显式fatal。

- V2 顶层仍有 360 个端口未归属现有 agent，主要是 TileLink/总线、perf/trace、L2 TLB/PMP、L2 prefetch、WFI/低功耗、外部控制边界端口；如后续测试目标需要覆盖，应按本文分类新增或扩展 agent。
