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

修改前 `memblock_connect.sv` 展开的多个 connect 文件仍引用 V3 聚合接口或旧 internal 层级，例如 `io_ooo_to_mem_intIssue_*`、`io_ooo_to_mem_vecIssue_*`、旧整数写回聚合接口、`io_mem_to_ooo_vecWriteback_*`、旧 L2TLB/PTW `_inner_*` 路径。这些引用在 V2 整核顶层不存在。

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
| 旧整数写回聚合 0/1/2 | 已统一替换为 V2 `io_mem_to_ooo_writebackLda_0/1/2_*` 原生命名。 |
| 旧整数写回聚合 3/4 | 已统一替换为 V2 `io_mem_to_ooo_writebackSta_0/1_*` 原生命名。 |
| 旧整数写回聚合 5/6 | 已统一替换为 V2 `io_mem_to_ooo_writebackStd_0/1_*` 原生命名。 |
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
- V2 内部 response 中的 `s2_entry_perm_g`、`s2_entry_perm_u` 已补齐到 agent interface/xaction/driver/monitor/sequence，并由 `memblock_tlb_entry.pte_g/pte_u` 真实驱动，不再在 active connect 路径固定为 0。
- active=0 时，agent request/response interface 保持非激活置 0，不驱动 DUT 内部 response，避免误声明接管。

### 3.5 L2TLB driver ready 修正

修改前 `L2tlb_agent_agent_driver::drive_idle()` 在 `memblock_sync_pkg::l2tlb_responder_active=1` 时把 `io_ptw_req_0_ready` 置 0。这会导致默认接管后，agent interface 一直不表示可接收 request，`request_fire()` 语义也无法成立。

修改后 `DRV_0` idle 分支在 responder active 时保持 `io_ptw_req_0_ready=1`，未接管时置 0。该修改只影响 L2TLB agent driver，不改变通用测试激励框架主逻辑。

### 3.6 L2TLB S2 `perm_g/u` 真实驱动链路

本轮追加修复 V2 内部 response 中 `s2_entry_perm_g/u` 被固定为 0 的问题。修改前这两个权限位没有进入 `L2TLB_agent` transaction，因此 connect 只能写常量 0；修改后它们和 S1 `perm_g/u` 一样，来自公共 TLB entry 的 `pte_g/pte_u`，由 responder sequence 写入 transaction，再经 driver/interface 驱动到 V2 内部 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u`。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv`，逻辑对象：S2 权限字段定义。
该字段定义把 V2 内部二级页表 response 的 G/U 权限位纳入 transaction，使 sequence、driver、compare 和日志打印都能携带这两个字段。

```systemverilog
// 中文注释：二级页表 entry 的 global/user 权限位。
// 置位来源：L2TLB responder sequence 根据 memblock_tlb_entry.pte_g/pte_u 填入 transaction。
// 作用：driver 通过 interface 真实驱动到 V2 DTLB/L2TLB response 内部连线，避免权限语义被固定为 0。
rand bit io_ptw_resp_bits_s2_entry_perm_g;
rand bit io_ptw_resp_bits_s2_entry_perm_u;
```

中文伪代码：该字段定义在当前 L2TLB responder flow 中承担“保存二级页表 G/U 权限位”的功能。sequence 构造 response transaction 时把公共 TLB entry 中的 `pte_g/pte_u` 写入这两个字段；driver 收到 transaction 后读取这两个字段并驱动 interface；connect active 分支再把 interface 值 force 到 V2 内部 response wire。字段本身不改变查表流程，但消除了 connect 只能固定 0 的旧限制。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，函数：`fill_dtlb_resp_from_entry()`。
该函数负责把 lookup 得到的 `memblock_tlb_entry` 转换成 `L2tlb_agent_agent_xaction` response。输入是公共 TLB entry 和待填充 transaction，副作用是写入完整 response 字段，供 driver 发送给 DUT 内部 DTLB/L2TLB response 通路。

```systemverilog
resp.io_ptw_resp_bits_s2_entry_perm_d = entry.pte_d;
resp.io_ptw_resp_bits_s2_entry_perm_a = entry.pte_a;
resp.io_ptw_resp_bits_s2_entry_perm_g = entry.pte_g;
resp.io_ptw_resp_bits_s2_entry_perm_u = entry.pte_u;
resp.io_ptw_resp_bits_s2_entry_perm_x = entry.pte_x;
resp.io_ptw_resp_bits_s2_entry_perm_w = entry.pte_w;
resp.io_ptw_resp_bits_s2_entry_perm_r = entry.pte_r;
```

中文伪代码：该逻辑在 L2TLB responder 中承担“把查表结果转换为二级页表权限 response”的功能。函数先使用 `entry.pte_d/a` 填入 dirty/accessed 权限，再使用本轮新增的 `entry.pte_g/u` 填入 global/user 权限，随后继续填入 execute/write/read 权限。调用方随后把这个 transaction 交给 L2TLB agent driver，因此这些赋值会成为实际驱动 DUT response 的来源。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，函数：`send_pkt()`。
该函数负责把 sequence 发送的 transaction 驱动到 L2TLB agent interface。输入是 `tr`，主要副作用是通过 `drv_cb` 更新 interface 中的 response 字段。

```systemverilog
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= tr.io_ptw_resp_bits_s2_entry_perm_d;
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= tr.io_ptw_resp_bits_s2_entry_perm_a;
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= tr.io_ptw_resp_bits_s2_entry_perm_g;
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= tr.io_ptw_resp_bits_s2_entry_perm_u;
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= tr.io_ptw_resp_bits_s2_entry_perm_x;
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= tr.io_ptw_resp_bits_s2_entry_perm_w;
vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= tr.io_ptw_resp_bits_s2_entry_perm_r;
```

中文伪代码：该逻辑在 driver 中承担“把 transaction 字段送到 interface”的功能。driver 按字段顺序读取 transaction 中的 S2 权限位，并在当前时钟驱动到 `drv_cb`。本轮新增的 G/U 权限位夹在 A 和 X 权限之间，与 RTL bundle 字段顺序一致；后续 connect 读取 interface 值时，不再需要为这两个字段提供常量占位。

源码位置：`mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`，宏：`MEMBLOCK__L2TLB_AGENT_CONNECT` 的 active 分支。
该 connect 宏负责把 agent interface 和 V2 内部 dtlbRepeater/inner_ptw response wire 对接。active 分支表示 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1`，也就是由 `L2TLB_agent` 接管内部 response。

```systemverilog
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_d = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_d; \
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_a = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_a; \
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_g; \
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_u = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_u; \
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_x = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_x; \
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_w = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_w; \
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_r = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_r; \
```

中文伪代码：该逻辑在 testbench connect 中承担“把 agent response 接到 V2 内部 response wire”的功能。active 分支逐个 force S2 权限字段，D/A 之后接入本轮新增的 G/U，再继续接 X/W/R。这样 DUT 内部 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 的值来自 interface，而 interface 又来自 driver 和 sequence transaction，完成真实驱动链路。关闭 takeover 时 interface 仍保持非激活置 0，不表示 agent 正在响应 DUT。

### 3.7 L2TLB 映射源码落点

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
- V2 内部 response bundle 中的 `s2_entry_perm_g/u` 已由 `L2TLB_agent` transaction/sequence 真实驱动；后续风险只剩 V2 RTL 重新生成后内部 wire 名可能变化，需要按 profile 复查连接点。
- 本轮接入点是生成后 Verilog 内部 wire，后续 V2 RTL 重新生成时这些 `_inner_*` 层级名可能变化，需要按 profile 重新复查。
- 对 V2 无等价字段，本轮为了结构编译闭合采用置 0 或默认值，需要后续按功能分类判断是否应新增 V2 transaction 字段或修改 RM。
- V2 顶层 `io_l2_tlb_req_resp_*`、`io_l2_pmp_resp_*`、`io_outer_l2PfCtrl_*`、`io_wfi_wfiSafe` 当前只在 `dut_inst.sv` 中实例化，尚未进入 agent。后续应先分析是否影响当前主功能、RM/checker 必要观察点或专项 testcase；若不影响当前主功能，暂不接入 agent。该待办已登记到 `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md`。
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

## 7. 本轮 V3-only/V2-only 接口统一处理 review

### 7.1 Review 范围

本轮针对用户提出的“不能一部分删除、一部分宏隔离”的要求，复查并落实 V2 分支接口适配的统一策略：

- 统一采用 V2 分支删除 V3-only 字段和逻辑的方式，不新增 V2/V3 版本宏隔离。
- 对 V2 有而 V3 没有、且当前 V2 DUT 顶层真实存在的关键字段，补齐到 connect、interface、xaction 和 monitor 链路。
- 不修改测试激励框架主逻辑，不引入 V3 兼容路径。

本轮重点覆盖以下源码区域：

```text
mem_ut/ver/ut/memblock/agent/**/src/*_interface.sv
mem_ut/ver/ut/memblock/agent/**/src/*_xaction.sv
mem_ut/ver/ut/memblock/agent/**/src/*_driver.sv
mem_ut/ver/ut/memblock/agent/**/src/*_monitor.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
mem_ut/ver/ut/memblock/seq/base_seq/*.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/*.sv
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
```

### 7.2 修改前逻辑

修改前 V2 分支中还残留部分 V3 顶层接口和 V3 聚合 transaction 字段，例如 `io_ooo_to_mem_intIssue_*`、旧 `io_ooo_to_mem_enqLsq_canAccept`、旧 `enqLsq_resp_*` 读取路径，以及 DCache/SBuffer transaction 中 V3-only TileLink user 字段。这些字段在 V2 整核 `build/rtl/MemBlock.sv` 顶层不存在，继续保留会造成两类问题：

- 编译或层级连接阶段可能引用不存在的 DUT 端口。
- sequence/driver 仍按 V3 聚合接口构造 transaction，和 V2 split issue/enqueue 形态不一致。

修改前也没有完整补齐 V2-only `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0..23` 的 agent 链路，后续如果 store address writeback 需要完整异常向量，会缺少采样字段。

### 7.3 修改后逻辑

本轮统一按 V2 分支处理，不再保留 V3 兼容逻辑：

| 修改类别 | 修改后行为 |
|---|---|
| V3-only 字段 | 从 agent interface、xaction、driver、monitor、connect 和相关 sequence 引用中删除。 |
| V2-only 字段 | 补齐 `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0..23` 的 interface、xaction、monitor 和 connect 链路。 |
| `lintsissue` | 改为 V2 七路 split issue：`issueLda_0..2`、`issueSta_0..1`、`issueStd_0..1`；ready 是 DUT output，driver clocking 中只采样。 |
| LSQ enqueue | 改为 V2 六路 `enqLsq_0..5`，删除 V3 `canAccept` 和 `enqLsq_resp_*` 旧读取路径；公共默认宽度同步为 6。 |
| issue 字段构造 | `issue_field_assigner.sv` 改为写 V2 `issueLda/issueSta/issueStd` 字段，不再写 V3-only STA/STD 字段。 |
| TileLink user 字段 | 删除 `memPageType_NC`、`memBackType_MM` 等 V3-only transaction 赋值。 |

其中 LSQ enqueue 的软件镜像确认不再依赖 V3 `enqLsq_resp_*`，而是使用 sequence 预分配的 `lq_keys/sq_keys` 完成 LQ/SQ key 记录。该变化只让 V2 分支绕开不存在的 DUT response 字段，不改变 testcase 主激励入口。

### 7.4 关键修改点伪代码说明

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`，逻辑对象：`confirm_lsq_candidates` 相关 LSQ enqueue 确认流程。

```systemverilog
// 本轮逻辑要点：V2 不再读取 enqLsq_resp_*，而是使用 sequence 分配阶段记录的 lq_keys/sq_keys。
// 具体源码以当前文件为准，review 关注的是旧 DUT response 依赖已被移除。
```

中文伪代码：该逻辑在 V2 LSQ enqueue flow 中承担“确认本拍已经送入 LSQ 的候选 uid 并维护软件侧 LQ/SQ key 镜像”的功能。执行时先从本拍 sequence 已经选中的候选项中取出 uid，再读取分配阶段保存的 `lq_keys` 或 `sq_keys`；如果当前 uid 是 load，就把对应 LQ key 写入公共状态镜像，如果当前 uid 是 store，就把对应 SQ key 写入公共状态镜像。该流程不再访问 V3 `enqLsq_resp_*`，因此不会等待或读取 V2 RTL 顶层不存在的 response 字段。

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_interface.sv`，逻辑对象：V2 issue ready 方向。

```systemverilog
// 本轮逻辑要点：issueLda/issueSta/issueStd 的 ready 在 drv_cb 中为 input。
// ready 来自 DUT，driver 只能采样，不能驱动。
```

中文伪代码：该逻辑在 V2 split issue agent 中承担“让 driver 判断 DUT 是否接收当前 issue transaction”的功能。每拍 driver 只读取 `issueLda/issueSta/issueStd` 各路 ready，若 valid 和 ready 同时成立，才认为对应 issue transaction 已 fire；driver 不向 ready 赋值，因此不会覆盖 DUT 输出的 backpressure 状态。valid 和 bits 字段仍由 driver 按 transaction 内容驱动到 DUT input。

### 7.5 正确性检查

本轮执行了以下静态检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock
```

结果通过，未发现空白或 patch 格式问题。

本轮还用 V2/V3 `MemBlock.sv` 顶层端口集合扫描 `agent`、`tb`、`seq` 中的 V3-only 引用：

```text
V2 ports 1334
V3 ports 1687
V3-only 921
V2-only 568
agent files_with_v3_only_refs 0
seq files_with_v3_only_refs 0
tb files_with_v3_only_refs 1
```

`tb` 中唯一命中是 `io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid` 包含 `io_fetch_to_mem_itlb_resp_bits_s2_entry_v` 前缀，不是旧 V3 字段残留。该端口在 V2 `build/rtl/MemBlock.sv` 中真实存在。

本轮检查 `dut_inst.sv` 与 V2 顶层端口闭合：

```text
rtl_ports 1334
inst_ports 1334
missing instance ports 0
extra instance ports 0
missing decl 2
MISS_DECL clock
MISS_DECL reset
```

`clock` 和 `reset` 由 testbench 顶层已有 `clk`、`tc_if.rst_n` 提供，不属于 DUT 接线遗漏。

本轮检查 `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0..23`，确认 interface、xaction、monitor、connect 四处均完整覆盖 0 到 23。

本轮检查版本宏隔离：

```bash
rg -n "MEMBLOCK_.*V[23]|V2|V3|KUNMINGHU|kunminghu|MEM_UT_UVM_V" \
  mem_ut/ver/ut/memblock/agent \
  mem_ut/ver/ut/memblock/tb \
  mem_ut/ver/ut/memblock/seq \
  mem_ut/ver/ut/memblock/env/plus.sv
```

结果未发现本轮新增的 V2/V3 版本宏隔离；命中项仅为已有 V2 注释、L2TLB V2 internal 说明和通用 V2 DUT 注释。

### 7.6 编译验证

本轮先执行默认远端编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

第一次默认 `partcmp_op=on` 在 VCS partcomp/KDB design resolution 阶段触发工具 SIGSEGV，并导致后续 `partcmp_op=off` 复跑暴露缓存损坏：

```text
VCS fails to access database file
base_fun/exec/simv.daidir/work.lib++/tdc.sdb
due to 'it is corrupted'.
```

该报错发生在 VCS 数据库/KDB 缓存阶段，不是 SystemVerilog 端口、字段或层级编译错误。按 VCS 提示清理 `base_fun` 编译产物后，使用非 partcomp 编译复跑：

```bash
cd mem_ut/ver/ut/memblock/sim
make clean mode=base_fun
make eda_compile tc=tc_sanity mode=base_fun partcmp_op=off
```

最终结果：

```text
Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)
```

### 7.7 Subagent 最终 review 结论

最后一轮 subagent 只读 review 结论为：

```text
最后一轮 review 未发现阻塞问题。
```

subagent 复查确认：

- 未发现 `agent`、`tb`、`seq` 中继续引用 V3-only 顶层端口的阻塞问题。
- 未发现新增 V2/V3 宏隔离。
- `writebackSta_0 exceptionVec[0:23]` 在 interface、xaction、monitor、connect 四处完整。
- `lintsissue` 七路 V2 split issue 映射一致，ready 在 driver clocking 中为 input。
- LSQ enqueue 已按 V2 六路处理，未发现 V3 `canAccept/resp` 旧引用。

非阻塞注意项：`mem_ut/ver/ut/memblock/sim/.compileDeletedAssertionPC` 是编译临时产物，后续提交时不应纳入 git。

### 7.8 Plan 对齐检查补充

本轮是用户在既有 V2 DUT coding 基础上的增量修正要求，核心新增约束是“删除和宏隔离统一一种方式去做”。当前源码实现与该约束一致：采用 V2 分支删除 V3-only、补齐 V2-only 的单一策略，未新增 V2/V3 宏隔离。

本轮未移动既有 plan 文件；后续测试框架行为适配仍应另行按 `AI_DOC/plan/test_framework/plan/undo` 下对应 plan 推进。

### 7.9 本轮接口 review 问题增量修复

本节记录 2026-07-08 基于 `AI_DOC/analysis/interface/v2/mem_ut_v2_interface_alignment_issue_review_20260708.md` 的增量代码修复。处理原则是：非测试框架、非需用户确认的问题直接修复；涉及测试框架运行期语义的问题只写入 plan；涉及用户确认的问题保留在 review 文档。

#### 7.9.1 `io_reset_backend` 漏接修复

功能特性：`io_reset_backend` 是 V2 DUT 顶层 output，表示后端 reset 状态。修改前 `other_ctrl_agent` 的 interface 和 monitor 已有该字段，但 connect 未从 RTL 驱动到 interface，monitor 会读取悬空值。

修改后逻辑：在 `MEMBLOCK_UT` 和非 `MEMBLOCK_UT` 两个分支都从 V2 RTL 顶层 `io_reset_backend` force 到 `other_ctrl_agent` interface，保证 monitor 采样真实 DUT output。

源码位置：`mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv`，逻辑对象：`MEMBLOCK__OTHER_CTRL_AGENT_CONNECT` 宏中的 RTL 到 interface 连接。

```systemverilog
force U_IF_NAME.io_reset_backend = RTL_PATH.io_reset_backend;
force U_IF_NAME.io_outer_cpu_halt = RTL_PATH.io_outer_cpu_halt;
```

中文伪代码：该逻辑在 V2 DUT 接口适配中负责把 RTL 顶层 output 同步到 `other_ctrl_agent` interface。每个仿真分支初始化时，先把 `io_reset_backend` 从 `RTL_PATH` force 到 `U_IF_NAME`，后续 monitor 从 interface 读取的就是 DUT 真实 reset backend 状态；随后继续连接 `io_outer_cpu_halt`，保持同类 DUT output 的采样路径一致。本段不调用子函数，副作用是消除 `io_reset_backend` 悬空采样和潜在 X/Z 误报。

正确性检查：`MEMBLOCK_UT` 和非 `MEMBLOCK_UT` 分支均已补齐同一连接；`other_ctrl_agent_agent_interface.sv` 中 `io_reset_backend` 在 `drv_cb` 和 `mon_cb` 均为 `input`，driver 不会驱动该 DUT output。

#### 7.9.2 `io_outer_cpu_halt` clocking 方向修复

功能特性：`io_outer_cpu_halt` 是 V2 DUT 顶层 output，测试环境只能采样，不能由 driver 驱动。修改前该字段在 `drv_cb` 中声明为 `output`，方向不符合 DUT output 的职责边界。

修改后逻辑：将 `io_outer_cpu_halt` 在 `drv_cb` 中改为 `input`，与 `mon_cb` 方向保持一致，避免后续 driver 扩展时误驱 DUT output。

源码位置：`mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_interface.sv`，逻辑对象：`other_ctrl_agent_agent_interface.drv_cb`。

```systemverilog
input  io_reset_backend;

input  io_outer_cpu_halt;
```

中文伪代码：该逻辑在 `other_ctrl_agent` interface 中负责声明 driver clocking block 可访问的信号方向。driver clocking block 只能读取 `io_reset_backend` 和 `io_outer_cpu_halt` 这两个 DUT output，不能对它们赋值；monitor clocking block 也按 input 方向采样同一组信号。该修改没有新增驱动逻辑，副作用是把 interface 方向约束和 V2 RTL 端口方向对齐。

正确性检查：扫描当前 `other_ctrl_agent` driver 未发现对 `io_outer_cpu_halt` 的赋值；方向修复后即使后续添加 driver 逻辑，也不能通过 `drv_cb` 误驱该信号。

#### 7.9.3 int writeback 非公共状态字段删除

功能特性：`io_mem_to_ooo_int_wb_agent` 早期 V2 适配中仍保留少量无 V2 顶层来源的旧写回聚合字段。本轮最终收敛后，这些字段以及后续所有旧写回聚合别名都已从 interface、xaction、monitor 和 connect 中删除。

修改前逻辑：driver 曾经暴露过无 V2 顶层来源的 ready/isRVC/pdest 等字段，monitor 也会对这些无来源字段做采样或 X/Z 检查，容易造成悬空字段和误报。

修改后逻辑：driver 保持空实现，不再驱动任何 int writeback 字段；interface 和 monitor 只保留 V2 `writebackLda/Sta/Std` 顶层真实 output。该阶段的删除项已被 7.10 的“全量 V2 命名统一”覆盖。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_driver.sv`，逻辑对象：`send_pkt()` 和 `drive_idle()`。

```systemverilog
task io_mem_to_ooo_int_wb_agent_agent_driver::send_pkt(io_mem_to_ooo_int_wb_agent_agent_xaction tr);

endtask:send_pkt

task io_mem_to_ooo_int_wb_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

endtask:drive_idle
```

中文伪代码：该逻辑在 int writeback agent driver 中表示当前 agent 只做 DUT output 被动采样。`send_pkt()` 不从 transaction 读取字段，也不向 interface 写信号；`drive_idle()` 在 reset 和 idle 周期同样不赋值任何字段。该 task 没有调用子函数，副作用是保证 V2 写回 output 不被 driver 误驱动。

正确性检查：当前 interface 的 `drv_cb` 和 `mon_cb` 对 109 个 V2 写回字段均声明为 `input`；driver 源码中没有对这些字段的赋值路径。

#### 7.9.4 测试框架待办和需确认项同步

本轮没有直接修改测试框架运行期语义。以下问题已转入测试框架 plan：

- `dispatch_raw_int_wb_t` 中 `rob_flag/lq_flag/lq_value/sq_flag/sq_value/exception_vec[]` 的 V2 语义来源，记录在 `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_plan_20260708.md`。
- 顶层 agent monitor 是否恢复 `mon_item_port.write(mon_tr)`、V2 新增顶层 output 是否影响主功能，记录在 `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md`。

以下问题继续保留为需用户确认或后续专项分析：

- `io_l2_tlb_req_resp_*`、`io_l2_pmp_resp_*`、`io_outer_l2PfCtrl_*` 和 `io_wfi_wfiSafe` 是否需要接入 agent、RM、checker 或 coverage。

#### 7.9.5 DUT output 的 `drv_cb` 方向增量修复

功能特性：本轮再次扫描 `tb/*_agent_connect.sv` 的 `MEMBLOCK_UT` 活动分支，发现 89 个字段的连接方向是 `RTL -> interface`，但对应 interface 的 driver clocking block 仍声明为 `output`。这些字段属于 DUT output 或 DUT ready 采样信号，driver 不应该拥有写权限。

修改前逻辑：`io_mem_to_ooo_ctrl_agent` 的 8 个 `lsqio_loadMmio/storeMmio` 字段、`io_mem_to_ooo_iq_feedback_agent` 的 6 个 `vstuIqFeedback` replay 字段、`io_mem_to_ooo_vec_wb_agent` 的 73 个 `writebackVldu_0/1` 字段，以及 `vecissue_agent` 的 2 个 `issueVldu ready` 字段在 `drv_cb` 中是 `output`。扫描 driver 后确认当前没有实际赋值这些字段，但方向声明不符合“driver 只能驱动 DUT input”的接口边界。

修改后逻辑：上述 89 个字段均改为 `drv_cb input`，与 `mon_cb input` 和 connect 中 `RTL -> interface` 的方向一致。该修改没有改动 connect、xaction、driver、monitor 的功能逻辑，也没有触碰测试框架 raw queue、RM 或 sequence 行为。

涉及源码：

| 文件 | 修复内容 |
|---|---|
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv` | 8 个 `lsqio_loadMmio/storeMmio` 字段由 `drv_cb output` 改为 `input`。 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_interface.sv` | 6 个 `vstuIqFeedback` replay 字段由 `drv_cb output` 改为 `input`。 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_interface.sv` | 73 个 `writebackVldu_0/1` 字段由 `drv_cb output` 改为 `input`。 |
| `mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_interface.sv` | `io_ooo_to_mem_issueVldu_0_ready`、`io_ooo_to_mem_issueVldu_1_ready` 由 `drv_cb output` 改为 `input`。 |

正确性检查：

- 修复前 driver 扫描确认这些字段没有被 driver 实际赋值，修改方向不会删除已有驱动行为。
- monitor 仍按 `mon_cb input` 采样这些字段，采样链路保持不变。
- connect 中这些字段仍由 V2 RTL 驱动到 interface，方向修复后 interface 权限边界和 V2 DUT 端口方向一致。
- 本轮重新执行活动 `MEMBLOCK_UT` 分支方向扫描，已确认不再存在 `RTL -> interface` 但 `drv_cb output` 的字段。

#### 7.9.6 本轮静态验证

本轮执行的静态检查包括：

```bash
rg -n "<旧写回聚合命名关键字>" \
  mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src \
  mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv
```

中文伪代码：该命令在 int writeback agent 源码目录和对应 connect 文件中查找旧写回聚合命名及其旧 bundle 字段。命令没有命中时，说明 interface、xaction、monitor、driver 和 connect 中已经没有旧聚合别名残留。该检查只验证源码命名收敛，不替代测试框架语义适配验证。

### 7.10 `io_mem_to_ooo_int_wb_agent` 全量 V2 命名统一

功能特性：根据用户要求“全部更换成 V2 的，不要混合命名”，本轮将 `io_mem_to_ooo_int_wb_agent_agent` 从旧整数写回聚合别名彻底收敛到 V2 `MemBlock` 顶层真实 `writebackLda/Sta/Std` output 端口命名。

修改前逻辑：interface、xaction、monitor 和 connect 同时存在旧写回聚合字段和 V2 `writebackLda/Sta/Std` 字段。connect 右侧已经接到 V2 RTL 顶层，但左侧 interface 和 raw queue 主路径仍使用旧聚合命名，且部分 ROB index 位宽仍是旧 9 bit。

修改后逻辑：四个文件均按 `build_memblock/rtl/MemBlock.sv` 中 109 个 V2 顶层 writeback output 重建。字段名、connect 左右侧、xaction 字段、monitor 采样和 X/Z 检查均使用 V2 原生命名；`robIdx_value` 位宽按 V2 顶层统一为 `[7:0]`。旧聚合别名不再保留，也不再作为常量占位字段存在。

涉及源码：

| 文件 | 修改内容 |
|---|---|
| `mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv` | 两个分支均改为 109 条 V2 同名 `RTL->IF` force。 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv` | interface logic、`drv_cb`、`mon_cb` 全部改为 V2 `writebackLda/Sta/Std` 字段。 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_xaction.sv` | transaction 字段、空约束、UVM field、打印和 compare 均重建为 109 个 V2 字段。 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv` | 本地变量、`mon_cb` 采样、X/Z 检查和 raw queue 写入均改用 V2 字段。 |

源码位置：`mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv`，宏：`MEMBLOCK__IO_MEM_TO_OOO_INT_WB_AGENT_CONNECT`。
该宏现在只负责把 V2 `MemBlock` 顶层写回真实 output 接到同名 interface 字段。

```systemverilog
force U_IF_NAME.io_mem_to_ooo_writebackLda_0_valid = RTL_PATH.io_mem_to_ooo_writebackLda_0_valid;
force U_IF_NAME.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value = RTL_PATH.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value;
force U_IF_NAME.io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value = RTL_PATH.io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value;
```

中文伪代码：该 connect 逻辑在本 agent 中承担“把 V2 顶层写回真实输出映射到被动采样 interface”的功能。宏展开后逐项读取 V2 `writebackLda`、`writebackSta` 和 `writebackStd` 顶层端口，并 force 到同名 interface 字段；monitor 后续只从这些同名字段采样。该逻辑不再创建旧聚合别名，也不再从非 L2TLB 内部层级补旧字段。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv`，逻辑对象：V2 字段声明和 clocking block。

```systemverilog
logic io_mem_to_ooo_writebackLda_0_valid;
logic [7:0] io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value;
logic [7:0] io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value;
```

中文伪代码：该 interface 逻辑声明 V2 顶层真实 output 对应的采样字段。每个字段在 `drv_cb` 和 `mon_cb` 中都按 `input` 暴露，表示 driver 不能写这些 DUT output，monitor 可以在采样点读取它们。位宽直接来自 V2 Verilog 顶层端口，ROB index value 不再沿用旧 9 bit 宽度。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv`，函数/task：`mon_data()` raw writeback 写入。
该 monitor 负责把 V2 LDA/STA/STD 写回采样转成 `memblock_sync_pkg::dispatch_raw_int_wb_t`。

```systemverilog
if (io_mem_to_ooo_writebackLda_0_valid) begin
    raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
    raw_int_wb.port_id = 0;
    raw_int_wb.rob_value = io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value;
    raw_int_wb.exception_vec[13] = io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13;
    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
end
```

中文伪代码：该 raw 写入逻辑在 LDA0 写回有效时创建一条 raw int writeback 事件。事件先清空为默认空值，再记录端口号、ROB index value 和 V2 顶层真实导出的异常位，最后压入公共 raw queue。LDA1/LDA2、STA0/STA1、STD0/STD1 使用同样的 V2 原生命名路径；STD 端口只写 V2 顶层真实导出的 valid 和 ROB value，不补不存在的 flag 或 SQ 字段。

文档同步：`AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md` 已同步更新。`io_mem_to_ooo_int_wb_agent_agent` 汇总为 `interface=109`、`xaction=109`、`connect=109`、`monitor=109`、`driver=0`，4.8 逐信号矩阵只列 V2 `writebackLda/Sta/Std` 字段。

正确性检查：

- 源码范围扫描旧写回聚合命名关键字无命中。
- `interface logic=109`、`xaction rand=109`、`extern constraint=109`、constraint body=109、`uvm_field_int=109`。
- `monitor logic=109`、`mon_cb` 采样=109、X/Z 检查=109。
- connect 两个分支共 218 条 force，唯一字段 109 个；左右侧 V2 字段名完全一致，每个字段出现 2 次。
- `robIdx_value` 在 interface、xaction、monitor 和 connect 中均按 V2 `[7:0]` 处理。

风险边界：本轮只统一 DUT interface/transaction/monitor/connect 的字段命名和 V2 顶层真实端口连接，不修改测试激励框架主流程。`dispatch_raw_int_wb_t` 当前仍使用 9 bit ROB value 保存公共 raw 事件，V2 8 bit value 会零扩展进入该公共字段；是否进一步收窄公共状态位宽属于测试框架语义适配范围。

### 7.11 `itlb_agent` S2 entry VMID 字段补齐

功能特性：本轮把 V2 顶层 output `io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid` 并入 `itlb_agent_agent`。该端口属于 `io_fetch_to_mem_itlb_resp_bits_s2_entry_*` 同一组 response bundle，和已接入的 `tag/n/pbmt/ppn/perm/level` 字段语义一致，应由 itlb agent 统一采样。

修改前逻辑：V2 `MemBlock` 顶层和 `dut_inst.sv` 已存在 `io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid`，但 `itlb_agent_agent_interface.sv`、`itlb_agent_connect.sv`、`itlb_agent_agent_xaction.sv` 和 `itlb_agent_agent_monitor.sv` 没有该字段。接口矩阵文档因此把它列入未归属端口，并建议扩展 itlb agent。

修改后逻辑：本轮将该字段补到 itlb agent 的 interface、connect、xaction 和 monitor。字段方向保持 `input`，由 V2 顶层 RTL output 通过 connect force 到 interface，再由 monitor 采样；driver 不驱动该字段。

源码位置：`mem_ut/ver/ut/memblock/tb/itlb_agent_connect.sv`，宏：`MEMBLOCK__ITLB_AGENT_CONNECT`。
该宏负责把 V2 顶层 Fetch/ITLB response 端口接入 itlb agent interface。

```systemverilog
force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag;
force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid;
force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_n = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_n;
```

中文伪代码：该 connect 逻辑在 itlb agent 中承担“把 V2 顶层 itlb response 的 S2 entry 字段送入 interface”的功能。执行时先连接 `tag`，再连接本轮新增的 `vmid`，随后继续连接 `n/pbmt/ppn/perm/level`。这样 monitor 采样到的 S2 entry bundle 不再缺少 VMID 字段。

源码位置：`mem_ut/ver/ut/memblock/agent/itlb_agent_agent/src/itlb_agent_agent_monitor.sv`，函数/task：`mon_data()`。
该 monitor 每拍从 `mon_cb` 采样 itlb interface 字段，并在 XZ 检查打开时检查字段是否为未知态。

```systemverilog
io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid = this.vif.mon_mp.mon_cb.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid;
`TCNT_CHECK_SIG_XZ(io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid,io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid,14);
```

中文伪代码：该逻辑在 monitor 中承担“采样并检查 S2 entry VMID”的功能。每拍 monitor 从 interface 的 `mon_cb` 读取 VMID 到本地变量；当 reset 已释放且 XZ 检查开启时，按 14 位宽检查该字段没有未知态。该字段目前只进入 agent transaction/日志/比较闭合，是否进入测试框架公共状态由后续测试框架适配 plan 决定。

文档同步：`AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md` 已同步更新，`itlb_agent_agent` 汇总从 63 项改为 64 项，并从未归属端口列表移除 `io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid`。

正确性检查：

- V2 `build/rtl/MemBlock.sv`、`dut_inst.sv` 均存在 `io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid`，位宽为 `[13:0]`。
- itlb agent 的 interface、connect、xaction、monitor 静态字段集合已补齐该字段。
- 该字段是 DUT output，被动采样，不需要 driver 驱动。


### 7.12 `dcache_agent` L2 hint/flush 顶层输入合并

功能特性：本轮按“L2 hint/flush 控制端口合并到 dcache agent”的方案，把 V2 顶层 input `io_l2_hint_valid`、`io_l2_hint_bits_sourceId`、`io_l2_hint_bits_isKeyword` 和 `io_l2_flush_done` 并入 `dcache_agent_agent`。这 4 个端口与 dcache/L2 侧 hint、flush 控制相关，当前 `dcache_agent_agent` 已覆盖 dcache TileLink 边界和 hint 相关 `needHint/isKeyword` 字段，因此直接扩展该 agent 比新增独立 agent 更符合当前划分。

修改前逻辑：`dut_inst.sv` 已声明并实例化这 4 个 V2 顶层 input，默认 initial 块将其置 0，但没有进入任何 agent。接口矩阵文档此前把它们列在“L2 hint/flush 控制端口未归属”分类中，建议新建 `l2_hint_flush_agent`。

修改后逻辑：本轮将这 4 个端口补入 `dcache_agent_agent_interface.sv`、`dcache_agent_connect.sv`、`dcache_agent_agent_xaction.sv`、`dcache_agent_agent_driver.sv` 和 `dcache_agent_agent_monitor.sv`。connect 方向为 `IF->RTL`，即由 dcache agent driver 通过 interface 驱动 DUT 顶层 input；monitor 同步采样并做 XZ 检查；xaction 保存字段、约束、UVM field、打印和 compare。测试激励主框架未修改，后续如果需要产生真实 L2 hint 或 flush done 场景，可通过 dcache transaction 字段驱动。

| 字段 | DUT方向 | 位宽 | agent处理 |
|---|---|---|---|
| `io_l2_hint_valid` | input | 1 | dcache driver 驱动，monitor 采样。 |
| `io_l2_hint_bits_sourceId` | input | `[3:0]` | dcache driver 驱动，monitor 采样。 |
| `io_l2_hint_bits_isKeyword` | input | 1 | dcache driver 驱动，monitor 采样。 |
| `io_l2_flush_done` | input | 1 | dcache driver 驱动，monitor 采样。 |

源码位置：`mem_ut/ver/ut/memblock/tb/dcache_agent_connect.sv`，宏：`MEMBLOCK__DCACHE_AGENT_CONNECT`。
该宏负责把 dcache agent interface 的 4 个 L2 hint/flush 控制字段驱动到 V2 `MemBlock` 顶层 input。

```systemverilog
force RTL_PATH.io_l2_hint_valid = U_IF_NAME.io_l2_hint_valid;
force RTL_PATH.io_l2_hint_bits_sourceId = U_IF_NAME.io_l2_hint_bits_sourceId;
force RTL_PATH.io_l2_hint_bits_isKeyword = U_IF_NAME.io_l2_hint_bits_isKeyword;
force RTL_PATH.io_l2_flush_done = U_IF_NAME.io_l2_flush_done;
```

中文伪代码：该 connect 逻辑在 dcache agent 中承担“把测试环境提供的 L2 hint/flush 控制值送入 DUT 顶层”的功能。执行时分别把 interface 中的 hint valid、hint sourceId、hint isKeyword 和 flush done force 到 `MemBlock` 顶层同名 input；这些字段不再停留在 `dut_inst.sv` 的默认 0，也不再属于未归属端口。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv`，函数/task：`send_pkt()`。
该 task 负责把 sequence 传入的 dcache transaction 字段驱动到 interface 的 `drv_cb`。

```systemverilog
vif.drv_mp.drv_cb.io_l2_hint_valid <= tr.io_l2_hint_valid;
vif.drv_mp.drv_cb.io_l2_hint_bits_sourceId <= tr.io_l2_hint_bits_sourceId;
vif.drv_mp.drv_cb.io_l2_hint_bits_isKeyword <= tr.io_l2_hint_bits_isKeyword;
vif.drv_mp.drv_cb.io_l2_flush_done <= tr.io_l2_flush_done;
```

中文伪代码：该驱动逻辑在有效 transaction 下把 L2 hint/flush 控制字段从 `tr` 拷贝到 interface。`io_l2_hint_valid` 控制本拍是否发起 hint；`io_l2_hint_bits_sourceId` 和 `io_l2_hint_bits_isKeyword` 携带 hint 属性；`io_l2_flush_done` 表示外部 L2 flush 完成反馈。字段被写入 `drv_cb` 后，connect 宏会继续把它们送到 DUT 顶层 input。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv`，函数/task：`drive_idle()`。
该 task 负责 reset、gap 和无 transaction 周期的默认驱动，避免 DUT input 悬空。

```systemverilog
vif.drv_mp.drv_cb.io_l2_hint_valid <= '0;
vif.drv_mp.drv_cb.io_l2_hint_bits_sourceId <= '0;
vif.drv_mp.drv_cb.io_l2_hint_bits_isKeyword <= '0;
vif.drv_mp.drv_cb.io_l2_flush_done <= '0;
```

中文伪代码：该 idle 逻辑在默认 `DRV_0` 和 `DRV_LST` 模式下关闭 L2 hint，并把 sourceId、isKeyword 和 flush done 清零。这样 reset 后和无有效 transaction 周期内，DUT 顶层 input 有明确默认值，不依赖 `dut_inst.sv` 的一次性 initial 赋值。`DRV_1`、`DRV_X` 和 `DRV_RAND` 分支也已同步覆盖这些字段，保持 driver 字段集合一致。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_monitor.sv`，函数/task：`mon_data()`。
该 task 每拍采样 dcache interface 字段，并在 XZ 检查打开时检查未知态。

```systemverilog
io_l2_hint_valid = this.vif.mon_mp.mon_cb.io_l2_hint_valid;
io_l2_hint_bits_sourceId = this.vif.mon_mp.mon_cb.io_l2_hint_bits_sourceId;
io_l2_hint_bits_isKeyword = this.vif.mon_mp.mon_cb.io_l2_hint_bits_isKeyword;
io_l2_flush_done = this.vif.mon_mp.mon_cb.io_l2_flush_done;
`TCNT_CHECK_SIG_XZ(io_l2_hint_bits_sourceId,io_l2_hint_bits_sourceId,4);
```

中文伪代码：该 monitor 逻辑每拍从 interface 的 `mon_cb` 读取 L2 hint/flush 字段到本地变量。reset 释放且 XZ 检查开启后，monitor 会按各字段位宽检查未知态，其中 `sourceId` 按 4 位检查。当前 monitor 只完成接口采样和 XZ 审计，不把这些字段接入 raw queue、RM 或 scoreboard，因此不修改测试框架主逻辑。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv`，逻辑对象：字段声明、约束和 UVM field 注册。
该 xaction 负责保存 L2 hint/flush 字段，并让字段进入 UVM 打印、复制、比较等通用 transaction 行为。

```systemverilog
rand bit io_l2_hint_valid;
rand bit [3:0] io_l2_hint_bits_sourceId;
rand bit io_l2_hint_bits_isKeyword;
rand bit io_l2_flush_done;
constraint dcache_agent_agent_xaction::default_io_l2_hint_bits_sourceId_cons{
    io_l2_hint_bits_sourceId inside {[4'd0:4'd15]};
}
```

中文伪代码：该 xaction 逻辑为每个 L2 hint/flush interface 字段建立同名 transaction 字段。`valid`、`isKeyword` 和 `flush_done` 约束为 0 或 1；`sourceId` 约束为 4 位合法范围。UVM field、`psdisplay()` 和 `compare()` 也同步加入这些字段，避免 driver、monitor 或后续 sequence 使用 transaction 时出现字段缺失。

文档同步：`AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md` 已同步更新。`dcache_agent_agent` 汇总从 53 项改为 57 项，新增 4 个 L2 hint/flush 字段；这 4 个端口已从未归属端口列表移除。

正确性检查：

- V2 `build/rtl/MemBlock.sv` 中 4 个端口均为 `input`，`io_l2_hint_bits_sourceId` 位宽为 `[3:0]`，其余为 1 bit。
- `dut_inst.sv` 中 4 个端口均已声明、默认置 0 并实例化到 `MemBlock`。
- `dcache_agent_agent` 当前 interface、connect、xaction、driver、monitor 均包含这 4 个字段，connect 方向为 `IF->RTL`。
- 本轮没有新增非 L2TLB 内部层级引用。

风险边界：本轮只完成接口归属、默认驱动、transaction 字段和 monitor 采样，不新增测试激励场景，也不改变 RM/scoreboard 行为。真实 L2 hint/flush 功能性覆盖需要后续在 sequence 或测试框架适配 plan 中定义激励策略。


### 7.13 `io_mem_to_ooo_ctrl_agent` externalInterrupt debug 字段补齐

功能特性：本轮把 V2 顶层 output `io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug` 并入已有 `io_mem_to_ooo_ctrl_agent_agent`。该字段属于 `io_mem_to_ooo_topToBackendBypass_externalInterrupt_*` 同一组输出，和已接入的 `mtip/msip/meip/seip/nmi` 字段方向、语义归属一致，因此应补到已有 `io_mem_to_ooo_ctrl_agent_agent`，而不是新建 agent，也不是并入反方向的 `backendToTopBypass_agent_agent`。

修改前逻辑：V2 `MemBlock` 顶层和 `dut_inst.sv` 已存在 `io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug`，但 `io_mem_to_ooo_ctrl_agent_agent_interface.sv`、`io_mem_to_ooo_ctrl_agent_connect.sv`、`io_mem_to_ooo_ctrl_agent_agent_xaction.sv` 和 `io_mem_to_ooo_ctrl_agent_agent_monitor.sv` 未包含该字段。接口矩阵文档把它列为唯一的 “Mem->OOO输出端口” 未归属项。

修改后逻辑：本轮将该字段补入 `io_mem_to_ooo_ctrl_agent_agent`。connect 方向为 `RTL->IF`，即 DUT 顶层 output 进入 interface；monitor 每拍采样并做 XZ 检查；xaction 保存字段、约束、UVM field、打印和 compare。该字段是 DUT output，被动采样，driver 不驱动。

源码位置：`mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_ctrl_agent_connect.sv`，宏：`MEMBLOCK__IO_MEM_TO_OOO_CTRL_AGENT_CONNECT`。
该宏负责把 V2 `MemBlock` 顶层 `io_mem_to_ooo_*` 控制类 output 接入 ctrl agent interface。

```systemverilog
force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug =
    RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
```

中文伪代码：该 connect 逻辑在 ctrl agent 中承担“把 DUT 顶层 debug interrupt 输出送入 interface”的功能。执行时从 `MemBlock` 顶层读取 `externalInterrupt_debug`，并 force 到同名 interface 字段；monitor 后续和同组 `mtip/msip/meip/seip/nmi` 字段一起采样。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`，函数/task：`mon_data()`。
该 monitor 每拍采样 ctrl agent interface 字段，并在 XZ 检查开启时检查未知态。

```systemverilog
io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug =
    this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
`TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug,
                  io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug,1);
```

中文伪代码：该 monitor 逻辑每拍从 interface 的 `mon_cb` 读取 debug interrupt 输出到本地变量。reset 释放且 XZ 检查开启后，按 1 bit 位宽检查该字段没有未知态。该字段当前只完成接口采样和 transaction 闭合，不改变 `dispatch_raw_ctrl_t` 写入逻辑。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_xaction.sv`，逻辑对象：字段声明、约束、UVM field、打印和 compare。
该 xaction 负责保存 `externalInterrupt_debug` 字段，并让它进入 ctrl agent transaction 的通用打印和比较行为。

```systemverilog
rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug_cons;
`uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug, UVM_ALL_ON);
```

中文伪代码：该 xaction 逻辑为 debug interrupt 输出建立同名 transaction 字段，并注册默认约束和 UVM field。后续如果 monitor 创建 transaction 或测试框架读取 ctrl transaction，该字段不会缺失；如果比较两个 ctrl transaction，compare 逻辑也会覆盖该字段。

文档同步：`AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md` 已同步更新。`io_mem_to_ooo_ctrl_agent_agent` 汇总从 41 项改为 42 项，新增 `io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug`；该字段已从未归属 DUT 顶层端口列表移除。

正确性检查：

- V2 `build/rtl/MemBlock.sv` 中该端口为 `output`，位宽 1 bit。
- `dut_inst.sv` 中该端口声明为 `wire` 并已实例化到 `MemBlock`。
- `io_mem_to_ooo_ctrl_agent_agent` 当前 interface、connect、xaction、monitor 均包含该字段。
- 该字段为 DUT output，driver 不驱动符合方向要求。

风险边界：本轮只完成 V2 顶层 output 的 agent 归属和被动采样，不修改 raw ctrl queue、RM、scoreboard 或测试激励框架。debug interrupt 是否需要参与后续测试框架状态判断，应由后续测试框架适配 plan 单独定义。


### 7.14 int writeback 最终命名复查

功能特性：本轮复查并覆盖此前 LDA exceptionVec 单独补齐的历史修改，最终状态不再是“旧聚合字段 + V2 字段”的混合形态，而是完整 V2 `writebackLda/Sta/Std` 原生命名集合。

修改前逻辑：此前文档曾把 LDA exceptionVec 作为增量补齐项记录，字段总数按混合命名统计。该统计已经不再代表当前源码状态。

修改后逻辑：当前源码以 V2 `MemBlock` 顶层 109 个 writeback output 为唯一字段集合，LDA exceptionVec 位也作为同名 V2 字段包含在 109 个字段内，不再单独通过旧兼容路径描述。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_xaction.sv`，逻辑对象：字段注册。

```systemverilog
rand bit io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13;
extern constraint default_io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13_cons;
`uvm_field_int(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13, UVM_ALL_ON);
```

中文伪代码：该 xaction 逻辑为 V2 LDA exceptionVec output 建立同名 transaction 字段，并把字段加入默认空约束和 UVM 自动注册。monitor 后续如果创建 transaction，该字段不会缺失；compare 检查两个 transaction 时，也能发现该异常位不一致。

正确性检查：当前 `io_mem_to_ooo_int_wb_agent_agent` 的 interface、xaction、monitor 和 connect 均为 109 个 V2 命名字段；此前混合命名统计已由本轮 V2 原生字段统计替代。

风险边界：本轮不改变 exceptionVec 在公共 raw queue 中的语义，只改变 DUT interface 层字段命名和连接来源。后续如果测试框架需要进一步按异常位类型细分行为，应在测试框架适配 plan 中单独定义。

## 8. Review 结论

当前代码已经满足 V2 整核 `MemBlock` 顶层端口闭合、connect/interface/xaction/driver/monitor 结构闭合、活动 `MEMBLOCK_UT` 分支 DUT output 方向闭合、L2TLB internal wire 存在性检查和远端 VCS 编译通过三个结构性标准。后续需要按测试框架行为适配 plan 继续处理 issue/writeback/L2TLB 的语义级适配、V2 新增顶层 output 主功能影响分析和 runtime 仿真问题。
