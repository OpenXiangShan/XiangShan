# 混合 NC 探索的 redirect 后迟到 Load 写回观察（2026-11-02）

## 1. 术语、范围与状态

| 术语 | 本文含义 |
| --- | --- |
| NC / PBMT | 不可缓存访问；本场景生效 PBMT 固定为 01 |
| PF | 页权限等条件引起的 Page Fault；日志 Load exceptionVec=0x2000 |
| redirect | 较老指令异常导致的重定向，可能撤销年轻动态指令 |
| ROB / UID | DUT 重排序队列标签 / 测试框架逻辑指令编号 |
| LDA / WB | 标量 Load 写回端口 / 写回事件 |
| active owner | 公共模型当前保有的 ROB 到 UID 动态归属 |
| A.fire / D.fire | 外部 TileLink 请求 / 返回通道的 valid 与 ready 同时有效 |
| FSDB / NPI | 波形文件 / Verdi 原生只读查询接口 |

状态：**PAUSED_ANALYSIS**。这是本次 routed PPN 方案验收之外的一次混合 NC 探索，原参数已重新复现。已确认异常 redirect 后出现完整 ROB 标签的 Load 写回，而归一化时 active owner 不存在；尚未完成测试框架与 DUT 的最终责任分类。不得称为已经确认的历史问题，也不得称为与本次改动无关的缺陷。

本轮仅读取日志与波形，并写本观察文档和结果目录中的波形摘录。没有修改生产代码、RTL、CSR 参数或 RM 比较规则，没有登记 Buglist。

## 2. 精确输入与产物

路径基准为 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028`。

原失败入口：

```text
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_dispatch_real_smoke
seed=21102101
mode=routed_ppn_20261102
+MEMBLOCK_MAIN_TRANS_NUM=120
+MEMBLOCK_MMU_SV39_M_PBMTE_EN=1
+MEMBLOCK_MMU_SV39_H_PBMTE_EN=1
+MEMBLOCK_L2TLB_S1_PBMT_0_WT=0
+MEMBLOCK_L2TLB_S1_PBMT_1_WT=100
+MEMBLOCK_L2TLB_S1_PBMT_2_WT=0
```

原参数复现使用独立 `mode=routed_ppn_nc_debug_20261102`，保持 testcase、vseq、cfg、seed 和上述运行参数，仅打开波形诊断。复现同样在 **7467.800ns** 报：

```text
[INT_WB_ATTACH] no active uid for ROB key=0/49
```

相关产物：

- 原日志：`mem_ut/ver/ut/memblock/sim/routed_ppn_20261102_results/final_nc/status/seed_21102101.final.log`。
- 原配置：同级 `config.txt`、`effective_plusargs.txt`、`frozen_plus_cfg/tc_dispatch_real_smoke.cfg`。
- debug 日志：`mem_ut/ver/ut/memblock/sim/routed_ppn_20261102_results/nc_debug/status/seed_21102101.final.log`。
- debug 汇总：同级 `summary.log`，`completed=1 / fail=1 / infra=0 / remote_unknown_jobs=0`。
- 波形：`mem_ut/ver/ut/memblock/sim/routed_ppn_nc_debug_20261102/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=21102101_rtl_eda_reg_20261102_200204_1439364_7355.fsdb`。
- 本轮只读摘录：`mem_ut/ver/ut/memblock/sim/routed_ppn_20261102_results/nc_debug/wave_observation.tsv`，记录信号完整路径、时间 ns 和十六进制值。

工具读取通过 eda01 的 Verdi `V-2023.12-SP1-1`、其自带 Python 3.6 与 `eda-sim/scripts/fsdb_reader.py` 的 NPI 调用完成。debug mode 的 VCS 编译与仿真产物未因读取工具切换而改变；切换的是只读波形读取工具。FSDB 时间单位 1ps、结束于 7467800ps。旧 Verdi R-2020.12-SP1 缺少该脚本所需的自带 Python，最初调用退到 CLI；最终关键证据使用 NPI 成功读取。

## 3. 这次配置实际包含的激励

不能把这次运行描述为纯正常 NC Load：

1. cfg 虽把 `OP_CLASS_STORE_WT=0`，但没有关闭地址复用。`plus.sv` 默认 `ADDR_REUSE_EN_1_WT/EN_0_WT=1/19`，`STORE_AFTER_LOAD_WT=1`。`memblock_dispatch_base_sequence::apply_addr_reuse_window()` 能把已经选定的 Load 改成 Store。日志中 UID6、36、83 确有 STA/STD 与 Store fault 事件，不能用最初 op 权重推断最终主表全为 Load。
2. cfg 没有覆盖 S1 PTE U；其默认权重仅为 11%。静态 CSR 处于 Sv39/U 态，因而本场景大量访问出现权限 PF。日志多次记录 `exception_vec=0x2000`，继而触发异常 redirect。
3. CBO 默认权重为 0，boundary profile 默认关闭。本轮没有证据将失败归因于这两项残余配置。

这些配置解释了为什么本次探索触发密集异常恢复，但不直接证明 fatal 的责任类别。

## 4. 日志与波形的有限证据链

下表的 `0x28=40`、`0x31=49` 已转换为十进制 ROB value；摘录文件保留十六进制原值。

| 时间 ns | 证据 | 可确认含义 |
| ---: | --- | --- |
| 2675.300 | 原日志 `L2TLB_ROUTED_PPN`：VPN=0x8e907、PPN=0x3a4e5463、PBMT=1、window=1 | UID49 后续使用的翻译落在 DRAM 命名窗口；不是历史 APLIC 洞地址 |
| 7265.300 | 原日志 `freeze UID issue-hit snapshot uid=49 vpn=0x8e907 ... generation=76` | 当前 UID49 关联该翻译；这不是通过 WB 数据反推的地址 |
| 7385.300 | 原日志 Uncache responder owner snapshot 包含 `addr0x3a4e5463440` | 同页物理请求确已到外部 responder；该例没有历史 B08 的无人路由卡住现象 |
| 7400.300～7405.300 | `top_tb.u_memblock__sbuffer_agent_if.auto_inner_buffers_out_d_valid=1` | 观察窗中有一个外部返回；仅凭 source=0 无法唯一绑定为 UID49，不能直接称为它的最终返回 |
| 7425.300 | 原日志 `request scalar fault redirect uid=40 rob=0/40 level=1 flush_itself=1` | 框架请求较老 UID40 的异常 redirect |
| 7435.300～7440.300 | `top_tb.U_MEMBLOCK.io_redirect_valid=1`、ROB value=0x28、level=1 | redirect 确实到达 DUT 顶层，目标为 ROB0/40 |
| 7440.300～7445.300 | `top_tb.U_MEMBLOCK.inner_LoadUnit_0.io_redirect_valid=1`，value=0x28 | LoadUnit 内部也已看到该 redirect；不能把异常仅归为 TB 请求未送到 DUT |
| 7442.800 | 原日志 `dut lqDeq preflight live ... uid=49 lq=0/20`，随后 `release lq mapping uid=49` | 框架消费了该实例的 LQ 释放事实 |
| 7460.300～7465.300 | `top_tb.io_mem_to_ooo_writebackLda_2_valid=1`，ROB flag=0/value=0x31 | DUT 标量 Load lane 2 输出了完整 ROB0/49 的 WB；不是 STD value-only 事件 |
| 同一 WB 有效窗 | lane2 的 PF13=0、replayInst=0、flushPipe=0、debug_isMMIO=0、debug_isNCIO=0 | 所取这几个字段没有标记 PF、replay 或 flush；没有据此宣称全部 exceptionVec 都为零，也没有用 debug NC=0 否定先前 PBMT |
| 7465.300 | `top_tb.u_memblock__io_mem_to_ooo_int_wb_agent_if.mon_cb` 的 lane2 valid=1、ROB0/0x31、PF13=0、replay=0、flush=0 | WB 已进入真实 monitor 采样，且采样值与先前 DUT 输出相符 |
| 7467.800 | 原日志与 debug 均报 `INT_WB_ATTACH ... ROB key=0/49` | adapter 消费该 WB 时 active ROB 归属不存在，运行失败 |

`dispatch_monitor_event_adapter::convert_raw_int_wb()` 的 scalar LDA 分支使用完整 ROB 标签进入 `attach_current_issue_snapshot()`，后者在 `lookup_active_uid_by_rob()` 失败时报本次 fatal。B05 处理的是 STD value-only 的双候选选择与已完成 owner 排除，路径不同。仅有“都涉及 ROB 归属”不能证明它是 B05 复发。

## 5. 当前能够确认与仍待确认的事项

能够确认：

- 原参数在独立 debug mode 复现相同 fatal、相同时间，结果已终态收敛，无未知远端任务。
- 相关 NC 翻译是合法 DRAM 地址，并有到达 responder 的请求证据；这次失败不是日志所述历史 B08 的 APLIC 洞阻塞形状。
- 较老指令 redirect 已到 DUT，随后出现 ROB0/49 的 scalar LDA2 写回；monitor 采样后无法匹配当前 active owner。
- 本次额外探索确实混合权限 PF、地址复用生成的 Store 和多次 redirect。

尚未确认：

- UID49 的 active ROB map 具体在哪一次 redirect 应用阶段删除；LQ release 日志本身不等价于 ROB map 删除证明。
- 该 WB 对应哪一代 UID49 动态实例，raw `sample_flush_epoch` 与该实例的生命周期是否一致。
- 外部 D 返回、Uncache slot 与 lane2 WB 的完整一一对应关系；现有有限摘录没有把同 source 的多个请求唯一归属，不能声称已经证明最终响应属于 UID49。
- DUT 是否应在此边界抑制该写回，还是框架应使用严格的历史实例证明处理迟到 WB。因此不能宣称最终责任已确定为 RTL 或 monitor adapter。
- route 策略是否通过改变请求目标/延迟分布暴露该边界。当前没有直接证据表明 route 选页错误，也不能据此宣称与本次改动无关。

后续应在独立异常恢复任务中，按原参数继续核对 raw epoch、ROB 删除点和相应 Uncache slot 的返回身份。本次没有为绕过 fatal 修改 active map、降低校验或吞掉 WB。

## 6. 对照与验收边界

方案内的正常 NC/IO 邻接采用父 `memblock_dispatch_real_smoke_vseq` 入口和 `tc_l2tlb_routed_ppn_narrow` 的完整安全配置，使用普通随机主表，没有窄窗口 intent：

- `final_nc_pure_load/results.tsv`：seed21102101，120 笔，PBMT01，PASS、UVM错误数0。
- `final_io_pure_load/results.tsv`：seed21102102，120 笔，PBMT10，PASS、UVM错误数0。

两份结果目录均位于 `mem_ut/ver/ut/memblock/sim/routed_ppn_20261102_results`。这些对照确认方案要求的正常流量可以完成，但参数与原失败不同，不能替代原失败或声称修复上述异常边界。主方案验收结果与这次额外失败分别记录；本观察保持 **PAUSED_ANALYSIS**。
