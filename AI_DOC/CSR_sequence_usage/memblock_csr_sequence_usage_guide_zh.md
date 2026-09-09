<!doctype html>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>MemBlock CSR Sequence 调用指南</title>

<style>
:root {
  --ink: #18202a;
  --muted: #596575;
  --line: #cfd6df;
  --paper: #ffffff;
  --blue-bg: #e9f3ff;
  --blue-line: #2474b5;
  --green-bg: #eaf7ef;
  --green-line: #27834a;
  --amber-bg: #fff4d8;
  --amber-line: #a66b00;
  --red-bg: #fdecec;
  --red-line: #b63b3b;
  --gray-bg: #f4f6f8;
}
html { scroll-behavior: smooth; }
body {
  max-width: 1180px;
  margin: 0 auto;
  padding: 36px 52px 80px;
  color: var(--ink);
  background: var(--paper);
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Noto Sans CJK SC",
               "Microsoft YaHei", Arial, sans-serif;
  line-height: 1.72;
}
h1, h2, h3, h4 { line-height: 1.3; }
h1 { margin: 0 0 10px; font-size: 2.05rem; border-bottom: 3px solid #202a35; padding-bottom: 14px; }
h2 { margin-top: 48px; padding: 8px 12px; background: #202a35; color: #fff; }
h3 { margin-top: 30px; padding-bottom: 6px; border-bottom: 2px solid var(--line); }
h4 { margin-top: 24px; }
p, li { font-size: 15px; }
a { color: #145c96; }
code {
  padding: 1px 5px;
  border: 1px solid #d7dde5;
  border-radius: 3px;
  background: #f3f5f7;
  color: #17212c;
  overflow-wrap: anywhere;
}
pre {
  padding: 16px;
  overflow: auto;
  border: 1px solid #c9d0d8;
  border-left: 4px solid #5f6d7b;
  background: #f6f8fa;
}
pre code { padding: 0; border: 0; background: transparent; }
table { width: 100%; border-collapse: collapse; margin: 14px 0 24px; font-size: 14px; }
th, td { border: 1px solid #cbd2da; padding: 8px 10px; vertical-align: top; text-align: left; }
th { background: #e7ebef; }
tbody tr:nth-child(even) { background: #fafbfc; }
.callout, .flow, .warn, .danger, .neutral {
  margin: 18px 0;
  padding: 14px 18px;
  border-left: 5px solid;
  border-radius: 4px;
}
.callout { background: var(--blue-bg); border-color: var(--blue-line); }
.flow { background: var(--green-bg); border-color: var(--green-line); white-space: pre-wrap; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size: 13px; line-height: 1.65; }
.warn { background: var(--amber-bg); border-color: var(--amber-line); }
.danger { background: var(--red-bg); border-color: var(--red-line); }
.neutral { background: var(--gray-bg); border-color: #6d7884; }
.meta { color: var(--muted); font-size: 14px; }
.ok { color: #17683a; font-weight: 700; }
.path { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; overflow-wrap: anywhere; }
.toc { padding: 14px 18px; border: 1px solid #cbd2da; background: #fafbfc; }
.toc a { display: block; padding: 2px 0; }
@media (max-width: 980px) {
  body { margin: 0; padding: 24px 18px 64px; }
  table { display: block; overflow-x: auto; }
}
@media print {
  body { max-width: none; margin: 0; padding: 0; }
  h2 { break-after: avoid; }
  table, pre, .callout, .flow, .warn, .danger { break-inside: avoid; }
}
</style>

<a id="top"></a>

# MemBlock CSR Sequence 调用指南

<div class="meta">
文档日期：2026-09-07<br>
适用仓库：<code>/nfs/home/wangyan/Memblock_env_20260716/XiangShan</code><br>
适用目录：<code>mem_ut/ver/ut/memblock</code><br>
推荐 testcase：<code>basicTest</code><br>
推荐入口：<code>memblock_csr_random_config_vseq</code><br>
运行节点：<code>eda01 / 172.28.10.101</code>
</div>

<div class="toc">
<strong>索引</strong>
<a href="#section-1">1. 快速选择调用方式</a>
<a href="#section-2">2. CSR sequence 架构与调用边界</a>
<a href="#section-3">3. 最推荐：只新增 cfg 直接调用</a>
<a href="#section-4">4. 仅启动期静态 CSR 配置</a>
<a href="#section-5">5. 启用主表动态 CSR 切换</a>
<a href="#section-6">6. Plus 参数分类与配置规则</a>
<a href="#section-7">7. 新增定制 scalar main/vseq</a>
<a href="#section-8">8. 编译与仿真命令</a>
<a href="#section-9">9. 日志与波形验收</a>
<a href="#section-10">10. 常见错误与定位</a>
<a href="#section-11">11. 不可破坏的不变量</a>
<a href="#section-12">12. 文件索引与参考用例</a>
</div>

<div class="callout">
<strong>最简调用结论：</strong>如果新用例的业务流量可以通过现有 AUTO main-table plus 参数表达，不需要新增任何 CSR sequence 类。新建一份独立 cfg，运行时选择 <code>memblock_csr_random_config_vseq</code> 即可。该 VSEQ 会按正确顺序完成启动期静态 CSR、启动唯一 CSR worker，并在主表 CSR marker 轮到且前序 ROB 已 commit 后自动调用动态 child sequence。
</div>

<div class="danger">
<strong>禁止直接调用：</strong>用例代码不得直接 <code>start()</code> <code>memblock_dynamic_csr_change_sequence</code>。动态 child 需要合法的 marker owner、dynamic epoch、generation、runtime baseline 和 commit barrier；绕过 action queue 会破坏前序 commit 隔离和 candidate/monitor 闭环。
</div>

<a id="section-1"></a>

## 1. 快速选择调用方式

| 需求 | 推荐入口 | 需要新增 SV | 动态 marker |
|---|---|---:|---:|
| 随机 load/store，同时配置 CSR | <code>memblock_csr_random_config_vseq</code> + 新 cfg | 否 | 可选 |
| 只在流量开始前配置一次 CSR | 同上，令 <code>MEMBLOCK_CSR_CONTROL_ENABLE=0</code> | 否 | 无 |
| 运行中按固定/随机间隔切换 CSR | 同上，AUTO topology + CSR marker | 否 | 有 |
| 精确 10,000 笔 scalar 压力流量并带结构审计 | 继承 <code>memblock_csr_scalar_stress_vseq_base</code> | 是 | 有 |
| 固定 Sv39、无 CSR 随机、无动态 worker 的 legacy real-smoke | <code>memblock_dispatch_real_smoke_vseq</code> + topology 0 | 否 | 无 |
| 手工指定每个 control UID | 现有 manual-control topology 3 | 通常是 | 不属于本指南首选随机 CSR 路径 |

优先级应当是：先判断 cfg 能否表达；不能表达时再增加 main/vseq。不要因为只想改变 CSR profile 就复制或改写 CSR driver、initial sequence、dynamic child 或 control service。

<a id="section-2"></a>

## 2. CSR sequence 架构与调用边界

### 2.1 三个 sequence 的职责

| Sequence | 谁启动 | 生命周期 | 用例是否直接调用 |
|---|---|---|---|
| <code>memblock_csr_initial_config_sequence</code> | CSR 专项 VSEQ | 启动期运行一次 | 只应在受控 VSEQ 的 CSR lane 中调用 |
| <code>memblock_csr_control_base_sequence</code> | CSR 专项 VSEQ | initial 完成后常驻，直到 worker shutdown | 由 VSEQ 启动一次 |
| <code>memblock_dynamic_csr_change_sequence</code> | CSR base worker | 每个有效 marker 启动一次 | <strong>禁止 testcase/VSEQ 直接调用</strong> |

### 2.2 正确的启动顺序

<div class="flow">basicTest 读取 +VSEQ_MAIN
  -> memblock_csr_random_config_vseq::body()
     -> seq_csr_common::init()，冻结 cfg/plus 快照
     -> csr_special_sequence_active = 1
     -> main sequence 建立 AUTO 主表
     -> initial_csr_seq.start(csr_ctrl_sqr)
        -> 等 main_table_ready/runtime ready
        -> 发送完整静态 CSR，等待 full snapshot
        -> 执行初始 PMP/PMA write plan
        -> publish committed state，置 csr_initial_config_done
     -> csr_control_seq.start(csr_ctrl_sqr)
     -> LSQ enqueue / issue / commit / L2TLB / SFence producer
        均在 csr_initial_config_done 后启动</div>

initial sequence 与 base worker 在同一条 fork 分支上顺序执行，因此同一个 <code>csr_ctrl_sqr</code> 不会出现双 producer 窗口。动态 child 又由 base worker同步 <code>start(m_sequencer, this)</code>，不同 marker 的动态 CSR item 也不会互相交错。

### 2.3 动态调度顺序

<div class="flow">AUTO 主表命中 MEMBLOCK_OP_CLASS_CSR_CONTROL marker
  -> marker 成为 active control barrier
  -> 阻止 UID > marker_uid 的 younger admission
  -> WAIT_OLDER_ROB_COMMIT
  -> commit_cursor_uid == marker_uid
  -> bind owner(uid/epoch/generation)
  -> enqueue CSR action + event
  -> memblock_csr_control_base_sequence pop action
  -> 启动 memblock_dynamic_csr_change_sequence
  -> driver 四路 changed 同拍置 1，下一拍清 0
  -> monitor/full snapshot/PMP-PMA model 确认
  -> control marker commit，解除 barrier</div>

动态切换的门槛是前序 UID 全部越过有序 ROB commit cursor。它不是 marker 建表时立即发生，也不是等待日志打印才发生；接口真实时间应以 FSDB 的 changed 上升沿为准。

<a id="section-3"></a>

## 3. 最推荐：只新增 cfg 直接调用

### 3.1 适用条件

以下内容都能由现有 cfg 表达时，只需新增 cfg：

- 主表规模；
- load/store operation class 权重；
- CSR marker 固定间隔或随机区间；
- static/dynamic CSR mode、ID、权限、privilege/virtualization 权重；
- 是否动态更新 SATP、VSATP、HGATP、permission、priv context、PMP/PMA；
- 启动期 enable 信号权重；
- L2TLB 响应、并发和其它流量约束。

建议将新 cfg 放在用例自己的隔离目录，例如：

```text
mem_ut/ver/ut/memblock/seq/my_csr_case/
└── cfg/
    └── tc_my_csr_case.cfg
```

### 3.2 可运行的最小动态 cfg 模板

下面模板启用 AUTO main table、启动期 CSR 配置和 SATP/priv/permission 动态切换。实际用例还应根据业务目的补充 operation、L2TLB、地址和错误注入约束。

```text
+MEMBLOCK_MAIN_TRANS_NUM=1010
+MEMBLOCK_USE_MANUAL_MAIN_TABLE=0
+MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=1
+MEMBLOCK_CSR_CONTROL_ENABLE=1
+MEMBLOCK_CSR_CONTROL_MIN_INTERVAL=100
+MEMBLOCK_CSR_CONTROL_MAX_INTERVAL=100
+MEMBLOCK_SFENCE_CONTROL_ENABLE=0

+MEMBLOCK_MAIN_MEM_RANGES_EN=1
+MEMBLOCK_PMA_PMP_MODEL_EN=1
+MEMBLOCK_LSQENQ_SEQ_EN=1
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
+MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0
+MEMBLOCK_LSQCOMMIT_SEQ_EN=1
+MEMBLOCK_L2TLB_SEQ_EN=1

+MEMBLOCK_OP_CLASS_INT_LOAD_WT=1
+MEMBLOCK_OP_CLASS_FP_LOAD_WT=0
+MEMBLOCK_OP_CLASS_STORE_WT=0
+MEMBLOCK_OP_CLASS_PREFETCH_WT=0
+MEMBLOCK_OP_CLASS_AMO_WT=0
+MEMBLOCK_OP_CLASS_CBO_WT=0

+MEMBLOCK_CSR_INIT_SATP_BARE_WT=0
+MEMBLOCK_CSR_INIT_SATP_SV39_WT=1
+MEMBLOCK_CSR_INIT_SATP_SV48_WT=0
+MEMBLOCK_CSR_INIT_PRIV_VIRT_0_WT=1
+MEMBLOCK_CSR_INIT_PRIV_VIRT_1_WT=1

+MEMBLOCK_CSR_CHANGE_SATP_ENABLE=1
+MEMBLOCK_CSR_CHANGE_VSATP_ENABLE=0
+MEMBLOCK_CSR_CHANGE_HGATP_ENABLE=0
+MEMBLOCK_CSR_CHANGE_PERMISSION_ENABLE=1
+MEMBLOCK_CSR_CHANGE_PRIV_CONTEXT_ENABLE=1
+MEMBLOCK_CSR_CHANGE_PMP_PMA_ENABLE=0

+MEMBLOCK_CSR_CHANGE_SATP_BARE_WT=1
+MEMBLOCK_CSR_CHANGE_SATP_SV39_WT=1
+MEMBLOCK_CSR_CHANGE_SATP_SV48_WT=1
+MEMBLOCK_CSR_CHANGE_SATP_ASID_MIN=0
+MEMBLOCK_CSR_CHANGE_SATP_ASID_MAX=65535

+MEMBLOCK_CSR_CHANGE_MXR_0_WT=1
+MEMBLOCK_CSR_CHANGE_MXR_1_WT=1
+MEMBLOCK_CSR_CHANGE_SUM_0_WT=1
+MEMBLOCK_CSR_CHANGE_SUM_1_WT=1
+MEMBLOCK_CSR_CHANGE_VMXR_0_WT=1
+MEMBLOCK_CSR_CHANGE_VMXR_1_WT=1
+MEMBLOCK_CSR_CHANGE_VSUM_0_WT=1
+MEMBLOCK_CSR_CHANGE_VSUM_1_WT=1

+MEMBLOCK_CSR_CHANGE_PRIV_VIRT_0_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_VIRT_1_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_IMODE_U_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_IMODE_S_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_IMODE_M_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_DMODE_U_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_DMODE_S_WT=1
+MEMBLOCK_CSR_CHANGE_PRIV_DMODE_M_WT=1

+MEMBLOCK_CSR_PMP_PMA_EXCEPTION_ENABLE=0
```

<div class="warn">
<strong>模板不是完整回归约束：</strong>如果目标是单独验证 CSR epoch，应像现有 stress cfg 一样关闭无关 fault/probe/reorder，并将 L2TLB response 配成确定且合法的叶子 PTE。否则 testcase 仍可能合法地产生与 CSR 无关的 fault，使结果不便归因。
</div>

### 3.3 AUTO 主表数量的含义

在 topology 1 中，<code>MEMBLOCK_MAIN_TRANS_NUM=N</code> 表示可预约的前 N 个 slot；builder 还会在最后增加一笔 <code>check_store</code>，因此最终主表总数是 <code>N+1</code>。CSR marker 是替换这些 N 个 slot，而不是额外插入。

当 interval 固定为 <code>I</code> 时：

```text
marker UID = I, 2I, 3I, ...，且 marker UID < N
marker 数 K = floor((N - 1) / I)
业务 load/store 数 = N - K
最终主表总数 = N + 1
```

例如要得到恰好 10,000 笔业务 load 和 10 个 marker，可配置 <code>N=10010</code>、<code>I=1000</code>；marker 位于 UID 1000、2000、...、10000，最终再追加 UID 10010 的 check_store。

如果 min/max 不相等，每次 marker 命中后会从当前 marker UID 重新随机下一段 interval，marker 数不再是固定值。需要严格审计 marker 数的用例应使用固定 interval。

<a id="section-4"></a>

## 4. 仅启动期静态 CSR 配置

如果用例只要求流量开始前完成一次 CSR 配置，不希望运行中切换，仍推荐使用 <code>memblock_csr_random_config_vseq</code>，配置如下：

```text
+MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=1
+MEMBLOCK_USE_MANUAL_MAIN_TABLE=0
+MEMBLOCK_CSR_CONTROL_ENABLE=0
+MEMBLOCK_SFENCE_CONTROL_ENABLE=0
+MEMBLOCK_MAIN_MEM_RANGES_EN=1
+MEMBLOCK_PMA_PMP_MODEL_EN=1
```

然后只设置 <code>MEMBLOCK_CSR_INIT_*</code> 和 <code>MEMBLOCK_CSR_ENABLE_*</code> 权重。此时：

1. initial sequence 仍会发送一次完整静态 CSR level，并等待 full snapshot。
2. 四路 <code>satp/vsatp/hgatp/priv_virt_changed</code> 在 initial item 中全部为 0。
3. initial PMP/PMA profile 和 write plan 仍会完成并发布 committed state。
4. 没有 CSR control marker，因此不会调用动态 child。
5. 普通 traffic producer 仍等待 <code>csr_initial_config_done</code>，不会抢在静态 CSR 前发送。

<div class="neutral">
<strong>另一条固定静态路径：</strong><code>memblock_mmu_sv39_csr_sequence</code> 只用于 <code>memblock_dispatch_real_smoke_vseq</code> 的 topology 0。它持续驱动固定 Sv39/U-mode profile，不使用本指南的可配置 initial randomizer，也不能与 active CSR control worker 并发。新用例需要可配置静态 CSR 时，不应选择这条 legacy 路径。
</div>

<a id="section-5"></a>

## 5. 启用主表动态 CSR 切换

### 5.1 必需配置

动态 CSR 至少需要：

```text
+MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=1
+MEMBLOCK_USE_MANUAL_MAIN_TABLE=0
+MEMBLOCK_CSR_CONTROL_ENABLE=1
+MEMBLOCK_CSR_CONTROL_MIN_INTERVAL=<大于等于1>
+MEMBLOCK_CSR_CONTROL_MAX_INTERVAL=<大于等于MIN>
```

并且下列六个动态组至少开启一个：

| Plus 参数 | 动态修改内容 |
|---|---|
| <code>MEMBLOCK_CSR_CHANGE_SATP_ENABLE</code> | SATP mode/ASID，PPN 使用固定合法 root |
| <code>MEMBLOCK_CSR_CHANGE_VSATP_ENABLE</code> | VSATP mode/ASID，PPN 使用固定合法 root |
| <code>MEMBLOCK_CSR_CHANGE_HGATP_ENABLE</code> | HGATP mode/VMID，PPN 固定且 x4 对齐 |
| <code>MEMBLOCK_CSR_CHANGE_PERMISSION_ENABLE</code> | MXR/SUM/VMXR/VSUM |
| <code>MEMBLOCK_CSR_CHANGE_PRIV_CONTEXT_ENABLE</code> | <code>priv_virt</code>、<code>priv_imode</code>、<code>priv_dmode</code> |
| <code>MEMBLOCK_CSR_CHANGE_PMP_PMA_ENABLE</code> | PMP/PMA semantic profile 和 distributed CSR write plan |

开启某一组后，该组对应的合法值权重不能全为 0；dynamic randomizer 还要求相对 committed state 至少存在一个合法变化解。

### 5.2 changed pulse 无需用例配置

用例不得在 cfg 或 sequence 中自行操作 changed pulse。每笔动态 CSR target 会统一设置：

```text
satp_changed      = 1
vsatp_changed     = 1
hgatp_changed     = 1
priv_virt_changed = 1
```

CSR driver 在下一 <code>drv_cb</code> 只把这四路清 0，其余 CSR level 保持新 target。即使只开启 SATP 动态组，四路 changed 仍统一产生一个周期脉冲；这是当前动态 item 的协议定义，不应按实际变化字段裁剪。

### 5.3 不会污染上一笔传输

dynamic child 只在 <code>commit_cursor_uid == marker_uid</code> 后启动，保证前序 UID 已完成有序 ROB commit。marker 后面的 UID 又被 control barrier 阻止 admission。旧请求在真实 L2TLB request-fire 时已经冻结自己的 CSR context，后续 RM 只读取该 UID 的 frozen context。

这里保证的是“上一笔 ROB commit 后切换”，不保证等到上一笔更晚的 LSQ dequeue/terminal_done 日志出现后才切换。不要把 ROB commit 和 LSQ terminal 混作同一个时序条件。

<a id="section-6"></a>

## 6. Plus 参数分类与配置规则

所有 plus 参数均集中定义在 <code>env/plus.sv</code>，由 <code>seq_csr_common::init()</code> 一次读取、校验并冻结为 <code>memblock_csr_sequence_cfg_t</code>。sequence 内不要再次直接读取命令行，也不要在仿真运行中修改参数。

### 6.1 启动期 ATP 与 ID 参数

| 参数组 | 合法值/说明 |
|---|---|
| <code>MEMBLOCK_CSR_INIT_SATP_{BARE,SV39,SV48}_WT</code> | SATP mode 相对权重 |
| <code>MEMBLOCK_CSR_INIT_VSATP_{BARE,SV39,SV48}_WT</code> | VSATP mode 相对权重 |
| <code>MEMBLOCK_CSR_INIT_HGATP_{BARE,SV39X4,SV48X4}_WT</code> | HGATP mode 相对权重 |
| <code>MEMBLOCK_CSR_INIT_SATP_ASID_{MIN,MAX}</code> | 0..65535 范围内 |
| <code>MEMBLOCK_CSR_INIT_VSATP_ASID_{MIN,MAX}</code> | 0..65535 范围内 |
| <code>MEMBLOCK_CSR_INIT_HGATP_VMID_{MIN,MAX}</code> | 0..16383 范围内 |

权重 0 表示禁止该值，正数表示相对概率。若希望每个合法 mode 都有机会，三个 mode 权重都设置为正数；例如 1/1/1 是等权，而 1/8/1 会提高 Sv39 的命中概率。

### 6.2 启动期 permission 与 privilege 参数

- <code>MEMBLOCK_CSR_INIT_{MXR,SUM,VMXR,VSUM}_{0,1}_WT</code>：4 个权限 bit 的 0/1 权重。
- <code>MEMBLOCK_CSR_INIT_PRIV_VIRT_{0,1}_WT</code>：虚拟化状态权重。
- <code>MEMBLOCK_CSR_INIT_PRIV_IMODE_{U,S,M}_WT</code>：instruction mode 权重。
- <code>MEMBLOCK_CSR_INIT_PRIV_DMODE_{U,S,M}_WT</code>：data mode 权重。

U/S/M 的合法编码为 00/01/11；保留编码 10 不会由 randomizer 生成。

### 6.3 启动期 enable 参数

以下 13 组支持 <code>_0_WT</code>/<code>_1_WT</code>：

```text
L1D_PF, L1D_PF_AGT, L1D_PF_PHT, L1D_PF_STRIDE,
L2_PF_MASTER, L2_PF_RECV, L2_PF_PBOP, L2_PF_VBOP,
LDLD_VIO, CACHE_ERROR, UNCACHE_OUTSTANDING,
MISALIGN_LD, MISALIGN_ST
```

这些 enable 只在 initial sequence 中求解一次，dynamic sequence 复制并保持它们。其余没有独立权重的 CSR control enable/valid 字段按静态默认表固定配置，不能臆造新的 plus 参数。

### 6.4 动态 ATP、permission 与 privilege 参数

动态参数与 INIT 命名一一对应，只把前缀替换为 <code>MEMBLOCK_CSR_CHANGE_</code>：

- ATP mode：<code>CHANGE_SATP_*</code>、<code>CHANGE_VSATP_*</code>、<code>CHANGE_HGATP_*</code>；
- ID range：<code>CHANGE_SATP_ASID_*</code>、<code>CHANGE_VSATP_ASID_*</code>、<code>CHANGE_HGATP_VMID_*</code>；
- permission：<code>CHANGE_{MXR,SUM,VMXR,VSUM}_{0,1}_WT</code>；
- privilege：<code>CHANGE_PRIV_VIRT_{0,1}_WT</code>、<code>CHANGE_PRIV_{I,D}MODE_{U,S,M}_WT</code>。

“全部值都有随机概率”意味着所有合法候选的权重都必须为正数，而不是只打开 <code>CHANGE_*_ENABLE</code>。例如 priv dmode 全覆盖必须同时设置 U/S/M 三个权重为正。

### 6.5 PMP/PMA 参数

| 参数 | 作用 |
|---|---|
| <code>MEMBLOCK_PADDR_BASE/RANGE</code> | normal memory/PMP/PMA 区域 |
| <code>MEMBLOCK_CSR_PMP_PMA_EXCEPTION_ENABLE</code> | 是否构造额外 exception region |
| <code>MEMBLOCK_CSR_PMP_PMA_EXCEPTION_BASE/RANGE</code> | exception region，必须合法、对齐且不与 normal region 重叠 |
| <code>MEMBLOCK_CSR_PMP_EXCEPTION_{R,W,X}_{0,1}_WT</code> | exception PMP 权限随机 |
| <code>MEMBLOCK_CSR_PMA_EXCEPTION_C_{0,1}_WT</code> | cacheable 属性随机 |
| <code>MEMBLOCK_CSR_PMA_EXCEPTION_ATOMIC_{0,1}_WT</code> | atomic 属性随机 |

PMP 的 W=1 必须满足 R=1。开启动态 PMP/PMA 后，每个 marker 除了四路 changed level item，还可能执行多笔 distributed CSR write，因此 <code>candidate staged and sent</code> 日志会明显晚于 changed 上升沿，这是正常完成语义。

<a id="section-7"></a>

## 7. 新增定制 scalar main/vseq

### 7.1 何时才需要新增代码

仅在以下情况新增 main sequence 和 VSEQ：

- 业务数量必须被 class 内审计为固定值；
- 只允许特定 operation class；
- 主表结构不能完全由现有 plus 表达；
- 需要用例专属的 build/final audit。

已有 10K scalar 用例采用以下结构：

```text
csr_scalar_stress/
├── base_seq/
│   └── memblock_csr_scalar_stress_main_sequence_base.sv
├── main_sequence/
│   └── memblock_csr_scalar_<case>_main_sequence.sv
├── virtual_sequence/
│   ├── memblock_csr_scalar_stress_vseq_base.sv
│   └── memblock_csr_scalar_<case>_vseq.sv
└── cfg/
    └── tc_csr_scalar_<case>.cfg
```

### 7.2 10K main sequence 模板

此模板只适用于继承现有 10K 审计基类的场景：

```systemverilog
class memblock_csr_scalar_mycase_10k_main_sequence extends
    memblock_csr_scalar_stress_main_sequence_base;
    `uvm_object_utils(memblock_csr_scalar_mycase_10k_main_sequence)

    function new(string name = "memblock_csr_scalar_mycase_10k_main_sequence");
        super.new(name);
    endfunction

    virtual function int unsigned expected_normal_slot_count();
        return 10010; // N：10000 business + 10 marker slots
    endfunction

    virtual function int unsigned expected_csr_marker_count();
        return 10;
    endfunction

    virtual function bit business_op_class_allowed(memblock_op_class_e op_class);
        return op_class == MEMBLOCK_OP_CLASS_INT_LOAD;
    endfunction

    virtual function string stress_profile_name();
        return "SCALAR_MYCASE_10K";
    endfunction
endclass
```

对应 cfg 必须令 <code>MEMBLOCK_MAIN_TRANS_NUM == expected_normal_slot_count()</code>，固定 interval 所生成的 marker 数必须等于 <code>expected_csr_marker_count()</code>。该基类还要求业务笔数恰好 10,000、最终只有一笔 check_store 且所有 UID terminal_done。

### 7.3 对应 virtual sequence 模板

```systemverilog
class memblock_csr_scalar_mycase_10k_vseq extends
    memblock_csr_scalar_stress_vseq_base;
    `uvm_object_utils(memblock_csr_scalar_mycase_10k_vseq)

    function new(string name = "memblock_csr_scalar_mycase_10k_vseq");
        super.new(name);
    endfunction

    virtual function memblock_csr_scalar_stress_main_sequence_base
        create_stress_main_sequence();
        memblock_csr_scalar_mycase_10k_main_sequence main_seq;
        main_seq = memblock_csr_scalar_mycase_10k_main_sequence::type_id::create("main_seq");
        return main_seq;
    endfunction
endclass
```

该 VSEQ base 已包含正确的 initial barrier、CSR worker、LSQ/issue/commit/L2TLB/SFence 并发关系；派生用例只替换 main sequence，不应覆写 CSR lane。

### 7.4 编译注册清单

新增 class 后必须完成：

1. 将目录加入 <code>seq/seq.f</code> 的 <code>+incdir+</code>，已有 <code>csr_scalar_stress</code> 子目录则无需重复添加。
2. 在 <code>seq/seq_pkg.sv</code> 按“main base → concrete main → VSEQ base → concrete VSEQ”顺序 include。
3. concrete class 使用 <code>`uvm_object_utils</code> 注册。
4. 在 <code>basicTest.sv::vseq_starts_l2tlb()</code> 加入新 VSEQ 名称。
5. 在 <code>basicTest.sv::vseq_supports_control_worker_topology()</code> 加入新 VSEQ 名称。
6. 不要把新 CSR VSEQ 加到 <code>vseq_owns_static_mmu_csr()</code>；该函数属于 topology 0 的 fixed Sv39 路径。
7. cfg 放在新用例目录，不修改原有 cfg。

<div class="warn">
如果不是 10K scalar 场景，不要继承 <code>memblock_csr_scalar_stress_main_sequence_base</code>，因为它硬性审计 10,000 笔业务。应当复用 <code>memblock_csr_random_config_vseq</code> 的通用 AUTO builder，或建立新的用例专属 main/vseq base，同时完整保留现有 CSR lane 启动顺序。
</div>

<a id="section-8"></a>

## 8. 编译与仿真命令

### 8.1 使用已有 stress 用例

```bash
cd /nfs/home/wangyan/Memblock_env_20260716/XiangShan/mem_ut/ver/ut/memblock/sim

make eda_compile \
  REMOTE_HOST=172.28.10.101 \
  mode=csr_sequence_my_run \
  tc=basicTest \
  ts=memblock_csr_scalar_load_10k_vseq

make eda_batch_run \
  REMOTE_HOST=172.28.10.101 \
  mode=csr_sequence_my_run \
  tc=basicTest \
  ts=memblock_csr_scalar_load_10k_vseq \
  plus_file=../seq/csr_scalar_stress/cfg \
  cfg=tc_csr_scalar_load_10k \
  seed=710105 \
  wave=on \
  note=csr_load_check
```

### 8.2 使用自定义 cfg、复用通用 VSEQ

```bash
make eda_batch_run \
  REMOTE_HOST=172.28.10.101 \
  mode=csr_sequence_my_run \
  tc=basicTest \
  ts=memblock_csr_random_config_vseq \
  plus_file=../seq/my_csr_case/cfg \
  cfg=tc_my_csr_case \
  seed=123456 \
  wave=on \
  note=my_csr_case
```

参数说明：

- <code>plus_file</code> 是 cfg 所在目录；
- <code>cfg</code> 是不带 <code>.cfg</code> 的文件名；
- <code>ts</code> 会生成 <code>+VSEQ_MAIN=&lt;ts&gt;</code>，因此 stress 风格 cfg 不需要重复写 <code>+VSEQ_MAIN</code>；
- <code>plus_arg='+KEY=value ...'</code> 可以临时覆盖 cfg 中同名 key；
- 编译和运行必须使用相同 <code>mode</code>；
- VCS/Verdi 工具由远端 wrapper 在 eda01 加载，不在当前节点直接启动仿真。

<a id="section-9"></a>

## 9. 日志与波形验收

### 9.1 最小通过判据

| 检查项 | 期望 |
|---|---|
| <code>phase=initial</code> CSR profile | 1 组 |
| <code>initial complete CSR configuration committed</code> | 恰好 1 次 |
| <code>phase=dynamic ... owner=uid_*</code> | 与 CSR marker 数一致；static-only 为 0 |
| <code>dynamic CSR candidate staged and sent</code> | 与 dynamic profile 数一致 |
| 四路 changed 上升沿 | 四路完全同拍，次数与 marker 数一致 |
| stress final audit | business/marker/check_store/table/nonterminal 全部符合用例期望 |
| UVM summary | <code>UVM_ERROR=0</code>、<code>UVM_FATAL=0</code> |

建议的日志检查命令：

```bash
rg -c 'MEMBLOCK_CSR_PROFILE_AUDIT.*phase=initial' <log>
rg -c 'MEMBLOCK_CSR_PROFILE_AUDIT.*phase=dynamic' <log>
rg -c 'dynamic CSR candidate staged and sent' <log>
rg 'MEMBLOCK_CSR_SCALAR_STRESS_AUDIT.*phase=final' <log>
rg 'UVM_ERROR :|UVM_FATAL :' <log>
```

### 9.2 波形信号

在 Verdi 中至少观察：

```text
/top_tb/u_memblock__csr_ctrl_agent_if/io_ooo_to_mem_tlbCsr_satp_changed
/top_tb/u_memblock__csr_ctrl_agent_if/io_ooo_to_mem_tlbCsr_vsatp_changed
/top_tb/u_memblock__csr_ctrl_agent_if/io_ooo_to_mem_tlbCsr_hgatp_changed
/top_tb/u_memblock__csr_ctrl_agent_if/io_ooo_to_mem_tlbCsr_priv_virt_changed
```

同时按目标 group 加入 SATP/VSATP/HGATP mode、ASID/VMID、permission、privilege 和 distributed CSR write 信号。

<div class="warn">
<code>dynamic CSR candidate staged and sent</code> 是完成侧日志，不是 changed 上升沿日志。无 PMP/PMA write 时它通常在 driver 下一拍清零后打印；有 write plan 时还会更晚。精确切换时间必须看 FSDB 的四路 changed 0→1。
</div>

<a id="section-10"></a>

## 10. 常见错误与定位

| 现象/FATAL | 常见原因 | 处理 |
|---|---|---|
| <code>requires AUTO control topology</code> | topology 不是 1 | 设置 <code>MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=1</code> |
| <code>requires main memory ranges and the PMA/PMP model</code> | 必需模型未开启 | 设置 <code>MEMBLOCK_MAIN_MEM_RANGES_EN=1</code>、<code>MEMBLOCK_PMA_PMP_MODEL_EN=1</code> |
| <code>AUTO_MAIN_TABLE requires MEMBLOCK_USE_MANUAL_MAIN_TABLE=0</code> | AUTO 与旧 manual 开关冲突 | 设置 manual=0 |
| <code>+VSEQ_MAIN type is not registered</code> | class 未 include/注册或名字错误 | 检查 <code>seq_pkg.sv</code>、factory 宏和 <code>ts</code> |
| <code>active control topology ... requires dispatch-capable scenario</code> | 新 VSEQ 未加 basicTest 能力白名单 | 更新两个 VSEQ capability 函数 |
| <code>initial CSR sequence started outside the dedicated CSR scenario</code> | 直接启动 initial 或 special scope 未建立 | 从 <code>memblock_csr_random_config_vseq</code> 派生/启动 |
| <code>no legal weighted solution</code> | 某启用组权重全 0、范围非法或无非当前候选 | 打开至少一个合法权重并检查 MIN/MAX |
| marker 数量不符合预期 | N/I 公式错误、interval 为随机范围或与 SFence 同位碰撞 | 固定 interval；按 <code>floor((N-1)/I)</code> 计算；专项 CSR 用例关闭 SFence |
| changed 次数少于 marker | action 未完成或仿真提前结束 | 查 owner/action queue、runtime snapshot、PMP/PMA model 和 global stop |
| candidate 日志比波形晚 | 正常完成侧日志语义 | 用 changed 上升沿作为真实接口时间 |
| CSR snapshot timeout | CSR sequencer 存在另一个 producer、monitor 未发布 full snapshot | 检查 default sequence replacement 和 VSEQ CSR lane |

<a id="section-11"></a>

## 11. 不可破坏的不变量

<div class="danger">
1. 同一时刻 <code>csr_ctrl_sqr</code> 只能有一个 CSR producer。<br>
2. initial sequence 必须先于普通 traffic producer 完成并发布 committed state。<br>
3. dynamic child 只能由 CSR worker 消费合法 action 后启动，不能由用例直接调用。<br>
4. marker 必须等待 <code>commit_cursor_uid == marker_uid</code>，不能改成看到主表 flag 就立即发送。<br>
5. 动态 item 的四路 changed 必须全部置 1，并由 driver 下一周期统一清 0。<br>
6. candidate 必须等 runtime/full snapshot 和需要的 PMP/PMA model 观察完成后再成为 committed state。<br>
7. dynamic sequence 不能随机 startup-only enable 字段。<br>
8. 新用例不得修改原有 cfg；应放入独立目录并用 <code>plus_file</code> 选择。<br>
9. 不得将 queue/event/sendover 日志当作 DUT 已观察完成的唯一事实。<br>
10. 不得为了新增用例修改 CSR driver 的普通工作路径或引入第二套 changed pulse 逻辑。
</div>

<a id="section-12"></a>

## 12. 文件索引与参考用例

| 类别 | 路径 | 用途 |
|---|---|---|
| 推荐 VSEQ | <span class="path">seq/virtual_sequence/memblock_csr_random_config_vseq.sv</span> | 通用 CSR initial/dynamic 场景入口 |
| initial sequence | <span class="path">seq/base_seq/memblock_csr_initial_config_sequence.sv</span> | 启动期完整静态 CSR 配置 |
| CSR worker | <span class="path">seq/base_seq/memblock_csr_control_base_sequence.sv</span> | action queue 消费和 dynamic child 调度 |
| dynamic child | <span class="path">seq/base_seq/memblock_dynamic_csr_change_sequence.sv</span> | 运行期合法随机和 CSR item/write plan |
| control service | <span class="path">seq/base_seq_help/memblock_control_barrier_service.sv</span> | commit gate、owner、action 和观察闭环 |
| 参数快照 | <span class="path">seq/base_seq_help/seq_csr_common.sv</span> | plus 读取、合法性校验和只读 getter |
| 参数定义 | <span class="path">env/plus.sv</span> | CSR plus 参数集中定义/加载 |
| CSR 类型 | <span class="path">seq/base_seq_help/memblock_dispatch_types.sv</span> | <code>memblock_csr_sequence_cfg_t</code> 等类型 |
| stress VSEQ base | <span class="path">seq/csr_scalar_stress/virtual_sequence/memblock_csr_scalar_stress_vseq_base.sv</span> | 定制 10K 用例的推荐 VSEQ 模板 |
| stress main base | <span class="path">seq/csr_scalar_stress/base_seq/memblock_csr_scalar_stress_main_sequence_base.sv</span> | 10K/marker/check_store/terminal 审计 |
| 已通过 cfg | <span class="path">seq/csr_scalar_stress/cfg/</span> | load/store/mixed/STA-STD 可复制参考 |
| factory 入口 | <span class="path">tc/src/basicTest.sv</span> | VSEQ 选择和 topology capability 白名单 |
| 编译注册 | <span class="path">seq/seq.f</span>、<span class="path">seq/seq_pkg.sv</span> | include directory 和 class include 顺序 |
| 仿真入口 | <span class="path">sim/Makefile</span>、<span class="path">sim/remote_eda_make.sh</span> | eda01 跨节点编译/运行 |

配套文档：

- [CSR Sequence 实施方案](/nfs/home/wangyan/Mem_env_work_20260716/csr_analysis/csr_sequence_implementation_plan/memblock_csr_sequence_implementation_plan_zh.md)
- [环境改动与 CSR 设计说明](/nfs/home/wangyan/Mem_env_work_20260716/csr_analysis/csr_sequence_plan_record/memblock_csr_sequence_environment_changes_and_design_zh.md)
- [压力测试总结](../analysis/testcase_flow/memblock_csr_scalar_10k_simulation_summary_20260909.md)
- [changed 日志与波形时序分析](./memblock_csr_changed_log_waveform_timing_analysis_zh.md)
- [CSR 静态默认值参考](/nfs/home/wangyan/Mem_env_work_20260716/csr_analysis/csr_sequence_configuration_guides/memblock_csr_static_default_value_reference_zh.md)
- [CSR 动静态配值指南](/nfs/home/wangyan/Mem_env_work_20260716/csr_analysis/csr_sequence_configuration_guides/memblock_csr_dynamic_static_value_guide_zh.md)
- [CSR enable 信号指南](/nfs/home/wangyan/Mem_env_work_20260716/csr_analysis/csr_sequence_configuration_guides/memblock_csr_enable_signal_guide_zh.md)

<p class="meta"><a href="#top">返回顶部</a></p>
