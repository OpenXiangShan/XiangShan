# mem_ut V2 轻量 L2Cache Response、Hint 与 Probe 专项 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2cache_response_hint_probe_model_coding_plan_20260717.md |
| 关联 Flow | AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md |
| 目标版本 | V2，分支 mem_ut_uvm_v2 |
| Review 范围 | DCache coherent responder、L2 sideband、参数链、driver 时序和相关文档 |
| 当前状态 | coding、文档同步、验证和最终独立 reviewer 已完成；review 结论 `FINAL PASS` |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 示例 |
|---|---|---|---|
| service_cycle | responder 自己维护的逻辑拍计数，不等同于绝对仿真时间 | dcache_mem__access_base_sequence::service_cycle | A.fire 后 delay=6，则首拍 D 在逻辑拍 A_cycle+6 到期 |
| armed snapshot | 已采到 DUT 的稳定 valid，但尚未在下一采样边界确认 handshake 的请求快照 | armed_a_req_xact、armed_c_req_xact | 本拍看到 A.valid，下一拍才根据 A.ready 确认 A.fire |
| fire | 某 channel 的 valid 和 ready 在同一采样边界同时为 1 | a_fire、b_fire、c_fire、d_fire、e_fire | 只有 fire 后才能建立 pending 状态 |
| pending D | 已真实接受、等待延迟到期或等待 DUT ready 的 D response | pending_d_* | GrantData 两个 beat 共用一个 pending owner |
| GrantAck owner | Grant/GrantData 最后一拍 D.fire 后，等待 E.fire 的生命周期所有权 | waiting_grant_ack、pending_grant_* | 只有 owner 存在时 e_ready=1 |
| cached line table | 已完成 GrantAck、可作为 Probe 候选的 64B line 影子表 | cached_alias_by_line | key 是 line 对齐地址，value 是 alias |
| Probe owner | 当前唯一的 B Probe 或其 C 回复生命周期 | pending_probe_b_valid、waiting_probe_c | B.fire 后等待 ProbeAck/ProbeAckData |
| C assembly | 收集 ProbeAckData/ReleaseData 两个 32B beat 的状态 | c_assembly_* | 第二个 beat 到达后才写回主存 |
| lockstep driver | driver 阻塞取 item、立即写 clocking output，由 sequence 下一边界确认握手的 driver 合同 | dcache_agent_agent_driver::main_phase() | 不复用上一 item，不插入额外 hold |
| safe idle | 所有 response valid/ready 与 sideband 为 0 的安全 item | build_dcache_idle_xaction() | reset/terminal 时发送一次 |
| in-flight | 尚未完成 handshake 或生命周期的状态；完成的 cache line map 不属于 in-flight | pending/owner/armed 字段 | global stop 必须等待它们归零 |
| owner | 某个协议生命周期的唯一状态维护者，第二条路径不得覆盖它 | waiting_grant_ack、c_assembly_owner、waiting_probe_c | C assembly 存在时不能新建 A pending D |
| drain | global stop 后只完成已有 DUT 事务、不再创建新 Probe 的收敛阶段 | body() stop 分支、dcache_responder_done | pending D 和 C assembly 清空后才退出 |
| sideband | 不属于 A/B/C/D/E 主握手的 Hint/flush 输入 | io_l2_hint_*、io_l2_flush_done | 发送前做四态和已知零检查 |

抽象功能说明：本专项把旧的“看到 A.valid 后阻塞发送 D”模型改为一个单一逐拍的轻量 DCache
coherent responder。它负责 A/C/D/E/B 的协议细节、Hint sideband、一个 cache line 影子表和一个
Probe 生命周期，但不实现完整 L2 directory、MSHR、权限目录或多 outstanding。主表、issue、
writeback、commit 和 terminal 的 owner 不被本专项替换。

## 2. Review 方法与调用关系

本 review 逐项对照 coding diff、执行 plan 的原始方案和 IMPLEMENTATION_DELTA，并复查 flow 文档
是否描述实际代码。主要调用关系如下：

| 顺序 | 函数/模块 | 输入 | 对外结果或副作用 |
|---|---|---|---|
| 1 | plus::reload_from_cmdline() | plusarg/cfg | 将五个 L2 参数加载到公共参数对象 |
| 2 | seq_csr_common::load_from_plus() | plus::* | 保存 runtime snapshot |
| 3 | seq_csr_common::validate_and_clamp() | snapshot | 对权重做 fail-fast 检查，不静默修正 |
| 4 | dcache_mem__access_base_sequence::body() | VIF、公共状态、DUT ready/valid | 每拍仲裁并提交一个 response item |
| 5 | build_dcache_idle_xaction() | 无 | 构造 known-zero 基线，e_ready 默认 0 |
| 6 | dcache_agent_agent_driver::main_phase() | sequence item | 立即写 clocking output，避免重复 beat |
| 7 | accept_dcache_a_request() | 已确认 A.fire 的快照 | 建立 Grant/GrantData/CBOAck pending D |
| 8 | process_d_fire() | 已确认 D.fire | 推进 beat 或建立 GrantAck owner |
| 9 | process_e_fire() | 已确认 E.fire | 校验 sink、插入 cached line table |
| 10 | start_c_assembly()/consume_c_beat() | 已确认 C.fire | 建立/推进 Probe 或 Release C 生命周期 |
| 11 | complete_*_c_assembly() | 完整 C data | 主存写回、删除 map、建立 ReleaseAck |

## 3. 公共参数链

### 3.1 plus.sv 参数声明和加载

源码位置：mem_ut/ver/ut/memblock/env/plus.sv:209-214, 397-401，对象：plus。

抽象功能描述：为公共测试框架提供 DCache responder 的三档 response delay 权重、Hint 命中权重
和 Probe 尝试权重；该对象只负责参数定义及从命令行读取，不负责随机调度或协议状态。

修改前逻辑：没有这五个 L2 专项参数，responder 只能使用固定行为；新增参数若散落在 sequence
会导致不同入口无法使用同一配置。

修改后关键源码：

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2_RSP_DELAY_SMALL_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2_RSP_DELAY_LARGE_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2_HINT_VALID_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2_PROBE_ENABLE_WT, int, 0)
```

中文伪代码：在公共 plus 类中定义五个带默认值的整数参数；启动时分别读取对应 plusarg，未提供时保留默认值；不在这里判断某次 request 是否产生 response。

reload_from_cmdline() 对五个 key 使用现有 load_int()，因此 cfg 和单独 plusarg 共享同一入口。参数
所有权仍在公共 runtime plus 层，没有新增 env cfg 或编译期结构镜像。

### 3.2 seq_csr_common 快照、校验和 getter

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv:188-201, 402-410,
421-520, 1506-1535。

抽象功能描述：把 raw plus 值转成本次仿真的只读 runtime snapshot，校验非法权重，并向 responder
提供只读 getter；它不保存 DCache pending state，也不参与 channel 仲裁。

修改后关键源码：

```systemverilog
l2_rsp_delay_small_wt  = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_SMALL_WT", plus::MEMBLOCK_L2_RSP_DELAY_SMALL_WT);
l2_rsp_delay_medium_wt = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT", plus::MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT);
l2_rsp_delay_large_wt  = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_LARGE_WT", plus::MEMBLOCK_L2_RSP_DELAY_LARGE_WT);
l2_hint_valid_wt       = get_non_negative_int("MEMBLOCK_L2_HINT_VALID_WT", plus::MEMBLOCK_L2_HINT_VALID_WT);
l2_probe_enable_wt     = get_non_negative_int("MEMBLOCK_L2_PROBE_ENABLE_WT", plus::MEMBLOCK_L2_PROBE_ENABLE_WT);
```

中文伪代码：初始化时逐项读取并拒绝负值；把五个值保存到静态 snapshot；后续 responder 只通过 getter 读取，不直接访问 plus 原始字段。

```systemverilog
fatal_if_all_zero3("DCache L2 response delay weights",
                   l2_rsp_delay_small_wt,
                   l2_rsp_delay_medium_wt,
                   l2_rsp_delay_large_wt);
if (l2_hint_valid_wt > 100) begin
    `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_L2_HINT_VALID_WT must be within [0:100]")
end
if (l2_probe_enable_wt > 100) begin
    `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_L2_PROBE_ENABLE_WT must be within [0:100]")
end
```

中文伪代码：三个 delay 权重全为零时立即 fatal；Hint/Probe 权重超过 100 时立即 fatal；合法值不被 clamp；getter 只返回 snapshot，不改变任何 responder 状态。

原因：delay 类别没有可选项时，继续运行会在 randomize 阶段卡住或产生误导；百分比越界若被静默
截断会使 testcase 配置与实际激励不一致。

## 4. 独立专项 cfg

源码位置：mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg 和
mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg。

抽象功能描述：为默认测试和 L2 专项测试提供稳定的参数 preset；cfg 只提供行为参数，不创建新的
sequence 或 driver。

修改前逻辑：没有 L2 专项 cfg；原 plan 曾将新 cfg 描述为只覆盖 L2 参数，但 Makefile 的 VCS
配置逻辑每次只读取一个 cfg，不会自动继承 tc_dispatch_real_smoke.cfg。

修改后关键 cfg：

```text
+MEMBLOCK_MAIN_TRANS_NUM=1
+MEMBLOCK_LSQENQ_SEQ_EN=1
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
+MEMBLOCK_LSQCOMMIT_SEQ_EN=1
+MEMBLOCK_L2TLB_SEQ_EN=1
+MEMBLOCK_L2_RSP_DELAY_SMALL_WT=6
+MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT=3
+MEMBLOCK_L2_RSP_DELAY_LARGE_WT=1
+MEMBLOCK_L2_HINT_VALID_WT=0
+MEMBLOCK_L2_PROBE_ENABLE_WT=0
```

中文伪代码：专项 cfg 显式复制 real-smoke 所需的主流程、LSQ、L2TLB 和 TLB 合法性开关，再覆盖三档 delay；Hint/Probe 默认关闭，专用定向场景通过单独 plusarg 打开。

这是本次执行中的必要补充：若 cfg 只含五个 L2 参数，直接使用该 cfg 会回到 plus 类默认的
DISPATCH_ISSUE_SEQ_EN=0，无法形成真实 dispatch 到 DCache 的 flow。

## 5. interface、xaction 与 generic sideband

### 5.1 interface time-zero 初始化

源码位置：mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv:74-77。

抽象功能描述：在 driver 或专用 responder 接管前，为四个 V2 L2 sideband 建立确定的零初值；不改变
TileLink channel 的握手定义。

```systemverilog
logic io_l2_hint_valid = '0;
logic [3:0] io_l2_hint_bits_sourceId = '0;
logic io_l2_hint_bits_isKeyword = '0;
logic io_l2_flush_done = '0;
```

中文伪代码：接口实例化时将 Hint valid、Hint payload 和 flush_done 置零；后续只有 clocking driver 或 responder item 能改变它们。

### 5.2 xaction 默认约束

源码位置：mem_ut/ver/ut/memblock/agent/dcache_agent_agent_xaction.sv:418-450。

抽象功能描述：限制 generic transaction/random 路径不能制造无 owner 的 Hint 或 flush sideband；专用
responder 通过手工赋值构造合法 Hint item，不调用该 item 的 randomize。

```systemverilog
constraint dcache_agent_agent_xaction::default_io_l2_hint_valid_cons{
    io_l2_hint_valid == 1'b0;
}
constraint dcache_agent_agent_xaction::default_io_l2_hint_bits_sourceId_cons{
    if (io_l2_hint_valid == 1'b0) io_l2_hint_bits_sourceId == 4'd0;
    else io_l2_hint_bits_sourceId inside {[4'd0:4'd15]};
}
constraint dcache_agent_agent_xaction::default_io_l2_flush_done_cons{
    io_l2_flush_done == 1'b0;
}
```

中文伪代码：generic random 默认 valid=0、payload=0、flush=0；valid=0 时 payload 必须继续为零；专用 responder 若需要 Hint，则创建 item 后手工覆盖 valid/payload。

### 5.3 driver sideband 检查

源码位置：mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv:77-94。

抽象功能描述：在任何 item 第一次写入 VIF 前检查 sideband 自洽性；driver 不维护 pending request map，
只负责合同检查和透传。

```systemverilog
function void dcache_agent_agent_driver::check_l2_sideband_item(dcache_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "cannot check a null DCache item")
    end
    if (tr.io_l2_flush_done !== 1'b0) begin
        `uvm_fatal(get_type_name(), "io_l2_flush_done must stay 0 in DCache responder items")
    end
    if (tr.io_l2_hint_valid !== 1'b0 && tr.io_l2_hint_valid !== 1'b1) begin
        `uvm_fatal(get_type_name(), "io_l2_hint_valid must be known before driving the DUT")
    end
    if (tr.io_l2_hint_valid === 1'b0 &&
        (tr.io_l2_hint_bits_sourceId !== '0 || tr.io_l2_hint_bits_isKeyword !== 1'b0)) begin
        `uvm_fatal(get_type_name(), "hint payload must be zero when valid is zero")
    end
    if (tr.io_l2_hint_valid === 1'b1 &&
        ((^tr.io_l2_hint_bits_sourceId === 1'bx) ||
         (tr.io_l2_hint_bits_isKeyword !== 1'b0 &&
          tr.io_l2_hint_bits_isKeyword !== 1'b1))) begin
        `uvm_fatal(get_type_name(), "hint payload must be known when valid is one")
    end
endfunction
```

中文伪代码：发送前先拒绝 null item；flush_done 必须为已知 0；Hint valid 必须为已知 0/1；valid=0
时 sourceId/isKeyword 必须全 0，valid=1 时 payload 必须已知；检查通过后才写入所有 channel 和
sideband，四态非法值不能绕过 fail-fast。

## 6. DCache driver 时序修复

源码位置：mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv:51-75，
函数：main_phase()。

抽象功能描述：在 responder 模式下把 sequence item 一对一提交到 clocking output，保证 sequence 的
last_cycle_xact 与实际驱动 item 一致；它不判断协议 fire，也不推进 DCache lifecycle。

修改前逻辑：使用 try_next_item()；无新 item 时等待 clocking event 并继续 drive_idle()，旧的
hold 行为可能在 DUT ready=1 时重复留下上一 D valid。

修改后源码：

```systemverilog
while (1) begin
    req = null;
    seq_item_port.get_next_item(req);
    if (req == null) begin
        `uvm_fatal(get_type_name(), "get_next_item returned a null DCache item")
    end
    this.send_pkt(req);
    seq_item_port.item_done();
end
```

中文伪代码：清空旧句柄；阻塞取得新 item；空 item 直接 fatal；非空 item 立即写 VIF；完成 item_done；循环等待下一 item，不再重复发送上一 item。

send_pkt() 还要求 pre_pkt_gap/post_pkt_gap 为 0。这是因为本 responder 的 delay、backpressure 和
owner 状态由 sequence 自己管理；若 driver 再插入 gap，service_cycle 与 DUT 采样边界会失配。

## 7. 主存范围和基础 helper

### 7.1 responder 启动时绑定 PADDR range

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1339-1346，函数：body() 初始化段。

抽象功能描述：把公共的物理地址窗口绑定到 DCache responder 私有主存 range，让完整 line 检查具有实际
约束；不改变主表虚拟地址生成，也不替代 TLB map builder。

```systemverilog
seq_csr_common::init();
clear_main_mem_ranges();
init_main_mem_range(mem_addr_t'(seq_csr_common::get_paddr_base()),
                    seq_csr_common::get_paddr_range());
check_l2_model_cfg();
```

中文伪代码：先初始化公共参数；清掉该 sequence 旧 range；读取 PADDR base/range；注册为当前 DCache 主存访问窗口；随后再检查 L2 参数并进入 service loop。

### 7.2 check_line_range() 和 load_grant_line()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:698-725。

抽象功能描述：在建立 GrantData 或接收 Release/Probe data 前检查完整 64B line，并用既有主存 helper
读取/写回两个 32B beat。

```systemverilog
full_line_mask = '0;
full_line_mask[63:0] = 64'hffff_ffff_ffff_ffff;
if (!is_main_mem_access_in_range(line_addr, full_line_mask)) begin
    `uvm_fatal(get_type_name(), "line is outside configured main memory range")
end
```

中文伪代码：构造覆盖 64 个字节的 mask；逐字节检查 line 是否落入共享 PADDR range；任一字节越界立即 fatal，不建立成功 Grant。

load_grant_line() 随后分别读取 line_addr 和 line_addr+32，任一 beat corrupt/denied 都 fatal；
C data 完成时使用同一主存 helper 进行两次全 mask store。主存数据仍只有
mem_access_base_sequence::main_mem 一份，cached line table 不复制 data。

## 8. responder 状态初始化和 item 基线

### 8.1 clear_runtime_state()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:510-531，函数：clear_runtime_state()。

抽象功能描述：在创建 sequence、reset 或清理周期统一释放所有 DCache responder 生命周期；可选地清除
已完成 cache line 表。

```systemverilog
a_accept_armed = 1'b0;
c_accept_armed = 1'b0;
armed_a_req_xact = null;
armed_c_req_xact = null;
waiting_grant_ack = 1'b0;
pending_probe_b_valid = 1'b0;
waiting_probe_c = 1'b0;
clear_pending_d_state();
clear_c_assembly_state();
clear_hint_state();
if (clear_cache_map) cached_alias_by_line.delete();
```

中文伪代码：清除 A/C armed snapshot、GrantAck owner、Probe owner、pending D、C assembly 和 Hint；reset 参数要求为 1 时再删除所有 cached line，避免旧 epoch 状态残留。

### 8.2 build_dcache_idle_xaction()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:533-565。

抽象功能描述：为每个 service cycle 提供 known-zero item 基线，防止上一拍的 valid、payload 或
sideband 泄漏到下一拍；GrantAck owner 之后由主循环局部覆盖 e_ready。

```systemverilog
rsp_xact.auto_inner_dcache_client_out_a_ready = 1'b0;
rsp_xact.auto_inner_dcache_client_out_b_valid = 1'b0;
rsp_xact.auto_inner_dcache_client_out_c_ready = 1'b0;
rsp_xact.auto_inner_dcache_client_out_d_valid = 1'b0;
rsp_xact.auto_inner_dcache_client_out_e_ready = 1'b0;
rsp_xact.io_l2_hint_valid = 1'b0;
rsp_xact.io_l2_flush_done = 1'b0;
rsp_xact.pre_pkt_gap = 0;
rsp_xact.post_pkt_gap = 0;
```

中文伪代码：先把 A/B/C/D/E 的 ready/valid、所有 payload、Hint 和 flush_done 清零；把 gap 固定为零；只有当前 owner helper 才能覆盖自己负责的字段，默认不开放 E。

## 9. A/C snapshot 与真实 fire

### 9.1 capture 和稳定性检查

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:567-649，函数：capture_dcache_a_xaction()、
capture_dcache_c_xaction()、check_a_payload_stable()、check_c_payload_stable()。

抽象功能描述：从 DUT input snapshot 复制协议 header/payload，并在 valid 保持期间检查下一拍
payload 未变化；这些 helper 不建立 pending response，只为下一拍 fire 判定提供证据。

修改前逻辑：旧 body 直接读 raw VIF 的 A.valid，随后发送 ready 并进入阻塞 response，缺少明确的
稳定 payload 和“已真实握手”边界。

修改后流程：看到 valid 时复制 snapshot 并 arm ready；下一次 drv_cb 采样用
last_cycle_xact 的 ready/valid 与当前对端值计算 fire；fire 成功才把 armed snapshot 交给
accept_dcache_a_request() 或 C assembly。

中文伪代码：采样 valid 时复制所有 header/data；下一边界若对端 ready=1 且上一 item valid/ready=1，则校验 snapshot 与实际字段一致；不一致 fatal；若 valid 消失则取消 armed snapshot，不建立 pending。

## 10. A request 分类和 pending D

### 10.1 accept_dcache_a_request()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:858-968。

抽象功能描述：消费一个已经确认的 A.fire，完成 coherent opcode/size/source/param/range 检查，
并创建唯一 pending D；它不发送 item，也不处理 D/E fire。

修改前逻辑：旧 dcache_d_opcode() 对未知 opcode 使用默认 fallback，A 接收后直接进入 for-loop。

修改后关键源码：

```systemverilog
line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
check_line_range(line_addr, "dcache coherent A");
clear_pending_d_state();
pending_d_due_cycle = accept_cycle + sample_l2_response_delay();
pending_d_line_addr = line_addr;
pending_d_size = req_xact.auto_inner_dcache_client_out_a_bits_size;
pending_d_source = req_xact.auto_inner_dcache_client_out_a_bits_source;
```

中文伪代码：先对齐地址并检查完整 line；清理旧 pending；只在 A.fire 的逻辑拍采样一次 delay；保存 line、size、source，后续不重采样。

```systemverilog
case (req_xact.auto_inner_dcache_client_out_a_bits_opcode)
  TL_A_OPCODE_ACQUIRE_BLOCK: begin
    load_grant_line(line_addr, line_data_low, line_data_high);
    pending_d_kind = DCACHE_PENDING_D_GRANT_DATA;
    pending_d_beat_count = 2;
    pending_d_sink = TL_FIXED_SINK;
  end
  TL_A_OPCODE_ACQUIRE_PERM: begin
    pending_d_kind = DCACHE_PENDING_D_GRANT;
    pending_d_beat_count = 1;
    pending_d_sink = TL_FIXED_SINK;
  end
  TL_A_OPCODE_CBO_CLEAN,
  TL_A_OPCODE_CBO_FLUSH,
  TL_A_OPCODE_CBO_INVAL: begin
    pending_d_kind = DCACHE_PENDING_D_CBO_ACK;
    pending_d_beat_count = 1;
  end
  default: `uvm_fatal(get_type_name(), "unsupported DCache coherent A opcode")
endcase
```

中文伪代码：AcquireBlock 检查 source/param 后读取两拍 line data，并可排 Hint；AcquirePerm 只接受 NtoT/BtoT 并建立单拍 Grant；CBO 只接受 source=17 并建立 CBOAck；其它 opcode 在 pending 建立前 fatal，不生成 AccessAckData。

### 10.2 sample_l2_response_delay()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:744-785。

抽象功能描述：使用三个公共权重先选择 delay 类别，再在固定区间内均匀选择具体 service-cycle 数；
它只返回一个值，不修改 pending state。

```systemverilog
if (!std::randomize(delay_class) with {
    delay_class dist {0 := small_wt, 1 := medium_wt, 2 := large_wt};
}) begin
    `uvm_fatal(get_type_name(), "failed to randomize DCache L2 response delay class")
end
```

中文伪代码：读取三个 getter；按 small/medium/large 权重抽类别；抽样失败 fatal；small 在 3..5、medium 在 6..15、large 在 16..50 内抽具体 delay；返回值固定到当前 pending response。

## 11. D response、D.fire 和 E.fire

### 11.1 build_pending_d_xaction()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:970-1009。

抽象功能描述：依据 pending kind 构造当前拍 D item；在 D.ready=0 时由主循环继续构造相同字段，
不推进 beat index。

```systemverilog
cycle_xact.auto_inner_dcache_client_out_d_valid = 1'b1;
cycle_xact.auto_inner_dcache_client_out_d_bits_param = pending_d_param;
cycle_xact.auto_inner_dcache_client_out_d_bits_size = pending_d_size;
cycle_xact.auto_inner_dcache_client_out_d_bits_source = pending_d_source;
cycle_xact.auto_inner_dcache_client_out_d_bits_sink = pending_d_sink;
```

中文伪代码：pending 到期时打开 D.valid，复制保存的 param/size/source/sink/error 字段；按 kind 选择 Grant、GrantData、CBOAck 或 ReleaseAck；GrantData 按 beat index 和 keyword 选择半行 data。

### 11.2 process_d_fire()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1011-1050。

抽象功能描述：只消费已经确认的 D.fire，推进 GrantData beat 或结束当前 D response；它不假设
d_valid=1 就已经完成。

```systemverilog
if (pending_d_kind == DCACHE_PENDING_D_GRANT_DATA &&
    (pending_d_beat_idx + 1) < pending_d_beat_count) begin
    pending_d_beat_idx++;
end else if (pending_d_kind == DCACHE_PENDING_D_GRANT_DATA ||
             pending_d_kind == DCACHE_PENDING_D_GRANT) begin
    waiting_grant_ack = 1'b1;
    pending_grant_expected_sink = pending_d_sink;
    clear_pending_d_state();
    clear_hint_state();
end
```

中文伪代码：GrantData 非最后 beat 只递增 beat index；最后 beat 或单拍 Grant 建立 GrantAck owner、保存 line/alias/sink、清 pending；CBOAck/ReleaseAck fire 后清 pending，CBO flush/inval 同时删除 map。

### 11.3 process_e_fire()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1052-1067。

抽象功能描述：消费 GrantAck E.fire，确认 E 属于当前 owner，并在协议生命周期真正完成后插入 cache
line table。

```systemverilog
if (!waiting_grant_ack) begin
    `uvm_fatal(get_type_name(), "unexpected E.valid when no GrantAck is pending")
end
if (dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink != pending_grant_expected_sink) begin
    `uvm_fatal(get_type_name(), "GrantAck sink mismatch")
end
record_cached_line(pending_grant_line, pending_grant_alias);
waiting_grant_ack = 1'b0;
```

中文伪代码：没有 owner 时的 E.valid 直接 fatal；sink 不等于保存的 sink 直接 fatal；匹配后插入 line/alias，清 owner；下一拍才重新开放 A/Probe。

## 12. Hint 和 Probe

### 12.1 sample_hint_enable() / service_hint()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:787-807, 1299-1309。

抽象功能描述：为每个已接受 AcquireBlock 选择至多一个 Hint，并在固定 due cycle 输出单拍 pulse；
不改变 D response 的 due cycle。

```systemverilog
if (hint_selected && !hint_sent && current_cycle == hint_due_cycle) begin
    cycle_xact.io_l2_hint_valid = 1'b1;
    cycle_xact.io_l2_hint_bits_sourceId = hint_source_id;
    cycle_xact.io_l2_hint_bits_isKeyword = hint_isKeyword;
    hint_sent = 1'b1;
end
```

中文伪代码：先由权重决定是否命中；命中则保存 sourceId/isKeyword 和 due cycle；当前拍到 due 且尚未发送时拉 valid 一拍并置 sent；其它拍保持 sideband 基线零。

### 12.2 try_start_probe() / select_random_cached_line()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:831-856, 1284-1297。

抽象功能描述：在 responder 完全空闲时，从已完成 GrantAck 的 line table 中选择一个 Probe 候选并
建立单 Probe owner；不模拟第二份 directory。

```systemverilog
if (!sample_probe_enable()) return;
if (!select_random_cached_line(selected_line, selected_alias)) return;
pending_probe_b_valid = 1'b1;
pending_probe_line = selected_line;
pending_probe_alias = selected_alias;
```

中文伪代码：权重未命中或 map 为空就跳过；否则等概率选一条 line，保存 line/alias，置 B pending；主循环保持 B.valid 直到 B.fire。

## 13. C-channel assembly 和主存副作用

### 13.1 start_c_assembly()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1181-1282。

抽象功能描述：根据首个已经确认的 C.fire 判断是 Probe 回复还是 Release/ReleaseData，校验 line、
size、param 和 owner，并为多拍 transaction 建立 assembly。

```systemverilog
case (c_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
  TL_C_OPCODE_PROBE_ACK: begin
    if (!waiting_probe_c) `uvm_fatal(get_type_name(), "ProbeAck without owner");
    remove_cached_line(line_addr, "probe_toN");
    waiting_probe_c = 1'b0;
  end
  TL_C_OPCODE_PROBE_ACKDATA: begin
    c_assembly_owner = DCACHE_C_OWNER_PROBE;
    consume_c_beat(c_req_xact, accept_cycle);
  end
  TL_C_OPCODE_RELEASE: begin
    pending_d_kind = DCACHE_PENDING_D_RELEASE_ACK;
    pending_d_valid = 1'b1;
  end
  TL_C_OPCODE_RELEASEDATA: begin
    c_assembly_owner = DCACHE_C_OWNER_RELEASE;
    consume_c_beat(c_req_xact, accept_cycle);
  end
  default: `uvm_fatal(get_type_name(), "unsupported DCache C opcode")
endcase
```

中文伪代码：ProbeAck 必须有 Probe owner，单拍完成后删 map；ProbeAckData 建立 Probe assembly 并消费首 beat；Release 直接建立延迟后的 ReleaseAck；ReleaseData 建立 Release assembly；其它 C opcode fatal。

### 13.2 consume_c_beat()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1126-1179。

抽象功能描述：检查多拍 C 的 opcode、line、source、size、param 是否稳定，累积 data 和 corrupt，
在收齐两拍时调用对应 completion task。

```systemverilog
if (c_req_xact.auto_inner_dcache_client_out_c_bits_opcode != c_assembly_opcode) begin
    `uvm_fatal(get_type_name(), "C opcode changed during multi-beat assembly")
end
if (c_assembly_received_beats == 0)
    c_assembly_data[255:0] = c_req_xact.auto_inner_dcache_client_out_c_bits_data;
else
    c_assembly_data[511:256] = c_req_xact.auto_inner_dcache_client_out_c_bits_data;
c_assembly_corrupt_seen |= c_req_xact.auto_inner_dcache_client_out_c_bits_corrupt;
c_assembly_received_beats++;
```

中文伪代码：没有 assembly owner 时 fatal；每个后续 beat 必须与首 beat 的 header 一致；首 beat 写低半行、次 beat 写高半行并累积 corrupt；达到两拍后按 owner 调 Probe 或 Release completion。

### 13.3 complete_probe_c_assembly() / complete_release_c_assembly()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1069-1124。

抽象功能描述：在完整两拍 data 到达后执行主存和影子表副作用，并结束对应 owner；这两个 task 不
判断 DUT pass/fail，只维护 responder 的协议状态。

```systemverilog
if (!c_assembly_corrupt_seen) begin
    dcache_mem_access_task(c_assembly_line, 1'b1, 32'hffff_ffff,
                           c_assembly_data[255:0], corrupt, denied, load_data_unused);
    dcache_mem_access_task(c_assembly_line + 48'd32, 1'b1, 32'hffff_ffff,
                           c_assembly_data[511:256], corrupt, denied, load_data_unused);
end
remove_cached_line(c_assembly_line, "probe_toN");
waiting_probe_c = 1'b0;
clear_c_assembly_state();
```

中文伪代码：无 corrupt 时用两次全 mask store 更新主存；有 corrupt 时跳过数据写回但仍结束协议；Probe completion 删除 map 并清 waiting Probe；Release completion 在同样处理后创建一拍 ReleaseAck pending D。

## 14. body() 主循环和 global stop

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1311-1584，函数：dcache_mem__access_base_sequence::body()。

抽象功能描述：统一驱动 reset、fire 事实采集、owner 状态转换、下一拍 response item 和 terminal
drain；它是本专项唯一的 DCache responder 调度入口。

修改前逻辑：看到 A.valid 后直接发送 ready，再用阻塞 for-loop 发送全部 D beat；没有服务 B/C/E、
延迟、Hint、Probe 或 map 生命周期，也可能在 global stop 看到 A 无 valid 时过早退出。

修改后关键源码片段：

```systemverilog
@(dcache_vif.drv_cb);
sampled_a_valid = (dcache_vif.drv_cb.auto_inner_dcache_client_out_a_valid === 1'b1);
sampled_c_valid = (dcache_vif.drv_cb.auto_inner_dcache_client_out_c_valid === 1'b1);
a_fire = 1'b0;
b_fire = 1'b0;
c_fire = 1'b0;
d_fire = 1'b0;
e_fire = 1'b0;
build_dcache_idle_xaction(cycle_xact);
```

中文伪代码：每轮先在 drv_cb 边界采样 DUT 输入；先清本轮 fire 标志；构造所有字段为零的基线 item；随后才进入 reset、旧 item fire 和下一 owner 仲裁。

```systemverilog
if (last_cycle_valid && (last_cycle_xact != null)) begin
    a_fire = (last_cycle_xact.auto_inner_dcache_client_out_a_ready == 1'b1) && sampled_a_valid;
    b_fire = (last_cycle_xact.auto_inner_dcache_client_out_b_valid == 1'b1) && sampled_b_ready;
    c_fire = (last_cycle_xact.auto_inner_dcache_client_out_c_ready == 1'b1) && sampled_c_valid;
    d_fire = (last_cycle_xact.auto_inner_dcache_client_out_d_valid == 1'b1) && sampled_d_ready;
    e_fire = (last_cycle_xact.auto_inner_dcache_client_out_e_ready == 1'b1) && sampled_e_valid;
    if (d_fire) process_d_fire();
    if (e_fire) process_e_fire();
end
```

中文伪代码：用上一 item 的输出和当前采样值计算 A/B/C/D/E fire；先推进 D，再消费 E；任何状态改变都以 fire 为触发，不以 valid 单独作为完成条件。

```systemverilog
if (c_fire) begin
    if (!c_accept_armed || (armed_c_req_xact == null))
        `uvm_fatal(get_type_name(), "C.fire observed without an armed C snapshot");
    capture_dcache_c_xaction(fired_c_req_xact);
    check_c_payload_stable(armed_c_req_xact, fired_c_req_xact);
    if (c_assembly_owner == DCACHE_C_OWNER_NONE)
        start_c_assembly(armed_c_req_xact, last_drive_cycle);
    else
        consume_c_beat(armed_c_req_xact, last_drive_cycle);
    c_accept_armed = 1'b0;
    armed_c_req_xact = null;
end
```

中文伪代码：C.fire 必须有对应 armed snapshot；校验实际 payload 稳定；无 assembly 时启动 owner，有 assembly 时收下一 beat；清 armed 状态；由于本轮 c_fire=1，后续所有 C arm 分支都跳过。

```systemverilog
else if (!c_fire && (c_assembly_owner != DCACHE_C_OWNER_NONE)) begin
    if (sampled_c_valid) begin
        capture_dcache_c_xaction(sampled_req_xact);
        cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
        c_accept_armed = 1'b1;
        armed_c_req_xact = sampled_req_xact;
    end
end
else if (!c_fire && waiting_probe_c) begin
    // 只接受 ProbeAck/Data 或 Release/Data，并 arm 下一拍 C.ready
end
else if (!c_fire && sampled_c_valid) begin
    // idle 只接受 Release/ReleaseData
end
```

中文伪代码：只有当前拍没有消费 C.fire 时，才允许 assembly、Probe 等待或 idle C 分支重新 arm；这样同一 C valid 不会被旧 owner和新 owner重复处理。

```systemverilog
else if (!a_fire && sampled_a_valid) begin
    capture_dcache_a_xaction(sampled_req_xact);
    cycle_xact.auto_inner_dcache_client_out_a_ready = 1'b1;
    a_accept_armed = 1'b1;
    armed_a_req_xact = sampled_req_xact;
end
```

中文伪代码：A.fire 已处理时不再重新 arm；没有 A.fire 且看到合法 A.valid 时保存快照并打开下一拍 ready；真正 A.fire 后才调用 accept_dcache_a_request()。

### 14.1 e_ready owner 分支

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1474-1495。

抽象功能描述：按当前唯一 owner 选择下一 item；GrantAck 等待期间只服务 E，避免 A/C/B 与 E
生命周期交叉。

```systemverilog
else if (waiting_grant_ack) begin
    cycle_xact.auto_inner_dcache_client_out_e_ready = 1'b1;
end
else if (pending_probe_b_valid) begin
    cycle_xact.auto_inner_dcache_client_out_b_valid = 1'b1;
end
```

中文伪代码：pending D 优先保持 D；没有 pending D 且等待 GrantAck 时只开 e_ready；没有 E owner 才允许 B/C/A 或 Probe 调度；idle 基线本身不开放 E。

### 14.2 global stop drain

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1427-1469。

抽象功能描述：在 global stop 后等待 DCache responder 自身所有 in-flight 状态收敛，再发最后 safe
idle 并退出；已完成的 cached line map 不阻塞退出。

```systemverilog
if (data.is_global_stop_requested() &&
    !pending_d_valid && !waiting_grant_ack &&
    !pending_probe_b_valid && !waiting_probe_c &&
    (c_assembly_owner == DCACHE_C_OWNER_NONE) &&
    !a_accept_armed && !c_accept_armed &&
    !sampled_a_valid && !sampled_c_valid) begin
    send_dcache_xaction(cycle_xact);
    break;
end
```

中文伪代码：stop 请求不是立即退出；若当前 A.valid 未由上一 item 的 A.ready 形成 fire，直接
fatal；stop 后不再创建 Probe；只有 pending D、GrantAck、Probe、C assembly、A/C armed 和当前 A/C
valid 全部为空时发送 terminal safe idle、置 done 标志并退出，否则继续循环并周期性 warning。

## 15. 与 Plan 的对齐检查

### 15.1 已按原 Plan 实现

1. A response 按 AcquireBlock/AcquirePerm/CBO/Release 分层，不再把未知 coherent opcode fallback 为 AccessAckData。
2. 三档 delay、Hint 单拍排期、64B line map、Probe B/C、ReleaseData assembly、GrantAck/E 生命周期均已落地。
3. interface/xaction/driver sideband known-zero 和 io_l2_flush_done=0 合同已落地。
4. 主循环使用 sequence-local last_cycle_xact 与 VIF clocking sample，不依赖当前仍关闭的 DCache monitor analysis port。
5. global stop 需要 responder 自身 in-flight drain 后才退出。
6. 无 GrantAck owner 的 E.valid、未知 E sink 和 sideband 四态非法值均在对应消费/发送边界 fail-fast。

### 15.2 对齐结论

原计划定义的 coherent response 分类、Hint/Probe owner、C assembly、参数链和 global-stop 主路径
均已实现。下面两个章节把执行前原始计划与 coding 期间补充的行为分开记录；plan 第 19 章的
`IMPLEMENTATION_DELTA` 只记录 coding 期间发现的调整，不能倒推为执行前计划已经覆盖。

## 16. 实现与 Plan 不一致项

### 16.1 专项 cfg 的 Hint/Probe 默认值

计划原有逻辑：早期执行描述建议专项 cfg 用非零 Hint/Probe 权重作为压力配置。

当前源码逻辑：`tc_dispatch_real_l2cache_model.cfg` 将两个权重设为 0，定向运行时通过显式
plusarg 单独打开。

不一致原因：用户明确要求默认先关闭，避免基础 real-smoke 在没有观察意图时主动产生 B/C 或 sideband
行为；公共 responder 能力仍保留，定向场景不受影响。

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg`。

```text
+MEMBLOCK_L2_HINT_VALID_WT=0
+MEMBLOCK_L2_PROBE_ENABLE_WT=0
```

中文伪代码：专项 cfg 启动时把 Hint 和 Probe 两个百分比权重初始化为零，因此 responder 默认不主动
生成这两类行为；命令行追加其中一个权重为 100 时，公共参数链读取覆盖值，才分别打开对应定向路径。

处理结论：保持当前实现；该差异已写入 plan 的 `IMPLEMENTATION_DELTA`，验证使用独立 plusarg 覆盖。

### 16.2 `e_ready` owner 收紧

计划原有逻辑：早期文字允许 idle/active 路径默认开放 E.ready，只在 E.fire 时检查 sink。

当前源码逻辑：idle item 默认 `e_ready=0`，只有 `waiting_grant_ack` owner 已建立时才开放 E.ready。

不一致原因：若在最后一拍 D.fire 前就开放 E，DUT 可能先发 E.valid，sequence 尚未保存 sink/line
owner，GrantAck 会被丢失。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`build_dcache_idle_xaction()` 与 `body()`。

```systemverilog
rsp_xact.auto_inner_dcache_client_out_e_ready = 1'b0;
if (waiting_grant_ack) begin
    cycle_xact.auto_inner_dcache_client_out_e_ready = 1'b1;
end
```

中文伪代码：先构造所有 item 的 E.ready 为零；只有上一拍最后一个 Grant/GrantData 已经建立
GrantAck owner 时，当前拍才把 E.ready 置一；E.fire 后校验 sink、写入 cached line table 并清除 owner。

处理结论：保持当前实现；这是协议时序修正，已写入 plan 的 `IMPLEMENTATION_DELTA`，不是静默偏离。

## 17. Plan 未说明但 Coding 落实的细节

### 17.1 DCache driver 的锁步 item 交付

细节功能：sequence 每拍生成一个 item 后，driver 阻塞取得该 item 并立即写入 clocking output，避免
旧 driver 在无新 item 时重复保持上一 D.valid。

为什么 plan 未覆盖：原计划定义了 sequence 的 `last_cycle_xact` 采样合同，但没有展开 agent driver
内部的 item 交付实现。

在本特性中的作用：保证 sequence 下一次 `drv_cb` 采样到的就是上一 item 的对端握手，避免同一 D beat
被重复计数。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv`，
`main_phase()`。

```systemverilog
req = null;
seq_item_port.get_next_item(req);
if (req == null) begin
    `uvm_fatal(get_type_name(), "get_next_item returned a null DCache item")
end
this.send_pkt(req);
seq_item_port.item_done();
```

中文伪代码：先清空上一轮 item 句柄，再阻塞等待一个新 item；如果 sequencer 返回空句柄立即 fatal；
否则立即把 item 写到 VIF，随后通知 sequencer 已消费。driver 不插入额外 clocking hold，也不复用旧 item。

是否需要回写 plan：已在 plan 的 `IMPLEMENTATION_DELTA` 和主循环章节补充，当前不再另建 plan。

### 17.2 同拍 fire 防重复 arm

细节功能：A/C fire 已在本轮被消费后，禁止同一轮的 sampled valid 再次进入新 snapshot。

为什么 plan 未覆盖：原计划说明了 fire 后推进 owner，但没有把“状态更新后再次经过仲裁分支”的同拍
重入边界展开到源码级。

在本特性中的作用：ProbeAck 完成或 A request 被接受时，不会在同一个 valid 周期再次建立第二个 owner。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，`body()`。

```systemverilog
else if (!c_fire && (c_assembly_owner != DCACHE_C_OWNER_NONE)) begin
    if (sampled_c_valid) begin
        cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
        c_accept_armed = 1'b1;
        armed_c_req_xact = sampled_req_xact;
    end
end
else if (!c_fire && waiting_probe_c) begin
    if (sampled_c_valid) begin
        cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
        c_accept_armed = 1'b1;
        armed_c_req_xact = sampled_req_xact;
    end
end
else if (c_fire) begin
    // 本拍 C.fire 后跳过 A/Probe 仲裁。
end
else if (!a_fire && !data.is_global_stop_requested() && sampled_a_valid) begin
    cycle_xact.auto_inner_dcache_client_out_a_ready = 1'b1;
    a_accept_armed = 1'b1;
    armed_a_req_xact = sampled_req_xact;
end
else begin
    try_start_probe(!data.is_global_stop_requested());
end
```

中文伪代码：每轮先根据上一 item 和当前 sampled valid/ready 计算 fire；若 C.fire 为真，完成当前
C 事件后跳过所有 C arm 分支；若 A.fire 为真，完成 A 接受后跳过 A arm 分支；只有未发生对应 fire
时，当前 valid 才能建立下一拍 snapshot。

是否需要回写 plan：已回写到 plan 的 `IMPLEMENTATION_DELTA`，不改变主体协议模型。

### 17.3 PADDR range 的实际注册

细节功能：在 DCache responder 启动时把公共 PADDR base/range 注册到共享内存 helper，使完整 64B
line 检查不因 helper 未配置范围而退化为“任意地址都合法”。

为什么 plan 未覆盖：原计划要求 `check_line_range()` 做边界检查，但没有明确该 helper 的共享
range 初始化时机。

在本特性中的作用：DCache 使用物理地址访问主存时，line 和 line+32 的每个有效字节都必须落在
V2 配置的物理窗口内；不改变主表虚拟地址生成。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，`body()` 初始化段。

```systemverilog
clear_main_mem_ranges();
init_main_mem_range(mem_addr_t'(seq_csr_common::get_paddr_base()),
                    seq_csr_common::get_paddr_range());
check_l2_model_cfg();
```

中文伪代码：先清掉本 sequence 旧的 range；读取公共 PADDR base 和容量并注册为主存合法窗口；后续
`check_line_range()` 逐字节检查完整 64B line，越界立即 fatal。

是否需要回写 plan：已记录在 plan 的 `IMPLEMENTATION_DELTA`，且已同步到 memory responder analysis/flow。

### 17.4 独立专项 cfg 的基础开关

细节功能：专项 cfg 显式包含 real-smoke 的主表、LSQ、issue、commit、L2TLB 和 TLB 合法性开关，
不依赖 Makefile 自动继承另一个 cfg。

为什么 plan 未覆盖：原计划只描述新增 L2 参数，未展开 Makefile 一次只加载一个 cfg 的行为。

在本特性中的作用：直接以 `cfg=tc_dispatch_real_l2cache_model` 运行时仍能形成完整 DUT dispatch 到
DCache 的 flow。

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg`。

```text
+MEMBLOCK_MAIN_TRANS_NUM=1
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
+MEMBLOCK_LSQCOMMIT_SEQ_EN=1
+MEMBLOCK_L2TLB_SEQ_EN=1
```

中文伪代码：Makefile 选择专项 cfg 后，cfg 自己提供真实主流程所需的开关，再由末尾 L2 参数决定
response delay、Hint 和 Probe 行为；因此不需要隐式合并其它 cfg 文件。

是否需要回写 plan：已记录在 plan 的 `IMPLEMENTATION_DELTA`，并同步到 flow/review 的运行命令。

### 17.5 删除 DCache responder 的无效 gap/delay 残留

细节功能：删除 DCache 类中已经不再读取的 `default_pre_pkt_gap`、`default_post_pkt_gap` 和
`sampled_delay` 局部变量；SBuffer 类的同名 gap 状态保持不变。

为什么 plan 未覆盖：原计划只要求 DCache item 的 gap 固定为 0，没有逐项要求清理旧字段声明。

在本特性中的作用：避免维护者误以为 DCache 仍有第二套 gap 或 delay 状态，保持唯一的
`service_cycle/pending_d_due_cycle` 时间来源。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，DCache class 和
`accept_dcache_a_request()`。

```systemverilog
rsp_xact.pre_pkt_gap  = 0;
rsp_xact.post_pkt_gap = 0;
pending_d_due_cycle = accept_cycle + sample_l2_response_delay();
```

中文伪代码：每个 DCache responder item 仍明确把 gap 置零；response 到期时间只由接受拍加一次
`sample_l2_response_delay()` 得到；不再保留未使用的旧 gap/delay 局部状态。SBuffer 的 legacy
single-request flow 继续使用自己的 gap 字段。

是否需要回写 plan：已同步到 plan 的主循环合同；删除属于实现清理，不新增功能。

### 17.6 通用 `drive_idle` 与 real-smoke owner 的边界

细节功能：generic `DRV_1/DRV_RAND` 仍可保留其它 TileLink channel 的既有驱动行为，但 E.ready 和
四个 L2 sideband 在所有 generic idle 模式都强制为安全 0；real-smoke responder 使用 owner-managed
item 时才可按状态打开 E.ready。

为什么 plan 未覆盖：专项原计划只覆盖 real-smoke responder 的 item 路径，未明确 driver 在其它
`drv_mode` 下的历史兼容行为。

在本特性中的作用：避免把通用 driver 的 `e_ready` 全 1 误判为 GrantAck owner；real-smoke 实际
通过 `sqr_sw=ON` 进入 `send_pkt()`，其 E.ready 只由 `waiting_grant_ack` 打开。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent_driver.sv`，`drive_idle()` 与
`send_pkt()`。

```systemverilog
else if (drv_mode==tcnt_dec_base::DRV_1) begin
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= '1;
end
vif.drv_mp.drv_cb.io_l2_hint_valid <= '0;
vif.drv_mp.drv_cb.io_l2_hint_bits_sourceId <= '0;
vif.drv_mp.drv_cb.io_l2_hint_bits_isKeyword <= '0;
vif.drv_mp.drv_cb.io_l2_flush_done <= '0;
vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= '0;
```

中文伪代码：generic 分支只保留其它 channel 的既有行为，不再给 E.ready 赋 1/X/随机值；函数末尾
无条件写 E.ready=0，四个 sideband 同样清零。
real-smoke 不依赖 generic idle，而是由 sequence item 携带 GrantAck owner 计算出的 E.ready，driver
不会把 generic item 误当作 owner。

是否需要回写 plan：已回写为 plan 的 `IMPLEMENTATION_DELTA` 边界；后续要用通用随机 channel 压测
必须另建专项，不在本 plan 扩大 driver 语义。

### 17.7 最终 review 的四态与无主 E.valid 收紧

细节功能：driver 在第一条 VIF 赋值前用 case equality/inequality 检查 sideband 的已知值合同；
sequence 在处理 D/E fire 后拒绝没有 GrantAck owner 的 E.valid，并用四态完全匹配检查 E sink。

为什么 plan 未覆盖：原 plan 定义了“非零/无 owner 要 fatal”的功能结果，但没有写清普通二态比较对
X/Z 的边界，也没有给出“先允许 D.fire 建 owner，再判断无主 E.valid”的精确顺序。

在本特性中的作用：避免未知 sideband 被送入 DUT，也避免 DUT 无 owner 地保持 E.valid 后被 responder
静默忽略；同时不误伤最后一拍 D.fire 刚刚建立 owner 的合法边界。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv` 的
`check_l2_sideband_item()`、`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` 的
`body()` 和 `process_e_fire()`。

```systemverilog
sampled_e_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_valid;
sampled_a_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_valid;
sampled_b_ready_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_b_ready;
sampled_c_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_valid;
sampled_d_ready_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_d_ready;
if (!reset_active &&
    ((sampled_a_valid_raw !== 1'b0 && sampled_a_valid_raw !== 1'b1) ||
     (sampled_b_ready_raw !== 1'b0 && sampled_b_ready_raw !== 1'b1) ||
     (sampled_c_valid_raw !== 1'b0 && sampled_c_valid_raw !== 1'b1) ||
     (sampled_d_ready_raw !== 1'b0 && sampled_d_ready_raw !== 1'b1) ||
     (sampled_e_valid_raw !== 1'b0 && sampled_e_valid_raw !== 1'b1))) begin
    `uvm_fatal(get_type_name(), "DCache E.valid sampled as X/Z outside reset")
end
sampled_e_valid = (sampled_e_valid_raw === 1'b1);
if (sampled_e_valid && !e_fire && !waiting_grant_ack) begin
    `uvm_fatal(get_type_name(), "E.valid observed without a pending GrantAck owner")
end
```

中文伪代码：先处理当拍 D.fire，使最后一个 Grant beat 可以建立 owner；合法 E.fire 已消费时不重复
检查；其余 E.valid 若仍没有 owner则 fatal。E sink 和四个 sideband字段使用四态检查，未知值不当作
相等或安全 0。

是否需要回写 plan：已作为第 19.10 节回写，并同步专项 flow 和源码分析文档。

### 17.8 C assembly 与 Probe/A 仲裁 hazard 修复

细节功能：首个 `C.fire` 建立 `ReleaseData`/`ProbeAckData` assembly 后，本拍不再允许 A 或 Probe
抢占唯一生命周期；Probe helper 也不依赖调用点而自行检查所有 owner。

为什么需要修复：旧分支在 C.fire 后仍可能落到 A arm 或 `try_start_probe()`，随后 A.fire 会重置
`pending_d`，导致 C 第二拍无法消费或 ReleaseAck 状态被覆盖。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`dcache_mem__access_base_sequence::body()` 和 `try_start_probe()`。

```systemverilog
else if (c_fire) begin
    // 本拍 C.fire 已完成或推进 C owner；禁止同拍 arm A/Probe。
end
else if (!a_fire && !data.is_global_stop_requested() && sampled_a_valid) begin
    cycle_xact.auto_inner_dcache_client_out_a_ready = 1'b1;
    a_accept_armed = 1'b1;
    armed_a_req_xact = sampled_req_xact;
end

if (!allow_new_probe || pending_d_valid || waiting_grant_ack ||
    pending_probe_b_valid || waiting_probe_c ||
    (c_assembly_owner != DCACHE_C_OWNER_NONE) ||
    a_accept_armed || c_accept_armed) begin
    return;
end
```

中文伪代码：若本拍 C.fire，先完成当前 C owner，然后显式跳过 A/Probe 分支；只有没有 C.fire、没有
stop 且采到合法 A.valid 时才 arm A；Probe helper 再次检查 pending D、GrantAck、Probe、assembly
和 armed 状态，任一存在则返回。该修复保持 C owner 的唯一写者，不改变普通 A/C 协议分类。

### 17.9 四态 channel 采样与 sideband 类型修复

细节功能：让 driver 的四态 sideband检查和 sequence 的 E.valid检查真正可达，避免 X/Z 在 transaction
或 local `bit` 赋值时被静默转换。

源码位置：`mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv`、
`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` 的 `body()`。

```systemverilog
rand logic io_l2_hint_valid;
rand logic [3:0] io_l2_hint_bits_sourceId;
rand logic io_l2_hint_bits_isKeyword;
rand logic io_l2_flush_done;

sampled_e_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_valid;
if (!reset_active &&
    (sampled_e_valid_raw !== 1'b0 && sampled_e_valid_raw !== 1'b1)) begin
    `uvm_fatal(get_type_name(), "DCache E.valid sampled as X/Z outside reset")
end
sampled_e_valid = (sampled_e_valid_raw === 1'b1);
```

中文伪代码：sideband字段使用四态 `logic`，普通 randomize 仍由约束生成已知值；service loop先保留
A.valid、B.ready、C.valid、D.ready、E.valid raw值，非 reset 时任一不是已知 0/1 就 fatal，之后才
转换为二态 sampled值；compare和E sink校验使用 case inequality。这样未知值不能绕过driver或owner
检查。

### 17.10 global stop drain 与 legacy phase 生命周期

细节功能：stop 后不再创建 Probe；上一 item 已打开 A.ready 且当前形成 A.fire 的请求仍完成 drain，
stop 后新出现且未 fire 的 A.valid直接 fatal；legacy testcase在DCache terminal idle后才结束phase。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` 的 `body()`、
`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`、
`mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`。

```systemverilog
if (data.is_global_stop_requested() && sampled_a_valid && !a_fire) begin
    `uvm_fatal(get_type_name(),
               "new DCache A.valid observed after global stop without a sampled fire")
end

memblock_sync_pkg::dcache_responder_done = 1'b0;
// terminal safe idle 已发送且所有 in-flight 清空
memblock_sync_pkg::dcache_responder_done = 1'b1;

run_real_smoke_sequence();
wait(memblock_sync_pkg::dcache_responder_done === 1'b1);
memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
phase.drop_objection(this);
```

中文伪代码：global stop期间只消费已在DUT channel上的C或已形成fire的A；不允许新A或Probe进入模型；
完成pending/owner/assembly后发送terminal idle并置done。canonical vseq继续用`wait fork`等待后台
responder，legacy testcase等待done再清active/drop objection，避免phase提前杀掉DCache sequence。

### 17.11 同步文档与旧逻辑残留检查

本轮对以下实际受影响文档逐文件同步并复查：

| 文档 | 同步内容 |
|---|---|
| `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md` | 术语表、Mermaid调用图、C独占仲裁、四态采样、stop done和验证口径 |
| `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md` | 共享DCache/SBuffer owner、generic E.ready、stop和legacy done |
| `AI_DOC/mem_ut_flow_doc/virtual_sequence_unified_dispatch_flow.md` | canonical join与legacy done边界 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/mem_base_sequence.md` | 新的raw采样、Probe gate、stop和driver职责 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_sync_pkg.md` | `dcache_responder_done`状态语义 |
| `AI_DOC/analysis/interface/memblock验证输入的程序框架.md` | sideband四态与E.ready owner |
| `AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md` | 字段producer和generic约束 |
| `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md` | legacy testcase等待done的调用链 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 五个L2参数既有登记复查 |
| 本专项 plan | `IMPLEMENTATION_DELTA 19.11`至`19.15` |

使用 `rg` 复查后，当前有效文档不再把 generic `e_ready=1`、stop后新 Probe、二态 sideband 或
“0 warning”描述成当前实现；历史文档中的旧行为仅保留在其历史 review/plan 对齐章节。

## 18. 修改类型总结

### 18.1 字段/参数适配

- dcache_agent_agent_interface 新增并初始化 V2 L2 Hint/flush sideband。
- dcache_agent_agent_xaction 对四个 sideband 增加默认值和约束。
- dcache_agent_agent_driver::send_pkt() 透传并检查 sideband。
- plus.sv -> seq_csr_common -> cfg -> getter 增加五个 runtime 行为参数。

### 18.2 新增或改变的功能逻辑

- 旧 A-to-D 阻塞 for-loop 替换为逐拍 fire 驱动的 responder 状态机。
- 新增 coherent response 分类、delay、GrantAck owner、E sink 校验和 line map。
- 新增 Hint 单拍排期、Probe 选择/B/C 生命周期、ReleaseData 两拍 assembly 和主存写回。
- driver 从 try_next_item + idle hold 改为阻塞 get_next_item + immediate send，消除同一 beat 重复握手风险。
- e_ready 从早期默认开放改为 GrantAck owner 专属开放。
- generic driver idle 的 E.ready 统一安全为 0；只有 GrantAck owner item 可打开 E.ready。
- sideband xaction 和 channel sample 保留四态并 fail-fast；无 GrantAck owner 的 E.valid 和未知 E sink 不再静默通过。
- C.fire 后本拍独占 C owner，禁止 A/Probe 抢占；Probe helper 自带 owner/stop gate。
- global stop 从“无 A.valid 即退出”改为禁止新 Probe、拒绝未握手新 A、等待所有 responder in-flight
  状态归零，并发布 `dcache_responder_done` 给 legacy testcase。

### 18.3 主体逻辑未改变的部分

本专项没有修改主表生成、LSQ enqueue、issue、writeback、ROB commit/deq、redirect/replay 或
pass/fail owner；DCache responder 只消费 DUT coherent channel，并维护自己的 response 生命周期。
主存数据仍由 mem_access_base_sequence 持有，cached line table 不会成为第二个参考模型。

## 19. 验证结果

### 19.1 静态检查

已执行 git diff --check，目标源码和本专项文档无 whitespace error。并检查旧的
last_sent_item、generic sideband random 驱动、无条件 E.ready=1、stop 后 Probe 和二态 E.valid
采样描述，未发现有效残留（历史 plan delta 中的“原描述”除外）。源码仍保留
`dcache_agent_agent_driver::drive_idle()` 在显式 `DRV_1/DRV_RAND` 下的其它通用 channel 行为；
但函数末尾无条件把 E.ready 和四个 V2 L2 sideband 覆盖为 known-zero。

### 19.2 编译和 smoke

已执行：

```text
make eda_compile tc=tc_sanity mode=v2_l2cache_lockstep_20260723
make eda_batch_run seed=666666 tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=off
make eda_batch_run seed=666669 tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=off plus_arg=+MEMBLOCK_L2_HINT_VALID_WT=100
make eda_batch_run seed=666668 tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=off plus_arg=+MEMBLOCK_L2_PROBE_ENABLE_WT=100
make eda_batch_run seed=666670 tc=tc_dispatch_real_smoke ts=virtual_base_sequence mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=off
```

结果：编译 0 error；VCS 日志包含 `LCA_FEATURES_ENABLED` 工具 warning，canonical 运行日志有既有
UVM resource/default-sequence warning；这些不是本专项 error。普通 canonical real smoke、Hint 定向
smoke、Probe 定向 smoke 和 legacy testcase 均 `TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0`。
legacy run 额外验证 `tc_dispatch_real_smoke` 等待 `dcache_responder_done` 后才结束 phase。

Hint/Probe 定向运行的功能日志证据如下：

- Hint，seed 666669：A 在 245.2ns 完成握手；`hint_valid` 仅在 280.2ns 到 285.2ns 为 1；
  `d_valid` 在 295.2ns 到 305.2ns 连续覆盖两个 ready beat；D 完成后 E.ready 在 305.2ns
  打开，310.2ns 与 E.valid 完成 GrantAck。
- Probe，seed 666668：GrantData/E 完成后，B Probe 在 290.4ns 以 opcode 6、地址
  `0x08b64e280` 完成握手；C 在 1470.4ns 给出同地址 opcode 4 ProbeAck，1475.4ns
  responder 开放 C.ready，1480.4ns 后 C.valid 撤销；退出日志为 `cached_lines=0`。

因此波形已经核对 GrantData 两个 beat 各一次握手、E 只在 GrantAck owner 建立后开放、Hint
单拍有效，以及 map-backed Probe B/C 的一次完整闭环。

## 20. 非本次修改的逻辑分析

### 20.1 `git status --short` 对比结论

本次 review 主题覆盖的逻辑文件和同步文档为：

- `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv`
- `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg`
- 本文关联的 L2Cache plan、flow、interface/源码 analysis 和参数规则同步段落。

当前工作区还存在以下不属于本专项的修改，未纳入本 review 的功能正确性判断：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| 文档搬迁/整理 | `AI_DOC/plan/test_framework/review_doc/{undo,do}/*` 中其它 review | 非本次逻辑 | 属于已有 review 归档和目录整理，不改变 DCache responder。 |
| 项目规则修改 | `AGENTS.md`、`AI_DOC/project_management/*` | 另行 review | 属于项目执行规则、文档规则或参数管理规则的用户修改；本专项只按其规则执行。 |
| 其它文档同步 | 与 L2Cache 无关的 `AI_DOC/analysis`、flow、plan | 另行 review | 属于其它 V2 flow 的历史同步，不由本专项代码触发。 |
| 会话产物 | `.humanize/*` | 非源码逻辑 | 属于工具会话记录，不参与编译和运行时行为。 |

未发现本专项相关源码文件被遗漏；DCache agent、公共 memory responder、L2 参数入口及独立 cfg 均已
在前述章节逐项覆盖。

## 21. Review 结论与剩余边界

从测试框架主体控制逻辑角度，本专项改动是 DCache responder 的局部协议细节适配，字段/参数链路
和新增生命周期均有明确 owner、设置点、清除点和退出条件。当前明确不覆盖：完整 L2 directory、
多 outstanding、多 Probe、denied/corrupt 注入、io_l2_flush_done 功能、非 DCache client 的
AccessAckData，以及 RM/scoreboard/coverage。

独立 subagent 已对最新工作区执行限定范围只读 review，结论最后一行严格为 `FINAL PASS`；本 agent
随后复核了代码、文档、compile/smoke 日志，满足 plan 归档和独立 commit 条件。
