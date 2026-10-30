# Rollback同拍来源关联实现评审

日期：2026-10-30。状态：coding、fresh compile、软件专项及20/20真实DUT通过（原1705742557与1992896882各完整10000笔及18邻接）；Turing最终独立`FINAL_IMPLEMENTATION_REVIEW_PASS`，方案移do，随本记录单Bug提交，提交后才恢复批量。
母工作区：`.codex_mother_integration_20261028`，V2；不修改RTL、RM、cfg或实际请求生成。

## 1. 术语与抽象功能说明

| 术语 | 当前中文含义 | 对象/落点 | 示例 |
|---|---|---|---|
| sample | 同一clocking边界前1ps的采样身份 | `sample_seq` | 144892与144894不是同一个事件 |
| candidate/winner | 六路回退候选及组合获选者 | `rollback_candidate_* / rollback_winner` | nuke0到nuke1先后发同ROB事件 |
| provenance/payload | 某拍的完整来源证明/内容 | `dispatch_raw_ctrl_t` | 保留获选、活动候选和原因 |
| cache | 框架有限来源缓存，不是DCache | `rollback_provenance_cache` | 4拍保存上界不等于允许跨拍匹配 |
| same-sample | 输入来源与输出编号必须完全相同 | resolver筛选条件 | 输出102只查102 |
| conflict | 同拍同ROB/level的重复证明互相矛盾 | `conflicting` | 非winner内容在同拍不一致仍拒绝 |
| snapshot | 按值复制的当前拍来源记录 | `entry.snapshot` | 不暴露可写owner |
| reset | 既有DUT复位清理协议 | `clear_rollback_provenance_cache()` | 清后本拍也不得借旧证明 |
| fixture/vseq | 仅验证该task的外加软件入口 | ignored独立vseq文件 | basicTest启动，无真实请求 |
| fallback | 缺本拍证据时借历史记录兜底 | 本次明确禁止 | 100记录不能服务102输出 |

抽象职责：生产修改只决定已有monitor事件应绑定哪一拍的来源，不决定RTL仲裁，也不计算异常预期。
独立责任review已确认根因；本review覆盖本Bug一个生产逻辑文件、一个既有软件定向文件及外加验证入口，不能代替最终动态验收。

## 2. 修改前后、调用与状态

修改前：monitor每拍按mon_cb生成来源记录，但resolver合并age0–4全部同ROB/level记录，要求其完整payload一致。
原seed1705742557的winner4→5跨拍合法变化因此报conflict；本拍完整来源实际存在。
修改后：sample成为精确身份，只在同sample、完整ROB/level范围比较重复记录；历史记录不参与当前冲突判断。
缺失/未来/错身份仍拒绝，同拍不同payload仍拒绝，不使用latest或旧snapshot。

| 顺序 | 现有函数 | 本流程职责与副作用 |
|---|---|---|
| 1 | monitor `sample_rollback_diagnostics()` | 同mon_cb读取六路候选/组合winner，检查selected及有效性/XZ，生成值型来源；本次不改 |
| 2 | `cache_rollback_input_provenance()` | 保存该拍完整记录，沿用原4拍过期清除；本次不改 |
| 3 | `resolve_rollback_output_provenance()` | 唯一生产修改点：精确同拍筛选、同拍重复冲突检查和按值输出 |
| 4 | monitor与`dispatch_monitor_event_adapter` | 复制已证明来源并保持原selected/原因解码；本次不改 |
| 5 | 原handler | 按现有UID/redirect语义消费事件；本次不改 |

无新增/删除字段、owner、queue、runtime参数、cfg或filelist。缓存所有权及reset清理不变。
高频路径沿用有限缓存扫描，不扫描main全表，不增加等待或周期状态。

## 3. 生产实现源码与正确性

抽象功能描述：`resolve_rollback_output_provenance()`由monitor在有效回退输出处调用，返回当前采样身份唯一一致的来源。
该函数不构造redirect，不改变DUT或提交游标；失败只返回清零输出与明确conflict标记。
源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`。以下是完整函数体控制流；接口参数不变。

```systemverilog
int selected_idx;

snapshot = make_empty_raw_ctrl();
input_sample_seq = 0;
conflicting = 1'b0;
selected_idx = -1;
while (rollback_provenance_cache.size() != 0 &&
       output_sample_seq > rollback_provenance_cache[0].sample_seq +
                           MEMBLOCK_ROLLBACK_PROVENANCE_MAX_AGE) begin
    rollback_provenance_cache.pop_front();
end
foreach (rollback_provenance_cache[idx]) begin
    // 中文注释：V2顶层仲裁为同拍组合输出；历史保存上界不是输出延迟容差。
    // 只比较本拍同身份的重复证明，不能把不同拍的合法winner变化归为冲突。
    if (!rollback_provenance_cache[idx].valid ||
        rollback_provenance_cache[idx].sample_seq != output_sample_seq ||
        rollback_provenance_cache[idx].snapshot.memory_violation_rob_flag != rob_flag ||
        rollback_provenance_cache[idx].snapshot.memory_violation_rob_value != rob_value ||
        rollback_provenance_cache[idx].snapshot.memory_violation_level != level) begin
        continue;
    end
    if (selected_idx >= 0 &&
        !rollback_provenance_payload_equal(
            rollback_provenance_cache[selected_idx].snapshot,
            rollback_provenance_cache[idx].snapshot)) begin
        conflicting = 1'b1;
        return 1'b0;
    end
    selected_idx = idx;
end
if (selected_idx < 0) begin
    return 1'b0;
end
snapshot = rollback_provenance_cache[selected_idx].snapshot;
input_sample_seq = rollback_provenance_cache[selected_idx].sample_seq;
return 1'b1;
```
中文伪代码：本逻辑负责把输出绑定到本拍输入。先用`make_empty_raw_ctrl()`建立全部字段的失败默认值，sample清0、conflict清0。
随后从队首删除超出保存边界的记录；这只处理既有缓存生命周期，不授权输出延迟。
遍历有限缓存，任何无效、不同sample或完整ROB/level不一致的记录都跳过，不能影响当前候选和冲突判断。
第一份匹配记录成为候选；之后调用原`rollback_provenance_payload_equal()`比较完整诊断，
同拍内容不同则置conflict并返回失败，snapshot/sample仍为默认值。相同重复证明可保留最后同拍索引。
无候选则失败；有候选只复制值型snapshot和该拍sample后成功，不消费当前记录、不驱动RTL、不改UID状态。

原比较函数仍覆盖候选known/valid/ROB/level、winnerknown/bitmask、LU原因、uncache状态及RAW附加诊断；不删字段。
修改只是将筛选的`sample_seq > output_sample_seq`改为`!=`，禁止历史记录参与同拍证明。
真实同拍冲突不会被吞掉；本拍缺失不会因旧key相同而误通过。V2组合Mux1H和统一1step采样是其适用依据，不声称支持未来任意pipeline。

## 4. 必要软件验证实现

抽象功能描述：`soft_test_memblock_pending_mmio_directed_sequence::run_rollback_cross_sample_cache_scenario()`直接调用生产API验证身份/冲突合同。
它不是实际DUT激励，不启动该软件sequence的body，也不修复其他软件场景。
源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`。

### 4.1 合法本拍、缺失拒绝与原winner4→5复刻

抽象功能描述：先建立完整合法证明，再验证旧拍不能服务新输出，最后在新拍切换来源而不改变输出身份。
初始结构由`make_empty_raw_ctrl()`清零，填写有效/完整、ROB1/126、level1、candidate known全1、winner known0x37、
candidate/winner4和RAW known全1，其他字段保持0；这与原失败的有效获选语义一致。

```systemverilog
memblock_sync_pkg::cache_rollback_input_provenance(input_snapshot, 100);
if (!memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 100,
        resolved_snapshot, input_sample_seq, conflicting) ||
    conflicting || input_sample_seq != 100 ||
    !memblock_sync_pkg::rollback_provenance_payload_equal(resolved_snapshot, input_snapshot)) begin
    `uvm_fatal(get_type_name(), "same-sample rollback provenance did not copy the complete snapshot")
end
if (memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 102,
        resolved_snapshot, input_sample_seq, conflicting) ||
    conflicting || input_sample_seq != 0 || resolved_snapshot.memory_violation_valid ||
    resolved_snapshot.rollback_diag_valid || resolved_snapshot.rollback_winner != '0 ||
    resolved_snapshot.rollback_candidate_valid != '0) begin
    `uvm_fatal(get_type_name(), "missing current sample borrowed historical rollback provenance")
end
repeated_snapshot = input_snapshot;
repeated_snapshot.rollback_candidate_valid = 6'b10_0000;
repeated_snapshot.rollback_candidate_rob_flag = 6'b10_0000;
repeated_snapshot.rollback_candidate_rob_value = '0;
repeated_snapshot.rollback_candidate_rob_value[5] = 8'h7e;
repeated_snapshot.rollback_candidate_level = 6'b10_0000;
repeated_snapshot.rollback_winner = 6'b10_0000;
memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 102);
if (!memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 102,
        resolved_snapshot, input_sample_seq, conflicting) ||
    conflicting || input_sample_seq != 102 ||
    !memblock_sync_pkg::rollback_provenance_payload_equal(resolved_snapshot, repeated_snapshot)) begin
    `uvm_fatal(get_type_name(), "cross-sample winner4 to winner5 change falsely conflicted")
end
```
中文伪代码：该段先用生产cache方法保存100，再要求resolver输出100成功、无冲突且完整内容一致。
同key输出102但尚无102记录时必须失败，sample及有效/winner/candidate字段保持默认，不允许100兜底。
随后按值复制结构，把活动candidate/ROB/level/winner全部从4切换为5，仍保持同一架构ROB/level，保存到102。
resolver必须只返回102的新内容，既不能报跨拍冲突，也不能选择旧100。完整payload比较用于确认返回值，不修改模型或DUT。

### 4.2 完整身份、同拍重复与冲突

抽象功能描述：分别扰动ROB flag/value/level，再区分跨拍非winner变化与同拍矛盾记录，保证修复没有放宽基础身份。
源码位置同上，以下为全部关键检查分支。

```systemverilog
for (int idx = 0; idx < 3; idx++) begin
    if (memblock_sync_pkg::resolve_rollback_output_provenance(
            idx == 0 ? 1'b0 : 1'b1,
            idx == 1 ? 8'h7f : 8'h7e,
            idx == 2 ? 1'b0 : 1'b1, 102,
            resolved_snapshot, input_sample_seq, conflicting) ||
        conflicting || input_sample_seq != 0 || resolved_snapshot.memory_violation_valid ||
        resolved_snapshot.rollback_diag_valid || resolved_snapshot.rollback_winner != '0) begin
        `uvm_fatal(get_type_name(), "rollback provenance ignored current ROB/level identity")
    end
end
repeated_snapshot.rollback_candidate_known[1] = 1'b1;
repeated_snapshot.rollback_candidate_valid[1] = 1'b1;
repeated_snapshot.rollback_candidate_rob_flag[1] = 1'b1;
repeated_snapshot.rollback_candidate_rob_value[1] = 8'h7f;
repeated_snapshot.rollback_candidate_level[1] = 1'b1;
memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 104);
memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 104);
if (!memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 104,
        resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 104 ||
    !memblock_sync_pkg::rollback_provenance_payload_equal(resolved_snapshot, repeated_snapshot)) begin
    `uvm_fatal(get_type_name(), "same-sample identical proof or cross-sample nonwinner change failed")
end
repeated_snapshot.rollback_candidate_rob_value[1] = 8'h80;
memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 104);
if (memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 104,
        resolved_snapshot, input_sample_seq, conflicting) ||
    !conflicting || input_sample_seq != 0 || resolved_snapshot.memory_violation_valid ||
    resolved_snapshot.rollback_diag_valid || resolved_snapshot.rollback_winner != '0) begin
    `uvm_fatal(get_type_name(), "same-sample rollback proof conflict was not rejected atomically")
end
```
中文伪代码：先在102本拍证明存在的条件下循环三次，每次只改一个完整身份分量；resolver都必须失败、无伪冲突、无输出泄漏。
接着在104增加未获选候选1，保存两份相同的完整证明，要求返回104且整份内容一致；旧102的不同非winner内容不应污染104。
最后仅把同拍第三份证明中候选1的ROB改为0x80，同拍同输出key但完整诊断矛盾必须conflict失败，不能留下先前成功输出。
所有公共方法仍是生产cache/resolver/payload_equal，不创建软件专用公共helper，也没有handler或队列驱动副作用。

### 4.3 未来、reset和过期边界

抽象功能描述：验证时间方向和既有清理生命周期，测试结束不留下来源缓存。
源码位置同上，完整剩余控制分支如下。

```systemverilog
memblock_sync_pkg::clear_rollback_provenance_cache();
memblock_sync_pkg::cache_rollback_input_provenance(input_snapshot, 108);
if (memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 107,
        resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 0 ||
    resolved_snapshot.memory_violation_valid || resolved_snapshot.rollback_diag_valid) begin
    `uvm_fatal(get_type_name(), "future rollback proof was used for an earlier output")
end
memblock_sync_pkg::clear_rollback_provenance_cache();
if (memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 108,
        resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 0 ||
    resolved_snapshot.memory_violation_valid || resolved_snapshot.rollback_diag_valid) begin
    `uvm_fatal(get_type_name(), "reset-cleared rollback provenance remained visible")
end
memblock_sync_pkg::cache_rollback_input_provenance(input_snapshot, 100);
if (memblock_sync_pkg::resolve_rollback_output_provenance(
        1'b1, 8'h7e, 1'b1, 105,
        resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 0 ||
    memblock_sync_pkg::rollback_provenance_cache.size() != 0) begin
    `uvm_fatal(get_type_name(), "expired rollback provenance remained visible")
end
memblock_sync_pkg::clear_rollback_provenance_cache();
`uvm_info("ROLLBACK_SAME_SAMPLE_DIRECTED",
          "winner4 to winner5 isolation, current identity, strict same-sample conflicts and reset checks PASS", UVM_LOW)
```
中文伪代码：先清缓存再只保存108，要求107不能读取未来证明。再调用既有reset清除方法，要求原108即使同拍也不能命中。
另一独立清理后的记录100在输出105时必须失败并实际从队列过期删除，不能仅因精确sample不匹配掩盖清理失效。
最后清空缓存并输出唯一完成marker；任何前面的断言失败都会fatal，不能靠空跑或仅marker计作通过。
这些是软件值型注入，不声称完成真实DUT reset/transport端到端验证。

## 5. 外加验证入口与输入边界

抽象功能描述：`rollback_same_sample_gate_vseq::body()`只在basicTest既有生命周期内执行上述一个task，不启动整个软件sequence场景。
源码位置：ignored证据目录`mem_ut/ver/ut/memblock/sim/mother_rollback_same_sample_gate_fixture_20261030/rollback_same_sample_gate_vseq.sv`。

```systemverilog
virtual task body();
    soft_test_memblock_pending_mmio_directed_sequence directed;
    directed = soft_test_memblock_pending_mmio_directed_sequence::type_id::create("rollback_same_sample_only");
    directed.run_rollback_cross_sample_cache_scenario();
endtask
```
中文伪代码：本入口创建既有软件sequence对象，但不启动其body；仅直接调用公开的目标task，等全部断言完成后同步返回。
basicTest仍按factory创建并启动vseq、持有objection；该未知非dispatch vseq沿用NO_DISPATCH拓扑，没有L2TLB owner或真实请求。
通过既有TB_LIST向独立mode追加文件，不改生产filelist或增加独立testcase；compile首行已确认文件实际参与。

原2540核心编译输入相对1343仅两个SV变化（一个生产逻辑文件、一个既有软件定向文件）；protocol mode另加fixture1，共2541项。
两处`actual_compile_inputs.json`保存逐路径hash及变化列表，不能用旧HEAD标签冒充已提交源码。
fixture SHA256=`4ce5a9660885f11cc84433e3393c5953d5713e25106646e663e4ee7e4c843967`；
主coverage mode simv SHA256=`6c2d50bf890df3fb5d5cc23b3f368d55e446e3e1593200d93863b3361c687abb`，
protocol mode simv SHA256=`ed1c0bc0c2c39ec1a8ee3c792345564cd26ad6579e2ba13b9ca1fcc2d60f14d0`。
软件结果的VSEQ_BODY开始/结束与唯一marker均在243.150ns实际执行，TEST_PASS/finish完整，所有六项错误计数0；
`software_acceptance.json`已落盘，不计DUT严格通过。
18个DUT邻接复用protocol mode：仅附带未执行的fixture类，2540核心输入逐项与主mode相同；不作为最终coverage来源。
两个长根seed使用不含fixture的独立coverage mode，仿真可并行、源码不并行修改。
旧NPI事实脚本现绑定归档`resolver_source_before_fix.sv`，与`git show 1343dbce3`逐字节一致，SHA为5bff70b5b7e8f43650228c166747083bfb5c54b4aea67b9503f646ad534f5b3a，
防止当前修后源码污染修前事实；Turing已独立重新运行断言PASS。

## 6. 验证台账与独立review

| 项目 | 当前状态 |
|---|---|
| 原seed修前带波形 | 同724655.8ns/ROB1/126/sample144894复现，1FAIL、unknown0，controller退出 |
| NPI原始事实 | 原生二进制与Hex双记录、1step前1ps，winner4→5各自本拍输出正确 |
| 机器断言/责任review | 主agent及Turing独立PASS，正式测试框架责任确认 |
| 正式方案review | Turing REVIEW_PASS，主agent复核通过后才coding |
| 修后原1705742557 10000笔 | `mother_rollback_same_sample_original1705742557_20261030_results`，fresh compile及完整原参数STRICT_PASS，controller rc0/unknown0、六项错误计数0、RM比较10000次 |
| 软件精确同拍协议 | `mother_rollback_same_sample_protocol_20261030_results`，fresh compile及软件专项PASS，不算DUT PASS |
| 首段原1992896882 10000笔 | `mother_rollback_same_sample_first_split_neighbor1992896882_20261030_results`，独立coverage mode fresh compile及完整原参数STRICT_PASS，controller rc0/unknown0、六项错误计数0、RM比较10000次 |
| 必要真实DUT邻接 | 已严格18/18，根目录`mother_rollback_same_sample_neighbors_20261030_results`；Sv393、CSR2、PBMT2、SFENCE2、cancel2、safe2、新随机fault3 MAIN1000五项全部通过，controller退出/unknown0 |
| 独立实现review | Turing `FINAL_IMPLEMENTATION_REVIEW_PASS`，20真实DUT、1软件、2541输入及三个simv均独立核验，无剩余blocker |
| 单Bug commit/恢复 | 方案移do，随本记录单Bug提交；提交后才启动98恢复和250矩阵，不合并失败记录覆盖率 |

修前原99恢复批次0PASS/1FAIL/19ABORTED/79NOT_RUN均保留。旧1343矩阵50已验收、9控制器PASS及fault3根1通过，
均是旧源码边界，不计当前新SV最终350项。strict-NC10项继续STOPPED_RTL_CONFIRMED。
原1705742557已越过旧724655.8ns首错点，原两个输入分别以144892/144894同拍age0成功关联；
`failure_point_acceptance.json`保留日志前缀182732144字节及SHA，且明确只证明过点，不代表完整10000笔PASS。
后续完整日志验收已完成，`strict_acceptance.json`记录SHA256为24b07012037609494916f60d7c29d58944ad92e0114aa1f64cd14a8080d39db4。
原1992896882完整日志SHA256=c9a3e9c0b34c94e4dc83fdf9190ebea2a0e9efaacee6b04eb55fddd649a147c5；独立coverage mode simv
SHA256=a5f98a4d590fe9577222de5d760b34c7faa4226798e93019826e732afebada55。两根和18邻接均已严格验收；Turing最终review PASS。
ignored结果校验脚本已按独立review补强提交/冻结输入/已验二进制的绑定、350项精确身份集合及URG实际testrecords核对、验收失败终态。
这些只影响证据验收，不改变生产编译输入；历史250报告的parser自测PASS不作为新版覆盖率结果。
Turing独立`READINESS_REVIEW_PASS`确认三项artifact缺口均关闭，内存负例及campaign列数检查通过。
主agent汇总门禁`FINAL_ROLLBACK_IMPLEMENTATION_GATE_PASS`；旧报告parser的缺项/重复/错误mode三负例全部拒绝。

## 7. Plan对齐检查

对应`AI_DOC/plan/test_framework/plan/do/memblock_rollback_same_sample_provenance_fix_plan_20261030.md`。
生产只改sample筛选，接口、缓存状态、完整payload比较、monitor/connect/adapter和RM均不变；软件只改指定task。
相关声明/调用名保持原样；外加vseq使用basicTest，不扩大到其他softcase或新生产参数。

### 实现与Plan不一致项

未发现功能不一致项。四个边界均按计划保留：精确sample、完整身份、同拍完整冲突、失败初始化。
若后续编译/动态发现需变更，必须记IMPLEMENTATION_DELTA再review；当前未引入额外生产机制。

### Plan未说明但Coding落实的细节

任务名保留历史`cross_sample_cache`，现在测试跨拍隔离而非跨拍容错；不因名称误读而扩大调用点重命名。
过期用例不仅验证失败，还检查queue.size()==0，防止精确sample过滤使过期清理验证变成空测。
这些是计划内验证边界的具体断言，不改变生产语义；无需新增公共helper。

## 8. 非本次修改的逻辑分析

`git status`中的四项既有知识库文档不纳入本Bug，不修改/暂存/提交：int_writeback_agent.md、
rob_compress_and_backend_instruction_flow.md、rtl/v2/index.md及backend_memory_writeback_flow.md。
当前仅一个生产逻辑SV及一个既有软件定向SV属于本Bug；其余修改为本Bug分析、W44 Buglist、plan/review及母任务状态。
ignored sim下脚本、fixture、编译/仿真/波形/覆盖率是证据，不进生产filelist，不整包提交。
先前1343首段RM修复已单独提交且无回退证据，不混同当前错误。无RTL或无关cfg改动。
