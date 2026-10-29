# 跨页Load首段数据错误提前终止实现评审

日期：2026-10-30。状态：**FINAL_IMPLEMENTATION_REVIEW_PASS**；原seed10000笔及21/21真实DUT专项、四类必要软件专项通过，随本记录单Bug提交。
工作区：`.codex_mother_integration_20261028`，V2。未修改RTL、cfg、请求生成或异常比较mask。

## 1. 术语与抽象职责

| 术语 | 中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| first split | mabuf实际首页段 | `status.first_split_access` | 跨页LH先处理页尾一字节 |
| attempt | 一次低段request到response | context的三个sample字段 | mabuf重放新request替换已完成旧attempt |
| dynamic/reset epoch | UID重发与DUT复位代次 | `status.dynamic_epoch`、context的`reset_epoch` | 任一不符均不能使用旧证据 |
| S2 take | LU实际取数时刻 | `data_sample_seq`、`s2_fire` | 不用response减拍或WB时间猜取数 |
| cache eligibility | 无miss/forward的纯cache资格 | `data_cache_eligible` | 不包含DUT异常或错误值 |
| replay/forward | 重新执行与数据转发 | `response_need_replay`及S2资格 | 纯replay返回、SQ/MSHR转发均不由捷径授权 |
| context/provenance | 固定执行上下文和身份依据 | `first_split_access`及readonly输出 | UID/ROB/原VA/实际拆分VA须一致 |
| frontier | 同reset连续完整消费边界 | `split_observed_sample_seq/reset_epoch` | 39未覆盖WB40，不能判断“无高页” |
| deferred | 尚未消费完，暂缓比较 | 四参数构造输出 | 不查第二页、不add_item、不置mismatch |
| D-error | 独立D.fire生效的line错误 | `mem_access_base_sequence`历史 | reserve或未来activation不等于已生效 |
| readonly view | 不可变值型副本 | API的context/error输出 | 不泄漏可写owner |
| partial PA | 真实已解析字节的物理地址 | `pa_valid_mask/pa_by_byte` | 低段异常可保留mask1，不填高页PA |
| LDA/WB | 实际Load写回 | `read_dut_load_writeback_for_rm()` | 只取身份/source/sample作上界 |
| owner | 唯一推进该通路的现有实例 | `lifecycle_owner_name` | 不新增驱动者 |
| active map | ROB到活动UID的映射 | `uid_by_active_rob` | 常数时间查找，不扫全主表 |
| baseline | reset后完整观察的静默起点 | 首次完整sample/ready历史 | 尚未开放ready且无split活动 |
| poll | 当前UID周期性比较尝试 | `observer_poll_process()` | 最多等待256次 |

以上概念的代码落点：first split/attempt/context均为`status.first_split_access`；dynamic epoch为`status.dynamic_epoch`，
reset epoch为`get_l2tlb_current_reset_epoch()`；S2 take/cache eligibility分别为`data_sample_seq/data_cache_eligible`；
replay/forward由`response_need_replay`及S2纯cache资格限制；frontier为`split_observed_sample_seq/reset_epoch`；
deferred为四参数构造函数的输出；D-error为`mem_access_base_sequence`独立历史；readonly view为API的按值输出；
partial PA为`item.pa_valid_mask/pa_by_byte`；LDA/WB为`read_dut_load_writeback_for_rm()`输出。

抽象职责：新增旁路只说明执行身份和时刻。RM先独立验证首页翻译、权限、PBMT/PMA与cache路径，
再按实际S2 sample查询独立错误事实；只有已经独立证明应异常终止时才不要求未执行的高页。

## 2. 修改前后与文件覆盖

修改前：构造全部字节翻译成功后，后置D-error helper才有机会执行。DUT低段错误取消高段时，
RM先因高页不存在而query miss，无法预测本应出现的数据异常。

修改后：首页解析完成、第二页resolver之前增加首段独立错误预测；正常访问完整PA要求和RM/LDA/status严格比较不变。
尚未settled只暂缓；settled但无证据仍走旧路径，真实缺翻译不会因“可能首段错误”被吞掉。

下列源码路径均相对`mem_ut/ver/ut/memblock`：

| 文件 | 实现与读写职责 |
|---|---|
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv` | 五个被动输入及drv/mon两个clocking清单同步 |
| `tb/L2tlb_agent_connect.sv` | V2实际S2 fire/ROB/vaddr及纯cache资格；inactive全部0，不驱动RTL |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv` | 五个非rand四态字段，随原immutable transport传输 |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv` | 原mon_cb边界按值复制；不改变owner/ready/response |
| `seq/base_seq/memblock_l2tlb_base_sequence.sv` | 原reset/anchor/history/lifecycle门禁之后消费首/后段，再发布连续边界 |
| `seq/base_seq_help/status_transaction.sv` | 每UID一个固定context，constructor/reset清0，terminal保留 |
| `seq/base_seq_help/common_data_transaction.sv` | 三个被动observer、两个水位标量、两个redirect重置入口清理 |
| `seq/base_seq_help/memblock_rm_readonly_api.sv` | 先frontier，再身份/资格/独立历史；输出pending或值型证明 |
| `env/src/memblock_rm/memblock_rm.sv` | 独立首段预测、跨页前接入、deferred有界处理；旧调用兼容 |
| `seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv` | 仅增加本Bug所需正负向；不更改无关soft_test预期 |
| `rule/version/v2/l2tlb_interface_profile.md` | 同步五个S2被动信号定位、有效性与原responder职责边界 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md` | 仓库根相对路径；同步首段执行与连续消费流程说明 |

profile和L2TLB源码分析同步解释被动诊断，不把L2TLB agent变为memory下游模型。

## 3. 首段状态与被动采样

### 3.1 `observe_first_split_request_sample()`

抽象功能描述：把实际mabuf请求绑定当前动态UID，建立低段attempt或使“无高页”的证明失效，不生成翻译。
源码位置：`seq/base_seq_help/common_data_transaction.sv`。调用者为已有L2TLB owner，不新增调度owner。

核心替换分支：

```systemverilog
if (same_instance &&
    (context_value.response_sample_seq == 0 ||
     context_value.response_sample_seq >= sample_seq)) begin
    `uvm_fatal("FIRST_SPLIT_ACCESS",
               $sformatf("overlapping low split uid=%0d epoch=%0d old_request=%0d old_response=%0d new_request=%0d",
                         uid, status.dynamic_epoch, context_value.request_sample_seq,
                         context_value.response_sample_seq, sample_seq))
    return;
end
```
中文伪代码：此分支保护正在执行的低段，不允许新请求覆盖未完成attempt。若同实例旧response不存在或不早于新request，
立即fatal且保持原结构；只有验证通过才构造新值并一次发布。前面的active ROB查找只定位当前UID，
不扫描主表；高页分支仅置later_page_seen，低段新attempt继承该标记，不会把已执行高页抹掉。

完整控制流：无fire返回；检查非零sample/当前reset/active ROB；检查scalar Load；
比较context的UID/epoch/reset/ROB；高页在无当前低段context时fatal，否则标记并返回；低页先执行上述冲突检查，再构造清零的新值，
填原VA、实际VA和request sample，继承同实例later_page_seen后按值写回。非法输入在状态副作用前失败。

### 3.2 `observe_first_split_data_sample()`

抽象功能描述：记录与当前低段attempt对应的真实取数时刻及路径资格，供独立历史查询，不采集DUT异常。
源码位置同上；输入来自新增五个S2被动字段。

```systemverilog
if (!context_value.valid || context_value.uid != uid ||
    context_value.dynamic_epoch != status.dynamic_epoch ||
    context_value.reset_epoch != reset_epoch ||
    reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
    context_value.rob_key != rob_key || context_value.request_vaddr != vaddr ||
    context_value.request_sample_seq >= sample_seq ||
    context_value.data_sample_seq >= sample_seq || context_value.response_sample_seq != 0) return;
status.first_split_access.data_sample_seq = sample_seq;
status.first_split_access.data_cache_eligible = cache_eligible;
```
中文伪代码：该逻辑在低段执行时关联取数。无有效事件或无活动ROB先返回；读取该UID的context副本，
身份、epoch/reset、实际VA、request先于data及响应尚未发生必须同时满足。只用严格更晚的S2覆盖旧取数，
因此LU内部fast replay无需新mabuf请求也能更新到最终数据时刻。获取reset epoch只读取当前复位代次，
不会清理或创建状态。输出仅为同结构两个字段，不产生pending工作项。

### 3.3 `observe_first_split_response_sample()`

抽象功能描述：保存同attempt低段返回的结构身份、路径与重放事实，不解码DUT异常形成预期。
源码位置同上。

```systemverilog
if (!context_value.valid || context_value.uid != uid ||
    context_value.dynamic_epoch != status.dynamic_epoch ||
    context_value.reset_epoch != reset_epoch ||
    reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
    context_value.rob_key != rob_key || context_value.request_vaddr != vaddr ||
    context_value.data_sample_seq == 0 || context_value.data_sample_seq >= sample_seq ||
    context_value.response_sample_seq != 0) return;
status.first_split_access.response_sample_seq = sample_seq;
status.first_split_access.response_vaddr = vaddr;
status.first_split_access.response_nc = nc;
status.first_split_access.response_mmio = mmio;
status.first_split_access.response_need_replay = need_replay;
```
中文伪代码：无有效响应或活动ROB先返回；读取该UID context后检查完整身份及严格data早于response。
高页VA与低段不同，不更新本结构；重复response不能覆盖已保存事实。通过后按值保存时刻、VA和路径/replay，
不会读取或保存exception。原later-page observer随后继续按原规则运行，职责不替代。

### 3.4 连续消费边界与清理

抽象功能描述：L2TLB owner证明split观测已完整推进到哪个sample，RM据此区分“没有后页”与“还没消费”。
源码位置：`seq/base_seq/memblock_l2tlb_base_sequence.sv::send_l2tlb_cycle()`。

```systemverilog
if (data.split_observed_sample_seq == 0 ||
    data.split_observed_reset_epoch != sample.sampled_reset_epoch ||
    sample_seq == data.split_observed_sample_seq + 1) begin
    data.split_observed_reset_epoch = sample.sampled_reset_epoch;
    data.split_observed_sample_seq = sample_seq;
end
```
中文伪代码：所有首段/后页observer完成后才进入此分支。首次或reset换代建立边界，已有同reset边界只能逐sample前进；
出现缺口不执行赋值，后拍不能跨过。首次建立前另行检查本reset未开放ready且无split活动，不用空context推断静默。
NOT_READY、history缺失、reset或stale sample早退都不会穿过本发布点；两个标量不影响ready或token调度。

`status_transaction::reset()`以及两个redirect重置入口均清first_split_access，reset epoch立即使旧证据不可读。
普通terminal保留固定事实直到status生命周期结束，不影响drain/end，不追加历史队列。每拍只做O(1)查找与结构更新。

### 3.5 最终资格与同拍顺序源码

抽象功能描述：connect仅观察本次实际S2是否来自无forward的纯cache，payload无效时不能把选择位当成有效路径。
源码位置：`tb/L2tlb_agent_connect.sv`。以下为最终资格完整表达式，其他五字段仍按原mon_cb复制。

```systemverilog
force U_IF_NAME.lu1_split_data_cache_eligible = \
    !RTL_PATH.inner_LoadUnit_1.io_dcache_resp_bits_miss && \
    !RTL_PATH.inner_LoadUnit_1.s2_full_fwd && !RTL_PATH.inner_LoadUnit_1.s2_fwd_frm_d_chan && \
    !RTL_PATH.inner_LoadUnit_1.io_forward_mshr_forward_result_valid && \
    !RTL_PATH.inner_LoadUnit_1.s2_nc_with_data && \
    !({RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_15, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_14, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_13, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_12, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_11, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_10, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_9, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_8, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_7, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_6, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_5, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_4, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_3, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_2, \
       RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_1, RTL_PATH.inner_LoadUnit_1.s2_fwd_mask_0}); \
```
中文伪代码：该表达式只生成路径资格，不生成异常期望。raw miss、完整转发、D-channel选择、有效MSHR数据、NC-with-data
或任意forward mask为1时都不允许purecache资格；不再读取result_valid=0时无意义的MSHR选择payload。
没有调用状态helper，不修改DUT；未知输入最多产生未知资格，消费时必须严格等于1才能置可授权，不能将X当true。

抽象功能描述：`send_l2tlb_cycle()`按实际时序把同拍请求、返回与取数关联，避免新S2覆盖返回仍需要的拍前取数。
源码位置：`seq/base_seq/memblock_l2tlb_base_sequence.sv`。首段三个观测调用块如下：

```systemverilog
data.observe_first_split_request_sample(
    sample.sampled_split_load_req_fire &&
        !sample.sampled_split_load_req_misalign_need_wakeup,
    '{flag: sample.sampled_split_load_req_rob_flag,
      value: sample.sampled_split_load_req_rob_value},
    sample.sampled_split_load_req_fullva, sample_seq, sample.sampled_reset_epoch);
data.observe_first_split_response_sample(
    sample.sampled_split_load_resp_valid &&
        !sample.sampled_split_load_resp_misalign_need_wakeup,
    '{flag: sample.sampled_split_load_resp_rob_flag,
      value: sample.sampled_split_load_resp_rob_value},
    sample.sampled_split_load_resp_vaddr,
    sample.sampled_split_load_resp_nc, sample.sampled_split_load_resp_mmio,
    sample.sampled_split_load_resp_need_replay,
    sample_seq, sample.sampled_reset_epoch);
data.observe_first_split_data_sample(
    sample.sampled_lu1_split_data_valid === 1'b1,
    '{flag: sample.sampled_lu1_split_data_rob_flag,
      value: sample.sampled_lu1_split_data_rob_value},
    sample.sampled_lu1_split_data_vaddr,
    sample.sampled_lu1_split_data_cache_eligible === 1'b1,
    sample_seq, sample.sampled_reset_epoch);
```
中文伪代码：先从真实request建立或校验低段attempt；再让response与此前取数关联并保存结构返回事实；
最后处理本拍S2。response已保存时，data observer会返回而不覆盖；无response的内部fast replay则可更新到更晚S2。
三个子函数分别修改同UID固定结构的请求、返回、取数部分，不驱动接口或生成翻译；原later-page流程随后继续，最后才发布连续水位。

### 3.6 固定字段和两个redirect清理入口

| 字段组 | 写入者 | 读取者与清理 |
|---|---|---|
| valid/UID/dynamic/reset/ROB/原VA/实际VA/request sample | request observer，替换前检查旧attempt完成 | data/response/readonly；status reset和两个redirect入口清0 |
| data sample/cache资格 | data observer，响应前保存最后一次匹配S2 | readonly按此sample查独立错误；新attempt清0，response后不改 |
| response sample/VA/NC/MMIO/need_replay | response observer，仅保存首次合法同attempt返回 | readonly；新attempt清0，不把异常向量存入结构 |
| later_page_seen | 同实例高页request置1，低段replay继承 | readonly拒绝早停；仅status/reset/redirect新实例清理 |
| 连续消费两个标量 | 现有L2TLB owner在全部observer完成后推进 | readonly先检查；constructor清0，reset以epoch隔离，缺口冻结 |

抽象功能描述：`reset_uid_after_older_redirect()`清除旧动态实例执行事实，使UID重新发射后不能借用旧低段返回。
源码位置：`seq/base_seq_help/common_data_transaction.sv`，实际新增清理点为：

```systemverilog
void'(clear_uid_dispatch_result(uid));
status.first_split_access = '{default:'0};
```
中文伪代码：先由既有dispatch清理helper清除旧发射结果，随即清除首段值型事实，再继续原redirect重发状态处理。
该子函数原有ROB/terminal合法性检查仍在前面执行；没有新增清除队列或改变旧epoch推进。

抽象功能描述：`commit_reset_uid_after_older_redirect_for_redirect_range()`在已验证redirect范围提交阶段同步清除旧事实。
源码位置同上，实际新增清理点为：

```systemverilog
status.clear_sta_late_fault_tombstones();
status.first_split_access = '{default:'0};
status.load_pass = 1'b0;
```
中文伪代码：在既有STA迟到fault历史清理后清除首段上下文，再继续原load结果复位；后面的dynamic epoch递增保持原样。
STA helper只清其原有历史，新增首段清理是固定结构赋零，不引入扫描或pending工作。

## 4. RM只读与预期构造

### 4.1 `try_read_first_split_cache_error_for_rm()`

抽象功能描述：以已独立解析的物理line和LDA时间上界，返回对应低段实际取数时已生效的独立错误。
源码位置：`seq/base_seq_help/memblock_rm_readonly_api.sv`。

```systemverilog
if (data.split_observed_sample_seq == 0 ||
    data.split_observed_reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
    data.split_observed_sample_seq < bound_load_sample) begin
    observation_pending = 1'b1;
    return 1'b0;
end
```
中文伪代码：此门禁在任何context有效性判断之前执行。当前reset消费边界未覆盖LDA时，返回pending，
不把尚未发布的context当作不存在。读取当前reset仅提供代次比较；不修改owner或创建表项。
边界已到后才检查UID/status/epoch/ROB、原VA、首页VPN、request/data/response顺序及response上界，
并按main fuOpType验证V2精确低段VA几何：LH原VA，LW低2位1减1，LD低3位1/2/3分别减1/2/3、5减1，其余原VA。
request还须与原VA同line，response等于request，同页不同line或同line错误拆分也不能借用原line错误。
拒绝后页已执行、forward、NC/MMIO及need_replay。最后用data_sample查询原独立D-error历史；
记录未就绪、仅reserve、未来activation、clean均不产生证明。成功时复制context/error值，失败输出保持无证明。

源码位置同上，完整低段几何选择与结构拒绝块：

```systemverilog
expected_low_vaddr = original_vaddr;
case (data.main_table_by_uid[uid].fuOpType[1:0])
    2'd1: ;
    2'd2: if (original_vaddr[1:0] == 2'd1) expected_low_vaddr--;
    2'd3: begin
        case (original_vaddr[2:0])
            3'd1, 3'd2, 3'd3: expected_low_vaddr -= original_vaddr[2:0];
            3'd5: expected_low_vaddr--;
            default: ;
        endcase
    end
    default: return 1'b0;
endcase
status = data.status_by_uid[uid];
candidate = status.first_split_access;
if (!candidate.valid || candidate.uid != uid || status.dynamic_epoch != dynamic_epoch ||
    candidate.dynamic_epoch != dynamic_epoch || candidate.rob_key != rob_key ||
    status.get_rob_key() != rob_key || candidate.original_vaddr != original_vaddr ||
    candidate.reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
    candidate.request_vaddr[63:12] != original_vaddr[63:12] ||
    candidate.request_vaddr != expected_low_vaddr ||
    candidate.request_vaddr[63:6] != original_vaddr[63:6] ||
    candidate.response_vaddr != candidate.request_vaddr ||
    candidate.request_sample_seq == 0 ||
    candidate.data_sample_seq <= candidate.request_sample_seq ||
    candidate.response_sample_seq <= candidate.data_sample_seq ||
    candidate.response_sample_seq > bound_load_sample ||
    candidate.later_page_seen || !candidate.data_cache_eligible ||
    candidate.response_nc || candidate.response_mmio || candidate.response_need_replay) return 1'b0;
```
中文伪代码：该块先按主表指令尺寸计算合法实际低段地址，LB等无本捷径支持的尺寸直接返回未证明。
再读取当前status的值型candidate，依次要求有效UID、当前dynamic/reset epoch、ROB、原VA、精确低段VA和同line，
并验证request早于data早于response且response不晚于LDA。最后排除已执行高页、非purecache、NC/MMIO和replay。
status的ROB getter与当前reset getter均只读；任何不符都不修改状态、不授予早停。

源码位置同上，独立历史查询与值型输出块：

```systemverilog
if (!read_l2_d_error_for_rm(line_addr, candidate.data_sample_seq, error_view) ||
    !error_view.valid || (!error_view.denied && !error_view.corrupt)) begin
    error_view = '{default:'0};
    return 1'b0;
end
context_view = candidate;
return 1'b1;
```
中文伪代码：只在结构身份全部通过后，以实际S2 sample读取指定line的既有独立D.fire历史。
历史未就绪、clean或尚未activation时清空错误输出并返回未证明；有独立有效错误才复制context并返回成功。
子函数不会创建backing、推进状态或消费队列，查询sample不是更晚WB，DUT异常向量不是输入。

### 4.2 `observer_try_apply_first_split_data_error()`

抽象功能描述：在第二页翻译前证明首页普通cache访问应已以数据异常结束，构造非零期望并保留部分PA。
源码位置：`env/src/memblock_rm/memblock_rm.sv`。

流程按源码顺序：检查prefix和PMA模型；逐已解析byte证明PA有效、同line、独立PMA/PMP有效且normal cacheable无AF；
按同ROB读取LDA，只校验身份、source、非replay/flush和sample，不根据exception值分支；调用只读首段证明；
未settled透传deferred；无证明返回旧翻译路径；有证明只从独立error置denied/corrupt及bit5/19，denied优先。
日志列出request/data/response/WB sample、activation/generation和partial mask，便于审计取数时序。
逐byte上限为scalar8字节，不扫描历史集合。既有后置D-error helper看到已形成异常后不会重复追加。

源码位置同上，前置PMA和LDA上界的关键完整分支：

```systemverilog
for (int unsigned idx = 0; idx < prefix_bytes; idx++) begin
    if (!item.pa_valid_mask[idx] ||
        item.pa_by_byte[idx][47:6] != item.pa_by_byte[0][47:6] ||
        !ro.read_pma_pmp_af_for_rm(item.uid, item.dynamic_epoch, 1'b1,
                                  item.pa_by_byte[idx][47:0], 1, PMA_PMP_CMD_READ, byte_view) ||
        !byte_view.valid || !byte_view.translation_eligible || !byte_view.af_decided ||
        !byte_view.normal_cacheable || byte_view.ld_access_fault) return 1'b0;
end
rob_key = '{flag: item.rob.flag, value: item.rob.value};
if (!ro.read_dut_load_writeback_for_rm(rob_key, load_view) ||
    !load_view.valid || !load_view.rob_valid || !load_view.rob_flag_valid ||
    load_view.rob_flag !== rob_key.flag || load_view.rob_value !== rob_key.value ||
    load_view.source_kind != 2'd1 || load_view.sample_seq == 0 ||
    load_view.replay_inst || load_view.flush_pipe) return 1'b0;
```
中文伪代码：逐首页已解析byte要求真实有效PA在同line，并从冻结PMA/PMP模型读取单byte只读view，普通cache且无AF才继续。
任何byte失败返回未证明，不绕过权限或MMIO。随后按同ROB读取真实LDA，验证来源/身份与非replay、非flush，
只取得sample上界；两个只读子函数不驱动DUT、不生成预期，也不以LDA exception值选择分支。

源码位置同上，独立异常预期生成块：

```systemverilog
if (!ro.try_read_first_split_cache_error_for_rm(
        item.uid, item.dynamic_epoch, rob_key, item.computed_vaddr,
        memblock_rm_readonly_api::mem_addr_t'({item.pa_by_byte[0][47:6], 6'b0}),
        load_view.sample_seq, context_view, error_view, deferred)) return 1'b0;
item.expected_dcache_denied = error_view.denied;
item.expected_dcache_corrupt = error_view.corrupt && !error_view.denied;
if (item.expected_dcache_denied) item.expected_exception[5] = 1'b1;
if (item.expected_dcache_corrupt) item.expected_exception[19] = 1'b1;
```
中文伪代码：以前述真实首页line、当前UID/epoch/ROB/原VA和LDA上界读取首段证据及独立错误。
未settled通过deferred返回等待，settled但无证明返回旧翻译路径；只有成功时从独立错误设置expected字段。
denied优先产生bit5，只有非denied的corrupt产生bit19，不读取DUT异常作期望、不填高页PA；后面只打印证据日志并成功返回。

### 4.3 构造接口与提交主流程

抽象功能描述：构造独立访问预期；仅已证明首段终止时不再要求未执行页，未settled则保留提交游标等待。
源码位置同上。

```systemverilog
if (deferred) begin
    observer_first_split_wait_count++;
    if (observer_first_split_wait_count < 256) return 1'b0;
    if (!observer_mismatch_reported) begin
        `uvm_error("RM_LS_FIRST_SPLIT_NOT_SETTLED",
                   $sformatf("uid=%0d split observation did not settle after 256 polls", main_view.uid))
        observer_mismatch_reported = 1'b1;
    end
    return 1'b0;
end
```
中文伪代码：构造报告deferred时递增当前UID固定等待计数；不到256次直接返回，既不add_item也不推进current_uid。
达到上限明确报观测未收敛并置永久mismatch，禁止无限等待；不会退回第二页查询。构造成功或普通错误清等待计数，
普通错误仍沿用原比较报错。constructor/configure清0。没有修改ROB提交、激励或terminal推进。

插入点仅在paged misaligned scalar Load首次越出首页、硬件支持拆分、PBMT=PMA且首页无翻译/PMA/MMIO异常时。
正常无异常访问仍必须具有完整PA。部分PA只随独立非零异常存在，既有优先级和严格比较mask均保留。
原三参数`observer_build_commit_item()`保留wrapper，只返回四参数`observer_build_commit_item_with_observation()`的成功值，
不能把deferred当成功；真实提交主流程直接调用四参数版本并处理等待。没有改动旧soft_test调用或其期望。

源码位置同上，第二页resolver前的完整新增分流：

```systemverilog
if (byte_va[63:12] != first_vpn && item.kind == RM_LS_KIND_LOAD &&
    misaligned && tlb_context.hd_misalign_ld_enable && all_bytes_pma_pbmt &&
    !translation_access_fault && !stage_one_fault && !stage_two_fault &&
    !pma_pmp_access_fault && !first_byte_is_pma_mmio) begin
    if (observer_try_apply_first_split_data_error(item, byte_index, deferred)) break;
    if (deferred) return 1'b0;
end
```
中文伪代码：只有准备越出首页、scalar Load支持硬件非对齐、PBMT=PMA且首页无任何已知翻译/PMA/MMIO异常才进入新预测。
首段独立错误helper证明终止则退出byte循环，保留其非零期望和部分PA；未消费完则退出构造让主流程等待。
已settled但无证明时继续紧接的原第二页resolver查询，真实query miss仍报错，不放宽正常访问完整PA要求。

抽象功能描述：三参数wrapper只兼容已有调用语法，不承担生产异步等待策略。源码位置同上，完整函数如下：

```systemverilog
function bit memblock_rm::observer_build_commit_item(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    memblock_rm_readonly_api::status_view_t status_view,
    output rm_ls_program_item_t item
);
    bit deferred;
    return observer_build_commit_item_with_observation(main_view, status_view, item, deferred);
endfunction:observer_build_commit_item
```
中文伪代码：接收原三个参数，声明本地等待输出，再调用四参数真实构造函数并原样返回成功值。
子函数负责全部预期/PA构造，未settled时本来返回失败，因此wrapper不能把等待解释为PASS；生产提交流程不调用本包装入口。

## 5. 验证台账

| 项目 | 当前证据/状态 |
|---|---|
| 原seed修前复现 | seed1992896882、10000笔，602001.1ns UID2401，debug重现、NPI独立分类PASS |
| 最小修前复现 | seed26013001，U=100，3498ns UID0第二页query miss；U=0旧尝试仅PF不算 |
| 首轮compile | 旧三参数调用缺deferred；未运行seed，结果保留 |
| r2 compile | 主表字段命名修正前失败；未运行seed，结果保留 |
| r3 compile | VCS拒绝output常量默认实参，未运行seed；改为原接口wrapper，失败证据保留 |
| r4 compile/最小真实DUT | compile PASS，真实最小用例FAIL；资格误读无效MSHR选择payload，NPI证据保留 |
| r4必要正负向 | PASS，marker一次；final ERROR/FATAL0，预期caught FATAL5/ERROR2、demoted0；不计DUT strict PASS |
| r5 compile/最小真实DUT | STRICT_PASS，`mother_first_split_compact_post_fix_r5_20261030_results`；参数与修前逐字节一致，NPI同拍资格0→1，其余DUT事实不变 |
| denied真实DUT | STRICT_PASS 1/1，`mother_first_split_denied_r5_20261030_results`；独立denied产生bit5，部分PA mask1 |
| clean真实DUT | STRICT_PASS 3/3，`mother_first_split_clean_r5_20261030_results`；完整双页PA mask3、异常0，无首段早停 |
| LW/LD corrupt真实DUT | 各STRICT_PASS 3/3，`mother_first_split_lw_corrupt_r5_20261030_results`、`mother_first_split_ld_corrupt_r5_20261030_results` |
| r5必要正负向 | 软件专项PASS，`mother_first_split_provenance_r5_20261030_results/software_acceptance.json`，caught单列，不算DUT严格PASS |
| 取消邻接定向 | 软件专项PASS，`mother_first_split_cancel_neighbor_r5_20261030_results`，四场景完成且预期caught FATAL19/ERROR0 |
| deferred/frontier协议 | 外加fixture r2软件专项PASS，`mother_first_split_protocol_gate_r2_20261030_results`，真实方法执行，预期caught ERROR1/FATAL0；新增admission-settled逐步断言确实执行 |
| Sv39/CSR邻接 | 分别STRICT_PASS 3/3与2/2，`mother_first_split_sv39_neighbor_r5_20261030_results`、`mother_first_split_csr_neighbor_r5_20261030_results` |
| 原seed10000笔 | STRICT_PASS 1/1，`mother_first_split_original1992896882_r5_20261030_results/strict_acceptance.json`；RM比较10000次，原effective plusargs逐字节一致、无临时覆盖 |
| PBMT/SFENCE邻接 | 各STRICT_PASS 2/2，`mother_first_split_pbmt_neighbor_r5_20261030_results/strict_acceptance.json`与`mother_first_split_sfence_neighbor_r5_20261030_results/strict_acceptance.json` |
| 原优先级软件邻接 | PASS，`mother_first_split_priority_neighbor_r5_20261030_results/software_acceptance.json`，预期caught ERROR3/FATAL0；原PBMT/后页优先级未放宽 |
| 独立实现review | Turing于2026-10-30完成最终只读源码、r2 fixture及全量动态证据复核，`FINAL_IMPLEMENTATION_REVIEW_PASS`，无剩余blocker |

真实DUT合计21/21（原seed1、LH corrupt1、denied1、clean3、LW3、LD3、Sv393、CSR2、PBMT2、SFENCE2）。
软件四类为provenance、cancel、protocol r2、priority，预期caught计数分别ERROR/FATAL=2/5、0/19、1/0、3/0；
均为raw/final/demoted ERROR/FATAL0，不计真实DUT严格通过或DUT有效覆盖贡献。各controller退出、unknown0。
以上结果目录均相对mother `mem_ut/ver/ut/memblock/sim`；各轮FAIL与编译失败保留，不覆盖原记录。
r5目录含`strict_acceptance.json`、`cache_eligibility_npi.json`、`npi_gate_acceptance.json`及`actual_compile_inputs.json`。
输入快照包含2540文件，只有本方案10个SV输入相对已通过基线变化，RTL保持不变；新simv不以旧HEAD标签冒充已提交版本。

必要软件定向覆盖边界：

| 覆盖项 | 例数/验证方式 | 不能冒称的范围 |
|---|---|---|
| 身份/时序/路径拒绝 | 16个独立反例，直接调用生产readonly API | 不是DUT接口故障注入 |
| LW/LD低段几何 | 10个合法组合及10个错误组合 | 真实DUT另以短小LW/LD验证，不混算 |
| 同拍response/data、资格1→0、内部重取数 | 生产observer按实际顺序直接调用，检查不覆盖旧响应取数 | 不模拟整个L2TLB驱动器 |
| 水位不足、context尚未创建、迟后settle、旧reset | 直接调用readonly，pending与证明分开 | 没有动态注入L2TLB transport缺口 |
| clean、reserve-only、data后/WB前activation | 独立D-error历史查询负向 | 不使用DUT异常决定期望 |
| 高页sticky、首次高页、status reset | 正向状态保持及预期fatal，原5个caught FATAL/2个caught ERROR单列 | caught不能计作真实DUT strict PASS |
| 连续frontier发布和256poll上限 | 外加fixture执行真实RM方法和真实`send_l2tlb_cycle()`，延迟恢复/第256次专用错误/缺口不自愈均PASS | 是软件sample注入，不是完整真实driver/ACK或owner释放验证 |

### 5.1 外加协议fixture的复现边界

fixture绝对路径：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_first_split_protocol_gate_fixture_20261030/first_split_protocol_gate_tc.sv`，
最终r2 SHA256=`661976be78f9e86b9013e870899a2fede586b91ff4274fdd125d77df3365bded`。
旧r1源码归档于`mother_first_split_protocol_gate_r1_20261030_results/first_split_protocol_gate_fixture_r1.sv`，
SHA256=`590b1c94a63f91762764523e412ce5740c1ad0e27b0d973a775de783db843bee`；r1不足以独立证明frontier分支已走到，不替代r2。
通过既有TB_LIST只向独立mode追加，实际编译命令在`status/compile.final.log`首行；生产10个SV输入不变，
原mode simv SHA仍`ed06da6e336aab91b015aa1f7254a5deb35b202f3351fdf71d97cc1b5e057d31`。
r2的`actual_compile_inputs.json`确认2540核心输入逐项与r5一致，只有1个外加fixture文件，共2541项；
相对旧通过基线仅本Bug10个SV输入变化，无RTL或cfg变化。

抽象功能描述：fixture只提供合法的模型/身份前置和输出记录接点，调用生产函数验证有界等待与连续消费协议，不定义另一套算法。
先用继承的helper构造合法首页翻译、S态PMP NAPOT权限和普通PMA，确保高页canonical表为空；注入明确软件LDA与独立corrupt。
不推进仿真时间地调用真实RM：3次pending检查无item/history/UID推进或mismatch，水位达到500后partial mask1/bit19恰提交一次；
重建模型后第1–255次保持等待，第256次只捕获`RM_LS_FIRST_SPLIT_NOT_SETTLED`，无item/history且UID仍0。

随后用test-only sequence继承真实service，仅替换虚拟send/ACK为本地计数；不重写frontier更新或`send_l2tlb_cycle()`。
合法软件owner、空pending/barrier、C−2 history和ready hold前置齐备，READY100/101、NOT_READY102、READY103/104的实际水位为100/101/101/101/101。
r2在READY每步检查admission-settled等于当前step，在NOT_READY102检查仍为101，证明READY103/104实际通过前置并到达缺口冻结分支；
不是因所有后续sample都在observer之前早退而伪通过。
零时间内恢复保存的owner/history数值，不制造真实driver ACK或release grant，不声称完整transport释放闭环。
软件日志只有预期caught ERROR1，raw/final/demoted ERROR/FATAL均0，controller rc0/unknown0；Turing最终fixture源码/动态review PASS。

## 6. Plan对齐检查

关联plan：`AI_DOC/plan/rm_plan/do/memblock_first_split_data_error_termination_fix_plan_20261030.md`。
字段、所有权、reset/redirect清理、只读身份及实际S2、首页PA/PMA、严格比较、deferred/连续边界均逐项对照。
专项及最终review均已完成，plan迁do；当前review按规则留undo，不因代码完成自动归档。

最终独立reviewer：Turing（`01a1875e-af6d-7941-b1e1-bd1bd9c69010`），2026-10-30，
`FINAL_IMPLEMENTATION_REVIEW_PASS`。主agent复跑统一验收脚本及2540输入逐项hash检查，
结果`mem_ut/ver/ut/memblock/sim/mother_fault3_recovery_plan_20261030/focused_gate_acceptance.json`为PASS。
本Bug专项关闭不代表母任务完成：剩99 fault3和修复后250矩阵尚未执行，旧250覆盖率不能当最终交付。

### 实现与Plan不一致项

执行中调整已记`IMPLEMENTATION_DELTA`：原三参数接口保留wrapper，生产使用独立四参数版本接收deferred，
output默认0候选因VCS不支持已弃用；同attempt响应前S2允许更晚取数覆盖，以适配LU内部fast replay。
后续独立review又补严精确低段VA/line、首次高页fail-fast和同拍处理顺序，当前顺序为request→response→data，
response成功后同拍S2不能覆盖既有取数。相关确定性反例同步补齐，仍仅保存最终一份值。
均不扩大DUT支持面、不改旧测试预期；独立review须确认，无专项证据前不视为闭合。

r4动态揭示`forward_result_valid=0`时MSHR选择payload不具资格含义；删除该无效payload否决，仍以valid排除有效转发，
valid为X仍经资格严格等于1而拒绝。Turing已对照数据选择源码review该修正方向PASS，r5须以相同最小参数证明真实修复。

逐项差异处置（源码支撑均已在前文章节紧邻中文伪代码展示）：

| 原方案 | 最终实现 | 原因与源码落点 | 支撑块/处理结论 |
|---|---|---|---|
| 构造函数直接增加deferred输出 | 原3arg wrapper保留，新4arg处理等待；output默认0候选弃用 | VCS旧调用源兼容及output实参规则，`memblock_rm.sv` | §4.3完整wrapper及分流；保持最终实现，弃用候选只留历史 |
| 每request保存一个S2取数 | 响应前允许更晚S2替换sample和资格，响应后锁定 | LU内部fast replay无需新mabuf请求，`common_data_transaction.sv` | §3.2完整状态更新；保留，资格反转/同拍定向已验证 |
| request→data→response | request→response→data | 同拍新S2不能使合法旧响应被静默丢弃，`memblock_l2tlb_base_sequence.sv` | §3.5完整调用块；保留，定向覆盖 |
| 同VPN低段身份 | 精确低段几何且同cacheline，首次高页无context fatal | 防止同页异line借错误和高页禁止事实遗忘，readonly/common_data | §4.1几何/拒绝及§3.1控制流；保留，16反例和20几何用例覆盖 |
| 资格否决MSHR选择payload | 仅有效数据valid参与该否决 | payload在valid0时可为1或X，`L2tlb_agent_connect.sv` | §3.5最终表达式、r4/r5 NPI对照；保留，真实最小DUT证明PASS |
| 只扩展原provenance helper | 另用外加fixture调用真实RM/sequence完成协议故障注入 | 不修改已冻结的生产输入，又需动态测256poll和缺口，fixture不进默认filelist | §5.1完整边界及独立输入/hash；作为附加验证，不能算DUT或完整transport释放PASS |

上述差异均写入plan的IMPLEMENTATION_DELTA，经独立方向review或实现复核；不把执行中修订伪装成原方案已定义。

### Plan未说明但Coding落实的细节

主表使用既有`fuType/fuOpType`，readonly视图才是snakecase字段；属于既有API适配，不是新增状态或行为。
新S2四态资格只有严格等于1才保存为可授权，未知资格不能变成cache证明。

## 7. 非本次修改的逻辑分析

`git status`中四项既有知识库修改不属于本Bug，不覆盖、不暂存、不提交：
`AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`、
`AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、
`AI_DOC/analysis/rtl/v2/index.md`及未跟踪的`AI_DOC/analysis/rtl/v2/flows/backend_memory_writeback_flow.md`。
母任务问题/方案总review和状态台账属于本轮任务文档，但不能替代本Bug实现与动态验证。
sim目录为ignored仿真/波形/覆盖率产物，保留全部失败轮，不纳入源码提交。
本Bug相关文档包括当前plan/review、fault3分析、L2TLB源码分析、V2接口profile、W44 RM Buglist及母状态表；
母任务问题/方案总review属于本轮管理索引，不混称本Bug逐函数审查。没有其他未分类生产源码修改。
