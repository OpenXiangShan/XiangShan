# V2 CMO 核心语义、真实请求证明与测试层实现复审

- 日期：2026-11-11；版本：V2 / `mem_ut_uvm_v2`。
- 状态：源码静态复审完成；实际 VCS 编译、软件自测和真实 DUT 测试结果由主 agent 补充，本文不据临时桩语法检查宣称功能通过。
- 范围：本次协作中 CMO 纯值核心、请求观察器及测试层适配；统一 Uncache 错误观察器和 DCache 错误来源观察器分别另有专项复审。

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| CMO / CBO | 缓存块操作，本轮覆盖最终 zero/clean/flush/inval | `rm_cmo_op_e` | inval 经框架改为 flush 后按最终 flush 比较 |
| isolated cold / whitelist | 独立冷目标故障用例 / 测试拓扑能力白名单 | 派生cold builder/VSEQ、basicTest能力函数 | 保留含年轻Store的原失败场景，另以年轻Load隔离CMO错误验收 |
| owner / epoch | 动态指令身份 / 同 UID 的实例代次，完全读取框架 | item 与 completion 的 UID、ROB、SQ、dynamic_epoch | UID2旧epoch完成不能提交新epoch实例 |
| final STA0 completion | 真正的 CMO 最终地址写回，包含异常和 flushPipe | `rm_cmo_completion_actual_t` | cacheable zero 最终写回须晚于真实 SB 成功 |
| flushPipe / flushAfter / redirect | 写回属性 / 框架保留当前项清除年轻项的语义 / 框架恢复事件 | readonly completion / 既有恢复链 | 正常 nonzero 比较 flushPipe1，RM不生成redirect |
| request proof | 实际接口请求、身份与成功事实组成的证明 | `rm_cmo_request_observer_t.requests` | 非零CMO内部请求不能替代真实TL CBO |
| batch / watermark | 同sample事件集合 / 已完整采集的sample边界 | `observe_batch()` / `watermark` | 水位未到completion暂等，水位齐后缺请求失败 |
| slot / generation / attempt | 真实SB位置 / 该位置本次占用代次 / 一次DCache尝试 | `zero_by_slot`、`slot_generation`、请求id | replay结束旧attempt，来源保留给下次请求 |
| token / source | 本次响应唯一标识 / 总线物理编号 | L2历史token、TL A source | 同source复用不能继承旧响应 |
| overlay / entries | RM期望字节表 / 最终逐笔比较记录 | `expected_store_byte` / `expected_store_entries` | 一个zero写64字节，同时产生8个8B记录 |
| SC shadow | 后续确定性写覆盖失败SC待检查字节 | `mark_sc_obligations_shadowed()` | zero覆盖失败SC地址后更新其覆盖证据 |
| backend / NC / MMIO / Uncache | 模拟内存后端 / 普通非缓存Store路径 / 同步设备路径 / 实际非缓存总线 | 统一错误观察器与已验证效果 | 已接受分段后参数denied仍保留该分段写效果 |
| CHECK_STORE / AUTO / DISABLED | 既有最终内存比较控制项 / 控制worker拓扑 / 静态CSR拓扑 | directed builder/vseq | AUTO尾项刷新并比较Store-table；DISABLED保留PBMT配置 |

## 2. 功能特性、修改前后与正确性检查

原核心只有标量访存/原子操作，CMO会落入无数据或标量长度分支，无法表达完整64B zero、最终STA0和真实CMO请求。新实现增加独立CMO类别，仍由程序顺序提交器消费当前UID；权限命令按zero写/nonzero读决定，最终异常通过Store体系比较。

原方案中的重定向讨论已收敛为观察写回属性。核心不修改框架epoch、UID+1或重新发射状态。请求观察器持续采集实际接口，因此RM排到当前CMO之前已发生的真实请求仍可校验；不会根据最终异常反推下游请求或denied/corrupt。

| 检查项 | 源码行为 | 正确性条件 |
|---|---|---|
| 四种最终操作 | 仅STU调用方识别最终fuOp，核心规范化data0/size0 | 其它功能单元重叠数值不能误当CMO |
| normal/fault终态 | 完整动态owner、唯一final STA0、架构异常和正常flushPipe | 缺失、旧epoch、重复完成不能提交 |
| cacheable zero | 实际SQ wline、真实SB请求/成功证明后写64B | 输入16B mask；SB输出64B mask；replay不算完成 |
| Uncache zero | 统一错误观察器给出合法连续分段及后端接受mask | 核心只应用mask；不重新查询或随机错误 |
| 前置异常 | 无请求、无数据效果 | 即使缺PA也可合法消费异常终态 |
| 数据副作用 | 复用现有写底座 | overlay、entries与SC shadow同时更新 |
| 高频边界 | 常数物理资源、当前owner小队列、增量事件消费 | 历史扫描仅reset代次边界和最终drain |

## 3. 字段、真源与生命周期

| 字段组 | 设置者 / 清除者 / 读取者 | 作用与状态含义 |
|---|---|---|
| item.cmo_op/path/permission_write/has_data_effect | 分类与独立翻译 / item重建 / 核心commit | 规范最终语义；过程态，不是DUT数据 |
| cmo_pre_access_exception | 独立权限/翻译 / item重建 / 请求与效果检查 | 前置异常真源；禁止任何下游请求 |
| cmo_requests_validated/count/token/watermark | 请求观察器 / item重建 / commit | 已验证证明；反复轮询不消费历史 |
| cmo_segment_sent/completed/write_mask | 统一Uncache观察器 / item重建 / zero效果 | 请求/响应/实际后端接受是三种不同事实 |
| pending_cmo_actual / last_consumed_cmo_actual | push与合法commit / reset或新owner替换 / 去重 | 当前最终写回快照与上一消费身份 |
| expected_store_byte / entries | 既有commit_memory_effect / model.reset / Load与最终Store比较 | 前者为最后字节值，后者为事务记录；两者互补 |
| requests / consumed / cancelled | 真实请求 / commit或reset / validate与final | 按reset代次+UID+epoch隔离，保留诊断历史 |
| sq_owner_by_value | 真实enqueue / reset或同value替换 / SQ->SB接受 | 64个固定位置，复制到请求后不再依赖active map |
| zero_by_slot / slot_generation / slot_request_* | 真实SB选择与请求/响应 / 成功或reset / attempt校验 | 不重建cache替换策略，只保存实际身份 |
| cmo_waiting / next_request_token | 内部CMO请求 / 真TL A / 对应匹配 | 顺序队列；本地cacheable token单调非零 |

## 4. 核心函数与调用关系

| 顺序 | 函数 | 功能与后续影响 |
|---|---|---|
| 1 | `rm_ls_assign_cmo_data_semantics()` | 冻结最终操作、权限和数据属性，避免读普通Store数据 |
| 2 | `observe_batch()`、`validate()` | 保存真实请求并验证当前动态owner，失败不提交 |
| 3 | `push_cmo_actual()`、`rm_ls_validate_cmo_owner()` | 冻结唯一STA0并拒绝旧实例/重复完成 |
| 4 | `rm_ls_commit_cmo()` | 联合请求、异常、终态和flushPipe决定唯一提交 |
| 5 | `rm_ls_apply_zero_line_effect()` / `rm_ls_apply_uncache_zero_effect()` | 选择完整64B或统一观察器已验证的分段集合 |
| 6 | `rm_ls_apply_uncache_write_effect()` / `rm_ls_apply_zero_segments()` | 将确定性效果转换为既有写底座输入 |
| 7 | `commit_memory_effect()` | 更新SC覆盖证据、字节表和比较记录 |
| 8 | `mark_consumed()` / `check_drained()` | 核心提交后标记请求消费，终场检查孤儿请求 |


### 4.1 最终CMO数据语义

抽象功能描述：调用方在标量长度判断前将最终CMO规范为独立语义；输入item，输出分类成功与规范字段，不生成数据效果。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function automatic bit rm_ls_assign_cmo_data_semantics`。以下源码展示当前实际判断与状态副作用。

```systemverilog
function automatic bit rm_ls_assign_cmo_data_semantics(input rm_ls_program_item_t item);
    rm_cmo_op_e decoded_op;
    if (item == null) return 1'b0;
    decoded_op = rm_ls_decode_cmo_final_op(item.op);
    if (decoded_op == RM_CMO_OP_NONE) return 1'b0;
    item.kind = RM_LS_KIND_CMO;
    item.cmo_op = decoded_op;
    item.cmo_permission_write = rm_ls_cmo_permission_is_write(decoded_op);
    item.cmo_expect_flush_pipe = decoded_op != RM_CMO_OP_ZERO;
    item.cmo_has_data_effect = decoded_op == RM_CMO_OP_ZERO;
    item.size_bytes = 0;
    item.store_data = '0;
    item.store_byte_mask = '0;
    return 1'b1;
endfunction
```
中文伪代码：

1. 功能：调用方在标量长度判断前将最终CMO规范为独立语义；输入item，输出分类成功与规范字段，不生成数据效果。
2. 顺序：空item或未识别操作返回失败；合法时设置CMO类别、操作、读写权限、正常flushPipe与是否写数据，清零标量长度、数据和mask，返回成功。
3. 子函数：rm_ls_decode_cmo_final_op按最终编码给出操作；rm_ls_cmo_permission_is_write仅对zero返回写权限。调用方必须先验证STU类别。


### 4.2 完整动态身份

抽象功能描述：最终写回接入与提交时共同验证owner，保证旧动态实例和重复终态不会污染当前item。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit rm_ls_validate_cmo_owner`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit rm_ls_validate_cmo_owner(input rm_ls_program_item_t item,
                                          input rm_cmo_completion_actual_t actual);
        if (item == null || actual == null || item.kind != RM_LS_KIND_CMO ||
            !actual.valid) begin
            set_error(RM_LS_ERR_CMO_OWNER_MISMATCH, "CMO owner validation requires valid item and actual");
            return 1'b0;
        end
        if (actual.uid == item.uid && actual.dynamic_epoch != item.dynamic_epoch) begin
            set_error(RM_LS_ERR_CMO_STALE_COMPLETION,
                      $sformatf("uid %0d CMO completion epoch %0d expected %0d",
                                item.uid, actual.dynamic_epoch, item.dynamic_epoch));
            return 1'b0;
        end
        if (actual.uid != item.uid || actual.rob != item.rob ||
            actual.sq_flag != item.sq_flag || actual.sq_value != item.sq_value) begin
            set_error(RM_LS_ERR_CMO_OWNER_MISMATCH,
                      $sformatf("uid %0d CMO completion owner mismatch actual_uid=%0d", item.uid, actual.uid));
            return 1'b0;
        end
        if (item.cmo_completion_consumed ||
            (last_consumed_cmo_actual != null &&
             last_consumed_cmo_actual.uid == actual.uid &&
             last_consumed_cmo_actual.rob == actual.rob &&
             last_consumed_cmo_actual.sq_flag == actual.sq_flag &&
             last_consumed_cmo_actual.sq_value == actual.sq_value &&
             last_consumed_cmo_actual.dynamic_epoch == actual.dynamic_epoch)) begin
            set_error(RM_LS_ERR_CMO_DUPLICATE_COMPLETION,
                      $sformatf("uid %0d CMO owner was already consumed", item.uid));
            return 1'b0;
        end
        return 1'b1;
    endfunction
```
中文伪代码：

1. 功能：最终写回接入与提交时共同验证owner，保证旧动态实例和重复终态不会污染当前item。
2. 顺序：先拒绝空对象或非CMO；同UID但epoch不等报旧完成；再核对UID/ROB/SQ；item已消费或上次消费身份相同报重复，均返回失败；剩余返回成功。
3. 子函数：set_error记录专门错误代码及说明，并增加模型错误计数；它不改变框架epoch。


### 4.3 最终完成快照

抽象功能描述：在当前item已登记后冻结唯一实际最终写回；允许同一sample无副作用轮询，拒绝第二笔不同完成。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit push_cmo_actual`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit push_cmo_actual(input rm_cmo_completion_actual_t actual);
        rm_ls_program_item_t item;
        if (!configured || actual == null || !actual.valid ||
            !items.exists(current_uid)) begin
            set_error(RM_LS_ERR_CMO_ACTUAL_NOT_READY, "CMO completion or current item unavailable");
            return 1'b0;
        end
        item = items[current_uid];
        if (!rm_ls_validate_cmo_owner(item, actual)) return 1'b0;
        // 中文注释：readonly新实例到达时，仅丢弃失效本地快照，不修改框架epoch。
        if (pending_cmo_actual != null &&
            (pending_cmo_actual.uid != item.uid || pending_cmo_actual.rob != item.rob ||
             pending_cmo_actual.sq_flag != item.sq_flag || pending_cmo_actual.sq_value != item.sq_value ||
             pending_cmo_actual.dynamic_epoch != item.dynamic_epoch)) pending_cmo_actual = null;
        if (pending_cmo_actual != null) begin
            if (pending_cmo_actual.sample_seq == actual.sample_seq &&
                pending_cmo_actual.exception_vec === actual.exception_vec &&
                pending_cmo_actual.flush_pipe === actual.flush_pipe &&
                pending_cmo_actual.fu_op_type == actual.fu_op_type &&
                pending_cmo_actual.source_sta0 == actual.source_sta0 &&
                pending_cmo_actual.is_final == actual.is_final &&
                pending_cmo_actual.normal_pass == actual.normal_pass &&
                pending_cmo_actual.fault_terminal == actual.fault_terminal)
                return 1'b1;
            set_error(RM_LS_ERR_CMO_DUPLICATE_COMPLETION,
                      $sformatf("uid %0d received a second final CMO completion", item.uid));
            return 1'b0;
        end
        pending_cmo_actual = new();
        pending_cmo_actual.copy_from(actual);
        return 1'b1;
    endfunction
```
中文伪代码：

1. 功能：在当前item已登记后冻结唯一实际最终写回；允许同一sample无副作用轮询，拒绝第二笔不同完成。
2. 顺序：配置/item/actual无效先报缺证据；完整owner失败立即返回；已有本地快照身份过期时仅删除该快照；同身份同sample所有最终属性相同则直接成功；属性或sample改变报重复；没有快照时创建并复制后成功。
3. 子函数：owner验证负责动态身份；copy_from复制值而非共享可变actual，之后外部修改输入不会改已冻结完成。


### 4.4 唯一CMO提交

抽象功能描述：当前程序项提交时联合独立期望、真实请求和最终STA0判定，合法zero应用确定性数据效果，nonzero只消费完成。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit rm_ls_commit_cmo`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit rm_ls_commit_cmo(input rm_ls_program_item_t item,
                                  input logic [23:0] commit_exception);
        rm_ls_history_record_t rec;
        rm_cmo_completion_actual_t actual;
        bit raw_match;
        bit cause_match;
        bit fault_expected;
        bit path_valid;
        rec = new();
        rec.uid = item.uid; rec.kind = item.kind; rec.rob = item.rob;
        rec.vaddr = item.computed_vaddr; rec.first_pa = item.pa_by_byte[0];
        rec.expected_exception = item.expected_exception; rec.actual_exception = commit_exception;
        if (pending_cmo_actual == null) begin
            set_error(RM_LS_ERR_CMO_ACTUAL_NOT_READY,
                      $sformatf("uid %0d CMO final STA0 completion unavailable", item.uid));
            return 1'b0;
        end
        actual = pending_cmo_actual;
        if (!rm_ls_validate_cmo_owner(item, actual)) return 1'b0;
        path_valid = (item.cmo_pre_access_exception != '0 && item.cmo_path == RM_CMO_PATH_NONE) ||
                     (item.cmo_op == RM_CMO_OP_ZERO &&
                      item.cmo_path inside {RM_CMO_PATH_CACHEABLE_ZERO, RM_CMO_PATH_UNCACHE_ZERO}) ||
                     (item.cmo_op inside {RM_CMO_OP_CLEAN, RM_CMO_OP_FLUSH, RM_CMO_OP_INVAL} &&
                      item.cmo_path == RM_CMO_PATH_DCACHE_CMO);
        if (!(item.cmo_op inside {RM_CMO_OP_ZERO, RM_CMO_OP_CLEAN,
                                  RM_CMO_OP_FLUSH, RM_CMO_OP_INVAL}) ||
            item.cmo_op != rm_ls_decode_cmo_final_op(item.op) || !path_valid ||
            item.cmo_permission_write != rm_ls_cmo_permission_is_write(item.cmo_op) ||
            item.cmo_expect_flush_pipe != (item.cmo_op != RM_CMO_OP_ZERO) ||
            item.cmo_has_data_effect != (item.cmo_op == RM_CMO_OP_ZERO) ||
            item.store_data != '0 || item.size_bytes != 0 ||
            (item.cmo_pre_access_exception != '0 &&
             item.expected_exception != item.cmo_pre_access_exception)) begin
            rec.error_code = RM_LS_ERR_CMO_CLASSIFICATION;
            rec.detail = $sformatf("uid %0d invalid CMO final operation/data/permission/path", item.uid);
        end else if (!actual.source_sta0 || !actual.is_final || actual.sample_seq == 0 ||
                     actual.fu_op_type != item.op) begin
            rec.error_code = RM_LS_ERR_CMO_COMPLETION_MISMATCH;
            rec.detail = $sformatf("uid %0d lacks final owner-bound STA0 CMO result", item.uid);
        end else if (!item.cmo_requests_validated ||
                     item.cmo_request_watermark < actual.sample_seq ||
                     (item.cmo_pre_access_exception != '0 && item.cmo_request_count != 0) ||
                     (item.cmo_pre_access_exception == '0 &&
                      (item.cmo_request_count == 0 || item.cmo_request_token == 0))) begin
            rec.error_code = RM_LS_ERR_CMO_REQUEST_MISMATCH;
            rec.detail = $sformatf("uid %0d CMO real request proof missing or inconsistent", item.uid);
        end else if (!compare_arch_exception(item.expected_exception, actual.exception_vec,
                                             1'b1, raw_match, cause_match) ||
                     !compare_arch_exception(item.expected_exception, commit_exception,
                                             1'b1, raw_match, cause_match)) begin
            rec.error_code = RM_LS_ERR_EXCEPTION_MISMATCH;
            rec.detail = $sformatf("uid %0d CMO exception expected=0x%0h STA0=0x%0h terminal=0x%0h",
                                   item.uid, item.expected_exception, actual.exception_vec, commit_exception);
        end else begin
            fault_expected = item.expected_exception != '0;
            if (actual.normal_pass == actual.fault_terminal ||
                actual.fault_terminal != fault_expected ||
                (!fault_expected && actual.flush_pipe !== item.cmo_expect_flush_pipe)) begin
                rec.error_code = RM_LS_ERR_CMO_COMPLETION_MISMATCH;
                rec.detail = $sformatf("uid %0d CMO normal/fault or flushPipe mismatch", item.uid);
            end else if (item.cmo_pre_access_exception != '0) begin
                if (item.cmo_segment_sent_mask != '0 || item.cmo_segment_write_mask != '0 ||
                    item.cmo_segment_completed_mask != '0) begin
                    rec.error_code = RM_LS_ERR_CMO_EFFECT_MISMATCH;
                    rec.detail = "pre-access CMO fault cannot carry data requests/effects";
                end
            end else if (item.cmo_path == RM_CMO_PATH_CACHEABLE_ZERO) begin
                if (fault_expected || item.cmo_request_count != 1 ||
                    !rm_ls_apply_zero_line_effect(item)) begin
                    rec.error_code = RM_LS_ERR_CMO_EFFECT_MISMATCH;
                    rec.detail = "cacheable zero lacks one successful whole-line request";
                end
            end else if (item.cmo_path == RM_CMO_PATH_UNCACHE_ZERO) begin
                if (item.cmo_request_count > 8 ||
                    item.cmo_request_count != $countones(item.cmo_segment_sent_mask) ||
                    !rm_ls_apply_uncache_zero_effect(item)) begin
                    rec.error_code = RM_LS_ERR_CMO_EFFECT_MISMATCH;
                    rec.detail = "Uncache zero lacks verified deterministic segment effects";
                end
            end else if (item.cmo_request_count != 1) begin
                rec.error_code = RM_LS_ERR_CMO_REQUEST_MISMATCH;
                rec.detail = "non-zero CMO requires one verified request chain";
            end
        end
        if (rec.error_code != RM_LS_ERR_NONE) begin
            set_error(rec.error_code, rec.detail);
            history.push_back(rec);
            return 1'b0;
        end
        item.cmo_completion_consumed = 1'b1;
        last_consumed_cmo_actual = new();
        last_consumed_cmo_actual.copy_from(actual);
        pending_cmo_actual = null;
        rec.compare_pass = 1'b1;
        rec.detail = item.expected_exception == '0 ? "normal CMO match" : "fault CMO match with verified effects";
        history.push_back(rec);
        return 1'b1;
    endfunction
```
中文伪代码：

1. 功能：当前程序项提交时联合独立期望、真实请求和最终STA0判定，合法zero应用确定性数据效果，nonzero只消费完成。
2. 顺序：先建历史记录并要求pending最终写回；完整owner匹配后验证操作/路径/权限/规范数据；验证来源STA0、最终标识和非零sample；要求请求证明和覆盖水位；比较实际STA与终态架构异常；normal和fault必须互斥且符合独立异常期望；正常flushPipe须匹配。前置fault拒绝分段效果；cacheable zero要求无fault且唯一请求并应用整行；Uncache zero检查段数与mask后应用统一效果；nonzero要求唯一请求。任一错误记录并返回；成功标item已消费，复制上次完成身份，清pending并保存通过历史。
3. 子函数：compare_arch_exception按架构cause规则比较；zero效果入口只消费独立证明；set_error和history保存诊断。核心不生成redirect，不自行创建epoch。


### 4.5 八字节子效果

抽象功能描述：将已验证的zero分段集合在一次无等待函数执行中落到现有写底座，保留其它写操作已有副作用。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit rm_ls_apply_zero_segments`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit rm_ls_apply_zero_segments(input rm_ls_program_item_t item,
                                           input bit [7:0] write_mask);
        rm_ls_program_item_t children[8];
        longint unsigned line_base;
        if (item == null || !item.valid || item.kind != RM_LS_KIND_CMO ||
            item.cmo_op != RM_CMO_OP_ZERO || !item.cmo_has_data_effect ||
            !item.translation_valid || !item.pa_valid_mask[0] ||
            item.cmo_effect_applied || item.cmo_completion_consumed) begin
            set_error(RM_LS_ERR_CMO_EFFECT_MISMATCH, "invalid or duplicate CMO zero effect");
            return 1'b0;
        end
        line_base = item.pa_by_byte[0] & ~64'h3f;
        if (|item.pa_by_byte[0][63:48] || line_base > 64'h0000_ffff_ffff_ffc0) begin
            set_error(RM_LS_ERR_BARE_PA_OUT_OF_RANGE, "CMO zero line is outside 48-bit PA range");
            return 1'b0;
        end
        // 中文注释：先构造并校验整组；之后无yield调用同一底座，不会留下半行RM副作用。
        for (int unsigned segment = 0; segment < 8; segment++) begin
            if (!write_mask[segment]) continue;
            children[segment] = new(item.uid);
            children[segment].valid = 1'b1;
            children[segment].kind = RM_LS_KIND_CMO;
            children[segment].op = item.op;
            children[segment].rob = item.rob;
            children[segment].sq_flag = item.sq_flag;
            children[segment].sq_value = item.sq_value;
            children[segment].dynamic_epoch = item.dynamic_epoch;
            children[segment].cmo_op = RM_CMO_OP_ZERO;
            children[segment].computed_vaddr = (item.computed_vaddr & ~64'h3f) + segment * 8;
            children[segment].main_vaddr = children[segment].computed_vaddr;
            children[segment].translation_valid = 1'b1;
            children[segment].size_bytes = 8;
            children[segment].pa_valid_mask = 16'h00ff;
            children[segment].store_data = '0;
            children[segment].store_byte_mask = 16'h00ff;
            for (int unsigned byte_index = 0; byte_index < 8; byte_index++)
                children[segment].pa_by_byte[byte_index] = line_base + longint'(segment) * 8 + longint'(byte_index);
        end
        for (int unsigned segment = 0; segment < 8; segment++) begin
            if (!write_mask[segment]) continue;
            if (!commit_memory_effect(children[segment], RM_MEMORY_EFFECT_CMO_ZERO,
                                      128'h0, 16'h00ff)) return 1'b0;
        end
        item.cmo_effect_applied = 1'b1;
        return 1'b1;
    endfunction
```
中文伪代码：

1. 功能：将已验证的zero分段集合在一次无等待函数执行中落到现有写底座，保留其它写操作已有副作用。
2. 顺序：拒绝无效/重复/缺PA或错误类别；对齐64B行并检查48位物理地址范围；先构造选中各段的独立8B子item，继承owner并设置8个连续PA和全mask；全部构造后再逐段调用写底座，失败立即返回；全成功后标记效果已应用。
3. 子函数：commit_memory_effect用既有SC覆盖、字节表及记录统一提交。整组几何预检查保证正常cacheable zero不会留下因地址mask失败而产生的半行RM效果。


### 4.6 同步Uncache效果

抽象功能描述：让Uncache zero和同步MMIO Store复用同一确定性写入口；资格来自统一观察器，写值仍由RM独立提供。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit rm_ls_apply_uncache_write_effect`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit rm_ls_apply_uncache_write_effect(input rm_ls_program_item_t item);
        bit [15:0] required_mask;
        if (item == null || !item.valid || !item.uncache_path) begin
            set_error(RM_LS_ERR_CMO_EFFECT_MISMATCH, "Uncache write effect requires valid path/item");
            return 1'b0;
        end
        if (item.kind == RM_LS_KIND_CMO) begin
            if (item.cmo_path != RM_CMO_PATH_UNCACHE_ZERO || !item.cmo_effect_valid) begin
                set_error(RM_LS_ERR_CMO_EFFECT_MISMATCH, "Uncache CMO effect lacks verified segment set");
                return 1'b0;
            end
            return rm_ls_apply_zero_segments(item, item.cmo_segment_write_mask);
        end
        if (item.kind != RM_LS_KIND_STORE || !item.uncache_effect_valid ||
            item.uncache_effect_applied) begin
            set_error(RM_LS_ERR_CMO_EFFECT_MISMATCH, "missing or duplicate synchronous Uncache Store effect");
            return 1'b0;
        end
        // 中文注释：拒绝写即为空集合，不要求不存在的数据PA；外层仍已比较STA/terminal异常。
        if (!item.uncache_write_accepted) begin
            item.uncache_effect_applied = 1'b1;
            return 1'b1;
        end
        if (!item.translation_valid || item.size_bytes == 0 || item.size_bytes > 8) begin
            set_error(RM_LS_ERR_TRANSLATION_NOT_READY, "MMIO write effect geometry unavailable");
            return 1'b0;
        end
        required_mask = 16'hffff >> (16 - item.size_bytes);
        if (item.store_byte_mask != required_mask ||
            (item.pa_valid_mask & required_mask) != required_mask) begin
            set_error(RM_LS_ERR_STORE_MASK_MISMATCH, "MMIO accepted effect mask/PA mismatch");
            return 1'b0;
        end
        if (!commit_memory_effect(item, RM_MEMORY_EFFECT_STORE,
                                  item.store_data, item.store_byte_mask)) return 1'b0;
        item.uncache_effect_applied = 1'b1;
        return 1'b1;
    endfunction
```
中文伪代码：

1. 功能：让Uncache zero和同步MMIO Store复用同一确定性写入口；资格来自统一观察器，写值仍由RM独立提供。
2. 顺序：先校验有效Uncache item；CMO必须是已验证Uncache zero并用write_mask写段；普通Store必须具备唯一未应用效果；后端拒绝为空效果并标已应用；接受写须有合法翻译、1至8字节、完整mask和PA；写底座成功后标已应用。
3. 子函数：rm_ls_apply_zero_segments按已接受段写零；commit_memory_effect按独立Store数据更新所有既有副作用，不从actual数据构造预期。


### 4.7 cacheable整行效果

抽象功能描述：仅正常cacheable zero可以申请全行零写。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit rm_ls_apply_zero_line_effect`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit rm_ls_apply_zero_line_effect(input rm_ls_program_item_t item);
        if (item == null || item.cmo_path != RM_CMO_PATH_CACHEABLE_ZERO ||
            item.expected_exception != '0 || item.cmo_pre_access_exception != '0) begin
            set_error(RM_LS_ERR_CMO_EFFECT_MISMATCH, "normal cacheable zero effect requires clean terminal");
            return 1'b0;
        end
        return rm_ls_apply_zero_segments(item, 8'hff);
    endfunction
```
中文伪代码：

1. 功能：仅正常cacheable zero可以申请全行零写。
2. 顺序：路径不是cacheable zero，或任一预期/前置异常非零则报错；否则以8段全选mask进入统一分段写底座。
3. 子函数：rm_ls_apply_zero_segments负责行对齐、子item构造和无等待写提交。


### 4.8 Uncache分段效果约束

抽象功能描述：提交统一错误观察器已经确认的分段集合，阻止将fault误当整行无写或整行全写。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit rm_ls_apply_uncache_zero_effect`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit rm_ls_apply_uncache_zero_effect(input rm_ls_program_item_t item);
        if (item == null || item.cmo_path != RM_CMO_PATH_UNCACHE_ZERO ||
            !item.cmo_effect_valid || item.cmo_pre_access_exception != '0 ||
            ((item.cmo_segment_write_mask & ~item.cmo_segment_sent_mask) != '0) ||
            item.cmo_segment_completed_mask != item.cmo_segment_sent_mask ||
            (item.expected_exception == '0 &&
             (item.cmo_segment_sent_mask != 8'hff || item.cmo_segment_write_mask != 8'hff))) begin
            set_error(RM_LS_ERR_CMO_EFFECT_MISMATCH, "Uncache zero effect lacks verified segment evidence");
            return 1'b0;
        end
        return rm_ls_apply_uncache_write_effect(item);
    endfunction
```
中文伪代码：

1. 功能：提交统一错误观察器已经确认的分段集合，阻止将fault误当整行无写或整行全写。
2. 顺序：检查路径/已验证资格/无前置异常；写集合必须属于已发送集合，已完成集合须等于已发送集合；正常完成必须八段全部已发送且实际接受写；失败报效果错配，成功进入统一Uncache效果。
3. 子函数：rm_ls_apply_uncache_write_effect按确定性集合应用数据；本函数不另建错误表、不随机denied/corrupt。


### 4.9 既有写底座

抽象功能描述：所有确定性写统一维护RM数据真源，新增CMO调用复用该既有底座。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`，函数：`function bit commit_memory_effect`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit commit_memory_effect(rm_ls_program_item_t item,
                                      rm_memory_effect_kind_e source_kind,
                                      bit [127:0] data,
                                      bit [15:0] byte_mask);
        rm_ls_store_cache_entry_t effect_entry;
        bit [15:0] required_mask;

        if (item == null || !item.translation_valid || item.size_bytes == 0 ||
            item.size_bytes > 16) return 1'b0;
        required_mask = 16'hffff >> (16 - item.size_bytes);
        if (byte_mask != required_mask ||
            (item.pa_valid_mask & required_mask) != required_mask) begin
            set_error(RM_LS_ERR_ATOMIC_MEMORY_EFFECT_MISMATCH,
                      $sformatf("uid %0d invalid memory effect mask pa=0x%0h data=0x%0h required=0x%0h",
                                item.uid, item.pa_valid_mask, byte_mask, required_mask));
            return 1'b0;
        end
        mark_sc_obligations_shadowed(item, byte_mask);
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            expected_store_valid[item.pa_by_byte[i]] = 1'b1;
            expected_store_byte[item.pa_by_byte[i]] = data[(i * 8) +: 8];
        end
        effect_entry = new();
        effect_entry.capture_effect(item, source_kind, data, byte_mask);
        expected_store_entries.push_back(effect_entry);
        return 1'b1;
    endfunction
```
中文伪代码：

1. 功能：所有确定性写统一维护RM数据真源，新增CMO调用复用该既有底座。
2. 顺序：先检查翻译和1至16字节；验证PA和写mask完整；标记失败SC被后续写覆盖；逐字节写期望有效位和数据；创建并捕获事务记录，追加到最终Store比较队列。
3. 子函数：mark_sc_obligations_shadowed保存先前失败SC的覆盖证据；capture_effect新增接受CMO类别，使8B子效果成为合法记录。


## 5. 真实请求观察器

修改前没有CMO真实请求账，最终写回可能掩盖下游路径错误。新增观察器只保存实际fire及必要的有限流水身份，不维护DCache命中/替换算法。


### 5.1 身份来源

抽象功能描述：在请求接受边界按真实ROB或SQ冻结唯一动态owner，避免靠地址猜测以及提交后active map释放。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`virtual function bit resolve_owner`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    virtual function bit resolve_owner(input cmo_path_probe_t event_i, output owner_t owner);
        memblock_rm_readonly_api ro;
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_t uid;
        int match_count;
        owner='0; ro=memblock_rm_readonly_api::get(); match_count=0;
        if(event_i.owner_valid) begin
            if(!ro.try_read_uid_by_rob_for_rm(
                '{flag:event_i.rob[8],value:event_i.rob[7:0]},uid) ||
               !ro.read_main_transaction_for_rm(uid,main_v) ||
               !ro.read_status_for_rm(uid,status_v)) return 0;
            owner='{valid:1,uid:uid,epoch:status_v.dynamic_epoch,rob:event_i.rob,
                    sq:{main_v.sq_idx_flag,main_v.sq_idx_value},op:main_v.fu_op_type,
                    cmo_class:main_v.fu_type==MEMBLOCK_FUTYPE_STU &&
                              rm_ls_decode_cmo_final_op(main_v.fu_op_type)!=RM_CMO_OP_NONE};
            return main_v.valid && status_v.valid && !status_v.flushed &&
                   {main_v.rob_idx_flag,main_v.rob_idx_value}==event_i.rob;
        end
        // 优先使用已冻结的实际enqueue；只有基线首拍尚无镜像才做两key有界只读查找。
        if(sq_owner_by_value[event_i.sq[5:0]].valid) begin
            owner=sq_owner_by_value[event_i.sq[5:0]];
            return 1;
        end
        // DataBuffer优化掉flag；常数两次key查询，无主表扫描。
        for(int flag_i=0;flag_i<2;flag_i++) begin
            if(ro.try_read_uid_by_sq_for_rm(
                '{flag:1'(flag_i),value:event_i.sq[5:0]},uid) &&
               ro.read_main_transaction_for_rm(uid,main_v) &&
               ro.read_status_for_rm(uid,status_v) &&
               status_v.active_sq_mapped && !status_v.flushed &&
               main_v.sq_idx_value==event_i.sq[5:0]) begin
                match_count++;
                owner='{valid:1,uid:uid,epoch:status_v.dynamic_epoch,
                        rob:{main_v.rob_idx_flag,main_v.rob_idx_value},
                        sq:{main_v.sq_idx_flag,main_v.sq_idx_value},op:main_v.fu_op_type,
                        cmo_class:main_v.fu_type==MEMBLOCK_FUTYPE_STU &&
                                  rm_ls_decode_cmo_final_op(main_v.fu_op_type)!=RM_CMO_OP_NONE};
            end
        end
        return match_count==1;
    endfunction
```
中文伪代码：

1. 功能：在请求接受边界按真实ROB或SQ冻结唯一动态owner，避免靠地址猜测以及提交后active map释放。
2. 顺序：存在实际ROB时通过readonly反查UID，读取主表与状态，构造完整owner并验证有效/未flush/ROB；否则优先读取真实enqueue冻结的当前SQ value身份；尚无镜像才对两种flag做常数次查找，只接受唯一合法owner。
3. 子函数：readonly函数只读框架主表/状态/映射；最终op仅在STU类别时识别CMO，避免MOU同编码碰撞。


### 5.2 请求建账

抽象功能描述：为当前动态owner登记一次真实SQ接受，重复请求与身份变化立即报告。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`function void note_request`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function void note_request(input accepted_t accepted);
        request_state_t state_v;
        owner_key_t k;
        k=key(accepted.owner);
        if(requests.exists(k)) state_v=requests[k];
        else begin
            state_v.owner=accepted.owner; state_v.sq_count=0; state_v.tl_count=0;
            state_v.first_sample=accepted.sample_seq; state_v.success_sample=0;
            state_v.token=next_request_token++; state_v.token_accept_sample=accepted.sample_seq;
            state_v.line={accepted.request.addr[63:6],6'b0};
            state_v.failed=0; state_v.consumed=0; state_v.cancelled=0; state_v.error="";
        end
        state_v.sq_count++;
        if(state_v.sq_count!=1 || state_v.owner!==accepted.owner) begin
            state_v.failed=1; state_v.error="CMO请求重复或owner变化";
            fail($sformatf("uid=%0d %s",accepted.owner.uid,state_v.error));
        end
        requests[k]=state_v;
    endfunction
```
中文伪代码：

1. 功能：为当前动态owner登记一次真实SQ接受，重复请求与身份变化立即报告。
2. 顺序：按reset代次+UID+epoch建立key；已有状态则取回，否则初始化计数/时间/行地址/非零单调token与消费标志；SQ请求计数增加；不为一或完整owner变化即标failed并报告；最后写回状态。
3. 子函数：key提供复位隔离；fail既增加局部错误计数，也向UVM报告，不吞掉负向事件。


### 5.3 实际事件关联主流程

抽象功能描述：唯一monitor消费者按真实因果链维护SQ到SB、SB到DCache以及内部CMO到TL CBO的请求证明。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`function void observe_batch`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function void observe_batch(input cmo_path_agent_xaction tr);
        accepted_t next_sb[2];
        accepted_t accepted;
        request_state_t state_v;
        owner_key_t k;
        longint unsigned token;
        bit [15:0] allocated_slots;
        if(tr==null) return;
        if(tr.reset_active) begin
            foreach(sb_input[i]) sb_input[i]='0;
            foreach(sq_owner_by_value[i]) sq_owner_by_value[i]='0;
            foreach(zero_by_slot[i]) zero_by_slot[i].delete();
            foreach(slot_request_valid[i]) slot_request_valid[i]=0;
            // 历史取消只在reset代次边界执行一次，长reset不逐拍扫描累计账本。
            if(reset_generation!=tr.reset_generation || watermark==0) begin
                foreach(requests[old_key]) begin
                    state_v=requests[old_key];
                    if(!state_v.consumed) begin state_v.cancelled=1; requests[old_key]=state_v; end
                end
            end
            cmo_waiting.delete(); reset_generation=tr.reset_generation;
            watermark=tr.sample_seq; return;
        end
        if(tr.reset_generation!=reset_generation && watermark!=0) begin
            fail("CMO monitor reset代际变化但未观察reset批次"); return;
        end
        if(tr.sample_seq<=watermark) begin
            fail("CMO monitor批次sample重复或倒退"); return;
        end
        foreach(next_sb[i]) next_sb[i]='0;
        allocated_slots='0;
        // accessIdx来自上一拍。先关联旧SQ接受，再采本拍SQ输入。
        foreach(tr.events[i]) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if(e.kind==CMO_PATH_SB_ACCESS && e.port_id<2 && e.slot<16) begin
                accepted=sb_input[e.port_id];
                if(!accepted.valid || accepted.sample_seq+1!=tr.sample_seq) begin
                    fail("SB accessIdx没有上一拍SQ接受事实"); continue;
                end
                // 双端同tag的新插入可指向同slot，canMerge均为0；同批只分配一次generation。
                if(!accepted.request.qual[2] && !allocated_slots[e.slot]) begin
                    if(zero_by_slot[e.slot].size()!=0 || slot_request_valid[e.slot])
                        fail("SB新slot覆盖了尚未完成zero来源或传输");
                    slot_generation[e.slot]++;
                    zero_by_slot[e.slot].delete();
                    slot_request_valid[e.slot]=0;
                    allocated_slots[e.slot]=1'b1;
                end
                if(accepted.request.qual[0] && accepted.request.qual[1])
                    zero_by_slot[e.slot].push_back(key(accepted.owner));
            end
        end
        // 同拍SQ内部接受先于TL CBO匹配，结果不依赖producer向events入队的顺序。
        for(int phase=0;phase<2;phase++) begin
        foreach(tr.events[i]) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if((e.kind==CMO_PATH_DCACHE_TL_A)!=(phase==1)) continue;
            case(e.kind)
                CMO_PATH_SQ_SBUFFER: begin
                    accepted='0; accepted.valid=1; accepted.request=e;
                    accepted.sample_seq=tr.sample_seq;
                    // 所有真实SQ->SB接受都解析owner：即使DUT错误地漏发wline，
                    // 也必须记录CMO走过该路径，不能让Uncache/front-fault的禁止请求检查漏检。
                    if(resolve_owner(e,accepted.owner) && is_cmo(accepted.owner)) begin
                        note_request(accepted);
                        if(rm_ls_decode_cmo_final_op(accepted.owner.op)!=RM_CMO_OP_ZERO ||
                           !e.qual[0] || !e.qual[1] || e.data!==0 ||
                           e.mask!==64'h0000_0000_0000_ffff) begin
                            k=key(accepted.owner); state_v=requests[k];
                            state_v.failed=1; state_v.error="CMO SQ->SB类型/wline/vecValid/零数据/输入mask错误";
                            requests[k]=state_v; fail(state_v.error);
                        end
                    end else if(e.qual[0]) begin
                        fail("SQ wline没有唯一cbo.zero owner"); continue;
                    end
                    if(e.port_id<2) next_sb[e.port_id]=accepted;
                end
                CMO_PATH_SQ_CMO_REQUEST: begin
                    accepted='0; accepted.valid=1; accepted.request=e;
                    accepted.sample_seq=tr.sample_seq;
                    if(!resolve_owner(e,accepted.owner) || !is_cmo(accepted.owner) ||
                       rm_ls_decode_cmo_final_op(accepted.owner.op)==RM_CMO_OP_ZERO) begin
                        fail("SQ CMO请求缺少唯一nonzero owner"); continue;
                    end
                    note_request(accepted);
                    if(e.op!=accepted.owner.op || e.addr[5:0]!=0 ||
                       e.id[1:0]!=accepted.owner.op[1:0]) begin
                        k=key(accepted.owner); state_v=requests[k];
                        state_v.failed=1; state_v.error="内部CMO opcode/line地址错误";
                        requests[k]=state_v; fail(state_v.error);
                    end
                    cmo_waiting.push_back(accepted);
                end
                CMO_PATH_SB_DCACHE_REQUEST: begin
                    if(e.slot>=16) begin fail("SB请求slot越界"); continue; end
                    if(slot_request_valid[e.slot])
                        fail("SB上一attempt尚未成功或replay便重复发射");
                    slot_request_valid[e.slot]=1; slot_request_id[e.slot]=e.id;
                    slot_request_generation[e.slot]=slot_generation[e.slot];
                    foreach(zero_by_slot[e.slot][j]) begin
                        k=zero_by_slot[e.slot][j]; state_v=requests[k];
                        if(e.addr[63:6]!=state_v.line[63:6] || e.addr[5:0]!=0 ||
                           e.mask!==64'hffff_ffff_ffff_ffff) begin
                            state_v.failed=1; state_v.error="zero SB请求物理行或整行mask错误";
                            fail(state_v.error);
                        end
                        state_v.tl_count++; requests[k]=state_v;
                    end
                end
                CMO_PATH_SB_DCACHE_SUCCESS,CMO_PATH_SB_DCACHE_REPLAY: begin
                    if(e.slot>=16) begin fail("SB响应slot越界"); continue; end
                    if(!slot_request_valid[e.slot] || slot_request_id[e.slot]!=e.id ||
                       slot_request_generation[e.slot]!=slot_generation[e.slot]) begin
                        fail("SB成功/replay没有匹配的真实请求attempt"); continue;
                    end
                    slot_request_valid[e.slot]=0;
                    if(e.kind==CMO_PATH_SB_DCACHE_SUCCESS) begin
                        foreach(zero_by_slot[e.slot][j]) begin
                            k=zero_by_slot[e.slot][j]; state_v=requests[k];
                            state_v.success_sample=tr.sample_seq; requests[k]=state_v;
                        end
                        zero_by_slot[e.slot].delete();
                    end
                end
                CMO_PATH_DCACHE_TL_A: if(e.op inside {9'd12,9'd13,9'd14}) begin
                    bit [8:0] expected_op;
                    if(cmo_waiting.size()==0) begin fail("TL CBO A缺少SQ CMO请求"); continue; end
                    accepted=cmo_waiting.pop_front(); k=key(accepted.owner); state_v=requests[k];
                    expected_op=rm_ls_decode_cmo_final_op(accepted.owner.op)==RM_CMO_OP_CLEAN ? 12 :
                                rm_ls_decode_cmo_final_op(accepted.owner.op)==RM_CMO_OP_FLUSH ? 13 : 14;
                    if(e.op!=expected_op || e.addr[63:6]!=state_v.line[63:6] ||
                       e.addr[5:0]!=0 || e.state[2:0]!=6 || tr.sample_seq<accepted.sample_seq) begin
                        state_v.failed=1; state_v.error="TL CBO opcode/line/size/顺序错误";
                        fail(state_v.error);
                    end
                    if(!mem_access_base_sequence::find_l2_response_at_accept(
                        tr.sample_seq,10'(e.id),0,token)) begin
                        state_v.failed=1; state_v.error="TL CBO A缺少独立响应token";
                        fail(state_v.error);
                    end else begin
                        mem_access_base_sequence::l2_response_owner_t bound_owner;
                        bound_owner='{valid:1,uid:accepted.owner.uid,
                            dynamic_epoch:accepted.owner.epoch,rob_flag:accepted.owner.rob[8],
                            rob_value:accepted.owner.rob[7:0],sq_flag:accepted.owner.sq[6],
                            sq_value:accepted.owner.sq[5:0]};
                        if(!mem_access_base_sequence::bind_l2_response_owner(token,tr.sample_seq,bound_owner))
                            fail("TL CBO响应token owner绑定失败");
                    end
                    state_v.token=token; state_v.token_accept_sample=tr.sample_seq;
                    state_v.tl_count++; requests[k]=state_v;
                end
                default: ;
            endcase
        end
        end
        // 先消费本拍旧SQ请求，再冻结本拍新enqueue；满队列同slot出队/复用时不能覆盖旧owner。
        foreach(tr.events[i]) begin
            cmo_path_probe_t e;
            owner_t enqueue_owner;
            e=tr.events[i];
            if(e.kind!=CMO_PATH_SQ_ENQUEUE) continue;
            if(!resolve_owner(e,enqueue_owner) || enqueue_owner.sq!=e.sq ||
               enqueue_owner.op!=e.op) begin
                fail("SQ enqueue身份与只读owner不匹配"); continue;
            end
            sq_owner_by_value[e.sq[5:0]]=enqueue_owner;
        end
        foreach(sb_input[i]) sb_input[i]=next_sb[i];
        watermark=tr.sample_seq;
    endfunction
```
中文伪代码：

1. 功能：唯一monitor消费者按真实因果链维护SQ到SB、SB到DCache以及内部CMO到TL CBO的请求证明。
2. 顺序：空输入直接返回。reset清有限流水队列，仅在reset代次变化时取消未消费历史并前进水位；正常sample重复/倒退或缺reset边界报错。先按上一拍SQ接受与本拍accessIdx关联SB slot；同批双端新插入共享slot只分配一次generation，已未完成来源被覆盖时报错。分两阶段处理事件，先SQ/SB再TL A，消除同拍producer发布顺序差异。SQ->SB解析所有owner，识别CMO即建账并检查zero/wline/vecValid/16B零输入；nonzero内部请求核对最终op/对齐行后进入等待队列。SB请求保存attempt及generation、验证每个zero来源行与64Bmask；响应检查attempt，replay只结束attempt，success标完成并清slot来源。TL CBO取最早内部owner，核对opcode/line/size6和时序，并取独立L2响应token绑定owner。所有旧请求处理后才更新本拍真实enqueue快照，再替换上一拍SB输入并推进水位。
3. 子函数：resolve_owner只冻结身份；note_request创建当前owner账；find_l2_response_at_accept按实际sample/source查框架独立响应；bind_l2_response_owner绑定身份不改数据/错误。所有运行期遍历限固定事件批次/16slot及其活跃来源；累计历史只在reset代次边界处理。


### 5.4 提交前证明

抽象功能描述：在最终CMO写回被只读观察后验证当前owner真实请求集合，反复调用不消费请求。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`function bit validate(`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit validate(input rm_ls_program_item_t item,
                          input longint unsigned completion_sample, output string detail);
        owner_key_t k;
        request_state_t state_v;
        detail=""; pending_evidence=0;
        if(item==null) begin detail="CMO请求检查缺少item"; return 0; end
        k={reset_generation,32'(item.uid),32'(item.dynamic_epoch)};
        if(watermark<completion_sample) begin
            pending_evidence=1; detail="CMO请求水位尚未覆盖completion"; return 0;
        end
        if(item.cmo_pre_access_exception!=0) begin
            if(!validate_no_requests(item,completion_sample,detail)) return 0;
            item.cmo_requests_validated=1; item.cmo_request_watermark=watermark; return 1;
        end
        if(!requests.exists(k)) begin detail="CMO缺少实际SQ请求"; return 0; end
        state_v=requests[k];
        if(state_v.failed || state_v.cancelled || state_v.consumed ||
           state_v.sq_count!=1 || state_v.first_sample>completion_sample ||
           state_v.owner.op!=item.op || state_v.owner.rob!={item.rob.flag,8'(item.rob.value)} ||
           |item.rob.value[15:8] ||
           state_v.owner.sq!={item.sq_flag,item.sq_value} ||
           state_v.line[63:6]!=item.pa_by_byte[0][63:6]) begin
            detail=$sformatf("CMO请求身份/计数/地址失败：%s",state_v.error); return 0;
        end
        if(item.cmo_path==RM_CMO_PATH_CACHEABLE_ZERO) begin
            if(state_v.tl_count==0 || state_v.success_sample==0 || state_v.success_sample>completion_sample) begin
                detail="cacheable zero最终STA0之前缺少实际SB请求及成功（replay不算完成）"; return 0;
            end
        end else if(item.cmo_path==RM_CMO_PATH_DCACHE_CMO) begin
            if(state_v.tl_count!=1 || state_v.token==0 || state_v.token_accept_sample>completion_sample) begin
                detail="nonzero CMO缺少completion之前唯一TL CBO请求"; return 0;
            end
        end else begin detail="Uncache zero须调用统一分段验证入口"; return 0; end
        item.cmo_requests_validated=1; item.cmo_request_count=state_v.sq_count;
        item.cmo_request_token=state_v.token; item.cmo_request_watermark=watermark;
        return 1;
    endfunction
```
中文伪代码：

1. 功能：在最终CMO写回被只读观察后验证当前owner真实请求集合，反复调用不消费请求。
2. 顺序：先清pending并检查item；水位未覆盖最终写回返回暂等；前置fault调用禁止请求校验后标证明有效。没有请求则失败；读取当前owner状态，拒绝failed/cancelled/consumed、计数/时间/完整身份/物理行不符。cacheable zero要求真实SB请求且success不晚于最终STA0；nonzero要求唯一TL请求、非零token且接受不晚于完成；Uncache zero须走统一分段入口。通过时填请求证明字段。
3. 子函数：validate_no_requests拒绝该动态owner的任意cacheable请求账；此函数不从final异常反推是否应有请求。


### 5.5 禁止路径证明

抽象功能描述：前置异常与Uncache zero提交时排除同owner非法cacheable请求。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`function bit validate_no_requests`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit validate_no_requests(input rm_ls_program_item_t item,
                                      input longint unsigned completion_sample,
                                      output string detail);
        owner_key_t k;
        detail=""; pending_evidence=0;
        if(item==null) begin detail="CMO禁止请求检查缺少item"; return 0; end
        if(watermark<completion_sample) begin
            pending_evidence=1; detail="CMO请求水位尚未覆盖completion"; return 0;
        end
        k={reset_generation,32'(item.uid),32'(item.dynamic_epoch)};
        if(requests.exists(k)) begin
            detail="CMO在禁止的cacheable SQ/SB/CMO路径产生了请求";
            return 0;
        end
        return 1;
    endfunction
```
中文伪代码：

1. 功能：前置异常与Uncache zero提交时排除同owner非法cacheable请求。
2. 顺序：先清暂等状态并拒绝空item；水位未覆盖completion则标pending并返回；构造reset代次+UID+epoch的key，存在账项即失败；否则成功。
3. 子函数：本函数无请求消费或数据副作用；readiness由水位提供而非固定sleep。


### 5.6 消费与终场

抽象功能描述：只有核心唯一提交成功才消费已验证请求证明，防止轮询重复消费。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`function bit mark_consumed`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit mark_consumed(input rm_ls_program_item_t item);
        owner_key_t k;
        request_state_t state_v;
        if(item==null || !item.cmo_requests_validated || !item.cmo_completion_consumed) return 0;
        k={reset_generation,32'(item.uid),32'(item.dynamic_epoch)};
        if(item.cmo_pre_access_exception!=0) return !requests.exists(k);
        if(!requests.exists(k)) return 0;
        state_v=requests[k];
        if(state_v.failed || state_v.cancelled || state_v.consumed) return 0;
        state_v.consumed=1; requests[k]=state_v;
        return 1;
    endfunction
```
中文伪代码：

1. 功能：只有核心唯一提交成功才消费已验证请求证明，防止轮询重复消费。
2. 顺序：item为空、证明未验证或完成未消费则失败；前置异常只允许无请求；常规请求须存在且未失败/取消/消费；标consumed并写回。
3. 子函数：key使用当前reset代次隔离历史；Uncache zero由统一观察器维护其请求终态，不通过此账消费。


### 5.7 收尾审计

抽象功能描述：真实测试结束时一次检查已接受CMO请求是否都有合法终态，不在高频路径扫描历史。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_cmo_request_observer.sv`，函数：`function bit check_drained`。以下源码展示当前实际判断与状态副作用。

```systemverilog
    function bit check_drained();
        bit clean;
        clean=1;
        foreach(requests[k]) begin
            if(!requests[k].cancelled && !requests[k].consumed) begin
                clean=0;
                fail($sformatf("CMO请求未消费 uid=%0d epoch=%0d",requests[k].owner.uid,requests[k].owner.epoch));
            end
        end
        if(cmo_waiting.size()!=0) begin clean=0; fail("CMO内部请求仍等待真实TL A"); end
        foreach(zero_by_slot[i])
            if(zero_by_slot[i].size()!=0) begin clean=0; fail("zero请求仍等待真实SB完成"); end
        return clean;
    endfunction
```
中文伪代码：

1. 功能：真实测试结束时一次检查已接受CMO请求是否都有合法终态，不在高频路径扫描历史。
2. 顺序：扫描请求账，未取消且未消费者报告身份；内部CMO等待TL A队列非空报错；固定16个SB slot存在zero来源报错；返回整个审计是否干净。
3. 子函数：fail保存局部计数并报告UVM错误。调用方不能把历史清空代替收尾验证。



### 5.8 已消费CMO的迟到完成审计

修改前：STA0只在RM当前UID查询时检查唯一数量；程序游标推进后不再重访该owner，因此后续重复fault可能被框架当旧事件丢弃而RM未检查。合法fault CMO之后框架已有一次normal STA尾拍吸收例外，不能粗暴将所有后续normal都当第二次架构完成。

修改后：核心成功消费后经readonly新API注册终态摘要；WB observer按实际后续STA0逐事件、ROB键常数查询。注册时只补查该ROB已有历史，覆盖RM程序游标较慢而尾拍已被采到的情况。无逐拍累计历史扫描；真实新动态owner优先替换旧ROB摘要，reset清摘要。

合法例外完全复用既有`dispatch_monitor_event_adapter::try_consume_cbo_fault_late_normal_sta()`来源契约：无live ROB、原UID/epoch/ROB/SQ/STA issue/replay/instance flush一致，已terminal且fault/sta_fault/sta_writeback成立，主表仍为同最终CMO，raw sample flush不早于原fault且等于当前框架flush。仅一次normal尾拍是已存在的非架构尾拍，第二fault或第二normal仍立即报错。不修改adapter行为，不发布新框架状态。

抽象功能描述：`mark_cmo_completion_consumed()`只在核心成功提交后冻结被动终态审计来源；不推进程序UID或修改框架epoch。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv`，函数：`mark_cmo_completion_consumed()`。

```systemverilog
    function bit mark_cmo_completion_consumed(input writeback_record_t completion);
        common_data_transaction data;
        main_control_transaction main_tr;
        status_transaction status_v;
        cmo_terminal_audit_t terminal_v;
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned instance_flush;
        data=common_data_transaction::m_inst;
        if(!completion.valid || !completion.owner_valid || completion.source_kind!=RM_WB_SOURCE_STA ||
           completion.lane!=0 || !completion.exception_valid || !completion.rob_flag_valid || data==null ||
           !data.is_valid_uid(completion.owner_uid)) return 0;
        status_v=data.get_status(completion.owner_uid); main_tr=data.get_main_transaction(completion.owner_uid);
        if(status_v==null || main_tr==null || main_tr.op_class!=MEMBLOCK_OP_CLASS_CBO ||
           status_v.dynamic_epoch!=completion.owner_dynamic_epoch ||
           status_v.get_rob_key()!=memblock_rob_key_t'{flag:completion.rob_flag,value:completion.rob_value}) return 0;
        if(!status_v.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,instance_flush)) begin
            if(!status_v.active_instance_flush_epoch_valid) return 0;
            instance_flush=status_v.active_instance_flush_epoch;
        end
        key={completion.rob_flag,completion.rob_value};
        if(cmo_terminal_by_rob.exists(key) &&
           cmo_terminal_by_rob[key].completion.owner_uid==completion.owner_uid &&
           cmo_terminal_by_rob[key].completion.owner_dynamic_epoch==completion.owner_dynamic_epoch) return 0;
        terminal_v='0; terminal_v.valid=1; terminal_v.completion=completion;
        terminal_v.sq='{flag:status_v.sqIdx_flag,value:status_v.sqIdx_value}; terminal_v.sta_issue_epoch=status_v.sta_issue_epoch;
        terminal_v.replay_seq=status_v.replay_seq; terminal_v.instance_flush_epoch=instance_flush;
        terminal_v.fu_op_type=main_tr.fuOpType;
        cmo_terminal_by_rob[key]=terminal_v;
        // 注册只发生一次：补查该ROB在RM排到此UID之前已采到的尾拍，避免程序cursor较慢漏审。
        if(sta0_history_by_rob.exists(key)) begin
            foreach(sta0_history_by_rob[key][i])
                if(sta0_history_by_rob[key][i].sample_seq>completion.sample_seq)
                    audit_cmo_sta0(sta0_history_by_rob[key][i]);
        end
        return 1;
    endfunction
```
中文伪代码：

1. 功能：登记已确认唯一架构完成的CMO，使之后事件无需RM重新访问旧UID也能受审计。
2. 顺序：要求有效真实STA0和冻结owner；读取该UID当前状态/主表，验证CMO分类、epoch和ROB；取得STA flush来源，缺STA专属时只沿既有readonly规则回退active flush；同owner重复注册返回失败；建立含原完成、SQ、STA issue/replay/flush和最终op的摘要，晚normal标志清零；按ROB保存后，仅补查该ROB时间晚于原完成的已采记录；最后返回登记成功。
3. 子函数：get_status/get_main_transaction仅查询既有事实；get_target_instance_flush_epoch取同动态实例来源；audit_cmo_sta0核对补查到的后续记录。任何发现通过UVM报错，不修改actual数据或框架恢复状态。

抽象功能描述：`audit_cmo_sta0()`按每笔真实后续STA0检查已消费CMO的重复完成，保留来源完备的一次合法fault后normal尾拍。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv`，函数：`audit_cmo_sta0()`。

```systemverilog
    function void audit_cmo_sta0(input writeback_record_t record);
        common_data_transaction data;
        cmo_terminal_audit_t terminal_v;
        status_transaction status_v;
        main_control_transaction main_tr;
        memblock_uid_t live_uid;
        memblock_rob_key_t rob_key;
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned instance_flush;
        if(!record.valid || record.source_kind!=RM_WB_SOURCE_STA || record.lane!=0 ||
           !record.rob_flag_valid) return;
        key={record.rob_flag,record.rob_value};
        if(!cmo_terminal_by_rob.exists(key)) return;
        terminal_v=cmo_terminal_by_rob[key];
        if(record.sample_seq<=terminal_v.completion.sample_seq) return;
        data=common_data_transaction::m_inst;
        rob_key='{flag:record.rob_flag,value:record.rob_value};
        if(record.owner_valid && (record.owner_uid!=terminal_v.completion.owner_uid ||
           record.owner_dynamic_epoch!=terminal_v.completion.owner_dynamic_epoch)) begin
            cmo_terminal_by_rob.delete(key); return;
        end
        if(data!=null && data.lookup_active_uid_by_rob(rob_key,live_uid)) begin
            status_v=data.get_status(live_uid);
            if(live_uid!=terminal_v.completion.owner_uid || status_v==null ||
               status_v.dynamic_epoch!=terminal_v.completion.owner_dynamic_epoch) begin
                // 真实ROB复用已明确新owner；新动态实例优先，不误判旧终态重复。
                cmo_terminal_by_rob.delete(key); return;
            end
        end
        if(terminal_v.completion.exception_vec!=0 && record.exception_valid &&
           record.exception_vec===24'b0 && !terminal_v.late_normal_seen && data!=null &&
           !data.lookup_active_uid_by_rob(rob_key,live_uid) &&
           data.is_valid_uid(terminal_v.completion.owner_uid)) begin
            status_v=data.get_status(terminal_v.completion.owner_uid);
            main_tr=data.get_main_transaction(terminal_v.completion.owner_uid);
            if(status_v!=null && main_tr!=null && !status_v.active && status_v.terminal_done &&
               status_v.fault && status_v.sta_fault && status_v.sta_writeback &&
               status_v.dynamic_epoch==terminal_v.completion.owner_dynamic_epoch &&
               status_v.get_rob_key()==rob_key && status_v.sqIdx_flag==terminal_v.sq.flag &&
               status_v.sqIdx_value==terminal_v.sq.value &&
               status_v.sta_issue_epoch==terminal_v.sta_issue_epoch && status_v.replay_seq==terminal_v.replay_seq &&
               status_v.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,instance_flush) &&
               instance_flush==terminal_v.instance_flush_epoch && main_tr.op_class==MEMBLOCK_OP_CLASS_CBO &&
               main_tr.fuOpType==terminal_v.fu_op_type && status_v.exception_vec!=0 &&
               record.sample_flush_epoch>=terminal_v.completion.sample_flush_epoch &&
               record.sample_flush_epoch==memblock_sync_pkg::dispatch_flush_epoch) begin
                terminal_v.late_normal_seen=1; cmo_terminal_by_rob[key]=terminal_v; return;
            end
        end
        report_cmo_audit_error($sformatf(
            "已消费CMO收到非法重复STA0 uid=%0d epoch=%0d rob=%0d/%0d terminal=%0d duplicate=%0d exception=0x%0h",
            terminal_v.completion.owner_uid,terminal_v.completion.owner_dynamic_epoch,
            record.rob_flag,record.rob_value,terminal_v.completion.sample_seq,record.sample_seq,record.exception_vec));
    endfunction
```
中文伪代码：

1. 功能：封闭RM游标推进后的重复完成漏洞。
2. 顺序：仅处理有效带完整ROB的STA0；无终态摘要或sample不晚于原完成直接返回；真实冻结owner或当前live owner明确不同则删除旧摘要，让ROB复用新实例优先。原完成为fault且当前为normal、未用例外、无liveowner时，检查现有终态全身份来源；全部一致才标记一次late_normal_seen并返回。其它情况报告非法重复，包含原UID/epoch/ROB/终态sample/后续sample/异常。
3. 子函数：lookup_active_uid_by_rob只查询当前所有者；状态字段与框架例外来源相同；report_cmo_audit_error增加局部审计错误计数并报告UVM_ERROR。它不消费adapter context，不生成第二次commit或redirect。

抽象功能描述：readonly的`mark_dut_cmo_completion_consumed_for_rm()`把当前owner唯一真实STA0转交被动审计登记，调用方只在核心compare_result成功后调用。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv`，函数：`mark_dut_cmo_completion_consumed_for_rm()`。

```systemverilog
function bit memblock_rm_readonly_api::mark_dut_cmo_completion_consumed_for_rm(input memblock_uid_t uid,
    input memblock_rob_key_t key,input int unsigned dynamic_epoch);
    dut_writeback_view_t completion;
    bit duplicate;
    if(!read_dut_cmo_writeback_for_rm(uid,key,dynamic_epoch,completion,duplicate)) return 0;
    return m_dut_writeback_observer.mark_cmo_completion_consumed(completion);
endfunction
```
中文伪代码：

1. 功能：提供核心成功消费到被动审计的只读薄桥接。
2. 顺序：按UID、ROB、epoch读取唯一真实STA0；缺失或重复返回失败；将实际值型记录交给observer登记并返回其结果。
3. 子函数：read_dut_cmo_writeback_for_rm维持原身份与flush代次选择；mark_cmo_completion_consumed建立审计摘要。调用方失败必须报告，不能静默忽略。

核心成功消费后的实际调用已集成；只对compare_result成功的CMO登记，失败明确报错。类型化ROB赋值模式用于比较上下文，避免VCS无法推断未标类型的结构模式。

抽象功能描述：`observer_process_current_commit()`在核心CMO已成功比较并消费后，把终态转入持续被动审计；失败路径不会误登记正常终态。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，`observer_process_current_commit()` 的成功CMO登记分支。

```systemverilog
    if(compare_result && item.kind==RM_LS_KIND_CMO &&
       !ro.mark_dut_cmo_completion_consumed_for_rm(item.uid,
            '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},item.dynamic_epoch))
        `uvm_error("RM_CMO_DUPLICATE_COMPLETION","CMO提交后终态审计注册失败")
```
中文伪代码：

1. 功能：把核心唯一完成和后续真实STA0审计衔接。
2. 顺序：仅核心compare_result成功且当前item是CMO时调用readonly登记；传入本item UID、完整ROB和动态epoch；登记失败立即报告UVM错误，避免静默丢失收尾能力。其他指令或核心失败不登记。
3. 子函数：mark_dut_cmo_completion_consumed_for_rm重新读取同owner唯一真实STA0并冻结审计摘要，不推进框架UID、epoch或redirect。

新增soft沿真实observer方法验证正常终态后重复、fault一次合法normal尾拍、第二normal/第二fault、错误replay、新owner优先、注册时补审和reset。13项新增断言与原request26项合计39项，最新真实VCS已全部通过，UVM_ERROR/UVM_FATAL均为0（主agent结果）。CSV独立登记此新增子场景。

## 6. 测试层适配及验证证据

既有 directed builder 的6条访存为 older Store、目标CMO、同line年轻Load/Store和不同line年轻Load/Store。它们用于覆盖CMO恢复后年轻项重建；恢复完全沿用测试框架，RM不参与UID+1或epoch创建。

修改前该builder/vseq强制DISABLED，无法放入CHECK_STORE。修改后允许既有AUTO模式，在该模式尾部追加UID6/ROB6的CHECK_STORE，使用父类既有CSR/fence worker完成flush和最终Store-table比较；DISABLED不追加控制项，继续使用父类静态Sv39 CSR以保留PBMT专项。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cbo_directed_sequence.sv`，`build_main_table()`。

抽象功能描述：场景建表阶段构造固定访存和所选拓扑合法的最终检查，运行期仍交由父类统一服务，不新增worker生命周期。

```systemverilog
    if (topology_mode == memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE) begin
        set_manual_main_transaction(
            6,
            make_control_main_transaction("cbo_final_check_store", 6,
                                           MEMBLOCK_OP_CLASS_CHECK_STORE,
                                           '{flag:1'b0,value:MEMBLOCK_ROB_VALUE_W'(6)}));
    end
    import_manual_main_table();
    check_main_table_control_policy("memblock_main_dispatch_cbo_directed_sequence");
```
中文伪代码：

1. 功能：为已有真实CMO场景建立可终场比较的数据验证边界。
2. 顺序：AUTO时创建唯一尾CHECK_STORE并加入手工表；随后导入全部条目和初始化状态；最后校验当前拓扑是否允许这些控制项，非法组合立即终止。DISABLED跳过控制项，保持六条访存。
3. 子函数：make_control_main_transaction创建无LSQ数据载荷的控制项；导入函数分配UID并验证表；拓扑检查防止将控制指令送进不支持的legacy流程。

| 文件/场景 | 修改与验证目的 | 当前证据 |
|---|---|---|
| `soft_test_rm_cmo_core.sv` | 整行64B/8entries、Uncache0/2/3/8段、SC覆盖、nonzero读权限、前置fault无PA、MMIO接受/拒绝、缺请求、重复/旧owner、错误flushPipe | 真实VCS通过，mode=cmo_plans_20261111、seed=666666，共920项core检查（主agent结果） |
| `soft_test_rm_cmo_request.sv` | 真实16B/64B mask、replay、共享slot、ROB flag、offset、真实TL、同拍发布顺序、reset、漏wline、禁止路径、成功晚于最终写回负向 | 真实VCS通过，同mode/seed，共39项request检查（包含13项提交后审计）（主agent结果） |
| `tc_dispatch_real_cbo_zero/clean/flush/inval.cfg` | 每次仅一种CMO，AUTO尾CHECK_STORE | 真实VCS四个操作均通过，请求/终态及尾CHECK_STORE通过，UVM_ERROR/FATAL为0 |
| `tc_dispatch_real_cbo_zero_uncache[_denied].cfg` | DISABLED静态Sv39，mPBMTE1/PBMTNC100%，denied0或25权重 | 真实VCS通过；正常8段/11项最终entry，denied两段/5项最终entry，通知比较通过 |
| 两份 `AI_DOC/seq/memblock_cmo_rm_*_20261111.csv` | 真实与soft分文件逐场景登记 | CSV可解析、名称唯一、分类正确；core920/request39/production translation92已通过；真实结果与RTL阻塞以CSV及最终集成review为准 |

`git diff --check` 对本轮目标文件通过。临时桩仅校验语法与类型可展开性，不能替代真实readonly身份和实际接口握手的端到端验证。


### 6.1 生产翻译/权限入口的软件验证补充

新增`soft_test_tc_rm_cmo_translation`继承既有PBMT生产路径测试，复用`make_uid_tlb_record()`冻结UID请求与TLB响应，直接调用真实`env.rm.observer_build_commit_item()`；没有复制RM权限算法，也没有以DUT最终异常作为期望。

覆盖矩阵包括：S1/S2合法只读页上clean/flush/inval通过、zero写权限失败并外显Store PF/GPF；全部四种CMO的S1 PF/AF、S2 GPF/GAF；PBMT=PMA/NC/IO的最终路径；offset63、src+imm和负立即数；锁定只读PMP允许nonzero但拒绝zero并外显Store AF；V2 PMA拒绝、允许PMA MMIO却禁止CMO的前置AF；opClass/fuType不一致拒绝。主表data和STD无效值不会参与CMO写期望。

该测试仅新增独立文件，tc_pkg由主agent在已有PBMT基类后添加include。真实VCS已通过全部92项生产入口检查；日志为`/tmp/cmo_translation_run_20261111.log`，mode=`cmo_plans_20261111`、seed=`666666`，CSV已同步PASS。测试使用固定有限矩阵，不进入运行期主循环，不新增公共参数或worker。

### 6.2 页权限与Store异常

抽象功能描述：通过真实生产入口证明读权限命令与Store异常外显相互独立。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_cmo_translation.sv`，函数：`function void check_page_permissions`。片段展示真实生产入口的输入构造与独立断言。

```systemverilog
    function void check_page_permissions();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        bit [1:0] stage;
        bit result_v;
        for(int s=0;s<2;s++) begin
            stage=s==0 ? 2'd0 : 2'd2;
            for(int op_i=0;op_i<4;op_i++) begin
                memblock_uid_t uid;
                uid=100+s*4+op_i;
                init_cmo_views(uid,cmo_op(op_i),64'h8123_403f+64'(uid)*8192,main_v,status_v);
                record=make_uid_tlb_record(uid,main_v.vaddr,stage,0,1,1);
                if(s==0) record.payload.s1_pte_w=0;
                else record.payload.s2_pte_w=0;
                result_v=env.rm.observer_build_commit_item(main_v,status_v,item);
                expect_cmo(result_v && item.kind==RM_LS_KIND_CMO &&
                    item.cmo_permission_write==(op_i==0),"CMO page permission command classification");
                expect_cmo(item.expected_exception==(op_i==0 ? (s==0 ? 24'h008000 : 24'h800000) : 24'h0),
                    $sformatf("CMO read-only page stage=%0d op=%0d got=0x%0h",s,op_i,item.expected_exception));
                expect_cmo(item.cmo_pre_access_exception==item.expected_exception && item.size_bytes==0 &&
                    item.store_data==0 && item.store_byte_mask==0,"CMO canonical data/size and pre-access fault");
                if(op_i!=0) expect_cmo(item.cmo_path==RM_CMO_PATH_DCACHE_CMO &&
                    item.computed_vaddr==main_v.vaddr && item.pa_by_byte[0][11:0]==main_v.vaddr[11:0] &&
                    item.pa_valid_mask==1,"normal nonzero offset geometry uses src+imm and one translated byte");
            end
        end
        init_cmo_views(108,MEMBLOCK_LSUOP_CBO_CLEAN,64'h8125_5001,main_v,status_v);
        record=make_uid_tlb_record(108,main_v.vaddr,0,0,0,1);
        expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
            item.expected_exception==24'h008000 && item.cmo_pre_access_exception==24'h008000,
            "nonzero denied read permission must expose Store Page Fault");
    endfunction
```
中文伪代码：

1. 功能：通过真实生产入口证明读权限命令与Store异常外显相互独立。
2. 顺序：遍历S1与S2的四种CMO，冻结合法页后只清W位；调用生产入口并断言CMO分类及zero写权限、期望PF/GPF、data/size归一化；nonzero还验证真实offset和单字节翻译几何；最后清R/W建立nonzero读权限失败并断言Store PF。
3. 子函数：init_cmo_views复用基类视图并构造src+imm；make_uid_tlb_record建立真实readonly可读记录；expect_cmo只断言生产函数结果，不实现权限算法。

### 6.3 TLB异常矩阵

抽象功能描述：从冻结TLB有效fault事实检查CMO前置异常外显类别，证明PBMT IO不会覆盖翻译异常。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_cmo_translation.sv`，函数：`function void check_translation_faults`。片段展示真实生产入口的输入构造与独立断言。

```systemverilog
    function void check_translation_faults();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        bit [23:0] expected_v;
        for(int op_i=0;op_i<4;op_i++) begin
            for(int fault_i=0;fault_i<4;fault_i++) begin
                memblock_uid_t uid;
                uid=112+op_i*4+fault_i;
                init_cmo_views(uid,cmo_op(op_i),64'h8140_003f+64'(uid)*8192,main_v,status_v);
                record=make_uid_tlb_record(uid,main_v.vaddr,fault_i<2 ? 2'd0 : 2'd2,2,1,1);
                case(fault_i)
                    0:begin record.payload.fault_effective_s1_pf=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
                        expected_v=24'h008000; end
                    1:begin record.payload.fault_effective_s1_af=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
                        expected_v=24'h000080; end
                    2:begin record.payload.fault_effective_s2_gpf=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
                        expected_v=24'h800000; end
                    default:begin record.payload.fault_effective_s2_gaf=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
                        expected_v=24'h000080; end
                endcase
                expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                    item.expected_exception==expected_v && item.cmo_pre_access_exception==expected_v &&
                    item.size_bytes==0,"CMO TLB fault priority and Store-class exception mapping");
            end
        end
    endfunction
```
中文伪代码：

1. 功能：从冻结TLB有效fault事实检查CMO前置异常外显类别，证明PBMT IO不会覆盖翻译异常。
2. 顺序：四种CMO依次配S1 PF、S1 AF、S2 GPF、S2 GAF；先建PBMT IO合法记录，再设置对应有效异常和阶段；调用生产入口，要求预期Store类异常、前置异常镜像和CMO零标量长度均正确。
3. 子函数：make_uid_tlb_record仍提供冻结stage/context；observer_build_commit_item执行实际异常优先级；expect_cmo核对事先选定的架构异常值。

### 6.4 PBMT与地址几何

抽象功能描述：在实际翻译入口验证PBMT仅改变zero数据路径而nonzero仍走DCache CMO，并保留非对齐原始地址低位。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_cmo_translation.sv`，函数：`function void check_pbmt_and_offset`。片段展示真实生产入口的输入构造与独立断言。

```systemverilog
    function void check_pbmt_and_offset();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        rm_cmo_path_e expected_path;
        for(int pbmt=0;pbmt<3;pbmt++) begin
            for(int op_i=0;op_i<4;op_i++) begin
                memblock_uid_t uid;
                uid=132+pbmt*4+op_i;
                init_cmo_views(uid,cmo_op(op_i),64'h8180_003f+64'(uid)*8192,main_v,status_v);
                record=make_uid_tlb_record(uid,main_v.vaddr,0,2'(pbmt),1,1);
                expected_path=op_i!=0 ? RM_CMO_PATH_DCACHE_CMO :
                    pbmt==0 ? RM_CMO_PATH_CACHEABLE_ZERO : RM_CMO_PATH_UNCACHE_ZERO;
                expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                    item.expected_exception==0 && item.cmo_path==expected_path && item.size_bytes==0 &&
                    item.pa_valid_mask==1 && item.computed_vaddr==main_v.vaddr &&
                    item.pa_by_byte[0][5:0]==6'h3f && item.store_data==0,
                    $sformatf("CMO PBMT=%0d op=%0d offset/path classification",pbmt,op_i));
            end
        end
        // 有符号imm必须仍按地址操作数处理，不允许误套原子src0-only约定。
        init_cmo_views(145,MEMBLOCK_LSUOP_CBO_ZERO,64'h81a0_003f,main_v,status_v);
        main_v.src_0=main_v.vaddr+1; main_v.imm=64'hfff;
        record=make_uid_tlb_record(145,main_v.vaddr,0,0,1,1);
        expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) && item.expected_exception==0 &&
            item.computed_vaddr==main_v.vaddr && item.pa_by_byte[0][5:0]==6'h3f,
            "CMO signed immediate -1 address geometry");
    endfunction
```
中文伪代码：

1. 功能：在实际翻译入口验证PBMT仅改变zero数据路径而nonzero仍走DCache CMO，并保留非对齐原始地址低位。
2. 顺序：遍历三种PBMT与四CMO，对offset63地址构造src0=VA-5、imm5；冻结合法响应后调用生产入口，断言无错、预期路径、单字节PA、零长度和数据。随后构造imm为12位-1、src0=VA+1，断言生产地址仍等于VA。
3. 子函数：make_uid_tlb_record让readonly获得有效PBMT；生产入口实际执行符号扩展与路径分类，测试独立指定预期路径而不调用分类helper推导。

### 6.5 PMA/PMP真实权限模型

抽象功能描述：将真实PMA/PMP生产模型纳入CMO权限验证，证明nonzero保持READ命令并最终外显Store AF。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_cmo_translation.sv`，函数：`function void check_pma_pmp_permissions`。片段展示真实生产入口的输入构造与独立断言。

```systemverilog
    function void check_pma_pmp_permissions();
        common_data_transaction data;
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        pma_pmp_eval_t eval_v;
        bit [47:0] address;
        data=common_data_transaction::get();
        seq_csr_common::pma_pmp_model_en=1;
        // 一个锁定的全空间NAPOT只读PMP项使M态也执行只读检查；合法PMA普通缓存区。
        data.pma_pmp_model.reset_and_init_v2_profile();
        data.pma_pmp_model.apply_csr_write(12'h3b0,64'h0000_1fff_ffff_ffff,1);
        data.pma_pmp_model.apply_csr_write(12'h3a0,64'h99,2);
        address=48'h0000_8000_103f;
        for(int op_i=0;op_i<4;op_i++) begin
            memblock_uid_t uid;
            uid=148+op_i;
            init_cmo_views(uid,cmo_op(op_i),64'h81c0_003f+64'(uid)*8192,main_v,status_v);
            record=make_uid_tlb_record(uid,main_v.vaddr,0,0,1,1);
            record.request_s1_resolved_ppn=44'(address[47:12]);
            capture_pma_context(record,status_v);
            expect_cmo(data.pma_pmp_model.evaluate_for_uid_epoch(uid,status_v.dynamic_epoch,1,
                address,1,op_i==0 ? PMA_PMP_CMD_WRITE : PMA_PMP_CMD_READ,eval_v) &&
                eval_v.valid && eval_v.pmp_st_fault==(op_i==0) && !eval_v.pma_st_fault &&
                !eval_v.pma_ld_fault,"read-only PMP precondition");
            expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                item.expected_exception==(op_i==0 ? 24'h80 : 24'h0) &&
                item.expected_pmp_fault==(op_i==0) && item.cmo_permission_write==(op_i==0),
                "nonzero PMP uses read permission but denied zero exposes Store AF");
        end
        // V2默认PMA [0x39002000,0x3a000000)无读写权限；M态无PMP项默认通过。
        data.pma_pmp_model.reset_and_init_v2_profile(); address=48'h0000_3900_303f;
        for(int op_i=0;op_i<4;op_i++) begin
            memblock_uid_t uid;
            uid=152+op_i;
            init_cmo_views(uid,cmo_op(op_i),64'h81e0_003f+64'(uid)*8192,main_v,status_v);
            record=make_uid_tlb_record(uid,main_v.vaddr,0,0,1,1);
            record.request_s1_resolved_ppn=44'(address[47:12]); capture_pma_context(record,status_v);
            expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                item.expected_exception==24'h80 && item.expected_pma_fault && !item.expected_pmp_fault &&
                item.cmo_pre_access_exception==24'h80,"PMA R/W denial maps every CMO to Store AF");
        end
        // PMA MMIO允许read/write仍使所有CMO在访问前报Store AF；PBMT IO本身不会报此异常。
        address=48'h0000_2001_003f;
        for(int op_i=0;op_i<4;op_i++) begin
            memblock_uid_t uid;
            uid=156+op_i;
            init_cmo_views(uid,cmo_op(op_i),64'h8200_003f+64'(uid)*8192,main_v,status_v);
            record=make_uid_tlb_record(uid,main_v.vaddr,0,0,1,1);
            record.request_s1_resolved_ppn=44'(address[47:12]); capture_pma_context(record,status_v);
            expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                item.expected_exception==24'h80 && item.all_pma_mmio &&
                !item.expected_pmp_fault && !item.expected_pma_fault,
                "allowed PMA MMIO rejects CMO independently from read/write permission AF");
        end
        seq_csr_common::pma_pmp_model_en=0;
    endfunction
```
中文伪代码：

1. 功能：将真实PMA/PMP生产模型纳入CMO权限验证，证明nonzero保持READ命令并最终外显Store AF。
2. 顺序：开启模型、复位V2 profile并通过CSR写创建锁定全空间只读PMP；在合法可缓存PA为四CMO捕获上下文，先确认只读前提再执行真实RM，zero须StoreAF、nonzero无错。再复位模型使用V2无读写PMA区域，四操作都须StoreAF且仅PMA诊断置位。最后使用允许读写的PMA MMIO区，要求四CMO前置StoreAF但基础PMA/PMP权限诊断不置位；结束关闭测试临时开关。
3. 子函数：apply_csr_write以真实模型CSR接口生成不可变表版本；capture_pma_context冻结UID/epoch权限版本；evaluate_for_uid_epoch只用于确认测试前提；observer_build_commit_item仍为被测真实入口。

### 6.6 错误分类边界

抽象功能描述：防止主表错误分类以及功能单元相同数值编码误进入CMO。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_cmo_translation.sv`，函数：`function void check_classification_guards`。片段展示真实生产入口的输入构造与独立断言。

```systemverilog
    function void check_classification_guards();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        rm_ls_program_item_t item;
        bit result_v;
        init_cmo_views(160,MEMBLOCK_LSUOP_CBO_CLEAN,64'h8240_0000,main_v,status_v);
        main_v.op_class=MEMBLOCK_OP_CLASS_STORE;
        result_v=env.rm.observer_build_commit_item(main_v,status_v,item);
        expect_cmo(!result_v || item.kind!=RM_LS_KIND_CMO,"ordinary Store opClass must not accept CMO final opcode");
        for(int fu_i=0;fu_i<2;fu_i++) begin
            init_cmo_views(161+fu_i,MEMBLOCK_LSUOP_CBO_ZERO,64'h8240_0000,main_v,status_v);
            main_v.fu_type=fu_i==0 ? MEMBLOCK_FUTYPE_LDU : MEMBLOCK_FUTYPE_MOU;
            result_v=env.rm.observer_build_commit_item(main_v,status_v,item);
            expect_cmo(!result_v || item.kind!=RM_LS_KIND_CMO,"non-STU fuType cannot be classified as CMO");
        end
    endfunction
```
中文伪代码：

1. 功能：防止主表错误分类以及功能单元相同数值编码误进入CMO。
2. 顺序：用普通Store opClass配STU CBO编码，调用生产入口并要求失败或非CMO；再用CBO opClass分别配LDU和MOU，要求同样拒绝；不捕获无关UVM报告，分类错误由核心状态码记录。
3. 子函数：init_cmo_views建立正确基线；每个负向只修改一个分类字段；生产入口新增一致性guard是被测行为。

### 6.7 真实测试入口的AUTO能力白名单

修改前basicTest的既有控制worker能力白名单没有directed CBO，因而AUTO preset在启动前被拒绝。修改后只加入该已支持AUTO的CBO VSEQ，使用原有worker能力校验；其它场景条目及生命周期没有变化。

抽象功能描述：basicTest选择主VSEQ后，在创建控制worker之前判定场景是否支持该拓扑，避免错误场景启动控制producer。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`，函数：`vseq_supports_control_worker_topology()`。

```systemverilog
      function bit vseq_supports_control_worker_topology(input string vseq_name);
          return vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_directed_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq" ||
                 vseq_name == "memblock_csr_random_config_vseq" ||
                 vseq_name == "memblock_csr_scalar_load_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_mixed_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_sta_std_10k_vseq";
      endfunction:vseq_supports_control_worker_topology
```
中文伪代码：

1. 功能：在场景入口确认AUTO控制worker拓扑能力。
2. 顺序：比较被选择VSEQ是否属于既有支持集合；directed CBO新增为合法成员；命中任意成员返回支持，其余返回不支持，由调用方现有校验拒绝非法组合。
3. 子函数：该函数无下级调用或状态写；只扩大一个已落实AUTO/CHECK_STORE场景的能力声明，不创建worker或修改主表。

该适配是实际真实zero AUTO首轮发现的入口遗漏，属于测试闭环所需的最小集成。应在plan实现细节中记录，避免后续只看builder允许AUTO便误认为basicTest入口同样允许。


### 6.8 冷目标CBOAck故障验收

真实普通directed CMO的年轻同line Load可以在CMO完成前进入DCache并先得到GrantData；当GrantData权重0时，该行首次选择立即冻结CLEAN，即使CBOAck权重100也不能重新随机。实际日志`/tmp/cmo_clean_denied_run_20261111.log`显示uid2同line Load在315.3ns发射、412.8ns真实LDA完成，而uid1 CMO最终STA0在1732.8ns；其“通过但异常0”不能作为故障覆盖。

新故障场景保持首行CLEAN永久策略与真实A.fire随机点：独立cold builder把目标CMO放uid0/0x80002000，年轻Load/Store分别放0x80003000/0x80004000，尾CHECK_STORE；该行第一次真实L2访问因此是CBO A。全部transaction helper和worker仍复用既有实现，没有预写DENIED/CORRUPT历史，也不按响应类别另建随机表。

抽象功能描述：`build_main_table()`在场景建表阶段构造冷目标与不同line年轻工作，让真实CBOAck首次选择可按现有权重注入；不修改运行期框架。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cbo_fault_cold_sequence.sv`，`build_main_table()`。

```systemverilog
    virtual task build_main_table();
        bit [8:0] op;
        ensure_dispatch_runtime_helpers();
        if(memblock_sync_pkg::get_control_worker_topology_mode()!=
           memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE)
            `uvm_fatal(get_type_name(),"cold CMO fault scenario requires existing AUTO topology")
        op=pick_fuop_by_weight(MEMBLOCK_OP_CLASS_CBO,"cold CMO fault target");
        if(!(op inside {MEMBLOCK_LSUOP_CBO_CLEAN,MEMBLOCK_LSUOP_CBO_FLUSH,MEMBLOCK_LSUOP_CBO_INVAL}))
            `uvm_fatal(get_type_name(),"cold CMO fault target must be final clean/flush/inval")
        clear_manual_main_table();
        set_manual_main_transaction(0,make_cbo_directed_transaction("cold_cmo_target",
            MEMBLOCK_OP_CLASS_CBO,op,0,64'h8000_2000));
        set_manual_main_transaction(1,make_cbo_directed_transaction("cold_cmo_younger_other_load",
            MEMBLOCK_OP_CLASS_INT_LOAD,MEMBLOCK_LSUOP_LD,1,64'h8000_3000));
        set_manual_main_transaction(2,make_cbo_directed_transaction("cold_cmo_younger_other_store",
            MEMBLOCK_OP_CLASS_STORE,MEMBLOCK_LSUOP_SD,2,64'h8000_4000));
        set_manual_main_transaction(3,make_control_main_transaction("cold_cmo_final_check_store",3,
            MEMBLOCK_OP_CLASS_CHECK_STORE,'{flag:1'b0,value:MEMBLOCK_ROB_VALUE_W'(3)}));
        import_manual_main_table();
        check_main_table_control_policy("cold CMO fault target");
        // 覆盖父类建表后必须交付同一个既有AUTO运行期bootstrap，worker仍由父VSEQ唯一启动。
        control_barrier_service.initialize_control_runtime_bootstrap();
    endtask
```
中文伪代码：

1. 功能：建立cold故障验收的四项主表，复用已有AUTO控制拓扑。
2. 顺序：初始化dispatch helper并要求AUTO；按既有CMO权重选择最终操作，zero或非法操作立即终止；清空手工表，依次加入目标CMO、不同line Load、不同line Store、尾CHECK_STORE；导入与初始化全表，校验控制拓扑，然后调用既有控制运行期bootstrap完成worker初始握手。
3. 子函数：既有transaction helper填写正常访存身份和地址；控制helper建立无LSQ载荷的最终检查；import为静态条目分配UID并初始化status；拓扑检查防止错误worker组合；initialize_control_runtime_bootstrap复用既有初始握手，不启动额外worker。

抽象功能描述：冷故障VSEQ的`body()`复用父类全部真实生命周期，在完成后检查实际故障与首次行选择，避免空覆盖仍被统计为故障通过。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cbo_fault_cold_vseq.sv`，`body()`。

```systemverilog
    virtual task body();
        common_data_transaction data;
        status_transaction status_v;
        mem_access_base_sequence::l2_d_error_line_record_t choice;
        mem_access_base_sequence::l2_d_error_state_e expected_choice;
        bit [23:0] expected_exception;
        int denied_wt,corrupt_wt;
        bit covered;
        seq_csr_common::init();
        denied_wt=seq_csr_common::get_l2_cbo_ack_denied_wt();
        corrupt_wt=seq_csr_common::get_l2_cbo_ack_corrupt_wt();
        if(denied_wt==100 && corrupt_wt==0) begin
            expected_choice=mem_access_base_sequence::L2_D_ERROR_DENIED; expected_exception=24'h80;
        end else if(denied_wt==0 && corrupt_wt==100) begin
            expected_choice=mem_access_base_sequence::L2_D_ERROR_CORRUPT; expected_exception=24'h080000;
        end else begin
            `uvm_fatal(get_type_name(),"cold CMO fault coverage requires exactly denied100/corrupt0 or denied0/corrupt100")
        end
        super.body();
        data=common_data_transaction::get();
        status_v=data.get_status(0); covered=1;
        if(status_v==null || !status_v.sta_fault || !status_v.terminal_done ||
           status_v.exception_vec!==expected_exception) begin
            covered=0;
            `uvm_error("CMO_COLD_FAULT_COVERAGE",$sformatf(
                "cold target未命中实际终态fault expected=0x%0h",expected_exception))
        end
        if(!mem_access_base_sequence::query_l2_line_history_at_sample(48'h8000_2000,
            memblock_sync_pkg::peek_current_dut_global_sample(),choice) || !choice.valid ||
            choice.first_kind!=3 || choice.state!=expected_choice) begin
            covered=0;
            `uvm_error("CMO_COLD_FAULT_COVERAGE",$sformatf(
                "cold target首访不是实际CBOAck或未命中配置error first_kind=%0d state=%0d expected=%0d",
                choice.first_kind,choice.state,expected_choice))
        end
        if(covered)
            `uvm_info("CMO_COLD_FAULT_COVERAGE",$sformatf(
                "实际CBOAck首访注错并完成DUT fault state=%0d expected_exception=0x%0h",choice.state,expected_exception),UVM_LOW)
    endtask
```
中文伪代码：

1. 功能：把实际CMO fault命中和独立首行选择纳入测试通过标准。
2. 顺序：读取现有CBOAck权重，只允许denied100/corrupt0或相反的确定性专项；调用父类完成全部真实发射、响应、恢复和最终Store-table；读uid0状态，未sta_fault/terminal或异常不等独立期望则报错并取消覆盖资格；只读目标行历史，首次来源不是CBOAck或状态不等目标错误则报错；所有条件满足才打印命中覆盖。
3. 子函数：super.body持有唯一worker生命周期；get_status只读实际框架状态；query_l2_line_history_at_sample只读真实首次选择记录；任何helper都不预写错误状态。

真实cold首轮在267.8ns报告`active control topology reached worker shutdown before bootstrap`：覆盖build_main_table后没有调用父类既有AUTO bootstrap。当前已在导入主表和拓扑校验后调用同一initialize_control_runtime_bootstrap，保持父VSEQ唯一启动worker，未增加公共机制。bootstrap修复后已真实注错且CMO异常与RM一致；含尾CHECK_STORE组合出现SQ状态机重入及排空停滞，pendingst接口时序责任仍待独立复核，保持BLOCKED。

新增配置`tc_dispatch_real_cbo_cold_denied.cfg`与`tc_dispatch_real_cbo_cold_corrupt.cfg`使用MAIN4/AUTO和clean单操作，分别配置CBOAck错误权重100。真实DUT CSV新增两个独立用例，执行前为IMPLEMENTED_NOT_RUN。root负责seq_pkg/filelist及basicTest能力白名单接入，实际运行结果另补。

### 6.9 独立冷目标故障与年轻 Store 恢复分开验收

抽象功能描述：新增 isolated 派生类使用三项主表：目标 CMO 与两条不同 cacheline 的年轻 Load，用于独立验证 CMO 自身故障、请求/异常以及后续 Load 恢复；没有 writer、没有尾 CHECK_STORE，不覆盖 Store-table 或 flushSb 排空。原 cold builder/VSEQ/preset 仍保留，原年轻 Store 的后续路径异常与 flushSb 不空问题继续单独定责，不能用新用例替代原失败结果。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cbo_fault_cold_sequence.sv` 文件末尾，类 `memblock_main_dispatch_cbo_fault_cold_isolated_sequence`。

```systemverilog
class memblock_main_dispatch_cbo_fault_cold_isolated_sequence extends memblock_main_dispatch_cbo_fault_cold_sequence;
    `uvm_object_utils(memblock_main_dispatch_cbo_fault_cold_isolated_sequence)
    function new(string name="memblock_main_dispatch_cbo_fault_cold_isolated_sequence"); super.new(name); endfunction
    // 抽象职责：真实cold CMO后仅安排两个不同line的年轻Load，验收指令故障及Load恢复，不检查Store/flush排空。
    virtual task build_main_table();
        bit [8:0] op;
        ensure_dispatch_runtime_helpers();
        if(memblock_sync_pkg::get_control_worker_topology_mode()!=
           memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED)
            `uvm_fatal(get_type_name(),"isolated cold CMO fault scenario requires existing DISABLED topology")
        op=pick_fuop_by_weight(MEMBLOCK_OP_CLASS_CBO,"isolated cold CMO fault target");
        if(!(op inside {MEMBLOCK_LSUOP_CBO_CLEAN,MEMBLOCK_LSUOP_CBO_FLUSH,MEMBLOCK_LSUOP_CBO_INVAL}))
            `uvm_fatal(get_type_name(),"isolated cold CMO target must be final clean/flush/inval")
        clear_manual_main_table();
        set_manual_main_transaction(0,make_cbo_directed_transaction("isolated_cold_cmo_target",
            MEMBLOCK_OP_CLASS_CBO,op,0,64'h8000_2000));
        set_manual_main_transaction(1,make_cbo_directed_transaction("isolated_cold_cmo_younger_load1",
            MEMBLOCK_OP_CLASS_INT_LOAD,MEMBLOCK_LSUOP_LD,1,64'h8000_3000));
        set_manual_main_transaction(2,make_cbo_directed_transaction("isolated_cold_cmo_younger_load2",
            MEMBLOCK_OP_CLASS_INT_LOAD,MEMBLOCK_LSUOP_LD,2,64'h8000_4000));
        // 手工表仅3项；既有import按实际表项数建立主表，没有writer也不添加CHECK_STORE。
        import_manual_main_table();
        check_main_table_control_policy("isolated cold CMO fault target");
        // DISABLED允许零control项；父real-smoke唯一启动既有静态Sv39 CSR，无AUTO worker/bootstrap。
    endtask
endclass
```

中文伪代码：

1. 功能：复用真实 dispatch helper、既有 DISABLED 静态 Sv39 CSR producer 和框架恢复，建立三项隔离主表。
2. 顺序：确认 DISABLED 拓扑；按现有 CBO 权重选择最终 clean/flush/inval，拒绝 zero；清本场景 manual table 后建立 uid0 CMO@0x80002000、uid1 LD@0x80003000、uid2 LD@0x80004000；不插入控制marker。
3. 调用 import_manual_main_table，按实际三项手工表执行 data.reset_all_tables(manual_num)，把构造输入交给原框架；随后执行既有 control policy 校验；DISABLED 要求零 control 项，因此不启动 AUTO worker 或调用其 bootstrap。
4. 不改错误随机点、行历史、响应 raw、监视器、RM 或后端接受行为；两个年轻 Load 均访问不同 line，目标行仍由真实 CBO A 首次建立选择。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cbo_fault_cold_vseq.sv` 文件末尾，类 `memblock_dispatch_real_cbo_fault_cold_isolated_vseq`。

抽象功能描述：本 VSEQ 只替换父 cold VSEQ 启动的建表类型，所有 responder、控制 worker、终态检查和首次行选择覆盖检查仍由父流程持有。

```systemverilog
class memblock_dispatch_real_cbo_fault_cold_isolated_vseq extends memblock_dispatch_real_cbo_fault_cold_vseq;
    `uvm_object_utils(memblock_dispatch_real_cbo_fault_cold_isolated_vseq)
    function new(string name="memblock_dispatch_real_cbo_fault_cold_isolated_vseq"); super.new(name); endfunction
    // 抽象职责：在父VSEQ现有worker生命周期内启动隔离建表，不改变错误响应或恢复检查。
    virtual task start_main_dispatch_sequence();
        memblock_main_dispatch_cbo_fault_cold_isolated_sequence main_seq;
        `uvm_do_on(main_seq,p_sequencer)
    endtask
endclass
```

中文伪代码：

1. 功能：在父 VSEQ 生命周期内使用隔离建表，不增加并发 producer。
2. start_main_dispatch_sequence 在同一 p_sequencer 启动 isolated builder。
3. body 不重写；父 body 仍先约束 denied100/corrupt0 或相反权重，运行真实场景后检查 uid0 sta_fault、terminal_done 和异常向量，并核对目标行 first_kind 为 CBOAck、首次错误状态等于目标。
4. 因此新场景不能只凭 RM 游标结束空覆盖通过，也不关闭原故障 guard。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv` 的能力函数；`seq/seq_pkg.sv` 继续 include 原两个 cold 文件，`seq/seq.f` 继续声明原文件路径，无新公共参数或新文件依赖。

抽象功能描述：basicTest 只把新 VSEQ 名称加入已有拓扑能力列表，保证真实 L2TLB、控制 worker 和静态 CSR 所有权按原分支选择，不绕过能力校验。

```systemverilog
      function bit vseq_starts_l2tlb(input string vseq_name);
          return vseq_name == "memblock_l2tlb_routed_ppn_vseq" ||
                 vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_directed_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_fault_cold_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_fault_cold_isolated_vseq" ||
                 vseq_name == "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq" ||
                 vseq_name == "memblock_csr_random_config_vseq" ||
                 vseq_name == "memblock_csr_scalar_load_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_mixed_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_sta_std_10k_vseq" ||
                 vseq_name == "memblock_dispatch_real_cancel_reconcile_vseq" ||
                 vseq_name == "memblock_l2tlb_pbmt_response_fault_vseq";
      endfunction:vseq_starts_l2tlb

      function bit vseq_supports_control_worker_topology(input string vseq_name);
          return vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_directed_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_fault_cold_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_fault_cold_isolated_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq" ||
                 vseq_name == "memblock_csr_random_config_vseq" ||
                 vseq_name == "memblock_csr_scalar_load_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_mixed_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_sta_std_10k_vseq";
      endfunction:vseq_supports_control_worker_topology

      function bit vseq_owns_static_mmu_csr(input string vseq_name);
          return (vseq_name == "memblock_l2tlb_routed_ppn_vseq" ||
                  vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                  vseq_name == "memblock_dispatch_real_cbo_directed_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_fault_cold_vseq" ||
                 vseq_name == "memblock_dispatch_real_cbo_fault_cold_isolated_vseq" ||
                  vseq_name == "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq" ||
                  vseq_name == "memblock_l2tlb_pbmt_response_fault_vseq") &&
                 !memblock_sync_pkg::uses_control_barrier_topology();
      endfunction:vseq_owns_static_mmu_csr

```

中文伪代码：

1. 功能：为新派生 VSEQ 声明其继承的真实 worker 能力。
2. vseq_starts_l2tlb 识别 isolated 名称并启用现有 responder；vseq_supports_control_worker_topology 保留该名称的继承能力声明；当前 isolated builder 严格只接受 DISABLED，不能据白名单切换 AUTO。
3. vseq_owns_static_mmu_csr 保持原条件：仅非 control barrier topology 使用稳定 CSR producer；新 isolated builder 要求 DISABLED，因此父 real-smoke 启动唯一既有 memblock_mmu_sv39_csr_sequence，不启动控制 worker；CSR monitor、L2TLB 和原 Sv39 启动屏障全部复用。
4. 名称匹配只影响既有能力路由，不改变配置字段、请求或结果比较。

执行入口复用 `tc_dispatch_real_cbo_cold_denied.cfg`、`tc_dispatch_real_cbo_cold_corrupt.cfg`，通过 `ts=memblock_dispatch_real_cbo_fault_cold_isolated_vseq` 和 `+MEMBLOCK_MAIN_TRANS_NUM=3 +MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0` 选择三项 DISABLED 场景；没有新增 preset 或公共参数。`AI_DOC/seq/memblock_cmo_rm_real_dut_testcases_20261111.csv` 单独登记 `cmo_cold_isolated_cbo_ack_denied` 与 `cmo_cold_isolated_cbo_ack_corrupt`，`entry` 指向新 VSEQ，`cfg` 仍为原 cold 两项，实际执行状态由 root 按日志统一更新；本节不宣称 isolated 已通过。

执行差异：原 cold 用例已独立完成 CMO denied/corrupt 的请求与异常比较，但年轻 Store 恢复后路径异常导致 flushSb 不空，故原场景整体验收仍失败或受阻。isolated 用例首轮保留 CHECK_STORE 时，CMO/两个 Load 均已通过 RM，但仍卡在尾 marker 触发的 flushSb；波形已证实 SQ 重入与排空阻塞；pendingst 上游时序契约尚需独立责任复核，详见专项分析文档。因此当前 isolated 删除该 marker，仅验证 CMO 与年轻 Load 指令结果，既不证明年轻 Store 恢复已修复，也不证明 flushSb 能排空；原含 Store/CHECK_STORE 的失败场景及记录均须保留。

拓扑契约复核：`memblock_dispatch_base_sequence::check_main_table_control_policy()` 明确 DISABLED/MANUAL_MAIN_TABLE 禁止 control 项，AUTO/MANUAL_CONTROL_TABLE 则要求至少一项。三项无 control 的 isolated 不能继续使用 AUTO；首轮出现的 `AUTO topology built no control entries` 属于测试构造不满足既有契约。现仅将派生 builder 改为 DISABLED 并移除 AUTO bootstrap，没有放宽公共 policy 或修改原 cold AUTO 类。稳定 Sv39 producer 仍由父 VSEQ 唯一持有，精确 fault/首次 CBOAck 覆盖检查保持原父 body。

## 7. 独立Uncache复查交接

对统一Uncache实现的独立复查发现并交实现agent处理两项：终态zero之后再发合法数量范围内迟到分段会逃过仅pending的终场检查；两个SQ flag旧记录与9B ROB环绕会造成多owner匹配，且同拍enqueue可能覆盖旧请求。实现agent已增加终态owner边界，并改成先缓存本拍enqueue、消费旧请求后替换并清相反flag。相关soft及最终结果见统一Uncache专项review。

本轮一度怀疑通知应读取D+2顶层enable，完整追踪后撤销：MemBlock内部csrCtrl本身已延迟2拍，与通知同延迟，所以应读取内部Uncache实际D消费sample的顶层输入enable；外部TL D经buffer的时延不固定，不是该消费边界。实现未按该误判改动。cacheable zero最终STA0必须在真实SB成功之后，亦已明确保留严格时间上限，不能沿用普通Store提前地址完成的假设。

## 8. Plan 对齐检查

对应plan：

- `AI_DOC/plan/rm_plan/do/memblock_cmo_blackbox_rm_coding_plan_20261103.md`
- `AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md`
- `AI_DOC/plan/test_framework/plan/do/memblock_l2_grantdata_first_line_weighted_error_plan_20261110.md`

| Plan要求 | 当前实现 | 结论 |
|---|---|---|
| zero写权限、nonzero读权限，Store异常外显 | 核心明确permission_write与exception分离；调用方独立翻译 | 核心对齐，翻译接入由主agent专项核验 |
| 最终op、data0规范化、非零无数据 | 独立CMO类别及规范字段；nonzero不调用效果 | 对齐 |
| zero64B通过既有底座保留SC副作用 | 整组8B子item预检查后复用commit_memory_effect | 对齐 |
| 实际SQ/SB/TL/Uncache请求证明 | cacheable/nonzero本观察器，Uncache统一观察器 | 源码对齐，正常及Uncache错误端到端通过；nonzero故障恢复排空受已复现集成阻塞限制，RTL与上游时序契约责任待复核 |
| denied/corrupt统一处理不重复实现 | CMO核心仅消费预期异常与已接受段mask | 对齐 |
| RM不生成redirect/epoch/UID+1恢复 | 核心只比较最终STA属性和完整动态身份 | 对齐 |
| 软件与真实DUT验证 | 新soft/preset/CSV；实际运行由主agent执行 | 代码及各已执行验证分别记录；CMO故障后的完整排空组合存在已复现阻塞，责任边界待独立复核，不能宣称全部DUT组合通过 |

## 9. 实现与 Plan 不一致项

### 9.1 静态 Uncache 最终比较适配与剩余测试边界

Plan 要求真实请求、异常及最终数据均比较。当前已经接入 `mem_ut/ver/ut/memblock/tc/src/tc_rm_cmo_uncache_final.sv`：该 testcase 继承 `basicTest` 并仅扩展 check_phase，在原 DISABLED 静态 Sv39/CSR 拓扑结束后，要求所有 writer 均实际走 Uncache、zero 请求/终态/效果已验证、DCache 与 SBuffer responder 均已 drain，才通过 readonly 复制真实 overlay 并复用 `compare_overlay_store_entry()` 逐项比较已有 expected entries。没有在 DISABLED 拓扑插入不支持的 CHECK_STORE marker，也没有双启动 CSR producer。

已取得真实结果：Uncache zero 正常场景最终11条 entry 通过；denied 分段停止场景最终5条 entry 通过，且对应统一通知比较与完整测试通过。来源及适配的完整源码、中文伪代码和实际边界分别见同目录 `memblock_cmo_d_error_rm_integration_review_20261111.md` 与 `memblock_uncache_error_implementation_review_20261111.md`，专项真实日志由 root 汇总。

该适配只支持所有 writer 实际 Uncache 且无 SC 无写义务的终场范围；任何 cacheable writer、未完成的分段/效果、responder 未 drain 或实际字节缺失均报错，不能泛化成绕开普通 cacheable Store-table 的读取门槛。最终 entry 证明确定写集合的最终值；未触及字节和首错后的停止仍依靠真实段检查及终态后迟到请求审计。isolated cold 与原年轻 Store 恢复问题按第6.9节分开记录，其余尚未执行组合仍保留测试风险，不能据已通过的 zero 子集宣称全部覆盖完成。

## 10. Plan 未说明但 Coding 落实的细节

### 10.1 同拍SB双端新插入和复位历史扫描

功能：同line双端插入可指向同一slot且两者canMerge均为0；每事件增加generation会清掉第一端zero来源。源码以本批allocated_slots对同slot只分配一次，证明来源保留。历史取消仅在reset代次变化边界执行一次，避免长reset逐拍扫描累计账。Plan以实际slot证明为主，未穷举这两个编码时序细节。源码位置为第5.3章observe_batch完整片段；其中文伪代码已逐序说明。结论：回写plan的请求时序边界有助后续维护，但不新增公共机制。

### 10.2 请求禁止路径与晚于completion的证据

功能：所有SQ->SB接受先识别CMO，漏wline/vecValid也建账报错；validate_no_requests覆盖Uncache和前置fault非法cacheable路径。zero成功和TL接受必须早于最终STA0，避免迟到合法请求补足非法早完成。源码与伪代码见第5.3至5.5章。原因是coding时按真实payload发现禁止路径负向漏检。结论：保持当前实现，并在plan中补充这些负向验收。

### 10.3 冷目标故障场景与覆盖防空检查

普通directed的年轻同line访问先冻结CLEAN，故另增独立cold场景隔离故障注入前提，并在真实完成后检查status和first_kind/error state。Plan主路径未说明测试发射顺序对首次随机的影响；实际场景、源码和中文伪代码见第6.8章。该细节应回写plan验收前提，不改变首次CLEAN永久策略或公共参数。

### 10.4 核心成功消费后的增量STA0审计

Plan已有同owner第二次完成识别与无副作用要求；原实现只覆盖当前UID查询和核心再次push的负向，未覆盖程序游标已经推进后才到达的重复。第5.8章补齐按终态owner/ROB的增量审计、注册时单key补查与既有fault normal尾拍例外，不新增公共参数或恢复行为。源码、中文伪代码及调用关系均在该章。

## 11. 非本次修改的逻辑分析

### 11.1 git status 对比结论

已运行`git status --short`并与本review范围比较。本主题直接覆盖新增生产权限TC、cold故障builder/vseq/两个cfg、basicTest能力白名单条目以及：`rm_ls_core.sv`、`rm_cmo_request_observer.sv`、两个CMO soft文件、directed builder/vseq、四个单op与两个Uncache preset，以及本轮两份CMO CSV。

| 类别 | 文件/目录 | 本次处理与原因 |
|---|---|---|
| 同功能独立组件 | `rm_dcache_error_observer.sv`、`rm_uncache_error_observer.sv`、对应soft、`subagent/cmo_path_agent`、connect | 统一错误观察和被动采样，由负责agent专项review；本轮只交接独立发现 |
| 新增测试入口 | `soft_test_tc_rm_cmo_translation.sv`、tc_pkg新增include、basicTest CBO白名单 | 本文第6.1至6.7章覆盖；生产权限TC真实VCS 92项通过，其余真实场景结果由主agent更新 |
| 主集成代码 | `memblock_rm.sv`、env/package/filelist、readonly、DUT WB observer、sync/CSR/other_ctrl监测、tc薄入口 | 主agent负责接入与实际编译；核心API调用边界在本文说明，完整集成另行核验 |
| 行历史相关实现 | `mem_base_sequence.sv`、l2_line_history soft/vseq、env plus、seq_csr_common、default及L2相关cfg、L2行历史CSV/review | 另一负责agent的token/beat/首行随机与Uncache producer实现，不能以本CMO核心review代替 |
| 既有来源软件测试 | `soft_test_rm_provenance_followup_sequence.sv`、`soft_test_rm_uncache_d_error_ledger_sequence.sv` | 被统一错误事实结构变更影响，单独运行验证 |
| 测试配置变更 | CSR scalar stress cfg、Sv39/strict NC/100k cfg、FCOV错误cfg、fault2 cfg | 参数兼容迁移方向，真实场景行为需对应回归，本文不扩大功能通过结论 |
| 文档同步 | responder flow、统一错误/旧sticky/firstsplit plan、fault2/fault3分析文档 | 前序工作或其它组件文档同步，未在此重写其结论 |
| 仿真/生成产物 | 当前status未见本轮新生成RTL作为源码修改 | 运行日志/临时lint文件不纳入实现正确性结论 |
| 未知或另行review | 当前status中上述未由本文逐函数展开的文件 | 应由其owner或主agent整合review覆盖；本文件不对共享工作区全量脏改动作通过判断 |


### 9.4 冷 CMO 故障后的排空阻塞证据与责任边界

详见 [V2 冷 CMO 错误响应后的 SQ 重入与排空阻塞分析](../../../analysis/testcase_flow/memblock_cmo_cboack_fault_sq_reentry_flush_stall_20261111.md)。实际波形已确认故障 WB 后 idle/req 重入、年轻 Store 的普通 Uncache 请求、保留 CMO 身份的迟到 normal 完成及 rdataPtr 越过恢复 Store；原四指令 denied/corrupt 场景未通过最终 CHECK_STORE。框架 pendingst 到 DUT 的撤销时间仍需与真实后端接口契约独立核对，因此本文不将其直接登记为已正式确认的 RTL 缺陷。三指令 isolated 的结果由主 agent 按日志记录，不能替代原场景的 Store 恢复和排空验收。

## 最终验证收口（2026-11-11）

真实统一VCS编译、软件检查及必要DUT验证已完成；具体命令、结果与未覆盖项见同目录`memblock_cmo_d_error_rm_integration_review_20261111.md`第5章。isolated三指令DISABLED场景denied/corrupt均确认实际CBOAck首次错误并完成CMO及两条Load比较，0 error/0 fatal；不覆盖Store或flushSb排空。原四指令恢复场景继续BLOCKED，详见`AI_DOC/analysis/testcase_flow/memblock_cmo_cboack_fault_sq_reentry_flush_stall_20261111.md`。该阻塞不通过RM跳过检查规避，也不登记为整例通过。
