# V2 CMO、denied/corrupt 与行历史三方案最小适配评审

- 日期：2026-11-10。
- 后续状态：原问题及本轮复审新增契约已修复，三方案可coding；最终基线与闭环结论见 `AI_DOC/plan/rm_review_doc/undo/memblock_cmo_d_error_three_plan_fix_recheck_20261110.md`。以下原始结论/行号/hash保留作为历史基线，不代表修复后的当前状态。
- 版本与分支：V2 / `mem_ut_uvm_v2`。
- 评审方式：独立 subagent 源码对照评审，主 agent 复核关键结论与跨方案契约。
- 结论：`NEEDS_PLAN_REVISION`，3项P1功能问题、2项P2契约或改动范围问题，当前版本不宜直接执行 coding。
- 本轮只新增评审文档；三份方案保持评审基线，未修改实现、未运行编译或仿真。
- 评审原则：先保证真实请求、响应错误、数据效果与比较完整，再复用已有机制缩减适配；不以减少代码为由取消用户要求的检查。

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| CMO / CBO | 四类缓存块操作，按最终指令类型比较 | CMO RM方案 | 原始inval最终改为flush时检查flush |
| denied / corrupt | 实际总线响应的拒绝/损坏位 | DCache、Uncache D响应 | denied优先于同响应的corrupt异常分类 |
| beat / fire | 一次数据传输 / valid与ready同时成立的接受事件 | GrantData及A/D通道 | 首beat已被Load消费时末beat可能仍未握手 |
| selected_state / activation | 每行首次冻结的注入选择 / 某次实际错误对访问适用的时间事实 | 行历史及响应历史 | 选择可永久固定，旧响应错误不能无条件套到新正常refill |
| owner / epoch | 动态指令身份 / 该实例代际 | UID、ROB、SQ/LQ、dynamic epoch | 同地址两次zero不能互相借用请求 |
| source / generation / token | 总线槽身份 / 槽使用代次 / 唯一传输编号 | 响应与被动请求记录 | source复用后旧响应不能匹配新指令 |
| refill / metadata | 缓存行回填 / 当前行的有效与错误等元信息 | DCache MissQueue/MainPipe/LoadPipe | 新的正常回填可写入error=0 |
| reqSource / MSHR | 已接出的请求来源字段 / 缓存未命中事务槽 | DCache A.user及MissQueue | Store来源保守正常，Load来源按权重注错 |
| backing / overlay / entry | 初始字节 / 程序顺序期望字节覆盖 / 最终比较的地址与来源记录 | rm_ls_core的现有内存状态 | 最终entry值按当前overlay生成 |
| SC obligation / shadowed | 失败SC的无写检查记录 / 已被后续合法写覆盖的字节 | sc_no_write_obligations及shadowed_mask | SC失败后zero覆盖同字节，不能继续要求该字节保持SC前原值 |
| backend / NC / MMIO | 模拟内存后端 / 非缓存普通访问 / 等待响应的设备或强顺序访问 | shared_mem_access_task及Uncache路径 | 响应注错前写已接受，与后端拒绝未写是两种事实 |
| FIFO / adapter / consumer | 事件队列 / 事实归档适配层 / 队列消费者 | env已有mon2rm FIFO和RM get端口 | 同一FIFO应只有一个归档消费者 |
| watermark / cursor | 已完整发布或归档到的采样水位 / 单调消费位置 | 被动事实服务、RM程序顺序位置 | 水位覆盖后缺记录应失败，不能永久等待 |
| P1 / P2 | 应在coding前修订的直接功能问题 / 需明确的数据契约或适配范围问题 | 本文问题编号 | 不把未执行计划当作当前实现缺陷 |

## 2. 范围、基线与结论

本评审针对方案本身能否正确落地，不将“代码尚未实现该计划”列为缺陷。源码用于确认现有功能、接口和可复用机制；辅助查看Scala未启动或更新V2 RTL知识库。

| 方案简称 | 仓库路径 | 评审时行数 | SHA-256 |
|---|---|---:|---|
| CMO RM方案 | `AI_DOC/plan/rm_plan/do/memblock_cmo_blackbox_rm_coding_plan_20261103.md` | 805 | `a11feba826f27c66b3514acab9e7c36b068752393fe68a034677b7ed694e8f70` |
| 统一错误方案 | `AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md` | 536 | `e06a962480bc8897cdd58342f904a36c54f2190dfcca87ed41630a8509155951` |
| 行历史方案 | `AI_DOC/plan/test_framework/plan/do/memblock_l2_grantdata_first_line_weighted_error_plan_20261110.md` | 463 | `0c613cff0c2d8ce1e84e0a754e4f032de55ce0d7976cac85d0a3705d52d16c46` |

| 优先级 | 问题 | 主要影响方案 | 功能影响 |
|---|---|---|---|
| P1-1 | 错误只在末beat激活 | 行历史、统一错误 | DUT已正确报错，RM却预测正常 |
| P1-2 | 永久首次错误污染新正常访问 | 行历史、统一错误 | 正常refill后的Load被错误判fault |
| P1-3 | 新写底座遗漏失败SC覆盖处理 | CMO、统一错误 | 合法zero/MMIO写触发SC终检误报 |
| P2-1 | NC后端拒绝后的数据效果未闭环 | 统一错误 | 通知匹配仍可能出现后续数据误报 |
| P2-2 | 未优先复用reqSource做基础分类 | 行历史、统一错误 | 将额外混合注错覆盖变成基本初始化前提，扩大改动 |

## 3. 需要修订的问题

### P1-1：仅末 beat 激活错误，会漏掉前 beat 已消费错误的 Load

- 计划位置：行历史方案 308、312、327；统一错误方案 235—242。
- 源码证据：`src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:817-822` 每次 D.fire 均产生 refill_to_ldq；同文件 930—938 按对应半行发布 firstbeat_valid/lastbeat_valid 与错误位；`DCacheWrapper.scala:715-743` 直接 D-channel forward 只匹配当前 beat，`761-777` MSHR forward 只要求目标半行 beat 有效；`LoadUnit.scala:1352-1357` 消费这些错误生成异常。
- 场景：第一 beat 已接受且 denied/corrupt，目标 Load 经该 beat 转发完成写回；第二 beat 被 backpressure 延后。即使 RM 最后才处理，用较早 WB sample 查末 beat 完成 activation，仍得到无错误。
- 影响：DUT 正确 fault 被 RM 误判；fault 入口若等待正常数据事实还可能卡住。
- 最小修订：保留最后 beat 的完整响应完成历史，同时在每次真实 D.fire 留存 beat 的 sample、覆盖字节、raw 位与已有 response token/MSHR generation。RM 以本次实际消费的 beat/转发事实形成期望，不用“全响应完成 sample”充当“目标数据已可消费 sample”。无需另建首次选择表或主动调度器。

### P1-2：永久首次错误 activation 不能代表后续本次 Load 的错误

- 计划位置：行历史方案 159—160、172、327；统一错误方案 143、237。
- 源码证据：`MainPipe.scala:442,594-595` 获取本次 refill 错误，`911-914` 在 miss 更新 error metadata 并写入本次错误值；`loadpipe/LoadPipe.scala:320,550-558` 只返回当前命中行 error metadata。现有 RM `observer_apply_load_d_error_expectation()` 按 PA/sample 查行（`memblock_rm.sv:1230` 起），不能证明具体访问消费了旧错误。
- 场景：Load 错误 refill 后，该行被 Probe/eviction 移出；同 PA 普通 cacheable Store miss，按新方案强制 clean refill；新 refill 在 metadata 写 error=0。之后 Load 正常命中。首次行选择仍固定 ERROR 合理，但历史首次 error activation 永远命中是不合理的。
- 影响：正常 Load 被永久预测为 fault，普通 Store 排除与 RM 行错误策略无法同时正确。
- 最小修订：首次 selected_state 始终保留，独立区分“本次响应事实/实际消费版本”。对本次有 token 的消费优先使用其 raw；cache hit 必须关联仍适用的 refill/metadata provenance。clean 替换 refill、失效及 reset 结束旧消费版本的适用范围，但不重抽首次选择。first-error 摘要只能诊断，不能单凭 PA 和 `first_error_sample <= WB_sample` 决定异常。可复用计划已有 source/token/被动来源观察，不复制 cache 数据模型。

### P1-3：新增 zero/MMIO 字节写入底座遗漏既有失败 SC 覆盖标记

- 计划位置：CMO方案 490—498；统一错误方案 383、390—391、409—414。
- 源码证据：`rm_ls_core.sv:574-590` 的 `mark_sc_obligations_shadowed()`；同文件 `593-617` 的 `commit_memory_effect()` 在更新字节前调用该 helper；`memblock_rm.sv:2207-2230` 终检对未 shadowed 的失败 SC 字节要求保持原值。
- 场景：失败 SC 留下无写承诺，然后同地址合法 cacheable zero；或已确认的 Uncache zero/MMIO 写覆盖其字节。计划直接更新 overlay/entries，未更新 obligation。
- 影响：后续合法写被误判成失败 SC 产生了写效果，普通零写正常场景即可误报。
- 最小修订：zero 构造八个 8B 子 item，整组完成预检查后统一调用既有 `commit_memory_effect()`；Uncache 确定性效果也复用相同写底座。若为了原子预构造必须抽公共 helper，也必须完整保留 SC shadow、byte overlay、entry 来源三个副作用，禁止复制仅 byte/entry 的第二套实现。

### P2-1：NC Store 的 backend 拒绝后数据契约与提交 overlay 冲突

- 计划位置：统一错误方案 169—173 与 414；NC Store验收项 仅要求 NC 提交不回滚，未说明 backend 不写后的数据期望。
- 源码证据：`mem_base_sequence.sv:1626-1632` 后端拒绝时立即返回，不入写批次；同文件 `1653-1666` 仅正常写入队。`rm_ls_core.sv:936-1000` 的普通 `commit_store()` 已提交 overlay；`memblock_rm.sv:2143-2185` 最终 Store-table 遍历该 entry 并比较实际字节。
- 场景：NC Store 正常提交后，下游后端范围/访问检查拒绝写，返回 denied；DUT 只发后台 bus-error，entry 回收后后续 Load/最终内存没有该新值，但 RM 仍保留该提交值。
- 影响：通知比较通过仍产生数据误报；后续相同地址被新 Store 覆盖时，简单回滚也会破坏较新的程序顺序结果。
- 最小修订：明确区分注入 denied（现有顺序 backend 已接受，保留 overlay）与 backend 确定拒绝（不写）。若本轮要支持后者，独立 NC 后台服务按请求 token、原字节版本/后续覆盖关系更新可比较数据效果，terminal 不回滚，不能按地址直接删除 overlay 或 entry。若功能范围只支持本轮参数化注入，最小实现可保证 NC 产生地址均在后端有效范围并在建响应时将真正 backend 拒绝明确作为激励/配置错误，不能同时声称支持该分支且继续正常最终数据比较；该取舍需在方案明确。

### P2-2：注入基础分类忽略已接出的 reqSource，强制重建所有 MSHR 来源过重

- 计划位置：统一错误方案 129—133；行历史方案 102—104、267—268。
- 源码证据：`MissQueue.scala:277-284,855-866` 已设置 CPULoadData/CPUStoreData/CPUAtomicData/L1DataPrefetch；`agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv:29`、`xaction.sv:24`、`monitor.sv:107`、`tb/dcache_agent_connect.sv:27` 已采集/连接 `auto_inner_dcache_client_out_a_bits_user_reqSource`，只是 monitor 发布块注释。MissQueue 合并 Store 后 req.source 转为 Store（`612-623`），输出也以 Store 分支优先。
- 影响：将全 MSHR 分配/合并消费者重建和“接受前所有混合消费者必须可分类”列为基础前置，增加内部探针、代次和同拍调度适配；这些并不是用户必须要求的混合注错覆盖。
- 最小修订：基础路径直接使用既有 reqSource：Store-origin（包括其混合消费者）保守 clean；Load/Atomic/Prefetch-origin 使用既有权重；source generation 复用 A/D 生命周期，未知编码明确诊断。这样仍支持用户要求的各类合法错误注入并保护 Store/zero；只会减少部分合并场景的错误覆盖。若明确要保留 A.fire 前混合 Load 可注错的扩展目标，再补最小 `has_demand_consumer` 事实，而非强制复制整个 MSHR 来源模型。需要同步修改三份方案中我们自行加入的混合请求验收；不应删掉请求 monitor 的真实请求证明。

## 4. 功能保留与最小适配落点

| 功能 | 评审结论 | 最小适配要求 |
|---|---|---|
| 普通cacheable Store/cacheable zero不注错 | 必须保留 | 生产端硬过滤，不改成RM忽略错误；优先用现有reqSource |
| CMO真实请求证明 | 必须保留 | 对应monitor采集并入队，owner/token关联；STA完成不能替代真实请求 |
| cacheable zero实际DCache通路 | 必须保留 | SQ wline→真实SB slot→DCache接受和成功链；允许命中、合并和合法重试，不要求TL CBO或TL Acquire必现 |
| 内部只读观察 | 必须补必要信号 | 当前无现成内部subagent可直接覆盖这些接口，可新增固定被动cmo_path；只采必要握手/身份，不复刻SB/CMO状态机 |
| Uncache zero | 必须保留完整支持 | 最多8个8B段；首错停止；出错段接受与拒绝分开；部分效果、STA与通知各自比较 |
| CBOAck/Uncache错误事实 | 必须保留 | 包括正常响应和错误响应的完成历史，不能临时context清除后丢失 |
| 原子CSR门控 | 必须保留 | 使用独立输入enable；enable=0继续正常数据比较，不复制另一套数据源 |
| Uncache错误通知 | 必须保留 | 比较valid/bits、地址、次数、两拍时序与reset代际；不模拟BEU |
| FIFO与事件归档 | 复用既有env接线 | dcache/sbuffer/other_ctrl均已有mon2rm FIFO和RM blocking_get端口；适配现有唯一consumer，不竞争get |
| monitor发布 | 恢复最小必要事件 | 不恢复整套无关事务；避免新事实经既有pack路径成为无对应actual的通用scoreboard expected流量 |
| source代次 | 复用现有A/D生命周期 | 注入分类不需要另外复制整套MSHR分配/合并模型；实际消费正确性仍需最小来源事实 |
| 字节效果 | 复用commit_memory_effect | 保留SC shadow、byte overlay和entry三个既有副作用；先整组预检查，再应用8B子效果 |
| 统一CMO错误入口 | 必须唯一实现 | 响应映射、Uncache部分效果、通知checker不在CMO plan再做一份 |

注入资格与RM实际消费证明是两件事：`reqSource`可以缩减基础注入分类，不能代替P1-1/P1-2所需的本次访问/响应来源关联。对于SBuffer slot关联，优先采集接受拍真实`writeReq.wvec`等已作出的选择；若采RegNext `accessIdx`，按已有拍号对齐，不再实现一套insert/merge选择算法。

当前 cacheable zero成功链与已消费owner迟到检查有功能依据，不因其需要新增少量只读探针就删除。CMO方案已有安全回收/水位要求，本轮未确认它存在额外独立缺陷。

## 5. 关键源码支撑与功能调用关系

本章只摘录当前源码的关键分支，不表示已经实施建议改动。源码简称`MainPipe.scala`、`MissQueue.scala`位于`src/main/scala/xiangshan/cache/dcache/mainpipe`；`LoadUnit.scala`位于`src/main/scala/xiangshan/mem/pipeline`；`rm_ls_core.sv`位于`mem_ut/ver/ut/memblock/env/src/memblock_rm`。

### 5.1 Load错误消费早于完整响应完成

**抽象功能描述：** LoadUnit在当前Load实际取得D通道或MSHR转发数据后，把对应响应原始错误转换成该指令异常，不负责等待整行回填完成。

源码位置：`src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:1352`，S2响应错误分类分支。

```scala
  s2_real_exceptionVec(loadAccessFault) := (s2_exception_vec(loadAccessFault) ||
    s2_fwd_frm_d_chan && s2_d_denied ||
    s2_fwd_data_valid && s2_fwd_frm_mshr && s2_mshr_denied) && !s2_prf ||
    (s2_out.isMisalign || s2_out.isFrmMisAlignBuf) && s2_actually_all_mmio
    s2_real_exceptionVec(hardwareError) := (s2_exception_vec(hardwareError) ||
    s2_fwd_frm_d_chan && s2_d_corrupt && !s2_d_denied ||
    s2_fwd_data_valid && s2_fwd_frm_mshr && s2_mshr_corrupt && !s2_mshr_denied) && !s2_prf
```

中文伪代码：先保留当前更早异常；D通道转发命中denied或有效MSHR数据带denied时形成访问错误；corrupt且同来源无denied时形成hardwareError，预取按其抑制规则处理。此分支不等待最后beat；其数据/错误输入由DCacheWrapper的当前beat转发和MissEntryForwardIO提供。因此RM需按实际可消费数据时刻查询，不能以末beat完成代替。

### 5.2 新正常回填可结束旧错误版本

**抽象功能描述：** MainPipe在完成回填相关元信息更新时，写入本次回填的错误状态，供后续命中访问使用；它不保存测试框架的首次注入选择。

源码位置：`src/main/scala/xiangshan/cache/dcache/mainpipe/MainPipe.scala:911`，错误元信息写分支。

```scala
  io.error_flag_write.valid := s3_fire && update_meta && (s3_l2_error_wb.asUInt.orR || s3_req.miss)
  io.error_flag_write.bits.idx := s3_idx
  io.error_flag_write.bits.way_en := s3_way_en
  io.error_flag_write.bits.error := s3_l2_error_wb
```

中文伪代码：当前S3实际完成且更新元信息时，如果本次有L2错误或本次属于miss，就把本次错误状态写到所选行。新的正常miss回填可写入0；后续Load从当前命中行元信息读到正常状态。首次注入选择不需要改变，但旧实际错误的适用范围已经变化。

### 5.3 现有内存效果底座还维护失败SC检查

**抽象功能描述：** `commit_memory_effect()` 在合法写效果提交时统一维护已提交期望内存及后续检查所需的覆盖关系，供普通Store和原子写复用。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv:610`，地址/mask预检查通过后的效果落地分支。

```systemverilog
        mark_sc_obligations_shadowed(item, byte_mask);
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            expected_store_valid[item.pa_by_byte[i]] = 1'b1;
            expected_store_byte[item.pa_by_byte[i]] = data[(i * 8) +: 8];
        end
        effect_entry = new();
        effect_entry.capture_effect(item, source_kind, data, byte_mask);
        expected_store_entries.push_back(effect_entry);
        return 1'b1;
```

中文伪代码：先调用SC覆盖处理，把当前写地址与已有失败SC检查范围的重叠字节标记为已被后续合法写覆盖；随后写入期望字节及有效位；再由当前item、效果类别、data和mask构造最终比较entry并追加，最后返回成功。新增zero/MMIO效果不能只复制后两项而漏掉第一项。zero需要先完成所有子段预检查，才统一进入这条共享落地路径。

### 5.4 后端拒绝与响应注错的写副作用不同

**抽象功能描述：** `shared_mem_access_task()` 在模拟内存访问时先做后端可访问性检查，仅正常Store才进入写批次；它不负责决定指令同步异常或bus-error。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1625`，后端检查分支。

```systemverilog
    begin_shared_mem_sample($time);
    main_mem_access_task(addr, 1'b0, byte_mask, '0, main_corrupt, main_denied, main_load_data);
    corrupt = main_corrupt;
    denied  = main_denied;
    if (corrupt || denied) begin
        load_data = '0;
        return;
    end
```

中文伪代码：先推进共享内存当前采样的既定批次边界；再由后端读取检查地址和mask，取得拒绝/损坏结果；有后端错误就清返回数据并提前返回，后续Store写批次不会执行。正常检查通过后才接受写事件，之后`create_uncache_response_record()`额外注入的denied不会撤销该事件。NC同步终态不回滚与后端数据确实未写不能混成一个结果。

### 5.5 最小适配调用关系

| 阶段 | 现有对象或计划入口 | 应承担的功能 |
|---|---|---|
| 请求采集 | 现有dcache/sbuffer monitor及新增必要内部被动观察 | 只保存真实fire、身份与路径，不生成RM期望 |
| 事件归档 | 已有env FIFO和RM唯一get consumer | 按新增事件建立owner/token引用，发布完整采样水位 |
| 注入资格 | 计划classify_response_error_injection | 先用reqSource确定基础资格，再查唯一首次行选择；不复制MSHR算法 |
| 实际错误留存 | 响应创建与D.fire事实发布 | 分清每beat可消费事实和完整响应完成，保存正常替换版本 |
| CMO检查 | observer_compare_cmo_requests及统一CMO错误入口 | 核对实际请求，关联本次响应，比较STA/terminal，结果各消费一次 |
| 数据落地 | 既有commit_memory_effect及薄封装 | 保留SC覆盖关系、overlay与entry，不新建第二套字节写模型 |
| 后台通知 | 独立Uncache错误通知checker | 按D.fire和有效enable比较顶层valid/bits，不阻塞程序游标 |

## 6. 复查与验收建议

修订后至少以以下定向情形重新review，不能仅做格式或函数名检查：

1. 首beat错误，第二beat长时间反压，Load在第二beat前已fault写回；延后RM处理也得到相同预期。
2. 错误回填后移出，再由排除类Store正常回填，同PA Load正常命中；首次选择不变但本次不误加异常。
3. 失败SC后执行cacheable zero、Uncache zero部分写及MMIO已接受写；只对实际覆盖字节结束原SC无写检查。
4. NC响应注入denied但写已接受，与backend拒绝未写分开；验证同地址更年轻写不会被后台处理错误回滚。
5. reqSource四类、Store合并来源、source复用、未知编码；基础分类不能要求重建全部MSHR。
6. 原有CBO真实请求、异常/正常互斥、Uncache分段停止、通知次数/地址/两拍时序、reset/旧epoch检查继续保留。

编码后再执行现有框架自测、真实DUT定向检查和必要标量/原子回归；按规则同步CSV。当前没有执行结果，不登记通过。

## 7. Plan对齐检查

三份方案保留了用户要求的主要功能，但上述P1影响实现后正确性，P2需要收敛数据契约和基础适配范围。建议先修订三份方案，再做一轮针对问题场景的复核；本轮没有覆盖方案原文，便于保留评审基线。

## 8. 实现与Plan不一致项

本轮没有执行coding，不评价本轮实现与Plan的一致性。本文当前源码仅作为计划设计依据，尚未实现的新功能不被登记成实现遗漏。

## 9. Plan未说明但Coding落实的细节

本轮没有coding，此项为空。复用既有reqSource/FIFO/写效果底座属于方案修订建议，并非已落地实现。
