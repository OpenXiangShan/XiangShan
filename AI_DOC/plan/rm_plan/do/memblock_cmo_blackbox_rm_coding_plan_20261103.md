# memblock CMO 黑盒 RM Coding Plan
- 状态：`DO`；编码、文档同步及必要验证完成（2026-11-11）；保留已复现的CMO故障后完整恢复排空集成阻塞，不表示全部DUT组合验收通过。
- 创建日期：2026-11-03
- 最近修订：2026-11-10，按三方案评审修复zero写底座的SC覆盖副作用，收敛既有FIFO/reqSource复用，并接入统一错误来源版本契约。
- 评审基线：`AI_DOC/plan/rm_review_doc/undo/memblock_cmo_d_error_three_plan_minimal_adaptation_review_20261110.md`；本文原始方案段保留用于追溯；当前实现与验证状态以文末实施收口和对应review为准。
- 适用分支：`mem_ut_uvm_v2`
- 适用版本：V2
- 计划类型：RM/scoreboard 参考模型 coding plan
- 目标源码：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`
- 目标源码：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`
- 行为依据：`AI_DOC/analysis/rtl/v2/flows/cbo_instruction_memblock_backend_flow.md` 中“mem_ut RM 黑盒可观察行为约束”章节
- 本计划包含 RM 所需被动 monitor、请求事实队列与只读关联适配，不包含测试框架激励、发射、调度、redirect 恢复、重发或资源分配设计。
- denied/corrupt 唯一实现方案：`AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md`（下称“统一错误方案”）。CMO 只调用其共享入口；不得重复实现错误查询、异常位映射、Uncache zero 部分写效果或 Uncache 错误通知比较。
- 配套测试框架方案：`AI_DOC/plan/test_framework/plan/undo/memblock_cmo_active_flow_coding_plan_20261103.md` 第 8、9 节；`flushAfter` 构造、exception redirect、`UID+1` 恢复和 epoch 重建均由该方案负责。
## 1. 术语表
| 术语 | 含义 |
|---|---|
| CBO | Cache Block Operation，本计划覆盖 `cbo.zero`、`cbo.clean`、`cbo.flush`、`cbo.inval`。 |
| CMO | Cache Management Operation，本文用于指代最终进入 MemBlock/StoreUnit 的 CBO 类操作。 |
| 最终 `fuOpType` | Decode 转换完成后、最终进入 StoreUnit 的操作类型；RM 必须以该值分类。 |
| `inval -> flush` | 原始 `cbo.inval` 可能被 Decode 转换为最终 `cbo.flush`；RM 不保留原始编码优先级。 |
| zero | 最终 `fuOpType` 为 `cbo.zero` 的 CMO。 |
| non-zero | 最终 `fuOpType` 为 `cbo.clean`、`cbo.flush` 或 `cbo.inval` 的 CMO。 |
| 黑盒 RM | 根据被动采集的真实接口握手、独立权限/响应事实及最终写回建立期望，不复制 DUT 内部 cache 和流水算法；最小内部接口探针只补足端口类型与 owner 证据。 |
| normal pass | 当前动态 CMO 以无异常最终完成，且通过 owner、写回属性和 epoch 检查。 |
| fault terminal | 当前动态 CMO 以异常结束，不允许同时获得 normal pass。 |
| final STA0 completion | 成功 CMO 的 StoreQueue 特殊写回，或 fault CMO 的 owner-bound STA0 异常完成；普通 STA/STD issue feedback 不能替代它。 |
| `flushPipe` | 最终写回属性；成功 zero 期望为 0，成功 non-zero 期望为 1。 |
| `flushAfter` | 测试框架根据成功 non-zero CMO 写回的 `flushPipe=1` 构造的 redirect 语义，保留当前 CMO、清除年轻动态实例；不是 DUT 写回上的独立字段。 |
| 静态 UID | Main table 中稳定的程序顺序编号；测试框架负责计算 redirect 的 `UID+1` 恢复边界，RM 只用 UID 匹配当前比较对象。 |
| dynamic epoch | 同一静态 UID 在 redirect 前后动态实例的代际标识，由测试框架维护，RM 通过 readonly 状态读取并匹配写回事实。 |
| owner | 由 UID、ROB、SQ 和 dynamic epoch 共同确定的动态 CMO 身份。 |
| stale completion | 来自旧 epoch 或已取消 owner、与 readonly 当前动态身份不符的完成事件；RM 不自行计算 redirect 范围。 |
| duplicate completion | 同一个 live owner 已消费最终完成后再次到达的完成事件。 |
| backing | `backing_valid/backing_byte` 保存的原始可读取内存字节。 |
| expected store overlay | `expected_store_valid/expected_store_byte` 保存的程序顺序可见写效果。 |
| expected store entry | `expected_store_entries` 中用于最终 Store-table 对比的地址/字节掩码与来源记录；最终字节值仍读取当前 overlay，不将更老 entry 的零值当最终值。 |
| 请求事实队列 | monitor 对真实 `valid && ready` 请求产生的不可变记录；DCache、Uncache 分路径保存，经有界 adapter 索引后供 RM 只读取得。 |
| producer watermark | monitor 已完整发布到的 sample；RM 只能把所有相关 producer 均覆盖的 sample 当作完整证据边界。 |
| request token | 本次真实请求接受的单调标识，联合 reset generation、路径、source generation 防止 source/地址重用误绑。 |
| source generation | 同一路 TL source/Uncache slot 每次重新占用的代际；source 本身不等于 CMO owner。 |
| path fact | SQ 到 SBuffer 的 `wline`、SQ 到 DCache 的 `cmoOpReq`、SQ 到 Uncache 的分段请求等被动身份与类型事实；它们是请求证据，不模拟 SQ 状态机。 |
| segment | Uncache zero 对同一 64B line 发出的一个 8B 零写段，合法段号为 0 至 7；错误后仅保留已实际接受段的确定性效果。 |
| backend_write_accepted | 统一错误方案记录后端按既定提交策略已接受该段写效果的事实；不等同于 D 响应成功，不直接复制最终内存值。 |
| 主表 `src_0` | 当前测试框架静态主表中的源操作数字段；STA 用它与 `imm` 形成地址，现有普通 Store RM 也把它作为 data。CMO RM 在完成分类后不得从该字段取得数据语义。 |
| CMO data 归一化 | RM 根据最终 `fuOpType` 自行赋予 CMO 的规范 data：zero 为 0，clean/flush/inval 为无数据效果且内部占位值为 0。 |
| line | 本计划固定按 V2 当前 64B cache line 处理 CMO 地址效果。 |
| line base | `pa & ~64'h3f`，即 64B 对齐后的物理行首地址。 |
| 程序顺序读值 | `build_load_expected()` 先读 expected store overlay，再读 backing 的既有顺序。 |
| SC obligation / shadowed | 既有失败SC无写终检记录 / 被后续合法写覆盖的字节；CMO写必须经公共效果底座维护覆盖标记。 |
| error origin slot version | 统一被动observer保存的实际缓存槽错误来源身份；只关联实际refill/读取，不在CMO RM建立缓存模型。 |
| reqSource / FIFO consumer | 既有A.user请求来源 / env队列唯一get消费者；基础注入分类及事件归档直接复用。 |
| 无副作用 | 不修改 backing、expected store overlay、expected store entries、cursor 或 live owner 状态。 |
## 2. 目标与完成定义
### 2.1 总体目标
1. 在现有标量 Load/Store RM 上增加 CMO 黑盒语义。
2. RM 不建立 cache，只复用已有字节级 backing 和 expected store overlay。
3. RM 以最终 `fuOpType` 区分 zero 与 non-zero。
4. RM 独立区分权限命令类别和最终异常外显类别。
5. RM 在最终合法 terminal 后提交 zero 效果：cacheable 正常路径一次原子清零 64B；Uncache 路径由统一错误方案按合法请求及后端接受事实提交确定性分段效果，fault 不能一律视为无写入。
6. RM 对 non-zero 检查权限、真实 CMO 请求类型/路径/地址/owner、异常和最终写回属性，包括正常完成的 `flushPipe=1`；不修改数据，不等待或消费框架 redirect。
7. RM 对每个 CMO 使用 UID、ROB、SQ、dynamic epoch 做 owner 校验。
8. RM 对 stale/duplicate completion 报错或丢弃，但绝不产生数据副作用。
9. RM 保证 exception path 与 normal pass path 互斥。
10. RM 根据 readonly 当前 owner/epoch 排除旧实例 completion；年轻实例取消、恢复边界和新 epoch 的建立由测试框架负责。
11. RM 在读取普通 Store 主表 data 前先识别 CMO；CMO 不使用主表 `src_0` 构造数据期望。
12. 成功写回不能替代请求证据；RM 到当前 CMO 提交/终态位置后，从对应 DCache/Uncache monitor 队列取得并核对真实请求。
## 3. 设计边界
### 3.1 RM 没有 cache
1. 不新增 cache line 对象。
2. 不新增 set、way、tag 或 replacement 状态。
3. 不新增 cache hit/miss 判定。
4. 不新增 line residency 状态。
5. 不新增 line writeback 状态。
6. zero 的 64B 效果表达为 expected store overlay 中的 64 个零字节。
7. non-zero 不创建任何数据 overlay。
8. backing 继续表示初始或外部已初始化内存。
9. expected store overlay 表示程序顺序处理后的确定性写效果；Uncache zero 的 fault 部分效果仅在合法 fault terminal 确认后按统一错误方案应用。
10. load 继续使用 overlay 优先、backing 次之的读取规则。
### 3.2 复用现有状态
1. 复用 `backing_valid`。
2. 复用 `backing_byte`。
3. 复用 `expected_store_valid`。
4. 复用 `expected_store_byte`。
5. 复用 `expected_store_entries`。
6. 复用 `commit_store()` 的异常互斥和原子提交模式。
7. 复用 `commit()` 的程序 cursor 推进模式。
8. 复用 `build_load_expected()` 的程序顺序读值模式。
9. 复用 `compare_arch_exception()` 的架构 cause 比较模式。
10. 复用 `observer_process_current_commit()` 的只读事实收集入口。
### 3.3 新增状态的最小范围
1. 新增 CMO 分类、路径与最小请求证据摘要字段；完整请求事实由被动 monitor 队列保存；SBuffer实际合并导致一个DCache请求可引用多个已接受写token，不另建payload期望模型。
2. 只新增 CMO owner 字段。
3. 新增当前commit cursor的STA0 completion快照字段；被动请求队列独立持续采集，不要求其请求发生时已经轮到RM。
4. 新增当前owner及已完成owner必要的请求/completion已消费摘要，用于迟到非法请求检查；不新增redirect消费记录。
5. UID、ROB、SQ 和 dynamic epoch 优先从现有 readonly Main/status view 读取，不复制框架 owner 表。
6. RM 沿用按当前 commit UID 取 actual 的路径，不预建年轻恢复状态；monitor 必须在真实请求 fire 时独立留存历史，不能等 RM 开始比较才采样。历史按公共 owner/epoch 规则过滤，不能删除仍被响应关联和比较使用的事实。
7. 不新增可配置深度。
8. 不新增可配置延迟。
9. 不新增 cache 参数。
10. 不复制内部流水/状态机算法；只增加真实握手和最小身份/slot关联探针。
## 4. 行为矩阵
| 最终操作 | 权限命令 | 成功写回 | RM 检查的 `flushPipe` | 测试框架后续动作（不作为 RM 比较条件） | RM data 赋值与效果 |
|---|---|---|---|---|---|
| `cbo.zero` | write | final STA0 | 0 | 正常提交，无 CBO `flushAfter` | 不读取主表 data；`item.store_data=0`，cacheable 正常 completion 后整 64B 清零；Uncache 按统一方案应用已接受段 |
| `cbo.clean` | read | final STA0 | 1 | 构造并执行 `flushAfter` | 不读取主表 data；内部占位 `item.store_data=0`，不产生数据效果 |
| `cbo.flush` | read | final STA0 | 1 | 构造并执行 `flushAfter` | 不读取主表 data；内部占位 `item.store_data=0`，不产生数据效果 |
| `cbo.inval` | read | final STA0 | 1 | 构造并执行 `flushAfter` | 不读取主表 data；内部占位 `item.store_data=0`，不产生数据效果 |

上述 `flushPipe` 约束仅用于正常完成。fault CMO 比较预期异常与实际异常；前置翻译/权限 fault 没有请求和数据效果，Uncache zero 响应 fault 的部分数据效果按统一错误方案计算。exception redirect 的生成和恢复由测试框架负责。
### 4.1 异常矩阵
1. zero 权限检查使用 write：S1/S2 page permission 和 PMP/PMA command 均按 write 语义。
2. zero 的 PMP/PMA permission command 使用 write。
3. non-zero 的 S1/S2 page permission 使用 read 语义。
4. non-zero 的 PMP/PMA permission command 使用 read。
5. 所有 CMO 都从 STU 路径外显终态异常。
6. non-zero read permission 失败不能被 RM 改判为 write permission 失败。
7. non-zero read permission 失败可以落到 store 类 access fault 位。
8. fault terminal 不允许 normal completion。
9. normal completion 不允许非零终态 exception。
10. denied/corrupt 的获取、owner 绑定和异常期望统一调用 `observer_apply_cmo_d_error_expectation()`；本计划不另建错误 ledger 或异常映射 helper。
11. 普通 cacheable Store 与 cacheable zero 的 responder 禁止注入 denied/corrupt；Uncache zero 不是排除类，合法 `AccessAck` 支持 denied，禁止构造协议不合法的 corrupt。
### 4.2 真实请求与正常/异常完成的联合矩阵

本节规定“DUT 确实发出请求”的证据，权限/属性期望先由 RM 独立构造。monitor 对实际 fire 留档，RM 事后按当前程序顺序取得和比较；不预测 DUT 尚未执行的请求。最终 STA0 写回是完成证据，不能替代本节请求证据。

| 独立期望路径 | 必须被动采集的请求 | 请求集合检查 | 不应要求/不得伪造的请求 |
|---|---|---|---|
| cacheable `cbo.zero` | SQ `io.sbuffer(i).fire` 且 `bits.wline=1`、`vecValid=1` 的整行零写，以及SBuffer->DCache `io.dcache.req.fire`；由真实SB slot关联绑定 DataBuffer 的 SQ pointer 与只读 owner | 对应 owner 恰好一个合法整行接受，目标物理 line 正确；按 wline 核对零语义，再证明该token经真实SB slot进入DCache请求并获得对应成功完成；允许普通Store合并和DCache合法replay | 不要求 TL A 必然出现；DCache 可能命中或合并。不把 AcquireBlock 当 zero 类型，不要求 CBO A/CBOAck，也不注入 denied/corrupt |
| non-zero `cbo.clean/flush/inval` | SQ `io.cmoOpReq.fire` 与 DCache TL `A.valid&&A.ready` 的 `CBOClean/CBOFlush/CBOInval` | 同一 owner 的内部请求与下游 A 请求一一绑定；最终 op、line base、size、source generation、顺序和计数一致；D `CBOAck` 由统一方案关联 | 不走 Uncache 零写；不得按原始 inval 编码覆盖最终 flush 类型 |
| Uncache `cbo.zero` | SQ `io.uncache.req.fire` 的零写段，以及 `sbuffer_agent` 采集的 Uncache TL A 请求 | 正常为段 0..7 各一笔 8B 零写；异常只允许从段 0 开始的连续合法前缀，错误停止位置与已接受请求集合交统一方案核对 | 不伪造 CBO opcode/CBOAck；不能用普通同地址 Store 或 responder 预建 response record 代替实际请求 |
| 翻译/权限/PMA 等前置 fault | 记录对应 owner 没有上述数据/CMO 请求的完整观测边界 | 等相关 monitor watermark 覆盖 fault terminal sample 后确认空集合 | 不因需要“请求比对”而要求 fault 指令仍发请求 |

non-zero 的请求路径是 SQ -> DCache CMOUnit -> DCache TL CBO，不能依据 NC 标签自动改为 Uncache；是否形成前置 fault 由独立 PMA/PBMT/权限事实确定。V2 `StoreUnit.scala` 的 `s2_actually_mmio && s2_isCbo` 可产生前置 store access fault，此时应使用无请求分支。

### 4.3 当前 monitor 能力与需补齐的被动观测

| 当前代码 | 已有事实 | 本计划补齐内容 |
|---|---|---|
| `agent/dcache_agent_agent/src/dcache_agent_agent_monitor.sv`、`tb/dcache_agent_connect.sv` | DCache TL A/B/C/D/E 和 L2 done；transaction 发布目前仍为注释，未建立 CMO 请求队列 | 对 TL A CBO 的真实 fire 发布不可变请求记录并排队；保存 CMO source 本次占用代际、sample 和实际字段，不由 responder 代写 |
| `agent/sbuffer_agent_agent/src/sbuffer_agent_agent_monitor.sv`、`tb/sbuffer_agent_connect.sv` | `auto_inner_buffers_out_*` 是 64-bit Uncache TL A/D；此 agent 名称不代表 SQ->SBuffer；transaction 发布目前仍为注释 | 对真实 Uncache A.fire 建立独立请求队列，完整保存 data/mask/size/source/sample；D.fire 响应事实与统一错误方案共用，不重复建错误历史 |
| 当前顶层两个 monitor | 没有 SQ->SBuffer `wline`、SBuffer->DCache整行Store，或SQ `cmoOpReq`、SQ->Uncache 请求的完整 owner 证据 | 待新增 `subagent/cmo_path_agent`，只读采集这些 MemBlock 内部接口及最小 SQ/DataBuffer/SBuffer slot身份镜像；内部探针不塞入顶层 TL interface |
| 当前 RM observer | STA/STD 前半段和最终写回事实不能独立证明下游请求 | 从三类 monitor 的 analysis FIFO/历史索引取得联合请求集，先比真实请求再进入唯一 terminal commit |

新增内部agent的同一只读interface/monitor还承载统一错误方案所需的错误来源版本与实际消费观察；责任由统一错误方案定义，不在本plan重复展开。slot来源索引只存observer身份/错误事实，不进入`rm_ls_model_t`，不保存数据或模拟cache。

新增内部 agent 按 `memblock_agent_add_rule.md` 补齐完整 package/filelist、cfg、interface、xaction、driver、monitor、sequencer 和 default sequence 结构。该 agent 固定被动：driver/default sequence 不驱动 DUT，不启动 responder；同步`cfg/tb.f`、`tb/top_tb.sv`只读连接和env内部agent实例/一条必要事实流。外部dcache/sbuffer/other_ctrl已有mon2rm FIFO及RM get端口直接复用，不新建同义FIFO或竞争消费者。已存在 `cmo_path_agent` 时复用并扩展，不创建第二套同义探针。新增字段的中文注释遵循 `memblock_code_comment_rule.md`。

**源码证据边界：** `StoreQueue.scala:830-915` 描述 Uncache zero 8B 地址/零数据/全字节 mask 与逐响应停止；`:992-1044` 区分 SQ->SBuffer zero 和 `cmoOpReq`；`:1342-1356` 给出真实 SBuffer fire。`DCacheWrapper.scala:619-627` 定义 CMO request/response 字段；`MissQueue.scala:299-376` 给出单 outstanding CMO 请求、TL A 转换和 CBOAck。`mem/sbuffer/Sbuffer.scala:473-490,689-754` 的实际insert/merge slot选择与输出id用于连接zero token到DCache接受。只采集已存在接口握手和身份，不复制这些状态机。

## 5. 数据结构修改计划
### 5.1 `rm_ls_kind_e`
**抽象功能职责：** 让 CMO 从普通 Store 和 OTHER 中独立出来，避免套用标量 Store 的 1/2/4/8B 数据提交语义。
**真实源码落点：** `rm_ls_core.sv` 的 `rm_ls_kind_e`。
**详细中文顺序伪代码：**
1. 在 `RM_LS_KIND_STORE` 后增加 `RM_LS_KIND_CMO`。
2. 保持既有枚举值的相对语义清晰。
3. 所有 `case (item.kind)` 增加 CMO 分支。
4. CMO 分支不得回落到普通 Store 分支。
**状态副作用：** 只改变分类；不直接修改内存状态。
### 5.2 `rm_cmo_op_e`（计划新增）
**抽象功能职责：** 保存基于最终 `fuOpType` 得到的 zero/clean/flush/inval 分类。
**真实源码落点：** `rm_ls_core.sv`，放在 `rm_ls_kind_e` 后。
**详细中文顺序伪代码：**
1. 定义 `RM_CMO_OP_NONE`。
2. 定义 `RM_CMO_OP_ZERO`。
3. 定义 `RM_CMO_OP_CLEAN`。
4. 定义 `RM_CMO_OP_FLUSH`。
5. 定义 `RM_CMO_OP_INVAL`。
6. 只允许分类 helper 写入该字段。
7. 不根据原始指令编码回写该字段。
**状态副作用：** 无；该枚举只用于冻结最终操作语义。
### 5.3 `rm_ls_program_item_t` 字段扩展
**抽象功能职责：** 在单个 RM item 中冻结 CMO 的最终操作、owner、权限类别和 completion 期望。
**真实源码落点：** `rm_ls_core.sv` 的 `rm_ls_program_item_t`。
**详细中文顺序伪代码：**
1. 增加 `rm_cmo_op_e cmo_op`。
2. 增加 `bit cmo_permission_write`。
3. 增加 `bit cmo_expect_flush_pipe`。
4. 增加 `bit sq_flag`。
5. 增加使用现有 SQ index 宽度的 `sq_value`。
6. 继续复用 `uid`。
7. 继续复用 `rob`。
8. 继续复用 `dynamic_epoch`。
9. 对 CMO 将 `size_bytes` 保持为 0，避免误走标量 Store mask 逻辑。
10. 增加 `bit cmo_has_data_effect`；只允许 zero 为 1，clean/flush/inval 为 0。
11. CMO 的 `store_data` 由 CMO 分类 helper赋值为 0，不允许从 Main table `src_0` 复制。
12. 增加 `bit cmo_completion_consumed` 或由 model ledger 保存等价状态。
13. 增加 `cmo_path`、`cmo_pre_request_fault`、`cmo_request_set_ref`、`cmo_requests_checked` 与统一方案返回的只读错误/分段效果结果；路径来源是独立地址属性，引用仅指向唯一 monitor 历史，不复制请求数组。
14. 构造函数把所有新增字段初始化为无效值。
**状态副作用：** item 获得完整 owner 快照；不修改 model 全局状态。
### 5.4 `rm_cmo_completion_actual_t`（计划新增）
**抽象功能职责：** 用值型对象保存最终 STA0 特殊写回及对应终态信息，不持有验证框架 owner。
**真实源码落点：** `rm_ls_core.sv`，与 `rm_ls_load_actual_t` 相邻。
**详细中文顺序伪代码：**
1. 保存 `valid`。
2. 保存 `uid`。
3. 保存 `rob`。
4. 保存 SQ flag/value。
5. 保存 `dynamic_epoch`。
6. 保存最终 `fuOpType`。
7. 保存写回来源是否为 STA0。
8. 保存 `flush_pipe`。
9. 保存 `exception_vec`。
10. 保存全局 sample 序号或等价单调观测序号。
11. 保存 normal/fault 终态分类结果。
12. 构造函数全部清零。
**状态副作用：** 无；对象只承载一次只读观测快照。
### 5.5 `rm_ls_model_t` 最小 CMO completion 记录
**抽象功能职责：** 只记录当前 commit cursor 对应的 CMO completion 和已消费 owner，阻止 stale/duplicate 事件产生副作用；不复制测试框架 per-UID 生命周期表。
**真实源码落点：** `rm_ls_core.sv` 的 `rm_ls_model_t`。
**详细中文顺序伪代码：**
1. 增加当前 UID 的单笔 pending CMO completion，或复用现有 pending actual 容器增加 CMO 分支。
2. 增加最后一个已消费 CMO owner key，用于识别同 owner duplicate 轮询。
3. live epoch、SQ 和 ROB 身份每次从 readonly status view 复核，不在 RM 中维护第二张 owner map。
4. reset 时清空 pending completion 和 consumed owner key。
5. 当前 pending completion 若与 readonly 当前 owner/epoch 不符，则按 stale 处理并丢弃；不按 CMO `UID+1` 遍历或清理年轻 UID，不预建全主表 epoch ledger。
6. 该记录不推进框架的 Main table、ROB、SQ 或 redirect owner。
7. 该记录不保存 cache metadata。
**状态副作用：** reset 清空本地 completion 记录，stale 过滤只丢弃失效的 pending 快照；已提交数据效果不回滚，不修改框架生命周期状态。
### 5.6 真实请求记录与只读索引（计划新增）

**抽象功能职责：** 为每个 monitor 保存真实请求发生的不可变历史，并以 owner 和传输 token 有界定位；RM 获取的是 monitor 队列派生事实，不是 responder 的预期请求。

**真实源码落点：** `common/memblock_common/src/memblock_sync_pkg.sv` 保存纯值型记录和只读事实访问；对应 monitor 是唯一生产者。env复用已有端口FIFO，必要内部agent仅新增一条事实流；RM在既有唯一get路径内归档和构建引用索引，完整payload只保存一份。必要A/D事实不经旧pack路径产生无对端scoreboard expected流量。

**字段与索引：**

| 记录/索引 | 必需字段或规则 |
|---|---|
| `cmo_request_observation_t` | reset generation、路径、producer/event 序号、fire sample、request token、source、source generation、实际 opcode/param/size/PA/data/mask、最终 op 的被动来源证据 |
| 内部 owner 证据 | UID、ROB flag/value、SQ flag/value、dynamic epoch；SQ->SBuffer 通过同拍 DataBuffer sqPtr/SQ identity镜像绑定，并沿真实SBuffer accessIdx/分配与merge事件连接到slot generation和DCache请求id；non-zero通过请求锁存的SQ/uncacheUop身份绑定，Uncache通过请求id/rob与slot映射绑定 |
| Uncache segment 关联 | owner+段号、SQ 请求 token、Uncache slot generation、实际外部 A token；必须保留零写序列，不根据主表地址补造未观察段 |
| `request_by_owner[owner_key]` | 指向完整请求历史的引用队列；cacheable zero固定一条wline接受及其DCache传输引用、non-zero固定一条CMO链、Uncache zero最多8个合法段；关联中的不合法额外事件另外计错，不能静默截断 |
| `request_by_transport[path,source,generation]` | 只服务当前活跃传输关联，DCache CMO最多1笔；Uncache按物理slot数量有界维护，不按全主表地址扫描 |
| consumed 与发布水位 | 请求集消费账归RM唯一维护；monitor只追加事实并发布sample完成水位，不能因RM轮询pop后让错误方案无法再查询 |

owner 不在 TL payload 中时，不从“最近 CBO”或同地址猜测。内部 monitor 先从真实已锁存身份建立请求链，再以实际接受的 slot/source generation 连接外部 monitor 记录；DCache CMO串行可按唯一在途内部token连接，Uncache按真实slot分配/仲裁选择和A握手连接。SBuffer `accessIdx` 是接受后的RegNext通知，须用其真实sample连接上一拍待关联wline token（或直接采集接受拍的writeReq.wvec）；不能把下一拍accessIdx误配当前wline。内部事实与外部 A 同拍时在 producer barrier 后归并，不能依赖 UVM task 调度先后顺序。

reset 丢弃未完成旧 generation 传输并标记取消，旧记录不得匹配新实例；框架 redirect 只改变 readonly live epoch，RM不自行创建epoch。history 在请求/响应比较与必要终检消费完成后才按单调回收游标释放；source复用不能删除尚未比较的旧请求。每个已消费owner保留最小terminal sample、最后合法token/段号与consumed摘要；迟到同owner额外CMO、第二个zero wline或错误后段请求仍立即报错，不能在commit后卸掉身份检查。摘要按公共生命周期完成与安全回收水位回收，不按每拍全历史扫描。测试结束时检查非取消的未匹配/未消费 CBO 请求，发现意外请求必须失败。

## 6. 分类与权限函数计划
### 6.1 `rm_ls_decode_cmo_final_op()`（计划新增）
**抽象功能职责：** 根据最终 `fuOpType` 唯一确定 CMO 类型，确保 `inval -> flush` 后以 flush 语义进入 RM。
**真实源码落点：** `rm_ls_core.sv` 的纯值型 helper 区域。
**详细中文顺序伪代码：**
1. 输入最终 `fuOpType`。
2. 若匹配 zero，输出 `RM_CMO_OP_ZERO`。
3. 若匹配 clean，输出 `RM_CMO_OP_CLEAN`。
4. 若匹配 flush，输出 `RM_CMO_OP_FLUSH`。
5. 若匹配 inval，输出 `RM_CMO_OP_INVAL`。
6. 其他操作输出 `RM_CMO_OP_NONE`。
7. 返回值表示是否为 CMO。
8. 不接收原始 decode opcode。
**状态副作用：** 无。
### 6.2 `rm_ls_cmo_permission_is_write()`（计划新增）
**抽象功能职责：** 将 CMO 操作映射为权限命令类别，不与异常外显类别混用。
**真实源码落点：** `rm_ls_core.sv` 的纯值型 helper 区域。
**详细中文顺序伪代码：**
1. 输入 `cmo_op`。
2. zero 返回 1。
3. clean 返回 0。
4. flush 返回 0。
5. inval 返回 0。
6. NONE 视为调用错误并返回 0。
**状态副作用：** 无。
### 6.3 `rm_ls_assign_cmo_data_semantics()`（计划新增）
**抽象功能职责：** 在最终 CMO 类型已经确定后，给 RM item 写入规范化 data 和“是否有数据效果”属性。它不读取 Main table 的 `src_0`，不访问 backing/overlay，也不提前提交 zero 效果。
**真实源码落点：** `rm_ls_core.sv` 的纯值型 helper 区域。
**详细中文顺序伪代码：**
1. 输入已经完成基础初始化的 `rm_ls_program_item_t item` 和 `item.cmo_op`。
2. 先把 `item.store_data` 置为 0。
3. 先把 `item.store_byte_mask` 置为 0，防止误走普通 1/2/4/8B Store mask。
4. zero 设置 `item.cmo_has_data_effect=1`。
5. clean 设置 `item.cmo_has_data_effect=0`。
6. flush 设置 `item.cmo_has_data_effect=0`。
7. inval 设置 `item.cmo_has_data_effect=0`。
8. NONE 视为调用错误，保持无数据效果并返回失败。
9. 该 helper 不接收 `main_view.src_0` 或任何 STD observed data。
10. cacheable zero 的 64B 零值只允许在合法 normal completion 后由 `rm_ls_apply_zero_line_effect()` 生成；Uncache zero 的正常/异常分段效果统一交 `rm_ls_apply_uncache_zero_effect()`，两者互斥调用。
**状态副作用：** 只修改当前 item 的规范化 data 字段和 `cmo_has_data_effect`；不修改 model 全局状态。
### 6.4 `observer_build_commit_item_with_observation()`
**抽象功能职责：** 从 Main/status/readonly fact 构造 CMO item，冻结最终操作、owner、地址翻译、权限期望和异常期望。
**真实源码落点：** `memblock_rm.sv` 的同名现有函数。
**详细中文顺序伪代码：**
1. 在标量 `observer_scalar_size()` 判零、普通 Store `item.store_data=main_view.src_0` 和任何数据 mask 构造之前，先调用 CMO 分类 helper。
2. 若是 CMO，设置 `item.kind = RM_LS_KIND_CMO`。
3. 从 `main_view.fu_op_type` 冻结 `item.cmo_op`。
4. 调用 `rm_ls_assign_cmo_data_semantics()`；zero 得到规范 data 0 和有效数据效果标记，non-zero 得到占位 data 0 和无数据效果标记。
5. CMO 分支禁止执行现有普通 Store 的 `item.store_data = main_view.src_0`；主表 `src_0` 只继续参与 `src_0+imm` 地址一致性计算。
6. 从 Main/status 的现有只读身份字段冻结 UID、ROB、SQ 和 dynamic epoch。
7. 若 readonly view 尚未暴露 SQ 身份，只增加只读字段，不增加框架控制行为。
8. CMO 地址继续使用 Main table 已冻结的有效地址和现有 VA 一致性检查。
9. CMO 不使用标量 `size_bytes` 推导数据宽度。
10. CMO 物理效果地址按目标地址翻译得到的 PA 计算 line base。
11. `permission_write = (cmo_op == ZERO)`。
12. `pma_pmp_cmd` 根据 `permission_write` 选择 WRITE 或 READ。
13. `observer_stage_permission_fault()` 的访问参数使用 `permission_write`。
14. 异常向量索引选择仍使用 CMO 的 STU/store 类架构外显。
15. 因此 non-zero 的 read permission fault 写入 store page/access fault 类期望位。
16. PMP response 的 `ld` 在 READ command 下作为 fault。
17. PMP response 的 `st` 在 WRITE command 下作为 fault。
18. 不根据最终异常名反推 permission command。
19. zero 设置 `cmo_expect_flush_pipe = 0`。
20. non-zero 设置 `cmo_expect_flush_pipe = 1`。
21. 权限或翻译 fault 时保留 PA 已知部分仅用于诊断，不提交 zero 效果。
22. 根据独立翻译/PBMT/PMA 属性冻结 CMO 路径与前置异常类别，不从 actual exception 或已看到的请求选择期望路径。
23. item 构造成功后进入现有 `ls_model.add_item()`；数据访问和请求核对等待当前 DUT 写回/提交或合法 fault terminal，不让 RM 先行。
**状态副作用：** 只创建/更新当前 item；不修改 backing 或 expected store overlay。
## 7. Completion 与 owner 函数计划
### 7.1 `observer_capture_cmo_completion()`（计划新增）
**抽象功能职责：** 捕获当前 CMO 的 owner-bound STA0 最终结果：成功路径要求 StoreQueue 特殊完成写回，fault 路径接受 StoreUnit/StoreQueue 提供的 STA0 异常完成；两者建立 normal/fault 互斥 actual 快照。
**真实源码落点：** `memblock_rm.sv`，与 `observer_capture_commit_actual()` 相邻；类 extern 声明同步增加。
**详细中文顺序伪代码：**
1. 输入 `main_view`、`status_view` 和 CMO item。
2. 通过现有 integer writeback observer/read-only API 按 ROB 查询 STA0 写回。
3. 若现有 API 只暴露 LDA，则只扩展同一只读 observer 的 STA0 查询，不新增主动 monitor 行为。
4. 要求存在与当前 ROB 匹配的最终 STA0 记录；权限/翻译 fault 不要求已经发出 CMO request 或出现 StoreQueue 成功特殊写回。
5. 要求来源明确为 STA0。
6. 要求写回 ROB 与 item ROB 完全一致。
7. 要求写回最终 `fuOpType` 与 item 冻结值一致。
8. 要求 status/Main 的 SQ 身份与 item SQ 一致。
9. 要求写回或伴随终态事实的 dynamic epoch 与 item 一致。
10. 若 owner 不一致，将事件分类为 stale 或 identity mismatch。
11. 若 item 已有同 owner completion，将新事件分类为 duplicate。
12. stale/duplicate 不覆盖首个合法 completion。
13. normal 候选要求 `status_view.exception_vec == 0`，并且记录属于成功 CMO 最终特殊写回。
14. fault 候选要求 `status_view.exception_vec != 0`，并且 STA0 异常记录与 status terminal exception 的架构 cause 一致。
15. normal 与 fault 同时成立时报错。
16. 只有 normal 候选检查 `flushPipe == item.cmo_expect_flush_pipe`；fault 候选不套用成功路径的 zero/non-zero `flushPipe` 约束。
17. zero normal 候选因此要求 `flushPipe=0`。
18. non-zero normal 候选因此要求 `flushPipe=1`。
19. 普通 STA/STD issue feedback、replay 写回或非 STA0 写回不得成为 completion。
20. 将合法 actual 写入 model 的当前 CMO pending completion 记录。
**状态副作用：** 只写 pending actual ledger；不提交 zero 数据效果，不推进 cursor。
### 7.2 `rm_ls_validate_cmo_owner()`（计划新增）
**抽象功能职责：** 在任何 CMO completion、异常比较或数据效果前统一验证动态 owner；只使用 readonly 身份事实，不消费 redirect。
**真实源码落点：** `rm_ls_core.sv` 的 `rm_ls_model_t`。
**详细中文顺序伪代码：**
1. 输入 item 与 completion actual。
2. 比较 UID。
3. 比较 ROB flag/value。
4. 比较 SQ flag/value。
5. 比较 dynamic epoch。
6. 检查该 UID 是否等于当前程序 cursor。
7. 通过 readonly status view 检查该 epoch 是否仍为该 UID 的 live epoch。
8. 检查该 owner 是否已消费。
9. 旧 epoch 返回 stale。
10. 已消费 owner 返回 duplicate。
11. 任一身份字段不一致返回 mismatch。
12. 只有全部匹配才返回 live owner。
**状态副作用：** 无；该函数只读 item、readonly status 和最小 consumed-owner 记录。
### 7.3 `rm_ls_commit_cmo()`（计划新增）
**抽象功能职责：** 联合已经核对的真实请求、独立异常期望和合法最终完成，消费当前 CMO 一次；按路径选择唯一数据效果入口。
**真实源码落点：** `rm_ls_core.sv` 的 `rm_ls_model_t`，由 `commit()` 调用。
**源码级伪代码：**
```text
验证 item 与 completion owner；stale/duplicate/mismatch 不提交。
要求 requests_checked、error_facts_ready 与所有 producer watermark 成立。
比较独立 expected exception、STA actual exception 和 terminal exception。
前置 fault：要求请求集合为空，禁止 normal pass，不应用 zero 效果。
响应 fault：要求合法请求/响应前缀与 fault terminal，禁止 normal pass。
正常路径：要求请求集合完整且 actual exception=0，检查 zero/non-zero flushPipe=0/1。
zero && cacheable && normal -> apply_zero_line_effect() 一次。
zero && uncache && 合法 normal/fault -> apply_uncache_zero_effect() 一次。
non-zero -> 无数据写函数。
全部通过后原子标记请求集消费和 completion 消费，写 history，允许 cursor 前进。
```
**详细中文顺序伪代码：**
1. 从 item 读取已验证的请求集合引用、统一错误方案的异常/分段效果结果与 pending completion。
2. 调用 `rm_ls_validate_cmo_owner()`，比较 UID、ROB、SQ、epoch 与 readonly 当前身份；stale/duplicate 返回无副作用，mismatch 报错。
3. 要求请求证据已按第 7.7 节完成核对，相关 monitor watermark 已覆盖 terminal sample；不能仅有 STA0 写回就进入成功路径。
4. 比较独立 expected exception、STA0 实际异常和 terminal exception。响应错误期望只能来自统一 `observer_apply_cmo_d_error_expectation()`，不能由 actual exception 反推。
5. 前置权限/翻译 fault 要求该 owner 请求集合为空，normal pass 为 0，fault terminal 有效；不应用 zero 效果。
6. 下游响应 fault 要求请求/响应关联和合法停止位置已由统一方案确认，normal pass 为 0；Uncache zero 的合法部分效果不能被“fault”分支丢弃。
7. 正常路径要求 actual exception 为 0、normal pass 为 1；zero 检查 `flushPipe=0`，non-zero 检查 `flushPipe=1`。
8. 正常 cacheable zero 调用 `rm_ls_apply_zero_line_effect()`，其依据已核对的 `wline` 请求及SBuffer->DCache成功完成链原子写 64B overlay 并登记 8 个 entry。
9. Uncache zero 正常或响应 fault 调用统一方案唯一实现的 `rm_ls_apply_uncache_zero_effect(item, segment_effect)`，只提交统一结果中确定接受的合法段；本计划不展开或复制其接受策略与字节写循环。
10. 两个 zero effect helper 必须按路径互斥；成功 Uncache 8 段也不得叠加整行 helper，所有路径复用同一 `expected_store_byte/expected_store_entries`。
11. non-zero 要求 `cmo_has_data_effect=0`，不调用任何数据写函数。
12. 错序、错 owner、错误后多发等非法请求导致本次比较失败；不能为“对齐 DUT 数据”而把其字节纳入合法期望。
13. 所有比较及唯一效果提交成功后，提交该 owner 的 request-consumed 和 completion-consumed 标记，写一条终态 history。fault 匹配同样只消费一次。
14. 不等待框架 `flushAfter`、exception redirect，不处理恢复或重发。
**状态副作用：** 按路径一次更新现有 overlay/entry、请求消费账、history 与 completion ledger；失败不产生 RM 数据副作用。
### 7.4 `commit()`
**抽象功能职责：** 保持唯一程序顺序提交入口，并为 CMO 增加独立分支。
**真实源码落点：** `rm_ls_core.sv` 的现有 `commit()`。
**详细中文顺序伪代码：**
1. 保留 configured/current UID/ROB 前置检查。
2. `RM_LS_KIND_LOAD` 继续调用 `compare_load()`。
3. `RM_LS_KIND_STORE` 继续调用 `commit_store()`。
4. `RM_LS_KIND_CMO` 调用 `rm_ls_commit_cmo()`。
5. CMO 等待 completion fact 时返回 0 且不推进 cursor。
6. CMO stale/duplicate/mismatch 不能推进 cursor。
7. CMO 正常或 fault terminal 语义完成后只推进一次 cursor。
8. 当前 CMO 的合法 terminal 只消费一次；RM 不因框架 redirect 回退 cursor，也不安排当前 UID 重新加入或发射。
9. 保留既有 `consumed_commit_count` 统计。
**状态副作用：** 仅在合法 terminal 后推进 `current_uid`；不负责测试框架 admission。
### 7.5 `cmo_path_agent_monitor::capture_cmo_path_requests()`（计划新增）

**抽象功能职责：** 被动记录 CBO 在 SQ 到实际数据/CMO路径上的接受事实和最小 owner 身份，补足顶层 TL 无法表示的 zero 类型；不预测应有请求、不生成响应。

**真实源码落点：** 待新增 `subagent/cmo_path_agent/src/cmo_path_agent_monitor.sv`；接口只读连接 SQ、DataBuffer/SBuffer接受和SBuffer->DCache请求处、Uncache slot分配与CMO传输关联所需字段。精确生成RTL别名在coding前对当前V2产物复核；已有语义端口见第4.3节，不能从不存在的TL owner字段取值。

**源码级伪代码：**
```text
每个已锚定 sample：reset优先；取得同拍只读 owner 视图。
SQ->SBuffer valid&&ready&&wline：捕获实际payload与dataBuffer sqPtr，绑定owner，追加DCACHE_ZERO_ACCEPT。
真实SB insert/merge选择与slot代际：连接zero token；SB->DCache req.fire：按实际id连接token，保留attempt及replay；main_pipe_hit_resp.valid连接同slot成功完成。
SQ cmoOpReq valid&&ready：捕获opcode/PA与锁存owner，分配内部CMO token。
SQ uncache req valid&&ready：捕获cmd/PA/data/mask/id/nc与owner，分配段token。
被动采集CMO发送/Uncache slot分配仲裁事实，给token绑定真实source-generation。
先发布本sample全部事件，后推进producer watermark；缺身份只发布关联错误，禁止补造owner。
```

**详细中文顺序伪代码：**
1. monitor 每拍只检查物理接口数量；reset 清空当前活跃关联，不清除已归档的旧generation诊断事实。
2. 观察 SQ `io.sbuffer(i).valid && ready && bits.wline && bits.vecValid`，捕获实际PA和payload，读取该口同拍DataBuffer sqPtr并与只读SQ/ROB/epoch身份关联。
3. 观察 `io.cmoOpReq.fire`，捕获实际opcode、地址及已锁存 `uncacheUop`/SQ owner；该接口无owner payload，因此必须补只读镜像，不能只按当前主表UID标记。
4. 观察 SQ `io.uncache.req.fire`，捕获真实请求id、ROB、地址、零数据/byte mask与nc属性，绑定同拍真实slot分配记录；只读最终op决定是否属于CBO，不用“data==0”把普通Store当zero。
5. CMO唯一在途链与Uncache物理slot map保存后续外部A关联所需token和generation。源复用每次增加generation，活跃旧请求未释放时不覆盖。SQ wline token通过SBuffer实际accessIdx/slot分配或merge事件绑定到slot generation；采集SBuffer->DCache `req.fire` 的实际id、地址、mask和data，沿该slot生成传输attempt引用；被动记录replay通知和最终main_pipe_hit_resp有效/id，将完成连回同slot generation。DCache replay可有多次attempt；同slot普通Store合并不建立第二套SB数据模型，也不要求每次整行payload仍全零。
6. 每条真实请求只发布一条记录；valid保持但未ready不入队。字段缺失或身份冲突报告明确的事实关联错误，不悄悄跳过请求。
7. 本sample全部事件发布完成后推进watermark，使RM可以确认某一terminal之前的请求集合完整。
**状态副作用：** 只追加被动事实、更新物理在途关联与watermark；不修改框架owner或RM字节。

### 7.6 两个现有端口 monitor 的 `mon_data()` 扩展

**抽象功能职责：** 将 DCache CBO 和 Uncache 的真实 TL 请求接受独立入队，使 RM 有实际端口证据；响应采集与统一错误方案共享。传输token/source generation由共享被动关联层唯一赋值，producer与responder引用同一值；不得各自独立计数后碰巧比较相同。

**真实源码落点：** `dcache_agent_agent_monitor::mon_data()`、`sbuffer_agent_agent_monitor::mon_data()`；同步对应 interface/xaction/analysis FIFO 接线，不再保留相关发布为注释。

**源码级伪代码：**
```text
按既有全局DUT sample锚定当前monitor采样；CSR monitor是唯一时钟sample发布者，是否更新CSR值不影响每拍采样；reset generation不一致则清当前活跃传输。
DCache A.fire且opcode属于CBO：捕获完整A payload，分配source本次generation与event token，发布到DCache请求FIFO。
Uncache A.fire：捕获完整A payload，按真实slot generation绑定内部请求token，发布到Uncache请求FIFO。
D.fire被动响应发布复用统一错误方案；不在本helper另建错误ledger。
发布当前sample全部请求后更新各自watermark。
```

**详细中文顺序伪代码：**
1. 从clocking block取真实握手值，与全局DUT sample对齐，不依赖是否发生CSR写入/值变化，不能在驱动前取responder打算发送的请求/响应。
2. DCache端只将真实CBO A opcode进入CMO队列，保存clean/flush/inval实际类型；普通Acquire不能因地址等于zero行就改标为zero。
3. Uncache端保存每次真实A.fire的size、mask、data、source和地址；把CBO身份留给内部token关联，外部普通Put并不自带CBO分类。
4. 发布不可变值型记录，连续相同地址请求也逐笔保存，不按地址或payload去重。8个zero段各占一个接受事件。
5. DCache CBO固定source每次占用仍有独立generation；Uncache A与D按slot generation匹配，不用下一笔同source响应补齐前一笔。
6. 既有response driver/responder记录只可供响应效果策略查询；不作为“DUT真实产生请求”的替代证据。
7. 发布watermark后，adapter才能按同拍屏障将内部与外部记录连接；不引入每拍全历史扫描。
**状态副作用：** 追加两个端口请求队列、活跃source map与watermark，供RM只读；不产生RM判定。

### 7.7 `memblock_rm::observer_compare_cmo_requests()`（计划新增）

**抽象功能职责：** 在当前 CMO 已达到实际写回/提交或合法 fault terminal 后，核对监视队列中属于该动态owner的完整真实请求集合，证明路径、类型和请求数量正确；它不做 denied/corrupt 异常映射或提交字节。

**真实源码落点：** `env/src/memblock_rm/memblock_rm.sv`；observer adapter从env analysis FIFO按新增事件游标归档，公共只读接口返回owner引用集合。

**源码级伪代码：**
```text
输入冻结item、terminal sample和readonly live owner；验证当前比较资格。
等待相关producer watermark>=terminal sample；只归档新增FIFO事件。
按owner_key取请求集合，不按地址找“最近一笔”。
前置fault -> 要求该owner集合为空；有请求立即失败。
cacheable zero -> 检查恰好一个SQ wline接受、正确line与zero语义，真实SB slot链有DCache请求接受与成功hit response，无CMO/Uncache链。
non-zero -> 检查一笔内部CMO接受与一笔真实TL CBO A，一一绑定且op/line/size相符。
Uncache zero -> 检查段0开始、地址每次+8、size=8B、mask=ff、data=0，SQ与外部A一一关联。
将核对后的token集合交统一错误入口判响应/停止与效果；禁止在此重复错误算法。
返回READY/WAIT/FAIL及不可变集合引用；消费提交留给唯一commit。
```

**详细中文顺序伪代码：**
1. 从独立item读取最终op、物理line、预期路径和前置异常。只在DUT实际normal写回与框架提交后或合法fault terminal后调用，不会提前查询尚未执行的DUT访问。
2. 等待相关producer发布水位覆盖terminal sample；barrier未满足返回WAIT。barrier满足后缺请求为FAIL，不能无限等待一个已错失的请求。
3. adapter按各FIFO的单调游标一次处理新增事件，并按owner/token索引定位；每次处理按物理端口数及当前sample新事件数量有界，未归档到terminal sample时继续返回WAIT，不扫描main_trans_num或全部历史。CMO逻辑请求集合上界是1条zero接受/1条CMO链/8个Uncache段；zero的DCache尝试由真实slot/attempt单独关联，合法replay不作为第二次zero。额外逻辑请求立即计错，不能丢弃后宣称集合合法。
4. 联合UID、ROB、SQ、epoch与reset generation复核每条记录。旧epoch事实排除为stale，不能取其payload满足当前请求；缺失或多owner关联报错。
5. cacheable zero检查真实wline接受、目标行、128-bit输入零数据和当前16-bit输入全mask（以实际接口宽度复用编译常量），`wline`语义表示整64B零写；不把16-bit输入mask误作64-bit输出mask。再沿实际SBuffer writeReq/accessIdx/slot generation与输出id，确认此token对应的SBuffer->DCache整行Store真实接受。
6. SBuffer `io.dcache.req.valid&&ready`仅证明请求入DCache；`replay_resp.valid`表示该slot需后续重试，不能当成功。要求真实同slot generation的 `main_pipe_hit_resp.valid` 成功通知，V2此接口为Valid而非Decoupled，不能等待不存在的ready。若生成RTL保留miss/replay字段则同时检查非miss/非replay；优化移除字段时按成功专用接口语义识别。terminal相关watermark完整后无该成功链报错，不以特殊写回或SB empty倒推成功。
7. 一个DCache请求允许携带同slot合并的多个写token，DCache replay允许多次合法attempt；不能将它们误判为重复CBO。wline输入核对零语义，DCache输出核对真实line/slot/传输关联与mask覆盖；普通Store合并后不强制整行data永远全零，不把实际SB输出payload复制为expected，也不复刻SB组装/替换算法；最终数据仍由程序顺序overlay/Store-table独立验证。
8. non-zero同时检查内部CMO请求和DCache TL CBO A；clean/flush/inval分别核对最终类型、line对齐PA、line size与source generation，错误类型、错误路径或重复A均失败。
9. Uncache zero检查真实接受的连续段号与每段实际字段、SQ token和外部A token一一关联。合法集合可能是1..8段，是否应正常完成、在哪段响应后停止交统一错误入口判断；不能为了接受fault而容忍跳段、重复段或错误后多发。
10. 前置fault在完整watermark范围要求无数据/CMO请求；同时检查未绑owner但与当前在途链冲突的孤儿事件，不能通过遗漏owner绑定使意外请求消失。
11. 返回不可变请求集合引用和结构检查结果。统一错误入口读取同一集合及响应事实；不在此pop破坏记录。唯一`rm_ls_commit_cmo()`成功后一起标记消费，重复轮询不重复消费。
12. end-check用索引pending计数检查未匹配、未消费或错误路径的CBO记录；reset/已取消旧generation与真实多发分别记录，不用“stale”掩盖当前owner的重复请求。
**状态副作用：** 更新本地引用索引、结构比较结果与诊断；不写overlay、创建epoch或推进程序cursor。

## 8. zero 数据效果函数计划
### 8.1 `rm_ls_apply_zero_line_effect()`（计划新增，既有效果底座的整行封装）

**抽象功能职责：** 在cacheable zero合法终态后，把整行效果拆成8个已预检查的8B子效果，经现有内存效果底座一次性提交；不复制overlay写循环，不承担Uncache分段策略。

**真实源码落点：** `rm_ls_core.sv` 的 `rm_ls_model_t`，由唯一 `rm_ls_commit_cmo()` 调用。

**源码级伪代码：**

```text
要求cacheable zero、normal completion、expected_exception=0、owner及请求链合法且未消费。
计算line_base，先校验整个64B PA范围及8个子效果。
每个子item新建独立对象，保留同owner，size=8、8个连续PA、pa_valid_mask=ff、data=0、byte_mask=ff。
为整组完成commit_memory_effect所需全部translation/PA/mask前置检查和entry准备。
任一检查失败：返回FAIL，整行字节和SC覆盖标记均不更新。
无等待阶段逐个调用既有commit_memory_effect(subitem, RM_MEMORY_EFFECT_CMO_ZERO, 0, ff)。
底座为每段维护SC shadow、expected byte overlay和一项entry；全部完成后外层commit标一次消费。
```

**详细中文顺序伪代码：**

1. 只接收已通过owner、真实SQ wline→DCache成功链和normal终态检查的cacheable zero；Uncache zero不进入此函数。
2. 从有效PA计算64B行首，逐段新建8个8B子item；不修改原64B CMO item或反复复用同一对象引用。
3. 子item复制UID/ROB/SQ/epoch及必要地址有效身份，设置独立8B PA列表、有效mask和zero常量；不读取Main table `src_0`。
4. 先检查全部64B PA及所有子item满足既有底座条件，再开始任何副作用。底座调用期间无yield，已验证条件不会变化，不能处理中途返回后保留半行RM更新。
5. 每段调用 `commit_memory_effect()`：它先把重叠失败SC obligation字节标为shadowed，再写期望byte/valid，最后登记该段entry。三项副作用缺一不可，不能只复制byte与entry代码。
6. 整组共64B零overlay及8项entry；子效果不新增指令history、不推进cursor，外层唯一commit只推进一次。backing和旧entry均保留，最终entry比较继续读取当前overlay。
7. 若需抽取接收预构造entry的已校验底座，则由既有`commit_memory_effect()`和本封装共同调用，保持所有普通Store/原子/CMO写使用同一套SC shadow及字节落地逻辑；不新建第二套写模型。

**状态副作用：** 仅在全部预检查后一次提交cacheable zero的整行集合，包含必要SC shadow；Uncache zero经统一方案薄封装使用同一底座，两个路径互斥。

### 8.2 `commit_store()`
**抽象功能职责：** 继续只处理普通标量 Store，避免 CMO 误用 1/2/4/8B mask 逻辑。
**真实源码落点：** `rm_ls_core.sv` 的现有 `commit_store()`。
**详细中文顺序伪代码：**
1. 保持现有普通 Store exception 比较。
2. cacheable Store和前置fault保持既有边界；MMIO正常/响应fault确定写效果接统一薄封装，后者复用commit_memory_effect的SC shadow。NC本轮仅允许backend已接受写后的参数denied，真实后端拒绝按统一scope终止测试，不在这里回滚overlay。
3. 保持正常 Store byte mask 检查。
4. 保持所有证据先检查、再原子写 overlay。
5. 增加防御性断言：`item.kind` 必须为 `RM_LS_KIND_STORE`。
6. 若传入 CMO，设置分类错误并返回。
7. CMO zero 只能经 `rm_ls_commit_cmo()` 提交。
**状态副作用：** 与现有普通 Store 一致；不承担 CMO 数据效果。
### 8.3 `build_load_expected()`
**抽象功能职责：** 按程序顺序从 expected store overlay/backing 构造后续 load 值，使 zero 和 non-zero 的数据效果自然可见。
**真实源码落点：** `rm_ls_core.sv` 的现有 `build_load_expected()`。
**详细中文顺序伪代码：**
1. 保持 exception 时不构造数据。
2. 保持 PA geometry 检查。
3. 每个 byte 先查询 `expected_store_byte_exists(pa)`。
4. 命中 overlay 时读取 `expected_store_byte[pa]`。
5. 未命中 overlay 时读取 `backing_byte[pa]`。
6. zero 成功后，同一 line 的后续 load 命中零 overlay。
7. non-zero 后不新增 overlay，因此后续 load 读到原有 store overlay 或 backing。
8. load actual 的有效性沿用公共 owner/epoch 匹配，旧 epoch actual 不得进入 compare；本函数不负责失效清理。
9. 测试框架恢复后的 load 到达当前 commit UID 时，observer 从 readonly 状态读取其当前动态身份并构造 item；本函数不创建 epoch。
10. 不判断数据来自 cache、forwarding 或 memory。
**状态副作用：** 无；只读 expected store overlay 和 backing。
## 9. 写回检查与 Redirect 职责边界
### 9.1 RM 与测试框架的职责
1. MemBlock 向后端写回的正常 CMO 属性是 `flushPipe`；RM 对成功 zero/non-zero 分别比较 0/1。
2. `flushAfter` 是测试框架模拟后端时根据成功 non-zero 写回构造的 redirect；RM 不生成、驱动或消费该控制事件，也不要求独立的 redirect 已消费标记。
3. fault CMO 由 RM 比较独立计算的预期异常、owner-bound STA0 实际异常和 fault terminal；exception redirect 属于测试框架控制逻辑，不作为 RM 异常比较的必要输入。
4. 从 CMO `UID+1` 恢复、保留当前 CMO、不重发当前 CMO、清理年轻 ROB/IQ/LSQ owner 以及为恢复实例建立新 dynamic epoch，均由配套测试框架方案负责。
5. RM 只读当前 UID/ROB/SQ/epoch 并匹配 completion，不计算 restart UID，不遍历框架年轻实例，不修改其状态。
6. 本计划不新增 CMO 专用的 `rm_ls_invalidate_younger_epoch_state()` 或 `observer_apply_cmo_redirect_terminal()`；写回和异常比较通过现有 observer/commit 路径完成。

### 9.2 RM 本地快照有效性
1. 沿用 `observer_process_current_commit()` 按当前程序 cursor、框架提交/终态事实构造 item 和捕获 completion 的路径，不为 CMO 提前捕获年轻投机 actual。
2. pending completion、已有 writeback history 或其他缓存中的事实，在参与比较前必须与 readonly 当前动态身份匹配；旧 epoch 或已取消 owner 的事实按公共 stale 规则排除。
3. 若既有公共机制已负责 RM 本地投机快照失效，则复用该机制；不得在 CMO 分支复制 `UID+1` 范围清理或 epoch 重建逻辑。
4. 不存在年轻投机快照时，不为可能的 redirect 新增年轻状态表、全表扫描或清理器。
5. reset 和无效 pending 快照的丢弃仅影响 RM 本地临时记录；更老已提交的 expected store overlay、当前 CMO 已提交的数据效果和 history 不因框架恢复回滚。
6. 测试框架恢复后的实例到达当前比较位置时，RM 使用 readonly 当前 owner/epoch 构造 item，不自行计算新 epoch，也不单独验收框架 epoch 如何递增。

### 9.3 `observer_process_current_commit()`
**抽象功能职责：** 作为程序顺序终态比较入口，从 readonly 框架状态与 DUT 写回事实构造当前 CMO 的比较输入，协调异常、写回属性和唯一 commit；redirect 恢复由测试框架负责。
**真实源码落点：** `memblock_rm.sv` 的现有同名函数。
**详细中文顺序伪代码：**
1. 保留 framework ready 和 Main table count 检查。
2. 保留 Main/status UID、ROB 一致性检查。
3. 对 CMO 增加 SQ 和 dynamic epoch 一致性检查，动态身份只从 readonly 状态取得。
4. 普通 Load/Store 继续执行现有路径。
5. 当前 CMO 的比较资格使用框架已有的提交/终态事实；fault 路径使用合法 fault terminal，不等待 normal pass。
6. CMO 不使用 `observer_capture_commit_actual()` 的 Load 分支。
7. CMO 调用 `observer_capture_cmo_completion()`；正常路径取得成功最终特殊写回，fault 路径取得 owner-bound STA0 异常完成和 fault terminal。
8. completion 未就绪时返回等待，不推进 cursor；这里等待的是写回/终态事实，不是 redirect 控制事件。
9. stale/duplicate completion 不调用 `ls_model.commit()`，不覆盖当前合法 completion。
10. 等相关 monitor watermark 覆盖 completion/terminal sample；调用 `observer_compare_cmo_requests()`，从对应路径事实队列取得真实请求并核对完整集合，缺失、错路、错类、错 owner、重复或意外请求均不能放行。
11. 调用统一错误方案的 `observer_apply_cmo_d_error_expectation()`，用已核对请求集合与独立响应/写接受事实补足期望异常和分段效果；producer/adapter水位尚未覆盖则等待；已覆盖且应有记录缺失立即报证据错误，不从实际异常补造期望。
12. 两类证据就绪后调用 `ls_model.commit()`，由 `rm_ls_commit_cmo()` 比较预期异常、实际异常及 normal/fault 互斥关系，并唯一提交对应效果。
13. 正常 zero/non-zero 分别检查 `flushPipe=0/1`；fault 不套用该成功路径约束。
14. 不等待或消费 `flushAfter`、exception redirect，不执行 `UID+1` 清理或年轻实例恢复。
15. zero 数据效果只能由 `rm_ls_commit_cmo()` 内部选择唯一 effect helper 提交；non-zero 与前置 fault 不改数据，Uncache zero 响应 fault 允许统一方案确认的部分效果。
16. 合法 terminal 比较完成后 current UID 只推进一次；duplicate 轮询不得再次写零。
17. 日志打印最终 op、UID、ROB、SQ、epoch、`flushPipe`、exception、normal/fault 分类和比较结果。
**状态副作用：** 协调 RM 请求/响应事实读取、completion 消费、history 和 model commit；不直接写 expected bytes，不修改测试框架 redirect、admission、owner 或 epoch。
## 10. 异常与 normal pass 互斥规则
`expected_exception` 或 actual exception 非零时禁止 normal pass；final STA0 normal completion 与 fault terminal 不得同时记账。前置 fault 禁止数据效果；Uncache zero 响应 fault 的确定性分段效果由统一错误方案计算，不能用 exception 非零直接清空。只有正常完成的 zero/non-zero 分别要求 `flushPipe=0/1`；fault 路径比较预期异常、STA0 实际异常和 terminal 异常，不套用正常完成的 `flushPipe` 约束。同一 owner 只能记录并推进一次。`flushAfter`、exception redirect 和 replay 的生成、仲裁与恢复由测试框架负责，RM 不等待这些控制动作完成才比较当前 CMO。
## 11. stale 与 duplicate 处理规则
owner 必须联合比较 UID、ROB、SQ 和 epoch；旧 epoch 为 stale，完整 owner 已消费为 duplicate。两者均不得写 overlay、增加 entry、推进 cursor 或清理新 epoch 状态，日志必须与普通 owner mismatch 分开。相同事实的重复轮询与DUT真实产生的第二次请求/完成必须区分：前者不重复消费，后者在终态摘要索引中报duplicate violation，不能静默忽略。
### 11.1 denied/corrupt 的唯一实现归属

统一观察层可保存真实slot来源版本及有资格错误输入，cache-hit不套永久行错误；这些只读事实不属于本CMO核心数据模型。基础注入资格复用reqSource，Store来源即使合并Load仍保守正常，不要求完整MSHR消费者重建。

CMO plan负责最终类型、独立权限/路径、真实请求结构、owner、最终STA0属性与唯一commit。统一错误方案独占响应历史查询、denied/corrupt异常映射、Uncache zero合法停止/部分效果、共享Uncache字节写入和 `io_uncacheError_ecc_error_valid/bits` 的采集/比较。CMO observer只调用 `observer_apply_cmo_d_error_expectation()`，CMO commit按路径只调用一个效果入口。不得另增CMO专用错误数组、通知checker或Uncache zero写循环；公共response monitor事实与队列消费账均复用同一份。

## 12. 参数结论
1. 本计划优先不新增参数。
2. 64B line size 作为 V2 当前 DUT 固定配置的实现常量或既有编译常量复用。
3. 本CMO计划不新增 runtime plusarg；错误注入使用统一错误方案既有六项权重与参数链，不再建立CMO重复开关/权重。
4. 不新增 testcase preset cfg。
5. 不新增 `memblock_env_cfg` 字段。
6. 不新增 user local cfg 字段。
7. 不新增 CMO enable 开关。
8. 不新增 completion timeout 参数。
9. 不新增 cache 容量、组数或路数参数。
10. 不新增 redirect 延迟参数。
11. 若 coding 时已有统一 line-byte 常量，直接复用。
12. 只有确认 V2 line size 不可从现有编译上下文获得时，才允许在 RM 内定义单一 `localparam 64`。
## 13. 明确不实现
1. CMO核心不实现cache metadata；统一被动observer仅保存实际slot错误来源身份，不模拟其算法。
2. 不实现 cache line valid。
3. 不实现 cache line dirty。
4. 不实现 coherence permission。
5. 不实现 cache hit/miss。
6. 不实现 cache replacement。
7. 不实现 Probe。
8. 不实现 MSHR。
9. 不实现 SBuffer。
10. 不实现 DataBuffer。
11. 不实现 StoreQueue 内部 CMO 状态机。
12. 不实现 LoadQueueRAW CAM。
13. 不实现 RAW oldest selection。
14. 不实现 LoadUnit fast nuke。
15. 不实现 forwarding 来源判断。
16. 不判断数据来自 StoreQueue forwarding。
17. 不判断数据来自 SBuffer forwarding。
18. 不判断数据来自 DCache 或 memory。
19. 不实现 CMOUnit FSM。
20. 不实现 `deqCanDoCbo`。
21. 不实现 `mmioState`。
22. 不实现内部 request 固定拍数。
23. 不实现内部 response 固定拍数。
24. 不实现内部流水算法。
25. 不实现测试框架 admission。
26. 不实现测试框架 issue 调度。
27. 不实现测试框架 replay 调度。
28. 不实现测试框架 redirect 重发。
29. 不实现测试框架 ROB/SQ 资源分配。
30. 不为 CMO 设计新的激励 sequence。
31. 不生成、驱动、消费或等待 `flushAfter` / exception redirect。
32. 不计算 CMO `UID+1` 恢复边界，不建立 CMO 专用年轻状态清理器或 epoch 恢复表。
## 14. Directed Check 计划
### 14.1 分类与权限
1. 最终 zero 被分类为 CMO zero。
2. 最终 clean 被分类为 non-zero。
3. 最终 flush 被分类为 non-zero。
4. 最终 inval 被分类为 non-zero。
5. 原始 inval、最终 flush 的场景只按 flush 检查。
6. zero 只读页必须产生权限 fault。
7. zero 可写页允许 normal completion。
8. non-zero 可读不可写页允许通过权限检查。
9. non-zero 不可读页产生 fault。
10. non-zero fault 可外显 `storeAccessFault`，但日志显示 permission command 为 READ。
11. 给 zero 的 Main table `src_0` 设置非零值，RM item 的规范化 `store_data` 仍必须为 0。
12. 给 clean/flush/inval 的 Main table `src_0` 设置不同非零值，RM 均必须标记无数据效果。
### 14.2 completion
1. zero final STA0 `flushPipe=0` 通过。
2. zero final STA0 `flushPipe=1` 失败。
3. non-zero final STA0 `flushPipe=1` 通过。
4. non-zero final STA0 `flushPipe=0` 失败。
5. 非 STA0 写回不得完成 CMO。
6. 普通 STA/STD feedback 不得完成 CMO。
7. replay 写回不得完成 CMO。
8. exception 与 normal pass 同时出现必须失败。
9. fault terminal 不得等待 normal completion 才禁止正常整行清零；Uncache 已接受分段的确定性效果仍按统一方案处理。
### 14.3 owner
1. UID mismatch completion 无副作用。
2. ROB mismatch completion 无副作用。
3. SQ mismatch completion 无副作用。
4. epoch mismatch completion 无副作用。
5. 旧 epoch completion 被识别为 stale。
6. 同 owner 第二次 completion 被识别为 duplicate。
7. stale zero 不写任何零字节。
8. duplicate zero 不重复增加 8 个 line entry。
9. stale non-zero 不推进 cursor、不覆盖当前 completion，也不触发任何框架控制或年轻状态清理。
### 14.4 数据效果
1. zero 前先在同一 line 写入非零 backing。
2. zero 前再叠加部分普通 Store overlay。
3. 成功 zero 后检查 64 个字节全部为零。
4. 检查 line 外前一字节不变。
5. 检查 line 外后一字节不变。
6. 前置权限/翻译 fault zero 后 64 个字节保持原值；Uncache zero 第 0/中间/第 7 段响应 fault 后按统一方案的接受段掩码检查，未接受字节保持原值。
7. stale zero 后 64 个字节保持原值。
8. duplicate zero 后 overlay entry 数量不重复增长。
9. clean 后数据不变。
10. flush 后数据不变。
11. inval 后数据不变。
12. zero 的结果不随 Main table `src_0` 或 STD payload 变化。
13. clean/flush/inval 的结果不随 Main table `src_0` 或 STD payload 变化。
### 14.5 写回边界与程序顺序
1. 成功 non-zero 的 owner-bound STA0 `flushPipe=1` 可以完成 RM 比较，不要求额外注入 `flushAfter` 事件。
2. owner-bound STA0 异常与预期、fault terminal 一致时，可以完成 fault CMO 比较，不要求额外注入 exception redirect 事件。
3. 同一 CMO owner 的合法 terminal 只消费一次，duplicate 不重复写 overlay 或推进 cursor。
4. readonly 当前 owner/epoch 变化后，旧 epoch completion 不得覆盖新实例的 actual 或产生数据副作用。
5. 恢复实例的当前 completion 与 readonly 身份匹配时可以正常比较，RM 不自行创建 epoch。
6. 旧 epoch 年轻 load completion 由公共身份有效性机制排除，不在 CMO 路径执行年轻范围清理。
7. 成功zero后的同line Load若无独立响应错误则读取零，不要求经过redirect；若实际hit仍消费旧error flag，按统一错误来源比较异常而非强制正常值。
8. non-zero 后的后续合法 load 读取原有 expected store overlay/backing 值。
9. fault zero后的后续无独立异常Load读取统一overlay：前置fault保持原值，Uncache响应fault包含已确认部分零字节。
10. `UID+1` 恢复、当前 CMO 不重发和年轻 owner/epoch 重建属于配套测试框架方案的 directed check 与验收，不作为本 RM plan 的控制逻辑测试项。
### 14.6 被动真实请求与错误协同

1. 四种最终op均提供相应被动请求证据；删去请求记录但保留成功STA0必须失败。
2. cacheable zero覆盖DCache命中、miss、同slot普通Store合并和合法DCache replay；必须有wline、SBuffer->DCache真实接受及最终成功通知链；仅有ready或replay通知不能通过，不要求TL CBO或TL Acquire必现。
3. clean/flush/inval逐类核对内部CMO与实际TL A；错opcode、错line、错size、错owner、错epoch、同source换代误绑、重复和意外Uncache请求必须失败。
4. Uncache zero正常核对8个8B零写；第0/中间/第7段denied均核对合法前缀、异常终态与部分效果，出错段backend接受/拒绝两种策略均检查。真实AccessAck不注入corrupt，soft分类单独覆盖硬件corrupt映射。
5. 错序、重复、跳段、数据非零、mask错误、错误响应后仍继续发送均失败；末段fault已写64B仍不记normal pass。
6. watermark未覆盖返回WAIT，覆盖后缺失请求/响应立即失败；同拍内部与外部producer调度换序不改变关联结果。
7. 连续同地址不同owner、source复用、reset/redirect旧generation不得错绑；已提交owner迟到的额外CMO/zero段仍失败。
8. 前置fault没有数据/CMO请求；观察到请求时即使STA异常正确也失败。
9. 普通cacheable Store/cacheable zero实际response错误位保持0；Uncache zero合法denied可通过参数触发。
10. CMO接入统一错误入口后异常、分段效果和Uncache valid/bits通知各消费一次，不产生第二份错误ledger或expected entries。
11. 失败SC后执行cacheable zero、Uncache部分zero和MMIO已接受写，实际覆盖字节均更新SC shadow，未覆盖字节继续原检查；数据/entry及cursor不重复提交。
12. 错误行失效后由Store正常refill，再跟随CMO/Load时使用新版本raw；首次行选择保留但旧错误不能污染新消费。

## 15. Soft Test 计划
1. soft test 只验证 RM 可观察结果，不在本计划定义 sequence 调度算法。
2. 每类 CMO 至少包含一个正常场景。
3. zero 包含整行前后边界数据检查。
4. zero 包含同 line 多种 load size 检查。
5. zero 区分前置 fault 无副作用和 Uncache 响应 fault 部分写效果检查；已写 64B 的末段 fault 仍必须比较为异常。
6. non-zero 包含 read permission pass 场景。
7. non-zero 包含 read permission fault、外显 store access fault 场景。
8. non-zero 包含正常写回 `flushPipe=1` 检查；不要求 soft test 提供 `flushAfter` 控制事件。
9. 包含同一 CMO completion 重复轮询只消费一次的检查。
10. 包含 readonly 当前 owner/epoch 更新后的身份匹配检查；不在 RM soft test 中实现或验收 `UID+1` 恢复算法。
11. 包含旧 epoch completion 注入后的无副作用检查。
12. 包含 duplicate completion 注入后的无副作用检查。
13. 后续无独立异常的Load按overlay读取成功zero的零值或Uncache fault的确定性部分零值；若zero命中仍带旧error flag，则由统一来源规则比较异常，不强制该Load正常完成。
14. 包含 clean/flush/inval 后 load 原值检查。
15. test pass 判据包含真实请求集合compare、终态compare、统一错误通知compare和Store-table compare；RM日志只作诊断。
16. soft test按各monitor producer构造值型请求、watermark、source-generation与响应事实，覆盖缺失/多发/错绑；不得只构造STA0完成让请求检查自动通过。
17. coding时同步 `AI_DOC/seq` 的真实DUT与soft/framework两份CSV；本次仅修改方案，不登记虚构已实现用例。
## 16. 验收标准
1. `rm_ls_core.sv` 编译通过。
2. `memblock_rm.sv` 编译通过。
3. 现有标量 Load/Store 行为不回归。
4. CMO item 不再被当作 `RM_LS_KIND_OTHER`。
5. CMO item 不进入普通 `commit_store()`。
6. CMO 分类发生在普通 Store 从 Main table `src_0` 取 data 之前。
7. zero 的 RM `store_data` 固定为 0，且不依赖 Main table `src_0`。
8. clean/flush/inval 的 RM data 为规范占位 0、`cmo_has_data_effect=0`，且不依赖 Main table `src_0`。
9. zero 权限使用 write。
10. non-zero 权限使用 read。
11. non-zero fault 可正确比较 store 类 exception。
12. final STA0 来源检查生效。
13. zero `flushPipe=0` 检查生效。
14. non-zero `flushPipe=1` 检查生效。
15. exception 与 normal pass 互斥检查生效。
16. UID/ROB/SQ/epoch owner 检查生效。
17. stale completion 无副作用。
18. duplicate completion 无副作用。
19. 成功 cacheable zero 一次提交 64B 零 overlay；成功 Uncache zero 只经统一分段 helper 应用 8 段，结果为整行零且不重复登记 entry。
20. non-zero 不修改数据。
21. 前置 fault zero 不修改数据；Uncache zero 响应 fault 按统一错误方案只应用一次确定性分段效果，不能假定整行不变。
22. non-zero 的写回比较不以框架 `flushAfter` 事件为必要输入。
23. fault CMO 的异常比较不以 exception redirect 事件为必要输入。
24. 当前 CMO 的合法 terminal 只消费一次，重复 completion 不重复提交或推进 cursor。
25. RM 从 readonly 读取当前动态身份并过滤 stale completion；不计算 `UID+1` 恢复范围，不创建 epoch 或调度重发。
26. 后续无独立异常Load按程序顺序读成功zero零值或Uncache fault的确定性分段值；zero本身正常完成不保证清除hit旧error flag，不覆盖统一异常规则。
27. non-zero 后 load 按程序顺序读原值。
28. `expected_store_entries` 可覆盖 zero 的 64B 最终 Store-table 检查。
29. CMO核心不新增cache、metadata、Probe/MSHR、SBuffer/DataBuffer或RAW/forwarding模型；允许统一observer维护最小slot错误来源身份以保证实际消费闭环。
30. 不新增参数，除非复用既有 line size 常量无法完成编译。
31. directed check 全部通过。
32. soft test 全部通过。
33. 日志可定位op、路径、owner、epoch、request token/source generation、sample、水位、completion、`flushPipe`、exception和数据效果。
34. 对应DCache/Uncache monitor真实请求队列与内部路径探针均已连接；RM缺请求即使STA正常也不能通过。
35. cacheable zero已验证SQ wline至真实SBuffer->DCache请求/成功完成链，允许合并/replay且不伪造TL CBO；non-zero核对内部CMO和实际TL CBO；Uncache zero逐段核对真实请求。
36. watermark缺事实行为、错owner/source复用、重复/多发/错误后继续发送和commit后迟到非法请求检查均生效。
37. 所有新增CMO写效果均复用既有SC shadow/overlay/entry底座，失败SC后覆盖场景不误报；CMO denied/corrupt与Uncache zero部分效果仅由统一错误方案实现，`io_uncacheError_ecc_error_valid/bits`复用其监视与比较，不存在两套checker。
## 17. 实施顺序
1. 在 `rm_ls_core.sv` 增加 CMO 枚举和 item 字段。
2. 在 `rm_ls_core.sv` 增加 completion 值型对象和 model ledger。
3. 在 `rm_ls_core.sv` 增加最终 `fuOpType` 分类 helper。
4. 在 `rm_ls_core.sv` 增加 CMO data 归一化 helper，zero 设 data 0，non-zero 标记无数据效果。
5. 在 `rm_ls_core.sv` 增加权限类别 helper。
6. 在 `memblock_rm.sv` 修改 item 构造顺序，先识别 CMO并绕过普通 Store 的 Main table data 读取。
7. 在 `memblock_rm.sv` 分离 permission command 与 exception store class。
8. 在只读 observer API 上补足 STA0/SQ/epoch 事实读取，但不增加控制行为。
9. 在 `memblock_rm.sv` 增加 CMO completion 捕获。
10. 在 `rm_ls_core.sv` 增加 owner validator。
11. 恢复两个现有端口monitor必要发布并复用env FIFO唯一consumer；新增内部cmo_path被动agent的必要连接，同时按统一方案承载错误来源观察；先验证水位与请求关联。
12. 在 `rm_ls_core.sv` 增加复用commit_memory_effect及SC shadow的cacheable zero整行薄封装；Uncache zero错误/部分效果只接统一方案共享函数，不另行实现。
13. 在 `rm_ls_core.sv` 增加 CMO commit 分支。
14. 将 CMO completion 接入公共 owner/epoch 有效性检查和 stale/duplicate 过滤，不新增 CMO 专用年轻状态清理。
15. 在 `observer_process_current_commit()` 中协调真实请求、统一错误入口、写回、异常和唯一commit，不增加redirect消费或等待。
16. 补充 trace 和 error 文本。
17. 先运行纯 RM directed check。
18. 再运行 CMO soft test。
19. 最后运行现有标量 Load/Store 回归。
## 18. 风险与控制措施
| 风险 | 影响 | 控制措施 |
|---|---|---|
| readonly view 未暴露 SQ 身份 | owner 检查不完整 | 只补只读字段，不引入调度或 owner 写操作。 |
| STA0 写回 API 当前只支持 LDA 查询 | completion 无法定位 | 在同一 observer history 增加 STA0 查询，保持被动观测。 |
| permission command 与 exception class 共用一个布尔量 | non-zero 权限语义错误 | 拆成 `permission_write` 与 `exception_store_class` 两个概念。 |
| CMO 被 `size_bytes==0` 提前归为 OTHER | 完全漏检 | 分类必须先于 scalar size early return。 |
| CMO 复用普通 Store 的 `item.store_data=main_view.src_0` | 地址基址被误当成 CMO 数据效果 | CMO 分类必须先于普通 Store data 读取，并统一调用 CMO data 归一化 helper。 |
| zero 提前写 overlay | fault/stale 场景污染内存 | 只允许 `rm_ls_commit_cmo()` 调用 zero line effect。 |
| cacheable 64B效果中途失败或漏SC shadow | 部分清零或SC终检误报 | 整组先预检查，再无等待复用commit_memory_effect的SC shadow/overlay/entry；Uncache已发生部分效果仍按事实保留 |
| duplicate completion 重复插入 entry | 最终 Store-table 记录膨胀 | completion consumed ledger 在数据提交前后严格检查。 |
| ROB/SQ 复用误认旧 completion | 新实例被污染 | UID、ROB、SQ、epoch 四元联合校验。 |
| 把 `flushAfter` 当成 DUT 独立写回字段 | 写回比较依赖框架控制输出，或等待无法取得的事实 | RM 比较 DUT 最终写回 `flushPipe`；`flushAfter` 构造和恢复由测试框架负责。 |
| fault 比较等待 exception redirect | 已有异常证据仍无法完成 RM 比较 | 以 owner-bound STA0 异常和 fault terminal 做比较，不新增 redirect 等待。 |
| 在 CMO RM 复制年轻 UID 恢复逻辑 | 与测试框架形成两套恢复边界或 epoch 状态 | `UID+1`、取消、重建和重发归配套测试框架方案；RM 仅匹配 readonly 当前身份。 |
| 旧 actual 被误用于恢复后的实例 | load/CMO 比较被旧实例污染 | 复用公共 owner/epoch 有效性过滤，不回滚已提交 overlay，不新增 CMO 专用清理器。 |
| non-zero 误改数据 | load 期望错误 | non-zero commit 分支禁止调用任何 byte 写函数。 |
| line size 硬编码漂移 | 后续配置变化失配 | 优先复用既有编译常量，必要时只设单一局部常量。 |
| 新增 CMO 分支影响普通 Store | 标量回归失败 | `commit_store()` 增加 kind 防御检查并保留原逻辑。 |
| 日志只写异常名 | 无法证明 read permission | 日志同时打印 permission command 和架构 exception。 |

### 18.1 本次修订新增风险控制

| 风险 | 影响 | 控制措施 |
|---|---|---|
| 仅看STA0完成即判CMO成功 | DUT漏发/错发请求仍通过 | 强制联合monitor请求集合与完成比较 |
| 把sbuffer_agent当作SQ->SBuffer | cacheable zero无法证明请求类型 | 识别其实际为Uncache TL，另用内部被动path probe补wline与SB->DCache证据 |
| 将SBuffer合并或DCache replay当重复CBO | 合法行为误报 | zero逻辑token与slot传输attempt分开，按真实slot generation连接，不复制SB算法 |
| 端口无owner时按地址找最近UID | 同行/同source复用错绑 | 使用被动锁存owner、真实slot分配和source generation |
| monitor发布不全或等待缺失事件 | 漏检/永久等待 | producer及adapter水位覆盖后缺失即失败，CSR值不变仍逐拍发布sample |
| commit后删除owner摘要 | 后续非法多发未检查 | 保留终态token/段号摘要与安全回收游标，迟到事件继续核对 |
| fault zero一律清空写效果 | 后续load/Store-table失配 | 统一错误方案按合法接受段应用一次，前置fault与响应fault分别处理 |
| CMO与错误方案重复实现 | 双重entry、异常或通知消费 | 本文只调用共享错误与Uncache效果入口；唯一overlay与消费账 |

## 执行中补充/修正（IMPLEMENTATION_DELTA，公用被动观察，2026-11-11）

1. 当前实现以 `cmo_path_agent` 一份按全局 sample 锚定的只读批次采集 SQ/SBuffer/Uncache 以及必要外部 TL A/D，RM 是唯一 FIFO 消费者。外部请求证据仍是顶层真实 `valid && ready`，不依赖 responder 预建响应；此工程落点替代原 plan 恢复两个旧 monitor 整包发布的方式，避免产生无对端 scoreboard expected 流量。
2. 内部 SQ→SB 的 DataBuffer 仅有 sqPtr.value。root adapter 在真实接受时调用既有 readonly SQ 映射确定完整动态 owner，不新建整张 SQ uop 镜像。SBuffer `accessIdx` 是下一拍通知，始终匹配上一拍接受。
3. 公用 agent 还承载统一 denied/corrupt 方案的 errorArray、Load/Atomic 实际消费观察。接口为 Valid 时采真实 valid；D direct offer、原子 CSR latch 拍、Load 分项 AF/HE 和 split 消费细节仅由统一错误方案实现，CMO 核心不重复实现。
4. fault 的 ROB commit 可能早于合法 fault terminal。实际 STA0 已捕获但 readonly `sta_fault && !terminal_done` 时等待终态事实，不提前判为 normal/fault 不匹配；RM 不发 redirect，不计算 UID+1。

具体代码与验证边界见同日 DCache implementation review；统一错误接口仍由 denied/corrupt plan 唯一约束。

### [IMPLEMENTATION_DELTA] CBO AUTO 定向验证的控制 bootstrap（2026-11-11）

来源：真实cacheable zero专项在进入指令发射前报 `active control topology reached worker shutdown before bootstrap`。原定向builder覆盖父类 `build_main_table()` 并手工导入静态表，遗漏父类在AUTO表完成后执行的控制运行期bootstrap。

实现调整：仅在 `memblock_main_dispatch_cbo_directed_sequence::build_main_table()` 的 `import_manual_main_table()` 与control policy检查完成后，active control topology调用现有 `control_barrier_service.initialize_control_runtime_bootstrap()`。它复用既有reset/action握手，不重新建表、不重启worker；DISABLED保持不调用。父VSEQ仍唯一启动CSR/SFence worker，公用service的必要fatal保持原有语义。

影响范围：CBO专项builder及其源码review；无需修改公共控制服务或VSEQ fork。真实zero/clean/flush/inval需重编后复测，本条不把初次失败写为通过。

## Cold CBOAck故障验收的建表与bootstrap补充

普通CMO directed场景保留同line年轻Load/Store，覆盖恢复与数据交互；其年轻Load可先于CMO真实TL请求进入L2并冻结CLEAN，因此即使CBOAck权重100，也不能将该场景的无fault通过统计为CBOAck故障覆盖。

独立cold故障场景使用四项表：UID0目标nonzero CMO@0x80002000、UID1不同line Load@0x80003000、UID2不同line Store@0x80004000、UID3 CHECK_STORE。使用既有AUTO拓扑，DENIED或CORRUPT的CBOAck权重分别100，真实A.fire才按现有首行规则冻结选择，不预写错误历史，不更改CLEAN永久策略。

抽象功能职责：cold builder只提供静态场景和既有控制运行期初始握手；真实worker生命周期仍由父VSEQ唯一管理。

实现顺序为：初始化既有dispatch helper，要求AUTO，按最终nonzero操作生成四项表，import完成主表/状态，执行check_main_table_control_policy，最后必须调用control_barrier_service.initialize_control_runtime_bootstrap。任何覆盖父类build_main_table的AUTO测试builder都必须完成这个既有bootstrap，不能只导入表却跳过初始worker握手。

运行结束后必须断言实际UID0的sta_fault、terminal_done及独立期望异常命中，并只读核对首次行历史first_kind为CBOAck、状态为目标错误；否则报告测试错误。首次修复bootstrap前日志在267.8ns报告worker shutdown before bootstrap，不能作为故障功能通过记录；修改后仍需原seed真实复测。

### [IMPLEMENTATION_DELTA] 完成后重复审计与独立故障验收（2026-11-11）

- CMO成功提交后，readonly调用WB observer按ROB键登记已消费终态，后续每笔STA0增量审计；注册时补审该ROB已采后续记录。完整UID/epoch/ROB/SQ/STA issue/replay/instance-flush证明下，fault后唯一normal尾拍沿现有框架语义允许；第二fault或第二normal均报错，ROB复用新owner优先。该审计不生成redirect或更改框架终态。
- 冷目标故障原场景保留年轻Store和尾CHECK_STORE，以暴露真实恢复排空问题；同文件派生isolated builder/VSEQ仅包含目标CMO和两个不同line年轻Load，手工import按3项建表，仅接受既有DISABLED topology，执行追加`+MEMBLOCK_MAIN_TRANS_NUM=3 +MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0`。零control项满足DISABLED policy，父real-smoke持有唯一既有静态Sv39 CSR，不调用AUTO bootstrap；原cold AUTO类不改。isolated无writer、不插入CHECK_STORE，仅覆盖CMO真实请求/故障及后续Load，不覆盖Store-table或flushSb排空；原失败场景不删改。覆盖guard仍要求真实fault和首访CBOAck错误，不能把未注错的PASS当覆盖。

## 实施收口记录（2026-11-11）

本轮按用户明确授权执行coding，并在执行前完成本地基线提交。三个方案共享的请求/响应/错误实现仅保留一套，使用subagent按首次行历史、CMO核心与真实请求、统一错误来源和通知、根RM集成顺序完成。功能分段本地提交包括`7e334444f1`、`472bceefe0`、`1c8089cb4e`、`f3f45501d1`、`4dc0fbb7b5`；最终集成与归档提交见git历史。

真实VCS编译及基础/dispatch smoke通过；core920、请求/重复完成39、生产翻译权限92、Uncache103项检查通过。四种cacheable CMO、Uncache zero完整/首段及中段错误部分效果、Load响应错、原子门控与LR/SC/CAS、真实软件预取错误、isolated nonzero CMO denied/corrupt均完成对应比较。完整命令、CSV、代码差异和验证边界见`AI_DOC/plan/rm_review_doc/undo/memblock_cmo_d_error_rm_integration_review_20261111.md`第5章。

**保留未满足项：** 原四指令CMO fault后年轻Store与CHECK_STORE完整排空场景保持BLOCKED。波形已确认SQ重入及指针异常，但pendingst真实BE契约与框架撤销时序的责任仍需复核，未修改RTL，未以精简测试替代该项验收。详见`AI_DOC/analysis/testcase_flow/memblock_cmo_cboack_fault_sq_reentry_flush_stall_20261111.md`。真实Uncache zero末段首错、动态CSR及全部随机组合的多seed验证未穷举，不能计入已通过。

归档仅表示本方案编码、同步文档、必要验证和源码review已完成，保留以上真实DUT集成阻塞及未覆盖项；关联review继续保留在undo。
