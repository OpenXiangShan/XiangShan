# L2TLB 可路由物理页随机覆盖方案独立评审

日期：2026-11-02。版本：V2。
评审对象：`AI_DOC/plan/test_framework/plan/do/memblock_l2tlb_routed_ppn_coverage_plan_20261102.md`。
评审方式：独立只读方案评审，核对现有源码和项目规则；未执行生产代码修改、编译或仿真。本结论不代表实现或测试已通过。

## 1. 术语与抽象功能说明

本评审判断地址生成方案是否完整、可落地，以及是否维持既有请求和配置生命周期；实现完成后仍需独立源码评审和专项测试。

| 英文术语 | 当前文档中的中文含义 | 代码对象或落点 | 示例 |
|---|---|---|---|
| PPN / PA / VPN | 物理页号、物理字节地址、虚拟页号 | `tlb_map_builder` | PPN需结合访问页内偏移形成PA |
| PMA / PMP | 物理属性与物理访问权限模型 | `memblock_pma_pmp_model` | c=1不创建路由出口 |
| PBMT | 翻译得到的访问属性 | `s1_entry_pbmt/s2_entry_pbmt` | 00采用PMA，01为NC，10为IO |
| NC / IO / MMIO | 不缓存主存、IO访问属性、按PMA分类的设备访问 | 现有MemBlock和参考模型 | PBMT00+c0重选后仍取c0 |
| sample / replay | 采样序号和已观测写事件的有序回放 | `common_data_transaction` | raw C写入在C+2生效 |
| generation / snapshot | 表版本及固定版本副本 | `pma_history` | C+1查询旧表，C+2查询新表 |
| shape / NAPOT | 翻译覆盖形状及连续页编码 | TLB条目的level/N字段 | 64KiB映射不能仅检查一个4KiB页 |
| allStage / GPA | 两阶段翻译及中间客户物理地址 | S1输出和S2 tag | 中间GPA遵守S2输入宽度 |
| intent | 上游生成时固定的目标地址意图 | 计划新增by-VPN表 | UART访问偏移位于128B窗口内 |
| reuse | 新条目复用已完成响应的物理页 | 公共PPN历史 | 必须按新条目属性重新判断资格 |
| token / owner | 一次真实请求记录及唯一生命周期管理者 | L2 responder | 不新增第二套队列管理者 |
| strict / fault | 严格地址范围模式和预期异常场景 | `MAIN_MEM_RANGES_EN`及现有权限流程 | 权限专项旁路新地址重选 |

## 2. 核对范围与源码依据

| 对象 | 核对依据 | 结论 |
|---|---|---|
| 实际外部路由 | `build/rtl/TLXbar_6.sv:123-177` | 应使用实际out1谓词，不能使用APLIC粗范围替代 |
| 原B08固定窗口 | `mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:160-197` | sparse统一压入512GiB窗口，确实限制cacheable地址 |
| PBMT阶段选择 | `src/main/scala/xiangshan/cache/mmu/TLB.scala:447-457` | allStage取S1非零PBMT，否则取S2 |
| 中间GPA编码 | `tlb_map_builder.sv::derive_allstage_raw_s2_tag` | Sv39x4和Sv48x4有独立输入宽度约束 |
| PMA匹配 | `memblock_pma_pmp_model.sv::entry_matches/find_first_match/evaluate` | 查询必须复用优先级、debug、未命中默认语义 |
| CSR写回放 | `common_data_transaction.sv::prepare_pma_pmp_csr_replay` | 原采集sample加2后写入模型历史 |
| 历史版本查询 | `memblock_pma_pmp_model.sv::get_generations_before_sample` | 实际比较effective sample小于等于请求sample |
| 请求构造边界 | `memblock_l2tlb_base_sequence.sv::capture_fired_request` | 新条目构造前必须准备属性版本 |
| 公共命中和复用 | `common_data_transaction.sv::get_or_create_l2tlb_entry_by_req_with_snapshot` | exact/range命中不应改写已发布映射 |
| 场景调度 | `memblock_dispatch_real_smoke_vseq.sv` | 可复用真实agent调度、CSR屏障和自然退出 |

## 3. 首轮问题与处理

| 问题 | 风险 | 最终方案处理 |
|---|---|---|
| 只保持c不能保持权限 | 同c区域可能有不同RWX、atomic或PMP结果 | strict、CSR权限专项、有效fault、PMA AF及response-only PTE旁路新策略；普通重选后由原RM检查最终地址，不承诺权限等价 |
| allStage放开S1后可能超输入位宽 | S2 tag构造在路由策略之前报错 | 独立约束中间GPA宽度；最终物理策略只作用S2输出 |
| 大页与目标窗口不兼容 | 普通随机配置因一次shape选择偶发失败 | 普通shape无候选时使用一次4KiB降级；显式形状不静默降级，仍无候选明确报错 |
| 跨PMA属性边界分类不明确 | 不知道应保持哪个c类别 | 用本次可表示的resolved anchor确定类别，完整替换shape满足同c和路由 |
| PMA区间遗漏debug特殊边界 | 区间内属性可能与原模型不一致 | 加入0x38020000和0x38021000，并复用原匹配逻辑 |
| intent生命周期不完整 | 同VPN冲突、重置或旧range命中可能吞掉目标 | 窄专项固定上下文、独立VPN、4KiB/N0、无reuse；先登记意图后发布主表；不在runtime reset后继续旧主表 |
| 场景和文档细节不足 | 无法直接执行coding或审查调度 | 补充专用vseq调度、主表发布屏障、自然drain及协同章节和函数差异 |

## 4. 第二轮问题与最终复核

第二轮发现原XOR候选可能超物理地址或S2接口位宽，以及旁路策略需要同时覆盖复用。最终版本已明确：

1. 先使用56/64位宽地址判断候选，禁止截断成48位后查询PMA。
2. 不可表示的原anchor不强行定义原c类别，直接在满足配置的合法集合内选择。
3. 最终策略结束后才进行S2 38位字段编码，避免合法化之前提前报位宽错误。
4. strict及权限/fault旁路同样跳过新增reuse过滤，保留原专项资格。

抽象功能说明：本次复核关注新构造条目从候选到发布的整体职责，不要求条目构造者消费CSR队列，也不允许重写已冻结请求。

修改前行为：sparse候选统一映射到B08固定窗口，PBMT在每阶段生成PPN之后选择；复用仅按既有形状、fault及接口资格筛选。

修改后计划行为：公共owner先提供成熟固定PMA版本；准备阶段冻结PBMT；候选使用宽地址及正确最终阶段判断，合法cacheable候选保留，其他候选按完整shape路由与属性集合选择；必要时仅一次4KiB降级；复用在相同只读版本下重新过滤，旁路专项保持兼容。

正确性检查：未发现该最终描述仍存在相互矛盾的必须修改项。C+2版本、allStage链条、宽地址编码、完整shape、命中只读、窄偏移意图与旁路边界均可形成明确实现入口。

## 5. 性能与验证边界

PMA导出只扫描32项配置及固定规模边界，发生于新条目构造；没有要求每拍扫描主表或UID历史。同一次构造只冻结一次generation并复用区间值，避免在每个窗口、候选或reuse循环中重复查历史。

后续必须按方案完成实际验证：静态路由一致性；软件边界和C+2专项；全量VCS编译；历史B08四个seed；至少三个真实窄窗口seed；cacheable、NC、IO相邻场景；实际memory-facing A.fire到达证据。当前评审没有执行这些测试，不声称仿真通过。

## 6. Plan对齐与评审结论

最终复核对象为本文件开头列出的方案，已核对其第3.3、5.1至5.5、6.1至6.5及验证章节。正式结论：**最终方案未发现必须修改项，可以进入按plan执行的代码修改、专项测试和独立源码评审。** 方案通过不替代实现和测试验收。

### 实现与Plan不一致项

本轮未进行实现验收，不对尚未完成的coding作一致性声明。后续implementation review需逐项记录实际差异。

### Plan未说明但Coding落实的细节

本轮不修改生产代码，因此没有本评审新增的coding细节；后续实现发现的新边界按执行规则回写方案并review。

## 7. 非本次修改的逻辑分析

评审前已执行`git status --short`。本次只新增本评审记录，不修改或提交其他文件。已有工作区内容归类如下：

| 类别 | 文件或目录 | 本次处理 |
|---|---|---|
| 其他RTL/接口分析 | `AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`、`AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、`AI_DOC/analysis/rtl/v2/index.md`、`AI_DOC/analysis/rtl/v2/flows/backend_memory_writeback_flow.md` | 与本次地址生成方案无直接实现关系，未纳入源码正确性验收 |
| 其他集成及缺陷分析 | `AI_DOC/project_management/memblock_mother_integration_status_20261028.md`、`AI_DOC/analysis/framework_design/memblock_b05_rob_sq_lifecycle_recheck_20261031.md` | 属于已有集成或B05方向，保留原内容 |
| 其他方案和评审 | `memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md`、`memblock_lsq_admission_b05_evidence_review_20261102.md` | 属于LSQ准入/B05方向，需独立评审 |
| 其他脚本或仿真文件 | `mem_ut/ver/ut/memblock/scripts/tests/test_runtime_parameter_priority.sh`、`mem_ut/ver/ut/memblock/signal.rc` | 参数优先级专项及信号配置，不属于本次方案评审 |
