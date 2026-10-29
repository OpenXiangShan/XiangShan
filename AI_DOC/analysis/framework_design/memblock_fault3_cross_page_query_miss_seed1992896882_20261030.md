# fault3跨页Load首段异常提前终止的根因与修复分析

日期：2026-10-30。状态：责任分类、方案及最终独立实现review PASS；r5原seed10000笔及21/21真实DUT专项通过，随本记录单Bug提交。
下文第1–5节保留发现/待确认过程，第6–7节为最终事实与责任结论，第8节为修复执行进展，不能把旧候选状态误读成当前仍未分类。

## 1. 原始复现合同与停止状态

- worktree：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028`。
- 生产SV输入：B1集成`60dcca7c6`；shell profile为`3bd0e9934`，2540项实际编译输入hash已复核未变。
- `tc=basicTest`、`ts=memblock_dispatch_real_smoke_vseq`、`cfg=tc_l2tlb_fault_2pct_no_trigger`、seed=`1992896882`。
- cfg绝对路径：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_fault_2pct_no_trigger.cfg`。
- 每seed10000笔，CHECK_TRIGGER0、ranges0、PMA/PMP model1，无临时plusarg；timeout100000000ns、UVM_LOW、monitor/record/coverage开启、首轮无波形。
- 原批次目录：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_post_wip_cov_20261029_results/fault3_final100_20261030`。
- 稳定日志：上述目录`/status/seed_1992896882.final.log`；monitor首错602001.100ns。
- controller退出rc1，1FAIL、19ABORTED、80NOT_RUN、0PASS、unknown0。根因状态先提交后联停：
  `committed_epoch_ns=1793295147707018991 < sent_epoch_ns=1793295155605732131`。

19个ABORTED不是19个独立功能失败；80个NOT_RUN未执行，专项闭环前不恢复它们。

## 2. 已有日志与源码事实

UID2401为FP Load，fu_op=1，访问起点`0x8ab4dfff`，日志显示RM在byte1查询下一VPN`0x8ab4e`失败。
首VPN为`0x8ab4d`；最终多次issue-hit记录使用token3825、generation672、response_sample118094。
CSR上下文为SATP8、VSATP/HGATP0、s2xlate0、csr_seq3。

601992.600ns，writeback状态记录异常向量`0x080000`；602000.100ns请求该UID fault redirect；
602001.100ns，RM进入跨字节翻译构造，在尚未执行数据错误预期计算之前查询第二页失败。
`0x080000`为bit19，即该版本的hardwareError；这不是日志已经证明的Load Page/Access Fault。
原稳定日志中未找到第二VPN的L2TLB accept，不能仅据此排除L1 hit或证明DUT完全没有后续翻译，需要波形核实。

关键源码位置（均相对上述mother根目录）：

- `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，`observer_build_commit_item()`：
  逐byte解析TLB；目前已在首地址翻译fault、PMA fault/MMIO或硬件不支持未对齐时停止，但数据错误检查在后续阶段。
- 同文件，`observer_capture_commit_actual()`与`observer_apply_load_d_error_expectation()`：
  已有独立L2 D-error ledger查询，但只有commit item完整构造成功后才进入该检查，故本轮先被第二页query miss打断。
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv`，`resolve_tlb_entry_for_rm()`：
  首VPN可用已完成UID payload，secondary VPN仍查current canonical exact/range；当前错误正来自secondary查询。
- `src/main/scala/xiangshan/package.scala`，`ExceptionNO.hardwareError=19`。
- `src/main/scala/xiangshan/mem/lsqueue/LoadMisalignBuffer.scala`，`s_resp`：
  split response出现有效异常或Uncache时可直接转写回，不保证继续发送下一段；是否为本轮实际路径尚未证明。

## 3. 候选解释与禁止提前得出的结论

候选：访问在首段/初始cache访问遇到数据错误后已经结束，DUT并不需要取得第二页翻译；
RM却先要求全部页翻译齐全，之后才预测数据错误，于是把“未执行的后续页”误判为缺状态。
这是当前优先验证方向，不是已确认责任。

尚需排除：第二页确实被DUT访问但框架未建/错误删除canonical；错误由另一动态实例的D-cache返回污染；
首地址翻译/PMA实际已异常而RM上下文不一致；或初始load与mabuf后续请求的实际执行顺序不同。
不得把status/DUT exception当作RM期望，不得看到bit19就跳过翻译/比较，也不得恢复未采用的A1 snapshot fallback。

## 4. 下一步门禁

1. 原tc/ts/cfg/seed/10000笔/timeout新compile带波形复现；保留原coverage mode，不覆盖旧日志。
2. debug mode=`mother_fault3_query_miss_debug_20261030`，结果目录为mother sim下同名`_results`，当前编译/复现中。
3. 核对UID2401初始issue、TLB hit/response、PMA分类、DCache/L2错误、mabuf请求/响应与最终LDA的ROB及采样身份。
4. 核对是否存在第二VPN真实访问，首异常发生在哪一段、哪一拍；以此区分模型越界查询与生产翻译丢失。
5. 原现象复现并完成独立责任review后才登记同周Buglist、编写最小方案并独立review；届时再按当前授权顺序coding。
6. 修复需覆盖原seed和确定性边界/相邻场景，专项通过后单Bug提交；恢复19个ABORTED并按规则处理80个NOT_RUN名额。

当前没有该问题的生产代码修改或Bug提交；原矩阵250/260通过与strict-NC10项延期状态不因此改写。

## 5. 独立预分析与复现进展

Turing独立只读预分析支持RM检查顺序缺口为候选，但结论仍为`PAUSED_ANALYSIS`，不批准编码。
需区分初始LoadUnit异常抑制mabuf admission、buffer首段异常终止及S3迟到错误等路径；
line/sample的D-error事实不是UID执行证明，不能用写回时line污染替代首段处理时已经污染的证据。
特别需要把原log601930.1ns的`line=0x803688efc0`与UID2401实际首PA绑定，不能仅凭时间邻近认领。

原seeddebug fresh compile已PASS，随后在**相同602001.100ns、相同UID2401/第二VPN**复现原query miss；
controller已退出，1FAIL、unknown0。波形绝对路径：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_fault3_query_miss_debug_20261030/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=1992896882_rtl_eda_reg_20261030_013722_3323996_16145.fsdb`。
波形时间单位1ps，范围0–617380100ps，203567个unique signals；正在提取原失败窗口。
本机通用reader wrapper固定到另一版FsdbReader库，不能直接使用；已通过与本轮VERDI_HOME一致的
R-2020.12-SP1 reader库直接调用reader二进制成功读取metadata，未修改工具安装。

## 6. NPI波形事实闭合

权威数据为debug结果目录`uid2401_npi_window.json`、自动断言报告`uid2401_wave_acceptance.json`，
使用Verdi V-2023.12-SP1内置Python3.6/NPI读取同一FSDB的HexStrVal，时间单位ps。
R-2020.12-SP1没有该脚本要求的内置Python路径；通用native reader对3/50等非整nibble位宽显示左移，
其CSV只作排查存档，不用于数值结论。NPI明确读取state3→5→0、真实VA8ab4dfff，避免把native的6→a→0误认成RTL状态。

| 波形时间（ns） | 同一实例的事实 | 含义 |
|---:|---|---|
| 600560.100 | mabuf保存ROB1/121、VA0x8ab4dfff | 与日志UID2401最终issue/写回身份一致 |
| 600570.100 | state2、curPtr0、unSentLoads3，首段valid | 尚有两个拆分片段，当前处理低段 |
| 600600.100 | 环境reserve line0x803688efc0、source0、accept_sample120081、state2、generation3 | 独立错误注入记录，不取DUT异常作期望 |
| 601930.100 | 环境commit同line/source/generation，sample120347，activation120347/0 | corrupt事实在最终低段完成之前已生效 |
| 601965.100 | mabuf仍ROB1/121、curPtr0，低段valid/ready1，VA0x8ab4dfff | 多次重放后最后一次低段请求，没有转到高段 |
| 601970.100 | state3；LU1有效请求对应s1 PA0x803688efff | 首段PA的cacheline正是0x803688efc0，排除仅时间邻近错误归因 |
| 601980.100 | split response valid，ROB1/121、VA0x8ab4dfff；ex19=1，ex4/5/13/21=0，nc/mmio=0；LU1 s3 ROB1/121，Dcorrupt1/denied0 | 实际是buffer低段的cache corrupt返回，不是初始admission被抑制、NC/MMIO或高段翻译异常 |
| 601985.100 | state5、unSentLoads0、curPtr0、globalException1 | 低段异常直接转写回，未发送高段被取消 |
| 601990.100 | buffer回idle | 没有继续执行第二页 |
| 601992.600 / 602001.100 | UID2401 WB/status HWE；随后RM第二页query miss | RM在完整翻译构造阶段被未执行页打断，未到独立D-error计算 |

自动遍历三个LoadUnit全部valid TLB request，600500–602020ns窗口无VPN0x8ab4e请求；
LU2在600540.1ns发初始首页请求，LU1之后持续处理低段首页。不是仅凭“没有L2 miss”推断没有第二页L1命中。
原回归与debug的effective plusargs逐字节一致，异常时间/UID/VPN也一致；所有controller均已退出、unknown0。

以上已作为正式独立责任分类review输入并通过。仍未修改生产代码、未用DUT ex19直接生成RM期望。
最小修复必须解决独立低段错误判定的身份/时序依据，尤其覆盖“低段响应后、WB前才污染”的反例，
不得只按WB时line已有错误就停止翻译，也不得伪造第二页PA或放宽异常比较。

## 7. 正式责任分类结论

Turing于2026-10-30独立读取NPI精简窗口与日志/源码，返回`REVIEW_PASS`：已确认RM的提前终止建模与检查顺序缺口，
本次没有RTL责任证据，也不是current-live或snapshot复用问题，不应修改resolver。
主agent独立完成NPI事件断言、三LU第二页请求遍历、实际首PA与独立D-error line匹配及同时间/UID复现比对，结论一致。
正式记录：mother `AI_DOC/buglist/rm/v2/rm_buglist_2026-W44_20261026.md`的同名首段数据错误章节。
历史阶段说明：完成责任分类时仅进入最小方案设计；当时尚未获得coding门禁。当前专项结果见第8节，不把旧停止点当最新状态。

## 8. 修复执行进展

正式方案：`AI_DOC/plan/rm_plan/do/memblock_first_split_data_error_termination_fix_plan_20261030.md`。
Turing多轮review已关闭高页标记必须跨重放保持、纯cache/非replay资格、消费完成连续边界及deferred有界重试问题，最终方案`REVIEW_PASS`。
母worktree顺序实施，不改RTL、resolver、cfg或异常比较mask。现有四项无关知识库脏文档保持原状。

真实最小修前复现：同basicTest/vseq/fault3 cfg、seed26013001，临时仅LH跨4KB、level0、无翻译fault、
PTE U/R/W/A/D/V有效、corrupt100%。3498ns UID0/VPN825ce query miss，WB为HWE，独立corrupt在取数前生效；
结果为`mem_ut/ver/ut/memblock/sim/mother_first_split_compact_pre_fix_u_20261030_results`，1FAIL/unknown0、controller退出。
较早U=0尝试虽PASS，但只产生合法PF，没有cache数据访问，不计复现或修复验收。

开发r1–r3编译失败均保留：先后发现旧调用参数、主表字段命名和VCS output默认实参限制；最终保留三参数wrapper，
生产改用独立四参数构造接收deferred。r4 compile PASS但真实最小用例FAIL；NPI证明result_valid=0时MSHR选择payload为1，
删除无效payload否决后，r5同参数最小真实DUT通过。各轮失败不是新的独立功能Bug，不覆盖原归档。

原seed1992896882的10000笔已完整STRICT_PASS，目录`mother_first_split_original1992896882_r5_20261030_results`，
RM比较10000次、六项错误计数0、正常finish，原effective plusargs逐字节一致且无临时覆盖。
真实DUT专项合计21/21（原seed1、LH corrupt1、denied1、clean3、LW3、LD3、Sv393、CSR2、PBMT2、SFENCE2）。
软件provenance/cancel/protocol r2/priority均独立PASS，预期caught计数单列，不混算DUT严格通过。
协议r2实际执行生产RM的deferred恢复及256poll报错，生产L2TLB消费在102缺口后冻结101；
READY103/104 admission-settled断言证明并非前置早退造成假阳性。fixture及2541项输入清单随r2目录保存。
Turing最终`FINAL_IMPLEMENTATION_REVIEW_PASS`，无剩余blocker，单Bug提交前不恢复剩余99项。原100seed首轮0PASS/1FAIL/19ABORTED/80NOT_RUN保留，
跨轮当前1/100、剩99。250通过旧SV基线覆盖率不能称为本修复后的最终版本覆盖率。
