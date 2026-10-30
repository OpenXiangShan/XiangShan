# Rollback同拍来源关联修复方案

日期：2026-10-30。状态：责任分类、方案和最终实现Turing独立review均PASS；coding、fresh compile、软件专项及20/20真实DUT专项通过（两个原始10000笔根seed加18邻接）。方案移do，随本记录单Bug提交；提交后恢复非strict-NC批量，不提前宣称350项或最终coverage完成。
范围：V2母任务测试框架事件关联，不修改RTL、RM、cfg、monitor采样边界或DUT激励；不新建worktree。

## 1. 定位、专有名词与抽象功能说明

| 术语 | 当前中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| rollback | DUT要求后端重新执行的回退事件 | `memoryViolation` | RAW-nuke0和1可先后针对同一Load产生事件 |
| sample | clocking block一次边沿前采样的单调编号 | `sample_seq` | 144892与144894是两个独立观察时刻 |
| winner/candidate | 六路回退候选中的组合获选者/候选 | `rollback_winner/candidate_*` | winner4之后两拍可变为winner5 |
| provenance/payload | 来源证明及该拍完整诊断内容 | `dispatch_raw_ctrl_t` | 保存winner、活动候选与原因 |
| cache | monitor保留的有限拍来源记录，不是DCache | `rollback_provenance_cache` | 4拍历史不能授权本拍输出 |
| same-sample | 输出只关联同一采样编号的输入证明 | resolver的sample筛选 | 输出144894只能读取144894 |
| conflict | 同一采样编号、相同完整ROB/level出现不一致证明 | `conflicting` | 同拍重复记录内容不一致必须拒绝 |
| reset | DUT复位及既有来源缓存清除 | `clear_rollback_provenance_cache()` | 清除后不得借旧记录关联 |

抽象功能：本方案只修复框架对已观察DUT事件的来源绑定，保证adapter接收当前拍的正确回退来源；
不构造新请求、不修改DUT正确性预期，不把合法异常或回退当作非法激励。

## 2. 已确认根因与功能流程

原入口：`basicTest / memblock_dispatch_real_smoke_vseq / tc_l2tlb_fault_2pct_no_trigger / seed1705742557 / MAIN10000`，
timeout100000000ns，无临时plusarg。724655.8ns的ROB1/126、level1因跨拍payload比较报ROLLBACK_XMR。
原参数波形同时间/ROB/sample复现；NPI证明144892为winner4、144894为winner5，中间无输出，两次都与各自本拍输入一致。
Turing正式责任分类`REVIEW_PASS`：测试框架责任，非RM、非RTL仲裁缺陷。
V2 `MemBlock.scala:1438–1454`及生成RTL30512行是组合Mux1H；输入/输出均由同mon_cb按1step采样。

最终流程：monitor保存本拍完整来源 -> resolver精确筛选同sample与ROB/level ->
同拍多份证明一致才返回 -> monitor/adapter原样校验selected及原因 -> 原handler处理。
同ROB跨拍来源变化相互隔离；本拍证据缺失或冲突仍失败，不用latest/历史fallback。

## 3. 文件与不变边界

| 文件 | 修改内容 |
|---|---|
| `common/memblock_common/src/memblock_sync_pkg.sv` | 仅收紧resolver的sample条件并补中文语义注释；保留缓存清除、4拍保存上界、完整payload比较和调用接口 |
| `seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv` | 仅更新本Bug相关`run_rollback_cross_sample_cache_scenario()`合同与反例；不运行/修理无关softcase |

上表路径相对`mem_ut/ver/ut/memblock`。无新字段、runtime参数、cfg、接口、filelist或生产helper。
缓存仍由既有monitor写入/复位清除；保存上界只是存储保全边界，不能解释为输出允许延迟。
高频resolver只扫描原有至多有限采样窗口，不扫描主表或增加queue/owner。
只读协议、selected唯一性、活动输入X/Z、ROB flag/value、level及原因检查均保留。

## 4. 主流程与helper

### 4.1 `memblock_sync_pkg::resolve_rollback_output_provenance()`

抽象功能描述：monitor收到有效输出时，用当前采样身份返回该拍获选输入的完整来源；无法证明时明确返回失败。
它不决定RTL回退优先级，不生成redirect，也不改变后续事件内容。

```text
清零输出snapshot、input_sample_seq和conflicting，选择索引置为无。
沿用既有过期清除：从队首删除超出4拍保存边界的记录。
遍历现有小缓存，跳过无效、sample不等于output_sample_seq或完整ROB/level不匹配的记录。
第一份匹配记录成为候选；后续同拍匹配记录由payload_equal比较全部既有来源字段。
若同拍内容不同，置conflicting并失败，输出仍为初始化值；一致重复记录允许。
没有匹配则失败；有匹配则复制该拍snapshot和input_sample_seq并成功。
```
中文文字伪代码：本函数只绑定本拍来源。先初始化失败输出，再调用原队列pop操作清理已过期记录。
随后按采样身份和完整ROB/level过滤；跨拍记录不参加候选或冲突判断。`rollback_provenance_payload_equal()`
仍比较完整候选/winner/原因/附加诊断，用于拒绝同拍相互矛盾的证明，不删除其任何字段。
成功只复制值型snapshot，不清除当前记录、不推进UID或驱动RTL；未命中或冲突由原monitor报错。

### 4.2 `soft_test_memblock_pending_mmio_directed_sequence::run_rollback_cross_sample_cache_scenario()`

抽象功能描述：仅直接调用生产cache/resolver方法，验证采样身份隔离和失败保守性；不启动其他软件场景或模拟DUT正确行为。

```text
清空缓存；构造原有合法winner/ROB/level完整snapshot。
保存sample100，要求输出100成功且精确复制；输出102无本拍证据必须失败。
保存同key但不同winner或非winner诊断的sample102，要求输出102成功且仅复制102。
验证错ROB flag、ROB value、level、本拍缺失和未来记录均拒绝，且失败输出清零。
相同sample存入完全相同重复记录应成功；同sample存入不同payload应conflict失败。
验证过期清除、显式reset清除后无旧证明；最终清空缓存，打印本Bug唯一完成marker。
```
中文文字伪代码：测试先用已知记录建立可关联前置，再分别改变时间、身份和内容。
每次调用生产resolver后同时检查返回值、conflicting、输出sample及snapshot，确保没有旧证明回退。
用不同采样编号的来源变化复刻本次根因，再以同拍冲突反例证明没有放宽真实冲突检查。
测试只复用现有缓存API，不在公共类增加软件专用方法；保持当前task名以避免无关调用点重命名。
其中winner变化必须明确复刻原例RAW-nuke0的winner4到RAW-nuke1的winner5，不仅测试非winner变化。

## 5. 验证与提交门禁

1. 先完成本正式方案独立review，主agent复核，无必须修改项后才按母任务已有测试框架coding授权实施。
2. 生产修改严格限一处resolver逻辑，软件仅一个相关task；`git diff --check`及调用点/旧合同检查通过。
3. 用ignored证据目录外加`rollback_same_sample_gate_vseq extends virtual_base_sequence`，通过现有TB_LIST追加，
   以`basicTest / rollback_same_sample_gate_vseq / default`调用上述唯一task；不新增生产testcase或filelist。
   该vseq仅创建既有软件sequence并调用目标task，不启动其body、其他场景、agent sequence或active owner。
   fresh compile后软件日志要求唯一marker、正常finish、raw/final/caught/demoted ERROR/FATAL均0；不算真实DUT通过。
4. 原seed1705742557以原tc/ts/cfg/MAIN10000/timeout1e8、无临时覆盖fresh compile复测，必须完整严格PASS。
5. 邻接真实DUT覆盖原首段seed1992896882、原Sv39 seed212889620、PBMT safe/cancel/CSR/SFENCE及新随机seed；
   保留严格比较与输入hash，必要软件负向不混算DUT PASS。保留原FAIL、ABORTED及NOT_RUN。
6. 独立最终源码/动态review PASS，生成逐函数implementation review；同步分析/Buglist/母状态/历史review说明。
   plan完成后移do，review留undo，每Bug唯一源码commit，不混入四项既有知识库脏文档。
7. 提交后再恢复剩余回归；新源码的最终350项与覆盖率重新确认，旧1343的50组内PASS不能冒充新SV最终结果。
   当前fault3首错已停止为1FAIL/19ABORTED/79NOT_RUN，余下恢复应保留19颗ABORTED原seed并另生成79颗不重复替换seed。

本方案不实现RM/checker/covergroup，不修改异常预期或覆盖率类型。最终覆盖率仍由通过日志白名单独立汇总。

## 6. 文档同步

更新W44 RM目录中的测试框架责任章节、rollback根因分析、母任务状态及总问题/方案review。
历史`memblock_load_mmio_r2_active_survivor_resolution_review_20261014.md`保留历史事实，添加本方案更正入口，
不把旧跨拍容错描述继续当作当前合同。无须更新V2 RTL知识库或四项既有脏文档。

## 与初步plan差异说明

本方案由已确认责任分析直接形成，没有独立初步plan。相对静态候选讨论，最终证据是winner4→winner5而非仅非winner变化。
修改目的：让采样编号参与事件身份，保留真实冲突的拒绝能力。
修改前文字伪代码：resolver扫描age0–4所有同ROB/level记录，任意完整诊断不同即失败；缺本拍证据仍可能借旧记录成功。
修改后文字伪代码：resolver只允许精确同sample的完整ROB/level记录进入候选，payload_equal只比较同拍重复证明；
没有本拍记录则失败，跨拍winner变化互不污染。该函数职责、cache API、存储边界、所有配置和激励均不变。
实施中如出现额外修改，必须新增IMPLEMENTATION_DELTA并重新review，不能隐藏扩大范围。
