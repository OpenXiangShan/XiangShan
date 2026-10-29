# CSR 非虚拟映射切换真实失效门禁方案

实现/专项状态：已完成源侧单Bug提交`5a4dc2df4d`及mother最终源码重新编译；源侧真实12/12、
mother真实14/14严格PASS，软件协议及15项隔离运输负例单列PASS。此plan按执行规则归入do；
整体旧WIP门禁的最终review/集成提交及矩阵恢复状态，以母任务台账为准，不把专项计作矩阵。

## 1. 术语

| 术语 | 含义 | 代码对象/示例 |
|---|---|---|
| mapping switch | SATP mode/root改变，旧或新至少一侧为分页 | Sv39↔Sv48、Bare↔Paged；纯ASID/权限不包括 |
| HS全fence | 非虚拟全地址全ASID SFENCE，包含global | valid=1、rs1=rs2=1、hv=hg=0 |
| C0/C4 | 顶层monitor采到fence/延迟4拍实际完成 | 现有raw fence调度与失效队列 |
| owner | 原CSR控制标记的唯一归属 | 保持kind=CSR，不伪装SFENCE控制标记 |
| barrier | 阻止年轻UID入场的既有CSR屏障 | child未sendover前保持CSR_CONFIG_PENDING |
| archive | 既有source内Git基线导出的专项编译目录 | 非Git worktree；真实index/WIP保持不动 |

## 2. 根因、范围和独立review

mother1494768720/1000在UID546旧mode复用；26012903/1000在UID912同类失败。
A1单独mode/root过滤候选修掉原四seed，但fresh26012903在UID941 query miss，8例为7PASS/1FAIL。
`TLBStorage.scala`有效表项不因CSR changed而清除，global可跨ASID保留；只删模型重建不是实际失效。
本Bug责任为测试框架：mapping switch未执行真实translation-cache失效及完成门禁。RTL不修改。

限定special、oldvirt=targetvirt=0、普通noS2xlate。target任何副作用前拒绝virt转换或virtual active路径；
非虚拟的无关VSATP/HGATP字段变化照常，不称其虚拟翻译已支持。
Turing于2026-10-29独立分类/最小范围及post-target方案REVIEW_PASS，无生产编码前blocker。
该方案替代A1大改作为当前场景根因修复候选；A1代码保留隔离，不提交、不合入。

## 3. 按职责实施

### 3.1 `hs_mapping_fence_required()`

抽象功能：纯判定SATP映射切换是否需要全fence，不读live CSR、不修改配置。
输入old/target；mode或root变化且至少一个mode非0返回真，纯ASID/权限返回假。
virtual范围检查由调用者在发送target前完成。

### 3.2 VSEQ与CSR worker注入

抽象功能：VSEQ把现有真实fence sequencer显式传给CSR worker，再传dynamic child；不查层次、不增加agent。
CSR worker仍拥有CSR action，fence worker只有普通SFENCE action；活动CSR屏障保证两种动作不重叠。
未需要mapping fence的legacy路径允许handle为空；special需要时为空必须在target副作用前fatal。

### 3.3 dynamic child真实fence阶段

抽象功能：保持原CSR barrier，确认target/PMP配置已稳定，执行真实HS全fence并等待观测完成，再允许CSR完成。
顺序：随机target→副作用前检查范围/handle→按原逻辑保存baseline sample与snapshot seq→写target/PMP→
等待monitor full92等于clean target且raw changed四位均退出、sample晚于原baseline→arm同CSR owner→
显式sequencer发送fence→等待同owner/event/reset的C0和C4→consume/disarm→stage candidate→mark sendover。
所有等待每次检查owner、control/l2 reset身份并用watchdog失败，不以固定4拍延时替代观测。
原expected write count、baseline sample/snapshot序号不被等待重采覆盖；原snapshot/PMA完成门禁仍保留。

### 3.4 common data的arm与C0/C4

抽象功能：复用现有fence观测槽把真实失效绑定到原CSR owner，绝不改变控制kind、audit或commit状态。
arm先验证active barrier、同owner、CSR_CONFIG_PENDING、pending action、观测槽空、payload为HS全fence；
冻结当前event baseline/reset，设置已有expected_sfence/armed字段。全部验证在状态写前。
C0记录增加CSR pending合法分支，仍要求payload/event/reset完整匹配；其余SFENCE路径不变。
统一C4失效继续注销canonical/range/parent/completed并按已有pending token时序取消，不伪造model删除。
child消费成功后清expected/armed/baseline字段；不记录SFENCE控制标记audit，不改变原CSR audit次数。

## 4. 隔离与单Bug提交

复用source原archive路径，不建立新worktree；从e2ed0f0cb重新导出未含A1的精确TB基线。
A1已冻结patch/tree和验证证据先保留。B1独立index、具名TB/localcfg/RTL输入hash、新mode编译；
实际路径禁止回读mixed树。commit-tree固定parent和已审tree，不推进source HEAD/真实index。
mother集成另按最终源码重新编译。真实WIP快照与指纹保持完整。

## 5. 验证与关闭

定向：global失效；Sv39↔Sv48、Bare↔Paged、root-only触发；pureASID/permission不触发；
真实C4前不允许sendover/年轻issue；错误owner/reset及超时负例。原有current-live合同不得反转。
最终B1-only tree fresh compile，原4＋fresh4共8个seed、1000请求全部重跑；Sv39/PBMT/fence邻接专项。
真实DUT所有raw/final/caught/demoted ERROR/FATAL为0，controller全PASS/unknown0；独立implementation review。
专项全部通过后单Bug提交，mother集成重新编译原seed/邻接通过，更新Buglist与状态表，才关闭旧WIP门禁。

## 与初步plan差异说明

放弃单独下游mode/root过滤的生产迁移，改修缺失的真实fence前置协议；保留current-live且不引ledger。
从pre-target fence改为post-target、等待完整稳定观测，消除C4后旧context再次refill的窗口。
收缩为已实测nonvirtual HS范围，VS/G协议不在本次伪称完成。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

- r1直接实例化抽象`uvm_sequence`，编译失败；改为同文件具体typed transport子类，
  只负责`start_item/finish_item`。r2重新编译及27项trigger定向通过，但不代表完整协议验收。
- r3用并行监护覆盖transport争用grant及等待item_done的整个阻塞区间；每1ns复核原owner、
  control/L2 reset及既有watchdog上限。异常时终止transport，不允许继续stage/sendover。
- `complete_hs_mapping_fence()`增加显式成功输出；默认失败，只有稳定target、transport及同owner
  C0/C4全部完成并consume/disarm后置成功。防止负向测试catch fatal后误走完成路径。
- r3冻结tree=`8d605e2cf351b735a34018e740f2030b1e7d32eb`，patch SHA256=
  `47250407a3258bc8c4cc395b42f3d57958145f4ab2e54c11ca52032968f5996b`。
  已发独立实现review并启动新mode重新编译，专项与review完成前不提交、不合入。
- r4软件协议定向首轮FAIL，根因为测试漏`publish_live_tlb_entry()`、仅建立completed；
  不是生产提前失效。修正为完整publish并补main数组、completed-only及索引/预约/audit逐项断言。
- 独立实现review发现post-target才采reset会把旧action重新接纳到新epoch。复用既有
  `action.control_reset_epoch/status.control_reset_epoch`，首次target副作用前冻结局部L2 epoch；
  后续只读复核原身份，不重采。special mapping路径的target/PMP grant/item_done同样加有界监护，
  失败显式停止后续beat和stage/sendover；legacy非mapping发送保持原路径。
- 测试范围补充分离软件零时间C0/C4协议与独立UVM transport负例：后者不得跨拍临时改写冻结topology
  或伪造ACK；只可用独立场景/build期driver替换，保留真实sequencer和原被测dynamic task。
- r6采用basicTest原CSR入口的factory派生负例，不新增testcase或生产配置。测试driver沿用reset安全idle，
  只替换main transport；真实CSR clean level、monitor full92及四路ACK是前提。无grant使用独立lock owner；
  无item_done仅最后一例，driver放弃held request并确认quiesced后，才对该隔离sequencer清理并结束。
  缺C4用生产schedule产生软件C0而不apply，不直接填写observation。此类负例不算真实DUT功能PASS。

## 最终候选验收进度

r8 tree=`90adb0e2b570b75678e6c62b04778284f0b9da38`；生产及fixture独立review PASS，
27 trigger/软件协议、15项隔离运输负例、原4+fresh4千请求8/8及PBMT0 safe邻接2/2 PASS。
SFENCE邻接2/2严格PASS，源侧真实DUT合计12/12严格验收；source Sv39旧基线已知a13d缺失FAIL由mother同seed严格PASS对照及独立review确认，
不在B1夹带旧修复。source单Bug提交、mother最终源码fresh compile/原seed及完整邻接前，门禁不能关闭。
