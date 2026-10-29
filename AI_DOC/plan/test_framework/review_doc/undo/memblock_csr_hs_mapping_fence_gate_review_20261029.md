# CSR HS映射失效门禁方案与实现评审

## 1. 术语

HS全fence指非虚拟SFENCE全地址全ASID（包括global）；C0/C4指monitor观测和既有4拍生效边界；
owner是原CSR控制标记，不能假扮SFENCE控制标记；archive是既有source内隔离源码归档，不是Git worktree。

## 2. 责任与方案评审

2026-10-29，Turing（01a1875e-af6d-7941-b1e1-bd1bd9c69010）确认：CSR changed对inflight的处理
不能证明L1 storage已失效。mapping mode改变后仅删模型可能和DUT global缓存分叉；须真实失效完成。
正式方案`memblock_csr_hs_mapping_fence_gate_plan_20261029.md`为REVIEW_PASS。

限定special oldvirt=targetvirt=0，SATP mode/root变化且至少一侧分页；纯ASID/permission不发fence。
先写target/PMP并等full92匹配clean target（含changed归零），保持原barrier，再驱动真实HS全fence，
按原CSR owner/event/reset等C0/C4，consume后清预约字段，最后stage candidate/sendover。
不覆盖原baseline、expected write count或snapshot/PMA完成门禁，不引A1、snapshot或ledger。

## 3. 实现第一轮

patch dd86a4f90af3c286f30a6517b9d3d2b2dd868e9a22ba7f4cb0fd504115127194，静态REVIEW_FAIL：
1. 直接实例化virtual `uvm_sequence`，编译确实报SV-ACCNBI，rc2，尚无功能仿真。
2. start_item/finish_item阻塞期间没有watchdog和owner/reset监护，需覆盖transport整个阶段。

第一项已改同文件具体typed transport子类；第二项待补，完成前不得形成Bug提交。
27项trigger定向尚不能代替global/C0C4/超时或原8seed真实仿真。

## 4. 实现第三轮待验收

r3已补具体transport类、覆盖grant/item_done阻塞的owner/reset/watchdog监护以及显式成功输出，
修正第一轮两项blocker。冻结tree和patch见plan的IMPLEMENTATION_DELTA。
2026-10-29再次交Turing独立只读review；新mode`source_csr_hs_fence_r3_20261029`编译/trigger专项进行中。
当前不能标记REVIEW_PASS或验证通过；仍需完整协议定向、原4+fresh4真实1000请求及邻接专项。

与原plan差异：具体transport类和失败后成功输出是必要实现细节；不改变批准的post-target
真实HS全fence、原CSR owner和current-live合同，不增加A1过滤、residency或RTL修改。

## 5. 第三、四轮review结果与整改

Turing独立`REVIEW_FAIL`：post-target采reset未对action/status原control epoch，须在首次target副作用前
冻结身份并贯穿稳定/transport/C4；special mapping的target/PMP阻塞也须监护，否则item_done丢失时
到不了事后检查。主agent正在按最小批准边界整改，不扩legacy非mapping路径。
r4定向实际FAIL@200.100ns：测试build返回detached entry，未publish就断言live存在。已通过真实publish
补canonical/range/parent，补main数组和completed-only覆盖；不是生产提前删除的证据。
当前未给最终REVIEW_PASS，未提交/集成，r3真实8/8保留原版本证据，不转记到修改后tree。

## 6. r6验证进度

r5编译仅因测试空结构比较使用无类型assignment pattern失败，未启动功能仿真；r6补显式类型后
fresh compile PASS。r6纯trigger及global/C0/C4/owner/event/reset/提前sendover/年轻admission软件协议
定向1/1 PASS，controller rc0、unknown0。隔离transport负例进行中，生产reset/写阶段监护等待独立review。
tree=`203ea2ea7d255ad788ea935def918fedd1d0d10c`，patch SHA256=
`79c3c06547b0b4ee5da895fca65846703443d86f08e7f0c9de5aeeb7232d297e`。

## 7. 最终r8实现Review

2026-10-29，Turing独立生产静态`REVIEW_PASS`，r8 fixture修正`REVIEW_PASS`；没有剩余生产blocker。
Kuhn复核隔离真实UVM运输负例，明确不作为DUT功能PASS。最终tree=`90adb0e2b570b75678e6c62b04778284f0b9da38`，
patch SHA256=`39c4092c1cabf4d100f22d547a2e6da127c1eb8c62fff32298d57cef6a3d25e4`。

### 7.1 功能、前后逻辑与正确性

| 特性 | 修改前 | 修改后 | 检查与风险边界 |
|---|---|---|---|
| 映射切换真实失效 | target/PMP写后直接candidate，storage global可残留 | SATP mode/root改变且至少一侧paged时，全HS SFENCE及C4完成后才candidate | 27项trigger、global/live/completed软件失效、真实8seed；虚拟路径副作用前拒绝 |
| 原身份跨等待保持 | post-target才采当前epoch，旧action可能重新接纳 | target前核对既有action/status control epoch、冻结局部L2 epoch，此后只比较 | stale action/status/L2、等待期owner/reset负例；不修补旧owner |
| 阻塞等待有界 | grant/item_done可能永不返回 | special mapping target/PMP与fence typed transport旁路监督 | 无grant、无item_done、owner/reset、超时；kill不等于撤销driver已接受的写 |
| 单CSR owner | 普通SFENCE观测只接SFENCE marker | 同一CSR_CONFIG_PENDING可预约已有两槽 | 不改kind/audit，不引双完成真源，失败不sendover |
| 完成前置 | CSR sendover未防armed | 拒绝残留expected/armed；finish同owner/event/reset消费并清预约 | before-C4、提前sendover、exactly-once、全预约清零、年轻admission仍阻塞 |
| 显式依赖注入 | CSR child无fence handle | VSEQ→CSR worker→dynamic child | 不查硬编码层次，无新agent，非mapping允许无handle |

### 7.2 字段与调用关系

| 对象 | 含义及读写责任 |
|---|---|
| `mapping_fence_sqr` | VSEQ注入现有真实fence sequencer，worker传递，只有special mapping使用。不是新队列/producer生命周期。 |
| `action_l2_reset_epoch` | body首次副作用前冻结的局部L2代；所有后续等待只读比较，不跨action保存。 |
| `action.control_reset_epoch` | service已创建的原control代，复用既有字段；必须与status/current一致，不重新采样覆盖。 |
| `transport_done/failed`、成功输出 | 单次task局部握手/监督结果；初始失败，仅完整正常路径置成功，调用者失败即return。 |
| `control_expected_sfence/control_sfence_c0_armed` | arm设置，C0匹配与sendover防线读取，成功finish清除；不改控制kind。 |
| `control_sfence_c0/effective_observation` | adapter在schedule/C4写入，child成功finish消费两槽；保留普通SFENCE service路径。 |

调用顺序：VSEQ负责真实sequencer注入；worker从既有队列取action；dynamic负责求解、发送、监督、等待；
common data负责预约及统一C4观测/删除；control service仍独占最终snapshot/PMA检查和commit/retire。
所有有界等待只读查询当前marker与观测，不加入每拍全主表扫描。

### 7.3 原身份判定源码支撑

抽象功能描述：拒绝跨reset或owner变动后继续完成旧action，不以“现在系统已ready”代替原身份。
源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dynamic_csr_change_sequence.sv`，`mapping_action_current()`。
函数功能简析：读取原action、status和两reset域；只返回真假，无状态副作用。下面是决定身份的核心返回分支。

```systemverilog
return memblock_control_owner_equal(status.control_owner, action.owner) &&
    status.control_kind == MEMBLOCK_CONTROL_KIND_CSR &&
    status.control_state == MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING &&
    status.control_action_enqueued && status.control_reset_epoch == action.control_reset_epoch;
```

中文伪代码：

本分支负责marker归属的最后一层核对。入口已排除无效UID、barrier不归属、control未ready、
原action/control代不同、L2 reset active或L2代改变；随后先用owner完整比较验证UID/dynamic代/action代/kind，
再要求status仍为CSR pending且动作未sendover，最后核对status保存的control代等于原action。
任一条件为否就返回失败，调用者停止后续发送/完成；不更新epoch或修复status。

### 7.4 Candidate前置源码支撑

抽象功能描述：真实C4成为既有candidate/sendover之前不可绕过的条件。
源码位置：同文件`body()`，发送target/PMP之后、candidate之前。
函数功能简析：输入clean target、原sample与冻结L2代；任务失败即return，原snapshot/PMA门禁不被替代。

```systemverilog
if (needs_mapping_fence) begin
    complete_hs_mapping_fence(data, clean, baseline_sample,
                              action_l2_reset_epoch, mapping_fence_completed);
    if (!mapping_fence_completed) return;
end
```

中文伪代码：

本分支只处理需要真实失效的映射切换。先调用完成任务：它等真实clean target，arm同owner，
经真实fence sequencer运输，等adapter的C0/C4并消费清预约；失败保持输出为否。
随后检查成功输出，为否立即结束，不执行后面的stage和sendover；为真才继续原candidate/model验证。

### 7.5 验证结果及边界

- r8 fresh compile PASS；provenance seed26012914，27 trigger及global/live/completed-only/C0/C4/归属/预约/audit/
  提前sendover/young admission协议PASS。software预期caught ERROR/FATAL=2/4，不计真实零错误。
- transport seed26012913，15例PASS、caught FATAL15、ERROR0、final/demoted0，无zombie/迟到item_done错误，正常finish。
- 15例包括stable timeout、fence/CSR write无grant与无item_done、stable及运输/观测等待期owner/reset拒绝。
  scenario8/9是post-transport的C0/C4观测等待，不声称已建立C0后的C4；只有scenario3用生产schedule建立软件C0。
- reset负例后真实CSR driver会清hold；r8先等真实reset ACK/ready，再真实重发clean并等新full92，最后新建下一case
  action，绝不把失败旧action重新绑定。两noack分别放在对应sequencer最后使用处，driver quiesce后才stop_sequences。
- 原4+fresh4最终r8千请求8/8、PBMT0 safe2/2、SFENCE1k2/2严格PASS，合计12/12，controller rc0/unknown0，
  主agent逐日志核对一次TEST_PASS、正常finish、raw及六项final/caught/demoted ERROR/FATAL全0。mother集成仍待完成。
- source Sv39邻接seed212889620是已知a13d later-split异常优先级旧基线缺陷：UID462、VA0x86ef7fff、120327ns
  与历史签名一致；mother本轮相同seed/参数严格PASS。不能把a13d与c976 redirect取消混称同根因，不迁入B1。

### 7.6 与Plan不一致/补充细节

具体typed运输类、显式失败输出、写阶段监督及独立factory负例属于IMPLEMENTATION_DELTA，已独立review；
非原plan隐藏功能。legacy非mapping握手保持，虚拟翻译不扩展，A1过滤/residency/RTL均未引入。
测试factory override仅由独立运行显式启用；普通真实8seed及邻接不带override。无新生产plus或testcase。

### 7.7 精确输入与提交门禁

r8具名TB/config清单480项通过；VCS实际parse路径清单证明仅archive、固定共享RTL和Synopsys输入，
没有回读原mixed TB。fingerprint simv SHA256=`facf99a532803a84e1803ad78a9bf2f768c23f8330a47f6cb5046d0b49cebd31`。
源码候选的`mem_ut` tree未再改变；正式提交另纳本Bug plan/review/flow/Buglist，不含source混合W44 startup章节。
原source真实index的binary diff与最初快照逐字节相同，四项WIP快照hash再次通过；source HEAD不推进。
源侧专项和独立review门禁完成，可形成单Bug commit；不等于mother最终集成验收或允许恢复批量。

## 8. Mother集成动态验证

源提交`5a4dc2df4d`；mother精确应用后独立静态review PASS，a13d/c976/91d/prefix/7632ade均保留，
未引入A1/ledger/RTL修改。mother新mode fresh compile，原8千请求＋Sv39/PBMT safe/SFENCE各2，
14/14严格PASS；软件协议及15运输负例单列PASS。所有controller rc0、unknown0、未完成分类0；
真实日志六项计数全0。完整验收报告及2540项实际输入清单见mother
`mem_ut/ver/ut/memblock/sim/mother_csr_hs_fence_gate_20261029_results/{acceptance,actual_compile_inputs}.json`。
Turing最终`FINAL_DYNAMIC_REVIEW_PASS`，无剩余动态blocker。随本提交完成mother集成并关闭旧WIP门禁，
之后才恢复非strict-NC批量；source旧Sv39 FAIL/ABORTED仍单独保留。plan实现已归档至mother `plan/do`。
