# 母任务测试框架与RM缺陷最终解法Review

日期：2026-10-31。对象：mother分支`codex/memblock-csr-prefix-fix-20261025`；生产基线固定为`d96427b89e8e68cce5c2f538cd459640874d6563`（最后实现为50e80d275，其后只有结果文档）。本次只同步记录，不修改代码、cfg、脚本、RTL，不重新仿真。状态：独立只读文档复核`DOCUMENT_REVIEW_PASS`，不能把本review冒充过去的事前审批。

## 1. 术语与抽象功能说明

| 术语 | 本文中的通俗含义 | 对象/例子 |
|---|---|---|
| 测试框架 | 负责准备、发送、观察请求及维护测试状态的工具 | cfg、sequence、monitor、driver、runner |
| RM | 在旁边独立计算“正确结果应该是什么”的参考模型 | PMA/PMP权限、异常位、跨页终止 |
| DUT / RTL | 真正被测的硬件 | 不能靠吞检查错误让它看似通过 |
| UID / epoch | 请求编号及该请求第几次执行的版本 | 同UID被redirect后重发不是同一动态实例 |
| ROB / LQ / SQ | 请求在后端、Load队列、Store队列中的位置 | 位置会回绕/复用，不能只拿数字猜身份 |
| owner | 某一时刻真正占用位置的请求实例 | 包含UID、epoch及完整位置 |
| redirect / reissue | 取消旧路径、重新执行请求 | 旧返回不能替新实例完成 |
| snapshot / provenance | 冻结的上下文、说明该事实来自哪次真实事件的身份凭证 | 已接收TLB返回不能被后来的返回冒名替换 |
| token / generation | 一次TLB握手的编号、模型状态的版本 | canonical表不是某次真实response的证明 |
| FIFO / raw / replay | 有序事实队列、尚未应用的原始事实、按顺序把事实应用到模型 | CSR写入先记录，再更新PMP表 |
| ticket / prefix | 整批准备结果及状态版本凭据、从队首连续可处理的部分 | 全部检查通过才一起提交，不能处理半笔 |
| PBMT / PMA / PMP | 页内存类型、物理属性、物理访问权限 | NC和IO不是同一类异常 |
| SFENCE | 让旧地址翻译失效的真实硬件操作 | 写了SATP不等于旧TLB项已经清除 |
| STA / STD / WB | Store地址、Store数据、真实写回 | 地址完成不能替代数据写回 |
| A.fire / D-error | TileLink请求握手、返回错误 | 请求晚握手时仍要认得旧owner |
| strict / clean legacy | 新旧用例各自的完整成功标志及零错误验收 | 都不能只看进程rc0或最终PASS字样 |
| caught fatal | 软件负例有意触发并捕获的拒绝报告 | 与真实DUT零错误PASS分开计数 |

本文回答“哪里出错、为什么改、最终怎么改”，按缺陷分组而不是重贴大函数。已有逐函数源码、调用关系、伪代码和plan对齐分析由各专项implementation review承担；本文不虚构对21个历史实现重新做了一轮全源码形式化验证。

## 2. Git范围、分类及判定方法

初始任务分支从`153977460cfb46108da0ad186c9b13f561b39f9c`创建，审计至d96427b89；48个Git提交中22个含生产/测试/cfg/脚本变更，净78个路径、120条commit/文件关系、737个文本diff块。按根因归并21项：**测试框架17项、RM4项**。source重构合入不重复算Bug；一个cfg提交含两个不同遗漏分别记录；一个静态矩阵集成并非仿真缺陷，不强行新建Bug章。

判定顺序为Git实际diff/当前源码优先，其次原Buglist及可读plan/review，最后现存结果。旧review只证明其当时版本；后续方案变化必须单列，不能回填虚构审批。版本化文档能从Git恢复，不代表被删除的ignored日志/FSDB也能从Git恢复。

原章在`AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md`及`rm_buglist_2026-W44_20261026.md`；本次逐项在原章补记，不新增同周重复文件。B编号与原始独立审计矩阵一致。下列路径均相对mother根目录；Buglist补记使用绝对路径。

## 3. 测试框架问题（17项）

### B01 配置没开权限模型，普通Load也无法分类

**出错场景**：functional cfg关闭PMA/PMP model，RM看到“不是MMIO、不是普通cacheable、也不是uncache”，报classification incomplete UVM_ERROR。16组130个cfg+seed日志出现该错误，其中126个以它为首错，另4个safe日志先报Store AF再报classification；126不是暂估，是首错统计。
**为什么修改**：不是硬件算错，而是测试没有向参考模型提供必需属性。删掉分类检查会让未知访问继续假比较。
**最终解法**：`77b803ee29`给17个相关preset显式开模型；主存范围开关按场景选，direct/sfence保留ranges0，固定主存场景ranges1，CSR random同时关闭level-weight。safe派生配置单列B02。
**保留边界**：不修改RM分类算法，不全局强制所有cfg的ranges都为同一个值，不把ranges0理解为任意物理地址都可路由。
**验证与原章**：30个cfg dry-run、safe10及五个BasicTest组50通过为原历史记录；附录A版本化全部130个原错误入口。旧修后manifest只有126个分类首错seed，safe原10另有_all10复测，不冒充原130完整manifest。W43「Functional cfg 未打开 PMA/PMP model 导致访问分类不完整」。

### B02 safe_nocross16只加了边界约束，却漏了基础配置

**出错场景**：派生cfg限制不跨16字节，但不会自动继承父cfg；缺主存范围和权限模型，原10 seed中6个首错是分类错误、4个首错是Store异常不匹配；后4个随后也报分类错误，不是互不相交的两类日志。
**为什么修改**：名字像“派生”不等于配置系统真的继承，必须把前置参数写完整。
**最终解法**：同一提交`77b803ee29`显式补MAIN_MEM_RANGES=1、PMA_PMP_MODEL=1，保持safe约束和PBMT分布。
**保留边界**：不靠删Store异常检查或换seed消错。
**验证与原章**：原10个seed及最终母矩阵对应10/10；W43「safe_nocross16 派生 cfg 遗漏主存范围和 PMA/PMP model」。

### B03 取消请求排了队，却没有真正发送redirect的人

**出错场景**：旧real-smoke把基类装好的redirect responder覆盖成空泛通用sequence。框架产生取消记录，硬件却没收到redirect，因此永远等不到观测锚点，快照积到15超过14。
**为什么修改**：积压只是后果，根因是接线/启动拓扑错误，不是队列上限太小。
**最终解法**：`dadbd03e89`删除派生testcase的一条错误override，继承唯一正确responder；排查用的压缩/搬一条对一次试验没有进入最终实现。
**保留边界**：14上限、精确目标拍和取消数比较均不变；不同时启动两个responder。
**验证与原章**：历史22个有效seed及cancel随机20通过，其他无进展按独立原因分开；W43「Legacy real-smoke 覆盖 redirect responder 导致 cancel record 永久未锚定」。不要与B21的“已有锚点但收尾被阻塞”混为同一Bug。

### B04 表建好了，但默认不开issue，硬件根本没收到请求

**出错场景**：default行建1000条请求但issue开关为0，terminal进度始终不前进。
**为什么修改**：把请求数调小也无效，需要启用真正发送路径，不能伪造完成。
**最终解法**：`7c3d766b84`先给runner精确一行override；`a15c8e8273`后来撤销该例外，改公共default ISSUE=1，直接调用和矩阵统一。**后者才是最终稿**。
**保留边界**：公共默认值变化确实影响默认激励使能，不能沿用旧“不影响所有普通default”的承诺；需要关闭者须显式关。
**验证与原章**：旧5＋10专项只属于runner方案；最终default行母矩阵10/10通过。未找到a15专门事前独立review，不伪造。W43「Functional coverage 将 default cfg 与关闭 issue 的配置组合导致无进展」，plan和旧implementation review已同步历史边界。

### B05 STD写回认人时，把已经完成的旧请求也算了进去

**出错场景**：V2 STD只带ROB value，框架找到两个候选，其中一个早已完成，却仍报身份歧义。
**为什么修改**：应从仍可能完成这次STD的实例中找人，不能把过去完成者一直留在候选池，也不能盲选一个。
**最终解法**：`e7d6ad78f5`在候选探测时过滤已有`std_writeback/std_pass/std_fault`的旧owner，保持无候选和真双候选fatal。
**保留边界**：不改变redirect旧事件保护、reissue清理，不把STA fault当STD真实写回。
**验证与原章**：原2＋随机10，以及fault-barrier helper专项；adapter动态raw未命中的边界如实保留。W43「STD value-only ROB 双候选把已完成旧 owner 重新计入候选」。

### B07 请求很晚才握手，框架却到那时才找已被取消的owner

**出错场景**：redirect已删活动ROB映射，旧Uncache slot合法继续排空；A.fire到来时再查active map必然找不到人。
**为什么修改**：活动映射删除本身正确，不能为晚握手把旧UID伪装成仍活动。应在身份尚明确时留下一份不可变凭证。
**最终解法**：`9d5d8212d8`在16个slot首次有效时冻结owner，source绑定后arm保存精确快照/mask；真实A.fire用它建返回记录，仅消费相应slot一次，req_valid归零才结束该占用。
**保留边界**：reset、撤销、source复用、global stop分别管理；连续有效slot不能悄悄换ROB。这里允许旧请求drain，不等于B21或strict-NC旧写回可以放行。
**验证与原章**：原133409275、940141520＋10随机，最终源码12/12；W43「SBuffer Uncache A.fire 过晚回查 active ROB owner」。

### B08 随机物理地址落进根本没有设备响应的洞

**出错场景**：sparse PPN算出PA0x311368c0，看似在APLIC大范围内，实际不落在任何真实manager，请求被永远背压。
**为什么修改**：不能向没有接收者的地址发请求，然后怪硬件不返回；也不能加万能TB responder掩盖错误地址。
**最终解法**：`17e2f2d67`保留确定性hash，把sparse输出映射到可路由的512GiB DRAM窗口，再执行页级别/NAPOT对齐。
**保留边界**：MAIN=1路径不变；MMIO覆盖由明确的定向场景承担，不靠随机落洞。
**验证与原章**：历史4 seed＋隔离20随机及软件定向/编译通过；原隔离目录删除后不伪装日志尚在。后续母矩阵10/10只有cfg相同：原`basicTest / memblock_dispatch_real_smoke_vseq`，矩阵为`tc_dispatch_real_smoke / virtual_base_sequence`，不算原tc/ts精确重放。W43「sparse PPN 命中 APLIC 无 manager 空洞导致 Uncache 永久阻塞」。

### B09 仿真已退出，但返回码晚到，调度器误以为进程失联

**出错场景**：远端worker结束与共享目录看到RC不是原子事件；probe看到退出/身份变化，却还没看到matching RC，直接quarantine。
**为什么修改**：要区分“已证实结束但RC未到”和“身份真的不明”，否则正常仿真被当基础设施失败。
**最终解法**：source f5eb对应mother`045011bf6b`，有限窗口从共享路径/有超时SSH恢复RC，严格核对job/token/PGID及完整schema；probe和TERM/KILL共用收口规则。
**保留边界**：真身份不明仍隔离；缺RC不能凭猜测PASS，明确EXITED但无RC记INFRA。进程rc0也不覆盖UVM fatal。
**验证与原章**：fake边界矩阵及真实jobs4 r3四seed通过；W43「并发任务退出时 RC 可见性竞态误触发 mode quarantine」。

### B11 软件定向想测4KB页，却让页大小随机变化

**出错场景**：PPN reuse smoke假定normal4KB leaf，builder却可能生成superpage；生产过滤器正确拒绝，测试误报。
**为什么修改**：固定定向测试的输入前提，而不是放宽生产reuse资格。
**最终解法**：source c491对应mother`fb9f04abe`，只在该cfg关level-weight，并在sequence入口核对getter已消费配置。
**保留边界**：不改其他cfg的页级别随机化，不改reuse算法。
**验证与原章**：原2＋新增20软件seed、Sv39邻接1；母侧复验22＋1单列，软件22不冒充DUT22。W43「L2TLB PPN reuse smoke 未固定 4KB page shape 导致随机误报」。

### B13 定向用例想要第二次翻译，却先被第一笔Store的fault缓存截住

**出错场景**：B/C都用Store requestor，B的superpage fault被DTLB缓存，C同范围命中旧fault，没有预期第二个L2TLB token。
**为什么修改**：场景本身没构造出想测的两次真实response，不能把checker改成“没有第二次也算过”。
**最终解法**：source43669的生产逻辑重构为mother`5c131a18de`：删A，B用Store、C用Load，共享canonical；强制两次真实token。CSR/phase/context及FIFO先完整预检再事务提交，边界replay与B10共用ticket。
**保留边界**：不迁移source33项caught-fatal软件候选，不改RTL/RM结果；B/C accepted/completed/canceled/outstanding保持2/2/0/0。
**验证与原章**：主场景原4＋权威10＋新30＝44严格通过；NC/IO软件邻接20按clean legacy另计。source Planck于2026-10-26批准的457行review可从43669的Git对象读取（blob ca3da472bf637479a3dbd922e36c1568db027912），包含当时33项负测，但不是mother迁移或重跑33项。mother Boyle旧review明确ticket/原子性，不夸大成逐helper事前批准。W43「PBMT response-fault directed 场景复用了 Store fault cache」。

### B14 redirect保留了合法CSR屏障，但框架把它判成越界

**出错场景**：UID前缀随redirect回退，合法保留的静态控制marker位于前缀之外；旧断言只看位置，不认保留来源，误报ROB_ADMISSION_PREFIX。
**为什么修改**：不能放宽到任何越界owner都合法，也不能删除仍要执行的屏障。
**最终解法**：source e2ed重构为mother`4948d291e`，保存保留来源身份，对redirect/commit完整预检再更新；按marker恰好一次维护计数，失败向上传播。
**保留边界**：不是把prefix随意放大；此集成另漏later-split取消，由独立B17修复，不能拿它混作同一个根因。
**验证与原章**：软件19预期caught fatal单列；四CSR scalar原10k seed真实严格通过。W43「CSR random redirect 后静态控制屏障被 ROB admission prefix invariant 误报」。

### B15 写完新SATP，不代表硬件旧翻译已经失效

**出错场景**：CSR映射切换后过早放行新请求，旧global TLB上下文仍在，产生context mismatch。
**为什么修改**：等待旧UID完成、CSR写完只管发送顺序；真实TLB失效还需要SFENCE及其完成观测。
**最终解法**：source5a4精确合入mother`60dcca7c6`，非虚拟special映射切换在target稳定后发真实HS全SFENCE，等待同owner/reset的C0/C4再放行。
**保留边界**：不采用source A1随意过滤旧TLB的候选，不引入未闭环residency机制。
**验证与原章**：source12、母14真实严格通过；软件协议和运输负例另计。W44「CSR 非虚拟映射切换缺少真实全 SFENCE 完成门禁」。

### B16 独立CSR cfg不会继承default，漏掉页级别开关

**出错场景**：四个CSR scalar入口0ns报LEVEL_WEIGHT需要MAIN=0，因为独立plus-file开MAIN=1却忘关level-weight。
**为什么修改**：这批测试确实需要固定主存范围，不能为过检查把MAIN关掉。
**最终解法**：source a0f对应mother`9a2df3d7e`，四cfg显式LEVEL_WEIGHT=0，保留MAIN/PMA=1，dry-run断言三个值。
**保留边界**：不改请求数、CSR随机分布；随后UID7000 prefix错误是B14而不是cfg复发。
**验证与原章**：四原10k seed在合入cfg及prefix修复后母侧通过；W44「CSR scalar 独立 cfg 遗漏 L2TLB level-weight 关闭项」。

### B17 主redirect入口漏调旧split记录取消函数

**出错场景**：外层已取消旧实例，later-split完成记录却没同步失效，旧完成仍能碰到新状态。
**为什么修改**：公共预检/提交重构不能漏掉原先一项生命周期副作用。
**最终解法**：`c9763bd7d`在mutation helper中恢复按UID＋旧epoch取消later-split，保留先完整预检后更新。
**保留边界**：不无条件丢掉所有completion，不放松新实例来源匹配。
**验证与原章**：修前定向201ns稳定失败；修后软件及39次真实DUT严格通过；W44「Redirect 提交 helper 集成遗漏 later-split 旧记录取消」。

### B18 PBMT只需两笔定向请求，runner却一律覆盖为1000

**出错场景**：两UID合同被统一MAIN1000覆盖，201ns发送前即拒绝。
**为什么修改**：cfg的2正确，错误是批量脚本忽略定向场景特殊计数。
**最终解法**：`3bd0e9934`给该行fixed_cfg路由，不追加MAIN；普通随机行仍1000，CSR scalar保留独立计数。
**保留边界**：不删合同检查，不把两笔通过说成原1000参数通过。
**验证与原章**：shell红绿测试、旧1000波形负例、修后原seed及恢复10/10；W44「功能矩阵统一请求数覆盖破坏PBMT两UID定向合同」。

### B20 不同拍的两个合法rollback winner被当作同一事实冲突

**出错场景**：sample144892的winner4与144894的winner5都合法，resolver因ROB/level一样把它们混成一条事件。
**为什么修改**：身份必须含采样拍；跨拍变化不等于同拍自相矛盾。
**最终解法**：`bba3008ba`只接受精确sample的来源证明，继续比较完整ROB/level/payload，缺证明或同拍真冲突仍拒绝。
**保留边界**：不延长模糊窗口、不挑一个winner凑数、不修改DUT仲裁。
**验证与原章**：原1705742557、1992896882各10k及18邻接＝20真实通过，软件另计；W44「Rollback来源缓存将不同拍的合法winner变化误报冲突」。

### B21 一个无关旧Store没结束，拖住了新的取消记录收尾

**出错场景**：record1658已锚定、扫描和取消数都齐，但旧UID1679等STD写回使全局deferred FIFO不空；框架因此不给当前记录结账，目标快照积到15。
**为什么修改**：应该等“会影响本记录取消数的事件”，不是等所有无关事件。
**最终解法**：`50e80d275`由唯一LSQ handler从真实head只读顺推FIFO覆盖位置；相关或身份不明继续阻塞，已证明无关live owner不再阻止冻结。真实deq仍等STD真实WB并按原完整raw原子提交。
**保留边界**：14不调大、不丢snapshot、不拿DUT数回填软件数、不提前释放旧Store。
**验证与原章**：四软件组合＋18只读负例，三个原10k根、8邻接及基础入口，随后统一350项通过；W44「Redirect取消收尾被无关延迟SQ出队阻塞导致快照积压」。

## 4. RM问题（4项，与测试框架分开）

### B06 RM把NC与IO压成一个布尔值，异常原因算错

**出错场景**：seed212889620的UID113 DUT为Load AF0x20，RM误报misaligned0x10；另一seed399662848证明NC Load可同时有0x30。
**为什么修改**：两种内存类型的异常逻辑不同；最终token已在issue前完成，后来的token不能倒过来替换它，根因不是先前猜测的旧snapshot复用。
**最终解法**：`75675a7fd0`保留首字节精确PBMT，区分translation AF和翻译成功后的PMA/PMP AF；按实际Load/Store路径分别组合bit4/5/6/7，后续split保留其位置/来源语义。
**保留边界**：只改expected算法及相关软件矩阵，不改DUT激励/token生命周期，不把所有AF都当作抑制misaligned。
**验证与原章**：软件四象限和优先级、两个历史＋10随机真实seed12通过，UID113/625逐值核对；W43「RM 折叠 PBMT-NC/IO 导致非对齐 Load cause 错误」。

### B10 CSR写入被记下了，但权限模型没有按时更新

**出错场景**：CSR monitor把PMPADDR0、PMPCFG0放进raw FIFO，后面没有统一消费服务，模型一直停在旧generation，结束时FIFO仍有2条。
**为什么修改**：监测到写入与应用到模型是两件事；不能靠结束时清队列或等下一次Load碰巧补更新。
**最终解法**：`91d403eb12`建立持续C+2成熟连续前缀服务；`5c131a18de`后来让issue/request边界也复用同一ticket事务。先离线prepare，再校验epoch/revision/前缀，最后把model和FIFO一起提交一次。
**保留边界**：多个入口不是多个破坏性消费者；都必须进入同一commit。同值accepted写也建generation；锁定/不支持写仍消费计数但不改表；终局仅审计。
**验证与原章**：91d隔离FIFO专项及Kant review、5c母PBMT集成和最终350证据分时点记录；旧r13日志删除不伪装已恢复。W43「PMA/PMP raw CSR FIFO consumer 断链」。

### B12 后一段跨页返回同时有uncache和fault，RM选错优先级

**出错场景**：首页正常，后续split实际response同时uncache与PF；DUT MisalignBuffer输出misaligned0x10，RM先聚合整笔PF得到0x2000。
**为什么修改**：不能只看canonical表猜硬件用了哪次response，必须知道真实后续split的来源及顺序。
**最终解法**：source52f2的生产逻辑重构为mother`a13d10814`，把UID/epoch/ROB/VA、attempt、真实request token及完成观测串起来；按实际later-split response解释优先级，hit/replay/canceled路径各守身份合同。
**保留边界**：不用新token替旧冻结实例；同epoch非法owner替换仍fatal，只有可证明旧epoch/stale才丢弃。source软件候选未整包迁移。
**验证与原章**：母原1＋known/random35＋closure10＋邻接、reviewfix5及postcommit原seed；另找到2026-10-29旧CSR候选的UID462同时间后续FAIL，具体路径/参数见W43补记，SHA256=`d47247621e038eb49fb334d87b9375a0707260d1233f3bd2efd7fda5fc9fe0b9`，不是原PPN失败日志恢复，更不是当前mother修后失败。W43「RM 未保留后续 split uncache 与翻译 fault 并发优先级」。

### B19 首页数据已报错终止，RM还强求未执行的第二页翻译

**出错场景**：seed1992896882 UID2401首页corrupt使DUT合法取消后半段；RM坚持先收齐两页TLB，报第二页query miss。
**为什么修改**：访问已经被首段独立错误终止，不存在的后续翻译不能成为验收前置；也不能只相信DUT异常位。
**最终解法**：`1343dbce3`用首段请求身份、实际S2取数、独立返回错误及连续消费凭证证明提前终止，在跨页查询前构造预期异常；未确认凭证可有界等待，不补造第二页。
**保留边界**：RM只读DUT事实作来源，不用最终WB异常自证正确；UID/epoch/VA和时序不匹配仍拒绝，不任意加IFERROR式兜底。
**验证与原章**：原完整10k及21次真实DUT、四类软件专项分别通过，Turing最终review；W44「跨页Load首段数据错误终止后RM仍强求未执行第二页翻译」。

## 5. 过期方案、证据及review如何补齐

1. B04的公共默认值和B10的最终调用链已按Git同步原Buglist/plan/implementation review；原方案保留为显式历史，不倒签审查。
2. 15项原章补最终提交、完整入口/manifest、当前源码/稳定函数、可读review与后续母侧验证。原周文件/原章不重复创建；同一问题跨source/母分支不重复计数。
3. 已删除工作区的文档优先用当前mother同相对路径或Git对象定位；ignored失败日志/FSDB若找不到同hash备份，明确写“原路径已失效、在本次已检查范围未找到可验证备份”，不声称全盘绝对不存在。旧路径与原现象保留，不能把修后PASS日志冒充原FAIL。
4. B04 a15缺少专门事前独立review等历史审批缺口只能如实说明；本次独立文档核对不是2026-10-25批准，不靠补文字抹去流程事实。
5. 最终350＝310真实DUT＋40软件；strict-NC10另有RTL Bug，2026-10-31新复现仍同787.300ns失败。本文21项均为已落地TB/RM修复，不把重复RTL复现或未采用WIP算新增修复。

## 6. 验证口径与当前结果

当前生产2540项输入没有变化。母矩阵25组250/250及fault3压力100/100均已按相同源码验收，独立回归和覆盖率review PASS。实际结果目录为`mem_ut/ver/ut/memblock/sim/mother_post_cancel_matrix_20261030_results`、`mother_post_cancel_fault3_final99_20261030_results`及`mother_final_cancel_nonrtl_pass_coverage_20261030`。
DUT line80.88%、condition60.54%、toggle38.56%、FSM9.24%；fcov/branch关闭，不声称功能覆盖率收敛。母任务360名额只350通过，整体仍STOPPED_RTL_CONFIRMED，不因本次文档补齐改变。
本次只做文档、Git及已存结果核对，没有新仿真或代码修复；每Bug原专项提交保留，本次结果为单独文档提交，不冒充新的单Bug源码提交。

## 7. Plan对齐检查

21项的plan/review映射由原Buglist补记及本review各节提供。新增文档不是新的执行方案，不调整生产实现；B04、B10原方案已被后续提交替代的部分按Git显式校正。其它算法/时序细节以既有专项review为准，没有凭缺失证据新增生产决策。

### 实现与Plan不一致项

B04 a15改变原“公共default不变”约束；B10 5c恢复共用request-bound replay。两者均已更新方案并保留原版本边界。旧review不能自动覆盖后续提交；独立事前review没有可靠记录的部分明确保留历史缺口。

### Plan未说明但Coding落实的细节

本次没有coding；21项原implementation delta继续保留原专项文档。软件预期拒绝不等于真实DUT零错误验收、固定cfg不继承default、原seed与随机替换身份均按实际记录，不填充不存在的验证。

## 8. 非本次修改的逻辑分析

`git status`中的`AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`、`AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、`AI_DOC/analysis/rtl/v2/index.md`、未跟踪`AI_DOC/analysis/rtl/v2/flows/backend_memory_writeback_flow.md`及`mem_ut/ver/ut/memblock/signal.rc`是既有无关内容，不修改、不暂存、不提交。未移动review到do，未新建worktree，未向公共origin推送。

## 9. 本次独立文档复核结论

2026-10-31由独立只读reviewer Dalton（agent `01a18fd3-1f09-7c42-8298-1cdc32fd6003`）复核10份文档，结论`DOCUMENT_REVIEW_PASS`，无剩余P0/P1/P2阻断项。复核关闭了分类错误130/126口径和severity、B08不同tc/ts、B09后来config与原证据区别、FIFO各入口sample来源四项文档问题；并核对Git对象、原130项入口/日志、B12后续FAIL、B13源review、框架/RM分类及软件/真实验证边界。

审查证据在`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_buglist_audit_20261031_results/review.json`和同目录`review.md`，记录文件SHA256；本文把结论纳入Git，ignored审计文件作逐项底稿。该PASS只覆盖当前文档一致性，不追认历史审批，不宣称丢失FAIL/FSDB全部恢复，不等于21项重新仿真，也不改变母任务strict-NC未通过的状态。

## 附录A 原classification错误精确入口（130项，不与B02重复计数）

下表来自2026-10-23原始config、results及错误日志逐文件核对，不是从计划seed推断。每行tc/ts/cfg全部列明；计数按cfg+seed唯一组合。原批次绝对根为`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/functional_cfg_cov_20261023_results`；每行`<cfg>/config.txt`、`results.tsv`和`status/seed_<seed>.final.log`可定位原参数及错误。共同临时配置MAIN1000/CHECK0、timeout1000000000ns、wave0、coverage1。

| tc | ts | cfg | 日志数 | 精确seed |
|---|---|---|---:|---|
| basicTest | memblock_dispatch_manual_control_vseq | csr_sfence_check_store_manual_control | 8 | 179754109,388165524,764979433,1074894061,1079825163,1224359045,1410680843,1762930443 |
| basicTest | memblock_dispatch_real_smoke_vseq | csr_sfence_check_store_rob_control | 10 | 4159930,10282055,435509395,514957749,804608929,1319328500,1476243655,1495802208,1840791619,2040657161 |
| tc_dispatch_real_smoke | virtual_base_sequence | default | 9 | 244294137,315465133,886742139,1396766687,1517022454,1528113408,1529000528,1965982108,1990296314 |
| basicTest | memblock_dispatch_real_cancel_reconcile_vseq | tc_dispatch_real_cancel_reconcile_smoke | 8 | 106952776,515565245,596462660,1002782503,1169322224,1490389807,1552605751,1857718311 |
| tc_dispatch_real_smoke | virtual_base_sequence | tc_dispatch_real_l2cache_model | 7 | 535869290,662257882,964553189,1025735027,1278178074,1291429752,1722934240 |
| tc_dispatch_real_mixed_sta_wb_smoke | virtual_base_sequence | tc_dispatch_real_mixed_sta_wb_smoke | 8 | 169414256,175442179,446254069,558245176,649914973,869418367,1423643028,2000022555 |
| tc_dispatch_real_mixed_wb_smoke | virtual_base_sequence | tc_dispatch_real_mixed_wb_smoke | 8 | 196096297,226346728,326301361,438144536,600422823,1352876455,1937215810,2131527821 |
| basicTest | memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq | tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16 | 10 | 253272942,408596162,705501646,715870991,1008225557,1430760738,1622153449,1910606046,2035231027,2093482640 |
| tc_dispatch_real_multi_store_wb_smoke | virtual_base_sequence | tc_dispatch_real_multi_store_wb_smoke | 3 | 774863542,1857178350,2100124559 |
| tc_dispatch_real_smoke | virtual_base_sequence | tc_dispatch_real_smoke | 10 | 136173539,406211268,893219364,927375894,1229434535,1554476699,1669254137,1873127194,1894600671,2026843552 |
| tc_dispatch_real_smoke | virtual_base_sequence | tc_dispatch_real_smoke_flushsb | 10 | 141120540,183944423,318782036,761623813,1115667397,1555691540,1578156644,1864170249,1868548195,1979871148 |
| tc_dispatch_real_store_smoke | virtual_base_sequence | tc_dispatch_real_store_smoke | 6 | 56252017,356792914,1379009016,1469407758,1470861613,1564071098 |
| tc_dispatch_real_store_sta_wb_smoke | virtual_base_sequence | tc_dispatch_real_store_sta_wb_smoke | 8 | 51550417,812093546,1196624107,1364716334,1532278001,1740758634,1928673490,1968767477 |
| tc_dispatch_real_store_wb_smoke | virtual_base_sequence | tc_dispatch_real_store_wb_smoke | 5 | 156143421,726638361,905010437,1405475176,1939750109 |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_sfence_stress_10k | 10 | 232688989,587924460,714676886,860020905,883426973,1011288446,1277959682,1768399716,1873433099,2053277337 |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_sfence_stress_1k | 10 | 910628785,916943347,1251775199,1400658178,1445792777,1573950218,1724859600,1924667053,2038148368,2138667069 |

合计16组130个日志都出现classification UVM_ERROR；其中126个为首错。safe组4个seed `408596162,1008225557,1430760738,1910606046` 先报Store AF、后报classification；safe其余6个为分类首错。此10项与B02是同一批证据，不另加10个失败。17个B01修复preset是Git修改范围，与16个原日志组不是一个集合。

历史修后manifest是`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/functional_cfg_pma_fix_retest_20261023_results/retest_manifest.tsv`，16行126个seed，SHA256=`7cfe84f138eb1c488379f7eecf037decdc28c7b8bc805632fd0d00c0e8c39aba`；它对应126个分类首错，不是130项原错误名单。safe原10项修后结果另在同结果根`tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16_all10`，避免把两份证据合计时重复计数。

逐日志行号、首错及后续分类原文另在ignored审计结果`mem_ut/ver/ut/memblock/sim/mother_buglist_audit_20261031_results/evidence_followup.json`保留；本表已将必要入口纳入Git，不依赖该ignored JSON才能找到精确seed。
