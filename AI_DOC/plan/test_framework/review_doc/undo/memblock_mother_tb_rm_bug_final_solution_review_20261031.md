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

## 10. 相对主分支变更审计（2026-11-02，限定生产框架、RM及仿真脚本）

### 10.1 比较基点、审计口径与结论

本节按用户最后缩小的范围，仅核对已提交的**生产测试框架逻辑、RM、仿真脚本**是否已被本文说明；不审计其他文档变更，不审计或枚举软件测试用例新增修改，不再建立全部77个代码路径的覆盖清单。脚本单测仅作为验证手段简述。本节是变更记录覆盖核对，不是重新证明所有函数正确，也不重新验证历史仿真。

权威比较范围固定如下，不能把原文的生产截止点误读成主分支：

| 项目 | 核实结果 |
|---|---|
| 本地主线 `mem_ut_uvm_v2` | `153977460cfb46108da0ad186c9b13f561b39f9c` |
| 母分支 HEAD | `e275e8819b11b2329f01f9788c47655e8711d107` |
| `git merge-base` | 等于上述主线；主线是母HEAD祖先 |
| `git rev-list --left-right --count base...head` | `0 49`，母分支独有49个提交，其中22个含代码变更 |
| 原审计截止点与当前HEAD | `d96427b89e8e68cce5c2f538cd459640874d6563..e275e8819b11b2329f01f9788c47655e8711d107`只有一条文档提交；未追加生产实现 |
| RTL及构建边界 | `src/`、`build/`、根`Makefile`、`build.sc`、`.gitmodules`无已提交diff；本范围没有RTL实现修改。TB内部信号观察连接变化仍属于测试框架，见B12/B19 |

**结论分开计数**：在本节核对的三类变更中，未发现B01–B21之外需要新增编号的独立生产修复根因；确认**1组支撑设计描述遗漏**，即`a625c5d9e9`的仿真配置目录入口与功能矩阵集成，补为A01。原文一句“静态矩阵集成并非仿真缺陷”说明了不计Bug的理由，却没有说明这组设计实际做了什么，不能据此宣称全部设计已说明。下列对已有B项的补充定位不重复计为新Bug或新遗漏。

本节没有取得新的全量独立审查PASS，不声称全部77路径、软件用例或分支所有文档均已审计；原全量子审计已按用户缩小范围要求停止。历史独立review与仿真结论仍只具有原文所述的版本、场景和证据边界。

### 10.2 测试框架逻辑修改：已有根因与关键设计落点

以下源码路径前缀统一为`mem_ut/ver/ut/memblock/`。表中函数说明是抽象职责：说明谁在什么阶段用它、输入来自哪里、对外改变什么，不展开长源码或软件测试实现。

| 已有记录 | 已提交变更 | 生产落点与通俗职责 |
|---|---|---|
| B01、B02、B04、B16 | `77b803ee29`、`7c3d766b84`、`a15c8e8273`、`9a2df3d7e2` | 运行cfg提供权限模型、主存范围、issue使能和页级别随机化的合法组合。`seq/plus_cfg/default.cfg`的公共ISSUE=1是最终行为；B04早期runner单行覆盖已被撤销。四个CSR scalar独立cfg不会继承default，不能把脚本选对目录等同于配置已合法 |
| B03、B05 | `dadbd03e89`、`e7d6ad78f5` | `tc/src/tc_dispatch_real_smoke.sv`继承正确redirect responder；`seq/base_seq_help/dispatch_monitor_event_adapter.sv`在STD身份解析时排除已经完成的候选。前者保证取消真正发给DUT，后者保证真实写回只归属于仍可能完成的实例 |
| B07 | `9d5d8212d8` | `seq/base_seq_help/mem_base_sequence.sv`的`service_uncache_slot_owner_cache()`在slot首次有效且身份尚可解析时保留owner，source绑定后由`capture_armed_uncache_load_owners()`冻结本次A请求的owner快照，`create_uncache_response_record()`在真实A.fire使用该身份建立返回记录。活动ROB映射删除与旧slot继续排空是两种生命周期，不能到晚握手时重新猜owner |
| B08 | `17e2f2d676` | `seq/base_seq_help/tlb_map_builder.sv::make_canonical_ppn()`为新翻译生成可路由物理页，再按页形状对齐；避免把请求送进无manager地址洞。该修复约束sparse地址生成，不能解释为已扩大MMIO随机覆盖 |
| B12、B17、B19的框架支撑 | `a13d108144`、`c9763bd7d4`、`1343dbce3b` | L2TLB agent/interface/connect与`seq/base_seq/memblock_l2tlb_base_sequence.sv`传递真实split观察。`common_data_transaction.sv`的`bind_later_split_l2_request_token()`、`complete_later_split_l2_response()`把后段归属与真实返回相连；`cancel_later_split_tlb_record_for_uid()`在旧实例取消时同步失效。首段的`observe_first_split_request_sample()`、`observe_first_split_data_sample()`、`observe_first_split_response_sample()`分别保留请求、实际S2取数及返回事实，供RM只读使用 |
| B13 | `5c131a18de` | `seq/base_seq/memblock_main_dispatch_pbmt_response_fault_sequence.sv`及对应PBMT虚拟sequence构造B Store/C Load两次真实翻译。`common_data_transaction.sv::resolve_and_plan_pbmt_rf_request_fire()`先形成合法请求计划，`commit_pbmt_rf_request_fire_prechecked()`发布已预检结果；`common/memblock_common/src/memblock_sync_pkg.sv::pbmt_rf_issue_gate()`按场景阶段开放对应UID。目的是让两笔请求确实处于要求的CSR/PMA上下文，避免用一次Store fault缓存命中冒充第二次response |
| B14 | `4948d291e9` | `seq/base_seq_help/common_data_transaction.sv`的`precheck_uid_redirect_mutation()`和相关提交helper把redirect处理分为先检查、后更新；`preserve_static_control_marker_on_redirect()`和控制生命周期来源记录解释“为什么这个前缀外marker仍合法”。`memblock_control_barrier_service.sv`只对通过检查的控制动作推进状态。这样既保留合法CSR屏障，又避免某项检查失败时状态已改了一半 |
| B15 | `60dcca7c67` | `seq/base_seq/memblock_dynamic_csr_change_sequence.sv::complete_hs_mapping_fence()`在映射切换后驱动并等待真实HS SFENCE；`common_data_transaction.sv::arm_csr_mapping_fence()`、`finish_csr_mapping_fence()`把完成观察与当前控制owner/reset绑定。CSR写入生效和旧翻译失效必须分别完成，才能开放后续请求 |
| B20 | `bba3008baa` | `common/memblock_common/src/memblock_sync_pkg.sv`的rollback来源解析使用精确采样拍区分合法winner。它改变的是事实归属检查，没有改变DUT仲裁，也不能用更宽时间窗消除真冲突 |
| B21 | `50e80d275e` | `seq/base_seq_help/lsq_commit_handler.sv::deferred_ctrl_blocks_deleted_owner_window()`从真实LQ/SQ队首只读顺推待处理出队事实，回答它们是否可能影响当前取消记录。`common_data_transaction.sv::service_redirect_deleted_owner_window()`使用此判断推进收尾。检查只读，不消费出队事实、不提前释放Store；相关或无法证明身份的事件仍阻塞 |

**已有设计补充说明，不新增根因**：B14的大量公共helper不是另外一批独立Bug，而是把屏障来源、计数、队列副作用纳入同一预检/提交规则；B17是后续补回其中遗漏的later-split取消。B12/B19的被动观察接口也不是新DUT功能，而是使RM有足够独立事实判断最终结果。上述设计已被原B项明确覆盖，本节补足生产函数定位和职责。

### 10.3 RM修改：异常算法与模型状态服务

| 已有记录 | 已提交变更 | 生产落点、修改目的与最终行为 |
|---|---|---|
| B06 | `75675a7fd0` | `env/src/memblock_rm/memblock_rm.sv::observer_build_commit_item_with_observation()`保留精确PBMT并结合翻译及物理权限结果构造预期异常，区分NC/IO与Load/Store路径；不能再用一个uncache布尔值代替不同异常原因 |
| B10，及B13共同集成 | `91d403eb12`、`5c131a18de` | `seq/base_seq_help/common_data_transaction.sv::service_csr_pmp_pma_write_observations()`持续处理已成熟CSR事实；`prepare_pma_pmp_csr_replay()`准备一个连续前缀，`commit_pma_pmp_csr_replay()`统一提交模型与FIFO；`apply_pma_pmp_csr_writes_before_request()`让请求边界也复用这条服务。`memblock_pma_pmp_model.sv::prepare_csr_replay_batch()`准备模型结果，`commit_csr_replay_batch()`发布结果，避免权限模型与原始写入队列各走各的 |
| B12 | `a13d108144` | `env/src/memblock_rm/memblock_rm.sv::observer_build_commit_item_with_observation()`通过`seq/base_seq_help/memblock_rm_readonly_api.sv::try_read_completed_later_split_response_for_rm()`读取真实后段完成来源，判断后续uncache与翻译fault的优先级。翻译表当前内容不能代替实际执行过的response |
| B19 | `1343dbce3b` | `env/src/memblock_rm/memblock_rm.sv::observer_try_apply_first_split_data_error()`在要求第二页翻译前，通过`memblock_rm_readonly_api.sv::try_read_first_split_cache_error_for_rm()`判断首段是否已有独立数据错误终止证据。先确认请求身份、实际取数、错误历史及连续观察边界，再生成预期异常；不能直接拿最终WB异常反证RM正确 |

**B10/B13的事务式设计补充**：旧式“先取走CSR事件，再逐条更新模型”可能在后续检查失败时留下半笔更新，也可能使不同入口看到不同权限版本。最终实现让准备票据保存reset、队列和服务版本以及所选前缀，提交前统一核验；核验成功后连续更新模型和FIFO，票据只消费一次。持续服务与请求边界都是同一提交路径的调用者，不是各自破坏性消费FIFO。高频准备只看待处理的成熟前缀和首个未来事件，不扫描全部UID或全部历史。这是原B10明确提及的最终方案细化，不另计遗漏或新Bug。

**证据边界**：本节依据上述已提交diff、当前生产函数和原B06/B10/B12/B19的既有验证记录解释最终设计。没有重跑原seed，没有核验全部历史日志或恢复已删除证据；原软件验证数量不在本节重新审计，也不作为新的DUT通过数。

### 10.4 仿真脚本修改：已有修复与A01设计补充

| 归属 | 提交与生产脚本 | 核对结论 |
|---|---|---|
| B09 | `045011bf6b`；`scripts/run_eda_regression.sh` | `validate_matching_rc_file()`检查当前job身份的完整RC，`consume_matching_rc_file()`只消费匹配结果，有限时SSH恢复路径处理进程退出与RC可见性的竞态。原B09已有明确记录；伪造远端响应的脚本单测只是边界验证，不能等同真实EDA回归 |
| B04 | `7c3d766b84`、`a15c8e8273`；`scripts/run_function_cfg_cov_20261022.sh` | runner的default单行ISSUE覆盖属于已被撤销的中间方案；最终公共default启用issue，不把这两次修改当作两个净有效功能 |
| B18 | `3bd0e99342`；同矩阵脚本`resolve_case_profile()` | 后增`fixed_cfg`分支让PBMT定向保留两笔请求；普通行继续公共事务数策略。这是对A01矩阵路由的后续修复，原B18已覆盖 |
| B10的静态检查支撑 | `91d403eb12`；`scripts/checks/check_pma_pmp_csr_fifo.py` | 新增生产调用白名单及C+2/history/终局审计的源码检查，目的在于防止重新出现多个独立FIFO消费者或终局补更新；它不运行DUT，不产生真实仿真PASS。该脚本仍保留91d隔离方案的旧断言，与5c后的当前实现有不一致，见下文保留项 |
| B12的固定回归输入 | `a13d108144`；`scripts/closure_seed_lists/tc_dispatch_real_mmu_sv39_smoke.seeds` | 新增版本化十seed清单，固定原Sv39闭环输入并纳入历史失败seed212889620，仍由既有`--seeds`入口启动；没有新增runner seed-file解析器。该文件只确定回归输入，清单存在不代表已执行或已通过 |
| A01 | `a625c5d9e9`；上述两个生产脚本 | 原文未具体说明专用cfg目录入口、参数校验、冻结审计链和矩阵profile路由；按下文补齐，保持非Bug设计项身份 |

#### A01 专用配置目录与功能矩阵集成（支撑设计描述遗漏1组）

**场景与原因**：四个CSR scalar cfg实际放在`seq/csr_scalar_stress/cfg`，旧runner只从`seq/plus_cfg`找配置；旧矩阵又统一覆盖事务数，即使找到专用cfg，也会把`10010/10012/10016/10025`改成普通行的1000。需要让批量任务选择正确配置目录，并保留不同场景各自的请求数合同。该项是在集成和静态检查中处理的设计问题，不补登记为一次新的仿真Bug。

**修改前后必须区分**：主线已经有30条case、配置副本冻结和第一行编译后复用的流程；`a625c5d9e9`没有新建30个用例，也没有首次实现“一次编译”。它新增目录选择、校验和四字段profile，把既有冻结与编译复用用于这些目录和不同计数策略。后续B16修复cfg本身的合法组合，B18补PBMT固定计数，两者不由本次集成自动解决。

| 生产落点 | 抽象功能与最终方案 | 例子及边界 |
|---|---|---|
| `scripts/run_eda_regression.sh::normalize_plus_file_dir()` | 参数解析接收`--plus-file <dir>`，把相对目录按调用者当前目录解析为物理绝对目录，要求可读、可遍历，并拒绝原始及规范化路径中的控制字符 | 接受专用cfg目录；该参数是目录入口，不是任意cfg文件名 |
| 同脚本`resolve_cfg_file()` | 使用受限cfg名称和固定`.cfg`后缀定位文件，再检查真实目标父目录等于所选目录 | 目录内symlink可指向目录内文件，不能指向目录外；缺失或不可读文件拒绝 |
| 同脚本冻结流程、`validate_cfg_plusargs()`、`write_config()` | 复制所选cfg到既有`frozen_plus_cfg/`，以只读副本供后续参数合并及执行使用；在进入Makefile参数解析前检查逐行`+KEY=VALUE`语法、重复key和非法字符。记录`source_plus_file/source_cfg/frozen_cfg/cfg_sha256`以对应源选择和实际执行副本 | 源文件之后变化不能被当作本批次配置；合法cfg键仍保留原有覆盖语义。目录入口和内容校验不会自动证明运行期sequence配置合法 |
| `scripts/run_function_cfg_cov_20261022.sh::resolve_case_profile()`与主循环 | 原三字段case扩为`cfg/tc/ts/profile`；按profile选择目录和是否注入公共事务数，继续共用mode并让首行`--compile`、后续`--no-compile` | `a625`时普通26行、scalar4行；最终HEAD经B18调整为普通25行、PBMT fixed1行、scalar4行。scalar四行保留各自精确数量，PBMT保留2；各行仍注入CHECK_TRIGGER=0 |
| 同矩阵脚本`valid_case_name()`与运行入口 | 在创建case结果目录前校验case字段，拒绝空字段、多余字段、非法名称以及`.`/`..`；未知profile拒绝。通过`FUNCTION_CFG_RUNNER`、`FUNCTION_CFG_RESULT_ROOT`、`FUNCTION_CFG_CASES_FILE`入口支持替身驱动和小矩阵验证，`pipefail`使管道错误可见 | 测试入口服务于脚本验证，不是新的DUT激励功能；编译复用仍受既有runner的mode及fingerprint规则约束 |

**调用关系及正确性检查**：runner先解析选项，再规范化目录、解析cfg真实位置、冻结副本、验证副本内容，之后才合并实际参数并记录来源。矩阵先验证case并解析profile，再组装数组参数调用runner。这样目录选择、结果位置和请求数策略都有明确入口；保持一次编译的调度方式不代表所有30行都已通过仿真。

**验证证据及边界**：已提交的A01专项implementation review为`AI_DOC/plan/test_framework/review_doc/undo/memblock_csr_scalar_functional_matrix_integration_fix_implementation_review_20261025.md`，第2、12、13节分别记录路由检查、修正后脚本门禁及真实运行发现的B16/B14边界。该历史记录包含30项路由、专用目录与非法输入的脚本单测及dry-run；本次只引用既有记录，不重跑、不把测试资产逐文件当作设计清单，也不把dry-run写成真实DUT通过。四CSR原seed的后续联合通过边界继续引用B14/B16，PBMT矩阵恢复继续引用B18。

**主审保留项：B10静态检查脚本仍约束旧方案。** 当前`check_pma_pmp_csr_fifo.py`明确断言生产源码中不得出现`apply_pma_pmp_csr_writes_before_request`，但母HEAD已由`5c131a18de`恢复该共用ticket wrapper，且该函数确实存在。其对已排除软件测试文件的旧要求不在本次审计范围内；仅这一处生产API断言已足以说明脚本不能直接作为当前最终方案的有效门禁。这里记录的是静态可确认的脚本与生产合同不同步，不增加仿真Bug数量，不声称本轮执行过该脚本或复现了DUT错误；后续若授权修改该检查脚本，应验证共用prepare/commit路径，而非禁止正确的边界wrapper。本次只补文档，不修改脚本。

### 10.5 最终净行为、未纳入内容与复核命令

本节按最终HEAD解释设计：B04的runner例外已撤销；B10在`91d403eb12`之后由`5c131a18de`统一请求边界服务；B14后由`c9763bd7d4`补later-split取消；A01后由`9a2df3d7e2`及`3bd0e99342`分别补配置前提、固定计数。中间实现不能与最终行为叠加计数，原21个B编号和前面的130项附录均保持原样。

未提交/未跟踪的LSQ门控方案及状态、B05后续分析、暂停的`scripts/tests/test_runtime_parameter_priority.sh`草稿及其他工作区文件不属于本节已提交实现证据，不修改，不视为设计已完成。软件测试用例和其他文档变更按用户最后指令排除，不据此声称没有变化或已完成审计。

关键只读命令如下；差异必须使用固定提交，不能拿工作区草稿代替母分支已提交代码：

```bash
git merge-base 153977460cfb46108da0ad186c9b13f561b39f9c e275e8819b11b2329f01f9788c47655e8711d107
git merge-base --is-ancestor 153977460cfb46108da0ad186c9b13f561b39f9c e275e8819b11b2329f01f9788c47655e8711d107
git rev-list --left-right --count 153977460cfb46108da0ad186c9b13f561b39f9c...e275e8819b11b2329f01f9788c47655e8711d107
git log --reverse --format='%h %s' 153977460cfb46108da0ad186c9b13f561b39f9c..e275e8819b11b2329f01f9788c47655e8711d107
git show a625c5d9e9 -- mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh mem_ut/ver/ut/memblock/scripts/run_function_cfg_cov_20261022.sh
git diff 153977460cfb46108da0ad186c9b13f561b39f9c e275e8819b11b2329f01f9788c47655e8711d107 -- src build Makefile build.sc .gitmodules
```

**Plan对齐检查**：A01对应既有`AI_DOC/plan/test_framework/plan/do/memblock_csr_scalar_functional_matrix_integration_fix_plan_20261025.md`及上述专项review，本次只补覆盖说明，不执行或修改plan。实现与Plan不一致项、Plan未说明但Coding落实的细节沿用专项review第9、10、12节的既有记录，本节不重新授予历史批准，也不宣称重新完成所有Plan逐条核验。本次唯一写入文件为本review，未修改生产源码、cfg、脚本或其他文档，未编译、未仿真、未提交。
