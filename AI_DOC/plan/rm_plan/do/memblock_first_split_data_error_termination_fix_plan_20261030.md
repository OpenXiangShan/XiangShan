# 跨页Load首段数据错误提前终止修复方案

日期：2026-10-30。状态：**已完成；原seed10000笔、21/21真实DUT及四类必要软件专项通过，Turing最终FINAL_IMPLEMENTATION_REVIEW_PASS，迁do并随本记录单Bug提交**。
范围：V2母任务中的RM检查顺序及必要的被动执行时序观测；不修改RTL、请求生成、TLB resolver或无关soft_test。

## 1. 专有名词与抽象功能说明

| 术语 | 当前中文含义 | 代码对象/落点 | 示例 |
|---|---|---|---|
| first split | mabuf对原访问首页发出的实际低段请求，不是初始RS issue | 现有split req/response旁路 | VA8ab4dfff先处理低段，未必发高段 |
| attempt | 同一动态UID的一次物理低段request→S2取数→response | 单份首段上下文及request sample | 低段miss重放产生新attempt，覆盖上一份证据 |
| dynamic epoch | 同UID被redirect后重发的动态版本 | `status.dynamic_epoch` | 旧版本首段响应不得授权新版本提前终止 |
| reset epoch | DUT/transport当前复位代次 | `sampled_reset_epoch`及现有getter | reset前证据即使UID相同也无效 |
| S2 take | LoadUnit在S2以`s2_fire`锁存该次数据的实际采样 | `RegEnable(..., s2_fire)` | 以此sample查询独立错误历史，不用更晚WB sample |
| D-error | 环境真实D.fire生效的line错误事实 | 既有`query_l2_d_error_at_sample()` | RESERVE不是生效，activation之后才可见 |
| context | 每UID一份有界、按值保存的首段执行上下文 | 新增status结构字段 | 不保存逐拍历史，不是residency/多级staging总账 |
| readonly view | 不可变值型证据副本 | RM只读API | 只返回身份、时刻、独立错误，不读DUT异常作为预期 |
| later page | 跨4KB访问的后续页，不等于低段的cache miss重放 | 既有later-split链 | 已真实发出高页时不得使用首段早停捷径 |

抽象职责：被动采样只证明“哪个动态UID在何时执行了哪个低段”，不判断DUT正确性；RM用独立翻译、
权限/PMA与D-error历史推导期望，仍与LDA/status完整比较。正常请求调度、驱动、CSR/SFENCE及取消消费顺序不变。

## 2. 已确认根因与目标

Buglist：`AI_DOC/buglist/rm/v2/rm_buglist_2026-W44_20261026.md`首段数据错误章节。
原入口`basicTest / memblock_dispatch_real_smoke_vseq / tc_l2tlb_fault_2pct_no_trigger / seed1992896882 / 10000笔`。
UID2401低段PA0x803688efff对应line0x803688efc0；独立corrupt在601930.1ns生效，601980.1ns低段HWE返回，
mabuf取消未发送高段；602001.1ns RM却先要求高页翻译，未进入后置D-error期望计算。
原seed带波形同时间/UID复现，Turing独立责任分类`REVIEW_PASS`，不属于RTL或snapshot/current-live复用。

目标：在RM越过首页、查询第二页之前，能独立证明已执行低段的数据错误应结束该次访问；保留真实部分PA，
产生严格的Load AF或Hardware Error预期，不伪造高页、不吞掉真实query miss、不用DUT异常决定期望。

## 3. 实现边界与文件

生产范围限定为：

| 文件 | 修改职责 |
|---|---|
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv` | 现有split诊断旁路增加S2取数有效、ROB flag/value、vaddr、纯cache资格五个被动输入及两个clocking输入清单 |
| `tb/L2tlb_agent_connect.sv` | V2被动采样适配：只连接LU1 S2结构身份/取数事件；inactive分支置0，不驱动RTL |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv` | immutable transport增加对应五个非rand、4态采样字段 |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv` | 在现有mon_cb边界按值复制，沿用原sample/reset/producer水位合同 |
| `seq/base_seq/memblock_l2tlb_base_sequence.sv` | 原sample anchor/lifecycle门禁之后，按request→response→S2顺序调用新被动观测函数；同拍response使用拍前取数 |
| `seq/base_seq_help/status_transaction.sv` | 每UID一份首段上下文结构及清理；不增加map、queue、随机状态或调度标志 |
| `seq/base_seq_help/common_data_transaction.sv` | O(1) ROB查UID、首段attempt关联、后页标记、连续消费水位及redirect清理 |
| `seq/base_seq_help/memblock_rm_readonly_api.sv` | 值型证据与独立cache错误历史查询；不改TLB lookup/resolver |
| `env/src/memblock_rm/memblock_rm.sv` | 首页段完成后、第二页查询之前推导首段数据异常；保持后续严格比较 |

上述路径相对`mem_ut/ver/ut/memblock`。四个S2输入为`lu1_split_data_valid/rob_flag/rob_value/vaddr`：
valid取`RTL_PATH.inner_LoadUnit_1.s2_fire && RTL_PATH.inner_LoadUnit_1.s2_in_r_isFrmMisAlignBuf`；
其余取同模块`s2_in_r_uop_robIdx_flag/value`、按现有接口方式符号扩展的`s2_in_r_vaddr[49:0]`。
必须观察实际拆分vaddr，不能取保留原架构地址的fullva：LW/LD部分case的低段会向下对齐，而fullva仍是原始地址。
这是现有split执行诊断的被动适配，
不连接DCache error数据，不把L2TLB agent改作L2Cache responder，不修改monitor owner或TLB response行为。
不得用固定“response减一拍”代替S2事件：S3存在ready/valid与取数时刻差异，必须实际观察S2 fire。

第五个输入`lu1_split_data_cache_eligible`只表达非forward的纯cache hit资格，不含异常/错误位。
生成RTL已优化掉`io_dcache_resp_valid`和`s2_dcache_should_resp`，因此使用实际存在的
`!io_dcache_resp_bits_miss && !s2_full_fwd && !s2_fwd_frm_d_chan &&
!io_forward_mshr_forward_result_valid && !s2_nc_with_data`，
并要求`s2_fwd_mask_15..0`全部为0。所有SQ/SBuffer/D-channel/MSHR forward均不由本捷径处理。
权限、翻译和cacheability仍由RM独立校验，不采异常位作为资格条件；response必须`need_replay=0`。
不满足资格时保留原路径诊断，不能用sticky错误覆盖纯replay或转发数据。

验证只扩展本Bug所需已有provenance定向helper，或使用现有真实DUT入口的临时参数构造短小跨页案例；
不修复无关soft_test。无新runtime参数、cfg开关、编译宏或worktree。

## 4. 状态与生命周期

在`status_transaction.sv`增加值型`memblock_first_split_access_context_t`和字段`first_split_access`。
结构保存：valid、uid、dynamic_epoch、reset_epoch、rob_key、original_vaddr、request_vaddr、request_sample_seq、
data_sample_seq、data_cache_eligible、response_sample_seq、response_vaddr、response_nc/mmio/need_replay、later_page_seen。
没有DUT exception、预期异常或实际数据字段。

- constructor/reset：清零。
- 首段新request：按当前UID/epoch/reset/ROB建立或替换attempt；清除旧data/response，但继承同实例later标记。
- S2事件：只允许同ROB/VA、当前epoch/reset、当前request之后的事件更新取数sample。
- 首段response：只允许同一attempt和地址，且已有对应S2 sample；保存时序、path/replay信息。
- 高页request：对同UID当前上下文置`later_page_seen=1`；同UID/epoch/reset期间单调保持，低段replay不能清除；不修改既有later-page状态机。
- redirect/reissue：两个已有重置入口`reset_uid_after_older_redirect()`及`commit_reset_uid_after_older_redirect_for_redirect_range()`清理首段上下文；epoch核对仍为必要门禁。
- runtime reset：通过reset epoch立即使旧证据不可读；不为此每拍/每reset扫描历史status。
- terminal retire：保留只读事实直到该UID status重置，避免RM稍后读取时被提前清掉；它不表示pending工作，不影响drain/end条件。
  每UID只有固定大小结构，沿用本来已分配的status生命周期，不追加逐拍/逐重放历史。后续新主表reset统一释放。

重放替换必须检查前一个attempt已观察到response且其sample早于新request，或已因epoch/reset失效；
不得让新request无条件覆盖仍在执行的同实例。正常内部mabuf重放不改变dynamic epoch，以request sample区分attempt。

`common_data_transaction`增加`split_observed_reset_epoch`及`split_observed_sample_seq`两个常数大小完成水位，
constructor清0；仅active L2TLB owner在本拍全部首段/后页观测函数完成之后按值更新。当前reset不匹配使旧水位失效。
readonly必须证明同reset水位已到达LDA写回sample，才能把“未见高页”解释为截至写回确实未执行；水位不足不授权早停，
返回`NOT_SETTLED`交给RM有界重试，不能只凭producer水位代替consumer完成。它们不参与DUT调度、不改变现有transport/producer水位。
水位是连续完整消费边界：同reset首次完整observer sample建立baseline，其后仅`sample_seq=frontier+1`推进；
NOT_READY、history或其他早退sample没有执行observer时不推进，下一完整sample不得跨缺口。
reset更换后重新建立baseline，旧context同时因reset失效。首次baseline必须位于现有startup/reset admission静默期：
responder本reset尚未开放过ready，reset已清空DTLB，初始翻译未完成不可能产生mabuf实际split。
首次完整sample同时校验`acceptance_opened_since_reset=0`且无split request/data/response；不满足时fatal，不能凭context为空断言完整。

## 5. 被动观测Flow

### 5.1 `observe_first_split_request_sample()`

抽象功能描述：在已有L2TLB sample anchor及lifecycle事件处理后，将真实mabuf请求绑定到当前UID；
为低段建立一次执行上下文，为后页请求标记首段早停已不适用。不生成请求或翻译条目。

输入：split fire、ROB、fullva、sample_seq、reset_epoch；公共active ROB map及main/status。
输出/副作用：只更新该UID的`first_split_access`。

```text
无fire则返回；按ROB常数时间查active UID，取main/status。
若不是scalar Load，作为基础接口/身份不一致报错，不修改状态。
若请求VPN不同于main首页，无同epoch/reset低段上下文则fatal；有上下文则标记later_page_seen并返回。
若已有同实例未结束attempt，先检查旧response与新request严格有序；不满足则报错，原子保持旧上下文。
构造清零的新值结构，填当前UID/epoch/reset/ROB、main原VA、实际低段VA及request sample，再一次性发布。
```
中文文字伪代码：该函数只为物理拆分请求建立身份锚点。先确认真实fire及现有active映射，再区分低页与高页。
高页仅使早停证据失效；低页必须在验证前一attempt完成后才替换，避免旧response被误解释成新attempt。
已有ROB查找和main/status getter负责唯一UID定位与基本有效性，函数不扫描主表或创建TLB映射。

### 5.2 `observe_first_split_data_sample()`

抽象功能描述：把LU1实际S2取数时刻绑定到已有低段attempt，供RM在准确时点查询独立错误历史；不观察DUT异常。

```text
无S2有效事件则返回；按ROB查当前UID/main/status。
若事件是高页或没有本轮低段上下文，则不授予首段证据。
校验当前epoch/reset、ROB、VA与request sample，要求data sample严格晚于request；失败不更新。
响应前允许严格更晚的S2替换对应data sample及纯cache资格，等待同attempt response；响应后不再更新。
```
中文文字伪代码：S2 fire证明该次数据锁存，而不是以较晚response/WB猜测何时取数。
高页仍由原链处理；低页只有匹配已观察的物理request才记录采样。每次新attempt先清旧sample，因此不会混用重放事件。

### 5.3 `observe_first_split_response_sample()`

抽象功能描述：保存低段返回发生的结构身份、采样及路径信息，证明同一次取数已有对应返回；不解码或保存exception。

```text
只接收原通路排除misalign wakeup后的response；查active UID。
高页不更新本结构；低页校验同attempt UID/epoch/reset/ROB/VA。
要求request < data < response，且response单调；校验失败不覆盖旧事实。
保存response sample、VA、NC/MMIO/need_replay；原later-page observer仍按原调用和语义运行。
```
中文文字伪代码：先区分返回所属页，再把返回与同一低段request/S2锁存关联；不拿返回中的异常位授权早停。
path/replay事实只描述执行证据；最终是否应异常由RM读取独立模型决定。

三个函数的必要接口/身份错误使用现有UVM error/fatal规则，不增加默认关闭的二次调试检查。
高频路径只有active map查找与固定结构更新，无全表扫描、额外queue或owner。

## 6. RM只读证据与独立错误Flow

### 6.1 `try_read_first_split_cache_error_for_rm()`

抽象功能描述：给定RM已验证的首段物理line、动态UID身份和LDA采样上界，只读返回此低段实际取数时已经生效的独立数据错误。
本函数不验证PMA/权限、不读DUT异常决定期望、不修改status或TLB表。

输入：uid、dynamic_epoch、ROB、原VA、RM首段line_addr、bound_load_sample；输出值型context/error view。

```text
清零输出；先检查同reset消费完成水位是否达到bound_load_sample。
水位未达时，observation_pending=1，返回NOT_SETTLED；不得先因context尚未创建而返回普通未证明。
水位已达后读取该UID status里的context副本。
若valid、UID/epoch/reset/ROB、原VA、首页VPN或request/data/response顺序不符，返回未证明。
若later_page_seen、data_cache_eligible=0、response_need_replay=1，或response晚于bound_load_sample，或NC/MMIO，返回未证明。
其余无证据为SETTLED_UNPROVEN；独立错误证明成立为PROVEN。不能以尚未消费的后页事件授权早停或直接查询第二页。
以context.data_sample_seq调用既有read_l2_d_error_for_rm(line_addr, sample)。
记录未就绪或无有效denied/corrupt时返回未证明；有独立错误则复制context及错误generation/activation，返回证明有效。
```
中文文字伪代码：只读接口先证明请求、取数、返回属于当前动态实例，再使用S2时刻读取既有历史。
查询只消费独立D.fire生效记录，不创建line，不把RESERVE或未来activation当作错误。
未证明不会变成PASS，RM仍走原完整翻译/错误诊断。需要展示的DUT异常仍仅由后面的实际值比较读取。

### 6.2 `observer_try_apply_first_split_data_error()`

抽象功能描述：RM准备进入第二页时，独立判断已经执行的首页段是否应以数据错误终止；成功时形成非零期望，保留真实部分PA。

```text
限定paged、misaligned scalar Load、跨4KB、硬件支持拆分且首页无TLB/PMA/权限/NC/MMIO异常。
核对首页段每个已解析PA byte有效、属于同一physical line，逐byte独立PMA/PMP判为normal cacheable且无AF。
按同ROB读取LDA view，只取有效身份/source/sample作为上界；不根据exception_vec的值分支或构造期望。
调用只读首段cache错误接口；仅SETTLED_UNPROVEN继续原第二页查询。
NOT_SETTLED透传deferred，构造函数退出但不报错、不进行第二页查询。
证明有效时独立设置expected_dcache_denied/corrupt及exception bit5/19，denied优先；不填任何高页PA。
返回“本段已终止”，使构造函数停止后续页循环。
```
中文文字伪代码：该函数只在当前RM已独立证明首页翻译与普通cache路径有效时处理数据错误，不能绕过首段权限异常。
逐byte循环上限为一次scalar访问最多8字节，不扫描历史表。实际写回只提供身份及时间边界，错误来自独立line历史。
未达到门禁时保留原lookup失败能力，不能把缺证据当作停止理由。

### 6.3 `observer_build_commit_item_with_observation()`集成

抽象功能描述：为提交比较构造独立架构访问预期及所需PA；遇到已证明的首段终止异常时不要求从未执行的页。

```text
保持现有首VPN上下文、PBMT/权限/TLB/PMA故障优先级和逐byte翻译顺序。
在byte VA第一次跨出首页、调用resolver之前，使用已完成首页段调用新数据错误预测函数。
若独立终止成立则结束循环；NOT_SETTLED通过新增output deferred退出；仅SETTLED_UNPROVEN按原逻辑解析下一页。
保持真实pa_valid_mask；非零预期异常沿用现有不要求完整PA的规则，正常访问仍必须完整PA。
不把部分访问设置成all_normal_cacheable；后置D-error helper沿用已有“已有异常则不重复添加”规则。
继续原observer_capture_commit_actual及rm_ls_core严格比较，不改实际异常、比较mask或允许值。
```
中文文字伪代码：插入点在第二页解析之前，但不会跳过首页异常判断。新分支必须已形成独立非零错误预期，
因此不靠放宽“正常访问完整PA”来通过；原a13d later-split异常优先级和current-live resolver原样保留。
Bare和非跨页/Store等相邻路径保持原行为。本方案不重构既有完整访问的WB时点D-error算法。
原三参数`observer_build_commit_item()`只转调本四参数函数并返回其成功值，不能把deferred解释为成功；
保留该接口仅为已有调用源兼容，生产提交主流程直接接收四参数的deferred。

### 6.4 `observer_process_current_commit()`延迟证据处理

抽象功能描述：维持原提交游标，在唯一的首段观测消费尚未完成时允许RM稍后重试，不把调度先后变成永久比较失败。

```text
调用build并读取deferred；构造成功或已settled则清observer_first_split_wait_count。
若deferred，递增固定单UID等待计数；不到256次poll则直接返回，保留current_uid、不add_item、不置mismatch。
达到256次poll仍未settled时，报专用首段观测未收敛错误并按原规则置mismatch，禁止无限等待或查询未知第二页。
非deferred的原构造错误仍照原逻辑报告；不把真实query miss降级为等待。
```
中文文字伪代码：RM仍由现有poll驱动，每次最多服务当前提交UID。等待计数constructor/configure清0，
只在同一UID被证据门禁暂缓时递增；构造成功或普通失败即清零。它不参与激励、提交或terminal推进。
consumer延迟几拍后到达LDA上界应正常继续；若缺口永久存在，有限等待后显式报证据合同错误。

## 7. 专项验证与验收

1. 修前短小真实DUT复现：使用既有basicTest/vseq/fault3 cfg临时权重，只生成一个跨4KB的LH/FP LH、
   level0无TLB/PMA fault、sticky corrupt100%且denied0、无地址复用；证明旧版仍查询不存在第二页，不以参数改变冒充原seed通过。
2. 必要定向helper：扩展既有provenance专项，不修无关soft_test。验证request→S2→response顺序、同实例重放替换、
   redirect/reset失效、错UID/epoch/ROB/VA、缺S2、只有RESERVE、未来activation、data之后至response/WB前activation均不能授权早停。
   首段clean/高页已真实请求必须继续完整翻译；补高页→低段replay后sticky保持、纯replay、forward、水位不足及旧reset水位负向。
   延迟消费后须可恢复且UID不提前推进；缺口不能伪settled，永久缺口必须在256poll内明确报错。
   独立corrupt/denied生效在S2之前才可分别证明bit19/bit5。
3. 真实DUT正向：corrupt、denied、clean跨页各专项及多seed；wave确认实际S2捕获sample，而非固定response减拍。
4. 原seed1992896882、原tc/ts/cfg/10000笔/无临时参数、原timeout完整复测，正常TEST_PASS/finish，raw及final/caught/demoted六项全0，controller rc0、unknown0。
5. 相邻回归：原Sv39 seed212889620、a13d/c976相关later-split定向及真实邻接、PBMT response-fault和CSR/SFENCE各必要seed。
6. 每次修改编译进simv的源码后fresh compile；软件负向预期caught单列，不计真实DUT严格PASS。
7. 独立implementation review及主agent最终复核，更新Buglist/analysis/flow/状态；本Bug专项通过后单独Git commit，不混四项无关知识库文档。
8. 后续才恢复fault3：19个ABORTED保留原seed，80个NOT_RUN按执行规则构造不重复替代seed，保留原FAIL和所有轮记录。
   25组覆盖率基线已存在，但修复后需评估同一稳定版本的最终250组验收/覆盖率边界，不称母任务已完成。

## 8. 文档同步与Plan差异

同步W44 RM Buglist、fault3分析、新implementation review及母状态；首段旁路观测如影响现有L2TLB flow，补充其诊断职责，
不改变DTLB/L2TLB上游响应模型。必要profile记录五个S2被动信号的V2定位，不能描述成memory下游响应。
方案完成coding/专项/review后移入`rm_plan/do`；review保持undo直到用户明确归档。

本方案由责任分析直接形成，无独立旧版plan；与候选讨论相比，明确要求观察真实S2取数sample，
不用response/WB时点倒推错误，不引通用ledger，也不改later-page TLB状态机或resolver。
如执行中确需改变状态、接口或验证边界，记录`IMPLEMENTATION_DELTA`并重新独立review，不隐藏扩大范围。

### 首轮独立review修订

Turing首轮`REVIEW_FAIL`的三项阻塞已落回正文：同实例高页标记单调保持；纯cache资格与非replay返回；
同reset连续消费完成水位覆盖LDA上界。第二轮指出未settled不能退回第二页查询，已补pending/deferred三态及256poll有界重试，
并明确缺口冻结、不得跨越。修前compact U=100、seed26013001在3498ns UID0/VPN825ce复现第二页query miss。
U=0旧尝试仅产生合法PF，不算复现或修复通过。最终修订由Turing再次独立review为`REVIEW_PASS`后才开始coding；
主agent复核通过，当前仅按该方案修改mother中的RM及必要被动观测链，不改RTL或激励。

## 执行中补充（IMPLEMENTATION_DELTA）

新增`observer_build_commit_item()`的`output deferred`使用默认值0，保持已有三参数调用源兼容；
真实提交主流程显式接收第四参数，并执行本方案的有界等待。默认值不改变既有soft_test的期望或检查行为。
首轮fresh compile发现旧三参数调用缺少参数，未启动任何seed；原失败结果目录保留，修正后重新compile，
不把开发中的编译错误登记为新的回归功能Bug，也不冒称最小用例已经PASS。

S2同attempt的响应前重复取数允许以严格更晚的sample替换，保存最后一次取数的cache资格。
原因：LU内部fast replay可不经过新的mabuf请求；只接受首个S2会把早先重放取数误绑到最终响应。
仍要求request < 最后data < response，response后不得更新；增加确定性重取数覆盖。
主表类型使用现有`fuType/fuOpType`，不是readonly view中的`fu_type/fu_op_type`；r2编译失败证据保留，未运行seed。

### 实现独立review后的门禁补严

上述output默认0候选在r3被VCS拒绝：output默认实参必须是可赋值对象，常量不是合法左值。
最终保留原三参数`observer_build_commit_item()`作为兼容wrapper，内部转入四参数
`observer_build_commit_item_with_observation()`；真实提交主流程直接调用四参数并接收deferred。
wrapper只忽略不适用于旧定向调用的等待输出，不修改原测试预期；r3失败证据保留，不计功能失败。

readonly在水位和基本身份通过后，增加精确低段几何：从main `fuOpType[1:0]`取尺寸，
LH为原VA；LW原VA低2位为1时减1，其余原VA；LD原VA低3位为1/2/3时分别减1/2/3、为5时减1，其余原VA。
该规则来自V2 `LoadMisalignBuffer.scala`的低段拆分，必须要求request VA等于此结果且与原VA同cacheline，
data/response继续等于request VA。同VPN但不同line绝不能借原line错误授权。LB等不支持尺寸不授权。

高页request在当前UID/epoch/reset尚无低段context时必须fail-fast，不允许丢掉“已执行高页”的禁止事实后再建立低段。
三observer调用顺序调整为request→response→data：response使用拍前已保存S2；成功保存response后，本拍S2因response已存在不能覆盖。
这处理同owner同拍data/response的边界，不能先写本拍data使合法旧响应因data==response被静默丢弃。
必要定向补同页异line、合法LW/LD向下对齐、错误几何、首次高页、同拍response/data及eligible1→0。

### r4动态发现的无效payload门控修正

r4 fresh compile通过，最小用例仍query miss；NPI在3471.25ns实际S2显示rawmiss0、fullfwd0、D-channel0、
MSHR result_valid0、NC0、16个fwd mask全0，但MSHR选择payload为1。无效payload不能否决purecache资格。
仅移除原`!io_forward_mshr_forward_mshr`；保留`!forward_result_valid`已排除任何有效MSHR数据，
`result_valid=X`通过资格`===1`保持fail-closed，独立D-channel限制不变。
该修正已由Turing对照`Bundles.scala::mergeTLData()`独立review为PASS，属于本Bug实现门禁修正，不扩大支持面。
权威证据：`mem_ut/ver/ut/memblock/sim/mother_first_split_compact_post_fix_r4_20261030_results/cache_eligibility_npi.json`。
r4必要软件provenance定向已PASS，真实最小用例仍FAIL；必须fresh compile后两者复测，不将软件PASS当DUT修复完成。

### 外加软件协议fixture验证（不改变生产输入）

为动态覆盖256poll与真实frontier发布分支，外加验证源码保存在ignored仿真证据目录
`mem_ut/ver/ut/memblock/sim/mother_first_split_protocol_gate_fixture_20261030/first_split_protocol_gate_tc.sv`。
独立mode通过既有Makefile的TB_LIST在原`-f ../cfg/tb.f`之后追加该文件，fresh compile；不改任何生产文件、cfg或filelist，
不影响正在运行的原seed10000笔所对应的2540输入快照和simv指纹。fixture及命令/hash均随结果归档，不能计作真实DUT PASS。

fixture继承既有软件RM测试的构造helper，但只执行本Bug协议：合法PMP/PMA、首页翻译、首段执行、独立corrupt和明确软件LDA。
无时间推进地调用真实`observer_process_current_commit()`，检查等待不产生item/history、不推进UID、不置mismatch；
settled后由partial PA/bit19路径恰推进一次；独立重建模型后第1–255次等待不报错、第256次只捕获指定未settled错误。
另派生test-only sequence，仅重写虚拟输出send/ACK为本地计数，不重写`send_l2tlb_cycle()`；注入合法同reset owner、C−2 CSR history，
READY100/101、NOT_READY102、READY103/104，直接核对生产frontier停在101。软件owner/history在同一零时段保存/恢复，
没有真实driver或真实ACK，不声明完整transport owner释放通过，不改生产lifecycle约束。
该测试方向已由Turing只读review认可；最终r2对READY100/101/103/104额外断言admission-settled等于step、NOT_READY102仍101，
避免全部sample早退造成水位缺口假阳性。r2真实方法测试PASS，预期caught ERROR1/FATAL0，raw/final/demoted均0；
fixture SHA256=`661976be78f9e86b9013e870899a2fede586b91ff4274fdd125d77df3365bded`。Turing已完成执行结果及fixture最终独立复核，PASS。

## 专项验收结果（2026-10-30）

原seed1992896882、原tc/ts/cfg/timeout、无临时plusarg、MAIN10000完整STRICT_PASS；RM比较10000次。
真实DUT专项合计21/21，包括上述原seed、LH corrupt1/denied1/clean3、LW3、LD3、Sv393、CSR2、PBMT2、SFENCE2。
四类必要软件专项provenance/cancel/protocol r2/priority分别PASS，预期caught ERROR/FATAL为2/5、0/19、1/0、3/0；不混算DUT PASS。
详细结果路径、输入清单和历史失败保全边界见对应implementation review第5节。最终review已PASS，单Bug提交后才恢复fault3剩余99项。
