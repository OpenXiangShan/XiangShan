# MemBlock V2 虚拟化框架与后端周期帧正式方案评审记录

- 日期：2026-11-17。
- 版本：`mem_ut_uvm_v2`，源码基线 `264d54012bd97d0bc09f2cbdef4a4a2722d95864`；包含当前工作区已有代码作为只读分析输入。
- 状态：方案静态评审通过；无必须修改项，可作为后续 coding 依据。
- 任务边界：仅生成和完善正式方案，未修改源码、cfg、脚本、RTL，未编译或仿真。

## 1. 术语与抽象功能说明

| 术语 | 中文含义、代码落点 | 场景示例 |
|---|---|---|
| plan、coding | 功能方案、后续源码实现 | 方案通过不等于代码已经完成 |
| subagent、review | 独立评审代理、方案复核 | 三个代理分别检查框架、后端时序、RM |
| frame、sample | 一组后端周期输入、真实采样编号 | fault帧在S消费，普通地址在S+2采集 |
| owner、barrier | 唯一状态管理者、控制屏障 | CSR切换由原owner等待旧访问排空 |
| CSR、PMM、SPVP | 控制寄存器、指针掩码模式、特殊访存身份选择 | PMM-only手工配置仍经过原配置流程 |
| RM、readonly | 参考检查组件、只读输入接口 | 恢复前冻结参考输入，RM可晚于恢复比较 |
| batch、rollback | 正常提交列表、访存违例回滚重发 | rollback拍可携带合法older提交 |
| preflight、applied | 驱动前资格检查、采样后状态应用 | 已发scommit不能在applied时被软件拒绝 |
| hook、legacy | 可覆盖配置入口、原有兼容流程 | 手工PMM不强制legacy建立special owner |
| UID、epoch、ROB | 指令编号、执行代次、重排序队列身份 | ROB位置复用时旧WB不能归属新实例 |
| HLV/HLVX/HSV、HFENCE/HINVAL | 特殊Guest读/执行许可读/写、翻译同步/失效操作 | HSV继续STA与STD两路真实完成 |

本记录只汇总方案修改和独立评审结论。函数实现级控制流以正式方案为准，本记录不重写或另设实现合同。

## 2. 评审对象与分工

| 对象 | 独立评审者 | 最终结论 |
|---|---|---|
| [虚拟化框架正式方案](../../plan/do/memblock_virtualization_framework_support_formal_plan_20261117.md) | `/root/virtualization_plan_review` | 最终全稿复审：无必须修改项 |
| [后端周期帧正式方案](../../plan/do/memblock_v2_backend_cycle_frame_formal_plan_20261117.md) | `/root/backend_frame_plan_review` | 最终复审及RM冻结插入点补充复核：无必须修改项 |
| [RM与PMM正式方案](../../../rm_plan/undo/memblock_v2_virtualization_rm_checker_support_formal_plan_20261117.md) | `/root/rm_pmm_plan_review` | 第三轮正式稿复审：无必须修改项，详见独立RM记录 |

主代理负责写正式稿、逐项落回合理意见、交叉接口核对及最终自查；三个独立代理只读review。初稿均保留，不覆盖既有讨论记录。

## 3. 已收口问题

| 问题 | 正式方案中的处理 | 保留的原能力 |
|---|---|---|
| 操作码重叠和FP候选误入 | 13条明确编码，联合fuType/opcode分类，枚举/最小模板/fallback全部隔离 | 原普通INT/FP、AMO、CBO类别 |
| 特殊地址仍带普通imm | 所有自动地址出口统一src_0目标VA、imm=0；手工输入只校验 | 原边界和地址关联生成 |
| PMP登记部分使用dmode | 四个生产入口改用special的SPVP身份 | 原PMP模型及上下文版本 |
| Guest维护被HS限制 | 有界维护步骤复用原owner/观察槽和C0/C4 | 原HS流程、失效服务和L2生命周期 |
| HINVAL与HFENCE运输不可区分 | raw/decode/payload/matcher加入flushPipe | 原fence完成和失效范围匹配 |
| CSR发送与稳定判断不完整 | 完整CSR item先发送，PMP/PMA按开关随后发送；changed事件和clean四拍稳定分别确认 | 原配置candidate、完整payload与PMP-only变化 |
| 手工PMM无入口或被随机器先拒绝 | 初始/动态customize_target与手工模式固定；手工跳过随机必变求解 | 默认随机模式与legacy生命周期 |
| 新调度可能增加owner | scheduler作为原唯一主service的有界helper | 两agent、端口写入者、原年龄仲裁与allocator |
| rollback误清所有提交或重用旧frame | 明确相邻物理来源，最终绑定E/S来源，主帧和R+1阻塞尾拍预装 | 原level、冲刷范围、重发规则 |
| 已发scommit后记账可能失败 | 驱动前检查，采样后只应用已批准列表 | 原正常/control/fault资格与单次状态更新 |
| 恢复先清掉fault身份 | WB semantic batch后应用frame，再处理deferred ctrl及原恢复 | 原cancel锚点和资源回收 |
| 非对齐标量被降级未支持 | 当前V2覆盖valid固定关闭，标量仍必需LSQ S+2 | HLV/HLVX/HSV非对齐异常地址检查 |

五类fence权重仍为1/1/1/0/0；13条特殊访存权重默认全0；scope在主表保存，自动全地址/全ID，手工指定零地址和零ID均合法。未增加范围随机、PMM RM override、第二翻译模型或replay记账系统。

## 4. 三份方案的协作与实施顺序

| 顺序 | 实施对象 | 单一责任 |
|---|---|---|
| 1 | 后端周期帧 | 双driver运输、实际S、采样应用先于恢复、普通地址S+2 |
| 2 | 虚拟化框架 | 指令生成、SPVP、CSR配置维护、主表fence范围与完整payload |
| 3 | RM/check | 五PMM字段链、两页参考冻结/只读导出、权限/数据/异常/地址比较 |

共享修改只实施一次：操作分类由框架提供；异常实际地址由后端方案提供；PMM raw/runtime/readonly和参考冻结按RM方案实现，框架使用其完整字段，不维护第二份控制镜像。参考冻结插入后端frame apply的rob_commit更新前，不等待RM通过，也不改变原恢复锚点。

可分阶段实施和编译，但全部已确认功能及三份验收矩阵完成后才能宣称整体支持。源代码修改须按现有授权和plan执行规则启动；本次评审不是coding授权。

## 5. 最终检查及验证边界

- 三份正式稿均有首章术语、实现flow、固定字段/参数、关键函数抽象职责与伪代码，以及末章初稿差异说明。
- 主代理核对跨方案字段责任、配置传播、frame采样与冻结位置、异常恢复顺序和正常完成条件，未发现剩余必须修改项。
- 四张Mermaid图均已由本地mmdc实际渲染成功；Markdown围栏与相对链接检查通过；本轮新增文档的空白检查通过。
- 评审是静态方案结论。实现后仍必须用当前生成RTL、远端编译/仿真和波形验证E/S/S+2及CSR传播，不能将此记录当作实现或回归通过证据。
