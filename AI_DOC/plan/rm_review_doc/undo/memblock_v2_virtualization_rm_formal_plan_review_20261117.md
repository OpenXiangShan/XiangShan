# MemBlock V2 虚拟化与PMM RM正式方案评审记录

- 日期：2026-11-17。
- 分支：`mem_ut_uvm_v2`。
- 对象：[RM与PMM正式方案](../../rm_plan/do/memblock_v2_virtualization_rm_checker_support_formal_plan_20261117.md)。
- 独立评审者：`/root/rm_pmm_plan_review`。
- 结论：第三轮正式稿评审通过，无必须修改项，可作为coding依据；代码未实施，未编译或仿真。

## 1. 术语与抽象功能说明

| 术语 | 中文含义与落点 | 示例 |
|---|---|---|
| RM、checker | 参考模型、正确性比较 | expected不能来自被比较的DUT输出 |
| snapshot、bundle | 同一动态实例的冻结参考集合 | 最多首后两页和原CSR |
| readonly、nested type | 只读导出接口、类内嵌套类型 | common_data不能引用之后才定义的readonly类型 |
| token、provenance | 实际翻译请求记录、参考来源身份 | 补GPA响应保留自身sample与payload |
| nonblocking、blocking | 当前非阻塞DTLB路径、另一种阻塞实现 | 本DUT不走blocking直接refill公式 |
| commit、replay、epoch | 提交消费、执行重试、动态代次 | HSV重试不产生RM第二次Store效果 |
| overlay、backing | 已提交写参考覆盖层、物理基础数据 | HFENCE不清除同PA数据 |
| PMM、SPVP、MXR | 指针掩码、特殊访存身份、执行页读取控制 | Guest的VS和G阶段控制不同 |
| GPA、HPA、GPF | Guest物理地址、最终物理地址、Guest页错误 | 隐式VS页表GPF需要独立GPA参考 |
| frame、applied | 后端输入周期帧、采样后的业务应用 | fault参考在applied内、恢复前冻结 |

本记录汇总方案评审，不另定义函数控制流。抽象职责、源码级与中文伪代码以正式方案为唯一实现合同。

## 2. 评审闭环

| 轮次 | 主要必须修改项 | 已落实结果 |
|---|---|---|
| 初稿 | PMM五字段缺链、四拍生效、Bare地址、迟到比较、隐式GPF、WB归属 | 完整raw/runtime/readonly、稳定配置、先预检查、两页冻结、独立GPA、实例过滤 |
| 正式稿复审 | 不可达blocking分支、snapshot编译依赖、冻结位置含糊、生命周期残留 | 固定nonblocking公式；前置snapshot class；frame apply逐UID更新前冻结；RM本地释放 |
| 最终复审 | 全稿、跨方案、最小改动及onlyS2边界复核 | 明确无必须修改项 |

## 3. 保留的完整功能和最小实现

支持13条特殊访存、有效身份、VS/G权限与MXR、HLVX执行权限及PMP READ_EXEC、STD独立数据、PMM五来源与优先级、符号/零扩展、首末字节预异常、Bare/两阶段/跨页、普通与隐式VS GPF地址、动态配置与迟到比较。

复用既有uid/later-split记录及翻译seq；两页深复制在common_data前可见的已有类型上定义，readonly后转换其nested view。正常标量与特殊标量在frame apply更新rob_commit前冻结，fault同样在恢复前冻结。RM继续rob_commit与原CMO例外，不统一等terminal_done，不增加replay/effect账本、通用每attempt历史、公共retain/release接口或第二fence owner。

formatter只接正确size/signed/fp_wen；物理backing、Store overlay、错误历史及既有LRSC规则不因fence/PMM变化被清除。

## 4. 静态差异与后续验收

onlyS2/Sv48x4/PMLEN16的bit49:48组合保留为必需验证点，不改responder key掩盖差异。实现结果检查与独立PMM语义检查不能互相冒充；仅缓存内部key不同不足以认定架构错误，需可区分的数据/权限/异常结果确认。方案可coding不意味着这一RTL边界已经由仿真证明正确。

后续按正式方案验收矩阵检查生产构造链、首后页hit/miss、fault恢复前冻结、ROB复用、PMM-only切换和普通路径。框架恢复不等待RM判pass，actual不能反填expected。

三份方案整体评审与实施责任见[框架及后端评审记录](../../test_framework/review_doc/undo/memblock_v2_virtualization_backend_formal_plan_review_20261117.md)。
