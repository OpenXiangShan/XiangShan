# MemBlock V2 可路由 PPN 地址合同分析

日期：2026-11-02。范围：测试框架地址生成及其实际RTL接口合同，不新增或维护RTL知识库。本文为源码同步分析，VCS/专项结果由本轮最终执行记录提供。

## 1. 术语与抽象功能说明

| 英文术语 | 当前分析中的中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| PMA / PMP | 地址属性和保护权限 | `memblock_pma_pmp_model` | c=1不能替代路由判断 |
| PPN / PA / VPN | 物理页号、物理字节地址、虚拟页号 | builder与主表 | raw PPN先保留高位，不能截断PA再检查 |
| PBMT / NC / IO / MMIO | 页面类型、不缓存主存、IO属性、按PMA判定设备访问 | `effective_pbmt()` | 00+c0重选时仍保留c0 |
| sparse / strict | 按需分配内存模式、限定配置PA范围模式 | `MAIN_MEM_RANGES_EN` | 新策略只改变普通sparse路径 |
| route / manager | 实际出口译码、接收地址的总线端 | V2 profile与TLXbar_6 | PMA允许但route不匹配仍会堵住 |
| snapshot / generation | 固定配置副本及其版本 | `pma_history` | 请求sample查询固定历史版本 |
| shape / canonical / NAPOT | 完整映射形状、对齐基页、连续64K编码 | policy、builder | policy不返回NAPOT标记 |
| intent / sector | 主表预登记目标、S1八扇区响应 | intent表、valididx | 普通4K仅一个扇区有效 |
| reuse / hit / miss | 历史PPN复用、已有映射命中、新建映射 | common data | 只在miss发布前改PPN |
| sample / FIFO / debug | 采样边界、待回放事件队列、调试访问属性 | common data及PMA模型 | CSR C采集到C+2成熟 |
| GPA / stage | S1中间地址、翻译阶段 | S2 tag及S1/S2响应 | allStage仅最终PA应用外部route |
| A.fire / reset | 外部请求握手、复位生命周期 | 专项vseq | 到达统计不等于选择统计 |

抽象功能说明：地址合同由两类输入组成。静态profile回答“当前生成DUT是否把这段PA送到UT外部响应端”，已提交PMA版本回答“这段PA的属性是什么”。策略从其交集选择地址，RM仍独立判断真实访问的数据及权限。

## 2. 为什么不能只列APLIC/UART设备地址

V2 UT的响应端接在MemBlock内部Uncache crossbar之后。当前RTL的out1译码合并后为四个半开区间：

| 起点，含 | 上界，不含 | 用途 |
|---|---|---|
| `0x0` | `0x31108000` | 低地址外部空间 |
| `0x31120000` | `0x31128000` | APLIC S有效域 |
| `0x31140000` | `0x38022000` | 后续低地址外部空间 |
| `0x38022080` | `0x1000000000000` | 内部控制器之后全部48位外部空间 |

两个空洞是`0x31108000..0x3111ffff`和`0x31128000..0x3113ffff`。`0x38022000..0x3802207f`送到DUT内部DCache控制器，具有伪错误注入副作用；它不是B08无人响应的洞，但普通随机内存访问仍排除。

这比真实SoC设备寄存器窗口大。UT外部responder可以为这些被路由出来的地址建模通用存储，地址生成无需进一步缩成真实设备寄存器集合。APLIC/UART/TIMER/PLIC是提高抽样概率的命名窗口，不能替代全集。

权威合同落点：`memblock_routed_ppn_profile_v2.svh`。核验脚本`check_memblock_v2_routed_ppn_profile.py`解析`build/rtl/TLXbar_6.sv`实际位切片、拼接、异或及或条件，精确归并out1谓词，并单独比较out0内部128B。RTL重生成后必须重新核验。

## 3. 新旧地址生成行为

| 路径 | 旧B08实现 | 当前实现 |
|---|---|---|
| 普通sparse cacheable | 全部压入512GiB固定窗口 | 原候选完整shape安全且同c1则保留 |
| 普通sparse PBMT00+c0 | 搬入默认c1 DRAM，访问类别可能改变 | 在同c0的外部合法shape中选择 |
| 普通sparse NC/IO | 固定512GiB DRAM内访问 | 外部合法全集与命名窗口均可选择 |
| strict配置 | 按配置PA窗口生成 | 保留原路径 |
| CSR权限/fault/非LEGAL专项 | 保留既有响应构造 | 使用旧安全候选，不以本策略消除预期异常 |
| UART128B、外部部分页 | 无上游offset意图 | 独立窄窗口随机主表与intent |
| PPN reuse | 主要检查shape/stage资格 | 另查本次固定PMA版本及完整页路由 |

普通cacheable保留规则不是“任何c1都一定有响应”。只有同时通过route与完整shape属性检查才保留。若原anchor超48bit，不得截断成低地址后定义目标c；当前策略直接从合法空间重选。

## 4. 属性区间导出与查询

### 4.1 `memblock_pma_pmp_model::read_pma_attribute_spans()`

抽象功能描述：为生成器导出指定历史版本的值型属性分段，不让生成器复制PMA匹配算法或读取不断变化的最新表。

源码核心：

```systemverilog
boundaries.push_back(49'h1_0000_0000_0000);
...
if (find_first_match(snapshot, span.first[47:0], 1, debug,
                     hit_index, matched_entry)) begin
    span.c = matched_entry.c;
    ...
end
```

中文伪代码：

```text
获取指定历史版本；失败返回false，不退回默认配置；
以49bit边界表示完整48bit空间的半开上界；
收集所有表项匹配边界和debug边界，包括OFF项对后续TOR的下界作用；
在每段首地址调用原模型first-match，复制c/r/w/x/atomic；
无匹配使用原模型默认零属性；相邻全部属性相同则合并；
仅返回值型队列，不写回模型，不消费CSR FIFO。
```

### 4.2 `memblock_routed_ppn_policy::configure()`与查询接口

抽象功能描述：policy实例固定本次构造的属性视图，向完整shape检查提供c分类。该视图不成为RM新的配置来源。

源码核心：

```systemverilog
if (model == null ||
    !model.read_pma_attribute_spans(generation, debug, attributes)) return 1'b0;
...
if (first >= {15'b0, c_spans[idx].first} &&
    limit <= {15'b0, c_spans[idx].limit}) begin
    cacheable = c_spans[idx].c;
    return 1'b1;
end
```

中文伪代码：

```text
configure先清本实例旧状态，再导出指定版本；
相邻同c区间继续合并，不因权限不同错误禁止本可映射的大页；
query_c查询一个PA；query_span_c检查完整区间是否同c；
两者用宽输入拒绝超48位与空区间，未配置返回失败；
不维护static模型快照，reset后新构造重新配置。
```

## 5. 地址选择、shape与随机分布

### 5.1 `memblock_routed_ppn_policy::select_ppn()`

抽象功能描述：在确定PBMT、原anchor类别及shape后，返回可路由映射和窗口ID；不改变PBMT或PTE权限。

源码核心：

```systemverilog
constrain_c = pbmt == 2'b00 && query_c(anchor_pa, target_c);
...
aligned_first = (first + shape_bytes-1) & ~(shape_bytes-1);
aligned_limit = limit & ~(shape_bytes-1);
if (aligned_first >= aligned_limit) continue;
```

中文伪代码：

```text
确认shape仅为4K/64K/2M/1G/512G；
PBMT00以原resolved anchor确定目标c，NC/IO不强制c；
合法c1候选且未指定窗口时直接保留；其它合法候选按权重保留或重选；
重选对窗口、外部route、PMA同c分段求交；
在交集内部选择对齐完整shape槽，不能先落地址再向下对齐出界；
先等权抽有候选窗口，再按槽数抽其中位置；
返回shape对齐canonical PPN，不含NAPOT标记；
无候选返回false，由builder完成有界降级或报错。
```

窗口ID为0全集、1 DRAM、2 APLIC M、3 APLIC S、4 UART所在4K页、5 TIMER、6 PLIC。ID4表示可路由页，真实UART128B由专项offset限制。窗口之间允许重叠，这是有意提高小窗抽样概率；因此最终地址不是全字节均匀分布。

随机权重只影响NC/IO及PBMT00+c0的主动重选；安全PBMT00+c1默认保留。显式窗口ID用于定向场景，不应当作为默认恢复cacheable覆盖的入口。

### 5.2 `tlb_map_builder::select_final_routed_ppn()`

抽象功能描述：把策略应用到最终PA对应stage，负责有界4K降级及编码衔接。

源码核心：

```systemverilog
anchor = resolve_ppn_from_raw(canonical_ppn, input_vpn, level, napot);
...
level = 0; napot = 0;
...
canonical_ppn = anchor;
fell_back = 1'b1;
```

中文伪代码：

```text
先检查普通合法sparse适用条件；
intent路径要求4K/N0，仅检查上游已保证的offset区间；
普通路径解析真实anchor后选择完整最终stage shape；
普通大shape无候选时以第一次anchor转为4K候选，只重试一次；
显式shape不能自动降级；4K无候选立即向母函数报失败；
成功后恢复NAPOT合法编码，母函数继续sector/resolved一致性构造。
```

完整shape保护用于应对未来range hit，而不是只检查第一次请求的PA。allStage保守检查最终PA stage的覆盖，S1中间GPA只受S2输入模式位宽约束，不按外设PA表收窄。

## 6. 部分页为什么必须配合主表

L2请求只有VPN/s2xlate，不能独自决定PA低12位。普通4K只能以整页保证可路由，因此不能把`0x38022000`这页作为普通未知offset映射，否则可能访问内部前128B。

`memblock_main_dispatch_routed_ppn_sequence::build_random_main_table()`在上游先随机PA，再给独立VPN复制PA低12位，登记`routed_ppn_intent_by_vpn`。专项固定静态Sv39、S1、4K/N0、PBMT00、普通LD、无地址及PPN复用。每轮随机排列六窗口，分别是UART128B、APLIC M/S、TIMER、PLIC、外部部分页`0x38022080..0x38022fff`。

普通4K的`build_s1_sector_payload()`只激活当前sector，最小有效映射不是32K，因而此intent不需扩大为八页。已有exact/range hit必须通过intent冲突检查，不允许悄悄改写旧条目。专项禁止大页和NAPOT，避免旧range条目吞掉新意图。

intent只表示输入构造约束，不是数据或异常期望。所有intent在主表ready之前注册，`reset_all_tables()`清空。窄vseq发现启动后runtime reset会终止，而不是让清表后的旧主表继续执行。

## 7. 权限、动态配置及复用边界

PMA导出包含r/w/x/atomic，当前policy只使用c。重选保证原PBMT00的c类别，但不承诺新地址与原候选的权限、atomic或PMP完全等价。原RM按照最终PA及动态访问冻结的generation独立检查。专门制造权限/fault的路径旁路地址重选。

真实请求在get/create之前调用已有成熟CSR回放，之后按sample选择PMA generation；builder不再自己消费raw FIFO。查询版本保证构造时属性一致，但不使PTE缓存期间的PMA永久不变。既有映射保持只读；若未来扩展动态配置场景，仍应分析其后续访问属性和场景结束行为。

`try_apply_l2tlb_ppn_reuse_to_new_entry()`只扫描配置容量的history FIFO，新增`check_reused_routed_ppn()`在同次PMA视图下检查候选。历史成功并不代表对所有新访问永久合法。strict与权限/fault路径保持原资格语义，窄intent不允许reuse。

## 8. 验证边界与历史文档关系

本次已执行的静态检查包括RTL/profile精确比较、13个路由边界断言、错误profile拒绝、Python语法检查及新增SV方法临时mock语法检查。临时Verilator不支持约束随机行为，不能据此认定VCS专项已通过。真正的UART/APLIC等访问证据来自窄vseq观察的外部A.fire日志，并需结合原RM与结束状态。

旧B08固定512GiB方案及其implementation review应保留为历史修复记录，追加2026-11-02演进注记：普通sparse改为当前属性/路由交集策略，旧窗口仅留给明确旁路路径，原“所有sparse地址都在固定窗口”的描述不再代表当前默认生成。本文不直接修改旧文档，最终同步由主执行者统一完成。
