# MemBlock V2 虚拟化、PMM 与 RM 实现评审

- 日期：2026-11-17。
- 范围：第三份RM plan的当前源码差异，版本V2，工作分支`codex/memblock-virtualization-20261117`。
- 状态：最终修复已独立复核；rm5编译及95场景184检查自测通过；真实专项及整轮回归待集成补入，未归档。
- 关联方案：[RM正式方案](../../rm_plan/do/memblock_v2_virtualization_rm_checker_support_formal_plan_20261117.md)。

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| UID、ROB key | 主表编号、包含flag的完整ROB身份 | common主表、status | 同UID重发不等于同次执行 |
| dynamic/issue/replay/reset epoch | 动态、目标发射、重试和复位身份 | snapshot、status、sync包 | 旧reset页不能用于新Bare预检查 |
| snapshot、provenance、generation | 参考值深复制、来源证据、映射版本 | `memblock_rm_access_snapshot_t` | fence后旧UID仍读旧generation |
| hit、miss、token | 已完成响应可复用、需要真实PTW、每次L2请求身份 | uid/later records、pending request | token0合法，不代表未产生 |
| raw/fullva/request_vaddr | 原操作数、完整检查地址、真实50位请求几何 | snapshot与RM item | 高tag仅作用完整地址处理，不擅自改VPN |
| PMM、PMLEN、SPVP、dmode/dvirt | 掩码模式/长度、特殊访存U/S身份、普通有效身份/虚拟状态 | 五PMM CSR及纯helper | HS执行HLV仍用Guest翻译 |
| HLV/HLVX/HSV、VS/G stage | Guest读/执行许可读/写，两阶段翻译 | 操作解码和权限helper | HLVX物理R和X都检查，外显Load异常 |
| MXR、SUM、PMP/PMA、PBMT | 可执行页读取、S访问U、物理保护/属性、页属性 | 原模型及新权限分支 | G-stage只取M侧MXR |
| masked reference、token bucket | issue时冻结的独立掩码映射、一个真实token关联的最多两个后页句柄 | `pmm_arch_reference`、`later_split_owners_by_token` | 两者均不建立第二个翻译服务或调度owner |
| CANCELLED_RESET/CANCELLED_COVERED | 原公共恢复决定的复位/覆盖取消 | 地址frame状态及`cancel_reason` | 地址未采到但已合法取消，仅结束该地址责任 |
| pre-fault、Bare、canonical | 翻译前高位错误、不翻译、规范虚拟地址 | PMM预检查 | 仅末字节非法上报其页首 |
| AF/PF/GPF、leaf/nonleaf/fake | 访问/页/Guest页错误、叶/非叶/伪PTE | entry及GPA helper | 隐式VS页表GPF不能把VA当GPA |
| frame、S/S+2、actual/expected | 后端周期对象、输入及地址输出拍、真实事实/独立期望 | backend frame、地址队列 | 输出为零仍是有效样本 |
| owner、map、cursor、prefix、queue | 唯一生命周期拥有者、索引、游标、连续进度、队列 | common/readonly/RM | 地址队首未到齐时停止，不重扫历史 |
| committed overlay、backing、obligation | 已提交写覆盖层、物理底层数据、待检查责任 | 原`rm_ls_model_t`、地址队列 | HSV提交一次，地址晚到不重复写 |
| READ_EXEC、STD、WB、CMO/AMO | 物理读加执行、Store数据、写回、块维护/原子 | 原PMP和写回模型 | HSV数据来自冻结STD而不是地址 |

组件抽象职责：common保存访问时参考输入；readonly只复制与核对身份；RM局部推导权限、异常、数据和地址；DUT observer保存真实事实；原L2TLB owner继续唯一管理ready、token、失效。

## 2. 修改范围与修改前后行为

| 特性 | 修改前逻辑 | 修改后逻辑 | 正确性检查与风险 |
|---|---|---|---|
| 13op | size/LOAD/STORE缺特殊编码 | 联合fuType/op白名单，HLV/HLVX宽度/扩展，HSV冻结STD | HLV/HSV复用数值编码，不得重复case；HSV不标signed |
| 权限 | S1只dmode，Guest/S2 MXR选择不完整 | 特殊SPVP，VS合并MXR，G只M侧MXR，HLVX取X/READ_EXEC | Host无效V原行为保留，fake/nonleaf豁免限定Guest/S2 |
| 参考历史 | 后页可查current，fault cancel可能清hit | 两页深复制，真实commit/fault恢复前freeze | 缺页返回MISSING，不用新generation补旧访问 |
| PMM | raw地址同时用于查表和Bare合法性 | 原始/有效完整/50位请求分离，首末预检查 | AMO/CMO保持原路；只key差异不足架构结论 |
| Load actual | 仅ROB latest可能混旧实例 | ROB局部索引加UID/dynamic/issue/下界/flush过滤 | older Load允许sample flush大于实例epoch |
| 异常地址 | WB比较之外未独立消费 | 数据commit后排地址责任，核对frame/S+2/身份并比VA/GPA | 不能因同值/零值丢样本；范围内缺证据报错 |
| 内存效果 | 原程序序commit与Store最终检查 | 沿用原backing/overlay，同HPA共享数据 | 不新增replay效果账本或新fence checker |

## 3. 参考生产与终态冻结

### 3.1 `freeze_access_reference_for_rm()`

抽象功能描述：正常提交或fault恢复前，common为当前标量实例保存不再改变的参考输入，允许RM晚读；它不建立新映射或推导DUT数据。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数同名。功能简析：消费主表、status及uid/later记录，将最多两页和CSR复制到UID历史map。

修改后源码摘要：

```systemverilog
snapshot.address_csr.copy_from(first.csr_snapshot);
snapshot.request_vaddr={14'b0,snapshot.raw_address[49:0]};
snapshot.pages[0].copy_from(first.payload);
snapshot.valid=1; snapshot.frozen=1;
rm_access_snapshot_by_uid[uid]=snapshot;
```

中文伪代码：本段负责在公共恢复前保存参考。先判断普通标量/特殊Guest读写，控制、AMO、CMO直接走原流程；相同执行身份重复冻结直接返回。没有真实issue record的人工soft构造保留旧入口，不伪造latest CSR。有效record则复制身份和完整CSR，计算raw操作数并保存50位请求；首页按真实完成或issue-hit来源复制；真实后页record同实例且未取消才保存第二页，缺payload保持page_valid为0。最后标记冻结并写入map。`copy_from`逐字段深复制PTE与sector，避免后续live变化；`get_rm_access_snapshot`只做UID索引，不删原记录。

正确性检查：`lsq_commit_handler::apply_sampled_backend_cycle()`在normal和fault的rob_commit前调用；fault继续调用原quiesce/cancel。legacy normal/fault mark同样接入。fence只删live/completed缓存，run表重置才删除snapshot。

### 3.2 后页真实来源

抽象功能描述：`capture_later_split_issue_hit()`和`observe_later_split_store_tlb_sample()`把真实分段边界绑定到已有后页record，不驱动TLB。

源码位置：同一common文件。功能简析：前者查询本边界之前已完成响应并复制，后者关联StoreMisalignBuffer、STU0与PTW0。

修改后源码摘要：

```systemverilog
record.payload.copy_from(entry);
record.issue_hit=1;
record.state=memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED;
```

中文伪代码：本段负责区分后页hit与miss。真实跨页split fire通过active ROB索引找到当前UID，记录ROB、地址和sample；Load用原LU1链，Load/Store分别保存单buffer owner游标，Store增加STU0旁路。真实分段TLB请求匹配后，查询已完成缓存取得当时hit来源；没有命中保持等待PTW。之后若真实PTW出现，则清hit候选，记录真实PTW拍并进入原WAIT_L2_TOKEN。token绑定固定检查两个active游标，允许同VPN的Load/Store同时关联一个真实token；completion按token bucket逐个复用原校验并保存最终payload、token、sample。取消同样消费bucket，旧replay handle不得清新owner游标。首页已completed的GPF仍可由真实补响应更新原sector。查completed函数只提供历史服务输入，不计算权限答案；L2 owner仍控制运输。

风险：新增旁路已静态核对generated wire；Store req kill当前源码固定false，生成RTL不保留该wire。后页真实hit/miss和GPF补响应时序需仿真确认。

## 4. 只读身份、页面证据及实际WB

### 4.1 `validate_access_snapshot_identity_for_rm()`与页面查询

抽象功能描述：readonly所有冻结入口确认同一次执行后复制值，防止Bare和预异常绕过身份过滤；页面查询只读最多两页。

源码位置：`seq/base_seq_help/memblock_rm_readonly_api.sv`。功能简析：身份helper返回FOUND/CANCELLED/MISSING；context入口不要求PTE，translation入口要求来源。

修改后源码摘要：

```systemverilog
if (snapshot.reset_epoch!=memblock_sync_pkg::get_l2tlb_current_reset_epoch())
    return RM_ACCESS_CANCELLED;
if (snapshot.response_sample[page_i]==0 || snapshot.generation[page_i]==0)
    return RM_ACCESS_MISSING;
```

中文伪代码：本段负责旧值隔离。先确认对象有效、UID/ROB/dynamic与请求一致；reset不同返回取消；读取同UID当前status和主表，ROB/dynamic已换返回取消，再按fuType选LOAD或STA issue并核对replay。context仅复制同一CSR和原地址，允许无页。翻译查询核对page_count、VPN和page_valid，再要求首页source1/2或后页3/4、非零sample/generation、payload generation/key/stage同源，token0仍合法。失败直接返回，不向current补取。`derive_tlb_request_fields`只从复制payload和冻结CSR得到PPN/GVPN，日志不改变公共状态。

### 4.2 `read_load_instance()`

抽象功能描述：WB observer从同ROB局部历史返回当前UID实例的最终Load事实，不影响框架提交。

源码位置：`seq/base_seq_help/memblock_rm_dut_writeback_observer.sv`。功能简析：采样时通过active ROB map冻结owner，查询按当前身份和时间筛选。

修改后源码摘要：

```systemverilog
candidate.owner_uid==uid && candidate.owner_dynamic_epoch==dynamic_epoch &&
candidate.owner_issue_epoch==issue_epoch && candidate.sample_seq>=minimum_sample
```

中文伪代码：本段负责actual归属。原采样按VIF复制四态WB数据和异常，同时记录owner UID/dynamic/LOAD issue；记录入同ROB索引。查询从该索引尾部寻找身份完全一致、sample不早于issue、非replay且非flush的记录；全局flush编号允许大于当前实例编号，因为年轻redirect不会杀older Load。reset路径清全部索引和旧历史。readonly适配层提供当前status身份和首issue sample；旧ROB latest接口仅保留原非标量consumer。

## 5. PMM、权限与生产构造

### 5.1 `rm_ls_select_pmm()`、`rm_ls_compute_address_precheck()`

抽象功能描述：两个纯helper按冻结控制选择PMM并计算首末字节完整地址错误，不读DUT异常、不操作映射。

源码位置：`env/src/memblock_rm/rm_ls_core.sv`。功能简析：输入特殊属性、有效身份、五PMM和翻译模式；输出PMM、有效fullva及两端错误位图。

修改后源码摘要：

```systemverilog
if (hlvx) return 0;
if (effective_mode == 3) return mseccfg;
if (guest ? (mxr || vmxr) : mxr) return 0;
```

中文伪代码：先HLVX关闭掩码；再M身份选mseccfg；再适用MXR关闭；Host S、Guest S、U执行特殊、普通U依次选对应来源。预检查拒PMM01，合法10/11保留57/48位并按虚拟/物理环境符号或零扩展；构造首末字节，按阶段检查Sv39/48规范性、GPA41/50位或PA48位。仅末端错误时故障位置变为末字节所在页首，其余保持有效首地址。可选TLB debug是编译profile，不能传runtime priv_debug。

### 5.2 `observer_build_commit_item_with_observation()`与权限helper

抽象功能描述：生产构造按原rob_commit资格读取参考，形成特殊及普通标量的独立expected，保留原非标量路径。

源码位置：`env/src/memblock_rm/memblock_rm.sv`。功能简析：13op、原地址核对、PMM、页面、权限、PMP/PMA和数据输入在同item汇合。

修改后源码摘要：

```systemverilog
pma_pmp_cmd=store_access ? PMA_PMP_CMD_WRITE : item.hlvx ? PMA_PMP_CMD_READ_EXEC : PMA_PMP_CMD_READ;
```

中文伪代码：先联合fuType/op识别特殊操作并验证imm零、地址等于src_0；HSV取冻结STD。读取冻结完整控制，计算PMM/预异常；预异常直接构造Load/Store cause和地址，不要求token。正常路径按低50位请求VPN消费两页，第一故障段确定后不继续换段。权限计算特殊用SPVP，普通用dmode；VS读用vmxr或mxr，G读只mxr，HLVX选X；仅Guest/S2的fake/nonleaf载荷避免制造S1 PF。翻译故障按AF/S1 PF/G fault既有优先级，合法PA才用原PMP/PMA模型，HLVX合并读执行失败。无fault的数据仍按参考HPA从原backing/overlay读取或提交；AMO/CMO保持原分支。

### 5.3 `rm_ls_check_pmm_stage2_reference()`

抽象功能描述：用已经冻结的S2范围独立核对PMM only-S2地址结果，既不改原responder key，也不查当前表；证据不足产生明确失败责任。

源码位置：`env/src/memblock_rm/rm_ls_core.sv`。功能简析：输入原/掩码/reference VPN、S2 level/N和原PPN，输出两种PA及三态参考结论。

```systemverilog
if((original_vpn & ~vpn_mask)!=(reference_vpn & ~vpn_mask) ||
   (masked_vpn & ~vpn_mask)!=(reference_vpn & ~vpn_mask))
    return RM_PMM_REFERENCE_MISSING;
```

中文伪代码：本段负责架构地址责任而非只打印键差异。先按level3/2/1或NAPOT定义该冻结entry忽略的VPN低位，再分别确认原和masked输入在同一reference范围；任一不属于返回MISSING。若同时属于，使用同payload分别拼接原/掩码VPN低位，投影最终48位PA并返回MATCH或MISMATCH；权限及PBMT在同payload内同源。生产item先保留同范围结论；范围不足继续由9.1～9.2的issue时独立masked输入及生产checker证明，仍缺才报参考输入错误。原数据compare/commit后对最终MISSING报参考缺失、对MISMATCH报已证明的差异，仅MATCH通过该子责任。函数没有映射写入，不能晚查current补齐。

## 6. Guest GPA和独立地址责任

抽象功能描述：`observer_compute_guest_fault_address()`用原S1 sector推导GPA/VS标记；`observer_service_exception_addresses()`消费已经归属的frame事实并结束地址责任，不重新提交内存效果。

源码位置：同一RM文件。功能简析：前者为纯期望计算，后者为待检查队列消费。

修改后源码摘要：

```systemverilog
identity_ok=actual.sample_seq==actual.target_sample && actual.sample_seq==actual.input_sample+2;
if(guest_fault) mismatch |= actual.gpaddr !== item.expected_gpaddr;
```

中文伪代码：Guest GPA先处理onlyS2输入；其余从冻结V/R/W/X及S1 fault判断leaf/fake/nonleaf，按sector低三位拼PPN、level/N解析GVPN，按层索引乘8或叶字节偏移形成GPA。预GPF已在预检查分支使用独立fault_report_va。数据commit推进原current_uid后，仅异常item入地址队列；服务按到齐前缀读取frame，队首等待即停止。原frame合法reset/covered取消时先核对required、未capture/非missing及reset/issue/replay，匹配才结束地址责任；其他到齐样本核对UID/ROB/dynamic、S+2和reset/issue/replay；VA始终比较，GPF另比GPA和VS标记，使用四态不等捕获X。缺必需样本/不匹配报错并消费责任，终场强制报告未完成项。readonly地址入口复制原frame整组事实，不重新抓拍。

## 7. 调用关系、生命周期与性能

| 调用位置 | 被调用对象在本链的职责 | 对外结果 |
|---|---|---|
| 原真实issue | uid record、完整CSR | 保存本次访问控制 |
| 原L2 completion / split sample | payload来源与token/sample | 保存该页参考输入 |
| sampled apply / legacy commit | freeze | 恢复前固定两页 |
| 原RM current_uid轮询 | readonly及生产构造 | 构造一条独立expected |
| 原commit | 原Load/Store模型 | 单次数据比较/内存效果，推进游标 |
| 地址前缀服务 | frame actual与GPA/VA expected | 单独收口地址责任 |

snapshot固定每UID最多两页实际访问来源，另有最多两个masked架构参考槽，不是可命中未来访问的TLB；fence不清backing、overlay、错误历史或旧参考。reset隔离旧actual/reference；run重置删除UID历史。常用查询为UID/ROB索引与2页/最多8字节循环，地址服务只推进到齐前缀。Load/Store后页PTW各只读单buffer游标，token绑定固定两项，completion/cancel只访问最多两个handle；唯一全后页表清理在reset路径，完成历史不会增加高频扫描成本。没有max_cycles成功出口、通用引用计数、replay效果账本或第二ready owner。

## 8. 评审发现、限制与验证

- 已修复：snapshot上下文早退绕过身份及reset核对；page_valid但来源不完整仍FOUND；HSV被误标signed；Host invalid-V不应被Guest fake豁免；首fault段保持。
- onlyS2 Sv48x4 PMLEN16已接入issue时独立masked canonical参考：同范围helper可直接证明，否则readonly返回预定义输入，生产checker独立比较PA、G权限、PBMT及PMA。缺参考仍是框架输入错误；已证明可观察结果不同才报告语义差异。
- PMM tagged首段D-error已修复：common按低50位判页，readonly截低50位再对齐并比page/line，原始64位主表一致性保留；connect的bit49符号扩展不会被误当另一个页。需专项确认Store hit/miss、GPF补响应、迟到RM晚于fence/reset及该几何边界；预留snapshot分段字段不能冒充已生产证据。
- 静态：新增Store连接已核对V2生成RTL内部wire，格式检查待最终集成重复。
- 编译/仿真：rm5编译及95场景184检查自测通过，权威日志见末节；真实专项和全部basicTest统计待主代理补齐。本review编写方未自行运行EDA。

当前结论：已补齐独立masked参考、后页并发归属、高频扫描及合法地址取消的实现缺口；rm5最终编译与95场景184检查自测已通过；真实专项和整轮basicTest证据仍待集成方填写，不能提前标记整体验收通过。


## 9. 与原plan不一致及实现补充的细节

本章对应plan的`IMPLEMENTATION_DELTA`；第二项框架的LSQ admission白名单遗漏在第三项联合验证中修复，不能计入第二项原提交已完成的能力。其余主体职责遵循原方案，以下为使全功能输入和并发闭环所需的具体落地补充。

### 9.1 issue时独立masked输入及只读消费

抽象功能描述：`capture_pmm_arch_reference_on_issue()`在真实issue计划阶段保留已有canonical masked输入；`read_pmm_arch_reference_for_rm()`在最终比较阶段只读导出同一冻结身份下的参考。两者不建立新TLB、不晚查current、不覆盖实际请求几何。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`及`memblock_rm_readonly_api.sv`。函数功能简析：新增三字段在UID record及snapshot中深复制，readonly共享原identity helper后复制CSR/entry，并从保存payload解析masked PPN。

```systemverilog
masked_vpn=record.vpn[37:0];masked_vpn[37:36]=0;
record.pmm_arch_reference[slot].copy_from(entry);
snapshot.pmm_arch_reference[slot].copy_from(first.pmm_arch_reference[slot]);
context_view.entry_key=snapshot.pmm_arch_reference[slot].lookup_key;
```

中文伪代码：本段提供原payload范围不足时的独立架构输入。issue计划已经初始化并深复制CSR后，仅only-S2、Sv48x4、VPN高两位非零尝试保存；清两位后由公共existing查询按同CSR查精确/range映射；不存在不创建，保留valid为零。成功时深复制非零generation的payload与masked VPN。record提交、终态freeze逐层深复制，fence不替换。readonly先取最终snapshot并核对reset/ROB/dynamic/issue/replay，再要求valid与非零generation；复制同源CSR并改返回视图的masked VPN/key，`derive_tlb_request_fields`只用保存payload生成请求PPN。缺输入返回false，不能查当前live补齐。

正确性检查：三字段在record构造、reset、init_context和commit_context复制路径覆盖；snapshot构造创建独立entry，freeze再次深复制。issue查询只读原canonical owner；保存旧值不要求以后current generation相同。测试必须在issue前预定义masked输入，缺失是框架输入错误。

### 9.2 `observer_check_pmm_arch_reference()`

抽象功能描述：生产item已经形成实际请求参考后，checker以独立冻结masked输入比较架构可观察结果；原数据、原PA列表与提交次数保持原来源。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`。函数功能简析：独立判断G-stage异常、权限、PBMT、PMA/PMP及逐字节PA，将参考结论写回item。

```systemverilog
if(!ro.read_pmm_arch_reference_for_rm(item.uid,item.dynamic_epoch,
    '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},masked_vpn,context_v,reference)) return;
item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
if(reference.fault_effective_s2_gpf || force_s2 ||
   observer_stage_permission_fault(reference,context_v,store_access,0,item.hlvx))
    reference_exception[store_access?23:21]=1;
```

中文伪代码：本段核对掩码改变有效GPA高位后的真正结果。非目标item直接返回。首先读取预定义并冻结的masked输入，缺失时保留先前同范围helper结果；取得独立输入后先置MISSING并验证only-S2/Sv48x4参考。按照Store、HLVX、普通Load选择WRITE、READ_EXEC、READ；复用PBMT及权限helper独立判断GAF/GPF，正常翻译再要求可解析PPN。按size最多8字节由level/N和raw PPN独立生成PA，复用既有PMA只读接口判断完整访问AF及逐字节cacheable/mmio；随后按原优先级形成非对齐/访问错误。比较独立阶段cause；正常路径再比逐字节PA、PBMT、cacheable/uncache，证据相同为MATCH，不同为MISMATCH，无法证明保持MISSING。下游D错误沿原链检查。原数据游标已单次消费后，MISSING/MISMATCH仍报错并禁止本条整体PASS。

### 9.3 后页token并发与扫描边界

抽象功能描述：`bind_later_split_l2_request_token()`保存原token到当前后页实例的归属；`complete_later_split_l2_response()`与`cancel_later_split_l2_token()`完成或取消同token全部已关联记录。它们不接管responder的pending queue和计数。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。函数功能简析：固定两个active游标替代历史扫描，token bucket只保存最多两个record handle。

```systemverilog
for(int slot=0;slot<2;slot++) begin
    // 仅检查Load/Store各自active游标。
    later_split_owners_by_token[request_token].push_back(record);
end
foreach(later_split_owners_by_token[request_token][idx])
    complete_one_later_split_l2_response(/* 既有完整归属输入 */);
later_split_owners_by_token.delete(request_token);
```

中文伪代码：本段支持合法同VPN后页Load/Store并发。各真实后页split设置自己的buffer游标，PTW只查该UID。bind要求真实非零sample，固定检查Load/Store各一记录，核对WAIT_L2_TOKEN、VPN/stage、active/dynamic及冻结CSR完整key；匹配者保存同token/sample并转WAIT_RESPONSE，无匹配返回null，匹配返回首handle维持pending接口。complete以token桶和首handle验证原归属，再逐个调用原逐owner校验函数保存相同response来源，最后删桶；responder仍仅完成一次token。cancel逐个取消WAIT_RESPONSE后删桶；只有该handle仍是当前UID canonical记录才清游标，防旧replay token误清新attempt。reset清桶与游标，frozen历史不因运行期完成而扫描删除。

### 9.4 地址取消和LSQ admission遗漏

抽象功能描述：`observer_service_exception_addresses()`在原frame明确合法取消时结束独立地址责任；LSQ xaction白名单让原driver接纳已支持的13种操作。这两项分别补齐取消和真实issue入口。

源码位置：RM主文件及`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`。

```systemverilog
actual.cancel_reason inside {int'(memblock_backend_cycle_frame_t::CANCELLED_RESET),
                             int'(memblock_backend_cycle_frame_t::CANCELLED_COVERED)}
`define MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES [9'd0:9'd6], 9'd8, 9'd9, 9'd10, [9'd16:9'd22], 9'd29, 9'd30
`define MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES [9'd0:9'd3], 9'd7, 9'd12, 9'd13, 9'd14, [9'd16:9'd19]
```

中文伪代码：本段关闭合法取消被误判地址不匹配的问题，并补真实admission。地址服务先读取归属frame；等待则停在队首。若取消原因确为reset或covered，frame有效、required、未capture、非missing且冻结身份相符，只删除地址责任；任意其他取消或missing仍按错误处理。LSQ admission依据原LDU/STU类别分别允许新增9种HLV/HLVX和4种HSV；原driver继续核对fuType，不扩大MOU/AMO。真实HLVB探针在issue前暴露白名单遗漏，该修复归入第三项提交；最终必须重测全部13op及普通路径。


## 10. 最终review发现的跨页参考缺口与修复

问题：第9章初版的独立masked参考仅一个slot。only-S2/Sv48x4/PMM16原VPN高位非零，8字节从offsetFFC跨两张独立4K页时，后页即使预定义仍不在首页reference范围，生产checker会误报MISSING。修复把独立参考固定扩为两项，实际访问两页和唯一翻译服务职责保持原值。

### 两槽参考生产与查询

抽象功能描述：issue保存完整标量范围所需的两份已有参考，readonly按指定masked页返回该旧值，避免合法跨页误缺参考。

源码位置：`common_data_transaction.sv::capture_pmm_arch_reference_on_issue`、`memblock_tlb_entry.sv`、`memblock_rm_readonly_api.sv::read_pmm_arch_reference_for_rm`。

```systemverilog
size_bytes=1<<main_tr.fuOpType[1:0];
request_start={2'b0,record.vpn[35:0],main_tr.vaddr[11:0]};
request_end=request_start+50'(size_bytes-1);
record.pmm_arch_reference[slot].copy_from(entry);
if(snapshot.pmm_arch_vpn[slot]!=masked_request_vpn) continue;
```

中文伪代码：本段提供固定两页masked输入。先用公共behavior确认fuType/op为合法标量，再复用原size编码得到宽度；先将issue VPN高两位屏蔽并拼主表页内offset，再按50位加末字节，保留bit48新进位；仅跨页处理slot1，两项直接取masked首末VPN，并从同一CSR的existing canonical精确/range查询取得输入，不创建。三个原字段名改固定数组，record构造/reset/init/commit、snapshot构造/freeze逐项深复制；不能保留live handle。readonly要求原统一实例身份，然后最多两次按masked VPN匹配有效slot，复制该payload/CSR并解析请求PPN；缺项返回false，不做晚查。

### 逐页架构比较

抽象功能描述：`observer_check_pmm_arch_reference()`在跨页时消费各页自己的独立输入，并只对实际应消费的前缀负责。

源码位置：`env/src/memblock_rm/memblock_rm.sv`。真实摘要：

```systemverilog
if(!reference_loaded || loaded_vpn!=masked_vpn) begin
    if(!ro.read_pmm_arch_reference_for_rm(item.uid,item.dynamic_epoch,
        '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},
        masked_vpn,context_v,reference)) return;
    // 每页独立检查PBMT和G权限；首fault立即停止。
end
if(model_enabled && pa_count==item.size_bytes && !first_split_data_stop &&
   !first_pma_fault && !first_mmio) begin
    // 只有完整PA范围可用时查询完整size PMA。
end
```

中文伪代码：本段消除单首页权限/范围支配整次访问的问题。先保留原实现cause并识别首页D错误停止的有效PA前缀；逐byte生成masked VPN，首次或换页时按该VPN读取对应slot。读取成功先置MISSING，验证only-S2/Sv48x4输入，再按该页PBMT、GAF/GPF及R/W/X权限决定fault，首fault立即停止。正常字节由该页raw PPN/level/N解析PA，逐byte累计PBMT是否全PMA、normal cacheable、MMIO及uncache和PA差异。首页PMA错误/MMIO/不支持非对齐停止，不要求后页参考；完整PA全部可用才调用完整size PMA。最终按原优先级形成非对齐/AF并比较本阶段cause，成功路径再比PA/PBMT/缓存路由。缺首次参考时允许保留原payload真正覆盖两个地址的既有结论；后页实际需要却缺slot必须MISSING。D错误本身仍由原事实链负责。

验证边界：字段/接口静态检查及diff格式通过；最终两独立4K成功、后页fault/缺失及首fault不需后页等场景已在rm5 seed6111801自测通过，真实端到端矩阵与普通回归仍待结果。


## 11. 专项入口、fixture适配与验证版本边界

本节对应正式plan最后一组`IMPLEMENTATION_DELTA`。修改前只有原30组真实虚拟化矩阵，缺少直接使PMM10/11、关闭预AF和only-S2别名产生可区分结果的真实入口；旧ROB admission fixture也没有完整首页issue CSR，新tagged fixture只建map没有active状态。修改后补齐四真实入口、两处basicTest拓扑白名单与两处最小fixture前置条件，生产检查保持严格。

### 真实PMM入口

抽象功能描述：新增`memblock_pmm10_real_vseq`及三个派生入口复用完整CSR dedicated父生命周期，只替换该实例的CSR候选和四条主表输入，并在结束时核对覆盖形状。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_pmm_real_vseq.sv`，注册在`seq/seq_pkg.sv`、`seq/seq.f`及`tc/src/basicTest.sv`两处拓扑白名单。

```systemverilog
factory.set_inst_override_by_type(memblock_csr_initial_config_sequence::get_type(),
    initial_type(),{get_full_name(),".initial_csr_seq"});
factory.set_inst_override_by_type(memblock_main_dispatch_auto_build_main_table_base_sequence::get_type(),
    main_type(),{get_full_name(),".main_seq"});
```

中文伪代码：本段在测试范围选择输入而不复制调度。先运行父pre_body，再对两个准确factory实例路径替换测试子类。CSR子类覆盖PMM、模式、SPVP、MXR和非对齐开关，真实初始化与四拍生效继续用父类；main子类通过原manual import构造LD、HSVD、LD、HLVD，HSV STD明确冻结AABBCCDDEEFF0011。PMM10/11使用高tag不同而低地址相同的Bare输入；PMM00期望四条对应AF；only-S2派生先按同CSR正式publish原/掩码两个level0 canonical映射到同PPN，不写UID/snapshot/token。父initial结束后publish无yield，背景producer等下一次#1轮询才开放issue。父生命周期结束后逐条核对数量、opcode/imm/raw形状及terminal/cause；生产RM继续核数据与地址。两basicTest白名单确保新入口使用AUTO dedicated和单L2TLB生命周期。

### 两处软件自测前置条件

抽象功能描述：`build_later_split_pending_owner()`和`check_tagged_first_split()`分别补齐自己调用生产helper所必需的冻结CSR和活跃状态，不改变生产行为。

源码位置：`seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv`及`seq/virtual_sequence/soft_test/memblock_virtualization_rm_selftest_vseq.sv`。

```systemverilog
first_record.init_context(uid,{14'b0,data.main_table_by_uid[uid].vaddr[49:12]},
    0,0,issue_csr,sample_seq);
lookup_key=issue_csr.make_lookup_key({14'b0,split_va[49:12]},0);
saved_active=data.status_by_uid[0].active;data.status_by_uid[0].active=1;
```

中文伪代码：本段让旧自测状态满足真实生产合同。ROB admission先按主表页内几何建立Sv39/S身份issue CSR与首页record，再调用正式split/PTW observer，后页token key由同一CSR生成，保持active/dynamic/full key核对；返回必须是该UID/token的WAIT_RESPONSE。tagged自测在写active map前保存旧active，临时设1，正式observer检查完成后恢复map与active，避免半激活状态触发正确断言。没有生产lookup宽松分支，也没有绕过正式bind。

### 验证现状

- 旧rm3编译版本30组真实矩阵已通过；不是最终两槽/四PMM新入口通过证据。
- 同版RM selftest seed6111801及debug复现均因tagged fixture漏active失败，已登记W47 Buglist；修复已落码，原seed在rm5最终版95场景184检查自测通过；debug失败证据保留为修复前记录。
- soft CSV最终95项、真实CSV现34项；最终basicTest清单为93配置组合/50入口。新场景包括两独立4K、后页权限/PBMT/fault/缺失、首fault不需后页和首页D错误前缀。
- rm5同版编译和RM selftest已通过；旧ROB admission、真实PMM及全部basicTest结果待主代理日志，当前不标整体验收PASS。


## 12. B1/B2/B3与真实PMM bootstrap最终修复


### 修复与抽象功能说明

本节覆盖独立复查发现的三个架构参考计算缺口及真实PMM测试启动遗漏；是最终源码合同，替代前述较早的简化内部步骤。修复只影响已有固定两槽参考/checker及测试主表初始化，未新增walker、TLB、ready owner或RTL修改。

| 编号 | 原缺口 | 最终实现与边界 |
|---|---|---|
| B1 | 首字节独立PMA/PMP AF使完整size查询跳过，原累计AF可能保持零 | 每个可信PA字节立即OR累计AF，完整size查询存在时继续OR；不能覆盖首fault；翻译fault仍不查无效PA的PMA |
| B2 | 后页NC与翻译fault优先级简化，且把NC/IO混同 | misaligned Load后页PBMT=NC独立覆盖为bit4；IO的mmio受翻译异常屏蔽，IO+GPF/GAF保留翻译fault；成功翻译的PMA-MMIO由独立模型证明且无AF才可委托非对齐 |
| B3 | 原地址先加末字节再清高位，会丢失masked地址的新bit48进位 | 先构成零扩展48位masked首地址，再以50位加size-1，直接取首末VPN，不再次mask末地址 |
| PMM测试bootstrap | 测试专用build_main_table只import，遗漏正式control runtime reset握手 | 先ensure_dispatch_runtime_helpers，import后按uses_control_barrier_topology调用既有initialize_control_runtime_bootstrap；不手置ready/done、不删四拍确认、不多建随机表 |

`observer_check_pmm_arch_reference()`最终抽象职责：从issue时两份独立masked输入重建实际消费范围的架构结果，区分翻译fault、PMA/PMP、NC和IO的条件，并与原实现参考结果比较。

源码支撑位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，同名函数。

```systemverilog
if(i>0 && !store_access && misaligned && reference.s2_entry_pbmt==1) begin
    reference_exception=0;reference_exception[4]=1;break;
end
pma_fault |= byte_access_fault;
if(i>0 && !store_access && misaligned && byte_mmio && !pma_fault) begin
    reference_exception=0;reference_exception[4]=1;break;
end
```

详细中文伪代码：本函数先限制到only-S2高位差异责任，保存原实现cause；若首页独立D错误已停止，按PA有效前缀限制字节数。每个masked页入口从readonly固定两槽选择同CSR旧输入，先独立形成该页GAF/GPF/PBMT/权限fault；后页misaligned Load的NC位独立存在，因此NC优先转bit4后停止。其他翻译fault先终止，不查无效PA的PMA；IO在此不覆盖翻译fault。正常PA逐字节查询只读PMA/PMP，将byte AF立即OR累计；首页另保存first_pma_fault/first_mmio，成功后页只有独立证明PMA-MMIO且无已知AF才转bit4。完整size PA全部可用且门禁允许时再查询完整size AF，并OR进已有结果，不能覆盖逐字节已证明AF。最后按原非对齐/访问异常规则合并cause，成功访问另比较PA和缓存路由；缺实际需要输入保持MISSING。各helper只解释冻结输入，不采DUT值作为期望。

`capture_pmm_arch_reference_on_issue()`最终抽象职责：issue时按masked有效地址范围保存最多两份已有canonical映射，确保末字节进位和checker's effective_fullva+byte一致。

源码支撑位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
request_start={2'b0,record.vpn[35:0],main_tr.vaddr[11:0]};
request_end=request_start+50'(size_bytes-1);
masked_vpn=slot==0?request_start[49:12]:request_end[49:12];
```

详细中文伪代码：复用公共标量分类和原size编码；先从原VPN低36位、页内offset组成零扩展masked首地址，再做50位末字节加法。首页和末页不同才保存第二槽，每槽直接取masked VPN，保留新产生的bit48。按同一冻结CSR只读existing精确/range输入，缺输入保持valid=0；非零generation的payload深复制。readonly按请求masked VPN选择固定两项，record与snapshot生命周期保持前述全链合同。

`memblock_pmm_main_sequence::build_main_table()`最终抽象职责：专项构造四条主表后完成生产控制服务的正式启动前置，后续真实CSR确认与流量开放仍归父生命周期。

源码支撑位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_pmm_real_vseq.sv`。

```systemverilog
ensure_dispatch_runtime_helpers();
// 构造四条确定输入。
import_manual_main_table();
if(memblock_sync_pkg::uses_control_barrier_topology())
    control_barrier_service.initialize_control_runtime_bootstrap();
```

详细中文伪代码：进入测试build时建立父类运行期helper；构造四条既定访存并导入，保持主表数量/opcode/STD不变；只有既有控制拓扑开启才调用幂等bootstrap，正式清action并请求当前epoch的真实多路driver/monitor ack。initial CSR继续等main ready/runtime ready，随后真实驱动PMP/PMA、等待四个连续CSR样本再置initial_done。未绕过等待，未伪造ready/done，未扩超时。初次PMM10 seed6111900超时发生在任何真实issue之前，责任已登记W47测试框架缺陷，修复后的四真实PMM已通过最终rm5复测（seed6111900..6111903）。


### 最终同版验证快照（2026-11-17，rm5）

- `virt_rm5_20261117`最终编译完成；seed6111801、`basicTest ts=memblock_virtualization_rm_selftest_vseq cfg=default`的95个场景通过，共184个检查，日志末有`TEST CASE PASSED checks=184`及`UVM_ERROR=0/UVM_FATAL=0`。
- 权威日志：`/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/rm5_selftest/status/seed_6111801.final.log`。报告有2条被catch的预期UVM_ERROR，来自故意错误地址sample/GPA的负例，测试分别要求恰好捕获一次；它们不属于被忽略的未知错误。
- 该同版自测覆盖固定两槽跨4K、首fault/首页D错误停止、后页NC+GPF优先级、独立对齐Load/Store PMP AF与差异，以及mask后跨bit48进位捕获。IO+GPF门控本轮为源码独立核对，未另加独立动态断言，不宣称该子组合已动态通过。
- 旧rm3的30组真实Guest矩阵通过仅是早期版本证据；最终rm5的30 Guest矩阵和4组真实PMM入口均已通过（seed6111810..6111839、6111900..6111903）。旧ROB admission入口、整轮93配置组合/50入口basicTest最终结果等待集成方统计；不得据soft PASS推定这些结果。
- 文档/源码格式检查通过；最终plan继续留在undo，不提前归档或提交。全任务完成仍需真实专项、普通回归和已登记缺陷原seed闭环。

## 13. 最终差异逐文件覆盖与普通基线

本节补齐运输、连接与注册的逐文件责任；前述各函数的抽象职责和详细中文伪代码仍是核心逻辑依据。

| 源码文件 | 原逻辑与本轮修改 | 检查与边界 |
|---|---|---|
| `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv` | 原采样仅有Load后页；新增13个Store旁路字段同一sample复制到不可变返回item，不驱动内部Store信号。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv` | 新增Store split/STU0/PTW旁路只读输入，driver及monitor clocking方向一致。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv` | 新增13字段保存同拍Store来源，字段类型/宽度逐项对应interface。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv` | 按原LDU/STU类别扩HLV/HLVX/HSV白名单，fuType门禁仍由driver检查。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv` | 新增特殊操作/权限/PMM/地址责任，仍按原commit游标单次比较和提交overlay，详见第2至12章。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv` | 新增PMM纯helper与item元数据，保留模型与物理backing；Bare几何仅新标量路径使用request地址。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv` | 同一send_l2tlb_cycle样本调用Store observer；真实response完成时把原token及sample传入首页记录，不重新采样。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv` | 旧fixture补首页CSR及WAITING索引正式登记；不放宽生产redirect检查。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 固定两页冻结参考、两个独立masked参考、Store后页及token双owner归属；所有权和恢复见第3/9/10/12章。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv` | 原normal/fault四个rob_commit入口在写提交及cancel前冻结参考，异常地址actual仍由原backend frame保存。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv` | LDA记录issue代次并按ROB局部索引，精确实例筛选不扫描全history。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv` | 导出值型快照、四态地址actual及独立masked输入，统一身份校验，禁止生产终态晚查current。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv` | 增加来源token/sample、独立masked两槽及RM快照对象，构造/reset/init/commit深复制闭环。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/seq.f` | 纳入两个新测试文件，顺序满足父类依赖。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/seq/seq_pkg.sv` | 纳入真实PMM sequence；RM自测依赖env包所以由tc_pkg纳入。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv` | 将V2真实Store旁路wire接到interface，50bit地址按bit49扩展；关闭连接时置零，不新增ready owner。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/tc/src/basicTest.sv` | 四个真实PMM入口加入既有L2TLB/控制生命周期白名单，运输与ready仍复用父拓扑。 | 已逐项对照diff；最终动态证据按下表记录。 |
| `mem_ut/ver/ut/memblock/tc/tc_pkg.sv` | 在env包导入后include RM自测，避免循环package依赖。 | 已逐项对照diff；最终动态证据按下表记录。 |

新增文件 `memblock_pmm_real_vseq.sv` 负责四个确定的真实输入入口；新增 `memblock_virtualization_rm_selftest_vseq.sv` 负责95个独立软件场景及184项断言。二者的注册、参数、输入与检查合同分别见第11/12章和两份CSV。当前git status所有SV/filelist修改均属于第三方案；没有RTL、构建产物或未知无关改动。

### 运行结果补充

- 最终rm5的30个Guest组合全部通过：seed6111810..6111839，结果目录 `/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/virt_rm5_20261117_real_00_real_hlvb_spvp0` 至 `_29_real_hlv_s1_af`；13op各SPVP0/1及PF/GPF/AF均包含，全部results.tsv为PASS/EXIT_CONFIRMED、rc0、uvm_count0，summary远端未知0。
- 四个真实PMM全部通过：seed6111900..6111903，结果目录 `/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/virt_rm5_20261117_pmm_0` 至 `_3`。only-S2别名日志实际产生两条PMM_ADDRESS_SEMANTIC_MATCH，HSVD冻结STD AABBCCDDEEFF0011被后续普通LD/HLVD读取；PMM00四条异常均完成S+2地址责任。
- 空vseq基线seed6111997和普通dispatch smoke seed6111998通过，结果目录 `/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/rm5_baseline_0`、`rm5_baseline_1`。
- ROB admission原seed6111999失败于测试fixture的WAITING索引遗漏，带波形同seed复现，已完成独立责任和最小方案review并只增加正式索引API调用；重新编译复测进行中。此时尚不归档plan或宣称完整回归通过。

### ROB fixture补充细节

抽象功能描述：`build_later_split_pending_owner()`为旧redirect自测建立完整首页控制来源及正式WAITING归属，允许原redirect计划核验保持有效；不改变生产issue或DUT激励。

```systemverilog
data.uid_tlb_record_by_uid[uid]=first_record;
data.add_waiting_uid_to_index(uid,first_record);
```

中文伪代码：创建首页CSR及WAITING记录后，先发布UID记录，再通过正式helper登记唯一索引成员；helper校验UID/key/state、清理过期成员并拒绝重复。之后复用原split/PTW observer和token绑定，redirect通过原cancel入口删除旧索引；新代再按同合同建立。该细节是执行中补充，已经写入plan的IMPLEMENTATION_DELTA及W47 Buglist；不将其冒充执行前原计划。

## 最终提交验收记录

第三方案源码与配套文档已完成，主代理独立检查diff、关键调用链及冻结/恢复边界，无已知blocker。独立整体review为 `/nfs/home/lixiangrui/.tmp/rm_final_independent_review.md`，B1/B2/B3独立复核及ROB索引最小修复review均通过。

验证：95场景184项RM检查、34个真实虚拟化/PMM组合、空vseq与普通dispatch smoke全部通过。ROB fixture索引修复后已重编译，同原seed6111999复测通过，结果目录 `/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/rm5_rob_retest`；原四个redirect迟到completion场景通过。最后重编仅增加旧软件fixture的一次正式索引登记，不改变生产RM、真实专项输入或DUT。所有结果已核验稳定summary/results、UVM_ERROR/FATAL=0和remote_unknown_jobs=0。

plan按实现完成归入do，implementation review保留undo。用户要求的93组合/50入口全部basicTest将用同版simv继续执行，结果单独更新；此处不把尚未运行的回归标成通过。IO+GPF目前仅有静态独立核验，不宣称独立动态场景覆盖。未修改RTL，未push。
