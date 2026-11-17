# V2虚拟化测试框架实现review（2026-11-17）

关联plan：`AI_DOC/plan/test_framework/plan/do/memblock_virtualization_framework_support_formal_plan_20261117.md`。
范围：第二项框架实现及配套框架自测，基于第一项已提交的后端周期帧与异常地址采样。不修改RTL，不提前实现第三项RM/PMM权限算法。

## 1. 术语与抽象功能说明

| 术语 | 当前含义 | 源码落点 | 例子 |
|---|---|---|---|
| Guest memory | 联合fuType/opcode识别的HLV/HLVX/HSV特殊访存 | `memblock_op_behavior_util` | MOU同值编码不会变成HLV |
| target VA／normalize | 构建期已确定地址／转换为所选操作可表达的源操作数 | dispatch自动地址模板 | HLV把完整地址放src_0，imm=0 |
| STA／STD／override | Store地址／Store数据／主表显式数据入口 | issue路由及slot0 | HSV地址与0xAABB数据可不同 |
| SPVP／dmode／dvirt | 特殊访问U/S身份／普通有效访存身份／有效数据虚拟化状态 | runtime CSR | 特殊PMP按SPVP，普通按dmode |
| PMM | 五组指针掩码控制模式；本项只配置运输 | raw/runtime五PMM字段 | PMM-only手工目标同样传播确认 |
| target／clean／changed | 完整候选配置／净化稳定配置／单拍更新协议位 | initial/dynamic CSR sequence | changed=1111后保持clean=0000 |
| snapshot／sample | 一份已观察配置／实际DUT公共采样编号 | runtime/full monitor发布 | 反复读同sample不算四拍 |
| owner／epoch | 某次控制动作归属／执行及reset代次 | control_owner、dynamic/reset epoch | reset后旧action不沿用新编号 |
| barrier／drain | 阻止年轻admission的屏障／等待旧活动收口 | active_control_barrier | 旧SQ未退出时CSR不产生副作用 |
| candidate／committed | 维护完成待service确认的目标／已确认配置基线 | common CSR配置状态 | 下一随机只能从committed复制 |
| scope／frozen type | 主表固定失效范围／一次选定后保持的fence类型 | sfence action | worker不随机地址和ID |
| C0／C4／event | fence真实观察／既有延迟生效／单笔事件身份 | fence观察槽 | 同owner/event/reset才能完成 |
| live entry／responder token | 可被fence失效的逻辑映射／已接纳翻译请求 | common TLB表／L2TLB sequence | 删除映射不篡改冻结请求快照 |
| prefix／cursor／active map | 连续进度／按序位置／在途资源索引 | dispatch公共状态 | 排空判断不全主表扫描 |
| legacy | 未开启special CSR场景的原控制配置分支 | run_legacy | ASID加一，不建special candidate |

## 2. 结论、验证与边界

独立源码review发现并已修复一个HS维护上下文阻塞：target.dvirt=1时普通SFENCE实际属于VS，不能预约为HS。最终实现仅在目标Host上发HS，并在每次Guest返回非Bare Host时补全范围HS维护；屏障持续到C4，不新增dirty镜像。复查四种dvirt组合及旧新VMID维护顺序，当前未发现已知源码blocker。

主代理已完成本项完整编译、框架专项和真实DUT基础专项，结果见末章。本文不把编译通过或框架helper断言当成13条指令端到端结果正确的证据。第三项仍需实现RM的符号扩展、HLVX权限、两阶段权限及PMM/checker，不关闭现有检查代替支持。

已执行只读检查：13个常量逐项对本地`package.scala`一致；四个生产PMP capture调用均使用有效身份helper；92项full pack与monitor字段及顺序一致；代码及本文修改范围`git diff --check`应在交付时再次执行。

## 3. 操作常量、公共权重与分类

修改前：INT/FP共用普通load候选；特殊访存不在普通LSQ行为识别里，单看数值可能和MOU混淆。权重累计旧实现采用32位。
修改后：增加9个HLV/HLVX和4个HSV常量及默认0权重，沿原LOAD/STORE类别；联合fuType分类，FP保持普通集合，64位累计/采样。五fence权重默认1/1/1/0/0，SPVP初始默认0，动态修改默认关闭。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`、`memblock_op_behavior_util.sv`；参数链为`env/plus.sv`→`seq/base_seq_help/seq_csr_common.sv`→`seq/plus_cfg/default.cfg`。

抽象功能描述：`is_guest_read/write/is_guest_memory/is_hlvx()`是无状态联合分类；`derive_op_behavior()`把特殊访存接入现有资源与路由，不创建第二种LSQ行为。

```systemverilog
return fuType == MEMBLOCK_FUTYPE_STU && fuOpType inside {
    MEMBLOCK_LSUOP_HSVB, MEMBLOCK_LSUOP_HSVH, MEMBLOCK_LSUOP_HSVW, MEMBLOCK_LSUOP_HSVD};
```

中文文字伪代码：先限定功能单元STU，再对四个HSV白名单匹配；读侧同理限定LDU与9个HLV/HLVX。derive先分fuType，再允许这些opcode形成普通STORE或LOAD kind，继续分配单个SQ/LQ element。MOU进入原atomic分支，不被特殊编码捕获。

抽象功能描述：`check_virtualization_weight_cfg()`在冻结参数时拒绝空候选；`get_fuop_weight()`按实际类别提供单项权重，不在运行期重新解析plusarg。

```systemverilog
if (op_class_fp_load_wt != 0 && ordinary_load_total == 0)
    `uvm_fatal("SEQ_CSR_CFG", "FP_LOAD has no positive ordinary load weight; HLV is integer-only")
```

中文文字伪代码：普通load、特殊load、store及fence各自逐项先扩宽再求和。启用INT时普通与特殊总和必须非零；启用FP必须普通load非零，不能被HLV掩盖；启用STORE或自动fence时其合法组必须非零。getter仅INT和STORE分支返回特殊权重，FP特殊编码返回0。新增参数均经定义、load、冻结及default四段同步，没有新增interface宽度的runtime镜像。

正确性检查：13常量与本地V2编码一致；HLVX.HU/WU低两位宽度分别2/4B；新增特殊权重均0保持默认分布。`memblock_csr_sequence_cfg_t`同时包含初始/动态SPVP权重与开关，action包含scope/addr/id/type_frozen，payload包含flushPipe。

## 4. 构建期候选、地址出口与手工输入

修改前：最小INT/FP模板共用load helper，边界模板产生负imm分解，复制参考地址可能把普通imm带入特殊操作。
修改后：最小模板按真实op_class选择；INT追加9条，STORE追加4条，FP保持原7条。目标VA先确定再规范表达，手工入口只验证不修复。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，`apply_minimal_op_template()`、`enumerate_fuoptypes()`、`derive_size_bytes()`、`weighted_pick_index()`。

抽象功能描述：候选与尺寸helper在构建期决定合法操作和访问宽度；权重选择只消费冻结单项权重，不维护状态表或DUT期望。

```systemverilog
tr.fuOpType = pick_fuop_by_weight(tr.op_class,"load_template");
// 特殊INT/STORE尺寸按已确认编码低两位派生
return 1 << fuOpType[1:0];
```

中文文字伪代码：INT和FP虽然共享模板代码，但传入各自类别给候选枚举；只有INT追加特殊读取。STORE追加HSV。derive先确认类别与特殊白名单再取低两位宽度，普通及AMO保持原实现。

抽象功能描述：`weighted_pick_index()`从当前小候选队列选择一项，不将累计值截为32位。

```systemverilog
threshold = (64'd0-total) % total;
do pick = {$urandom(),$urandom()}; while (pick < threshold);
pick = pick % total;
```

中文文字伪代码：64位累加所有权重，总和0时fatal。两个随机字组成64位值，拒绝模偏区间后取总和余数，再按64位累计区间找到下标。没有全主表扫描或新增生命周期状态。

抽象功能描述：`normalize_guest_memory_address()`在自动模板确定目标后转换操作数表达；`fixup_after_addr_reuse()`把参考最终VA交给同一转换。

```systemverilog
if (memblock_op_behavior_util::is_guest_memory(tr.fuType,tr.fuOpType)) begin
    tr.src_0 = target_va;
    tr.imm = 0;
end
tr.update_vaddr();
```

中文文字伪代码：联合分类特殊操作时把完整target写src_0、清imm，普通操作保留原基址立即数分解，再重算vaddr。合法模板、边界模板、参考复制均走这里；无参考fallback重新运行最终类别模板；跨度收口只改为普通默认opcode，保持目标VA。无人调用的公共`set_transaction_ls_kind()`也先保存VA再规范，避免未来子类遗漏。

抽象功能描述：`validate_main_table_entry()`是主表最终合法性入口，拒绝不可表达的手工指令；不把预期异常地址修成成功地址。

```systemverilog
if (memblock_op_behavior_util::is_guest_memory(tr.fuType,tr.fuOpType) &&
    (tr.imm != 0 || tr.op_class == MEMBLOCK_OP_CLASS_FP_LOAD))
    `uvm_fatal(get_type_name(), "Guest memory requires integer route and imm=0")
```

中文文字伪代码：原derived fields、ROB、类别、行为和LSQ检查照常执行。特殊访存带非零imm或FP类别立即报错；手工导入在post_manual_config后调用此最终入口，不调用自动normalize。INT类别决定原rfWen，HLV/HLVX不会产生fpWen。

正确性检查：合法、boundary负imm、参考普通地址、无参考fallback、宽度收口均保持最终VA且特殊imm=0；AMO地址策略独立。构建扫描只遍历当前候选或有界recent窗口，未增加O(N²)全表扫描。

## 5. HSV数据冻结与真实issue身份

修改前：普通Store数据可沿src_0 fallback，特殊地址同占src_0，缺少明确最终冻结；PMP四入口直接传dmode。
修改后：HSV最终主表接纳时冻结slot0，issue及request补登记统一按该次CSR的SPVP选择身份；普通仍dmode。

源码位置：`seq/base_seq_help/memblock_op_behavior_util.sv`、`common_data_transaction.sv`（位于`mem_ut/ver/ut/memblock`）。

抽象功能描述：`freeze_guest_std_operand()`在`set_main_transaction()`最终接纳时保存可重发的数据；不会重抽数据或覆盖显式用户输入。

```systemverilog
if (!tr.std_data_override_valid[0]) begin
    tr.std_data_override_valid[0] = 1;
    tr.std_data_override[0] = tr.src_0;
end
```

中文文字伪代码：只对联合分类HSV执行。slot0已有效则保留；否则沿既有普通Store默认入口把src_0当数据并冻结到独立slot0。原atomic冻结先执行且按AMO类别分隔。后续STD assigner和只读主表复用同一override，因此地址变动、重试及replay不重新生成数据。当前默认数据恰好等地址合法，显式目标数据必须在override入口保存。

抽象功能描述：`effective_data_privilege()`把当前主表操作和实际issue/request快照转换为既有PMP模型的有效U/S/M实参，不重建PMP保护算法。

```systemverilog
return is_hypervisor_tlb_inst(tr) ?
    (snapshot.priv_spvp ? 2'd1 : 2'd0) : snapshot.priv_dmode;
```

中文文字伪代码：联合分类为特殊则SPVP=1返回S、0返回U；普通直接返回dmode。`register_uid_tlb_record_on_issue()`在真实issue冻结CSR并登记TLB/PMP，即使hit无request也有身份；该函数回放前后两次plan，以及`mark_waiting_uid_records_on_request_fire()`、`resolve_and_plan_pbmt_rf_request_fire()`共四处生产capture调用均替换实参。快照来源沿原各路径，不在fault head拿最新CSR覆盖原上下文。

正确性检查：AMO同数值联合分类返回非Guest；普通LD即使SPVP不同仍用dmode。异常地址沿第一项frame中冻结issue CSR及动态实例，资源回收仍走原redirect流程；本项不从异常输出反推身份或建立平行异常采样器。

## 6. CSR完整字段、SPVP求解与手工目标

修改前：SPVP未参与初始/动态求解和must-change，raw/runtime缺少五PMM，比较不覆盖root与PMM-only目标，手工PMM-only可能被随机必变约束阻断。
修改后：初始及动态SPVP受公共参数控制，动态禁改时保持；target从committed复制，PMM保持；manual hook跳过随机，统一输入校验及实际变化检查。

源码位置：`seq/base_seq_help/memblock_csr_randomizer.sv`、`memblock_csr_config_state.sv`、`mmu_csr_runtime_state.sv`、`common/memblock_common/src/memblock_sync_pkg.sv`及CSR monitor。

抽象功能描述：`configure()`/`c_spvp`/`apply_to_transaction()`只构造候选，保持关闭组及未随机PMM，不驱动接口或宣告生效。

```systemverilog
if (!dynamic_mode)
    priv_spvp dist {0 := cfg.init_spvp_0_wt, 1 := cfg.init_spvp_1_wt};
else if (cfg.change_spvp_enable)
    priv_spvp dist {0 := cfg.change_spvp_0_wt, 1 := cfg.change_spvp_1_wt};
else priv_spvp == cur_priv_spvp;
```

中文文字伪代码：初始按1/0默认选择0；动态关闭保持committed，开启按1/1默认权重求解，并把SPVP不同加入动态must-change。没有非当前合法解时一次randomize失败报错，不无限重抽。apply写SPVP；目标原从committed复制，PMM未被静态默认重置；mode相同的root也保留当前值，避免权限-only改掉手工root。原`log_selected_profile()`恢复动态调用并增加SPVP选择/权重审计。

抽象功能描述：`differs_from()`、`make_runtime_payload()`和`check_target()`分别负责真实变化比较、导出完整runtime目标和基础输入检查，不选择替代值。

```systemverilog
if (tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg == 2'b01 ||
    tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg == 2'b01 ||
    tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg == 2'b01 ||
    tr.io_ooo_to_mem_tlbCsr_pmm_hstatus == 2'b01 ||
    tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg == 2'b01)
    `uvm_fatal("CSR_CONFIG", "CSR PMM target uses reserved encoding 01")
```

中文文字伪代码：比较增加三root、SPVP、五PMM及PBMTE；导出五PMM到raw目标。check拒绝非法mode/privilege、VMID高位、HGATP未对齐和PMM01，不把合法0当缺失。monitor逐拍填五PMM，raw差异函数也比较它们；runtime reset/update_from_csr_ctrl/update_from_raw/copy全部保持同一字段集，snapshot仍是唯一观察真源。

抽象功能描述：initial `customize_target(target)`及dynamic `use_manual_target()/customize_target(committed,target)`给派生sequence确定完整目标入口，仍由公共运输链执行副作用。

```systemverilog
if (!use_manual_target()) begin
    randomizer.configure(cfg,1'b1,committed,current_region);
    // 原单次求解与apply
end
customize_target(committed,target);
```

中文文字伪代码：initial随机后调用空默认hook；dynamic随机模式沿原must-change，manual模式直接复制committed并保留current_region，不依赖随机开关。hook返回后检查合法编码及CSR或region至少一项变化，再进入原owner运输；hook不自己写接口或修改runtime。PMP-only变化通过region比较接受。

正确性检查：完整full pack与monitor的92字段及顺序匹配；SPVP-only和五PMM-only均能识别；raw/runtime补齐作为本项运输依赖，RM的权限计算留第三项。

## 7. 初始传播、动态排空与维护步骤

修改前：观察一份clean full即可返回；Guest动态路径副作用前直接拒绝；映射维护只HS helper。
修改后：初始等PMP模型与四个真实相邻clean；动态先旧访问排空，统一target与fence步骤，确认changed事件及四拍传播后才进入目标fence。

源码位置：`seq/base_seq/memblock_csr_initial_config_sequence.sv`、`memblock_dynamic_csr_change_sequence.sv`及`seq/base_seq_help/common_data_transaction.sv`。

抽象功能描述：initial `body()/wait_for_full_snapshot()`负责普通producer开放前完成配置；dynamic `wait_target_propagation()`负责单action观察边界，两者都只消费真实monitor样本。

```systemverilog
if (observed.payload == payload)
    stable_count = observed.sample_seq == last_sample + 1 ? stable_count + 1 : 1;
else stable_count = 0;
last_sample = observed.sample_seq;
if (stable_count >= 4) return;
```

中文文字伪代码：只处理drive后的不同sample；重复latest不计数，相邻匹配累加，跳号重新从1，不匹配清0。initial先完整level、PMP beats及model确认，再等四拍后发布done。dynamic另要求新runtime序号、target全字段匹配和四changed齐全；changed证据与clean稳定各自确认，不能只靠driver完成或固定等待宣布生效。

抽象功能描述：`csr_old_context_drained()`在CSR副作用前判断旧运行状态是否收口；它是只读谓词，不暂停主service或清理资源。

```systemverilog
return uid_by_active_rob.num() == 1 &&
       uid_by_active_rob.exists(rob_order_util::rob_to_map_key(get_main_transaction(owner.uid).get_rob_key())) &&
       uid_by_lq.num() == 0 && uid_by_sq.num() == 0 &&
       load_issue_q.size() == 0 && sta_issue_q.size() == 0 && std_issue_q.size() == 0 &&
       backend_frame == null && !backend_redirect_reserved && !backend_fault_prepared &&
       backend_lsq_transport_q.size() == 0 && backend_redirect_transport_q.size() == 0 &&
       !backend_fault_address_pending() && !active_redirect.valid && !flush_in_progress &&
       !cancel_reconcile_pending() && !has_pending_redirect_drive();
```

中文文字伪代码：先要求原barrier owner有效。只允许控制UID留在active ROB，旧LQ/SQ、issue队列、prepared或未应用frame、运输队列、异常地址目标、redirect/flush/cancel均为空。等待在独立child进行，主service仍先处理WB、frame采样、deq/cancel、redirect恢复与flushSb，因此无新增互相等待环。查询使用map大小和有界queue，不扫完整历史表。

抽象功能描述：`build_mapping_maintenance_steps()`构造有限串行副作用，`body()`逐步执行，全部成功才stage；错误时不公布candidate。

```systemverilog
if (guest_changed) maintenance_steps.push_back(FENCE_VS);
maintenance_steps.push_back(WRITE_TARGET);
if (!target.io_ooo_to_mem_tlbCsr_priv_virt &&
    (hs_mapping_fence_required(old_level,target) ||
     (old_level.io_ooo_to_mem_tlbCsr_priv_virt &&
      (old_level.io_ooo_to_mem_tlbCsr_satp_mode != 0 ||
       target.io_ooo_to_mem_tlbCsr_satp_mode != 0))))
    maintenance_steps.push_back(FENCE_HS);
if (guest_changed) maintenance_steps.push_back(FENCE_VS);
if (guest_changed || pmm_width) maintenance_steps.push_back(FENCE_G);
```

中文文字伪代码：VSATP模式/root/ASID、HGATP模式/root/VMID或dvirt改变，且至少一侧Guest翻译非Bare，则先旧VMID VVMA。唯一WRITE_TARGET始终先完整CSR四changed，再可选PMP beats与模型观察，最后传播确认。目标Host执行HS根变化维护，或从Guest返回非Bare Host补维护；目标Guest暂不可达HS，延后到返回Host，避免把VS观察当HS。随后Guest变化执行新VMID VVMA和全VMID GVMA。henvcfg进入／离开PMLEN16且任一HGATP=Sv48x4也要求GVMA，合并为一次。SPVP、权限、普通PMM变化只有WRITE_TARGET，不清全部软件映射。

正确性检查：四种dvirt组合与无root再次变化的Guest返回Host均被专项helper覆盖；同一配置期间不会提前放行年轻访问。PMP model仍由monitor写FIFO回放，sequence不直接改model。

## 8. Fence运输、raw字段与观察槽

修改前：worker随机三类及范围；raw匹配忽略flushPipe，无法区分HFENCE/HINVAL；mapping预约只允许HS。
修改后：五权重一次选择，主表四scope确定运输；flushPipe贯穿raw和matcher；CSR维护允许合法HS/VS/G全范围，复用同owner单槽。

源码位置：`seq/base_seq/memblock_sfence_control_base_sequence.sv`、`seq/base_seq_help/main_control_transaction.sv`、`memblock_control_barrier_service.sv`、`common_data_transaction.sv`、`dispatch_monitor_event_adapter.sv`、`agent/fence_agent_agent/src/fence_agent_agent_monitor.sv`及sync/types。

抽象功能描述：`main_control_transaction::do_copy()/validate_main_transaction()`保留并验证手工scope；barrier的`enqueue_sfence_action()`只转送主表原始目标，不决定随机类型。

```systemverilog
action.scope = data.get_main_transaction(status.control_owner.uid).sfence_scope;
action.addr = data.get_main_transaction(status.control_owner.uid).sfence_addr;
action.id = data.get_main_transaction(status.control_owner.uid).sfence_id;
```

中文文字伪代码：主表scope不声明rand，构造默认全全和0目标；手工post_manual_config保留字段，非法枚举拒绝。新增do_copy显式复制全部既有主表字段、STD数组与新scope/addr/id，do_print打印目标。barrier完成原flushSb流程后复制三字段到action，自动仍全地址全ID。

抽象功能描述：`configure_sfence_control_xaction()`消费scope与五权重，首次冻结类型和payload，重试不再抽样。

```systemverilog
if (!std::randomize(pick) with { pick < total_weight; })
    `uvm_fatal(get_type_name(), "SFence weighted type selection failed")
action.expected_fence.hv = selected_kind inside {1,3};
action.expected_fence.hg = selected_kind inside {2,4};
action.expected_fence.flushPipe = selected_kind < 3;
```

中文文字伪代码：五项先扩为64位累计，64位变量在总区间求解；顺序SFENCE、HFENCE.VVMA/GVMA、HINVAL.VVMA/GVMA。scope确定rs1/rs2忽略位，addr/id直接原值，合法0保留；GVMA指定VMID超14位报错，GPA>>2不再位移。类型冻结后再次configure保留相同hv/hg/flushPipe。

抽象功能描述：`decode_raw_sfence()/control_sfence_payload_matches()`解释同sample上下文并比较实际语义，adapter继续独占C4失效，不因新增HINVAL复制失效器。

```systemverilog
payload.flushPipe = raw.flushPipe;
// 同源语义比较新增一项
expected.flushPipe == observed.flushPipe
```

中文文字伪代码：monitor采完整位到raw；decode保留flushPipe，按hg优先G、hv或同拍virt解释VS、否则HS。matcher要求范围、原始目标、hv/hg、flushPipe及阶段一致。adapter日志同时打印完整payload；失效器仍按原冻结stage、ASID/VMID匹配live entry，不把HINVAL另当数据清理操作。

抽象功能描述：`complete_mapping_fence()/arm_csr_mapping_fence()/finish_csr_mapping_fence()`在单CSR action中顺序复用真实fence和观察槽；不消费五随机权重、不产生新owner。

```systemverilog
if (!data.arm_csr_mapping_fence(action.owner, payload)) return;
// 真实运输，后续必须等同event生效
if (data.get_control_sfence_c0_observation(action.owner,c0) &&
    data.finish_csr_mapping_fence(action.owner,c0.lifecycle_event_seq,reset_epoch,effective)) break;
```

中文文字伪代码：arm只接受全范围、flushPipe=1和合法单HS/VS/G组合，并确认原owner/pending/reset与空槽。发送确定payload后仍等待实际C0和相同event/reset的C4；finish消费两观察槽并清expected/armed/event下界，下一步才可预约。阻塞运输和等待均检查原owner及两reset域，失败不stage/sendover，不把新的epoch重新绑定到旧action。

## 9. Driver、legacy与barrier提交

修改前：clean hold只受special标志驱动；旧legacy字段重建未复制PMM；barrier可在未应用后端frame存在时起动作。
修改后：完整四changed legacy item同样保持净化level；legacy补字段与四拍传播；barrier先等frame及必需地址事件收口。原candidate提交所有权未迁移。

源码位置：CSR driver、`memblock_dynamic_csr_change_sequence::run_legacy()`、`memblock_control_barrier_service::service_active_control_barrier()`及common候选状态。

抽象功能描述：driver `send_pkt()/capture_csr_level_hold()/drive_idle()`运输并保持完整配置电平，所有协议脉冲消毒，原L2 hold优先。

```systemverilog
if (memblock_sync_pkg::csr_special_sequence_active ||
    (tr.io_ooo_to_mem_tlbCsr_satp_changed && tr.io_ooo_to_mem_tlbCsr_vsatp_changed &&
     tr.io_ooo_to_mem_tlbCsr_hgatp_changed && tr.io_ooo_to_mem_tlbCsr_priv_virt_changed))
    capture_csr_level_hold(tr);
```

中文文字伪代码：special或四changed完整动态item先复制hold，清changed/write/trigger/BCLEAR/power/flush协议位；驱动原item后下一边界驱动clean。idle先L2 hold、再CSR hold、最后原默认；reset清两种。未触发完整动态hold的普通idle保持旧行为。

抽象功能描述：`run_legacy()`保留原ASID加一及原runtime完成方式，只补全baseline字段和传播确认；不创建special committed/profile/candidate。

中文文字伪代码：从action baseline复制ATP、权限、SPVP、五PMM、PBMTE及原字段，改SATP ASID。发送四changed完整target，构造clean，等原owner和reset有效的新changed及四相邻clean后sendover；后续原control service匹配runtime进入commit ready。

抽象功能描述：barrier service在提交前沿绑定唯一owner，在全部实际观察到达后完成控制UID；child只产生候选与sendover，不直接释放barrier。

```systemverilog
if (data.active_redirect.valid || data.backend_frame != null ||
    data.backend_redirect_reserved || data.backend_fault_prepared ||
    data.backend_fault_address_pending()) return;
```

中文文字伪代码：在WAIT_OLDER_ROB_COMMIT阶段先等redirect、prepared/未应用frame及必须异常地址结束，再按commit cursor绑定owner。child全部维护完成后stage candidate/sendover；service等新runtime事件，并由原commit_csr_dynamic_candidate_if_observed核对完整clean与PMP模型，才公布committed并转CONTROL_COMMIT_READY。frame与恢复服务在barrier外继续推进。

## 10. 配套测试与构建入口

源码位置：`seq/virtual_sequence/soft_test/memblock_virtualization_framework_selftest_vseq.sv`、`seq/virtual_sequence/memblock_csr_mapping_transport_negative_vseq.sv`、`seq/seq.f`、`seq/seq_pkg.sv`、`tc/src/basicTest.sv`及`AI_DOC/seq/virtualization_framework_soft_framework_testcases_20261117.csv`。

抽象功能描述：自测三组task直接调用生产helper验证输入构造与生命周期合同，不为DUT制造虚假WB或翻译期望；原运输负例继续通过真实driver/monitor检验失败出口。

```systemverilog
check_guest_operations();
check_csr_helpers();
check_fence_helpers();
```

中文文字伪代码：guest组覆盖13指令、尺寸与整数写回、三个自动地址出口、手工非法imm、FP/MOU隔离、STD冻结和有效身份；CSR组覆盖SPVP初始精确权重、禁改保持、单字段必变、五PMM/root比较、Guest及PMM维护步骤、inactive HS返回Host策略；fence组等真实monitor baseline后构造四scope的零/非零目标、GVMA单位和同action重试，单非零权重固定，多权重记录分布，不把有限随机抽样当必现保证。

框架CSV目前包含35个子场景，另已在自测中加入inactive HS进入Guest及返回Host两条检查，提交前需由主代理把对应独立子场景补入CSV。

原15类运输负例按传播／CSR写／fence等待分别调用拆分helper；保留owner/control reset/L2 reset/noack及watchdog检查。场景8/9改为等待负例driver的真实completed_item_count递增，再改变owner或reset，确保取消发生在运输完成后的C4等待阶段，避免固定5ns尚处运输阶段而命中错误失败出口。新增soft_test include目录及package include，basicTest只给明确自测和原运输负例加入active-control白名单，不扩大任意VSEQ权限。CSV每个可独立说明的子场景单行，结果待主代理实际执行后更新。

## 11. 实现与Plan不一致项

以下均已在plan的IMPLEMENTATION_DELTA登记，不把执行中变更当作原始plan覆盖。

| 原plan逻辑 | 最终实现 | 原因与处理 |
|---|---|---|
| 五PMM raw/runtime/readonly由后续RM统一列项 | 本项先补raw/runtime运输，readonly及RM运算留第三项 | CSR-only变化、legacy保留和传播依赖这些值；保留当前实现 |
| 所有HS+Guest同时改变固定目标后HS | target Guest不发伪HS，每次返回非Bare Host后补全HS | SFENCE按实际dvirt解码，否则C0必不匹配；修正维护策略 |
| 旧complete_hs_mapping_fence统包传播与HS fence | wait_target_propagation与complete_mapping_fence分离，15负例分别适配 | 多阶段维护必须复用观察槽；保留原测试目的 |

抽象功能描述：HS维护调整使每笔实际fence在其真实CSR上下文中可表达，同时在访问重新使用HS前清理旧映射。

```systemverilog
if (!target.io_ooo_to_mem_tlbCsr_priv_virt &&
    (hs_mapping_fence_required(old_level,target) ||
     (old_level.io_ooo_to_mem_tlbCsr_priv_virt &&
      (old_level.io_ooo_to_mem_tlbCsr_satp_mode != 0 ||
       target.io_ooo_to_mem_tlbCsr_satp_mode != 0))))
    maintenance_steps.push_back(FENCE_HS);
```

中文文字伪代码：目标仍Guest则不发HS，普通及特殊访问均走Guest翻译；返回Host时即使当前root未变，也在任一非Bare条件下发全HS。C4之前不放行，因此Guest期间多次SATP变化无需dirty历史仍被覆盖。

抽象功能描述：raw/runtime字段前置使完整目标观察可用于本项配置协议，不把权限算法搬进激励主流程。

```systemverilog
raw.pmm_henvcfg = tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg;
pmm_henvcfg = raw.pmm_henvcfg;
```

中文文字伪代码：target导出和monitor采样同一字段，再由原runtime更新复制；五组同理。只运输实际配置，RM期望仍后续独立实现。

抽象功能描述：旧负例在helper拆分后仍分别检验相同失败职责。

```systemverilog
if (propagation_case)
    child.wait_target_propagation(local_data,expected,expected_clean,
                                 baseline_sample,reset_epoch,completed_ok);
else if (write_case)
    child.drive_mapping_csr_item(local_data,clean,reset_epoch,completed_ok);
else child.complete_mapping_fence(local_data,payload,reset_epoch,completed_ok);
```

中文文字伪代码：传播场景故意不产生新target事件或匹配clean；写入场景等待原握手；fence场景使用确定全HS payload。catcher对准该职责的错误，避免拆分后负例空跑。

## 12. Plan未说明但Coding落实的细节

### 12.1 Legacy四changed hold触发

抽象功能描述：driver在legacy动态item结束后持续真实target电平，使四拍传播有实际样本支撑，不依赖special状态。

```systemverilog
if (csr_level_hold_valid) begin
    drive_csr_level_hold();
    return;
end
```

中文文字伪代码：完整动态item触发capture后，idle只要hold有效就保持；reset清除。此工程细节已登记DELTA-2，未增加legacy候选系统。

### 12.2 无当前调用者的公共类别helper

抽象功能描述：公共`set_transaction_ls_kind()`保持切换前VA并满足新特殊opcode格式，防止未来派生类通过该API引入不合法imm。

```systemverilog
tr.update_vaddr();
target_va = tr.vaddr;
apply_minimal_op_template(tr);
normalize_guest_memory_address(tr,target_va);
```

中文文字伪代码：从旧操作数重算并保存目标，再改类别选操作，最后规范新表达。无新状态且不改变生产路径，已登记DELTA-4。

### 12.3 动态非根变化保留手工root

抽象功能描述：`apply_to_transaction()`不让权限-only求解覆盖committed的手工根PPN，保持完整目标的来源一致。

```systemverilog
if (dynamic_mode) begin
    if (satp_mode == cur_satp_mode) tr.io_ooo_to_mem_tlbCsr_satp_ppn = cur_satp_ppn;
    if (vsatp_mode == cur_vsatp_mode) tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = cur_vsatp_ppn;
    if (hgatp_mode == cur_hgatp_mode) tr.io_ooo_to_mem_tlbCsr_hgatp_ppn = cur_hgatp_ppn;
end
```

中文文字伪代码：原fixed roots应用后，仅动态且对应mode未变时恢复committed root；mode变化仍沿原固定root策略。该细节支撑完整基线保持原则，不引入新的根表随机参数，建议保持并在实现review说明。

## 13. git status覆盖与非本次修改的逻辑分析

| 文件或同职责文件组 | 本轮有意义修改覆盖位置 |
|---|---|
| env/plus.sv、seq_csr_common.sv、default.cfg、memblock_dispatch_types.sv | 第3章：13权重、SPVP、5fence、scope/action/payload类型与校验 |
| memblock_op_behavior_util.sv、memblock_dispatch_base_sequence.sv | 第3—5章：分类、候选、地址、宽度、权重、STD冻结 |
| common_data_transaction.sv | 第5、7—9章：Guest识别、4PMP身份、drain、raw decode、fence预约与匹配 |
| main_control_transaction.sv | 第8章：非rand范围、默认、copy/print、手工校验 |
| memblock_csr_randomizer.sv、memblock_csr_config_state.sv | 第6章：SPVP、root保持、比较、PMM合法性和runtime导出 |
| memblock_sync_pkg.sv、mmu_csr_runtime_state.sv、csr_ctrl monitor | 第6章：五PMM字段、变化、reset/update/copy/采样 |
| memblock_csr_initial_config_sequence.sv、memblock_dynamic_csr_change_sequence.sv | 第6—7章及第9章：hook、传播、维护、PMP运输、legacy |
| csr_ctrl driver | 第9章：legacy hold、pulse清理、idle与reset |
| memblock_sfence_control_base_sequence.sv、memblock_control_barrier_service.sv | 第7—9章：类型范围、action复制、barrier及frame门禁 |
| fence monitor、dispatch_monitor_event_adapter.sv | 第8章：raw flushPipe及完整payload日志 |
| soft selftest、mapping transport negative、seq.f、seq_pkg.sv、basicTest.sv、CSV | 第10章：生产helper专项及真实负例构建入口 |
| 三份flow、正式plan及本文review | 文档同步与IMPLEMENTATION_DELTA；不属于额外运行逻辑 |

当前git status中相关源码均纳入上述章节。第一项后端frame已在独立commit内，不在本次diff重复review；第三项RM源码尚未改动。没有把仿真生成产物或无关RTL改动纳入本文；若主代理随后产生新diff，提交前需重新核对范围并补充本表。

## 14. 主代理最终验证与独立复核

2026-11-17，完整编译 `virt_framework4_20261117` 成功（`compile_virt4.log`）。日志根目录为 `/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/`。

| 检查 | 证据与结果 |
|---|---|
| 13操作/CSR/范围/冻结helper | `run_virt4_selftest_mixed.log` 994项断言；五个`run_virt4_selftest_single0..4.log`各1122项断言，均TEST_PASS、UVM_ERROR/FATAL为0 |
| CSR隔离负例 | `run_virt4_negative_isolated.log` 0..14共15场景通过，UVM_ERROR/FATAL为0；预期fatal由精确catcher消费 |
| 基础与真实dispatch | `run_virt4_base.log`、`run_virt4_smoke.log`均TEST_PASS、UVM_ERROR/FATAL为0 |
| 动态CSR与legacy | `run_virt4_csr.log`、`run_virt4_legacy.log`均TEST_PASS、UVM_ERROR/FATAL为0 |
| 真实HINVAL | `run_virt_hinval_vvma.log`、`run_virt_hinval_gvma.log`已在virt3编译下通过；真实raw flushPipe=0及同event C4完成 |

负例运行必须附加 `+MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=20`、`+uvm_set_type_override=fence_agent_agent_driver,memblock_csr_mapping_negative_fence_driver`、`+uvm_set_type_override=csr_ctrl_agent_agent_driver,memblock_csr_mapping_negative_csr_driver`。主代理曾漏两个override，导致scenario8等待隔离driver计数但真实driver不生产该计数；此误跑不作为功能缺陷或通过证据。补齐原隔离拓扑后15场景通过。

主代理重新检查dispatch地址出口、五PMM和SPVP传递、CSR旧上下文排空、返回Host的HS维护、raw flushPipe及matcher，以及负例运输阶段/完成阶段分离；与独立review交叉核对后未发现未关闭blocker。CSV现登记51项soft子场景和6项真实DUT专项。新增Guest指令的端到端数据、两阶段权限及PMM正确性由第三项RM/check实施和联合专项收口，不能由本次helper测试代替。全部basicTest回归在第三项完成后执行。
