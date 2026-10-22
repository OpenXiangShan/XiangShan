# MemBlock L2TLB 大小页重叠分配保护方案 Review 记录

状态：REVIEW_PASS，无必须修改项

Review 日期：2026-10-20

Review 对象：

`AI_DOC/plan/test_framework/plan/do/memblock_l2tlb_overlapping_page_allocation_guard_plan_20261020.md`

## 1. Review 范围

独立 reviewer 对方案与当前测试框架源码进行了逐项核对，重点检查：

1. 4KB、64KB、2MB、1GB、512GB coverage shape及allStage组合规则。
2. parent occupancy正常路径性能、上下文隔离和注册/删除对称性。
3. builder两阶段接口、权重聚合、fallback及runtime调用顺序。
4. live唯一性、completed历史和UID issue-time provenance边界。
5. Fence、reset、copy、directed soft test的完整生命周期。
6. 修改范围、函数级伪代码、测试矩阵和验收标准的可执行性。

## 2. 第一轮 Review

结论：`REVIEW_FAIL`，共7项必须修改项。

主要问题：

1. allStage/NAPOT候选shape未区分无fault收敛和fault raw level语义。
2. builder与occupancy之间缺少可实现的prepare-mask-finalize接口。
3. completed查询与UID snapshot存在provenance循环依赖。
4. reset、copy、directed install和统一删除生命周期不完整。
5. global scope未明确只由active S1的`PTE.G`控制。
6. 反向重叠测试未覆盖全部10种child-to-parent组合。
7. 新增helper缺少规则要求的完整函数描述和差异说明。

修订结果：

1. 分离fault和无fault的level/N shape规则，并定义allStage 16种raw组合及权重聚合。
2. 引入`prepare_tlb_payload_context()`、allowed mask和finalize两阶段builder流程。
3. capture前按completed response sample选择唯一最新可见响应，capture后RM只读冻结snapshot。
4. 补齐reset/copy/Fence/direct test生命周期和统一publish/delete入口。
5. 固定S1 global、onlyStage2和allStage的ASID/VMID语义。
6. 补齐全部10种反向重叠基础矩阵及上下文交叉验证。
7. 按plan规则补齐helper目的、输入、输出、副作用和两类伪代码。

## 3. 第二轮 Review

结论：`REVIEW_FAIL`，共4项必须修改项。

主要问题：

1. allStage shape helper位于builder之后，存在定义顺序和复用冲突。
2. runtime miss未明确generation、PPN reuse和publish的完整顺序。
3. 正权重候选全被屏蔽时的fallback规则不完整。
4. provenance soft test仍存在直接写表、旧最大页预期和非法重叠注入问题。

修订结果：

1. 将算法定义为先于builder可见的`memblock_tlb_entry::derive_allstage_lookup_shape()`，由builder、range index和raw matcher共享。
2. 固定runtime顺序为`prepare -> mask -> finalize -> generation -> PPN reuse -> publish`。
3. 正权重候选全部被屏蔽时强制生成普通4KB，并增加独立统计与定向验证。
4. 将provenance soft test纳入修改范围，删除RM最大页重选预期，并定义test-only非法注入和expected-fatal catcher。

## 4. 第三轮 Review

结论：`REVIEW_FAIL`，共3项必须修改项。

主要问题：

1. allStage未在组合权重前分别按每个active stage的Sv39/Sv48能力过滤raw level。
2. 零权重bucket可能绕过N override和forced 4KB判断。
3. provenance replacement是字段不完整的空壳entry，无法通过统一publish校验。

修订结果：

1. 每个active stage先按自身translation mode过滤raw level；mode过滤后stage源权重和为0保持现有配置fatal语义。
2. `raw_weight=0`不创建bucket，并以`total_positive_weight==0`触发N override或forced 4KB。
3. 增加混合Sv39/Sv48 allStage测试。
4. replacement先由builder生成stage、sector、PPN均完整的合法entry，再执行定向PF覆盖、完整性检查和统一publish。

## 5. 第四轮 Review

结论：`REVIEW_PASS：无必须修改项`。

独立 reviewer 再次对照现有源码确认：

1. 每个stage的mode过滤、allStage effective shape和fault/NAPOT规则一致且可实现。
2. prepare-mask-finalize接口及runtime顺序不存在定义依赖或半发布状态。
3. parent occupancy正常路径仅执行固定次数count查询，不扫描live表或owner集合。
4. parent owner仅用于O(1)注册、删除和fatal诊断，生命周期与reset/Fence/copy语义对称。
5. completed响应按issue boundary和最新sample建立唯一snapshot，RM不再重新扫描或按最大页重选。
6. directed soft test迁移、非法状态注入、测试矩阵和验收标准完整。

## 6. 最终结论

方案已满足进入coding阶段的条件。本轮review仅确认方案正确性和可执行性，未进行代码实现、编译或仿真。
