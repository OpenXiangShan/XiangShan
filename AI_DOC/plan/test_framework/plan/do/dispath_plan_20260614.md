背景知识：为memory UT环境设计一套可参数化的测试激励框架，该框架用于发送load、store请求，所有测试用例均需基于此框架生成；生成的测试用例需严格符合memblock DUT的合法行为，尤其需保证各信号字段的赋值、信号之间的依赖关联及时序逻辑，均满足 DUT的相关约束。

核心要求补充：本任务中所有生成的表（第一套主表、TLB相关表、3张子表），最终均需存入common\_data\_transaction\.sv文件中；该文件需通过单例模式实现，确保在其他任何环境中均可被共享访问，所有生成器task生成的表数据，最终落脚点均为该common\_data\_transaction\.sv文件。同时需重点基于性能（读写速度、内存占用、访问效率）考量，选择最优的数据存储方式（链表、数组或其他合适形式），优先保证数据存储和共享的性能表现。

核心任务：开发第一套主表生成逻辑，及一套发射队列相关task（整合TLB地址映射、子表拆分、发射控制、异常处理等功能），实现全流程闭环，具体要求如下：

## 一、第一套公共Transaction定义与主表生成（主表生成器）

1. 定义第一套公共Transaction/主控制表：该主表不是“后端到MemBlock issue接口”的逐字段镜像，而是整个memory UT测试框架的全局控制表。主表中优先保存第一类字段，即后续入队、TLB映射、子表拆分、异常/提交处理都需要共享查询的全局控制字段：sqidx\_flag、sqidx\_value、lqidx\_flag、lqidx\_value、futype、fuOpType、src\_0、Imm、vaddr、Robidx\(flag/value\)、tlbaf（原AF修正）、TLBPF（原TLPF修正）、TLB GPF（原TRB GPF修正）、PBMT（原DMT修正）、PMA_af（PMA的after forty）、delay（与子表delay字段同步添加，用于发射gap控制）、corrupt、denied（DCache回复相关）及必要状态标识。第二类字段（如loadWaitBit、waitForRobIdx\_flag/value、storeSetHit、loadWaitStrict、isFirstIssue）和第三类字段（如pc、isRVC、ftqIdx\_flag/value、ftqOffset、pdest、rfWen、fpWen）不作为主表预先强随机的核心字段；后续在数据真正发往load/STA/STD流水线前，由专门的pipeline字段填充函数根据主表、状态表、指令模板和发射场景统一派生并赋值。Transaction中重要的控制字段需要通过约束权重进行控制（例如futype/fuOpType,以及src0、Imm，tlbaf、TLBPF、TLB GPF、PBMT、PMA_af、delay，corrupt、denied 如果有其他也可以）可参考参数控制形式：constraint c_trans_dly {trans_dly dist { 0     :/plus::plus_itlb_dly_0_wt,[1:20] :/plus::plus_itlb_dly_1_20_wt,[21:50]:/plus::plus_itlb_diy_21_50_wt};}
1. 开发主表生成器task任务，功能如下（所有参数通过plus来控制）：
- 接收输入参数（生成数量，如100笔），基于上述第一套公共Transaction进行随机生成，生成对应数量的Transaction实例。
- 将所有随机生成的Transaction实例整理为一张主表，主表需满足以下约束：
  - robidx字段按从小到大的顺序依次递增。
  - lqidx和sqidx先保留字段由后续lsq_crtl任务进行分配
  - vaddr（结合上下文为地址相关字段）需通过src_0和Imm两个变量计算得出；
- 主表存储方式评估：结合性能优先原则，对比关联数组及其他合适存储形式的读写速度、内存占用、访问效率，选择最优存储形式（优先兼顾存储速度和内存占用，储存到common_data_transaction.sv）。
1. 第一套生成器实现要求：通过约束机制完成所有字段的自动生成，在Transaction内部做好各字段的关联逻辑；VDDR（字段填充相关）的填充规则需在post_rodmoize中协同明确，确保每次约束触发即可完成所有字段的完整生成(个别一些字段需要在后续函数中分配例如lqidx和sqidx类)，无需额外手动填充；
1. 主表字段分类及分析：需评估所有字段对测试框架控制流、MemBlock流水线行为和后端写回/调试元信息的影响。字段不再简单划分为“关键/非关键且可独立随机”，而是按以下三类处理：
- 第一类：主表全局控制字段（必须进入主表，约束性强）。主表是整个测试框架的控制表，负责驱动入队、TLB映射、子表拆分、发射、异常和提交处理，不是后端issue接口字段的简单复制。
  - futype、fuOpType：用于区分操作类型（load/store/AMO/prefetch等），是子表拆分和后续发射路径选择的核心依据。二者必须按合法指令模板成对生成，不能独立随机，例如ldu只能匹配load/prefetch类fuOpType，stu只能匹配store/CBO/HSV类fuOpType，mou匹配AMO/LR/SC类fuOpType。
  - src\_0、Imm、vaddr：用于计算访存虚拟地址，vaddr由src\_0+SignExt\(Imm低12位\)派生，是TLB映射、异常注入和DCache访问场景的核心输入。
  - Robidx\(flag/value\)：作为主表全局索引和状态表映射键，用于flush、异常处理、写回关联、pass/fault/commit状态维护；主表生成阶段按从小到大递增。
  - lqidx\_flag/lqidx\_value、sqidx\_flag/sqidx\_value：用于定位load/store在LSQ中的位置。主表中保留字段，但具体值由后续lsq_ctrl任务根据futype和LSQ资源分配后回填，不能在主表生成阶段无约束随机。
  - tlbaf、TLBPF、TLB GPF、PBMT、PMA_af：属于地址翻译、权限、内存属性和异常控制字段，直接影响TLB相关表、异常场景和最终访存结果，应作为主表控制字段保留，并通过plus权重及CSR/虚拟化场景约束生成。
  - corrupt、denied：属于DCache回复/总线返回场景控制字段，会影响写回结果和异常/失败路径，应作为主表控制字段保留，并与访问类型、TLB/PMA场景保持一致。
  - delay：属于测试框架发射节奏控制字段，主表和子表均保留。子表发射task通过gap transaction使用该字段控制实际送入driver的时序。
- 第二类：流水线发射/相关性字段（不在主表中预先强随机，发射前由专门函数派生赋值）。这类字段会影响load等待、MDP相关性、首次发射/重发等行为，但它们应根据主表记录、状态表和当前发射场景生成，不能脱离上下文独立随机。
  - loadWaitBit、waitForRobIdx\_flag/value、storeSetHit、loadWaitStrict：属于MDP/StoreSet/等待前序store相关字段。约束行为包括：waitForRobIdx应指向合法前序store；loadWaitStrict表示更强的等待策略；storeSetHit需要和ssid/waitForRobIdx等相关信息保持一致。普通load场景可置默认值，MDP专项场景再按权重打开。
  - isFirstIssue：合并到第二类处理。首次发射时置为有效；replay、redirect后重发或异常恢复路径需要由发射控制/异常处理函数重新判定，不能在主表生成阶段固定随机。
  - 处理方式：后续新增pipeline字段填充函数，例如fill_issue_dep_fields\(\)，在load/STA/STD transaction真正发往流水线前，根据robidx状态、前序store状态、replay状态、MDP专项场景和plus权重统一填充第二类字段。
- 第三类：后端写回/前端调试元信息字段（不主导主表控制流，发射前由专门函数派生赋值）。这类字段主要随流水线携带，用于写回、ROB/前端redirect、debug或提交信息，但仍需满足基本合法性约束，不能完全独立随机。
  - pc、isRVC、ftqIdx\_flag/value、ftqOffset：属于指令/前端元信息，主要用于异常、redirect、debug、前端训练或回放定位。普通随机场景可由函数统一生成；异常/redirect专项场景需要和robidx、pc序列及ftq信息保持可追踪关系。
  - pdest、rfWen、fpWen：属于写回目标和写回使能字段。它们应根据futype/fuOpType/op_class派生：整数load、HLV、AMO/LR/SC写整数寄存器，rfWen=1且fpWen=0；FP load写浮点寄存器，rfWen=0且fpWen=1；store、FP store、prefetch、CBO等无寄存器写回，rfWen=0且fpWen=0。rfWen和fpWen必须互斥；pdest只有在rfWen或fpWen为1时才有实际意义。
  - 特别约束：fpWen对LoadUnit数据格式有实际影响。fuOpType=lh/lw时，fpWen=0表示整数LH/LW并执行符号扩展；fpWen=1表示FLH/FLW并执行FP boxing。因此fpWen不能作为普通无关随机位处理。
  - 处理方式：后续新增pipeline字段填充函数，例如fill_backend_meta_fields\(\)，在发射前基于第一类字段和指令模板补齐pc/isRVC/ftq/pdest/rfWen/fpWen等第三类字段。
- 推荐生成流程：
  - 主表生成阶段：只生成并保存第一类全局控制字段，同时建立robidx索引和状态表。
  - lsq_ctrl阶段：根据主表futype/op_class和LSQ资源分配lqidx/sqidx，并回填主表。
  - 入队/TLB/子表阶段：基于主表第一类字段生成TLB表及load/STA/STD子表。
  - 发射前字段填充阶段：调用fill_issue_dep_fields\(\)和fill_backend_meta_fields\(\)，为即将进入流水线的transaction补齐第二类和第三类字段。

## 二、发射队列相关task开发（整合TLB地址映射、子表拆分）

核心目标：开发一套发射队列相关task，整合TLB地址映射、子表拆分、数据入队、发射控制、异常监测及写回处理全流程，所有task需与common_data transaction.SV文件（单例模式）强关联，操作数据均来自该文件，存储方式与主表保持统一，严格遵循DUT约束及时序要求。

主任务task:通过调用子任务task控制整个流程：该task信息记录：维护一个内部状态表:(通过robidx和主表映射查询)，记录主表对应robidx字段条目的状态（包括ent_q(入队)、dipatch(代表已经发射到流水线上),wb（从流水线上写回）,fault（写回后有异常）,pass（写回之后正常）,commit(提交)）主任务1发射：子阶段1-1，获取common_data transaction.SV文件中主表子阶段1-2、调取发射入队task进行入队操作子阶段1-3、调取发射控制task进行发射注意：入队和发射之间是否有时序要求，是听一拍还是有什么控制信号需要查询源码后确认这部分逻辑

主任务2发射：监测任务写回及处理（异常处理和正常写回提交）子阶段2-1-异常处理：当写回之后有异常的情况下（根据异常原因进行redirect以及重发）：这部分逻辑需要参考源码给出一个方案子阶段2-2-正常写回之后提交（更新lsq_commit端口发射到dut中）

*每个阶段及时更新状态表

以及主任务1和主任务2需要fork并行处理

子任务task：

1. task0:lsq_ctrl，该任务是维护lsq中lqidx和sqidx的分配和回收，需要根据主表的字段，按照robidx依次分配，参考scala源码逻辑完善分配和回收逻辑
1. Task1：发射入队task
- 数据提取规则：从common_data_transaction.sv文件中的主表，按robidx从小到大的顺序提取对应Transaction数据字段，完成入队操作。
- 入队参数约束：
  - 当前入队数量：提取的最大数据量不超过lsq端口宽度（可参数化配置），该参数用于确定每拍最大入队数量。
  - 端口分配规则：根据每拍入队数量（由参数控制），入队端口按从小到大依次递增分配；若每拍入队数量为8，则8个端口全部启用，依次对应入队数据。
- 入队核心操作：-根据参数确定当拍入队数量-根据入队数量监测lsq信号看是否能入队，
  - 分配字段：调取lsq_ctrl task为每笔入队数据分配lqidx和sqidx，并更新主表。
  - 根据futype分配和主表中字段为Lsq_enq transaction赋值进行入队
1. Task2：TLB地址映射task（生成tlb_transaction表）输出产物：储存在common_data_transaction.sv文件中的tlb_transaction的表，触发机制：由Task1（入队task）触发，每有数据入队，立即执行该task，与入队操作同步联动。
- 输入来源： 获取状态表,从已经入队的robidx中读取入队的的Transaction数据，生成对应的tlb_transaction的表，提取主表的VPN、TLBAF、TLBPF、TLBGPF、PBMT字段作为输入；同时接收env_csr_common.sv控制串传入的地址约束参数（起始地址+范围），和配置csr参数用于控制TLB表的随机生成规则。
- 核心功能：以VPN为索引，完成虚拟地址到最终物理地址（PPN）的映射，同步完成以下操作：
  - 同步修改映射对应的tag字段，提取入队数据中的PBMT字段，将其匹配赋值到TLB相关表对应的PBMT字段；将入队数据中的TLB AF、TLBPF、TLB GPF，分别匹配赋值到TLB相关表对应的TLBAF、TLBPF、TLBGPF字段。
  - 动态配置字段：ASID、VMID需根据当前CSR值进行动态配置；S2Xlate字段需根据csr配置虚拟化进行配置。
  - 映射的paddr需要约束到env_csr_common.sv控制串传入的地址约束参数（起始地址+范围）内
  - S2xlate配置规则：根据csr配置
    - 非虚拟化场景：需配置S1；
    - Only S2场景：仅配置S2；
    - All stage场景：S1、S2均需配置。
  - 字段作用范围约束：（根据源码进行检查这些规则对不对，不对则修改）
    - Only S2场景：入队数据中的PBMT、TLBAF、TLB_gPF作用于对应表项S2；将对应表项TLB_PF置为0
    - All stage场景：入队数据中的PBMT、TLBAF、TLB_gPF作用于对应表项S2；对应表项TLB_PF赋值给S1；
    - Only S1：对应表项TLBgPF置为0，直接将PBMT、TLBAF、TLBPF作用于对应表项S1的映射结果。
  - 关联计算：根据输入的VPN值，计算生成PTE index、ppn_low、Valid index（PTE index与ppn_low VPN存在强关联，需基于VPN推导得出，无需额外手动配置）。
  - 约束性随机：其他权限位、n位、v位需进行约束性随机生成（这些约束在tlb_transaction中控制通过plus控制权重）（需符合TLB权限控制逻辑，不可完全无约束随机）；可参考参数控制形式：constraint c_trans_dly {trans_dly dist { 0     :/plus::plus_itlb_dly_0_wt,[1:20] :/plus::plus_itlb_dly_1_20_wt,[21:50]:/plus::plus_itlb_diy_21_50_wt};}transaction中预留post_randomize():做合法性约束
- 实现要求：与入队数据强关联，确保字段映射准确、配置规则贴合TLB虚拟化逻辑；生成的TLB相关表需存入common data transaction DSV文件，遵循单例模式的共享要求，存储方式与主表保持一致，确保整体共享性能。
1. Task3：子表拆分与实时维护task（整合原第三套子表拆分逻辑，关联TLB映射）获取状态表,从已经入队的robidx中通过fuType区分ld还是store,分别将主表提取出load,sta,std三张内部表（表中添加delay字段）注释：先通过 fuType == stu 判断这是一条 store，需要分配 SQ；随后上游调度逻辑把这条 store 同时送到 STA issue queue 和 STD issue queue。STA 侧用 base\+imm 生成地址，STD 侧用 store data 源写 SQ data。到了 MemBlock Verilog 端口时，STA/STD 已经是不同入口，不是在 MemBlock 里再靠 fuType 拆
- 触发机制：由Task2（TLB地址映射task）触发，TLB映射完成后立即执行，实现入队→TLB映射→子表拆分的连贯流程。
- 核心逻辑：基于入队并完成TLB映射后的Transaction数据，按futype字段进行拆分，生成并维护3张子表（分别对应load、Sta、Std类型），无需重复生成，仅随入队数据实时更新。
- 子表字段要求：每张子表需新增delay，与主表保持一致：
- delay：每个Transaction实例对应新增delay字段（与主表delay字段同步，无额外约束，随子表生成同步添加：plus参数控制）。
- 维护要求：实时监测入队及TLB映射后的新数据，一旦有新数据，立即更新对应子表（新增数据至对应子表），确保三张子表的数据与入队数据、TLB映射数据、主表数据一致，同步存入common data transaction DSV文件，存储方式与主表、TLB相关表保持统一。
1. Task4：发射控制task（分3个专项task，分别对应load、Sta、Std）
- 专项Task4\-1：load发射task
  - 数据来源：从load子表中提取未发射数据（发射标识未置1的数据）。
  - 发射数量控制：由参数控制每拍发射数量，最大发射数量不超过3条；若参数值大于子表中未发射数据量，则按子表实际未发射数量发射。
  - 选取规则：从load子表未发射数据中乱序选取，发射完成后，将该条数据的“发射标识”置1，后续不再选取该条数据。
- 专项Task4\-2：Sta发射task、专项Task4\-3：Std发射task
  - 逻辑与load发射task一致，区别在于：每拍最大发射数量不超过2条，由独立参数分别控制Sta、Std每拍发射数量；数据来源分别为Sta子表、Std子表，发射后同步置位对应数据的“发射标识”。
- 发射时序控制：每张子表中已有的delay字段，用于控制对应数据发射至driver的时序，通过gap transition中的gap参数，实现delay时间的精准控制，符合DUT时序约束。
1. Task5：( 对应主任务二)异常监测与写回处理task
- 监测目标：实时监测memblock to ooo的写回后端的端口，同时监测lqidx（LB index）、Robidx（rob index）的写回信息。
- 异常处理：若监测到memory block写回异常，立即将主表、TLB相关表及对应状态表中异常的“异常标识”置高；若发射成功，则将对应状态表的“pass标识”置高。
- 异常标识后进入异常处理，根据异常类型 发射redirect，或者选择重发(这里需要同步好发射task) (redirect如何同步，重发如何同步) (需要确认发射redirect具体流程，稍后是否需要flush操做，具体异常类型如何redirect以及重发需要参考源码给出方案)
- 发射成功需要参考源码逻辑进行提交逻辑处理，配置lsq commit字段发射到提交(添加一个参数能控制提交的延时)
- 数据清除：根据写回的lqidx，将主表、TLB相关表及子表中对应lqidx的数据一一清除，确保数据一致性。

## 三、发射队列task补充要求

1. 所有发射队列相关task（入队、TLB映射、子表维护、发射控制等），均需读取、操作common data transaction DSV文件中的数据，遵循单例模式，确保数据共享一致性，存储方式与主表保持统一，优先保障性能。
1. 所有参数（入队宽度、每拍入队数量、每拍发射数量、TLB相关参数等）均需支持参数化配置，适配不同测试场景需求，与整体测试激励框架的参数化要求保持一致。
1. 各task之间的联动需清晰，触发机制明确（入队→TLB映射→子表拆分→发射→异常监测），避免逻辑冲突，确保时序符合DCache DUT的合法行为约束。
1. 所有标识字段（发射标识、success、异常标识、access标识）及TLB相关表、子表数据，需同步存入common data transaction DSV文件，便于后续调试、监测及数据追溯。

## 整体要求

1\.  主表生成与发射队列相关task需协同关联，发射队列的所有操作均基于主表数据，确保数据流转准确无误；

2\.  所有表的生成、更新逻辑需关联协同，主表的约束规则需同步作用于TLB相关表、子表，确保所有表数据一致；所有生成的表均需存入common data transaction DSV文件，通过单例模式实现多环境共享；

3\.  优先通过约束机制实现字段自动生成、关联及配置，减少手动干预，提升生成效率；

4\.  所有表的存储方式需基于性能（读写速度、内存占用、访问效率）优先原则选择，优先适配common data transaction DSV文件的单例共享需求，所有表的存储方式尽量统一，保障整体性能；

5\.  严格按三类字段组织第一套主表及流水线transaction：第一类全局控制字段进入主表并严格遵循约束；第二类流水线发射/相关性字段、第三类后端写回/前端调试元信息字段不在主表阶段独立随机，而是在实际发往流水线前由专门函数基于主表、状态表和指令模板派生赋值；

6\.  TLB地址映射逻辑需贴合TLB接口逻辑，配置规则、字段关联需符合虚拟化场景要求，确保PPN约束口子可正常接收外部约束，同时受公共CSR控制串的地址约束参数控制；

7\.  单例模式实现需确保common data transaction DSV文件的共享安全性和访问效率，避免多环境访问冲突，同时配合最优存储方式，最大化整体性能；

8\.  子表拆分需严格按futype字段执行，send ID约束、字段新增需符合要求，确保子表与主表、TLB相关表的数据一致性和关联性。
