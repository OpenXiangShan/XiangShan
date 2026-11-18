# H 扩展标签说明

当前共有 45 处 `H_EXT_CTRL(hasHext)` 标签，涉及 18 个 Scala 文件。标签用于定位后续添加 H 扩展开关的代码，目前只增加了注释。

| 被标注的模块（源码文件） | 为什么标注 | 标签覆盖的内容 |
| --- | --- | --- |
| [XSCoreParameters（Parameters.scala）](../src/main/scala/xiangshan/Parameters.scala) | H 功能开关需要与对外 ISA 声明一致；现有参数还影响硬件结构。 | `HasHExtension` 与地址位宽、VMID 字段、仲裁端口的关联；`ISAExtensions` 中的 H 相关能力声明。 |
| [TLB / TlbRequestPrepare（TLB.scala）](../src/main/scala/xiangshan/cache/mmu/TLB.scala) | 这是 guest 翻译的入口，决定请求采用哪种地址检查、权限和翻译阶段。 | `hyperinst/hlvx` 请求限定、非虚拟 CSR 控制、`noS2xlate` 阶段选择，以及 S0 预检查与主 TLB 的一致性。 |
| [TlbSectorEntry（MMUBundle.scala）](../src/main/scala/xiangshan/cache/mmu/MMUBundle.scala) | H 专用字段随每个 TLB 表项重复存储，是可裁剪的面积来源。 | `g_perm`、`g_pbmt`、`vmid`、`s2xlate` 字段，以及裁剪这些字段时关联的 `hit/wbhit/apply` 逻辑。 |
| [TLBFA（TLBStorage.scala）](../src/main/scala/xiangshan/cache/mmu/TLBStorage.scala) | H 元数据按 way 复制，匹配和选择逻辑又按访问端口展开。 | 表项寄存器阵列中的 H 部分、多端口 VMID/stage 比较、G-stage 权限/PBMT 选择，以及 HFENCE.VVMA/GVMA 失效网络。 |
| [L2TLBImp（L2TLB.scala）](../src/main/scala/xiangshan/cache/mmu/L2TLB.scala) | 这里实例化 HPTW 并分配配套资源，是关闭主要 H 硬件的集成入口。 | HPTW 实例、请求/响应仲裁、page-cache 仲裁端口、S1/S2 结果合并、访存源 ID、访存端口及专用 PMPChecker；相关 cache 握手、MPT/bitmap、响应和 refill 接线合并在主入口说明中。 |
| [PTW / LLPTW / LLPTWEntry / HPTW（PageTableWalker.scala）](../src/main/scala/xiangshan/cache/mmu/PageTableWalker.scala) | HPTW 是独立 G-stage 硬件；普通 walker 内还保留了嵌套翻译的状态、寄存器和仲裁。 | 整个 HPTW 类；PTW 首次/末次 HPTW 请求与等待；LLPTWEntry 中的 H 结果；LLPTW 的 H 状态及 `hyper_arb1/2`。普通页表遍历部分不属于整块关闭对象。 |
| [PtwCache（PageTableCache.scala）](../src/main/scala/xiangshan/cache/mmu/PageTableCache.scala) | H 标签和 VMID 按缓存表项复制，另有 G-stage 返回及 HFENCE 比较网络。 | L0/L1 的 H-stage/VMID 阵列、L2/L3/SP 对应元数据、G-stage bypass/响应和 HFENCE 失效逻辑；普通 PTE SRAM 保留。 |
| [GPAMem（GPAMem.scala）](../src/main/scala/xiangshan/backend/GPAMem.scala) | 整块存储用于取指 guest fault，容量随 FTQ 深度增长，是明确的 H 专用面积对象。 | GPA 和 VS 非叶 PTE 标记的存储阵列、读写选择、GPA 地址加法器。 |
| [CtrlBlock（CtrlBlock.scala）](../src/main/scala/xiangshan/backend/CtrlBlock.scala) | GPAMem 在这里实例化，裁剪实体需要同时处理上下游连接。 | `gpaMem` 实例化及后续 module 引用、IFU 写入和 ROB 读取接线；公共异常接口可保留并返回无效 guest 信息。 |
| [NewCSR（NewCSR.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala) | 这是虚拟化状态、H 指令权限和外部控制输出的集中入口，关系到关闭 H 后的功能正确性。 | H/VS CSR 地址分类、guest 中断输入、`nextV` 的 trap/xRET 更新、VGEIN/VS claim、CSR→MMU 控制，以及 `hlsv/hfenceGVMA/hfenceVVMA` 非法指令信号。 |
| [CSRPermitModule（CSRPermitModule.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala) | 仅让 CSR 读回 0 不能阻止访问副作用，需要在权限入口统一处理。 | H/VS 专用 CSR 的读写拒绝，包含 VS AIA/间接 CSR；通过 `EX_II` 联动写使能、IMSIC 异步请求和 claim 副作用控制。 |
| [M 级 CSR 定义（MachineLevel.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala) | 这些共享 CSR 中的 H 字段决定虚拟态恢复、能力声明和 guest 中断行为，位数虽少但功能关键。 | `mstatus.GVA/MPV`、`mnstatus.MNPV`、`misa.H`，以及 `mideleg/mie/mip` 中的 VS、SGE 中断位和关联更新。 |
| [HypervisorLevel（HypervisorLevel.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/HypervisorLevel.scala) | H CSR bank 直接控制 guest 翻译和中断，内部值及硬件更新也需要受开关约束。 | `hstatus.SPV/SPVP/HU/VGEIN`、`hgatp`、guest IRQ 控制，以及硬件更新和中断别名副作用。 |
| [VirtualSupervisorLevel（VirtualSupervisorLevel.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/VirtualSupervisorLevel.scala) | VS CSR bank 保存 guest 状态，不能只屏蔽软件访问而忽略状态更新和别名输出。 | VSATP、guest 中断控制、VS 状态更新及中断别名副作用。 |
| [DcsrBundle（DebugLevel.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala) | `dcsr.V` 虽然只有一位，但能影响 DRET 是否恢复虚拟态。 | `dcsr.V` 的软件可见值和内部有效值，包括 debug-entry 更新。 |
| [InterruptFilter（InterruptFilter.scala）](../src/main/scala/xiangshan/backend/fu/NewCSR/InterruptFilter.scala) | VS 优先级树、流水和注入选择是成组的 H 相关逻辑，适合集中控制。 | `hvipriosSort`、分组选择、`hvipriosReg`、最终选择树，以及 Candidate1..5 / `hvictl` 的 IID/IPRIO 选择；M/HS 通路保留。 |
| [CSR 包装模块（wrapper/CSR.scala）](../src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala) | 这里实例化 IMSIC，裁剪 guest files 会影响主仓库与中断控制器的接口。 | IMSIC 实例，以及 guest pending 切片、topei/claim、MSI 编码和地址配置关联；保留 M/S files。 |
| [IMSIC / IntFile（ChiselAIA 子模块）](../ChiselAIA/src/main/scala/IMSIC.scala) | 每个 guest interrupt file 都有独立存储和优先级逻辑，是可整体裁剪的 H 面积来源。 | IntFile 的 EIP/EIE 存储；实例化循环中 `i == 1 && j >= 1` 的 guest files，以及相关控制、优先级选择、gateway 和返回汇总逻辑。 |
