# CSR 下划线命名到驼峰命名修改清单

基线分支：`kunminghu-v3`。基线提交：`a253c1163982ae0b485b3a2ed9b61253d980d837`。

## 范围和命名规则

覆盖 `backend/fu/NewCSR`、CSR 包装器和接口、`util/CSRConst.scala`，以及共享的 CSR 控制、分布式更新和不可屏蔽中断接口。同步更新所有受影响的 Scala 符号引用。

变量、方法、参数和状态采用小驼峰，例如 `reset_mtvec` → `resetMtvec`、`L2_PF_ENABLE` → `l2PfEnable`。异常缩写保留含义，例如 `csrAccess_EX_II` → `csrAccessExII`。寄存器编号范围用 `To` 连接，例如 `isMireg2_6` → `isMireg2To6`。

以下项目保留原名：Chisel 规定的覆写入口 `do_asUInt`；Scala 一元运算符入口 `unary_~`；外部库和本次范围外模块定义的符号；已停用的块注释代码。CSR 地址、位域位置、复位值、读写行为以及外部协议的字符串标识不变。

## 定义修改表

共 281 项定义修改，涉及 32 个源码文件（包括引用更新）。模块列记录定义所属的 Scala 类、对象或方法；同名定义在不同模块中分别列出。完整数据另见 `csr-camel-case-renames.csv`。

| 文件 | 模块 | 旧名称 | 新名称 |
| --- | --- | --- | --- |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `bp_ctrl` | `bpCtrl` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `cache_error_enable` | `cacheErrorEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `commit_stuck_check_enable` | `commitStuckCheckEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `distribute_csr` | `distributeCsr` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `flush_l2_enable` | `flushL2Enable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `frontend_trigger` | `frontendTrigger` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `fusion_enable` | `fusionEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `hd_misalign_ld_enable` | `hdMisalignLdEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `hd_misalign_st_enable` | `hdMisalignStEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `high_density_rob_compression_enable` | `highDensityRobCompressionEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `ldld_vio_check_enable` | `ldldVioCheckEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `lvpred_disable` | `lvpredDisable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `lvpred_timeout` | `lvpredTimeout` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `mem_trigger` | `memTrigger` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `no_spec_load` | `noSpecLoad` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `pf_ctrl` | `pfCtrl` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `power_down_enable` | `powerDownEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `sbuffer_threshold` | `sbufferThreshold` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `sbuffer_timeout` | `sbufferTimeout` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `soft_prefetch_enable` | `softPrefetchEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `storeset_no_fast_wakeup` | `storesetNoFastWakeup` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `storeset_wait_store` | `storesetWaitStore` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `uncache_write_outstanding_enable` | `uncacheWriteOutstandingEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `CustomCSRCtrlIO` | `wfi_enable` | `wfiEnable` |
| `src/main/scala/xiangshan/Bundle.scala` | `DistributedCSRUpdateReq.apply` | `src_description` | `srcDescription` |
| `src/main/scala/xiangshan/Bundle.scala` | `NonmaskableInterruptIO` | `nmi_31` | `nmi31` |
| `src/main/scala/xiangshan/Bundle.scala` | `NonmaskableInterruptIO` | `nmi_43` | `nmi43` |
| `src/main/scala/xiangshan/backend/fu/CSR.scala` | `CSRFileIO` | `wfi_event` | `wfiEvent` |
| `src/main/scala/xiangshan/backend/fu/CSR.scala` | `FpuCsrIO` | `dirty_fs` | `dirtyFs` |
| `src/main/scala/xiangshan/backend/fu/CSR.scala` | `VpuCsrIO` | `dirty_vs` | `dirtyVs` |
| `src/main/scala/xiangshan/backend/fu/CSR.scala` | `VpuCsrIO` | `set_vstart` | `setVstart` |
| `src/main/scala/xiangshan/backend/fu/CSR.scala` | `VpuCsrIO` | `set_vtype` | `setVtype` |
| `src/main/scala/xiangshan/backend/fu/CSR.scala` | `VpuCsrIO` | `set_vxsat` | `setVxsat` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRAIA.scala` | `NewCSR.AIAToCSRBundle` | `notice_pending` | `noticePending` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `bp_ctrl` | `bpCtrl` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `cache_error_enable` | `cacheErrorEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `commit_stuck_check_enable` | `commitStuckCheckEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `flush_l2_enable` | `flushL2Enable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `fusion_enable` | `fusionEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `hd_misalign_ld_enable` | `hdMisalignLdEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `hd_misalign_st_enable` | `hdMisalignStEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `high_density_rob_compression_enable` | `highDensityRobCompressionEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `ldld_vio_check_enable` | `ldldVioCheckEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `lvpred_disable` | `lvpredDisable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `lvpred_timeout` | `lvpredTimeout` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `no_spec_load` | `noSpecLoad` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `pf_ctrl` | `pfCtrl` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `power_down_enable` | `powerDownEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `sbuffer_threshold` | `sbufferThreshold` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `sbuffer_timeout` | `sbufferTimeout` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `soft_prefetch_enable` | `softPrefetchEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `storeset_no_fast_wakeup` | `storesetNoFastWakeup` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `storeset_wait_store` | `storesetWaitStore` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `uncache_write_outstanding_enable` | `uncacheWriteOutstandingEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala` | `NewCSR.CSRBundles.CSRCustomState` | `wfi_enable` | `wfiEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.McorepwrBundle` | `COMMIT_STUCK_CHECK_ENABLE` | `commitStuckCheckEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.McorepwrBundle` | `POWER_DOWN_ENABLE` | `powerDownEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.MflushpwrBundle` | `FLUSH_L2_ENABLE` | `flushL2Enable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.MflushpwrBundle` | `L2_FLUSH_DONE` | `l2FlushDone` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `ABTB_ENABLE` | `abtbEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `ITTAGE_ENABLE` | `ittageEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `MBTB_ENABLE` | `mbtbEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `RAS_ENABLE` | `rasEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `SC_ENABLE` | `scEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `TAGE_ENABLE` | `tageEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SbpctlBundle` | `UBTB_ENABLE` | `ubtbEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SlvpredctlBundle` | `LVPRED_DISABLE` | `lvpredDisable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SlvpredctlBundle` | `LVPRED_TIMEOUT` | `lvpredTimeout` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SlvpredctlBundle` | `NO_SPEC_LOAD` | `noSpecLoad` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SlvpredctlBundle` | `STORESET_NO_FAST_WAKEUP` | `storesetNoFastWakeup` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SlvpredctlBundle` | `STORESET_WAIT_STORE` | `storesetWaitStore` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `CACHE_ERROR_ENABLE` | `cacheErrorEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `HD_MISALIGN_LD_ENABLE` | `hdMisalignLdEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `HD_MISALIGN_ST_ENABLE` | `hdMisalignStEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `LDLD_VIO_CHECK_ENABLE` | `ldldVioCheckEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `SBUFFER_THRESHOLD` | `sbufferThreshold` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `SBUFFER_TIMEOUT` | `sbufferTimeout` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `SOFT_PREFETCH_ENABLE` | `softPrefetchEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SmblockctlBundle` | `UNCACHE_WRITE_OUTSTANDING_ENABLE` | `uncacheWriteOutstandingEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_ACTIVE_STRIDE` | `l1dPfActiveStride` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_ACTIVE_THRESHOLD` | `l1dPfActiveThreshold` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_BERTI_ENABLE` | `l1dPfBertiEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_ENABLE` | `l1dPfEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_ENABLE_AGT` | `l1dPfEnableAgt` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_ENABLE_PHT` | `l1dPfEnablePht` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_STREAM_ENABLE` | `l1dPfStreamEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_STRIDE_ENABLE` | `l1dPfStrideEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1D_PF_TRAIN_ON_HIT` | `l1dPfTrainOnHit` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L1I_PF_ENABLE` | `l1iPfEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_CDP_ENABLE` | `l2PfCdpEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_DELAY_LATENCY` | `l2PfDelayLatency` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_ENABLE` | `l2PfEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_PBOP_ENABLE` | `l2PfPbopEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_RECV_ENABLE` | `l2PfRecvEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_STORE_ONLY` | `l2PfStoreOnly` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_TP_ENABLE` | `l2PfTpEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SpfctlBundle` | `L2_PF_VBOP_ENABLE` | `l2PfVbopEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SrnctlBundle` | `FUSION_ENABLE` | `fusionEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SrnctlBundle` | `HIGH_DENSITY_ROB_COMPRESSION_ENABLE` | `highDensityRobCompressionEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala` | `NewCSR.SrnctlBundle` | `WFI_ENABLE` | `wfiEnable` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala` | `NewCSR.Ireg` | `isInMireg2_6` | `isInMireg2To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala` | `NewCSR.Ireg` | `isInMireg3_6` | `isInMireg3To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala` | `NewCSR.Ireg` | `isInSireg2_6` | `isInSireg2To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala` | `NewCSR.Ireg` | `isInSireg3_6` | `isInSireg3To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala` | `NewCSR.Ireg` | `isInVSireg2_6` | `isInVSireg2To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala` | `NewCSR.Ireg` | `isInVSireg3_6` | `isInVSireg3To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitIO` | `EX_II` | `exII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitIO` | `EX_VI` | `exVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `Xret_EX_II` | `xRetExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `Xret_EX_VI` | `xRetExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `csrAccess_EX_II` | `csrAccessExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `csrAccess_EX_VI` | `csrAccessExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `directPermit_illegal` | `directPermitIllegal` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `indirectPermit_EX_II` | `indirectPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `indirectPermit_EX_VI` | `indirectPermitExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `mPermit_EX_II` | `mPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `pPermit_EX_II` | `pPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `pPermit_EX_VI` | `pPermitExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `sPermit_EX_II` | `sPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `vPermit_EX_II` | `vPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.CSRPermitModule` | `vPermit_EX_VI` | `vPermitExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessAIA_EX_II` | `accessAIAExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessContext_EX_II` | `accessContextExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessCustom_EX_II` | `accessCustomExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessEnvcfg_EX_II` | `accessEnvcfgExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessHPM_EX_II` | `accessHPMExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessIND_EX_II` | `accessINDExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessStateen_EX_II` | `accessStateenExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `accessTopie_EX_II` | `accessTopieExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `fpOff_EX_II` | `fpOffExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `fpVec_EX_II` | `fpVecExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `rwSatp_EX_II` | `rwSatpExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `rwScountinhibit_EX_II` | `rwScountinhibitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `rwStimecmp_EX_II` | `rwStimecmpExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `rwStopei_EX_II` | `rwStopeiExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `vecOff_EX_II` | `vecOffExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule` | `xstateControlAccess_EX_II` | `xstateControlAccessExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.MLevelPermitModule.$anon` | `mLevelPermit_EX_II` | `mLevelPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.PrivilegePermitModule.$anon` | `privilege_EX_II` | `privilegeExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.PrivilegePermitModule.$anon` | `privilege_EX_VI` | `privilegeExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.SLevelPermitModule` | `accessCustom_EX_II` | `accessCustomExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.SLevelPermitModule` | `accessHPM_EX_II` | `accessHPMExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.SLevelPermitModule.$anon` | `sLevelPermit_EX_II` | `sLevelPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessAIA_EX_VI` | `accessAIAExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessContext_EX_VI` | `accessContextExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessCustom_EX_VI` | `accessCustomExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessEnvcfg_EX_VI` | `accessEnvcfgExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessHPM_EX_VI` | `accessHPMExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessIND_EX_VI` | `accessINDExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessStateen_EX_VI` | `accessStateenExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `accessTopie_EX_VI` | `accessTopieExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `readScountovf_EX_VI` | `readScountovfExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `rwSatp_EX_VI` | `rwSatpExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `rwScountinhibit_EX_VI` | `rwScountinhibitExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `rwSip_Sie_EX_VI` | `rwSipSieExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `rwStimecmp_EX_VI` | `rwStimecmpExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `rwStopei_EX_VI` | `rwStopeiExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `rwVStopei_EX_II` | `rwVStopeiExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule` | `xstateControlAccess_EX_VI` | `xstateControlAccessExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule.$anon` | `virtualLevelPermit_EX_II` | `virtualLevelPermitExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.VirtualLevelPermitModule.$anon` | `virtualLevelPermit_EX_VI` | `virtualLevelPermitExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule` | `dret_EX_II` | `dretExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule` | `mnret_EX_II` | `mnretExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule` | `mret_EX_II` | `mretExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule` | `sret_EX_II` | `sretExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule` | `sret_EX_VI` | `sretExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule.$anon` | `Xret_EX_II` | `xRetExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala` | `NewCSR.XRetPermitModule.$anon` | `Xret_EX_VI` | `xRetExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala` | `NewCSR.TrigMatch` | `MASK_HI` | `maskHi` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala` | `NewCSR.TrigMatch` | `MASK_LO` | `maskLo` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala` | `NewCSR.TrigMatch` | `NMASK_HI` | `nmaskHi` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala` | `NewCSR.TrigMatch` | `NMASK_LO` | `nmaskLo` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala` | `NewCSR.TriggerVer` | `Spec_1dot0` | `spec1dot0` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala` | `NewCSR.TriggerVer` | `Spec_2302` | `spec2302` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_BP` | `exBp` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_DBLTRP` | `exDbltrp` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_HSCALL` | `exHscall` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_HWE` | `exHwe` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_IAF` | `exIaf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_IAM` | `exIam` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_IGPF` | `exIgpf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_II` | `exII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_IPF` | `exIpf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_LAF` | `exLaf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_LAM` | `exLam` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_LGPF` | `exLgpf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_LPF` | `exLpf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_MCALL` | `exMcall` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_SAF` | `exSaf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_SAM` | `exSam` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_SGPF` | `exSgpf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_SPF` | `exSpf` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_SWC` | `exSwc` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_UCALL` | `exUcall` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_VI` | `exVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala` | `NewCSR.ExceptionBundle` | `EX_VSCALL` | `exVscall` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwMireg2_6_EX_II` | `rwMireg2To6ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwMireg_EX_II` | `rwMiregExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwSireg2_6_EX_II` | `rwSireg2To6ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwSireg2_6_EX_VI` | `rwSireg2To6ExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwSireg_EX_II` | `rwSiregExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwSireg_EX_VI` | `rwSiregExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectAIAPermitModule` | `rwVSiregX_EX_II` | `rwVSiregXExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRPermitModule.$anon` | `indirectCSR_EX_II` | `indirectCSRExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRPermitModule.$anon` | `indirectCSR_EX_VI` | `indirectCSRExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRWindowPermitModule` | `isMireg2_6` | `isMireg2To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRWindowPermitModule` | `isSireg2_6` | `isSireg2To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRWindowPermitModule` | `isSireg3_6` | `isSireg3To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRWindowPermitModule` | `isVSireg2_6` | `isVSireg2To6` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRWindowPermitModule.$anon` | `indirectCSR_EX_II` | `indirectCSRExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectCSRWindowPermitModule.$anon` | `indirectCSR_EX_VI` | `indirectCSRExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwMireg2_6_EX_II` | `rwMireg2To6ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwSireg2_6_EX_II` | `rwSireg2To6ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwSireg2_6_EX_VI` | `rwSireg2To6ExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwSireg_EX_II` | `rwSiregExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwSireg_EX_VI` | `rwSiregExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwVSireg2_6_EX_II` | `rwVSireg2To6ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectIMSICPermitModule` | `rwVSireg_EX_II` | `rwVSiregExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectOtherPermitModule` | `rwMiregX_EX_II` | `rwMiregXExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectOtherPermitModule` | `rwSiregX_EX_II` | `rwSiregXExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectOtherPermitModule` | `rwVSiregX_EX_II` | `rwVSiregXExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectSmcdelegPermitModule` | `rwMiregX_EX_II` | `rwMiregXExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectSmcdelegPermitModule` | `rwSireg1_2_EX_II` | `rwSireg1To2ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectSmcdelegPermitModule` | `rwSireg3_6_EX_II` | `rwSireg3To6ExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectSmcdelegPermitModule` | `rwSiregX_EX_VI` | `rwSiregXExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala` | `NewCSR.IndirectSmcdelegPermitModule` | `rwVSiregX_EX_II` | `rwVSiregXExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptBundle.scala` | `NewCSR.NonMaskableIRNO` | `NMI_31` | `nmi31` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptBundle.scala` | `NewCSR.NonMaskableIRNO` | `NMI_43` | `nmi43` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptBundle.scala` | `NewCSR.NonMaskableIRPendingBundle` | `NMI_31` | `nmi31` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptBundle.scala` | `NewCSR.NonMaskableIRPendingBundle` | `NMI_43` | `nmi43` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptFilter.scala` | `NewCSR.InterruptFilterIO.$anon` | `notice_pending` | `noticePending` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala` | `NewCSR.HasNonMaskableIRPBundle` | `NMI_31` | `nmi31` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala` | `NewCSR.HasNonMaskableIRPBundle` | `NMI_43` | `nmi43` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala` | `NewCSR.MachineLevel` | `mbmc_lock` | `mbmcLock` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala` | `NewCSR.MachineLevel` | `reset_mtvec` | `resetMtvec` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala` | `NewCSR.MachineLevel.$anon.$anonfun` | `should_reset` | `shouldReset` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.CSRConfig` | `EXT_DBLTRP` | `extDbltrp` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.CSRConfig` | `EXT_SSTC` | `extSstc` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSR` | `imsic_EX_II` | `imsicExII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSR` | `imsic_EX_VI` | `imsicExVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSR` | `s_finish` | `sFinish` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSR` | `s_idle` | `sIdle` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSR` | `s_waitIMSIC` | `sWaitIMSIC` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSR.$anon` | `reset_mtvec` | `resetMtvec` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSROutput` | `EX_II` | `exII` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` | `NewCSR.NewCSROutput` | `EX_VI` | `exVI` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/PFEvent.scala` | `NewCSR.PFEvent` | `distribute_csr` | `distributeCsr` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/PMAEntryModule.scala` | `NewCSR.PMAReadWrite` | `get_napot` | `getNapot` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/PMAEntryModule.scala` | `NewCSR.PMAReadWrite` | `shift_addr` | `shiftAddr` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/TrapHandleModule.scala` | `NewCSR.TrapHandleModule` | `m_EX_DT` | `mExDT` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/TrapHandleModule.scala` | `NewCSR.TrapHandleModule` | `s_EX_DT` | `sExDT` |
| `src/main/scala/xiangshan/backend/fu/NewCSR/TrapHandleModule.scala` | `NewCSR.TrapHandleModule` | `vs_EX_DT` | `vsExDT` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `Hgatp_Addr_len` | `hgatpAddrLen` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `Hgatp_Mode_len` | `hgatpModeLen` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `Hgatp_Vmid_len` | `hgatpVmidLen` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_DEBUG` | `irqDebug` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_MEIP` | `irqMeip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_MSIP` | `irqMsip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_MTIP` | `irqMtip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_SEIP` | `irqSeip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_SGEIP` | `irqSgeip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_SSIP` | `irqSsip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_STIP` | `irqStip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_UEIP` | `irqUeip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_USIP` | `irqUsip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_UTIP` | `irqUtip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_VSEIP` | `irqVseip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_VSSIP` | `irqVssip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `IRQ_VSTIP` | `irqVstip` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `Satp_Addr_len` | `satpAddrLen` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `Satp_Asid_len` | `satpAsidLen` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `Satp_Mode_len` | `satpModeLen` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst` | `satp_part_wmask` | `satpPartWmask` |
| `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala` | `util.HasCSRConst.satp_part_wmask` | `max_length` | `maxLength` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSR` | `debug_seqNumReg` | `debugSeqNumReg` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSRInput` | `reset_mtvec` | `resetMtvec` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSRToDecode` | `commit_stuck_check_enable` | `commitStuckCheckEnable` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSRToDecode` | `fusion_enable` | `fusionEnable` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSRToDecode` | `high_density_rob_compression_enable` | `highDensityRobCompressionEnable` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSRToDecode` | `wfi_enable` | `wfiEnable` |
| `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala` | `wrapper.CSRToDecode` | `wrs_nto` | `wrsNto` |

## 引用更新文件

- `src/main/scala/xiangshan/Bundle.scala`
- `src/main/scala/xiangshan/backend/Backend.scala`
- `src/main/scala/xiangshan/backend/CtrlBlock.scala`
- `src/main/scala/xiangshan/backend/fu/CSR.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/CSRAIA.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/CSRBundles.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/CSRIND.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/CSRPermitModule.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/ExceptionBundle.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/HypervisorLevel.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/IndirectCSRPermitModule.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptBundle.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/InterruptFilter.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/PFEvent.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/PMAEntryModule.scala`
- `src/main/scala/xiangshan/backend/fu/NewCSR/TrapHandleModule.scala`
- `src/main/scala/xiangshan/backend/fu/util/CSRConst.scala`
- `src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala`
- `src/main/scala/xiangshan/backend/vector/Decoder/DecodeChannel/SimpleDecodeChannel.scala`
- `src/main/scala/xiangshan/backend/vector/Decoder/DecodeStage.scala`
- `src/main/scala/xiangshan/frontend/Frontend.scala`
- `src/main/scala/xiangshan/mem/MemBlock.scala`
- `src/main/scala/xiangshan/mem/mdp/StoreSet.scala`
- `src/main/scala/xiangshan/mem/mdp/WaitTable.scala`
- `src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala`
- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala`
- `src/main/scala/xiangshan/mem/sbuffer/Sbuffer.scala`
- `src/test/scala/xiangshan/backend/fu/NewCSR/McorepwrCommitStuckCheckTest.scala`
