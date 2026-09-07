simSetSimulator "-vcssv" -exec "./pbmt0_non_nc_10k_multiseed_20260906/exec/simv" \
           -args
debImport "-dbdir" "./pbmt0_non_nc_10k_multiseed_20260906/exec/simv.daidir"
verdiInvokeApp -vdCov
wvCreateWindow
wvSetPosition -win $_nWave2 {("G1" 0)}
wvOpenFile -win $_nWave2 \
           {/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xz_wave.fsdb}
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSignalView -on
srcSignalViewSetFilter "* writebackLda_2_vali"
srcSignalViewSetFilter "*writebackLda_2_valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_valid"
srcSignalViewAddSelectedToWave -win $_nTrace1
srcSignalViewSelect "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_valid"
srcSignalViewAddSelectedToWave -win $_nTrace1
wvSelectSignal -win $_nWave2 {( "G1" 2 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 1)}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 680136.601809 701278.677994
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_mem_to_ooo_writebackLda_2_valid" -line 876 -pos 1 -win \
          $_nTrace1
srcAction -pos 875 3 30 -win $_nTrace1 -name "io_mem_to_ooo_writebackLda_2_valid" \
          -ctrlKey off
wvSetCursor -win $_nWave2 683726.765689 -snap {("G1" 1)}
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldin_ready" -line 67 -pos 1 -win $_nTrace1
srcSelect -signal "io_ldin_valid" -line 68 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 1)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvAddSignal -win $_nWave2 "/top_tb/U_MEMBLOCK/inner_LoadUnit_2/io_ldin_ready" \
           "/top_tb/U_MEMBLOCK/inner_LoadUnit_2/io_ldin_valid"
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetCursor -win $_nWave2 670961.738559 -snap {("G2" 2)}
wvSetCursor -win $_nWave2 676945.345026 -snap {("G2" 1)}
wvSetCursor -win $_nWave2 685920.754727 -snap {("G2" 2)}
wvSetCursor -win $_nWave2 610128.406139 -snap {("G2" 1)}
wvSetCursor -win $_nWave2 606338.788709 -snap {("G2" 2)}
wvSelectSignal -win $_nWave2 {( "G2" 1 )} 
wvSetOptions -win $_nWave2 -hierName on
wvSetOptions -win $_nWave2 -hierName off
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_ready" -line 90 -pos 1 -win $_nTrace1
srcSelect -signal "io_ldout_valid" -line 91 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G1" 1)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvAddSignal -win $_nWave2 "/top_tb/U_MEMBLOCK/inner_LoadUnit_2/io_ldout_ready" \
           "/top_tb/U_MEMBLOCK/inner_LoadUnit_2/io_ldout_valid"
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G3" 2)}
wvSetPosition -win $_nWave2 {("G3" 2)}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_bits_uop_robIdx_flag" -line 103 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_bits_uop_robIdx_value" -line 104 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_bits_uop_robIdx_value" -line 104 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_bits_data" -line 106 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_bits_debug_isMMIO" -line 107 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_ldout_bits_debug_isNCIO" -line 108 -pos 1 -win $_nTrace1
wvSetCursor -win $_nWave2 684038.048254 -snap {("G3" 2)}
wvSelectSignal -win $_nWave2 {( "G3" 2 )} 
wvSetCursor -win $_nWave2 692162.638099 -snap {("G3" 2)}
wvSetCursor -win $_nWave2 686658.883687 -snap {("G3" 2)}
tfgSetPreference -traceNonTrigX TRUE -trXStopAtBlackBox TRUE -trXVCOnly TRUE -trXShowOnTFV TRUE -trXTraceCauses 2  -trXCauseCNT 1  -trXCycleCNT 0
tfgBehaviorAnalysis  -incr -clockSkew 0 -loopUnroll 0 -bboxEmptyModule 0 -bboxIgnoreProtected 0 -cellModel 0 -traceFlattenMDA 0 -confined_flattern 32768
tfgTrX -noBBox -traceNonTrigX -causeCnt 1 -showOnTFG -time 685600 "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_valid#T"
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 664643.866042 708935.984876
wvSetCursor -win $_nWave2 690721.508928 -snap {("G3" 2)}
wvSetCursor -win $_nWave2 685633.577380 -snap {("G3" 2)}
wvSetCursor -win $_nWave2 680083.106601 -snap {("G4" 0)}
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_3
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_3
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_3
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.loadQueueRAW.freeList.io_canAllocate_0_r#620600#T"
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.loadQueueRAW.io_query_2_req_ready#680600#T"
tfgDrag -win $_tFlowView3
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.loadQueueRAW.io_query_2_req_ready_0#680600#T"
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.loadQueueRAW.io_query_2_req_ready_0#680600#T"
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_LoadUnit_2.s2_full_fwd#0#T"
tfgDrag -win $_tFlowView3
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_LoadUnit_2.s2_fwd_mask_0#0#T"
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.io_forwardValid_2#0#T"
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgFolderClick  -funcblk  -win $_tFlowView3 "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.io_forwardValid_2#0#T"
tfgDrag -win $_tFlowView3
tfgDrop -win $_tFlowView3
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.io_forwardValid_2#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.data_0_valid#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.needCheck0Reg_112#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.parallelFwdResult_res_222_valid#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.parallelFwdResult_res_226_valid#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.parallelFwdResult_res_233_valid#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.parallelFwdResult_res_247_valid#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.parallelFwdResult_res_275_valid#0#T"
tfgAddRefSignals -win $_tFlowView3 -ref "top_tb.U_MEMBLOCK.inner_lsq.storeQueue.dataModule.data16_0.parallelFwdResult_res_331_valid#0#T"
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
tfgDrag -win $_tFlowView3
verdiDockWidgetSetCurTab -dock widgetDock_MTB_SOURCE_TAB_1
srcSignalViewFilterByType -all off
srcSignalViewFilterByType -inout on
srcSignalViewSetFilter "*trigger"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_bits_uop_trigger\[3:0\]"
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvSelectGroup -win $_nWave2 {G4}
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_trigger\[3:0\]"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_1_bits_uop_trigger\[3:0\]"
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvAddSignal -win $_nWave2 \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_0_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_1_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_2_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_1_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackVldu_0_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackVldu_1_bits_uop_trigger\[3:0\]"
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G4" 7)}
wvSetPosition -win $_nWave2 {("G4" 7)}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvSetCursor -win $_nWave2 581039.215700 -snap {("G4" 1)}
wvSetCursor -win $_nWave2 587591.304285 -snap {("G4" 1)}
srcSignalViewSetFilter "*writeback*exception*"
srcSignalViewSetFilter "*writeback*exception*3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3"
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 1)}
wvSetPosition -win $_nWave2 {("G1" 0)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvAddSignal -win $_nWave2 \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3"
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSelectSignal -win $_nWave2 {( "G2" 8 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G2" 8 )} 
wvSelectSignal -win $_nWave2 {( "G2" 8 )} 
wvSelectSignal -win $_nWave2 {( "G2" 8 9 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSelectSignal -win $_nWave2 {( "G2" 1 )} 
wvSelectSignal -win $_nWave2 {( "G2" 1 2 3 4 5 6 7 )} 
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 7)}
wvSetPosition -win $_nWave2 {("G5" 7)}
wvZoom -win $_nWave2 557189.613252 632669.673749
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 513406.750018 663442.736314
wvSetCursor -win $_nWave2 596664.025813 -snap {("G4" 1)}
wvSetCursor -win $_nWave2 606659.221155 -snap {("G4" 1)}
wvSetCursor -win $_nWave2 616762.472662 -snap {("G4" 1)}
wvSetCursor -win $_nWave2 626163.359092 -snap {("G4" 1)}
wvCreateWindow
wvSetPosition -win $_nWave5 {("G1" 0)}
wvOpenFile -win $_nWave5 \
           {/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/trigger_gate_output_only_on_20260905/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=666666_rtl_output_only_on.fsdb}
wvSetPrimaryWindow -win $_nWave5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvSetCursor -win $_nWave5 27698.883256
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3"
srcSignalViewSetFilter "*writeback*exception*_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3"
wvSetPosition -win $_nWave2 {("G4" 3)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3"
wvSetPosition -win $_nWave5 {("G1" 0)}
wvSetPosition -win $_nWave5 {("G1" 7)}
wvSetPosition -win $_nWave5 {("G1" 7)}
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvSelectSignal -win $_nWave5 {( "G1" 1 )} 
wvSelectSignal -win $_nWave5 {( "G1" 2 )} 
wvSelectSignal -win $_nWave5 {( "G1" 1 )} 
wvSelectSignal -win $_nWave5 {( "G1" 2 )} 
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
srcSignalViewSetFilter "*mem_to_ooo*writeback_0*"
srcSignalViewSetFilter "*mem_to_ooo*writeback*"
srcSignalViewSetFilter "*mem_to_ooo*writebacksta*"
srcSignalViewSetFilter "*mem_to_ooo*writebackSta*"
srcSignalViewSelect "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value\[7:0\]" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO" \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO"
wvSetPosition -win $_nWave5 {("G2" 0)}
wvAddSignal -win $_nWave5 "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_valid" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value\[7:0\]" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO" \
           "/top_tb/U_MEMBLOCK/io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO"
wvSetPosition -win $_nWave5 {("G2" 0)}
wvSetPosition -win $_nWave5 {("G2" 31)}
wvSetPosition -win $_nWave5 {("G2" 31)}
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvSetCursor -win $_nWave5 719047.537927 -snap {("G2" 1)}
wvZoom -win $_nWave5 698138.239570 734613.348925
wvZoomOut -win $_nWave5
wvSetCursor -win $_nWave5 674070.958689 -snap {("G2" 5)}
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSelectSignal -win $_nWave5 {( "G2" 2 )} 
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 1
wvSelectSignal -win $_nWave5 {( "G1" 4 )} 
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0" -line \
          896 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_mem_to_ooo_writebackSta_0_valid" -line 895 -pos 1 -win \
          $_nTrace1
wvScrollDown -win $_nWave5 0
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSignalViewCaseSensitive off
srcSignalViewCaseSensitive on
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvSelectGroup -win $_nWave2 {G6}
wvSetPrimaryWindow -win $_nWave2
wvSelectGroup -win $_nWave2 {G6}
srcSignalViewFilterByType -output on
srcSignalViewFilterByType -inout off
srcSignalViewSetFilter "* writebackLda_2_vali"
srcSignalViewSetFilter "* "
srcSignalViewSetFilter "*"
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
srcSignalViewSelect "top_tb.U_MEMBLOCK.clock"
srcSignalViewSelect "top_tb.U_MEMBLOCK.reset"
srcSignalViewSelect "top_tb.U_MEMBLOCK.reset"
wvSetPosition -win $_nWave5 {("G1" 0)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvAddSignal -win $_nWave2 "/top_tb/U_MEMBLOCK/reset"
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G6" 1)}
wvSetPosition -win $_nWave2 {("G6" 1)}
wvSetCursor -win $_nWave2 114530.508462 -snap {("G6" 1)}
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvScrollDown -win $_nWave5 1
wvSelectSignal -win $_nWave5 {( "G1" 4 )} 
wvSelectSignal -win $_nWave5 {( "G2" 29 )} 
wvSelectSignal -win $_nWave5 {( "G2" 30 )} 
wvSelectSignal -win $_nWave5 {( "G2" 31 )} 
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvSetCursor -win $_nWave5 684913.940234 -snap {("G2" 23)}
wvSetCursor -win $_nWave5 64367.630795 -snap {("G2" 16)}
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvSetCursor -win $_nWave5 721083.529058 -snap {("G2" 1)}
wvZoom -win $_nWave5 703160.785571 734586.965932
wvSetCursor -win $_nWave5 725402.627984 -snap {("G2" 1)}
wvSetCursor -win $_nWave5 720238.405961 -snap {("G2" 1)}
wvSelectSignal -win $_nWave5 {( "G1" 1 )} 
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollUp -win $_nWave5 1
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvSelectSignal -win $_nWave5 {( "G2" 26 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 28 )} 
wvSelectSignal -win $_nWave5 {( "G2" 30 )} 
wvSelectSignal -win $_nWave5 {( "G2" 30 )} 
wvSelectSignal -win $_nWave5 {( "G2" 31 )} 
wvSelectSignal -win $_nWave5 {( "G2" 28 )} 
wvZoom -win $_nWave5 709789.745491 733850.414830
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
wvSetCursor -win $_nWave5 724920.122230 -snap {("G2" 29)}
wvSetCursor -win $_nWave5 720355.506270 -snap {("G2" 28)}
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvScrollUp -win $_nWave5 1
wvSetCursor -win $_nWave5 720355.506270 -snap {("G1" 4)}
wvSetCursor -win $_nWave5 725177.283692 -snap {("G1" 4)}
wvSetCursor -win $_nWave5 721769.894314 -snap {("G1" 4)}
wvSelectSignal -win $_nWave5 {( "G1" 1 )} 
wvSelectSignal -win $_nWave5 {( "G1" 4 )} 
wvSelectSignal -win $_nWave5 {( "G1" 6 )} 
wvSelectSignal -win $_nWave5 {( "G1" 7 )} 
wvScrollDown -win $_nWave5 1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_StoreUnit_0" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_StoreUnit_0" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_StoreUnit_0" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_StoreUnit_0" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_stin_ready" -line 66 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_valid" -line 67 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {67 73 4 10 9 11}
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_stin_ready" -line 66 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_valid" -line 67 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_uop_imm" -line 69 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_uop_fuOpType" -line 68 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_uop_robIdx_flag" -line 70 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_uop_robIdx_value" -line 71 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_uop_sqIdx_flag" -line 72 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_uop_sqIdx_value" -line 73 -pos 1 -win $_nTrace1
srcSelect -signal "io_stin_bits_src_0" -line 74 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave5 {("G3" 0)}
wvAddSignal -win $_nWave5 "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_ready" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_valid" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_uop_imm\[31:0\]" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_uop_fuOpType\[8:0\]" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_uop_robIdx_flag" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_uop_robIdx_value\[7:0\]" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_uop_sqIdx_flag" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_uop_sqIdx_value\[5:0\]" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stin_bits_src_0\[63:0\]"
wvSetPosition -win $_nWave5 {("G3" 0)}
wvSetPosition -win $_nWave5 {("G3" 9)}
wvSetPosition -win $_nWave5 {("G3" 9)}
wvScrollDown -win $_nWave5 2
wvScrollDown -win $_nWave5 0
wvScrollDown -win $_nWave5 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_misalign_enq_req_ready" -line 294 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_misalign_enq_req_bits_uop_exceptionVec_0" -line 296 -pos 1 \
          -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_misalign_enq_req_ready" -line 294 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_misalign_enq_req_valid" -line 295 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_vecstin_ready" -line 268 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_stout_valid" -line 234 -pos 1 -win $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_3" -line 235 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_6" -line 236 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_7" -line 237 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_15" -line 238 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_19" -line 239 -pos 1 -win \
          $_nTrace1
srcSelect -toggle -signal "io_stout_bits_uop_exceptionVec_19" -line 239 -pos 1 \
          -win $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_19" -line 239 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_uop_exceptionVec_23" -line 240 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_uop_trigger" -line 241 -pos 1 -win $_nTrace1
srcSelect -signal "io_stout_bits_uop_robIdx_flag" -line 242 -pos 1 -win $_nTrace1
srcSelect -signal "io_stout_bits_uop_robIdx_value" -line 243 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_stout_bits_debug_isMMIO" -line 244 -pos 1 -win $_nTrace1
srcSelect -signal "io_stout_bits_debug_isNCIO" -line 245 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave5 {("G1" 0)}
wvSetPosition -win $_nWave5 {("G4" 0)}
wvAddSignal -win $_nWave5 "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_valid" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_exceptionVec_6" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_exceptionVec_7" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_exceptionVec_15" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_exceptionVec_19" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_exceptionVec_23" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_robIdx_flag" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_uop_robIdx_value\[7:0\]" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_debug_isMMIO" \
           "/top_tb/U_MEMBLOCK/inner_StoreUnit_0/io_stout_bits_debug_isNCIO"
wvSetPosition -win $_nWave5 {("G4" 0)}
wvSetPosition -win $_nWave5 {("G4" 12)}
wvSetPosition -win $_nWave5 {("G4" 12)}
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvSelectSignal -win $_nWave5 {( "G4" 1 )} 
wvSelectSignal -win $_nWave5 {( "G3" 1 )} 
wvSetCursor -win $_nWave5 612565.000000 -snap {("G3" 1)}
wvZoom -win $_nWave5 568126.416834 714209.052104
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvSelectSignal -win $_nWave5 {( "G4" 1 )} 
wvScrollUp -win $_nWave5 9
wvScrollUp -win $_nWave5 13
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSetCursor -win $_nWave5 720255.195887 -snap {("G2" 1)}
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G4" 12)}
srcTraceConnectivity "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid" -win \
           $_nTrace1
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G4" 12)}
srcTraceConnectivity "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid" -win \
           $_nTrace1
wvSelectSignal -win $_nWave5 {( "G2" 2 )} 
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G4" 12)}
srcTraceConnectivity \
           "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0" \
           -win $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G2" 1)}
wvMoveSelected -win $_nWave5
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G2" 0)}
wvSetPosition -win $_nWave5 {("G2" 2)}
srcTraceConnectivity "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid" -win \
           $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_mem_to_ooo_writebackSta_0_valid" -line 30495 -pos 1 -win \
          $_nTrace1
srcAction -pos 30494 3 15 -win $_nTrace1 -name \
          "io_mem_to_ooo_writebackSta_0_valid" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "_inner_StoreUnit_0_io_stout_valid" -line 30496 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "inner__7" -line 30496 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "inner_" -line 30496 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "_inner_StoreUnit_0_io_stout_valid" -line 30496 -pos 1 -win \
          $_nTrace1
srcAction -pos 30495 9 7 -win $_nTrace1 -name "_inner_StoreUnit_0_io_stout_valid" \
          -ctrlKey off
wvSetCursor -win $_nWave5 727471.561047 -snap {("G2" 1)}
wvSetCursor -win $_nWave5 720450.232783 -snap {("G2" 1)}
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G2" 2)}
srcTraceConnectivity "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid" -win \
           $_nTrace1
wvSetPrimaryWindow -win $_nWave5
wvSetPosition -win $_nWave5 {("G2" 1)}
srcTraceConnectivity "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid" -win \
           $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
nsMsgSelect -range {4-4}
nsMsgSelect -range {4 0-0}
nsMsgSelect -range {4-4}
nsMsgSelect -range {4 0-0}
nsMsgSelect -range {4 1-1}
nsMsgSelect -range {4 0-0}
nsMsgAction -tab trace -index {4 0}
nsMsgAction -tab trace -index {4 0}
nsMsgSelect -range {4-4}
nsMsgSelect -range {4 0-0}
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvScrollDown -win $_nWave5 10
wvScrollDown -win $_nWave5 7
wvSelectSignal -win $_nWave5 {( "G4" 1 )} 
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvScrollDown -win $_nWave5 0
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G2" 2)}
srcSignalViewSetFilter "io_mem_to_ooo_writebackSta_0_valid"
srcSignalViewFilterByType -inout on
srcSignalViewSelect "top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid"
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_mem_to_ooo_writebackSta_0_valid" -line 895 -pos 1 -win \
          $_nTrace1
srcAction -pos 894 3 26 -win $_nTrace1 -name "io_mem_to_ooo_writebackSta_0_valid" \
          -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "inner__7" -line 30496 -pos 1 -win $_nTrace1
srcAction -pos 30495 1 4 -win $_nTrace1 -name "inner__7" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "_inner_storeMisalignBuffer_io_writeBack_valid" -line 6615 -pos \
          1 -win $_nTrace1
srcBackwardHistory -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "inner__7" -line 30496 -pos 1 -win $_nTrace1
srcAction -pos 30495 1 5 -win $_nTrace1 -name "inner__7" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "_inner_storeMisalignBuffer_io_writeBack_valid" -line 6615 -pos \
          1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "_inner_storeMisalignBuffer_io_writeBack_valid" -line 6615 -pos \
          1 -win $_nTrace1
srcAction -pos 6614 7 31 -win $_nTrace1 -name \
          "_inner_storeMisalignBuffer_io_writeBack_valid" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_writeBack_valid_0" -line 1528 -pos 1 -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -delim "." -win \
           $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
wvScrollDown -win $_nWave5 18
wvSelectSignal -win $_nWave5 {( "G3" 1 )} 
wvScrollDown -win $_nWave5 6
wvSelectSignal -win $_nWave5 {( "G3" 1 2 3 4 5 6 7 8 9 )} {( "G4" 1 2 3 4 5 6 \
           7 8 9 10 11 12 )} 
wvCut -win $_nWave5
wvSetPosition -win $_nWave5 {("G2" 2)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_writeBack_valid" -line 200 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_writeBack_ready" -line 199 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {199 211 4 4 15 22}
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_writeBack_ready" -line 199 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {199 211 4 4 13 24}
wvSetPosition -win $_nWave5 {("G2" 11)}
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G3" 0)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_valid" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_exceptionVec_3" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_exceptionVec_6" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_exceptionVec_7" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_exceptionVec_15" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_exceptionVec_19" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_exceptionVec_23" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_trigger\[3:0\]" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_robIdx_flag" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_uop_robIdx_value\[7:0\]" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_writeBack_bits_debug_isMMIO"
wvSetPosition -win $_nWave5 {("G3" 0)}
wvSetPosition -win $_nWave5 {("G3" 11)}
wvSelectSignal -win $_nWave5 {( "G3" 1 )} 
wvSelectSignal -win $_nWave5 {( "G3" 2 )} 
wvSelectSignal -win $_nWave5 {( "G3" 5 )} 
wvSelectSignal -win $_nWave5 {( "G3" 8 )} 
wvSelectSignal -win $_nWave5 {( "G3" 9 )} 
wvSelectSignal -win $_nWave5 {( "G3" 10 )} 
wvSelectSignal -win $_nWave5 {( "G3" 11 )} 
wvSelectSignal -win $_nWave5 {( "G3" 10 )} 
wvSelectSignal -win $_nWave5 {( "G3" 8 )} 
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -delim "." -win \
           $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcSignalViewFilterByType -inout off
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_ready" -line 65 -pos 1 -win $_nTrace1
srcSelect -signal "io_enq_0_req_valid" -line 66 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 18)}
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G3" 0)}
wvSetPosition -win $_nWave5 {("G3" 2)}
wvSetPosition -win $_nWave5 {("G3" 4)}
wvSetPosition -win $_nWave5 {("G3" 7)}
wvSetPosition -win $_nWave5 {("G3" 9)}
wvSetPosition -win $_nWave5 {("G3" 10)}
wvSetPosition -win $_nWave5 {("G3" 11)}
wvSetPosition -win $_nWave5 {("G5" 0)}
wvSetPosition -win $_nWave5 {("G4" 0)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_0_req_ready" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_0_req_valid"
wvSetPosition -win $_nWave5 {("G4" 0)}
wvSetPosition -win $_nWave5 {("G4" 2)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_1_req_valid" -line 104 -pos 1 -win $_nTrace1
srcSelect -signal "io_enq_1_req_ready" -line 103 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 21)}
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G4" 2)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_1_req_valid" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_1_req_ready"
wvSetPosition -win $_nWave5 {("G4" 2)}
wvSetPosition -win $_nWave5 {("G4" 4)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_1_req_ready" -line 103 -pos 1 -win $_nTrace1
srcAction -pos 102 3 14 -win $_nTrace1 -name "io_enq_1_req_ready" -ctrlKey off
srcBackwardHistory -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_1_req_valid" -line 104 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_1_req_valid" -line 104 -pos 1 -win $_nTrace1
srcAction -pos 103 3 13 -win $_nTrace1 -name "io_enq_1_req_valid" -ctrlKey off
srcBackwardHistory -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_StoreUnit_1" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_ready" -line 65 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_valid" -line 66 -pos 1 -win $_nTrace1
srcAction -pos 65 3 11 -win $_nTrace1 -name "io_enq_0_req_valid" -ctrlKey off
wvSelectSignal -win $_nWave5 {( "G2" 12 )} 
wvSelectSignal -win $_nWave5 {( "G2" 29 )} 
wvSelectSignal -win $_nWave5 {( "G2" 28 )} 
wvSelectSignal -win $_nWave5 {( "G2" 29 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 29 )} 
srcBackwardHistory -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_StoreUnit_0" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_bits_uop_uopIdx" -line 90 -pos 1 -win $_nTrace1
srcSelect -signal "io_enq_0_req_bits_uop_robIdx_flag" -line 91 -pos 1 -win \
          $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G3" 11)}
wvSetPosition -win $_nWave5 {("G4" 1)}
wvSetPosition -win $_nWave5 {("G4" 2)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_0_req_bits_uop_uopIdx\[6:0\]" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_0_req_bits_uop_robIdx_flag"
wvSetPosition -win $_nWave5 {("G4" 2)}
wvSetPosition -win $_nWave5 {("G4" 4)}
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_1_req_bits_uop_uopIdx" -line 128 -pos 1 -win $_nTrace1
srcSelect -signal "io_enq_1_req_bits_uop_robIdx_flag" -line 129 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_1_req_bits_uop_robIdx_value" -line 130 -pos 1 -win \
          $_nTrace1
srcSelect -signal "io_enq_1_req_bits_uop_robIdx_flag" -line 129 -pos 1 -win \
          $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G5" 0)}
wvSetPosition -win $_nWave5 {("G4" 6)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_1_req_bits_uop_robIdx_value\[7:0\]" \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_1_req_bits_uop_robIdx_flag"
wvSetPosition -win $_nWave5 {("G4" 6)}
wvSetPosition -win $_nWave5 {("G4" 8)}
wvSelectSignal -win $_nWave5 {( "G4" 3 )} 
wvSetPosition -win $_nWave5 {("G4" 3)}
wvSetPosition -win $_nWave5 {("G4" 8)}
wvSetPosition -win $_nWave5 {("G2" 18)}
wvSetPosition -win $_nWave5 {("G4" 8)}
srcTraceConnectivity \
           "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer.io_enq_0_req_bits_uop_uopIdx\[6:0\]" \
           -win $_nTrace1
srcBackwardHistory -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_storeMisalignBuffer" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_bits_uop_trigger" -line 86 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_bits_uop_robIdx_flag" -line 91 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "io_enq_0_req_bits_uop_robIdx_value" -line 92 -pos 1 -win \
          $_nTrace1
wvSetPosition -win $_nWave5 {("G2" 12)}
wvSetPosition -win $_nWave5 {("G4" 8)}
wvSetPosition -win $_nWave5 {("G4" 2)}
wvSetPosition -win $_nWave5 {("G4" 3)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/inner_storeMisalignBuffer/io_enq_0_req_bits_uop_robIdx_value\[7:0\]"
wvSetPosition -win $_nWave5 {("G4" 3)}
wvSetPosition -win $_nWave5 {("G4" 4)}
wvSelectSignal -win $_nWave5 {( "G4" 2 )} 
wvSelectSignal -win $_nWave5 {( "G4" 3 )} 
wvSelectSignal -win $_nWave5 {( "G4" 4 )} 
wvSelectSignal -win $_nWave5 {( "G2" 27 )} 
wvSelectSignal -win $_nWave5 {( "G2" 28 )} 
wvSelectSignal -win $_nWave5 {( "G2" 29 )} 
wvSelectSignal -win $_nWave5 {( "G2" 28 )} 
wvSelectSignal -win $_nWave5 {( "G2" 29 )} 
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvSelectSignal -win $_nWave5 {( "G4" 3 )} 
wvCut -win $_nWave5
wvSetPosition -win $_nWave5 {("G4" 4)}
wvSetPosition -win $_nWave5 {("G4" 3)}
wvSelectSignal -win $_nWave5 {( "G4" 3 )} 
wvSetCursor -win $_nWave5 669350.565973 -snap {("G4" 2)}
wvSelectSignal -win $_nWave5 {( "G4" 4 )} 
wvSetCursor -win $_nWave5 676566.931133 -snap {("G4" 2)}
wvSetCursor -win $_nWave5 670325.750454 -snap {("G4" 2)}
wvSetCursor -win $_nWave5 719475.048302 -snap {("G3" 1)}
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvSetCursor -win $_nWave2 687183.050774 -snap {("G3" 2)}
wvSelectSignal -win $_nWave2 {( "G1" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 2 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSetOptions -win $_nWave2 -hierName on
wvSetOptions -win $_nWave2 -hierName off
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_1" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_LoadUnit_1" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_1" -win $_nTrace1
srcSignalViewCaseSensitive off
srcSignalViewFilterByType -inout on
srcSignalViewSetFilter "*valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_1.io_ldin_valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_1.io_ldin_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_1.io_ldout_valid"
wvSetPosition -win $_nWave5 {("G4" 8)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvAddSignal -win $_nWave2 "/top_tb/U_MEMBLOCK/inner_LoadUnit_1/io_ldin_valid" \
           "/top_tb/U_MEMBLOCK/inner_LoadUnit_1/io_ldout_valid"
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G7" 2)}
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_0" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_LoadUnit_0" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_0" -win $_nTrace1
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_0.io_ldin_valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_0.io_ldin_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_0.io_ldout_valid"
wvSetPosition -win $_nWave2 {("G8" 0)}
wvAddSignal -win $_nWave2 "/top_tb/U_MEMBLOCK/inner_LoadUnit_0/io_ldin_valid" \
           "/top_tb/U_MEMBLOCK/inner_LoadUnit_0/io_ldout_valid"
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 2)}
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldin_valid"
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldin_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_valid"
wvSetPosition -win $_nWave2 {("G9" 0)}
wvAddSignal -win $_nWave2 "/top_tb/U_MEMBLOCK/inner_LoadUnit_2/io_ldin_valid" \
           "/top_tb/U_MEMBLOCK/inner_LoadUnit_2/io_ldout_valid"
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 2 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSelectSignal -win $_nWave2 {( "G1" 1 )} 
wvSetPosition -win $_nWave2 {("G1" 1)}
wvSetPosition -win $_nWave2 {("G9" 1)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetCursor -win $_nWave2 686658.883687 -snap {("G9" 2)}
wvSelectSignal -win $_nWave2 {( "G9" 1 )} 
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G9" 2 )} 
wvSelectSignal -win $_nWave2 {( "G9" 1 )} 
wvSelectSignal -win $_nWave2 {( "G9" 2 )} 
wvSetCursor -win $_nWave2 687387.505441 -snap {("G9" 2)}
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvSetPrimaryWindow -win $_nWave2
wvSetCursor -win $_nWave2 691483.974484 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 685202.721951 -snap {("G9" 2)}
tfgSetPreference -traceNonTrigX TRUE -trXStopAtBlackBox TRUE -trXVCOnly TRUE -trXShowOnTFV TRUE -trXTraceCauses 1  -trXCauseCNT 1  -trXCycleCNT 0
tfgBehaviorAnalysis  -incr -clockSkew 0 -loopUnroll 0 -bboxEmptyModule 0 -bboxIgnoreProtected 0 -cellModel 0 -traceFlattenMDA 0 -confined_flattern 32768
tfgTrX -noBBox -traceNonTrigX -showOnTFG -time 685600 "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_valid#T"
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
srcSignalViewSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldin_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ldout_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_vecldin_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_vecldout_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_misalign_ldin_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_misalign_ldout_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_tlb_req_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_tlb_resp_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_dcache_req_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_sbuffer_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_sbuffer_matchInvalid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ubuffer_valid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_ubuffer_matchInvalid" \
           "top_tb.U_MEMBLOCK.inner_LoadUnit_2.io_lsq_ldin_valid"
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
verdiDockWidgetHide -dock widgetDock_<Decl._Tree>
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
verdiWindowResize -win $_Verdi_1 "291" "636" "1292" "700"
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK.inner_LoadUnit_2" -win $_nTrace1
verdiWindowResize -win $_Verdi_1 "291" "636" "900" "700"
wvSetCursor -win $_nWave2 111.898477 -snap {("G4" 4)}
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoom -win $_nWave5 684109.878419 726693.920973
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomOut -win $_nWave5
wvZoomIn -win $_nWave5
wvZoomOut -win $_nWave5
wvSelectSignal -win $_nWave5 {( "G3" 1 )} 
wvSelectSignal -win $_nWave5 {( "G2" 28 )} 
wvScrollUp -win $_nWave5 14
wvScrollUp -win $_nWave5 13
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvSelectSignal -win $_nWave5 {( "G2" 6 )} 
wvSelectSignal -win $_nWave5 {( "G2" 6 7 8 9 10 11 12 13 14 15 16 17 )} 
wvCut -win $_nWave5
wvSetPosition -win $_nWave5 {("G4" 8)}
wvSelectSignal -win $_nWave5 {( "G2" 6 )} 
wvSelectSignal -win $_nWave5 {( "G2" 6 7 8 9 10 11 12 13 )} 
wvCut -win $_nWave5
wvSetPosition -win $_nWave5 {("G4" 8)}
wvSelectSignal -win $_nWave5 {( "G2" 7 )} 
wvSelectSignal -win $_nWave5 {( "G2" 9 )} 
wvSelectSignal -win $_nWave5 {( "G2" 9 )} 
wvSelectSignal -win $_nWave5 {( "G2" 7 )} 
wvSelectSignal -win $_nWave5 {( "G2" 6 )} 
wvSelectSignal -win $_nWave5 {( "G2" 8 )} 
wvSelectSignal -win $_nWave5 {( "G2" 9 )} 
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollUp -win $_nWave5 1
wvScrollDown -win $_nWave5 1
wvScrollUp -win $_nWave5 4
srcSignalViewFilterByType -inout off
srcSignalViewSetFilter "csr*"
srcSignalViewSetFilter "*csr*"
srcSignalViewSetFilter "*csr*trigger*"
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSetScope "top_tb.U_MEMBLOCK" -delim "." -win $_nTrace1
srcHBSelect "top_tb.U_MEMBLOCK" -win $_nTrace1
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid"
srcSignalViewSelect \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr\[1:0\]" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType\[1:0\]" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action\[3:0\]" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2\[63:0\]" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode" \
           "top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp"
wvSetPosition -win $_nWave2 {("G10" 0)}
wvSetPosition -win $_nWave5 {("G2" 4)}
wvSetPosition -win $_nWave5 {("G2" 3)}
wvSetPosition -win $_nWave5 {("G2" 2)}
wvSetPosition -win $_nWave5 {("G2" 1)}
wvSetPosition -win $_nWave5 {("G2" 0)}
wvSetPosition -win $_nWave5 {("G1" 7)}
wvSetPosition -win $_nWave5 {("G1" 6)}
wvSetPosition -win $_nWave5 {("G1" 5)}
wvSetPosition -win $_nWave5 {("G1" 6)}
wvSetPosition -win $_nWave5 {("G1" 7)}
wvSetPosition -win $_nWave5 {("G2" 0)}
wvSetPosition -win $_nWave5 {("G1" 7)}
wvSetPosition -win $_nWave5 {("G1" 4)}
wvSetPosition -win $_nWave5 {("G1" 3)}
wvSetPosition -win $_nWave5 {("G1" 2)}
wvSetPosition -win $_nWave5 {("G1" 1)}
wvSetPosition -win $_nWave5 {("G1" 0)}
wvSelectSignal -win $_nWave5 {( "G2" 1 )} 
wvSelectSignal -win $_nWave5 {( "G1" 2 )} 
wvScrollDown -win $_nWave5 0
wvSelectSignal -win $_nWave5 {( "G1" 1 )} 
wvScrollUp -win $_nWave5 1
wvSelectSignal -win $_nWave5 {( "G1" 1 2 3 4 5 6 7 )} 
wvCut -win $_nWave5
wvSetPosition -win $_nWave5 {("G1" 0)}
wvAddSignal -win $_nWave5 \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr\[1:0\]" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType\[1:0\]" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action\[3:0\]" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2\[63:0\]" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_debugMode" \
           "/top_tb/U_MEMBLOCK/io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp"
wvSetPosition -win $_nWave5 {("G1" 0)}
wvSetPosition -win $_nWave5 {("G1" 15)}
wvGetSignalOpen -win $_nWave5
wvGetSignalSetScope -win $_nWave5 "/top_tb"
wvSetCursor -win $_nWave5 720300.000000
srcSignalView -off
verdiDockWidgetMaximize -dock windowDock_nWave_5
wvCreateWindow
verdiDockWidgetSetCurTab -dock windowDock_nWave_8
wvCloseWindow -win $_nWave8
srcSignalView -on
srcSignalView -off
srcSignalView -on
verdiDockWidgetRestore -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_6
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvScrollDown -win $_nWave2 0
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_6
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_6
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiWindowResize -win $_Verdi_1 "0" "34" "2160" "1334"
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock windowDock_tFlowView_6
tfgLevelClick -win $_tFlowView6  -level 999993
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
tfgDrag -win $_tFlowView6
