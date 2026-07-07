# V2 整核与 V3 MemBlock 逐端口差异清单

本文为机器生成的逐端口清单，配合 `memblock_v2whole_v3_memblock_interface_delta_20260707.md` 使用。

## 1. 统计

- V2-only：568
- V3-only：921
- 同名但方向/位宽不同：25

## 2. V2-only 端口

| 接口族 | 端口 | 方向 | 位宽 |
| --- | --- | --- | --- |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_hd_misalign_st_enable` | input | `[0:0]` |
| CSR/TLB CSR | `io_ooo_to_mem_tlbCsr_priv_debug` | input | `[0:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay` | output | `[0:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask` | output | `[15:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx` | output | `[3:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay` | output | `[0:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask` | output | `[15:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx` | output | `[3:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_flushPipe` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_fuOpType` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_lastUop` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_trigger` | input | `[3:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_flushPipe` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_fuOpType` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_lastUop` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_trigger` | input | `[3:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_flushPipe` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_fuOpType` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_lastUop` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_trigger` | input | `[3:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_flushPipe` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_fuOpType` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_lastUop` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_trigger` | input | `[3:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_flushPipe` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_fuOpType` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_lastUop` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_trigger` | input | `[3:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_flushPipe` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_fuOpType` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_lastUop` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_trigger` | input | `[3:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value` | output | `[7:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value` | output | `[7:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value` | output | `[7:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_loadMmio_0` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_loadMmio_1` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_loadMmio_2` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_storeMmio` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value` | output | `[7:0]` |
| LSQ 状态/反馈 lsqio | `io_ooo_to_mem_lsqio_pendingMMIOld` | input | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_ooo_to_mem_lsqio_pendingst` | input | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_ooo_to_mem_lsqio_scommit` | input | `[3:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_out_a_bits_user_needHint` | output | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_src_0` | input | `[63:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_fpWen` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset` | input | `[3:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value` | input | `[5:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_fuOpType` | input | `[8:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_imm` | input | `[31:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value` | input | `[6:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_pc` | input | `[49:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_pdest` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_rfWen` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_ready` | output | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_0_valid` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_src_0` | input | `[63:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_fpWen` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset` | input | `[3:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value` | input | `[5:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_fuOpType` | input | `[8:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_imm` | input | `[31:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value` | input | `[6:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_pc` | input | `[49:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_pdest` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_rfWen` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_ready` | output | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_1_valid` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_src_0` | input | `[63:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_fpWen` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset` | input | `[3:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value` | input | `[5:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_fuOpType` | input | `[8:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_imm` | input | `[31:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value` | input | `[6:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_pc` | input | `[49:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_pdest` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_rfWen` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag` | input | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value` | input | `[7:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_ready` | output | `[0:0]` |
| V2 load issue issueLda | `io_ooo_to_mem_issueLda_2_valid` | input | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_data` | output | `[63:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_fpWen` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_pdest` | output | `[7:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_replayInst` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_rfWen` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_bits_uop_trigger` | output | `[3:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_0_valid` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_data` | output | `[63:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_fpWen` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_pdest` | output | `[7:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_replayInst` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_rfWen` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_bits_uop_trigger` | output | `[3:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_1_valid` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_data` | output | `[63:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_fpWen` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_pdest` | output | `[7:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_replayInst` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_rfWen` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_bits_uop_trigger` | output | `[3:0]` |
| V2 load writeback writebackLda | `io_mem_to_ooo_writebackLda_2_valid` | output | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_src_0` | input | `[63:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_fuOpType` | input | `[8:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_fuType` | input | `[34:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_imm` | input | `[31:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_pdest` | input | `[7:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_rfWen` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_ready` | output | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_0_valid` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_src_0` | input | `[63:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_fuOpType` | input | `[8:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_fuType` | input | `[34:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_imm` | input | `[31:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_pdest` | input | `[7:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_rfWen` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_ready` | output | `[0:0]` |
| V2 store address issue issueSta | `io_ooo_to_mem_issueSta_1_valid` | input | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_bits_uop_trigger` | output | `[3:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_0_valid` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_bits_uop_trigger` | output | `[3:0]` |
| V2 store address writeback writebackSta | `io_mem_to_ooo_writebackSta_1_valid` | output | `[0:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_bits_src_0` | input | `[63:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_bits_uop_fuOpType` | input | `[8:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_bits_uop_fuType` | input | `[34:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_ready` | output | `[0:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_0_valid` | input | `[0:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_bits_src_0` | input | `[63:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_bits_uop_fuOpType` | input | `[8:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_bits_uop_fuType` | input | `[34:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_ready` | output | `[0:0]` |
| V2 store data issue issueStd | `io_ooo_to_mem_issueStd_1_valid` | input | `[0:0]` |
| V2 store data writeback writebackStd | `io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 store data writeback writebackStd | `io_mem_to_ooo_writebackStd_0_valid` | output | `[0:0]` |
| V2 store data writeback writebackStd | `io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 store data writeback writebackStd | `io_mem_to_ooo_writebackStd_1_valid` | output | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_flowNum` | input | `[4:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_src_0` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_src_1` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_src_2` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_src_3` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_src_4` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset` | input | `[3:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value` | input | `[5:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType` | input | `[8:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_fuType` | input | `[34:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value` | input | `[6:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_pdest` | input | `[7:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vecWen` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vlWen` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf` | input | `[2:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew` | input | `[1:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul` | input | `[2:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew` | input | `[1:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart` | input | `[7:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx` | input | `[6:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_vecReplayMask` | input | `[15:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx` | input | `[3:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_ready` | output | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_0_valid` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_flowNum` | input | `[4:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_src_0` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_src_1` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_src_2` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_src_3` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_src_4` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset` | input | `[3:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value` | input | `[5:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType` | input | `[8:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value` | input | `[6:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_pdest` | input | `[7:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value` | input | `[7:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value` | input | `[5:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vecWen` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vlWen` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf` | input | `[2:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew` | input | `[1:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul` | input | `[2:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask` | input | `[127:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew` | input | `[1:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart` | input | `[7:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta` | input | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx` | input | `[6:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_vecReplayMask` | input | `[15:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx` | input | `[3:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_ready` | output | `[0:0]` |
| V2 vector load issue issueVldu | `io_ooo_to_mem_issueVldu_1_valid` | input | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_data` | output | `[127:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType` | output | `[8:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_pdest` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_trigger` | output | `[3:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew` | output | `[1:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask` | output | `[127:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew` | output | `[1:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx` | output | `[6:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_vdIdx` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_0_valid` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_data` | output | `[127:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType` | output | `[8:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_pdest` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_trigger` | output | `[3:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew` | output | `[1:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask` | output | `[127:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew` | output | `[1:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart` | output | `[7:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta` | output | `[0:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx` | output | `[6:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_vdIdx` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField` | output | `[2:0]` |
| V2 vector load writeback writebackVldu | `io_mem_to_ooo_writebackVldu_1_valid` | output | `[0:0]` |
| backendToTopBypass | `io_ooo_to_mem_backendToTopBypass_cpuHalted` | input | `[0:0]` |
| sfence | `io_ooo_to_mem_sfence_bits_flushPipe` | input | `[0:0]` |
| store exception control | `io_ooo_to_mem_isStoreException` | input | `[0:0]` |
| 其他 io | `io_outer_cpu_halt` | output | `[0:0]` |
| 其他 io | `io_reset_backend` | output | `[0:0]` |

## 3. V3-only 端口

| 接口族 | 端口 | 方向 | 位宽 |
| --- | --- | --- | --- |
| CPU halt/WFI/control | `io_outer_cpu_wfi` | output | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing` | input | `[0:0]` |
| CSR control | `io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing` | input | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState` | output | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag` | output | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value` | output | `[6:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag` | output | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value` | output | `[8:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType` | output | `[3:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState` | output | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag` | output | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value` | output | `[6:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag` | output | `[0:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value` | output | `[8:0]` |
| IQ feedback staIqFeedback | `io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType` | output | `[3:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value` | output | `[6:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value` | output | `[8:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType` | output | `[3:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value` | output | `[5:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value` | output | `[6:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value` | output | `[8:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType` | output | `[3:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag` | output | `[0:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value` | output | `[5:0]` |
| IQ feedback vlduIqFeedback | `io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid` | output | `[0:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag` | output | `[0:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value` | output | `[8:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType` | output | `[3:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag` | output | `[0:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value` | output | `[8:0]` |
| IQ feedback vstuIqFeedback | `io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType` | output | `[3:0]` |
| L2TLB/PMP | `io_l2_pmp_resp_atomic` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_pmp_resp_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_pmp_resp_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_checkfullva` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_debug_isFirstIssue` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_debug_robIdx_flag` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_debug_robIdx_value` | input | `[8:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_fullva` | input | `[63:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_hlvx` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_hyperinst` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_memidx_idx` | input | `[6:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_memidx_is_ld` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_memidx_is_st` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_bits_pmp_addr` | input | `[47:0]` |
| L2TLB/PMP | `io_l2_tlb_req_req_kill` | input | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_debug_isFirstIssue` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_debug_robIdx_flag` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_debug_robIdx_value` | output | `[8:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_af_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_af_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_gpf_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_gpf_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_isHyper` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_pf_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_pf_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_0_vaNeedExt` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_af_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_af_ld` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_af_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_gpf_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_gpf_ld` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_gpf_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_isHyper` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_pf_instr` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_pf_ld` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_pf_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_excp_1_vaNeedExt` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_fastMiss` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_fullva` | output | `[63:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_gpaddr_0` | output | `[63:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_gpaddr_1` | output | `[63:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_isForVSnonLeafPTE` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_memidx_idx` | output | `[6:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_memidx_is_ld` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_memidx_is_st` | output | `[0:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_paddr_1` | output | `[47:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_pbmt_1` | output | `[1:0]` |
| L2TLB/PMP | `io_l2_tlb_req_resp_bits_ptwBack` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_0_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_0_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_1_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_1_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_2_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_2_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_3_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_3_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_4_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_4_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_5_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_5_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_6_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_6_sqIdx_value` | output | `[5:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_7_lqIdx_value` | output | `[6:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag` | output | `[0:0]` |
| LSQ 入队响应 enqLsq_resp | `io_ooo_to_mem_enqLsq_resp_7_sqIdx_value` | output | `[5:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_fuType` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value` | input | `[6:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_numLsElem` | input | `[4:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value` | input | `[5:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_bits_uopIdx` | input | `[6:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_6_valid` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_fuType` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value` | input | `[6:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_numLsElem` | input | `[4:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag` | input | `[0:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value` | input | `[5:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_bits_uopIdx` | input | `[6:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_7_valid` | input | `[0:0]` |
| LSQ 分配需求 enqLsq_needAlloc | `io_ooo_to_mem_enqLsq_needAlloc_6` | input | `[1:0]` |
| LSQ 分配需求 enqLsq_needAlloc | `io_ooo_to_mem_enqLsq_needAlloc_7` | input | `[1:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_lqCanAccept` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_mmioBusy` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_sqCanAccept` | output | `[0:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_vl` | output | `[7:0]` |
| LSQ 状态/反馈 lsqio | `io_mem_to_ooo_lsqio_vstart` | output | `[7:0]` |
| TileLink/auto | `auto_inner_buffers_out_a_bits_user_memBackType_MM` | output | `[0:0]` |
| TileLink/auto | `auto_inner_buffers_out_a_bits_user_memPageType_NC` | output | `[0:0]` |
| TileLink/auto | `auto_inner_dcache_client_out_a_bits_user_memBackType_MM` | output | `[0:0]` |
| TileLink/auto | `auto_inner_dcache_client_out_a_bits_user_memPageType_NC` | output | `[0:0]` |
| TileLink/auto | `auto_inner_dcache_client_out_c_bits_user_memBackType_MM` | output | `[0:0]` |
| TileLink/auto | `auto_inner_dcache_client_out_c_bits_user_memPageType_NC` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_corrupt` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_data` | input | `[255:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_mask` | input | `[31:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_opcode` | input | `[3:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_param` | input | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_size` | input | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_user_alias` | input | `[1:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_user_memBackType_MM` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_a_bits_user_reqSource` | input | `[4:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_d_bits_denied` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_d_bits_param` | output | `[1:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_d_bits_sink` | output | `[9:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_d_bits_size` | output | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_in_d_ready` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icache_out_a_bits_user_memBackType_MM` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icachectrl_out_a_bits_corrupt` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icachectrl_out_a_bits_param` | output | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icachectrl_out_d_bits_corrupt` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icachectrl_out_d_bits_denied` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icachectrl_out_d_bits_param` | input | `[1:0]` |
| TileLink/auto | `auto_inner_frontendBridge_icachectrl_out_d_bits_sink` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_corrupt` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_data` | input | `[63:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_mask` | input | `[7:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_opcode` | input | `[3:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_param` | input | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_size` | input | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_source` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_user_memBackType_MM` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_a_bits_user_memPageType_NC` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_d_bits_denied` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_d_bits_opcode` | output | `[3:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_d_bits_param` | output | `[1:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_d_bits_sink` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_d_bits_size` | output | `[2:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_in_d_ready` | input | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_out_a_bits_user_memBackType_MM` | output | `[0:0]` |
| TileLink/auto | `auto_inner_frontendBridge_instr_uncache_out_a_bits_user_memPageType_NC` | output | `[0:0]` |
| TileLink/auto | `auto_inner_l2_pf_sender_out_l2_pf_en` | output | `[0:0]` |
| TileLink/auto | `auto_inner_l3_pf_sender_out_l2_pf_en` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_fpWen` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_ftqOffset` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_imm` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_isRVC` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value` | input | `[6:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_pc` | input | `[49:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_pdest` | input | `[7:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_rfWen` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_storeSetHit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_0_0_valid` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_fpWen` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_ftqOffset` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_imm` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_isRVC` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value` | input | `[6:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_pc` | input | `[49:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_pdest` | input | `[7:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_rfWen` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_storeSetHit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_1_0_valid` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_fpWen` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_ftqOffset` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_imm` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_isRVC` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value` | input | `[6:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_pc` | input | `[49:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_pdest` | input | `[7:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_rfWen` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_storeSetHit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_2_0_valid` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_ftqOffset` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_fuType` | input | `[35:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_imm` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_isRVC` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_pdest` | input | `[7:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_ssid` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_bits_storeSetHit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_3_0_valid` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_ftqOffset` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_fuType` | input | `[35:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_imm` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_isRVC` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_pdest` | input | `[7:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_ssid` | input | `[4:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_bits_storeSetHit` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_4_0_valid` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_fuType` | input | `[35:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_5_0_valid` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_fuOpType` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_fuType` | input | `[35:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_robIdx_value` | input | `[8:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_bits_src_0` | input | `[63:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_ready` | output | `[0:0]` |
| V3 integer issue intIssue | `io_ooo_to_mem_intIssue_6_0_valid` | input | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toFpRf_bits_data` | output | `[63:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toFpRf_bits_pdest` | output | `[7:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toFpRf_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toIntRf_bits_data` | output | `[63:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toIntRf_bits_isFromLoadUnit` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toIntRf_bits_pdest` | output | `[7:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toIntRf_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_debugInfo_isMMIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_debugInfo_isNCIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_0` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_1` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_10` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_11` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_12` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_13` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_14` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_15` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_16` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_17` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_18` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_19` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_2` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_20` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_21` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_22` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_23` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_3` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_4` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_5` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_6` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_7` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_8` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_exceptionVec_9` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_isRVC` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_lqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_lqIdx_value` | output | `[6:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_bits_trigger` | output | `[3:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_0_0_toRob_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toFpRf_bits_data` | output | `[63:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toFpRf_bits_pdest` | output | `[7:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toFpRf_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toIntRf_bits_data` | output | `[63:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toIntRf_bits_pdest` | output | `[7:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toIntRf_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_debugInfo_isMMIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_debugInfo_isNCIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_0` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_1` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_10` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_11` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_12` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_13` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_14` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_15` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_16` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_17` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_18` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_19` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_2` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_20` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_21` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_22` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_23` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_3` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_4` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_5` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_6` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_7` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_8` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_exceptionVec_9` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_isRVC` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_lqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_lqIdx_value` | output | `[6:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_bits_trigger` | output | `[3:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_1_0_toRob_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toFpRf_bits_data` | output | `[63:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toFpRf_bits_pdest` | output | `[7:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toFpRf_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toIntRf_bits_data` | output | `[63:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toIntRf_bits_pdest` | output | `[7:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toIntRf_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_debugInfo_isMMIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_debugInfo_isNCIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_0` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_1` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_10` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_11` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_12` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_13` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_14` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_15` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_16` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_17` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_18` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_19` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_2` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_20` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_21` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_22` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_23` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_3` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_4` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_5` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_6` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_7` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_8` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_exceptionVec_9` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_isRVC` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_lqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_lqIdx_value` | output | `[6:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_bits_trigger` | output | `[3:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_2_0_toRob_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_debugInfo_isMMIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_debugInfo_isNCIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_exceptionVec_15` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_exceptionVec_19` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_exceptionVec_23` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_exceptionVec_3` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_exceptionVec_6` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_exceptionVec_7` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_isRVC` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_sqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_sqIdx_value` | output | `[5:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_bits_trigger` | output | `[3:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_3_0_toRob_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_debugInfo_isMMIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_debugInfo_isNCIO` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_exceptionVec_15` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_exceptionVec_19` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_exceptionVec_23` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_exceptionVec_3` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_exceptionVec_6` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_exceptionVec_7` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_isRVC` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_sqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_sqIdx_value` | output | `[5:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_bits_trigger` | output | `[3:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_4_0_toRob_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_5_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_5_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_5_0_toRob_bits_sqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_5_0_toRob_bits_sqIdx_value` | output | `[5:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_5_0_toRob_valid` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_6_0_toRob_bits_robIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_6_0_toRob_bits_robIdx_value` | output | `[8:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_6_0_toRob_bits_sqIdx_flag` | output | `[0:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_6_0_toRob_bits_sqIdx_value` | output | `[5:0]` |
| V3 integer writeback intWriteback | `io_mem_to_ooo_intWriteback_6_0_toRob_valid` | output | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset` | input | `[4:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_fuOpType` | input | `[8:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_fuType` | input | `[35:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value` | input | `[6:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_numLsElem` | input | `[4:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_pdest` | input | `[6:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_pdestVl` | input | `[4:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value` | input | `[8:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_src_0` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_src_1` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_src_2` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_src_3` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_v0Wen` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vecWen` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vl` | input | `[7:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vlWen` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen` | input | `[15:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart` | input | `[7:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx` | input | `[6:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_ready` | output | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_0_0_valid` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value` | input | `[5:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset` | input | `[4:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_fuOpType` | input | `[8:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value` | input | `[6:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_numLsElem` | input | `[4:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_pdest` | input | `[6:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_pdestVl` | input | `[4:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value` | input | `[8:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value` | input | `[5:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_src_0` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_src_1` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_src_2` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_src_3` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_v0Wen` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vecWen` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vl` | input | `[7:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vlWen` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen` | input | `[15:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul` | input | `[2:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask` | input | `[127:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart` | input | `[7:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta` | input | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx` | input | `[6:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm` | input | `[1:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_ready` | output | `[0:0]` |
| V3 vector issue vecIssue | `io_ooo_to_mem_vecIssue_1_0_valid` | input | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_data_0` | output | `[127:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_debug_isMMIO` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_debug_isNCIO` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_debug_isPerfCnt` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_debug_paddr` | output | `[47:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_debug_vaddr` | output | `[49:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_13` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_15` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_19` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_21` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_23` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_3` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_4` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_5` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_6` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_exceptionVec_7` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_pdest` | output | `[6:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_pdestVl` | output | `[4:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_robIdx_flag` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_robIdx_value` | output | `[8:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_trigger` | output | `[3:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_v0Wen` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vecWen` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vlWen` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_isIndexed` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_isMasked` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_isStrided` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_isVecLoad` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_isVlm` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_isWhole` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_oldVdPsrc` | output | `[7:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vdIdx` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vdIdxInField` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isFP32Instr` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isFP64Instr` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isFoldTo1_2` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isFoldTo1_4` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isFoldTo1_8` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isFpToVecInst` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_fpu_isReduction` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_frm` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isDependOldVd` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isDstMask` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isExt` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isMove` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isNarrow` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isOpMask` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isReverse` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isVleff` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_isWritePartVd` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_lastUop` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_maskVecGen` | output | `[15:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_nf` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_sew16` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_sew32` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_sew64` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_sew8` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_specVill` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_specVlmul` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_specVma` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_specVsew` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_specVta` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_veew` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vill` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vl` | output | `[7:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vlmul` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vm` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vma` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vmask` | output | `[127:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vsew` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vstart` | output | `[7:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vta` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vuopIdx` | output | `[6:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_bits_vls_vpu_vxrm` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_ready` | input | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_0_0_valid` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_data_0` | output | `[127:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_debug_isMMIO` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_debug_isNCIO` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_debug_isPerfCnt` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_debug_paddr` | output | `[47:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_debug_vaddr` | output | `[49:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_13` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_15` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_19` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_21` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_23` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_3` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_4` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_5` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_6` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_exceptionVec_7` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_pdest` | output | `[6:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_pdestVl` | output | `[4:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_robIdx_flag` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_robIdx_value` | output | `[8:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_trigger` | output | `[3:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_v0Wen` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vecWen` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vlWen` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_isIndexed` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_isMasked` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_isStrided` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_isVecLoad` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_isVlm` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_isWhole` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_oldVdPsrc` | output | `[7:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vdIdx` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vdIdxInField` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isFP32Instr` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isFP64Instr` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isFoldTo1_2` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isFoldTo1_4` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isFoldTo1_8` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isFpToVecInst` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_fpu_isReduction` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_frm` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isDependOldVd` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isDstMask` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isExt` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isMove` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isNarrow` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isOpMask` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isReverse` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isVleff` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_isWritePartVd` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_lastUop` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_maskVecGen` | output | `[15:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_nf` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_sew16` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_sew32` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_sew64` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_sew8` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_specVill` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_specVlmul` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_specVma` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_specVsew` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_specVta` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_veew` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vill` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vl` | output | `[7:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vlmul` | output | `[2:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vm` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vma` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vmask` | output | `[127:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vsew` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vstart` | output | `[7:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vta` | output | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vuopIdx` | output | `[6:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_bits_vls_vpu_vxrm` | output | `[1:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_ready` | input | `[0:0]` |
| V3 vector writeback vecWriteback | `io_mem_to_ooo_vecWriteback_1_0_valid` | output | `[0:0]` |
| backendToTopBypass | `io_ooo_to_mem_backendToTopBypass_cpuWfi` | input | `[0:0]` |
| backendToTopBypass | `io_ooo_to_mem_backendToTopBypass_msiAck` | input | `[0:0]` |
| debug/topdown | `io_debugTopDown_robHeadVaddr_bits` | input | `[49:0]` |
| debug/topdown | `io_debugTopDown_robHeadVaddr_valid` | input | `[0:0]` |
| debug/topdown | `io_debugTopDown_toCore_robHeadLoadMSHR` | output | `[0:0]` |
| debug/topdown | `io_debugTopDown_toCore_robHeadLoadVio` | output | `[0:0]` |
| debug/topdown | `io_debugTopDown_toCore_robHeadMissInDCache` | output | `[0:0]` |
| debug/topdown | `io_debugTopDown_toCore_robHeadTlbMiss` | output | `[0:0]` |
| debug/topdown | `io_debugTopDown_toCore_robHeadTlbReplay` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_0` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_1` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_10` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_11` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_2` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_3` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_4` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_5` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_6` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_8` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_replayCause_9` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s1_isTlbFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s1_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s2_isBankConflict` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s2_isDcacheFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s2_isForwardFail` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s2_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s3_isReplay` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s3_isReplayFast` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s3_isReplaySlow` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_0_s3_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_0` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_1` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_10` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_11` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_2` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_3` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_4` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_5` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_6` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_8` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_replayCause_9` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s1_isTlbFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s1_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s2_isBankConflict` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s2_isDcacheFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s2_isForwardFail` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s2_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s3_isReplay` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s3_isReplayFast` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s3_isReplaySlow` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_1_s3_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_0` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_1` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_10` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_11` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_2` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_3` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_4` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_5` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_6` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_8` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_replayCause_9` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s1_isTlbFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s1_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s2_isBankConflict` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s2_isDcacheFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s2_isForwardFail` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s2_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s3_isReplay` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s3_isReplayFast` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s3_isReplaySlow` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_2_s3_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_3_s1_isTlbFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_3_s1_robIdx` | output | `[8:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_4_s1_isTlbFirstMiss` | output | `[0:0]` |
| debug/topdown | `io_debug_ls_debugLsInfo_4_s1_robIdx` | output | `[8:0]` |
| fetch_to_mem | `io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch` | output | `[0:0]` |
| fetch_to_mem | `io_fetch_to_mem_itlb_resp_bits_s2_entry_asid` | output | `[15:0]` |
| fetch_to_mem | `io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch` | output | `[0:0]` |
| fetch_to_mem | `io_fetch_to_mem_itlb_resp_bits_s2_entry_v` | output | `[0:0]` |
| hart/perf/top state | `io_inner_hartId` | output | `[5:0]` |
| hart/perf/top state | `io_inner_hc_perfEvents_68_value` | output | `[5:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx` | output | `[8:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits` | output | `[49:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits` | output | `[47:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx` | output | `[8:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx` | output | `[8:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits` | output | `[49:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits` | output | `[47:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx` | output | `[8:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx` | output | `[8:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits` | output | `[49:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits` | output | `[47:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid` | output | `[0:0]` |
| lsTopdownInfo | `io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx` | output | `[8:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_debug_runahead_checkpoint_id` | output | `[63:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag` | output | `[0:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_ftqIdx_value` | output | `[5:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_ftqOffset` | output | `[4:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_isRVC` | output | `[0:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_level` | output | `[0:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_robIdx_flag` | output | `[0:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_robIdx_value` | output | `[8:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag` | output | `[0:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value` | output | `[5:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_stFtqOffset` | output | `[4:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_stIsRVC` | output | `[0:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_bits_target` | output | `[49:0]` |
| mdpTrain | `io_mem_to_ooo_mdpTrain_valid` | output | `[0:0]` |
| memInfo | `io_memInfo_dcacheMSHRFull` | output | `[0:0]` |
| memInfo | `io_memInfo_lqFull` | output | `[0:0]` |
| memInfo | `io_memInfo_sqFull` | output | `[0:0]` |
| mem_to_ooo other | `io_mem_to_ooo_sqDeqPtr_flag` | output | `[0:0]` |
| mem_to_ooo other | `io_mem_to_ooo_sqDeqPtr_value` | output | `[5:0]` |
| mem_to_ooo other | `io_mem_to_ooo_stIssuePtr_flag` | output | `[0:0]` |
| mem_to_ooo other | `io_mem_to_ooo_stIssuePtr_value` | output | `[5:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_debug_runahead_checkpoint_id` | output | `[63:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag` | output | `[0:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value` | output | `[5:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_stFtqOffset` | output | `[4:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_stIsRVC` | output | `[0:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_target` | output | `[49:0]` |
| ooo_to_mem other | `io_ooo_to_mem_enqLsq_canAccept` | output | `[0:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_0_bits_robIdx_flag` | output | `[0:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_0_bits_robIdx_value` | output | `[8:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_0_bits_ssid` | output | `[4:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_0_bits_storeSetHit` | output | `[0:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_0_valid` | output | `[0:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_1_bits_robIdx_flag` | output | `[0:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_1_bits_robIdx_value` | output | `[8:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_1_bits_ssid` | output | `[4:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_1_bits_storeSetHit` | output | `[0:0]` |
| updateLFST | `io_mem_to_ooo_updateLFST_1_valid` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_0_bits_v0Wen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_0_bits_vecWen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_0_bits_vlWen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_1_bits_v0Wen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_1_bits_vecWen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_1_bits_vlWen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_2_bits_v0Wen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_2_bits_vecWen` | output | `[0:0]` |
| wakeup | `io_mem_to_ooo_wakeup_2_bits_vlWen` | output | `[0:0]` |
| 其他 io | `io_outer_hc_perfEvents_0_value` | input | `[5:0]` |
| 其他 io | `io_outer_msi_ack` | output | `[0:0]` |

## 4. 同名但方向/位宽不同端口

| 接口族 | 端口 | V2 方向 | V2 位宽 | V3 方向 | V3 位宽 |
| --- | --- | --- | --- | --- | --- |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_fuType` | input | `[34:0]` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_fuType` | input | `[34:0]` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_fuType` | input | `[34:0]` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_fuType` | input | `[34:0]` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_fuType` | input | `[34:0]` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_fuType` | input | `[34:0]` | input | `[35:0]` |
| LSQ 入队请求 enqLsq_req | `io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| LSQ 状态/反馈 lsqio | `io_ooo_to_mem_lsqio_pendingPtr_value` | input | `[7:0]` | input | `[8:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_ftqOffset` | output | `[3:0]` | output | `[4:0]` |
| memory violation | `io_mem_to_ooo_memoryViolation_bits_robIdx_value` | output | `[7:0]` | output | `[8:0]` |
| redirect | `io_redirect_bits_robIdx_value` | input | `[7:0]` | input | `[8:0]` |
| trace | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset` | input | `[3:0]` | input | `[4:0]` |
| trace | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire` | input | `[6:0]` | input | `[7:0]` |
| trace | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset` | input | `[3:0]` | input | `[4:0]` |
| trace | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire` | input | `[6:0]` | input | `[7:0]` |
| trace | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset` | input | `[3:0]` | input | `[4:0]` |
| trace | `io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire` | input | `[6:0]` | input | `[7:0]` |
| trace | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iretire` | output | `[6:0]` | output | `[7:0]` |
| trace | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iretire` | output | `[6:0]` | output | `[7:0]` |
| trace | `io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iretire` | output | `[6:0]` | output | `[7:0]` |
