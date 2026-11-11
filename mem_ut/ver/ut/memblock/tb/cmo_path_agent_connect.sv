`ifndef CMO_PATH_AGENT_CONNECT_SV
`define CMO_PATH_AGENT_CONNECT_SV
// 当前V2生成RTL内部只读探针；所有赋值方向均为DUT到观察interface。
cmo_path_agent_interface cmo_path_if(clk,tc_if.rst_n);
initial begin
    uvm_config_db#(virtual cmo_path_agent_interface)::set(null,"*u_cmo_path_agent*","vif",cmo_path_if);
    if (`MEMBLOCK_DUT_LOAD_PIPE_NUM != 3)
        $fatal(1,"cmo_path只读connect需要与当前V2的3个真实Load端口同步");
    if ($bits(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_bits_idx)>16 ||
        $bits(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_bits_way_en)>16)
        $fatal(1,"cmo_path观察记录容量小于当前errorArray物理端口");
end
// SQ接受身份：真实enqueue且未被redirect取消。
assign cmo_path_if.probes[0] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_0_valid && !U_MEMBLOCK.inner_lsq.storeQueue.enqCancel_0),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_ENQUEUE,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_0_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_0_bits_robIdx_value}),
    sq: 7'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_0_bits_sqIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_0_bits_sqIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_0_bits_fuOpType),
    default: '0
};
// SQ接受身份：真实enqueue且未被redirect取消。
assign cmo_path_if.probes[1] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_1_valid && !U_MEMBLOCK.inner_lsq.storeQueue.enqCancel_1),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_ENQUEUE,
    port_id: 8'(1),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_1_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_1_bits_robIdx_value}),
    sq: 7'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_1_bits_sqIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_1_bits_sqIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_1_bits_fuOpType),
    default: '0
};
// SQ接受身份：真实enqueue且未被redirect取消。
assign cmo_path_if.probes[2] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_2_valid && !U_MEMBLOCK.inner_lsq.storeQueue.enqCancel_2),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_ENQUEUE,
    port_id: 8'(2),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_2_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_2_bits_robIdx_value}),
    sq: 7'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_2_bits_sqIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_2_bits_sqIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_2_bits_fuOpType),
    default: '0
};
// SQ接受身份：真实enqueue且未被redirect取消。
assign cmo_path_if.probes[3] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_3_valid && !U_MEMBLOCK.inner_lsq.storeQueue.enqCancel_3),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_ENQUEUE,
    port_id: 8'(3),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_3_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_3_bits_robIdx_value}),
    sq: 7'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_3_bits_sqIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_3_bits_sqIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_3_bits_fuOpType),
    default: '0
};
// SQ接受身份：真实enqueue且未被redirect取消。
assign cmo_path_if.probes[4] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_4_valid && !U_MEMBLOCK.inner_lsq.storeQueue.enqCancel_4),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_ENQUEUE,
    port_id: 8'(4),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_4_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_4_bits_robIdx_value}),
    sq: 7'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_4_bits_sqIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_4_bits_sqIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_4_bits_fuOpType),
    default: '0
};
// SQ接受身份：真实enqueue且未被redirect取消。
assign cmo_path_if.probes[5] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_5_valid && !U_MEMBLOCK.inner_lsq.storeQueue.enqCancel_5),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_ENQUEUE,
    port_id: 8'(5),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_5_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_5_bits_robIdx_value}),
    sq: 7'({U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_5_bits_sqIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_5_bits_sqIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.io_enq_req_5_bits_fuOpType),
    default: '0
};
// SQ->SB真实fire，sq仅低位value有效；qual[0]=wline，[1]=vecValid，[2]=接受拍canMerge。
assign cmo_path_if.probes[6] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_valid && U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_ready),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_SBUFFER,
    port_id: 8'(0),
    sq: 7'(U_MEMBLOCK.inner_lsq.storeQueue._dataBuffer_io_deq_0_bits_sqPtr_value),
    addr: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_bits_addr),
    data: 512'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_bits_data),
    mask: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_bits_mask),
    qual: 16'({13'b0,U_MEMBLOCK.inner_sbuffer.canMerge_0,U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_bits_vecValid,U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_0_bits_wline}),
    default: '0
};
// accessIdx为上一拍接受通知；adapter须关联上一拍SQ_SBUFFER，不能连当前输入。
assign cmo_path_if.probes[7] = '{
    valid: 1'(U_MEMBLOCK.inner_sbuffer.accessIdx_0_valid_REG),
    kind: cmo_path_agent_dec::CMO_PATH_SB_ACCESS,
    port_id: 8'(0),
    slot: 32'(U_MEMBLOCK.inner_sbuffer.accessIdx_0_bits_r),
    default: '0
};
// SQ->SB真实fire，sq仅低位value有效；qual[0]=wline，[1]=vecValid，[2]=接受拍canMerge。
assign cmo_path_if.probes[8] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_valid && U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_ready),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_SBUFFER,
    port_id: 8'(1),
    sq: 7'(U_MEMBLOCK.inner_lsq.storeQueue._dataBuffer_io_deq_1_bits_sqPtr_value),
    addr: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_bits_addr),
    data: 512'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_bits_data),
    mask: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_bits_mask),
    qual: 16'({13'b0,U_MEMBLOCK.inner_sbuffer.canMerge_1,U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_bits_vecValid,U_MEMBLOCK.inner_lsq.storeQueue.io_sbuffer_1_bits_wline}),
    default: '0
};
// accessIdx为上一拍接受通知；adapter须关联上一拍SQ_SBUFFER，不能连当前输入。
assign cmo_path_if.probes[9] = '{
    valid: 1'(U_MEMBLOCK.inner_sbuffer.accessIdx_1_valid_REG),
    kind: cmo_path_agent_dec::CMO_PATH_SB_ACCESS,
    port_id: 8'(1),
    slot: 32'(U_MEMBLOCK.inner_sbuffer.accessIdx_1_bits_r),
    default: '0
};
// SB->DCache真实fire，id低4bit为实际SB slot；允许同slot replay/普通Store合并。
assign cmo_path_if.probes[10] = '{
    valid: 1'(U_MEMBLOCK.inner_sbuffer.io_dcache_req_valid && U_MEMBLOCK.inner_sbuffer.io_dcache_req_ready),
    kind: cmo_path_agent_dec::CMO_PATH_SB_DCACHE_REQUEST,
    port_id: 8'(0),
    addr: 64'(U_MEMBLOCK.inner_sbuffer.io_dcache_req_bits_addr),
    data: 512'(U_MEMBLOCK.inner_sbuffer.io_dcache_req_bits_data),
    mask: 64'(U_MEMBLOCK.inner_sbuffer.io_dcache_req_bits_mask),
    id: 32'(U_MEMBLOCK.inner_sbuffer.io_dcache_req_bits_id),
    slot: 32'(U_MEMBLOCK.inner_sbuffer.io_dcache_req_bits_id[3:0]),
    default: '0
};
assign cmo_path_if.probes[11] = '{
    valid: 1'(U_MEMBLOCK.inner_sbuffer.io_dcache_main_pipe_hit_resp_valid),
    kind: cmo_path_agent_dec::CMO_PATH_SB_DCACHE_SUCCESS,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.inner_sbuffer.io_dcache_main_pipe_hit_resp_bits_id),
    slot: 32'(U_MEMBLOCK.inner_sbuffer.io_dcache_main_pipe_hit_resp_bits_id[3:0]),
    default: '0
};
assign cmo_path_if.probes[12] = '{
    valid: 1'(U_MEMBLOCK.inner_sbuffer.io_dcache_replay_resp_valid),
    kind: cmo_path_agent_dec::CMO_PATH_SB_DCACHE_REPLAY,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.inner_sbuffer.io_dcache_replay_resp_bits_id),
    slot: 32'(U_MEMBLOCK.inner_sbuffer.io_dcache_replay_resp_bits_id[3:0]),
    default: '0
};
// nonzero CMO真实fire；owner由SQ已锁存uncacheUop.rob绑定，不把当拍deqPtr当旧owner。
assign cmo_path_if.probes[13] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_cmoOpReq_valid && U_MEMBLOCK.inner_lsq.storeQueue.io_cmoOpReq_ready),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_CMO_REQUEST,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.uncacheUop_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.uncacheUop_robIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.uncacheUop_fuOpType),
    addr: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_cmoOpReq_bits_address),
    id: 32'(U_MEMBLOCK.inner_lsq.storeQueue.io_cmoOpReq_bits_opcode),
    default: '0
};
// SQ->Uncache真实分段fire；op来自锁存uop，nc=qual[0]；id是SQ请求id而非外TL source。
assign cmo_path_if.probes[14] = '{
    valid: 1'(U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_valid && U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_ready),
    kind: cmo_path_agent_dec::CMO_PATH_SQ_UNCACHE_REQUEST,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_robIdx_flag,U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_robIdx_value}),
    op: 9'(U_MEMBLOCK.inner_lsq.storeQueue.uncacheUop_fuOpType),
    addr: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_addr),
    data: 512'(U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_data),
    mask: 64'(U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_mask),
    id: 32'(U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_id),
    qual: 16'(U_MEMBLOCK.inner_lsq.storeQueue.io_uncache_req_bits_nc),
    default: '0
};
// 统一Uncache请求真实fire，slot为真实e0选择；qual[0]=merge，[1]=nc。与SQ记录按实际请求id关联。
assign cmo_path_if.probes[15] = '{
    valid: 1'(U_MEMBLOCK.inner_uncache.io_lsq_req_valid && U_MEMBLOCK.inner_uncache.io_lsq_req_ready),
    kind: cmo_path_agent_dec::CMO_PATH_UNCACHE_ALLOC,
    port_id: 8'(0),
    addr: 64'(U_MEMBLOCK.inner_uncache.io_lsq_req_bits_addr),
    data: 512'(U_MEMBLOCK.inner_uncache.io_lsq_req_bits_data),
    mask: 64'(U_MEMBLOCK.inner_uncache.io_lsq_req_bits_mask),
    op: 9'(U_MEMBLOCK.inner_uncache.io_lsq_req_bits_cmd),
    id: 32'(U_MEMBLOCK.inner_uncache.io_lsq_req_bits_id),
    slot: 32'(U_MEMBLOCK.inner_uncache.e0_canMerge ? U_MEMBLOCK.inner_uncache.e0_mergeIdx : U_MEMBLOCK.inner_uncache.e0_allocIdx),
    qual: 16'({14'b0,U_MEMBLOCK.inner_uncache.io_lsq_req_bits_nc,U_MEMBLOCK.inner_uncache.e0_canMerge}),
    default: '0
};
// Valid写口接受；idx/way是真实阵列位置，id=s3 miss_id对应L2 source，state[0]=s3_req_miss。
assign cmo_path_if.probes[16] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_valid),
    kind: cmo_path_agent_dec::CMO_PATH_ERROR_ARRAY_WRITE,
    port_id: 8'(0),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_bits_idx),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_bits_way_en),
    raw: 2'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_bits_error_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_write_0_bits_error_tl_corrupt}),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_addr),
    id: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_miss_id),
    state: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_miss),
    default: '0
};
// Valid读口每次锁存真实idx；qual[0]标识对应pipe实际S0接受。actual_by_way每way两位[denied,corrupt]。
assign cmo_path_if.probes[17] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_0_valid),
    kind: cmo_path_agent_dec::CMO_PATH_ERROR_ARRAY_READ,
    port_id: 8'(0),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_0_bits_idx),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_0.s0_valid),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_0_tl_corrupt}),
    default: '0
};
// Valid读口每次锁存真实idx；qual[0]标识对应pipe实际S0接受。actual_by_way每way两位[denied,corrupt]。
assign cmo_path_if.probes[18] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_ERROR_ARRAY_READ,
    port_id: 8'(1),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_1_bits_idx),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_1.s0_valid),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_0_tl_corrupt}),
    default: '0
};
// Valid读口每次锁存真实idx；qual[0]标识对应pipe实际S0接受。actual_by_way每way两位[denied,corrupt]。
assign cmo_path_if.probes[19] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_2_valid),
    kind: cmo_path_agent_dec::CMO_PATH_ERROR_ARRAY_READ,
    port_id: 8'(2),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_2_bits_idx),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_2.s0_valid),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_0_tl_corrupt}),
    default: '0
};
// Valid读口每次锁存真实idx；qual[0]标识对应pipe实际S0接受。actual_by_way每way两位[denied,corrupt]。
assign cmo_path_if.probes[20] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_3_valid),
    kind: cmo_path_agent_dec::CMO_PATH_ERROR_ARRAY_READ,
    port_id: 8'(3),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.errorArray.io_read_3_bits_idx),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s0_fire),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_0_tl_corrupt}),
    default: '0
};
// LoadPipe S1真实命中way，qual[0]=s1_kill；raw由实际extra_meta输入按way选出。
assign cmo_path_if.probes[21] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.ldu_0.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_PIPE_HIT,
    port_id: 8'(0),
    lq: 8'({U_MEMBLOCK.inner_dcache.dcache.ldu_0.s1_req_lqIdx_flag,U_MEMBLOCK.inner_dcache.dcache.ldu_0.s1_req_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.ldu_0.io_lsu_s1_paddr_dup_lsu),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_0.s1_tag_match_way_dup_dc),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_0.io_lsu_s1_kill),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_0_0_tl_corrupt}),
    default: '0
};
// Load S1来源身份，下一拍对应S2 direct/MSHR输入；id=mshrid，aux=当拍TL D source，raw=TL D错误。qual[0]=tlD valid，[1]=forward请求valid，[2]=D末beat。
assign cmo_path_if.probes[22] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_0.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_FORWARD_SOURCE,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_0.s1_in_r_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_0.s1_in_r_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_0.s1_in_r_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_0.s1_in_r_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_0.io_forward_mshr_paddr),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_0.s1_in_r_mshrid),
    aux: 32'(U_MEMBLOCK.inner_LoadUnit_0.io_tl_d_channel_mshrid),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_0.io_tl_d_channel_denied,U_MEMBLOCK.inner_LoadUnit_0.io_tl_d_channel_corrupt}),
    qual: 16'({13'b0,U_MEMBLOCK.inner_LoadUnit_0.io_tl_d_channel_last,U_MEMBLOCK.inner_LoadUnit_0.io_forward_mshr_valid,U_MEMBLOCK.inner_LoadUnit_0.io_tl_d_channel_valid}),
    default: '0
};
// Load S2真实消费；raw=direct，raw_extra=MSHR。qual[0]=direct有效，[1]=MSHR有效，[2]=vecActive，[3]=非Prefetch，[4]=非nc_with_data，[5]=s2_fire，[6]=kill。id为DCache当前响应mshr_id，S1来源id由前拍FORWARD_SOURCE冻结。
assign cmo_path_if.probes[23] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_0.s2_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_S2,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_paddr),
    mask: 64'(U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_mask),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_0.io_dcache_resp_bits_mshr_id),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_0.s2_d_denied,U_MEMBLOCK.inner_LoadUnit_0.s2_d_corrupt}),
    raw_extra: 2'({U_MEMBLOCK.inner_LoadUnit_0.io_forward_mshr_denied,U_MEMBLOCK.inner_LoadUnit_0.io_forward_mshr_corrupt}),
    qual: 16'({9'b0,U_MEMBLOCK.inner_LoadUnit_0.s2_kill,U_MEMBLOCK.inner_LoadUnit_0.s2_fire,!U_MEMBLOCK.inner_LoadUnit_0.s2_nc_with_data,!U_MEMBLOCK.inner_LoadUnit_0.s2_in_r_isPrefetch,U_MEMBLOCK.inner_LoadUnit_0.s2_vecActive,(U_MEMBLOCK.inner_LoadUnit_0.io_forward_mshr_forward_result_valid && U_MEMBLOCK.inner_LoadUnit_0.io_forward_mshr_forward_mshr),U_MEMBLOCK.inner_LoadUnit_0.s2_fwd_frm_d_chan}),
    default: '0
};
// Load S3延迟hit输入独立于数据选路；qual[0]=active，[1]=非Prefetch，[2]=非ncData，[3]=最终有效输出，[4]=ready，[5]=dcacheS3有效，[6]=真实mabuf返回，[7]=frmMabuf，[8]=无replay，[9]=cache路径，[10]=finalSplit。
assign cmo_path_if.probes[24] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_0.s3_valid_last_REG),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_S3,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_0.s3_in_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_0.s3_in_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_0.s3_in_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_0.s3_in_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_0.s3_in_paddr),
    mask: 64'(U_MEMBLOCK.inner_LoadUnit_0.s3_in_mask),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_0.s3_in_rep_info_mshr_id),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_0.io_dcache_resp_bits_tl_error_delayed_tl_denied,U_MEMBLOCK.inner_LoadUnit_0.io_dcache_resp_bits_tl_error_delayed_tl_corrupt}),
    qual: 16'({5'b0,U_MEMBLOCK.inner_LoadUnit_0.s3_in_isFinalSplit,(!U_MEMBLOCK.inner_LoadUnit_0.s3_in_nc && !U_MEMBLOCK.inner_LoadUnit_0.s3_in_mmio),!(|U_MEMBLOCK.inner_LoadUnit_0.s3_mab_sel_rep_cause),U_MEMBLOCK.inner_LoadUnit_0.s3_in_isFrmMisAlignBuf,(U_MEMBLOCK.inner_LoadUnit_0.io_misalign_ldout_valid && !U_MEMBLOCK.inner_LoadUnit_0.s3_misalign_wakeup_req_valid_REG_2),U_MEMBLOCK.inner_dcache.dcache.ldu_0.s3_valid,U_MEMBLOCK.inner_LoadUnit_0.io_ldout_ready,U_MEMBLOCK.inner_LoadUnit_0.s3_out_valid,!U_MEMBLOCK.inner_LoadUnit_0.s3_nc_with_data,!U_MEMBLOCK.inner_LoadUnit_0.s3_in_isPrefetch,U_MEMBLOCK.inner_LoadUnit_0.s3_vecActive}),
    aux: 32'(U_MEMBLOCK.inner_LoadUnit_0.s3_in_vaddr),
    default: '0
};
// LoadPipe S1真实命中way，qual[0]=s1_kill；raw由实际extra_meta输入按way选出。
assign cmo_path_if.probes[25] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.ldu_1.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_PIPE_HIT,
    port_id: 8'(1),
    lq: 8'({U_MEMBLOCK.inner_dcache.dcache.ldu_1.s1_req_lqIdx_flag,U_MEMBLOCK.inner_dcache.dcache.ldu_1.s1_req_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.ldu_1.io_lsu_s1_paddr_dup_lsu),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_1.s1_tag_match_way_dup_dc),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_1.io_lsu_s1_kill),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_1_0_tl_corrupt}),
    default: '0
};
// Load S1来源身份，下一拍对应S2 direct/MSHR输入；id=mshrid，aux=当拍TL D source，raw=TL D错误。qual[0]=tlD valid，[1]=forward请求valid，[2]=D末beat。
assign cmo_path_if.probes[26] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_1.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_FORWARD_SOURCE,
    port_id: 8'(1),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_1.s1_in_r_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_1.s1_in_r_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_1.s1_in_r_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_1.s1_in_r_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_1.io_forward_mshr_paddr),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_1.s1_in_r_mshrid),
    aux: 32'(U_MEMBLOCK.inner_LoadUnit_1.io_tl_d_channel_mshrid),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_1.io_tl_d_channel_denied,U_MEMBLOCK.inner_LoadUnit_1.io_tl_d_channel_corrupt}),
    qual: 16'({13'b0,U_MEMBLOCK.inner_LoadUnit_1.io_tl_d_channel_last,U_MEMBLOCK.inner_LoadUnit_1.io_forward_mshr_valid,U_MEMBLOCK.inner_LoadUnit_1.io_tl_d_channel_valid}),
    default: '0
};
// Load S2真实消费；raw=direct，raw_extra=MSHR。qual[0]=direct有效，[1]=MSHR有效，[2]=vecActive，[3]=非Prefetch，[4]=非nc_with_data，[5]=s2_fire，[6]=kill。id为DCache当前响应mshr_id，S1来源id由前拍FORWARD_SOURCE冻结。
assign cmo_path_if.probes[27] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_1.s2_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_S2,
    port_id: 8'(1),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_paddr),
    mask: 64'(U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_mask),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_1.io_dcache_resp_bits_mshr_id),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_1.s2_d_denied,U_MEMBLOCK.inner_LoadUnit_1.s2_d_corrupt}),
    raw_extra: 2'({U_MEMBLOCK.inner_LoadUnit_1.io_forward_mshr_denied,U_MEMBLOCK.inner_LoadUnit_1.io_forward_mshr_corrupt}),
    qual: 16'({9'b0,U_MEMBLOCK.inner_LoadUnit_1.s2_kill,U_MEMBLOCK.inner_LoadUnit_1.s2_fire,!U_MEMBLOCK.inner_LoadUnit_1.s2_nc_with_data,!U_MEMBLOCK.inner_LoadUnit_1.s2_in_r_isPrefetch,U_MEMBLOCK.inner_LoadUnit_1.s2_vecActive,(U_MEMBLOCK.inner_LoadUnit_1.io_forward_mshr_forward_result_valid && U_MEMBLOCK.inner_LoadUnit_1.io_forward_mshr_forward_mshr),U_MEMBLOCK.inner_LoadUnit_1.s2_fwd_frm_d_chan}),
    default: '0
};
// Load S3延迟hit输入独立于数据选路；qual[0]=active，[1]=非Prefetch，[2]=非ncData，[3]=最终有效输出，[4]=ready，[5]=dcacheS3有效，[6]=真实mabuf返回，[7]=frmMabuf，[8]=无replay，[9]=cache路径，[10]=finalSplit。
assign cmo_path_if.probes[28] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_1.s3_valid_last_REG),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_S3,
    port_id: 8'(1),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_1.s3_in_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_1.s3_in_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_1.s3_in_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_1.s3_in_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_1.s3_in_paddr),
    mask: 64'(U_MEMBLOCK.inner_LoadUnit_1.s3_in_mask),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_1.s3_in_rep_info_mshr_id),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_1.io_dcache_resp_bits_tl_error_delayed_tl_denied,U_MEMBLOCK.inner_LoadUnit_1.io_dcache_resp_bits_tl_error_delayed_tl_corrupt}),
    qual: 16'({5'b0,U_MEMBLOCK.inner_LoadUnit_1.s3_in_isFinalSplit,(!U_MEMBLOCK.inner_LoadUnit_1.s3_in_nc && !U_MEMBLOCK.inner_LoadUnit_1.s3_in_mmio),!(|U_MEMBLOCK.inner_LoadUnit_1.s3_mab_sel_rep_cause),U_MEMBLOCK.inner_LoadUnit_1.s3_in_isFrmMisAlignBuf,(U_MEMBLOCK.inner_LoadUnit_1.io_misalign_ldout_valid && !U_MEMBLOCK.inner_LoadUnit_1.s3_misalign_wakeup_req_valid_REG_2),U_MEMBLOCK.inner_dcache.dcache.ldu_1.s3_valid,U_MEMBLOCK.inner_LoadUnit_1.io_ldout_ready,U_MEMBLOCK.inner_LoadUnit_1.s3_out_valid,!U_MEMBLOCK.inner_LoadUnit_1.s3_nc_with_data,!U_MEMBLOCK.inner_LoadUnit_1.s3_in_isPrefetch,U_MEMBLOCK.inner_LoadUnit_1.s3_vecActive}),
    aux: 32'(U_MEMBLOCK.inner_LoadUnit_1.s3_in_vaddr),
    default: '0
};
// LoadPipe S1真实命中way，qual[0]=s1_kill；raw由实际extra_meta输入按way选出。
assign cmo_path_if.probes[29] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.ldu_2.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_PIPE_HIT,
    port_id: 8'(2),
    lq: 8'({U_MEMBLOCK.inner_dcache.dcache.ldu_2.s1_req_lqIdx_flag,U_MEMBLOCK.inner_dcache.dcache.ldu_2.s1_req_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.ldu_2.io_lsu_s1_paddr_dup_lsu),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_2.s1_tag_match_way_dup_dc),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.ldu_2.io_lsu_s1_kill),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_2_0_tl_corrupt}),
    default: '0
};
// Load S1来源身份，下一拍对应S2 direct/MSHR输入；id=mshrid，aux=当拍TL D source，raw=TL D错误。qual[0]=tlD valid，[1]=forward请求valid，[2]=D末beat。
assign cmo_path_if.probes[30] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_2.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_FORWARD_SOURCE,
    port_id: 8'(2),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_2.s1_in_r_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_2.s1_in_r_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_2.s1_in_r_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_2.s1_in_r_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_2.io_forward_mshr_paddr),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_2.s1_in_r_mshrid),
    aux: 32'(U_MEMBLOCK.inner_LoadUnit_2.io_tl_d_channel_mshrid),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_2.io_tl_d_channel_denied,U_MEMBLOCK.inner_LoadUnit_2.io_tl_d_channel_corrupt}),
    qual: 16'({13'b0,U_MEMBLOCK.inner_LoadUnit_2.io_tl_d_channel_last,U_MEMBLOCK.inner_LoadUnit_2.io_forward_mshr_valid,U_MEMBLOCK.inner_LoadUnit_2.io_tl_d_channel_valid}),
    default: '0
};
// Load S2真实消费；raw=direct，raw_extra=MSHR。qual[0]=direct有效，[1]=MSHR有效，[2]=vecActive，[3]=非Prefetch，[4]=非nc_with_data，[5]=s2_fire，[6]=kill。id为DCache当前响应mshr_id，S1来源id由前拍FORWARD_SOURCE冻结。
assign cmo_path_if.probes[31] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_2.s2_valid),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_S2,
    port_id: 8'(2),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_paddr),
    mask: 64'(U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_mask),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_2.io_dcache_resp_bits_mshr_id),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_2.s2_d_denied,U_MEMBLOCK.inner_LoadUnit_2.s2_d_corrupt}),
    raw_extra: 2'({U_MEMBLOCK.inner_LoadUnit_2.io_forward_mshr_denied,U_MEMBLOCK.inner_LoadUnit_2.io_forward_mshr_corrupt}),
    qual: 16'({9'b0,U_MEMBLOCK.inner_LoadUnit_2.s2_kill,U_MEMBLOCK.inner_LoadUnit_2.s2_fire,!U_MEMBLOCK.inner_LoadUnit_2.s2_nc_with_data,!U_MEMBLOCK.inner_LoadUnit_2.s2_in_r_isPrefetch,U_MEMBLOCK.inner_LoadUnit_2.s2_vecActive,(U_MEMBLOCK.inner_LoadUnit_2.io_forward_mshr_forward_result_valid && U_MEMBLOCK.inner_LoadUnit_2.io_forward_mshr_forward_mshr),U_MEMBLOCK.inner_LoadUnit_2.s2_fwd_frm_d_chan}),
    default: '0
};
// Load S3延迟hit输入独立于数据选路；qual[0]=active，[1]=非Prefetch，[2]=非ncData，[3]=最终有效输出，[4]=ready，[5]=dcacheS3有效，[6]=真实mabuf返回，[7]=frmMabuf，[8]=无replay，[9]=cache路径，[10]=finalSplit。
assign cmo_path_if.probes[32] = '{
    valid: 1'(U_MEMBLOCK.inner_LoadUnit_2.s3_valid_last_REG),
    kind: cmo_path_agent_dec::CMO_PATH_LOAD_S3,
    port_id: 8'(2),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_LoadUnit_2.s3_in_uop_robIdx_flag,U_MEMBLOCK.inner_LoadUnit_2.s3_in_uop_robIdx_value}),
    lq: 8'({U_MEMBLOCK.inner_LoadUnit_2.s3_in_uop_lqIdx_flag,U_MEMBLOCK.inner_LoadUnit_2.s3_in_uop_lqIdx_value}),
    addr: 64'(U_MEMBLOCK.inner_LoadUnit_2.s3_in_paddr),
    mask: 64'(U_MEMBLOCK.inner_LoadUnit_2.s3_in_mask),
    id: 32'(U_MEMBLOCK.inner_LoadUnit_2.s3_in_rep_info_mshr_id),
    raw: 2'({U_MEMBLOCK.inner_LoadUnit_2.io_dcache_resp_bits_tl_error_delayed_tl_denied,U_MEMBLOCK.inner_LoadUnit_2.io_dcache_resp_bits_tl_error_delayed_tl_corrupt}),
    qual: 16'({5'b0,U_MEMBLOCK.inner_LoadUnit_2.s3_in_isFinalSplit,(!U_MEMBLOCK.inner_LoadUnit_2.s3_in_nc && !U_MEMBLOCK.inner_LoadUnit_2.s3_in_mmio),!(|U_MEMBLOCK.inner_LoadUnit_2.s3_mab_sel_rep_cause),U_MEMBLOCK.inner_LoadUnit_2.s3_in_isFrmMisAlignBuf,(U_MEMBLOCK.inner_LoadUnit_2.io_misalign_ldout_valid && !U_MEMBLOCK.inner_LoadUnit_2.s3_misalign_wakeup_req_valid_REG_2),U_MEMBLOCK.inner_dcache.dcache.ldu_2.s3_valid,U_MEMBLOCK.inner_LoadUnit_2.io_ldout_ready,U_MEMBLOCK.inner_LoadUnit_2.s3_out_valid,!U_MEMBLOCK.inner_LoadUnit_2.s3_nc_with_data,!U_MEMBLOCK.inner_LoadUnit_2.s3_in_isPrefetch,U_MEMBLOCK.inner_LoadUnit_2.s3_vecActive}),
    aux: 32'(U_MEMBLOCK.inner_LoadUnit_2.s3_in_vaddr),
    default: '0
};
// MainPipe S1候选命中way；qual[0]=首S1（s1_extra_meta_last_REG），[1]=s1_fire；需首拍冻结并跨stall保持。
assign cmo_path_if.probes[33] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_valid),
    kind: cmo_path_agent_dec::CMO_PATH_MAIN_PIPE_S1,
    port_id: 8'(0),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_req_addr),
    id: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_req_miss_id),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_tag_ecc_match_way),
    qual: 16'({14'b0,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_fire,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_extra_meta_last_REG}),
    state: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s1_req_source),
    actual_by_way: 32'({U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_3_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_3_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_2_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_2_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_1_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_1_tl_corrupt,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_0_tl_denied,U_MEMBLOCK.inner_dcache.dcache.errorArray.io_resp_3_0_tl_corrupt}),
    default: '0
};
// MainPipe所有S2接受，qual[0]=s2_fire_to_s3，state[0]=miss；slot预期沿S1冻结引用传递，不能只跟refill。
assign cmo_path_if.probes[34] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_valid),
    kind: cmo_path_agent_dec::CMO_PATH_MAIN_PIPE_S2,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_req_miss_id),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_req_addr),
    raw: 2'({U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_flag_error_tl_denied,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_flag_error_tl_corrupt}),
    state: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_req_miss),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_fire_to_s3),
    default: '0
};
// S2真实refill输入，id=s2_req_miss_id，用已送达L2 token独立形成expected；raw仅用于actual核对。
assign cmo_path_if.probes[35] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_valid && U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_req_miss && U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_refill_info_valid),
    kind: cmo_path_agent_dec::CMO_PATH_MAIN_PIPE_REFILL,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_req_miss_id),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_req_addr),
    raw: 2'({U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_refill_info_bits_error_tl_denied,U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_refill_info_bits_error_tl_corrupt}),
    raw_extra: 2'({U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_flag_error_tl_denied,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_flag_error_tl_corrupt}),
    qual: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s2_fire_to_s3),
    default: '0
};
// 原子DCache请求真实fire；ROB/op来自AtomicsUnit锁存uop，不猜最新主表项。
assign cmo_path_if.probes[36] = '{
    valid: 1'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_req_valid && U_MEMBLOCK.inner_atomicsUnit.io_dcache_req_ready),
    kind: cmo_path_agent_dec::CMO_PATH_ATOMIC_REQUEST,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_atomicsUnit.uop_robIdx_flag,U_MEMBLOCK.inner_atomicsUnit.uop_robIdx_value}),
    op: 9'(U_MEMBLOCK.inner_atomicsUnit.uop_fuOpType),
    addr: 64'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_req_bits_addr),
    data: 512'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_req_bits_amo_data),
    mask: 64'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_req_bits_amo_mask),
    default: '0
};
// AtomicsUnit响应是Valid无ready；state[0]=miss，[1]=replay，只有二者均0为最终响应。
assign cmo_path_if.probes[37] = '{
    valid: 1'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_valid && U_MEMBLOCK.inner_atomicsUnit.state==4'd5),
    kind: cmo_path_agent_dec::CMO_PATH_ATOMIC_RESPONSE,
    port_id: 8'(0),
    owner_valid: 1'(1'b1),
    rob: 9'({U_MEMBLOCK.inner_atomicsUnit.uop_robIdx_flag,U_MEMBLOCK.inner_atomicsUnit.uop_robIdx_value}),
    op: 9'(U_MEMBLOCK.inner_atomicsUnit.uop_fuOpType),
    addr: 64'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_req_bits_addr),
    id: 32'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_bits_id),
    data: 512'(U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_bits_data),
    raw: 2'({U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_bits_tl_error_tl_denied,U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_bits_tl_error_tl_corrupt}),
    state: 32'({30'b0,U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_bits_replay,U_MEMBLOCK.inner_atomicsUnit.io_dcache_resp_bits_miss}),
    default: '0
};
// MainPipe最终原子两项贡献必须逐位OR。raw=refill，raw_extra=slot flag；id=miss_id，state[0]=s3_req_miss，[1]=ack_miss_queue，[2]=miss/replay。
assign cmo_path_if.probes[38] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_atomic_resp_valid),
    kind: cmo_path_agent_dec::CMO_PATH_MAIN_ATOMIC_RESPONSE,
    port_id: 8'(0),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_addr),
    id: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_atomic_resp_bits_miss_id),
    raw: 2'({U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_l2_error_wb_tl_denied,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_l2_error_wb_tl_corrupt}),
    raw_extra: 2'({U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_flag_error_beu_tl_denied,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_flag_error_beu_tl_corrupt}),
    state: 32'({29'b0,U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_atomic_resp_bits_miss,U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_atomic_resp_bits_ack_miss_queue,U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_miss}),
    aux: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_atomic_resp_bits_source),
    default: '0
};
// 真实coherence元信息写，state=coh状态；不写error的普通meta更新不能清错误来源。
assign cmo_path_if.probes[39] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_meta_write_valid),
    kind: cmo_path_agent_dec::CMO_PATH_META_WRITE,
    port_id: 8'(0),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_meta_write_bits_idx),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_meta_write_bits_way_en),
    state: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_meta_write_bits_meta_coh_state),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_addr),
    default: '0
};
// 真实tag替换写接受，配合meta结束旧line适用身份但不凭空清errorArray。
assign cmo_path_if.probes[40] = '{
    valid: 1'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_tag_write_valid),
    kind: cmo_path_agent_dec::CMO_PATH_TAG_WRITE,
    port_id: 8'(0),
    idx: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_tag_write_bits_idx),
    way: 16'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.io_tag_write_bits_way_en),
    addr: 64'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_addr),
    aux: 32'(U_MEMBLOCK.inner_dcache.dcache.mainPipe.s3_req_miss_id),
    default: '0
};
// 外TL真实A.fire；id=source，op=opcode，state[2:0]=size，[5:3]=param；DCache aux=reqSource，qual[0]=echo.isKeyword。
assign cmo_path_if.probes[41] = '{
    valid: 1'(U_MEMBLOCK.auto_inner_dcache_client_out_a_valid && U_MEMBLOCK.auto_inner_dcache_client_out_a_ready),
    kind: cmo_path_agent_dec::CMO_PATH_DCACHE_TL_A,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_source),
    op: 9'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_opcode),
    addr: 64'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_address),
    data: 512'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_data),
    mask: 64'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_mask),
    state: 32'({26'b0,U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_param,U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_size}),
    aux: 32'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_user_reqSource),
    qual: 16'(U_MEMBLOCK.auto_inner_dcache_client_out_a_bits_echo_isKeyword),
    default: '0
};
// 外TL真实D.fire；id=source，op=opcode，state[2:0]=size，[4:3]=param，raw为实送错误；beat由唯一adapter按token累计。
assign cmo_path_if.probes[42] = '{
    valid: 1'(U_MEMBLOCK.auto_inner_dcache_client_out_d_valid && U_MEMBLOCK.auto_inner_dcache_client_out_d_ready),
    kind: cmo_path_agent_dec::CMO_PATH_DCACHE_TL_D,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_source),
    op: 9'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_opcode),
    data: 512'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_data),
    raw: 2'({U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_denied,U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_corrupt}),
    state: 32'({27'b0,U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_param,U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_size}),
    aux: 32'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_sink),
    qual: 16'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_echo_isKeyword),
    default: '0
};
// 外TL真实A.fire；id=source，op=opcode，state[2:0]=size，[5:3]=param；DCache aux=reqSource，qual[0]=echo.isKeyword。
assign cmo_path_if.probes[43] = '{
    valid: 1'(U_MEMBLOCK.auto_inner_buffers_out_a_valid && U_MEMBLOCK.auto_inner_buffers_out_a_ready),
    kind: cmo_path_agent_dec::CMO_PATH_UNCACHE_TL_A,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.auto_inner_buffers_out_a_bits_source),
    op: 9'(U_MEMBLOCK.auto_inner_buffers_out_a_bits_opcode),
    addr: 64'(U_MEMBLOCK.auto_inner_buffers_out_a_bits_address),
    data: 512'(U_MEMBLOCK.auto_inner_buffers_out_a_bits_data),
    mask: 64'(U_MEMBLOCK.auto_inner_buffers_out_a_bits_mask),
    state: 32'({26'b0,U_MEMBLOCK.auto_inner_buffers_out_a_bits_param,U_MEMBLOCK.auto_inner_buffers_out_a_bits_size}),
    qual: 16'((U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd0) ? U_MEMBLOCK.inner_uncache.entries_0_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd1) ? U_MEMBLOCK.inner_uncache.entries_1_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd2) ? U_MEMBLOCK.inner_uncache.entries_2_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd3) ? U_MEMBLOCK.inner_uncache.entries_3_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd4) ? U_MEMBLOCK.inner_uncache.entries_4_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd5) ? U_MEMBLOCK.inner_uncache.entries_5_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd6) ? U_MEMBLOCK.inner_uncache.entries_6_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd7) ? U_MEMBLOCK.inner_uncache.entries_7_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd8) ? U_MEMBLOCK.inner_uncache.entries_8_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd9) ? U_MEMBLOCK.inner_uncache.entries_9_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd10) ? U_MEMBLOCK.inner_uncache.entries_10_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd11) ? U_MEMBLOCK.inner_uncache.entries_11_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd12) ? U_MEMBLOCK.inner_uncache.entries_12_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd13) ? U_MEMBLOCK.inner_uncache.entries_13_nc : (U_MEMBLOCK.auto_inner_buffers_out_a_bits_source == 4'd14) ? U_MEMBLOCK.inner_uncache.entries_14_nc : U_MEMBLOCK.inner_uncache.entries_15_nc),
    default: '0
};
// 外TL真实D.fire；id=source，op=opcode，state[2:0]=size，[4:3]=param，raw为实送错误；beat由唯一adapter按token累计。
assign cmo_path_if.probes[44] = '{
    valid: 1'(U_MEMBLOCK.auto_inner_buffers_out_d_valid && U_MEMBLOCK.auto_inner_buffers_out_d_ready),
    kind: cmo_path_agent_dec::CMO_PATH_UNCACHE_TL_D,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.auto_inner_buffers_out_d_bits_source),
    op: 9'(U_MEMBLOCK.auto_inner_buffers_out_d_bits_opcode),
    data: 512'(U_MEMBLOCK.auto_inner_buffers_out_d_bits_data),
    raw: 2'({U_MEMBLOCK.auto_inner_buffers_out_d_bits_denied,U_MEMBLOCK.auto_inner_buffers_out_d_bits_corrupt}),
    state: 32'({27'b0,U_MEMBLOCK.auto_inner_buffers_out_d_bits_param,U_MEMBLOCK.auto_inner_buffers_out_d_bits_size}),
    default: '0
};
// D direct广播只要求真实GrantData valid；qual[0]=echo.isKeyword，[1]=D.ready。beat由当前token已fire计数决定，不能等待未来fire冒充当拍消费。
assign cmo_path_if.probes[45] = '{
    valid: 1'(U_MEMBLOCK.auto_inner_dcache_client_out_d_valid),
    kind: cmo_path_agent_dec::CMO_PATH_DCACHE_TL_D_OFFER,
    port_id: 8'(0),
    id: 32'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_source),
    op: 9'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_opcode),
    data: 512'(U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_data),
    raw: 2'({U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_denied,U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_corrupt}),
    qual: 16'({14'b0,U_MEMBLOCK.auto_inner_dcache_client_out_d_ready,U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_echo_isKeyword}),
    default: '0
};
// 原子state6实际门控；qual[0]仅核对延迟连接。独立CSR答案取顶层历史state6_sample-2。
assign cmo_path_if.probes[46] = '{
    valid: 1'(U_MEMBLOCK.inner_atomicsUnit.state==4'd6),
    kind: cmo_path_agent_dec::CMO_PATH_ATOMIC_LATCH_CONTROL,
    port_id: 8'd0,
    qual: 16'(U_MEMBLOCK.inner_atomicsUnit.io_csrCtrl_cache_error_enable),
    owner_valid: 1'b1,
    rob: {U_MEMBLOCK.inner_atomicsUnit.uop_robIdx_flag,U_MEMBLOCK.inner_atomicsUnit.uop_robIdx_value},
    op: U_MEMBLOCK.inner_atomicsUnit.uop_fuOpType,
    raw: {U_MEMBLOCK.inner_atomicsUnit.dcache_resp_tl_error_tl_denied,U_MEMBLOCK.inner_atomicsUnit.dcache_resp_tl_error_tl_corrupt},
    default: '0
};
// 外D经过TLBuffer到达Uncache后的真实消费；当前内部D.ready恒1，不能把外部fire时刻当作busError生成时刻。
assign cmo_path_if.probes[47] = '{
    valid: 1'(U_MEMBLOCK.inner_uncache.auto_client_out_d_valid),
    kind: cmo_path_agent_dec::CMO_PATH_UNCACHE_CONSUME_D,
    port_id: 8'd0,
    id: 32'(U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source),
    raw: {U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_denied,U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_corrupt},
    addr: 64'(U_MEMBLOCK.inner_uncache.io_busError_ecc_error_bits),
    qual: 16'({1'((U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd0) ? (U_MEMBLOCK.inner_uncache.entries_0_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd1) ? (U_MEMBLOCK.inner_uncache.entries_1_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd2) ? (U_MEMBLOCK.inner_uncache.entries_2_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd3) ? (U_MEMBLOCK.inner_uncache.entries_3_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd4) ? (U_MEMBLOCK.inner_uncache.entries_4_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd5) ? (U_MEMBLOCK.inner_uncache.entries_5_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd6) ? (U_MEMBLOCK.inner_uncache.entries_6_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd7) ? (U_MEMBLOCK.inner_uncache.entries_7_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd8) ? (U_MEMBLOCK.inner_uncache.entries_8_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd9) ? (U_MEMBLOCK.inner_uncache.entries_9_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd10) ? (U_MEMBLOCK.inner_uncache.entries_10_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd11) ? (U_MEMBLOCK.inner_uncache.entries_11_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd12) ? (U_MEMBLOCK.inner_uncache.entries_12_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd13) ? (U_MEMBLOCK.inner_uncache.entries_13_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd14) ? (U_MEMBLOCK.inner_uncache.entries_14_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.entries_15_cmd == 5'd1)),U_MEMBLOCK.inner_uncache.io_busError_ecc_error_valid}),
    default: '0
};
`endif
