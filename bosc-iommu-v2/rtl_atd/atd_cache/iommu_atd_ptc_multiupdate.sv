//
//
//
module iommu_atd_ptc_multiupdate #(
    parameter               CACHE_TYPE                  = 0, // 0:S2TC, 1:S1PTC
    parameter               TLB_QIDX_WIDTH              = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    parameter               TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter               CABIN_UPD_IDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_UPD_NUM               = 2**CABIN_UPD_IDX_WIDTH,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter               SPARE_PARAM                 = 0
)(
    input  logic                                        clk,
    input  logic                                        rstn,
    //
    input  logic                                        update_req_valid_i,
    output logic                                        update_req_ready_o,
    input  logic [TLB_QIDX_WIDTH-1:0]                   update_req_idx_i,
    input  logic [2:0]                                  update_req_lvl_i,
    input  logic                                        update_req_prefetched_i,
    input  logic [511:0]                                update_req_i,
    input  logic [63:0]                                 update_req_svnapot_i,
    input  logic [19:0]                                 update_req_pscid_i,
    input  logic                                        update_req_gv_i,
    input  logic [15:0]                                 update_req_gscid_i,
    input  logic [63:12]                                update_req_va_i,
    input  logic                                        update_req_sxl_i,
    output logic                                        update_ack_valid_o,
    output logic [TLB_QIDX_WIDTH-1:0]                   update_ack_idx_o,
    //
    output logic                                        tc_update_req_valid_o,
    input  logic                                        tc_update_req_ready_i,
    output UPDATE_REQ_TYPE                              tc_update_req_o,
    input  logic                                        tc_update_ack_valid_i,
    input  UPDATE_REQ_TYPE                              tc_update_ack_i,
    //
    input  logic                                        tc_ram_initial_done_i,
    //
    input  logic                                        csr_fctl_gxl_i,
    //
    input  logic                                        spare_in
);
//=== Declare === {{{
    localparam S_IDLE                                   = 2'b00;
    localparam S_REQ                                    = 2'b10;

    logic [1:0]                                         cs, ns;

    logic [63:0]                                        update_pte[0:7];
    logic [63:0]                                        update_pte_ff[0:7];
    logic [511:0]                                       update_req_ff;
    logic [7:0]                                         update_pte_valid, update_pte_valid_raw;

    logic [63:12]                                       update_req_gpa_i, update_req_gpa_ff;
    logic [2:0]                                         update_pte_position;
    logic                                               update_pte_is_svnapot, update_pte_is_svnapot_ff;

    logic                                               tc_update_req_valid;

    logic [2:0]                                         curr_update_req_idx;
    logic [2:0]                                         first_update_req_idx, next_update_req_idx;
    logic                                               curr_update_req_accepted;
    logic [7:0]                                         all_update_req_accepted;

    logic [TLB_QIDX_WIDTH-1:0]                          update_req_idx;
    logic [2:0]                                         update_req_lvl;
    logic                                               update_req_prefetched;
    logic [19:0]                                        update_req_pscid;
    logic                                               update_req_gv;
    logic [15:0]                                        update_req_gscid;
    logic [63:12]                                       update_req_va;
    logic                                               update_req_sxl;

    logic [TLB_QIDX_WIDTH-1:0]                          update_req_idx_ff;
    logic [2:0]                                         update_req_lvl_ff;
    logic                                               update_req_prefetched_ff;
    logic [19:0]                                        update_req_pscid_ff;
    logic                                               update_req_gv_ff;
    logic [15:0]                                        update_req_gscid_ff;
    logic [63:12]                                       update_req_va_ff;
    logic                                               update_req_sxl_ff;

    typedef struct packed {
        logic                       valid;
        logic [7:0]                 ack_got;
        logic [TLB_QIDX_WIDTH-1:0]  tlb_qidx;
    } update_ack_entry_t;

    update_ack_entry_t [CABIN_UPD_NUM-1:0]              update_ack_entry;   // use to store {TLB_QIDX, ack_got[7:0]} info

    logic [CABIN_UPD_NUM-1:0]                           update_ack_entry_insel;
    logic [CABIN_UPD_NUM-1:0]                           update_ack_entry_outsel;
    logic [CABIN_UPD_NUM-1:0]                           update_ack_entry_valid;
    logic                                               update_ack_entry_full;
    logic [CABIN_UPD_NUM-1:0]                           update_ack_entry_out_valid;
    logic [CABIN_UPD_NUM-1:0] [TLB_QIDX_WIDTH-1:0]      update_ack_entry_out_idx;

    logic [63:12]                                       update_req_addr_muxed;

//}}}

//=== Main Code === {{{
//=== pte extract {{{
generate
    if(CACHE_TYPE==0) begin : s2_update_pte_positioin_gen
        assign update_req_gpa_i    = update_req_va_i;
        assign update_req_gpa_ff   = update_req_va_ff;
        assign update_pte_position = (cs==S_IDLE) ? ((update_req_lvl_i =='d4) ? {update_req_gpa_ff[50:48]} :
                                                     (update_req_lvl_i =='d3) ? {update_req_gpa_ff[41:39]} :
                                                     (update_req_lvl_i =='d2) ? {update_req_gpa_ff[32:30]} :
                                                     (update_req_lvl_i =='d1) ? (csr_fctl_gxl_i==1'b1 ? {update_req_gpa_ff[24:22]} : {update_req_gpa_ff[23:21]}) :
                                                     (update_req_lvl_i =='d0) ? {update_req_gpa_ff[14:12]} : 3'b0
                                                    ) :
                                                    ((update_req_lvl_ff=='d4) ? {update_req_gpa_ff[50:48]} :
                                                     (update_req_lvl_ff=='d3) ? {update_req_gpa_ff[41:39]} :
                                                     (update_req_lvl_ff=='d2) ? {update_req_gpa_ff[32:30]} :
                                                     (update_req_lvl_ff=='d1) ? (csr_fctl_gxl_i==1'b1 ? {update_req_gpa_ff[24:22]} : {update_req_gpa_ff[23:21]}) :
                                                     (update_req_lvl_ff=='d0) ? {update_req_gpa_ff[14:12]} : 3'b0
                                                    );
    end
    else begin : s1_update_pte_position_gen
        assign update_req_gpa_i    = 'd0;
        assign update_req_gpa_ff   = 'd0;
        assign update_pte_position = (cs==S_IDLE) ? ((update_req_lvl_i =='d4) ? {update_req_va_i[50:48]} :
                                                     (update_req_lvl_i =='d3) ? {update_req_va_i[41:39]} :
                                                     (update_req_lvl_i =='d2) ? {update_req_va_i[32:30]} :
                                                     (update_req_lvl_i =='d1) ? (update_req_sxl_i==1'b1 ? {update_req_va_i[24:22]} : {update_req_va_i[23:21]}) :
                                                     (update_req_lvl_i =='d0) ? {update_req_va_i[14:12]} : 3'b0
                                                    ) :
                                                    ((update_req_lvl_ff=='d4) ? {update_req_va_ff[50:48]} :
                                                     (update_req_lvl_ff=='d3) ? {update_req_va_ff[41:39]} :
                                                     (update_req_lvl_ff=='d2) ? {update_req_va_ff[32:30]} :
                                                     (update_req_lvl_ff=='d1) ? (update_req_sxl_ff==1'b1 ? {update_req_va_ff[24:22]} : {update_req_va_ff[23:21]}) :
                                                     (update_req_lvl_ff=='d0) ? {update_req_va_ff[14:12]} : 3'b0
                                                    );
    end
endgenerate

    assign update_pte[0] = (cs==S_IDLE) ? update_req_i[63 :  0] : update_req_ff[63 :  0];
    assign update_pte[1] = (cs==S_IDLE) ? update_req_i[127: 64] : update_req_ff[127: 64];
    assign update_pte[2] = (cs==S_IDLE) ? update_req_i[191:128] : update_req_ff[191:128];
    assign update_pte[3] = (cs==S_IDLE) ? update_req_i[255:192] : update_req_ff[255:192];
    assign update_pte[4] = (cs==S_IDLE) ? update_req_i[319:256] : update_req_ff[319:256];
    assign update_pte[5] = (cs==S_IDLE) ? update_req_i[383:320] : update_req_ff[383:320];
    assign update_pte[6] = (cs==S_IDLE) ? update_req_i[447:384] : update_req_ff[447:384];
    assign update_pte[7] = (cs==S_IDLE) ? update_req_i[511:448] : update_req_ff[511:448];

    assign update_pte_ff[0] = update_req_ff[63 :  0];
    assign update_pte_ff[1] = update_req_ff[127: 64];
    assign update_pte_ff[2] = update_req_ff[191:128];
    assign update_pte_ff[3] = update_req_ff[255:192];
    assign update_pte_ff[4] = update_req_ff[319:256];
    assign update_pte_ff[5] = update_req_ff[383:320];
    assign update_pte_ff[6] = update_req_ff[447:384];
    assign update_pte_ff[7] = update_req_ff[511:448];

    assign update_req_idx       = (cs==S_IDLE) ? update_req_idx_i       : update_req_idx_ff       ;
    assign update_req_lvl       = (cs==S_IDLE) ? update_req_lvl_i       : update_req_lvl_ff       ;
    assign update_req_prefetched= (cs==S_IDLE) ? update_req_prefetched_i: update_req_prefetched_ff;
    assign update_req_pscid     = (cs==S_IDLE) ? update_req_pscid_i     : update_req_pscid_ff     ;
    assign update_req_gv        = (cs==S_IDLE) ? update_req_gv_i        : update_req_gv_ff        ;
    assign update_req_gscid     = (cs==S_IDLE) ? update_req_gscid_i     : update_req_gscid_ff     ;
    assign update_req_va        = (cs==S_IDLE) ? update_req_va_i        : update_req_va_ff        ;
    assign update_req_sxl       = (cs==S_IDLE) ? update_req_sxl_i       : update_req_sxl_ff       ;

    assign update_pte_is_svnapot= (cs==S_IDLE) ? update_pte[update_pte_position][63] : update_pte_is_svnapot_ff;

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            update_req_ff           <= 'd0;
            update_req_idx_ff       <= 'd0;
            update_req_lvl_ff       <= 'd0;
            update_req_prefetched_ff<= 'd0;
            update_req_pscid_ff     <= 'd0;
            update_req_gv_ff        <= 'd0;
            update_req_gscid_ff     <= 'd0;
            update_req_va_ff        <= 'd0;
            update_req_sxl_ff       <= 'd0;
            update_pte_is_svnapot_ff<= 'd0;
        end
        else begin
            if(cs==S_IDLE & ns!=S_IDLE) begin
                update_req_ff           <= update_req_i;
                update_req_idx_ff       <= update_req_idx_i;
                update_req_lvl_ff       <= update_req_lvl_i;
                update_req_prefetched_ff<= update_req_prefetched_ff;
                update_req_pscid_ff     <= update_req_pscid_i;
                update_req_gv_ff        <= update_req_gv_i;
                update_req_gscid_ff     <= update_req_gscid_i;
                update_req_va_ff        <= update_req_va_i;
                update_req_sxl_ff       <= update_req_sxl_i;
                update_pte_is_svnapot_ff<= update_pte_is_svnapot;

            end
        end
    end


genvar a;
generate
    for(a=0; a<8; a++) begin : pte_valid_check_gen
        iommu_atd_ptc_multiupdate_ptecheck U_update_pte_valid_raw(update_pte[a], update_req_lvl, update_pte_valid_raw[a]);
        assign update_pte_valid[a] = (update_pte_is_svnapot) ? ((a==update_pte_position) ? 1'b1 : 1'b0) :
                                                               update_pte_valid_raw[a];
    end
endgenerate

    assign tc_update_req_valid  = tc_ram_initial_done_i & update_req_valid_i;
    assign update_req_ready_o   = tc_ram_initial_done_i & (cs==S_IDLE) & (~update_ack_entry_full);
//}}}

//=== FSM Stage 1 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
//}}}

//=== FSM Stage 2 {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            // valid_input            ack_entry_allocate_ok      has_valid_pte
            if(tc_update_req_valid & (~update_ack_entry_full) & (|update_pte_valid)) begin
                ns = S_REQ;
//                if(update_pte_is_svnapot)
//                    ns = S_SVNAPOT;
//                else
//                    ns = S_MUL;
            end
            else
                ns = S_IDLE;
        end
//        S_MUL, S_SVNAPOT: begin
        S_REQ: begin
            if(&all_update_req_accepted)
                ns = S_IDLE;
            else
                ns = S_REQ;
        end
        default: ns = cs;
        endcase
    end
//}}}

//=== FSM Stage 3 {{{
//=== pte_idx {{{
    assign curr_update_req_accepted = tc_update_req_valid_o & tc_update_req_ready_i;

    iommu_atd_ptc_multiupdate_firstidxcal U_first_update_req_idx(update_pte_valid, first_update_req_idx);
    iommu_atd_ptc_multiupdate_idxcal U_next_update_req_idx(curr_update_req_idx, update_pte_valid, next_update_req_idx);

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            curr_update_req_idx <= 'd0;
        else begin
//            if(ns==S_SVNAPOT) begin
//                if(cs==S_IDLE)
//                    curr_update_req_idx <= update_pte_position;
//                else
//                    curr_update_req_idx <= curr_update_req_idx;
//            end
//            else if(ns==S_MUL) begin
//                if(curr_update_req_accepted)
//                    curr_update_req_idx <= next_update_req_idx;
//                else
//                    curr_update_req_idx <= curr_update_req_idx;
//            end
            if(ns==S_REQ) begin
                if(cs==S_IDLE)                                              // the first pte
                    curr_update_req_idx <= first_update_req_idx;
                else if(curr_update_req_accepted)                           // a pte is accepted
                    curr_update_req_idx <= next_update_req_idx;
                else                                                        // a pte is pending or no pte is sending
                    curr_update_req_idx <= curr_update_req_idx;
            end
            else
                curr_update_req_idx <= curr_update_req_idx;
        end
    end
//}}}

//=== tc_update_req out {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            tc_update_req_valid_o <= 1'b0;
        else begin
            if(ns==S_IDLE)
                tc_update_req_valid_o <= 1'b0;
//            else if(ns==S_SVNAPOT)
//                tc_update_req_valid_o <= 1'b1;
//            else if(ns==S_MUL) begin
//                if(cs==S_IDLE)
//                    tc_update_req_valid_o <= 1'b1;
//                else if(curr_update_req_idx != next_update_req_idx)
//                    tc_update_req_valid_o <= 1'b1;
//                else
//                    tc_update_req_valid_o <= 1'b0;
//            end
            else if(ns==S_REQ) begin
                if(cs==S_IDLE)                                              // the first pte
                    tc_update_req_valid_o <= 1'b1;
                else if(tc_update_req_valid_o & (~tc_update_req_ready_i))   // a pte is pendding 
                    tc_update_req_valid_o <= 1'b1;
                else if(curr_update_req_idx != next_update_req_idx)         // more pte need send
                    tc_update_req_valid_o <= 1'b1;
                else                                                        // no more pte to be sent
                    tc_update_req_valid_o <= 1'b0;
            end
        end
    end

generate
    if(CACHE_TYPE==0) begin : s2_tc_update_req_o_gen
        assign update_req_addr_muxed = (update_req_lvl_ff=='d4) ? {update_req_gpa_ff[63:51], curr_update_req_idx/*update_req_gpa_ff[50:48]*/, update_req_gpa_ff[47:12]} :
                                       (update_req_lvl_ff=='d3) ? {update_req_gpa_ff[63:42], curr_update_req_idx/*update_req_gpa_ff[41:39]*/, update_req_gpa_ff[38:12]} :
                                       (update_req_lvl_ff=='d2) ? {update_req_gpa_ff[63:33], curr_update_req_idx/*update_req_gpa_ff[32:30]*/, update_req_gpa_ff[29:12]} :
                                       (update_req_lvl_ff=='d1) ? (csr_fctl_gxl_i==1'b1 ? {update_req_gpa_ff[63:25], curr_update_req_idx/*update_req_gpa_ff[24:22]*/, update_req_gpa_ff[21:12]} : {update_req_gpa_ff[63:24], curr_update_req_idx/*update_req_gpa_ff[23:21]*/, update_req_gpa_ff[20:12]}) :
                                       (update_req_lvl_ff=='d0) ? {update_req_gpa_ff[63:15], curr_update_req_idx/*update_req_gpa_ff[14:12]*/} : 'd0;

        always@(*) begin
            tc_update_req_o.idx          = {curr_update_req_idx, 1'b0, update_req_idx_ff}; //{pte_idx, refill_flag(always 0), TLBQ_IDX}
            tc_update_req_o.lvl          = update_req_lvl_ff;
            tc_update_req_o.prefetched   = update_req_prefetched_ff;
            tc_update_req_o.addr         = update_req_addr_muxed;//{update_req_va_ff[63:15], curr_update_req_idx};
            tc_update_req_o.gscid        = update_req_gscid_ff;
            tc_update_req_o.N            = update_pte_ff[curr_update_req_idx][63];
            tc_update_req_o.PBMT         = update_pte_ff[curr_update_req_idx][62:61];
            tc_update_req_o.D            = update_pte_ff[curr_update_req_idx][7];
            tc_update_req_o.A            = update_pte_ff[curr_update_req_idx][6];
            tc_update_req_o.PPN          = update_pte_ff[curr_update_req_idx][53:10];
            tc_update_req_o.PERM         = update_pte_ff[curr_update_req_idx][5:1];
            tc_update_req_o.V            = update_pte_ff[curr_update_req_idx][0];
        end
    end
    else begin : s1_tc_update_req_o_gen
        assign update_req_addr_muxed = (update_req_lvl_ff=='d4) ? {update_req_va_ff[63:51], curr_update_req_idx/*update_req_va_ff[50:48]*/, update_req_va_ff[47:12]} :
                                       (update_req_lvl_ff=='d3) ? {update_req_va_ff[63:42], curr_update_req_idx/*update_req_va_ff[41:39]*/, update_req_va_ff[38:12]} :
                                       (update_req_lvl_ff=='d2) ? {update_req_va_ff[63:33], curr_update_req_idx/*update_req_va_ff[32:30]*/, update_req_va_ff[29:12]} :
                                       (update_req_lvl_ff=='d1) ? (update_req_sxl_ff==1'b1 ? {update_req_va_ff[63:25], curr_update_req_idx/*update_req_va_ff[24:22]*/, update_req_va_ff[21:12]} : {update_req_va_ff[63:24], curr_update_req_idx/*update_req_va_ff[23:21]*/, update_req_va_ff[20:12]}) :
                                       (update_req_lvl_ff=='d0) ? {update_req_va_ff[63:15], curr_update_req_idx/*update_req_va_ff[14:12]*/} : 'd0;

        always@(*) begin
            tc_update_req_o.idx          = {curr_update_req_idx, 1'b0, update_req_idx_ff}; //{pte_idx, refill_flag(always 0), TLBQ_IDX}
            tc_update_req_o.lvl          = update_req_lvl_ff;
            tc_update_req_o.prefetched   = update_req_prefetched_ff;
            tc_update_req_o.addr         = update_req_addr_muxed;//{update_req_va_ff[63:15], curr_update_req_idx};
            tc_update_req_o.gv           = update_req_gv_ff;
            tc_update_req_o.gscid        = update_req_gscid_ff;
            tc_update_req_o.pscid        = update_req_pscid_ff;
            tc_update_req_o.sxl          = update_req_sxl_ff;
            tc_update_req_o.N            = update_pte_ff[curr_update_req_idx][63];
            tc_update_req_o.PBMT         = update_pte_ff[curr_update_req_idx][62:61];
            tc_update_req_o.D            = update_pte_ff[curr_update_req_idx][7];
            tc_update_req_o.A            = update_pte_ff[curr_update_req_idx][6];
            tc_update_req_o.PPN          = update_pte_ff[curr_update_req_idx][53:10];
            tc_update_req_o.PERM         = update_pte_ff[curr_update_req_idx][5:1];
            tc_update_req_o.V            = update_pte_ff[curr_update_req_idx][0];
        end
    end
endgenerate
//}}}

//=== tc_update_req accept monitor {{{
genvar k;
generate
    for(k=0; k<8; k++) begin : update_req_accept_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                all_update_req_accepted[k] <= 'd0;
            else begin
                if(ns==S_IDLE)
                    all_update_req_accepted[k] <= 'd0;
                else if(ns==S_REQ) begin
                    if(cs==S_IDLE) begin
                        if(~update_pte_valid[k])
                            all_update_req_accepted[k] <= 1'b1;                     // invalid pte is initialized to be already accepted
                        else
                            all_update_req_accepted[k] <= 1'b0;                     // valid pte should waiting for real ready_i
                    end
                    else if((curr_update_req_idx==k) & curr_update_req_accepted)    // valid pte is accepted
                        all_update_req_accepted[k] <= 1'b1;
                end
            end
        end
    end
endgenerate
//}}}

//=== tc_update_ack monitor and update_ack out gen {{{
    assign update_ack_entry_full = &update_ack_entry_valid;

genvar m;
generate
    for(m=0; m<CABIN_UPD_NUM; m++) begin : update_ack_entry_gen
        assign update_ack_entry_valid[m] = update_ack_entry[m].valid;

        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                update_ack_entry[m].tlb_qidx<= 'd0;
                update_ack_entry[m].ack_got <= 'd0;
                update_ack_entry[m].valid   <= 1'b0;
            end
            else begin
                if(update_ack_entry[m].valid) begin
                    if(update_ack_entry_outsel[m]) begin                            // select this entry for update_ack to TLBQ
                        update_ack_entry[m].valid <= 1'b0;
                    end
//                    else begin                                                      // monitor update_ack from TLB
                        if( tc_update_ack_valid_i & 
                            (tc_update_ack_i.idx[TLB_QIDX_WIDTH-1:0]==update_ack_entry[m].tlb_qidx) &
                            (tc_update_ack_i.idx[TLB_QIDX_WIDTH]==1'b0) &   // not a refile update_ack
                            1'b1
                            ) begin
                            case(tc_update_ack_i.idx[TLB_QIDX_WIDTH+3:TLB_QIDX_WIDTH+1])
                            3'd 0:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b00000001;
                            3'd 1:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b00000010;
                            3'd 2:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b00000100;
                            3'd 3:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b00001000;
                            3'd 4:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b00010000;
                            3'd 5:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b00100000;
                            3'd 6:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b01000000;
                            3'd 7:  update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got | 8'b10000000;
                            default:update_ack_entry[m].ack_got <= update_ack_entry[m].ack_got;
                            endcase
                        end
//                    end
                end
                else begin
                    if(ns==S_REQ & cs==S_IDLE) begin                                // a new update_req is sending
                        if(update_ack_entry_insel[m]) begin                         // select this entry to store ack info
                            update_ack_entry[m].tlb_qidx<= update_req_idx;
                            update_ack_entry[m].ack_got <= ~update_pte_valid;
                            update_ack_entry[m].valid   <= 1'b1;
                        end
                    end
                end
            end
        end

        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(CABIN_UPD_IDX_WIDTH)) U_update_ack_entry_insel(
            .valid_i    (update_ack_entry_valid     ),
            .update_i   ((cs==S_IDLE & ns==S_REQ)   ),
            .tag_i      (CABIN_UPD_IDX_WIDTH'(m)    ),
            .update_o   (update_ack_entry_insel[m]  )
        );

        assign update_ack_entry_out_valid[m]= update_ack_entry[m].valid & (update_ack_entry[m].ack_got==8'hff);
        assign update_ack_entry_out_idx[m]  = update_ack_entry[m].tlb_qidx;
    end
endgenerate
    
    iommu_acd_bus_handler_trans_arb #(
    /*parameter    */               .ARB_TYPE            (0                          ),   //= 0,
    /*parameter    */               .REQ_NUM             (CABIN_UPD_NUM              ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (logic [TLB_QIDX_WIDTH-1:0] ),   //= logic,
    /*parameter    */               .AXIVLDRDY           (1                          )    //= 1
    ) U_update_ack_arb(
    /*input  logic                                  */  .clk                        (clk                        ),
    /*input  logic                                  */  .rstn                       (rstn                       ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_i                      (update_ack_entry_out_valid ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_prior_i                ({CABIN_UPD_NUM{1'b0}}      ),
    /*input  DATA_TYPE [REQ_NUM-1:0]                */  .data_i                     (update_ack_entry_out_idx   ),
    /*output logic [REQ_NUM-1:0]                    */  .gnt_o                      (update_ack_entry_outsel    ),
    /*output logic                                  */  .req_o                      (update_ack_valid_o         ),
    /*output DATA_TYPE                              */  .data_o                     (update_ack_idx_o           ),
    /*input  logic                                  */  .gnt_i                      (1'b1                       ) 
    );



//}}}

//}}}

//}}}

endmodule



module iommu_atd_ptc_multiupdate_ptecheck #( //{{{
    parameter  CACHE_TYPE                  = 0, //0:S1PTC 1:S2PTC
    parameter  SPARE_PARAM                 = 0
)(
    input  logic [63:0]                                 pte_i,
    input  logic [2:0]                                  lvl_i,
    output logic                                        valid_o
);
//    generate
//        if(CACHE_TYPE==0) begin : s1ptc_valid_check_gen
//            //                               v!=0       a!=0        reserve=0             not_w_only/[wxr]-all-0   [ad]-all-0           [u]=0
              assign valid_o = (lvl_i=='d0) ? (pte_i[0] & pte_i[6] & (pte_i[60:54]=='d0) & ~(pte_i[2] & ~pte_i[1])                                          ) :
                                              (pte_i[0] &            (pte_i[60:54]=='d0) & (pte_i[3:1]==3'b000)   & (pte_i[7:6]==2'b0) & (pte_i[4]==1'b0) );
//        end
//        else begin : s2ptc_valid_check_gen
//            //               v!=0       a!=-0      reserve=0             not_w_only             & g
//            assign valid_o = pte_i[0] & pte_i[6] & (pte_i[60:54]=='d0) & (pte_i[2] & ~pte_i[1]);
//        end
//    endgenerate

endmodule //}}}



module iommu_atd_ptc_multiupdate_idxcal(
    input  logic [2:0]                                  curr_idx,
    input  logic [7:0]                                  valid_list,
    output logic [2:0]                                  next_idx
);

    logic [7:0]  valid_list_masked;

    assign valid_list_masked = {valid_list >> (curr_idx+1)} << (curr_idx+1);

    always@(*) begin
        next_idx = curr_idx;
        casez(valid_list_masked)
        8'b???????1: next_idx = 3'd0;
        8'b??????10: next_idx = 3'd1;
        8'b?????100: next_idx = 3'd2;
        8'b????1000: next_idx = 3'd3;
        8'b???10000: next_idx = 3'd4;
        8'b??100000: next_idx = 3'd5;
        8'b?1000000: next_idx = 3'd6;
        8'b10000000: next_idx = 3'd7;
        8'b00000000: next_idx = curr_idx;
        default:     next_idx = curr_idx;
        endcase
    end
endmodule

module iommu_atd_ptc_multiupdate_firstidxcal(
    input  logic [7:0]                                  valid_list,
    output logic [2:0]                                  idx
);
    always@(*) begin
        idx = 3'd0;
        casez(valid_list)
        8'b???????1: idx = 3'd0;
        8'b??????10: idx = 3'd1;
        8'b?????100: idx = 3'd2;
        8'b????1000: idx = 3'd3;
        8'b???10000: idx = 3'd4;
        8'b??100000: idx = 3'd5;
        8'b?1000000: idx = 3'd6;
        8'b10000000: idx = 3'd7;
        8'b00000000: idx = 3'd0;    // this branch should never active
        default:     idx = 3'd0;
        endcase
    end
endmodule
