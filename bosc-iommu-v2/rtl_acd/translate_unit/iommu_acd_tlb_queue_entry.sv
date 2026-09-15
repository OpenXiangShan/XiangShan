//////////////////////////////////////////////////////////////////
// iommu_acd_tlb_queue_entry
//////////////////////////////////////////////////////////////////
module iommu_acd_tlb_queue_entry #( //{{{
//{{{ PARAM
    parameter               TLB_QUEUE_DBG               = 1'b0,
    parameter               TRANS_QIDX_WIDTH            = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter               TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter               TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          TRANSLATE_REQ_TYPE          = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE          = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter type          FAULT_RPT_TYPE              = iommu_acd_pkg::FAULT_RPT_TYPE,
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter               INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter               SPARE_PARAM                 = 0
//}}}                                                   
)(                                                      
//{{{ IO                                                
    input  logic                                        clk,
    input  logic                                        rstn,
    input  logic [TLB_QIDX_WIDTH  :0]                   idx,
    // BUS_HANDLER                                      
    input  logic                                        update_i,
    input  logic                                        translate_req_valid_i,
    input  TRANSLATE_REQ_TYPE                           translate_req_i,
    output logic                                        translate_ack_valid_o,
    input  logic                                        translate_ack_ready_i,
    output TRANSLATE_ACK_TYPE                           translate_ack_o,
    // PTW                                              
    output logic                                        ptw_req_valid_o,
    input  logic                                        ptw_req_ready_i,
    output PTW_REQ_TYPE                                 ptw_req_o,
    input  logic                                        ptw_ack_valid_i,
    input  PTW_ACK_TYPE                                 ptw_ack_i,
    // LOOKUP                                           
    output logic                                        lookup_req_valid_o,
    input  logic                                        lookup_req_ready_i,
    output LOOKUP_REQ_TYPE                              lookup_req_o,
    input  logic                                        lookup_ack_valid_i,
    input  LOOKUP_ACK_TYPE                              lookup_ack_i,
    // UPDATE                                           
    output logic                                        update_req_valid_o,
    input  logic                                        update_req_ready_i,
    output UPDATE_REQ_TYPE                              update_req_o,
    input  logic                                        update_ack_valid_i,
    input  UPDATE_REQ_TYPE                              update_ack_i,
    // FAULT                                            
    output logic                                        fault_rpt_valid_o,
    input  logic                                        fault_rpt_ready_i,
    output FAULT_RPT_TYPE                               fault_rpt_o,
    //                                                  
    output logic                                        valid_o,
    output logic                                        ptw_ongoing_o,
    output logic                                        wr_o,
    input  logic [TLB_QUEUE_DEPTH-1:0]                  valid_i,
    input  logic [TLB_QUEUE_DEPTH-1:0]                  depend_bits_i,
    output logic                                        depend_bit_o,
    input  logic                                        relook_hint_i,
    // CFG                                              
    input  logic                                        csr_fctl_gxl_i,
    input  logic [3:0]                                  csr_ddtp_iommu_mode_i,
    // INFO OUT
    output logic [3:0]                                  info_fsm_o,
    output logic [TRANS_QIDX_WIDTH  :0]                 info_tidx_o,
    output logic                                        info_priv_o,
    output logic                                        info_ext_o,
    output logic                                        info_wr_o,
    output logic                                        info_is_translated_o,
    output logic                                        info_process_id_valid_o,
    output logic [19:0]                                 info_process_id_o,
    output logic [23:0]                                 info_device_id_o,
    output logic [63:12]                                info_va_o,
    // INV
    output logic                                        invalid_req_valid_o,
    input  logic                                        invalid_req_ready_i,
    output INVALID_REQ_TYPE                             invalid_req_o,
    input  logic                                        invalid_ack_valid_i,
    input  INVALID_REQ_TYPE                             invalid_ack_i,
    // MRIF CREDIT
    output logic                                        mrif_credit_req_o,
    input  logic                                        mrif_credit_ack_i,
    //                                                  
    input  logic                                        spare_in
//}}}                                                   
);                                                      
//=== Declare === {{{                                   
    localparam S_IDLE                                   = 4'b0000;
    localparam S_WAIT_DEPEND                            = 4'b0001;
    localparam S_LOOKUP                                 = 4'b0010;
    localparam S_TRANSLATE                              = 4'b0011;
    localparam S_UPDATE                                 = 4'b0100;
    localparam S_UPACK                                  = 4'b1100;
    localparam S_OUTPUT                                 = 4'b0101;              // check permission in this STATE
    localparam S_RELOOK                                 = 4'b0111;
    localparam S_FAULT_RPT                              = 4'b1111;              // rpt fault
    localparam S_MRIF                                   = 4'b1101;              // MRIF
                                                        
    logic [3:0]                                         cs, ns;
    logic [TLB_QUEUE_DEPTH-1:0]                         depend_bits, depend_status; // timely dependency status
    logic                                               hit, hit_ff;            // lookup hit and stored hit info
    logic                                               translate_fail;         // translate fail, and the fail is not permission fail
    logic                                               translate_deny;         // translate got valid pte, but permission fail
    logic                                               translate_no_cache;     // translate do not be cache hint
                                                        
    logic                                               ptw_ack_valid_ff;       // valid ptw_ack store flag
    PTW_ACK_TYPE                                        ptw_ack_ff;             // ptw_ack stored
                                                        
    logic                                               ptw_ack_valid;
                                                        
    logic [8:0]                                         ddi2_i;                 // input device_id's DDI2
    logic [8:0]                                         ddi1_i;                 //                   DDI1
    logic [5:0]                                         ddi0_i;                 //                   DDI0
                                                        
                                                        
    logic [TRANS_QIDX_WIDTH  :0]                        tidx;                   // stored trans_queue idx
    logic                                               priv;                   // stored privilege
    logic                                               ext ;                   // stored execute
    logic                                               wr  ;                   // stored 1 write 0 read
    logic                                               is_translated;          // stored is_tranalted
    logic                                               process_id_valid;       // stored pv
    logic [19:0]                                        process_id;             // stored process_id
    logic [23:0]                                        device_id;              // stored device_id
    logic [63:12]                                       va;                     // stored IOVA
                                                        
    //LOOKUP_REQ_TYPE                                     lookup_req_content;
                                                        
    logic                                               mode_bare, mode_off, mode_2lvl, mode_1lvl;
    logic                                               fault_detect;           // disallow transaction got or lookup hit but permission check fail
    logic [11:0]                                        fault_cause;
    logic                                               fault_dtf;
//    logic                                               process_id_width_err;   // process_id width mismatch with PDTMODE
    logic [2:0]                                         pdi2;                   // process_id's PDI2
    logic [8:0]                                         pdi1;                   //              PDI1
    logic [7:0]                                         pdi0;                   //              PDI0
                                                        
    logic                                               work_ens, work_sum, work_pid_valid, work_priv;
    logic [19:0]                                        work_process_id;
                                                        
    logic [63:12]                                       work_ppn;               // the valid ppn for translate_ack 
    logic [1:0]                                         work_resp;              // the valid resp for translate_ack
    logic                                               svnapot_en;             // pte.N got

    logic [63:12]                                       lookup_ack_gppn_ff;

    logic                                               lookup_ack_i_s1_bare;
    logic                                               lookup_ack_i_s1_sv32;
    logic                                               lookup_ack_i_s1_sv39;
    logic                                               lookup_ack_i_s1_sv48;
    logic                                               lookup_ack_i_s1_sv57;
    logic                                               lookup_ack_i_s2_bare;
    logic                                               lookup_ack_i_s2_sv32x4;
    logic                                               lookup_ack_i_s2_sv39x4;
    logic                                               lookup_ack_i_s2_sv48x4;
    logic                                               lookup_ack_i_s2_sv57x4;
    logic                                               lookup_ack_i_s1_2m;
    logic                                               lookup_ack_i_s1_4m;
    logic                                               lookup_ack_i_s2_2m;
    logic                                               lookup_ack_i_s2_4m;
    logic                                               lookup_ack_i_is_2m;
    logic                                               lookup_ack_i_is_4m;
    logic                                               lookup_ack_i_s1_1g;
    logic                                               lookup_ack_i_s2_1g;
    logic                                               lookup_ack_i_is_1g;
    logic                                               lookup_ack_i_s1_512g;
    logic                                               lookup_ack_i_s2_512g;
    logic                                               lookup_ack_i_is_512g;

    logic                                               ptw_ack_ff_s1_bare;
    logic                                               ptw_ack_ff_s1_sv32;
    logic                                               ptw_ack_ff_s1_sv39;
    logic                                               ptw_ack_ff_s1_sv48;
    logic                                               ptw_ack_ff_s1_sv57;
    logic                                               ptw_ack_ff_s2_bare;
    logic                                               ptw_ack_ff_s2_sv32x4;
    logic                                               ptw_ack_ff_s2_sv39x4;
    logic                                               ptw_ack_ff_s2_sv48x4;
    logic                                               ptw_ack_ff_s2_sv57x4;
    logic                                               ptw_ack_ff_s1_2m;
    logic                                               ptw_ack_ff_s1_4m;
    logic                                               ptw_ack_ff_s2_2m;
    logic                                               ptw_ack_ff_s2_4m;
    logic                                               ptw_ack_ff_is_2m;
    logic                                               ptw_ack_ff_is_4m;
    logic                                               ptw_ack_ff_s1_1g;
    logic                                               ptw_ack_ff_s2_1g;
    logic                                               ptw_ack_ff_is_1g;
    logic                                               ptw_ack_ff_s1_512g;
    logic                                               ptw_ack_ff_s2_512g;
    logic                                               ptw_ack_ff_is_512g;

//    logic                                               lookup_ack_i_S1_A;
    logic                                               lookup_ack_i_S1_D;
//    logic                                               lookup_ack_i_S2_A;
    logic                                               lookup_ack_i_S2_D;

    logic                                               pc_located;

    logic                                               is_dbg_entry;
    logic [2:0]                                         work_range, lookup_ack_i_range, ptw_ack_ff_range;

    logic                                               dirty_check_fail, dirty_check_fail_ff; // D check fail with SADE/GADE active

    logic [1:0]                                         work_pbmt;

    logic                                               work_mrif;
    logic [55:12]                                       work_nppn;
    logic [10:0]                                        work_nid;

    logic                                               ptw_ack_mrif;
//}}}

//=== Main Code === {{{
    assign info_fsm_o               = cs;
    assign info_tidx_o              = tidx             ;
    assign info_priv_o              = priv             ;
    assign info_ext_o               = ext              ;
    assign info_wr_o                = wr               ;
    assign info_is_translated_o     = is_translated    ;
    assign info_process_id_valid_o  = process_id_valid ;
    assign info_process_id_o        = process_id       ;
    assign info_device_id_o         = device_id        ;
    assign info_va_o                = va               ;

    assign is_dbg_entry = idx[TLB_QIDX_WIDTH];

    assign ddi2_i   = translate_req_i.device_id[23:15];
    assign ddi1_i   = translate_req_i.device_id[14:6];
    assign ddi0_i   = translate_req_i.device_id[5:0];

    assign pdi2     = process_id[19:17];
    assign pdi1     = process_id[16:8];
    assign pdi0     = process_id[7:0];

    assign mode_bare        = (csr_ddtp_iommu_mode_i == iommu_acd_pkg::IOMMU_MODE_BARE);
    assign mode_off         = (csr_ddtp_iommu_mode_i == iommu_acd_pkg::IOMMU_MODE_OFF);    // should not have transaction input
    assign mode_2lvl        = (csr_ddtp_iommu_mode_i == iommu_acd_pkg::IOMMU_MODE_2LVL);
    assign mode_1lvl        = (csr_ddtp_iommu_mode_i == iommu_acd_pkg::IOMMU_MODE_1LVL);

    // Will locate PC when following conditions hold:
    // 1) Transaction type is not a Translated request
    // 2) DC.tc.PDTV is 1
    // 3) Transaction has a valid process_id or DC.tc.DPE is 1
    // 4) DC.fsc.pdtp.MODE is not Bare
    assign pc_located       = ~is_translated & lookup_ack_i.PDTV & (lookup_ack_i.DPE | process_id_valid) & (lookup_ack_i.PDTMODE!=iommu_acd_pkg::PDTMODE_BARE);
    assign work_ens         = pc_located ? lookup_ack_i.ENS : 1'b1; //((~lookup_ack_i.PDTV) | (lookup_ack_i.PDTV & ~lookup_ack_i.DPE & (~process_id_valid | lookup_ack_i.PDTMODE==iommu_acd_pkg::PDTMODE_BARE))) ? 1'b1 : lookup_ack_i.ENS;
    assign work_sum         = pc_located ? lookup_ack_i.SUM : 1'b0; //((~lookup_ack_i.PDTV) | (lookup_ack_i.PDTV & ~lookup_ack_i.DPE & (~process_id_valid | lookup_ack_i.PDTMODE==iommu_acd_pkg::PDTMODE_BARE))) ? 1'b0 : lookup_ack_i.SUM;
    assign work_pid_valid   = (lookup_ack_i.PDTV & lookup_ack_i.DPE & ~process_id_valid) ? 1'b1 : process_id_valid;
    assign work_process_id  = (lookup_ack_i.PDTV & lookup_ack_i.DPE & ~process_id_valid) ? 'd0  : process_id;
    assign work_priv        = (lookup_ack_i.PDTV & lookup_ack_i.DPE & ~process_id_valid) ? 1'b0 : priv;

//=== FSM stage1 === {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
//}}}

//=== FSM stage2 === {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE       : begin
            if(update_i) begin
                if(   mode_bare
                    | mode_off
                    | (mode_2lvl & (ddi2_i != 'd0))
                    | (mode_1lvl & ((ddi2_i != 'd0) | (ddi1_i != 'd0)))
                  )
                    ns = S_OUTPUT;
                //else if(depend_bits_i == 'b0)
                //    ns = S_WAIT_DEPEND;
                else
                    ns = S_LOOKUP;
            end
            else
                ns = S_IDLE;
        end
        S_WAIT_DEPEND: begin
            if(depend_status == 'd0)
                ns = S_RELOOK;
            else
                ns = S_WAIT_DEPEND;
        end
        S_LOOKUP, S_RELOOK: begin
            if(lookup_ack_valid_i) begin
                if(hit) begin
                    if(dirty_check_fail & (TLB_QUEUE_DBG=='d0))   // for debug_entry, no ADE handle
                        ns = S_TRANSLATE;
                    else
                        ns = S_OUTPUT;
                end
                else begin
                    if(depend_status != 'd0)
                        ns = S_WAIT_DEPEND;
                    else
                        ns = S_TRANSLATE;
                end
            end
            else
                ns = S_LOOKUP;
        end
        S_TRANSLATE  : begin
            if(ptw_ack_valid) begin                         // get ptw_ack
                if(dirty_check_fail_ff)                     // internal invalid not finish 
                    ns = S_TRANSLATE;
                else if(translate_fail | translate_no_cache)// translate_fail and not just permission fail, ack to BUS_HANDLER without update
                    ns = S_OUTPUT;                          // if non-cache hint, do not update
                else if(ptw_ack_mrif & ~wr)                 // RD & MRIF active, no need credit
                    ns = S_OUTPUT;
                else if(ptw_ack_mrif & wr)                  // WR & MRIF active, need credit
                    ns = S_MRIF;
                else                                        // translate_success or just permission fail, update to CACHE
                    ns = S_UPDATE;
            end
            else
                ns = S_TRANSLATE;                           // neither ptw_ack nor INV got, keep waiting
        end
        S_UPDATE     : begin                                // only when ptw_ack non-fail, will go into S_UPDATE
            if(update_req_ready_i)
                ns = S_UPACK;
            else
                ns = S_UPDATE;
        end
        S_UPACK      : begin
            if(update_ack_valid_i)
                ns = S_OUTPUT;
            else
                ns = S_UPACK;
        end
        S_OUTPUT     : begin
            if(translate_ack_ready_i)                       // ack output to BUS_HANDLER finish
                if(fault_detect)
                    ns = S_FAULT_RPT;
                else
                    ns = S_IDLE;
            else
                ns = S_OUTPUT;
        end
        S_FAULT_RPT : begin
            if(fault_rpt_ready_i | fault_dtf)
                ns = S_IDLE;
            else
                ns = S_FAULT_RPT;
        end
        S_MRIF      : begin                                 // req credit and wait for ack
            if(mrif_credit_ack_i)
                ns = S_OUTPUT;
            else
                ns = S_MRIF;
        end
        default:    ns = S_IDLE;
        endcase
    end
//}}}

//=== FSM stage3 === {{{
//=== valid_o ===
    assign valid_o = (cs != S_IDLE);
    assign ptw_ongoing_o = (cs == S_TRANSLATE) | (cs == S_UPDATE);
//=== info store {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            tidx            <= 'd0;
            priv            <= 1'b0;
            ext             <= 1'b0;
            wr              <= 1'b0;
            is_translated   <= 'd0;
            process_id_valid<= 'd0;
            process_id      <= 'd0;
            device_id       <= 'd0;
            va              <= 'd0;
        end
        else if((ns != S_IDLE) & (cs == S_IDLE)) begin
            tidx            <= translate_req_i.idx              ;
            priv            <= is_dbg_entry ? translate_req_i.priv  : (translate_req_i.priv & translate_req_i.process_id_valid);     // only when process_id_valid==1 and priv==1 , the final priv is 1(privileged)
            ext             <= is_dbg_entry ? translate_req_i.ext   : (translate_req_i.ext & ~translate_req_i.wr & (~translate_req_i.is_translated | translate_req_i.process_id_valid));
            wr              <= is_dbg_entry ? ~translate_req_i.wr   : (translate_req_i.wr);
            is_translated   <= translate_req_i.is_translated    ;
            process_id_valid<= translate_req_i.process_id_valid ;
            process_id      <= translate_req_i.process_id       ;
            device_id       <= translate_req_i.device_id        ;
            va              <= translate_req_i.va               ;
        end
    end

    assign wr_o = wr;
//}}}

//=== depend_bits {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            depend_bits <= 'b0;
        else if((ns != S_IDLE) && (cs == S_IDLE))
            depend_bits <= depend_bits_i;
        else begin
            for(int unsigned kk=0; kk<TLB_QUEUE_DEPTH; kk++) begin
                if(depend_bits[kk] & (~valid_i[kk]))
                    depend_bits[kk] <= 1'b0;
            end
        end
    end
    //assign depend_status = depend_bits & valid_i;
    assign depend_status = depend_bits;

//=== depend_bit_o
    assign depend_bit_o = translate_req_valid_i & valid_o & (
                          (is_translated   == translate_req_i.is_translated   ) &
                          (process_id_valid== translate_req_i.process_id_valid) &
                          (process_id      == translate_req_i.process_id      ) &
                          (device_id       == translate_req_i.device_id       ) &
                          (va              == translate_req_i.va              ));
//}}}

//=== lookup {{{
    //assign hit = lookup_ack_i.hit;
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            hit_ff <= 1'b0;
        else begin 
            if((ns == S_IDLE) && (cs != S_IDLE))
                hit_ff <= 1'b0;
            else if(lookup_ack_valid_i)
                hit_ff <= lookup_ack_i.hit;
        end
    end
    assign hit = lookup_ack_valid_i ? lookup_ack_i.hit : hit_ff;
    //assign lookup_req_content = (cs==S_IDLE) ?  {
    //                                                idx,
    //                                                translate_req_i.is_translated,
    //                                                translate_req_i.process_id_valid,
    //                                                translate_req_i.process_id,
    //                                                translate_req_i.device_id
    //                                                translate_req_i.va
    //                                            } :
    //                                            {
    //                                                idx
    //                                                is_translated,
    //                                                process_id_valid,
    //                                                process_id,
    //                                                device_id
    //                                                va
    //                                            };
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            lookup_req_valid_o  <= 1'b0;
            //lookup_req_o        <= 'd0;
        end
        else begin
            if(lookup_req_valid_o & ~lookup_req_ready_i)
                lookup_req_valid_o <= 1'b1;
            else if(lookup_req_valid_o & lookup_req_ready_i)
                lookup_req_valid_o <= 1'b0;
            else if(
                  ((ns == S_LOOKUP) & (cs != S_LOOKUP)  )
                | ((cs == S_RELOOK) & relook_hint_i     )
                ) begin
                lookup_req_valid_o  <= 1'b1;
                //lookup_req_o        <= lookup_req_content;
            end
        end
    end

    assign lookup_req_o = (cs==S_IDLE) ?  {
                                            idx,
                                            translate_req_i.is_translated,
                                            translate_req_i.process_id_valid,
                                            translate_req_i.process_id,
                                            translate_req_i.device_id,
                                            translate_req_i.va
                                            } :
                                            {
                                            idx,
                                            is_translated,
                                            process_id_valid,
                                            process_id,
                                            device_id,
                                            va
                                            };

//}}}

//=== translate {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ptw_req_valid_o <= 1'b0;
            //ptw_req_o       <= 'd0;
        end
        else begin
            if(ptw_req_valid_o & ~ptw_req_ready_i)
                    ptw_req_valid_o <= 1'b1;
            else if(ptw_req_valid_o & ptw_req_ready_i)
                    ptw_req_valid_o <= 1'b0;
            else if((ns == S_TRANSLATE) & (cs != S_TRANSLATE))
                ptw_req_valid_o <= 1'b1;

            //if((ns == S_TRANSLATE) & (ns != S_TRANSLATE)) begin
            //    ptw_req_o.idx               <= idx              ;
            //    ptw_req_o.priv              <= priv             ;
            //    ptw_req_o.ext               <= ext              ;
            //    ptw_req_o.wr                <= wr               ;
            //    ptw_req_o.is_translated     <= is_translated    ;
            //    ptw_req_o.process_id_valid  <= process_id_valid ;
            //    ptw_req_o.process_id        <= process_id       ;
            //    ptw_req_o.device_id         <= device_id        ;
            //    ptw_req_o.va                <= va               ;

            //end
        end
    end

    assign ptw_req_o.idx               = idx             ;
    assign ptw_req_o.priv              = priv            ;
    assign ptw_req_o.ext               = ext             ;
    assign ptw_req_o.wr                = wr              ;
    assign ptw_req_o.is_translated     = is_translated   ;
    assign ptw_req_o.process_id_valid  = process_id_valid;
    assign ptw_req_o.process_id        = process_id      ;
    assign ptw_req_o.device_id         = device_id       ;
    assign ptw_req_o.va                = va              ;

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ptw_ack_valid_ff    <= 1'b0;
            ptw_ack_ff          <= 'd0;
        end
        else begin
            if((ns == S_IDLE) & ( cs != S_IDLE)) begin
                ptw_ack_valid_ff    <= 1'b0;
                ptw_ack_ff          <= 'd0;
            end
            else if(ptw_ack_valid_i) begin
                ptw_ack_valid_ff    <= 1'b1;
                ptw_ack_ff          <= ptw_ack_i;
            end
        end
    end
    //assign translate_fail = ptw_ack_valid_ff ? (ptw_ack_ff.opcode == 'b10) : (ptw_ack_i.opcode == 'b10);
    assign translate_fail       = ptw_ack_valid_i ? (ptw_ack_i.opcode[1:0] == 'b10) : (ptw_ack_ff.opcode[1:0] == 'b10);
    assign translate_deny       = ptw_ack_valid_i ? (ptw_ack_i.opcode[1:0] == 'b01) : (ptw_ack_ff.opcode[1:0] == 'b01);
    assign translate_no_cache   = ptw_ack_valid_i ? ptw_ack_i.opcode[2] : ptw_ack_ff.opcode[2];
    assign ptw_ack_valid        = ptw_ack_valid_i;
    assign ptw_ack_mrif         = ptw_ack_valid_i ? ptw_ack_i.MRIF : ptw_ack_ff.MRIF;
//}}}

//=== update {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            update_req_valid_o  <= 1'b0;
            update_req_o        <= 'd0;
        end
        else begin
            if(update_req_valid_o & ~update_req_ready_i)
                update_req_valid_o  <= 1'b1;
            else if(update_req_valid_o & update_req_ready_i)
                update_req_valid_o  <= 1'b0;
            else if((ns == S_UPDATE) & (cs != S_UPDATE)) begin
                update_req_valid_o  <= 1'b1;
                update_req_o.idx                <= idx              ;
                update_req_o.is_translated      <= is_translated    ;
                update_req_o.process_id_valid   <= process_id_valid ;
                update_req_o.PBMT               <= (cs==S_TRANSLATE) ? ptw_ack_i.PBMT       : ptw_ack_ff.PBMT       ;
                update_req_o.GPPN               <= (cs==S_TRANSLATE) ? ptw_ack_i.GPPN       : ptw_ack_ff.GPPN       ;
                update_req_o.ENATS              <= (cs==S_TRANSLATE) ? ptw_ack_i.ENATS      : ptw_ack_ff.ENATS      ;
                update_req_o.T2GPA              <= (cs==S_TRANSLATE) ? ptw_ack_i.T2GPA      : ptw_ack_ff.T2GPA      ;
                update_req_o.DTF                <= (cs==S_TRANSLATE) ? ptw_ack_i.DTF        : ptw_ack_ff.DTF        ;
                update_req_o.PDTV               <= (cs==S_TRANSLATE) ? ptw_ack_i.PDTV       : ptw_ack_ff.PDTV       ;
                update_req_o.DPE                <= (cs==S_TRANSLATE) ? ptw_ack_i.DPE        : ptw_ack_ff.DPE        ;
                update_req_o.SXL                <= (cs==S_TRANSLATE) ? ptw_ack_i.SXL        : ptw_ack_ff.SXL        ;
                update_req_o.ENS                <= (cs==S_TRANSLATE) ? ptw_ack_i.ENS        : ptw_ack_ff.ENS        ;
                update_req_o.SUM                <= (cs==S_TRANSLATE) ? ptw_ack_i.SUM        : ptw_ack_ff.SUM        ;
                update_req_o.S1_D               <= (cs==S_TRANSLATE) ? ptw_ack_i.S1_D       : ptw_ack_ff.S1_D       ;
                update_req_o.S2_D               <= (cs==S_TRANSLATE) ? ptw_ack_i.S2_D       : ptw_ack_ff.S2_D       ;
                update_req_o.SADE               <= (cs==S_TRANSLATE) ? ptw_ack_i.SADE       : ptw_ack_ff.SADE       ;
                update_req_o.GADE               <= (cs==S_TRANSLATE) ? ptw_ack_i.GADE       : ptw_ack_ff.GADE       ;
                update_req_o.N                  <= (cs==S_TRANSLATE) ? ptw_ack_i.N          : ptw_ack_ff.N          ;
//                update_req_o.S1_PERM_D          <= (cs==S_TRANSLATE) ? ptw_ack_i.S1_PERM_D  : ptw_ack_ff.S1_PERM_D;
//                update_req_o.S1_PERM_A          <= (cs==S_TRANSLATE) ? ptw_ack_i.S1_PERM_A  : ptw_ack_ff.S1_PERM_A;
                update_req_o.S1_PERM            <= (cs==S_TRANSLATE) ? ptw_ack_i.S1_PERM    : ptw_ack_ff.S1_PERM    ;
//                update_req_o.S2_PERM_D          <= (cs==S_TRANSLATE) ? ptw_ack_i.S2_PERM_D  : ptw_ack_ff.S2_PERM_D;
//                update_req_o.S2_PERM_A          <= (cs==S_TRANSLATE) ? ptw_ack_i.S2_PERM_A  : ptw_ack_ff.S2_PERM_A;
                update_req_o.S2_PERM            <= (cs==S_TRANSLATE) ? ptw_ack_i.S2_PERM    : ptw_ack_ff.S2_PERM    ;
                update_req_o.S2SIZE             <= (cs==S_TRANSLATE) ? ptw_ack_i.S2SIZE     : ptw_ack_ff.S2SIZE     ;
                update_req_o.S1SIZE             <= (cs==S_TRANSLATE) ? ptw_ack_i.S1SIZE     : ptw_ack_ff.S1SIZE     ;
                update_req_o.S2MODE             <= (cs==S_TRANSLATE) ? ptw_ack_i.S2MODE     : ptw_ack_ff.S2MODE     ;
                update_req_o.S1MODE             <= (cs==S_TRANSLATE) ? ptw_ack_i.S1MODE     : ptw_ack_ff.S1MODE     ;
                update_req_o.PDTMODE            <= (cs==S_TRANSLATE) ? ptw_ack_i.PDTMODE    : ptw_ack_ff.PDTMODE    ;
                update_req_o.PSCID              <= (cs==S_TRANSLATE) ? ptw_ack_i.PSCID      : ptw_ack_ff.PSCID      ;
                update_req_o.GSCID              <= (cs==S_TRANSLATE) ? ptw_ack_i.GSCID      : ptw_ack_ff.GSCID      ;
                update_req_o.PPN                <= (cs==S_TRANSLATE) ? ptw_ack_i.PPN        : ptw_ack_ff.PPN        ;
                update_req_o.process_id         <= process_id      ;
                update_req_o.device_id          <= device_id       ;
                update_req_o.va                 <= va              ;
            end
        end
    end
//}}}

//=== fault rpt {{{
//    assign process_id_width_err =     ((lookup_ack_i.PDTMODE == iommu_acd_pkg::PDTMODE_PD8)  & (pdi2 != 'd0) & (pdi1 != 'd0)   )
//                                    | ((lookup_ack_i.PDTMODE == iommu_acd_pkg::PDTMODE_PD17) & (pdi2 != 'd0)                   );

//    assign lookup_ack_i_S1_A       = lookup_ack_i.N ? lookup_ack_i.S1_PERM_A[va[15:12]] : lookup_ack_i.S1_PERM_A[0];
//    assign lookup_ack_i_S1_D       = lookup_ack_i.N ? lookup_ack_i.S1_PERM_D[va[15:12]] : lookup_ack_i.S1_PERM_D[0];
    assign lookup_ack_i_S1_D        = lookup_ack_i.S1_D;
//    assign lookup_ack_i_S2_A       = lookup_ack_i.N ? lookup_ack_i.S2_PERM_A[va[15:12]] : lookup_ack_i.S2_PERM_A[0];
//    assign lookup_ack_i_S2_D       = lookup_ack_i.N ? lookup_ack_i.S2_PERM_D[va[15:12]] : lookup_ack_i.S2_PERM_D[0];
    assign lookup_ack_i_S2_D        = lookup_ack_i.S2_D;
    
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            fault_detect<= 1'b0;
            fault_cause <= 12'd0;
            fault_dtf   <= 1'b0;
        end
        else begin
            if((ns != S_FAULT_RPT) & (cs == S_FAULT_RPT)) begin
                fault_detect    <= 1'b0;
                fault_cause     <= 'd0;
                fault_dtf       <= 1'b0;
            end
            else if((cs == S_IDLE) & (ns == S_OUTPUT))begin
                // if ddtp.iommu_mode==Off, then stop and report "All inbound transaction disallowed" (cause=256)
                if(csr_ddtp_iommu_mode_i == iommu_acd_pkg::IOMMU_MODE_OFF) begin
                    fault_detect<= 1'b1;
                    fault_cause <= iommu_acd_pkg::FCODE_ALL_INBOUND_TRANSACTION_DISALLOWED;
                    fault_dtf   <= 1'b0;
                end
                // if ddtp.iommu_mode==Bare and any of the following conditions hold, then stop and report "Transaction type disallowed" (cause=260)
                // a. Transaction type is a Translated request
                // b. If the device_id is wider than supported by the IOMMU mode
                else if(
                          (mode_bare & translate_req_i.is_translated        )                               // Transaction type disallowed, cause=260
                        | (mode_2lvl & (ddi2_i != 'd0)                      )                               // 260
                        | (mode_1lvl & ((ddi2_i != 'd0) | (ddi1_i != 'd0))  )                               // 260
                        ) begin
                    fault_detect<= 1'b1;
                    fault_cause <= iommu_acd_pkg::FCODE_TRANSACTION_TYPE_DISALLOWED;
                    fault_dtf   <= 1'b0;
                end
                else begin
                    fault_detect<= 1'b0;
                    fault_cause <= 'd0;
                    fault_dtf   <= 1'b0;
                end
            end
            else if(((cs == S_LOOKUP) | (cs == S_RELOOK)) & (ns == S_OUTPUT)) begin
                // if any of the following conditions hold then stop report "Transaction type disallowed" (cause=260)
                // a. Transaction type is a Translated request and DC.tc.ENATS is 0
                // b. Transaction has a valid process_id and DC.tc.PDTV is 0
                // c. Transaction hsa a valid process_id and DC.tc.PDTV is 1 and process_id is wider than that supported by pdtp.MODE
                if(                                                                                         // transaction type check
                      ((~lookup_ack_i.ENATS) & is_translated                            )                   //
                    | ((~lookup_ack_i.PDTV)  & process_id_valid                         )
// remove process_id width check in ACD, 20241125
// original process_id is packed in PTW_REQ to ATD
// if DC.tc.PDTV==1 and process_id width invalid with DC.fsc.pdtp.MODE. PTW_ACK from ATD should be opcode fail
// so no process_id width err translation catched in TLB and invalid process_id can not hit in TLB
//                    | (  lookup_ack_i.PDTV   & process_id_valid & process_id_width_err  )
                  ) begin
                    fault_detect<= 1'b1;
                    fault_cause <= iommu_acd_pkg::FCODE_TRANSACTION_TYPE_DISALLOWED;
                    fault_dtf   <= lookup_ack_i.DTF;
                end
                // when PC is located, ENS check processing
                // if any of the following conditions hold then stop and report "Transaction type disallowed" (cause=260)
                // a. The transaction requests supervisor privilege but PC.ta.ENS is not set
                else if(~work_ens & priv) begin
                        fault_detect<= 1'b1;
                        fault_cause <= iommu_acd_pkg::FCODE_TRANSACTION_TYPE_DISALLOWED;
                        fault_dtf   <= lookup_ack_i.DTF;
                end

// remove iova/gpa width and upper bits check, 20241126
// original iova/gpa is packed in PTW_REQ to ATD
// if iova/gpa width and upper bits error, PTW_ACK from ATD should be opcode fail
// so no iova/gpa width/upper-bits err translation cached in TLB and invalid iova/gpa can not hit in TLB
//                // do IOVA width check when input address is IOVA when iosatp.MODE is not Bare
//                // a. iosatp.MODE is not Bare (the transaction type can not be Translated request), and SXL==1, now is Sv32, upper bits should all be 0
//                else if(lookup_ack_i.SXL & (                    ~is_translated & lookup_ack_i.S1MODE!='d0 & va[63:32]!='d0)) begin // SXL=1, S1 not Bare, and the input is untranslated VA, bit[63:32] not 0
//                    fault_detect <= 1'b1;
//                    fault_cause <=  (ext) ? iommu_acd_pkg::FCODE_INSTRUCTION_PAGE_FAULT :
//                                    (~wr) ? iommu_acd_pkg::FCODE_READ_PAGE_FAULT        :
//                                            iommu_acd_pkg::FCODE_WRITE_AMO_PAGE_FAULT   ;
//                end
//                // b. iosatp.MODE is not Bare (the transaction type can not be Translated request), and SXL==0, now is Sv39,Sv48 or Sv57, upper bits should be same with the highest valid bit
//                else if(~lookup_ack_i.SXL & (                   ~is_translated & lookup_ack_i.S1MODE!='d0 &
//                                                                (
//                                                                  (lookup_ack_i.S1MODE=='d8  & va[63:39]!={25{va[38]}}) |   // sv39
//                                                                  (lookup_ack_i.S1MODE=='d9  & va[63:48]!={16{va[47]}}) |   // sv48
//                                                                  (lookup_ack_i.S1MODE=='d10 & va[63:57]!={ 7{va[56]}})     // sv57
//                                                                )
//                                            )
//                        ) begin
//                    fault_detect<= 1'b1;
//                    fault_cause <= (ext) ? iommu_acd_pkg::FCODE_INSTRUCTION_PAGE_FAULT :
//                                   (~wr) ? iommu_acd_pkg::FCODE_READ_PAGE_FAULT        :
//                                           iommu_acd_pkg::FCODE_WRITE_AMO_PAGE_FAULT   ;
//                end
//                // do GPA width check when input address is GPA and iohgatp.MODE is not Bare (the transaction type must be Translated request with DC.tc.T2GPA==1
//                // a. iohgatp.MODE is not Bare and SXL==1, S2 must be Sv32x4, upper bits (>=34) must all be 0
//                else if(lookup_ack_i.SXL & (lookup_ack_i.T2GPA & is_translated & lookup_ack_i.S2MODE!='d0 & va[63:34]!='d0)) begin // SXL=1, S2 not Bare, and the input is translated GPA, bit[62:34] not 0
//                    fault_detect <= 1'b1;
//                    fault_cause <=  (ext) ? iommu_acd_pkg::FCODE_GUEST_INSTRUCTION_PAGE_FAULT :
//                                    (~wr) ? iommu_acd_pkg::FCODE_GUEST_READ_PAGE_FAULT        :
//                                            iommu_acd_pkg::FCODE_GUEST_WRITE_AMO_PAGE_FAULT   ;
//                end
//                // b. iohgatp.MODE is not Bare and SXL==0, S2 may be Sv39x4,Sv48x4 or Sv57x4, upper bits must all be 0
//                else if(~lookup_ack_i.SXL & (lookup_ack_i.T2GPA & is_translated & lookup_ack_i.S2MODE!='d0 &
//                                                                (
//                                                                  (lookup_ack_i.S2MODE=='d8  & va[63:39]!='d0) |   // sv39x4
//                                                                  (lookup_ack_i.S2MODE=='d9  & va[63:48]!='d0) |   // sv48x4
//                                                                  (lookup_ack_i.S2MODE=='d10 & va[63:57]!='d0)     // sv57x4
//                                                                )
//                                            )
//                        ) begin
//                    fault_detect <= 1'b1;
//                    fault_cause <=  (ext) ? iommu_acd_pkg::FCODE_GUEST_INSTRUCTION_PAGE_FAULT :
//                                    (~wr) ? iommu_acd_pkg::FCODE_GUEST_READ_PAGE_FAULT        :
//                                            iommu_acd_pkg::FCODE_GUEST_WRITE_AMO_PAGE_FAULT   ;
//                end

// permission check should pass when corresponding stage is BARE, 20241115
//                else if(
//                          (ext & (~lookup_ack_i.S1_PERM[2] | ~lookup_ack_i.S2_PERM[2])  )                   // X check
//                        | (wr  & (~lookup_ack_i.S1_PERM[1] | ~lookup_ack_i.S2_PERM[1])  )                   // W check
//                        | (~wr & (~lookup_ack_i.S1_PERM[0] | ~lookup_ack_i.S2_PERM[0])  )                   // R check
//                      ) begin

                // do S1 permission check when iosatp.MODE is not Bare
                // do S2 permission check when iohgatp.MODE is not Bare
                else if(
                        (lookup_ack_i.S1MODE!=0 & (
                                                      (ext & ~lookup_ack_i.S1_PERM[2])                      // X check
                                                    | (wr  & ~lookup_ack_i.S1_PERM[1])                      // W check
                                                    | (~wr & ~lookup_ack_i.S1_PERM[0])                      // R check
                                                    | (is_dbg_entry & ~lookup_ack_i.S1_PERM[0])             // when dbg, always require R permission
                                                    )) |                                                    // S1 not bare but permission check fail
                        (lookup_ack_i.S2MODE!=0 & (
                                                      (ext & ~lookup_ack_i.S2_PERM[2])                      // X check
                                                    | (wr  & ~lookup_ack_i.S2_PERM[1])                      // W check
                                                    | (~wr & ~lookup_ack_i.S2_PERM[0])                      // R check
                                                    | (is_dbg_entry & ~lookup_ack_i.S2_PERM[0])             // when dbg, always require R permission
                                                    ))                                                      // S2 not bare but permission check fail
                        ) begin
                        fault_detect<=  1'b1;
                        fault_cause <=  (ext & (~lookup_ack_i.S1_PERM[2])) ? iommu_acd_pkg::FCODE_INSTRUCTION_PAGE_FAULT       :
                                        (~wr & (~lookup_ack_i.S1_PERM[0])) ? iommu_acd_pkg::FCODE_READ_PAGE_FAULT              :
                                        ( wr & (~lookup_ack_i.S1_PERM[1])) ? iommu_acd_pkg::FCODE_WRITE_AMO_PAGE_FAULT         :
                                        (ext & (~lookup_ack_i.S2_PERM[2])) ? iommu_acd_pkg::FCODE_GUEST_INSTRUCTION_PAGE_FAULT :
                                        (~wr & (~lookup_ack_i.S2_PERM[0])) ? iommu_acd_pkg::FCODE_GUEST_READ_PAGE_FAULT        :
                                        ( wr & (~lookup_ack_i.S2_PERM[1])) ? iommu_acd_pkg::FCODE_GUEST_WRITE_AMO_PAGE_FAULT   : 'd0;
                        fault_dtf   <= lookup_ack_i.DTF;
                end
                // when iosatp.MODE is not Bare, do priv and U check
                // a. non-priviledge request with U==0
                // b. When ENS is 1, supervisor privilege transaction that read with execute intent to pages mapped with U bit in PTE set to 1 will fault, regardless of the state of SUM
                // c. When ENS is 1, the SUM bit modifies the privilege with whitch supervisor privilege transactions access virtual meomory. when SUM is 0, supervisor priviledge transactions to pages mapped with U==1 will fault
                // ATTENTION: SUM is never used for S2_LEAF_PTE.U check. "when checking the U bit (S2_LEAF_PTE's U), the current privilege mode is always taken to be U-mode, impilies that U must be 1 to be legal"
                else if(lookup_ack_i.S1MODE!='d0 & 
                            (                                                                                    // U check
                               (~priv                    & ~lookup_ack_i.S1_PERM[3])                             // U_MODE with U==0
                             | ( priv &  ext             &  lookup_ack_i.S1_PERM[3])                             // S_MODE with EXCUTE but U==1
                             | ( priv & ~ext & ~work_sum &  lookup_ack_i.S1_PERM[3])                             // s_MODE without EXCUTE but SUM==0 and U==1
                            )
                        )begin
                    fault_detect<= 1'b1;
                    fault_cause <= ext ? iommu_acd_pkg::FCODE_INSTRUCTION_PAGE_FAULT   :
                                   ~wr ? iommu_acd_pkg::FCODE_READ_PAGE_FAULT          :
                                         iommu_acd_pkg::FCODE_WRITE_AMO_PAGE_FAULT     ;
                    fault_dtf   <= lookup_ack_i.DTF;
                end
                // A/D(SADE) check, no SADE support by now
                else if(lookup_ack_i.S1MODE!='d0 &
                            (
//                              ~lookup_ack_i_S1_A |
                              (~lookup_ack_i_S1_D & wr & ~lookup_ack_i.SADE)
                            )
                       ) begin
                    fault_detect<= 1'b1;
                    fault_cause <= ext ? iommu_acd_pkg::FCODE_INSTRUCTION_PAGE_FAULT   :
                                   ~wr ? iommu_acd_pkg::FCODE_READ_PAGE_FAULT          :
                                         iommu_acd_pkg::FCODE_WRITE_AMO_PAGE_FAULT     ;
                    fault_dtf   <= lookup_ack_i.DTF;
                end
                // A/D(GADE) check, no GADE support by now
                else if(lookup_ack_i.S2MODE!='d0 &
                            (
//                              ~lookup_ack_i_S2_A |
                              (~lookup_ack_i_S2_D & wr & ~lookup_ack_i.GADE)
                            )
                       ) begin
                    fault_detect<= 1'b1;
                    fault_cause <= ext ? iommu_acd_pkg::FCODE_GUEST_INSTRUCTION_PAGE_FAULT   :
                                   ~wr ? iommu_acd_pkg::FCODE_GUEST_READ_PAGE_FAULT          :
                                         iommu_acd_pkg::FCODE_GUEST_WRITE_AMO_PAGE_FAULT     ;
                    fault_dtf   <= lookup_ack_i.DTF;
                end
                else begin
                    fault_detect<= 1'b0;
                    fault_cause <= 'd0;
                    fault_dtf   <= 1'b0;
                end
            end
            else if((cs == S_TRANSLATE) & (ns == S_OUTPUT)) begin   // this only happens with translate_fail, no fault rpt
                fault_detect<= 1'b0;
                fault_cause <= 'd0;
                fault_dtf   <= 1'b0;
            end
            else if((cs == S_UPDATE) & (ns == S_OUTPUT)) begin      // this only happens with translate success or translate permission fail, 
                fault_detect<= 1'b0;                                // if translate permission fail, the fault has already reported at ATD, no need report here
                fault_cause <= 'd0;
                fault_dtf   <= 1'b0;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            fault_rpt_valid_o   <= 1'b0;
            //fault_rpt_o         <= 1'b0;
        end
        else begin
            if(fault_rpt_valid_o & ~fault_rpt_ready_i) begin
                fault_rpt_valid_o <= 1'b1;
            end
            else if(fault_rpt_valid_o & fault_rpt_ready_i) begin
                fault_rpt_valid_o <= 1'b0;
            end 
            else if((ns == S_FAULT_RPT) & (cs == S_OUTPUT) & ~fault_dtf) begin
                fault_rpt_valid_o <= 1'b1;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            lookup_ack_gppn_ff <= 'd0;
        else if(lookup_ack_valid_i)
            lookup_ack_gppn_ff <= {2'b0, lookup_ack_i.GPPN};
    end
    
    assign fault_rpt_o.ioval2   = {2'b0, lookup_ack_gppn_ff[61:12], 12'b0};//lookup_ack_i.GPPN;   // bit0 always 0 as no implicit memory access in ACD
                                                                                            // bit1 always 0 as no hardward updating A/D and implicit memory write
                                                                                            // bit12~2 always 0, as always same with IOVA, no need rpt to FQ
    assign fault_rpt_o.ioval    = {va[63:12], 12'b0};                                       // bit12~0 always 0, as always same with IOVA, no need rpt to FQ
    assign fault_rpt_o.cause    = fault_cause;
    assign fault_rpt_o.did      = device_id;
    assign fault_rpt_o.pid      = process_id;
    assign fault_rpt_o.pv       = process_id_valid;
    assign fault_rpt_o.priv     = priv;
    assign fault_rpt_o.ttyp[5:3]= 'd0;
    assign fault_rpt_o.ttyp[2]  = is_translated;
    assign fault_rpt_o.ttyp[1:0]= ext ? 2'b01 :
                                  ~wr ? 2'b10 :
                                   wr ? 2'b11 : 2'b00;
//}}}

//=== OUTPUT {{{
    //always@(posedge clk or negedge rstn) begin
    //    if(~rstn) begin
    //        translate_ack_valid_o   <= 1'b0;
    //        translate_ack_o         <= 'd0;
    //    end
    //    else begin
    //        if(translate_ack_valid_o & ~translate_ack_ready_i)
    //            translate_ack_valid_o <= 1'b1;
    //        else if(translate_ack_valid_o & translate_ack_ready_i)
    //            translate_ack_valid_o <= 1'b0;
    //        else if((ns == S_OUTPUT) & (cs == S_IDLE)) begin
    //            translate_ack_valid_o <= 1'b1;
    //            if(mode_off)
    //                translate_ack_o.resp <= 2'b11;
    //            else if(mode_bare) begin
    //                if(translate_req_i.is_translated)
    //                    translate_ack_o.resp <= 2'b11;
    //                else
    //                    translate_ack_o.resp <= 2'b00;
    //            else if(mode_2lvl & (ddi2_i != 'd0))
    //                translate_ack_o.resp <= 2'b11;
    //            else if(mode_1lvl & ((ddi2_i != 'd0) | (ddi1_i != 'd0)))
    //                translate_ack_o.resp <= 2'b11;
    //        end
    //        else if()
    //    end
    //end
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            translate_ack_valid_o <= 1'b0;
        else begin
            if(translate_ack_valid_o & ~translate_ack_ready_i)
                translate_ack_valid_o <= 1'b1;
            else if(translate_ack_valid_o & translate_ack_ready_i)
                translate_ack_valid_o <= 1'b0;
            else if((ns == S_OUTPUT) & (cs != S_OUTPUT))
                translate_ack_valid_o <= 1'b1;
        end
    end
    
    assign translate_ack_o.idx  = tidx;
    assign translate_ack_o.resp = fault_detect  ? 2'b11 :                           // transaction disallowed or lookup hit but permission check fail
                                  (translate_deny | translate_fail) ? 2'b11 :       // translate fail or translate permission deny
                                  2'b00;
    assign translate_ack_o.pa   = mode_bare     ? va    : work_ppn;
    assign translate_ack_o.trange= mode_bare     ? 3'b111: work_range;
    assign translate_ack_o.pbmt = mode_bare     ? 2'b00 : work_pbmt;
    assign translate_ack_o.mrif = mode_bare     ? 1'b0  : work_mrif;
    assign translate_ack_o.nid  = mode_bare     ? 'd0   : work_nid;
    assign translate_ack_o.nppn = mode_bare     ? 'd0   : work_nppn;

//==========
    assign lookup_ack_i_s1_bare     = lookup_ack_i.S1MODE=='d0;
    assign lookup_ack_i_s1_sv32     = lookup_ack_i.SXL ? lookup_ack_i.S1MODE=='d8 : 1'b0;
    assign lookup_ack_i_s1_sv39     = lookup_ack_i.SXL ? 1'b0                     : lookup_ack_i.S1MODE=='d8;
    assign lookup_ack_i_s1_sv48     = lookup_ack_i.SXL ? 1'b0                     : lookup_ack_i.S1MODE=='d9;
    assign lookup_ack_i_s1_sv57     = lookup_ack_i.SXL ? 1'b0                     : lookup_ack_i.S1MODE=='d10;
    assign lookup_ack_i_s2_bare     = lookup_ack_i.S2MODE=='d0;
    assign lookup_ack_i_s2_sv32x4   = csr_fctl_gxl_i   ? lookup_ack_i.S2MODE=='d8 : 1'b0;
    assign lookup_ack_i_s2_sv39x4   = csr_fctl_gxl_i   ? 1'b0                     : lookup_ack_i.S2MODE=='d8;
    assign lookup_ack_i_s2_sv48x4   = csr_fctl_gxl_i   ? 1'b0                     : lookup_ack_i.S2MODE=='d9;
    assign lookup_ack_i_s2_sv57x4   = csr_fctl_gxl_i   ? 1'b0                     : lookup_ack_i.S2MODE=='d10;
    assign lookup_ack_i_s1_2m       = ~lookup_ack_i_s1_sv32   & (lookup_ack_i.S1SIZE == 'b01);
    assign lookup_ack_i_s2_2m       = ~lookup_ack_i_s2_sv32x4 & (lookup_ack_i.S2SIZE == 'b01);
    assign lookup_ack_i_s1_4m       =  lookup_ack_i_s1_sv32   & (lookup_ack_i.S1SIZE == 'b01);
    assign lookup_ack_i_s2_4m       =  lookup_ack_i_s2_sv32x4 & (lookup_ack_i.S2SIZE == 'b01);
    assign lookup_ack_i_is_2m       = (~lookup_ack_i_s1_bare & ~lookup_ack_i_s2_bare) ? ((lookup_ack_i_s2_2m & (lookup_ack_i_s1_2m | lookup_ack_i_s1_1g | lookup_ack_i_s1_512g)) | (lookup_ack_i_s1_2m & (lookup_ack_i_s2_2m | lookup_ack_i_s2_1g | lookup_ack_i_s2_512g))) :
                                                                                        ((lookup_ack_i_s2_2m & ~lookup_ack_i_s2_bare) | (lookup_ack_i_s1_2m & ~lookup_ack_i_s1_bare));
    assign lookup_ack_i_is_4m       = (~lookup_ack_i_s1_bare & ~lookup_ack_i_s2_bare) ? ((lookup_ack_i_s2_4m & (lookup_ack_i_s1_4m)) | (lookup_ack_i_s1_4m & (lookup_ack_i_s2_4m | lookup_ack_i_s2_1g |lookup_ack_i_s2_512g))) :
                                                                                        ((lookup_ack_i_s2_4m & ~lookup_ack_i_s2_bare) | (lookup_ack_i_s1_4m & ~lookup_ack_i_s1_bare));
    assign lookup_ack_i_s1_1g       = lookup_ack_i.S1SIZE == 'b10;
    assign lookup_ack_i_s2_1g       = lookup_ack_i.S2SIZE == 'b10;
    assign lookup_ack_i_is_1g       = (~lookup_ack_i_s1_bare & ~lookup_ack_i_s2_bare) ? ((lookup_ack_i_s2_1g & (lookup_ack_i_s1_1g | lookup_ack_i_s1_512g)) | (lookup_ack_i_s1_1g & (lookup_ack_i_s2_1g | lookup_ack_i_s2_512g))) :
                                                                                        ((lookup_ack_i_s2_1g & ~lookup_ack_i_s2_bare) | (lookup_ack_i_s1_1g & ~lookup_ack_i_s1_bare));
    assign lookup_ack_i_s1_512g     = lookup_ack_i.S1SIZE == 'b11;
    assign lookup_ack_i_s2_512g     = lookup_ack_i.S2SIZE == 'b11;
    assign lookup_ack_i_is_512g     = (~lookup_ack_i_s1_bare & ~lookup_ack_i_s2_bare) ? (lookup_ack_i_s2_512g & lookup_ack_i_s1_512g) :
                                                                                        ((lookup_ack_i_s2_512g & ~lookup_ack_i_s2_bare) | (lookup_ack_i_s1_512g & ~lookup_ack_i_s1_bare));
// why lookup_ack_i_is_2m: s2_not_bare cause 2m->4m? 20251127 gpf
//    assign lookup_ack_i_range       = lookup_ack_i_is_2m    ? {
//                                                                ~lookup_ack_i_s2_bare ? 3'b001 : 3'b101
//                                                               } :
//                                      lookup_ack_i_is_1g    ? {
//                                                                ~lookup_ack_i_s2_bare ? 3'b010 : 3'b110
//                                                               } :
//                                      lookup_ack_i_is_512g  ? 3'b011 : 3'b000;

    assign lookup_ack_i_range       = lookup_ack_i_is_2m    ? 3'b001 :
                                      lookup_ack_i_is_4m    ? 3'b101 :
                                      lookup_ack_i_is_1g    ? 3'b010 :
                                      lookup_ack_i_is_512g  ? 3'b011 : 3'b000;

    assign ptw_ack_ff_s1_bare       = ptw_ack_ff.S1MODE=='d0;
    assign ptw_ack_ff_s1_sv32       = ptw_ack_ff.SXL ? ptw_ack_ff.S1MODE=='d8 : 1'b0;
    assign ptw_ack_ff_s1_sv39       = ptw_ack_ff.SXL ? 1'b0                     : ptw_ack_ff.S1MODE=='d8;
    assign ptw_ack_ff_s1_sv48       = ptw_ack_ff.SXL ? 1'b0                     : ptw_ack_ff.S1MODE=='d9;
    assign ptw_ack_ff_s1_sv57       = ptw_ack_ff.SXL ? 1'b0                     : ptw_ack_ff.S1MODE=='d10;
    assign ptw_ack_ff_s2_bare       = ptw_ack_ff.S2MODE=='d0;
    assign ptw_ack_ff_s2_sv32x4     = csr_fctl_gxl_i   ? ptw_ack_ff.S2MODE=='d8 : 1'b0;
    assign ptw_ack_ff_s2_sv39x4     = csr_fctl_gxl_i   ? 1'b0                     : ptw_ack_ff.S2MODE=='d8;
    assign ptw_ack_ff_s2_sv48x4     = csr_fctl_gxl_i   ? 1'b0                     : ptw_ack_ff.S2MODE=='d9;
    assign ptw_ack_ff_s2_sv57x4     = csr_fctl_gxl_i   ? 1'b0                     : ptw_ack_ff.S2MODE=='d10;
    assign ptw_ack_ff_s1_2m         = ~ptw_ack_ff_s1_sv32 & (ptw_ack_ff.S1SIZE == 'b01);
    assign ptw_ack_ff_s2_2m         = ~ptw_ack_ff_s1_sv32 & (ptw_ack_ff.S2SIZE == 'b01);
    assign ptw_ack_ff_s1_4m         =  ptw_ack_ff_s1_sv32 & (ptw_ack_ff.S1SIZE == 'b01);
    assign ptw_ack_ff_s2_4m         =  ptw_ack_ff_s1_sv32 & (ptw_ack_ff.S2SIZE == 'b01);
    assign ptw_ack_ff_is_2m         = (~ptw_ack_ff_s1_bare & ~ptw_ack_ff_s2_bare) ? ((ptw_ack_ff_s2_2m & (ptw_ack_ff_s1_2m | ptw_ack_ff_s1_1g | ptw_ack_ff_s1_512g)) | (ptw_ack_ff_s1_2m & (ptw_ack_ff_s2_2m | ptw_ack_ff_s2_1g | ptw_ack_ff_s2_512g))) :
                                                                                    ((ptw_ack_ff_s2_2m & ~ptw_ack_ff_s2_bare) | (ptw_ack_ff_s1_2m & ~ptw_ack_ff_s1_bare));
    assign ptw_ack_ff_is_4m         = (~ptw_ack_ff_s1_bare & ~ptw_ack_ff_s2_bare) ? ((ptw_ack_ff_s2_4m & (ptw_ack_ff_s1_4m)) | (ptw_ack_ff_s1_4m & (ptw_ack_ff_s2_4m | ptw_ack_ff_s2_1g | ptw_ack_ff_s2_512g))) :
                                                                                    ((ptw_ack_ff_s2_4m & ~ptw_ack_ff_s2_bare) | (ptw_ack_ff_s1_4m & ~ptw_ack_ff_s1_bare));
    assign ptw_ack_ff_s1_1g         = ptw_ack_ff.S1SIZE == 'b10;
    assign ptw_ack_ff_s2_1g         = ptw_ack_ff.S2SIZE == 'b10;
    assign ptw_ack_ff_is_1g         = (~ptw_ack_ff_s1_bare & ~ptw_ack_ff_s2_bare) ? ((ptw_ack_ff_s2_1g & (ptw_ack_ff_s1_1g | ptw_ack_ff_s1_512g)) | (ptw_ack_ff_s1_1g & (ptw_ack_ff_s2_1g | ptw_ack_ff_s2_512g))) :
                                                                                    ((ptw_ack_ff_s2_1g & ~ptw_ack_ff_s2_bare) | (ptw_ack_ff_s1_1g & ~ptw_ack_ff_s1_bare));
    assign ptw_ack_ff_s1_512g       = ptw_ack_ff.S1SIZE == 'b11;
    assign ptw_ack_ff_s2_512g       = ptw_ack_ff.S2SIZE == 'b11;
    assign ptw_ack_ff_is_512g       = (~ptw_ack_ff_s1_bare & ~ptw_ack_ff_s2_bare) ? (ptw_ack_ff_s2_512g & ptw_ack_ff_s1_512g) :
                                                                                    ((ptw_ack_ff_s2_512g & ~ptw_ack_ff_s2_bare) | (ptw_ack_ff_s1_512g & ~ptw_ack_ff_s1_bare));

// why ptw_ack_ff_is_2m: s2_not_bare cause 2m->4m? 20251127 gpf
//    assign ptw_ack_ff_range         = ptw_ack_ff_is_2m      ? {
//                                                                ~ptw_ack_ff_s2_bare ? 3'b001 : 3'b101
//                                                               } :
//                                      ptw_ack_ff_is_1g      ? {
//                                                                ~ptw_ack_ff_s2_bare ? 3'b010 : 3'b110
//                                                               } :
//                                      ptw_ack_ff_is_512g    ? 3'b011 : 3'b000;
    assign ptw_ack_ff_range         = ptw_ack_ff_is_2m      ? 3'b001 :
                                      ptw_ack_ff_is_4m      ? 3'b101 :
                                      ptw_ack_ff_is_1g      ? 3'b010 :
                                      ptw_ack_ff_is_512g    ? 3'b011 : 3'b000;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            work_ppn <= 52'd0;
        else begin
            if((ns == S_OUTPUT) & ((cs == S_LOOKUP) | (cs == S_RELOOK))) begin
                if(is_translated & lookup_ack_i.ENATS & ~lookup_ack_i.T2GPA) begin  // Translated request with PA
                    work_ppn <= va;
                end
                else if(lookup_ack_i.S1MODE=='d0 & lookup_ack_i.S2MODE=='d0) begin  // S1 and S2 are both Bare
                    work_ppn <= va;
                end
                else if(lookup_ack_i.N)                                             // Svnapot active
                    work_ppn <= {lookup_ack_i.PPN[63:16], va[15:12]};
                else begin                                                          // return ppn according to effective pagesize
                    if(lookup_ack_i_is_2m)
                        work_ppn <= {lookup_ack_i.PPN[63:21], va[20:12]};
                    else if(lookup_ack_i_is_1g)
                        work_ppn <= {lookup_ack_i.PPN[63:30], va[29:12]};
                    else if(lookup_ack_i_is_512g)
                        work_ppn <= {lookup_ack_i.PPN[63:39], va[38:12]};
                    else
                        work_ppn <= lookup_ack_i.PPN;
                end
            end
            else if((ns == S_OUTPUT) & (cs == S_UPACK)) begin
                if(~translate_deny & ~translate_fail) begin                         // translate not fail but permission check fail, or just translate fail
                    if (is_translated & ptw_ack_ff.ENATS & ~ptw_ack_ff.T2GPA) begin
                        work_ppn <= va;
                    end
                    else if(ptw_ack_ff.S1MODE=='d0 & ptw_ack_ff.S2MODE=='d0) begin  // S1 and S2 are both Bare
                        work_ppn <= va;
                    end
                    else if(ptw_ack_ff.N)                                           // Svnapot active
                        work_ppn <= {ptw_ack_ff.PPN[63:16], va[15:12]};
                    else begin                                                      // return ppn according to effective pagesize
                        if(ptw_ack_ff_is_2m)
                            work_ppn <= {ptw_ack_ff.PPN[63:21], va[20:12]};
                        else if(ptw_ack_ff_is_1g)
                            work_ppn <= {ptw_ack_ff.PPN[63:30], va[29:12]};
                        else if(ptw_ack_ff_is_512g)
                            work_ppn <= {ptw_ack_ff.PPN[63:39], va[38:12]};
                        else
                            work_ppn <= ptw_ack_ff.PPN;
                    end
                end
            end
            else if((ns == S_OUTPUT) & (cs == S_MRIF)) begin
                work_ppn <= {5'b0, ptw_ack_ff.PPN[58:12]};
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            work_range  <= 3'b000;
        else begin
            if((ns == S_OUTPUT) & ((cs == S_LOOKUP) | (cs == S_RELOOK))) begin
                if(is_translated & lookup_ack_i.ENATS & ~lookup_ack_i.T2GPA) begin  // Translated request with PA
                    work_range <= lookup_ack_i_range;
                end
                else if(lookup_ack_i.S1MODE=='d0 & lookup_ack_i.S2MODE=='d0) begin  // S1 and S2 are both Bare
                    work_range <= 3'b111;
                end
                else if(lookup_ack_i.N) begin                                       // Svnapot active
                    work_range <= 3'b100;
                end
                else begin                                                          // return ppn according to effective pagesize
                    work_range <= lookup_ack_i_range;
                end
            end
            else if((ns == S_OUTPUT) & (cs == S_UPACK)) begin
                if(~translate_deny & ~translate_fail) begin                         // translate not fail but permission check fail, or just translate fail
                    if (is_translated & ptw_ack_ff.ENATS & ~ptw_ack_ff.T2GPA) begin
                        work_range <= ptw_ack_ff_range;
                    end
                    else if(ptw_ack_ff.S1MODE=='d0 & ptw_ack_ff.S2MODE=='d0) begin  // S1 and S2 are both Bare
                        work_range <= 3'b111;
                    end
                    else if(ptw_ack_ff.N) begin                                     // Svnapot active
                        work_range <= 3'b100;
                    end
                    else begin                                                      // return ppn according to effective pagesize
                        work_range <= ptw_ack_ff_range;
                    end
                end
            end
            else if((ns == S_OUTPUT) & (cs == S_MRIF)) begin
                    work_range <= 3'b101;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            work_pbmt <= 2'b00;
        else begin
            if((ns == S_OUTPUT) & ((cs == S_LOOKUP) | (cs == S_RELOOK))) begin
                work_pbmt <= lookup_ack_i.PBMT;
            end
            else if((ns == S_OUTPUT) & (cs == S_UPACK)) begin
                work_pbmt <= ptw_ack_ff.PBMT;
            end
            else if((ns == S_OUTPUT) & (cs == S_MRIF)) begin
                work_pbmt <= ptw_ack_ff.PBMT;
            end
        end
    end

    assign work_mrif= ptw_ack_ff.MRIF;
    assign work_nid = {ptw_ack_ff.GPPN[61:56], ptw_ack_ff.PPN[63:59]};
    assign work_nppn= ptw_ack_ff.GPPN[55:12];
    assign mrif_credit_req_o= (cs == S_MRIF);
//}}}

//=== invalid {{{
    assign dirty_check_fail =   (cs == S_LOOKUP) &
                                lookup_ack_valid_i &
                                ((lookup_ack_i.S1MODE!='d0 & ~lookup_ack_i_S1_D & wr & lookup_ack_i.SADE) |
                                 (lookup_ack_i.S2MODE!='d0 & ~lookup_ack_i_S2_D & wr & lookup_ack_i.GADE)) &
                                 1'b1;
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            dirty_check_fail_ff    <= 1'b0;
            invalid_req_valid_o <= 1'b0;
            invalid_req_o       <= 'd0;
        end
        else begin
            if(invalid_ack_valid_i & dirty_check_fail_ff)
                dirty_check_fail_ff <= 1'b0;
            else if(dirty_check_fail) begin
                    dirty_check_fail_ff     <= 1'b1;
                    invalid_req_o.idx       <= {1'b1,
                                                (INTERNAL_INV_IDX_WIDTH)'(idx)
                                                };
                    invalid_req_o.itype     <= 2'b10;   // vma with av
                    invalid_req_o.dv_gv     <= (lookup_ack_i.S2MODE != 'd0) ? 1'b1 : 1'b0;
                    invalid_req_o.did_gscid <= {8'd0, lookup_ack_i.GSCID};
                    invalid_req_o.pscv      <= (lookup_ack_i.S1MODE != 'd0) ? 1'b1 : 1'b0;
                    invalid_req_o.pid_pscid <= lookup_ack_i.PSCID;
                    invalid_req_o.av        <= 1'b1;
                    invalid_req_o.addr      <= va;
            end

            if(invalid_req_valid_o & ~invalid_req_ready_i)
                invalid_req_valid_o <= 1'b1;
            else if(dirty_check_fail)
                invalid_req_valid_o <= 1'b1;
            else
                invalid_req_valid_o <= 1'b0;
        end
    end
//}}}

//}}}

//}}}


endmodule
//}}}



