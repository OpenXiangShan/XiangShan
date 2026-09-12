////////////////////////////////////////////////////////////
// iommu_acd_micro_tlb
////////////////////////////////////////////////////////////
module iommu_acd_micro_tlb #( //{{{
    parameter               TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter               MICRO_TLB_IDX_WIDTH         = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    parameter               MICRO_TLB_DEPTH             = 2**MICRO_TLB_IDX_WIDTH,
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter               SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // LOOKUP                                           
    input  logic                                        lookup_req_valid_i,
    output logic                                        lookup_req_ready_o,
    input  LOOKUP_REQ_TYPE                              lookup_req_i,
    output logic                                        lookup_ack_valid_o,
    input  logic                                        lookup_ack_ready_i,
    output LOOKUP_ACK_TYPE                              lookup_ack_o,
    // UPDATE                                           
    input  logic                                        update_req_valid_i,
    input  UPDATE_REQ_TYPE                              update_req_i,
    input  logic                                        mtlb_update_req_valid_i,
    output logic                                        mtlb_update_req_ready_o,
    input  UPDATE_REQ_TYPE                              mtlb_update_req_i,
    // INV                                              
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    // INV_UPDATE_READY
    output logic                                        update_inv_ready_o,
    // MAIN_TLB                                         
    output logic                                        mtlb_lookup_req_valid_o,
    input  logic                                        mtlb_lookup_req_ready_i,
    output LOOKUP_REQ_TYPE                              mtlb_lookup_req_o,
    output logic                                        mtlb_refill_req_valid_o,
    input  logic                                        mtlb_refill_req_ready_i,
    output UPDATE_REQ_TYPE                              mtlb_refill_req_o,
    // CFG                                              
    input  logic                                        csr_fctl_gxl_i,
    input  logic                                        multi_hit_check_i,
    output logic                                        multi_hit_fault_o,
`ifdef IOMMU_IDBG
    // IDBG
    input  iommu_acd_pkg::IDBG_TYPE_M                   idbg_intf_m_i,
    output iommu_acd_pkg::IDBG_TYPE_S                   idbg_intf_s_o,
`endif
    //                                                  
    input  logic                                        spare_i
//}}}                                                   
);
//=== Declare === {{{
    typedef struct packed {
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } microtlb_tag_t;

    typedef struct packed {
        logic [61:12]                                   GPPN;
        logic [19:0]                                    PSCID;
        logic [15:0]                                    GSCID;
    } microtlb_inv_tag_t;

    typedef struct packed {
        logic [1:0]                                     PBMT;
        logic                                           ENATS;
        logic                                           T2GPA;
        logic                                           DTF;
        logic                                           PDTV;
        logic                                           DPE;
        logic                                           SXL;
        logic                                           ENS;
        logic                                           SUM;
        logic                                           S1_D;
        logic                                           S2_D;
        logic                                           SADE;
        logic                                           GADE;
        logic                                           N;
//        logic [15:0]                                    S1_PERM_D;
//        logic [15:0]                                    S1_PERM_A;
        logic [4:0]                                     S1_PERM;        // G U X W R
//        logic [15:0]                                    S2_PERM_D;
//        logic [15:0]                                    S2_PERM_A;
        logic [4:0]                                     S2_PERM;        // G U X W R
        logic [1:0]                                     S1SIZE;         // 00:4K; 01:2M; 10:1G; 11:512G
        logic [1:0]                                     S2SIZE;         //
        logic [3:0]                                     S1MODE;
        logic [3:0]                                     S2MODE;
        logic [3:0]                                     PDTMODE;
        logic [63:12]                                   PPN;
    } microtlb_content_t;

    logic            [MICRO_TLB_DEPTH-1:0]              entry_lookup_req_valid_i;
    LOOKUP_REQ_TYPE  [MICRO_TLB_DEPTH-1:0]              entry_lookup_req_i      ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_lookup_ack_valid_o;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_lookup_ack_ready_i;
    LOOKUP_ACK_TYPE                                     entry_lookup_ack_o[MICRO_TLB_DEPTH-1:0];
    logic            [MICRO_TLB_DEPTH-1:0]              entry_update_req_valid_i;
    UPDATE_REQ_TYPE  [MICRO_TLB_DEPTH-1:0]              entry_update_req_i      ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_inv_req_valid_i   ;
    INVALID_REQ_TYPE [MICRO_TLB_DEPTH-1:0]              entry_inv_req_i         ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_csr_fctl_gxl_i    ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_valid_o           ;
    microtlb_tag_t   [MICRO_TLB_DEPTH-1:0]              entry_hit_tag_o         ;
    microtlb_inv_tag_t [MICRO_TLB_DEPTH-1:0]            entry_inv_tag_o         ;
    microtlb_content_t [MICRO_TLB_DEPTH-1:0]            entry_content_o         ;

    logic            [MICRO_TLB_DEPTH-1:0]              entry_update_req_hit_o  ;

    logic [MICRO_TLB_DEPTH-1:0]                         plru_idx;
    logic [MICRO_TLB_DEPTH-1:0]                         lu_hit_list;
    logic [MICRO_TLB_DEPTH-1:0]                         lu_hit_masked;

    logic                                               multi_hit_flag;

    LOOKUP_REQ_TYPE                                     lookup_req_ff;
    LOOKUP_ACK_TYPE                                     lookup_ack;

    logic [MICRO_TLB_DEPTH-1:0]                         plru_list;
    logic                                               plru_valid;

    logic                                               lookup_ack_ready;

    UPDATE_REQ_TYPE                                     update_req_muxed;

    logic                                               all_entry_valid;
    microtlb_tag_t                                      mtlb_refill_tag_or_t;
    microtlb_inv_tag_t                                  mtlb_refill_itag_or_t;
    microtlb_content_t                                  mtlb_refill_content_or_t;
    logic                                               entry_update_req_ready;

//}}}

//=== Main Code === {{{
//=== UPDATE {{{
    assign update_req_muxed =   mtlb_update_req_valid_i ? mtlb_update_req_i :
                                update_req_valid_i      ? update_req_i      : 'd0;

    assign plru_list  = (|entry_lookup_ack_valid_o) ? lu_hit_list :                 // PLRU updated with lookup_hit
                        (|entry_update_req_valid_i) ? entry_update_req_valid_i:     // PLRU updated with update, make sure no later update 
                                                      'd0;
    assign plru_valid = (|entry_lookup_ack_valid_o & lookup_ack_ready_i) | (|entry_update_req_valid_i & entry_update_req_ready);

    iommu_acd_micro_tlb_plru#(
        .WIDTH          (MICRO_TLB_DEPTH            )
    ) U_plru(
        .clk_i          (clk                        ),
        .rst_ni         (rstn                       ),
        .lu_hit         (plru_list                  ),
        .lu_hit_valid   (plru_valid                 ),
        .replace_en     (plru_idx                   )
    );

genvar ss;
generate
    for(ss=0; ss<MICRO_TLB_DEPTH; ss++) begin : entry_updte_gen
        iommu_acd_micro_tlb_entry_update_wrap #(
        /*parameter type         */ .UPDATE_REQ_TYPE    (UPDATE_REQ_TYPE    ), // = logic,
        /*parameter type         */ .TAG_TYPE           (microtlb_tag_t     ), // = logic,
        /*parameter type         */ .CONTENT_TYPE       (microtlb_content_t ), // = logic,
        /*parameter type         */ .INV_TAG_TYPE       (microtlb_inv_tag_t ), // = logic,
        /*parameter */ .IDX_WIDTH          (MICRO_TLB_IDX_WIDTH) //= 3,
        ) U_entry_updte(
        /*input  TAG_TYPE                            */ .hit_tag_i          (entry_hit_tag_o                    [ss]),
        /*input  INV_TAG_TYPE                        */ .inv_tag_i          (entry_inv_tag_o                    [ss]),
        /*input  CONTENT_TYPE                        */ .content_i          (entry_content_o                    [ss]),
        /*input  logic                               */ .update_req_valid_i (mtlb_update_req_valid_i | update_req_valid_i),
        /*input  UPDATE_REQ_TYPE                     */ .update_req_i       (update_req_muxed                       ),
        /*input  logic [DEPTH-1:0]                   */ .valid_i            (entry_valid_o                          ),
        /*input  logic [IDX_WIDTH-1:0]               */ .tag_i              (MICRO_TLB_IDX_WIDTH'(ss)               ),
        /*input  logic [DEPTH-1:0]                   */ .plru_idx_i         (plru_idx                               ),
        /*output logic                               */ .update_o           (entry_update_req_valid_i           [ss]),
        /*input  logic                               */ .csr_fctl_gxl_i     (entry_csr_fctl_gxl_i               [ss]),
        /*output logic                               */ .update_req_hit_o   (entry_update_req_hit_o             [ss]),
        /*input  logic [DEPTH-1:0]                   */ .update_req_hit_list_i(entry_update_req_hit_o               ),
        /*input  logic                               */ .spare_in           (1'b0                                   ) 
        );
    end
endgenerate

//}}}

//=== ready {{{
    // microTLB active priority in decreasing order: MTLB_UPDATE, UPD_INV, LOOKUP
    // external control logic should guarantee that UPD_REQ and INV_REQ never at the same cycle, and the priority control for UPD&INV is outside microTLB
    assign update_inv_ready_o = mtlb_update_req_valid_i ? 1'b0 : 
                                update_req_valid_i      ? entry_update_req_ready :
                                                          1'b1;
    // when mainTLB can not accept LOOKUP, block LOOKUP input
    assign lookup_req_ready_o = (mtlb_update_req_valid_i | update_req_valid_i | inv_req_valid_i) ? 1'b0 : lookup_ack_ready;
    assign all_entry_valid = &entry_valid_o;
    assign entry_update_req_ready = all_entry_valid ?                                                               // when all entry occupied
                                                        (   (|entry_update_req_valid_i) ? mtlb_refill_req_ready_i : // do have one entry swap-out, check the refill ready from MTLB
                                                                                        1'b1    )                   // no entry swap-out, just accept the update_req and drop it,
                                                        : 1'b1;                                                     // not all entry occupied, no swap-out concern
    assign mtlb_update_req_ready_o= entry_update_req_ready;
//}}}

//=== mtlb refill {{{
    assign mtlb_refill_req_valid_o = all_entry_valid & (|entry_update_req_valid_i);

    always@(*) begin
        mtlb_refill_tag_or_t    = 'd0;
        mtlb_refill_itag_or_t   = 'd0;
        mtlb_refill_content_or_t= 'd0;
        for(int unsigned rfil=0; rfil<MICRO_TLB_DEPTH; rfil++) begin
            if(entry_update_req_valid_i[rfil]) begin
                mtlb_refill_tag_or_t    = entry_hit_tag_o[rfil];
                mtlb_refill_itag_or_t   = entry_inv_tag_o[rfil];
                mtlb_refill_content_or_t= entry_content_o[rfil];
            end
        end
    end

    assign mtlb_refill_req_o.idx                = {1'b0, TLB_QIDX_WIDTH'(0)};
    assign mtlb_refill_req_o.is_translated      = mtlb_refill_tag_or_t.is_translated;
    assign mtlb_refill_req_o.process_id_valid   = mtlb_refill_tag_or_t.process_id_valid;
    assign mtlb_refill_req_o.PBMT               = mtlb_refill_content_or_t.PBMT     ;
    assign mtlb_refill_req_o.GPPN               = mtlb_refill_itag_or_t.GPPN;
    assign mtlb_refill_req_o.ENATS              = mtlb_refill_content_or_t.ENATS    ;
    assign mtlb_refill_req_o.T2GPA              = mtlb_refill_content_or_t.T2GPA    ;
    assign mtlb_refill_req_o.DTF                = mtlb_refill_content_or_t.DTF      ;
    assign mtlb_refill_req_o.PDTV               = mtlb_refill_content_or_t.PDTV     ;
    assign mtlb_refill_req_o.DPE                = mtlb_refill_content_or_t.DPE      ;
    assign mtlb_refill_req_o.SXL                = mtlb_refill_content_or_t.SXL      ;
    assign mtlb_refill_req_o.ENS                = mtlb_refill_content_or_t.ENS      ;
    assign mtlb_refill_req_o.SUM                = mtlb_refill_content_or_t.SUM      ;
    assign mtlb_refill_req_o.S1_D               = mtlb_refill_content_or_t.S1_D     ;
    assign mtlb_refill_req_o.S2_D               = mtlb_refill_content_or_t.S2_D     ;
    assign mtlb_refill_req_o.SADE               = mtlb_refill_content_or_t.SADE     ;
    assign mtlb_refill_req_o.GADE               = mtlb_refill_content_or_t.GADE     ;
    assign mtlb_refill_req_o.N                  = mtlb_refill_content_or_t.N        ;
//    assign mtlb_refill_req_o.S1_PERM_D          = mtlb_refill_content_or_t.S1_PERM_D;
//    assign mtlb_refill_req_o.S1_PERM_A          = mtlb_refill_content_or_t.S1_PERM_A;
    assign mtlb_refill_req_o.S1_PERM            = mtlb_refill_content_or_t.S1_PERM  ;
//    assign mtlb_refill_req_o.S2_PERM_D          = mtlb_refill_content_or_t.S2_PERM_D;
//    assign mtlb_refill_req_o.S2_PERM_A          = mtlb_refill_content_or_t.S2_PERM_A;
    assign mtlb_refill_req_o.S2_PERM            = mtlb_refill_content_or_t.S2_PERM  ;
    assign mtlb_refill_req_o.S1SIZE             = mtlb_refill_content_or_t.S1SIZE   ;
    assign mtlb_refill_req_o.S2SIZE             = mtlb_refill_content_or_t.S2SIZE   ;
    assign mtlb_refill_req_o.S1MODE             = mtlb_refill_content_or_t.S1MODE   ;
    assign mtlb_refill_req_o.S2MODE             = mtlb_refill_content_or_t.S2MODE   ;
    assign mtlb_refill_req_o.PDTMODE            = mtlb_refill_content_or_t.PDTMODE  ;
    assign mtlb_refill_req_o.PSCID              = mtlb_refill_itag_or_t.PSCID;
    assign mtlb_refill_req_o.GSCID              = mtlb_refill_itag_or_t.GSCID;
    assign mtlb_refill_req_o.PPN                = mtlb_refill_content_or_t.PPN;
    assign mtlb_refill_req_o.process_id         = mtlb_refill_tag_or_t.process_id;
    assign mtlb_refill_req_o.device_id          = mtlb_refill_tag_or_t.device_id;
    assign mtlb_refill_req_o.va                 = mtlb_refill_tag_or_t.va;
//}}}

//=== multi_hit {{{
    assign lu_hit_masked = lu_hit_list & entry_lookup_ack_valid_o;
    iommu_multi_bit_1_check#(MICRO_TLB_DEPTH) U_multi_hit_check(.din(lu_hit_masked), .ok(multi_hit_flag));
    assign multi_hit_fault_o = multi_hit_check_i ? multi_hit_flag : 1'b0;
//}}}

//=== LOOKUP_ACK & MAIN_TLB_LOOKUP{{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            lookup_req_ff <= 'd0;
        // bug_fix, lookup_req_ff may updated with following req as the lookup_req_ready_o is update 1 cycle delay with input req. 20241030
        else if(lookup_req_valid_i & lookup_req_ready_o)
            lookup_req_ff <= lookup_req_i;
    end

    assign lookup_ack_valid_o = |lu_hit_masked;
    
    always@(*) begin
        lookup_ack_o        = 'd0;
        lookup_ack          = 'd0;
        for(int unsigned kk=0; kk<MICRO_TLB_DEPTH; kk++) begin
            if(lu_hit_masked[kk]) begin
                lookup_ack              = entry_lookup_ack_o[kk];
                lookup_ack_o.idx        = lookup_req_ff.idx     ;
                lookup_ack_o.hit        = lookup_ack.hit        ;
                lookup_ack_o.PBMT       = lookup_ack.PBMT       ;
                lookup_ack_o.GPPN       = lookup_ack.GPPN       ;
                lookup_ack_o.ENATS      = lookup_ack.ENATS      ;
                lookup_ack_o.T2GPA      = lookup_ack.T2GPA      ;
                lookup_ack_o.DTF        = lookup_ack.DTF        ;
                lookup_ack_o.PDTV       = lookup_ack.PDTV       ;
                lookup_ack_o.DPE        = lookup_ack.DPE        ;
                lookup_ack_o.SXL        = lookup_ack.SXL        ;
                lookup_ack_o.ENS        = lookup_ack.ENS        ;
                lookup_ack_o.SUM        = lookup_ack.SUM        ;
                lookup_ack_o.S1_D       = lookup_ack.S1_D       ;
                lookup_ack_o.S2_D       = lookup_ack.S2_D       ;
                lookup_ack_o.SADE       = lookup_ack.SADE       ;
                lookup_ack_o.GADE       = lookup_ack.GADE       ;
                lookup_ack_o.N          = lookup_ack.N          ;
//                lookup_ack_o.S1_PERM_D  = lookup_ack.S1_PERM_D  ;
//                lookup_ack_o.S1_PERM_A  = lookup_ack.S1_PERM_A  ;
                lookup_ack_o.S1_PERM    = lookup_ack.S1_PERM    ;      // G U X W R
//                lookup_ack_o.S2_PERM_D  = lookup_ack.S2_PERM_D  ;
//                lookup_ack_o.S2_PERM_A  = lookup_ack.S2_PERM_A  ;
                lookup_ack_o.S2_PERM    = lookup_ack.S2_PERM    ;      // G U X W R
                lookup_ack_o.S1SIZE     = lookup_ack.S1SIZE     ;      // 00:4K; 01:2M; 10:1G; 11:512G
                lookup_ack_o.S2SIZE     = lookup_ack.S2SIZE     ;      // 
                lookup_ack_o.S1MODE     = lookup_ack.S1MODE     ;
                lookup_ack_o.S2MODE     = lookup_ack.S2MODE     ;
                lookup_ack_o.PDTMODE    = lookup_ack.PDTMODE    ;
                lookup_ack_o.GSCID      = lookup_ack.GSCID      ;
                lookup_ack_o.PSCID      = lookup_ack.PSCID      ;
                lookup_ack_o.PPN        = lookup_ack.PPN        ;
            end
        end
    end

    assign mtlb_lookup_req_valid_o  = (|entry_lookup_ack_valid_o) & (lu_hit_masked=='d0);
    assign mtlb_lookup_req_o        = lookup_req_ff;

    assign lookup_ack_ready         = mtlb_lookup_req_valid_o ? mtlb_lookup_req_ready_i : lookup_ack_ready_i;
//}}}

//}}}

//=== entry inst === {{{
genvar i;
generate
    for(i=0; i<MICRO_TLB_DEPTH; i++) begin : micro_tlb_entry_gen
        assign entry_lookup_req_valid_i [i] = lookup_req_valid_i & lookup_req_ready_o;
        assign entry_lookup_req_i       [i] = lookup_req_i      ;
        assign entry_lookup_ack_ready_i [i] = lookup_ack_ready  ;
        assign entry_update_req_i       [i] = update_req_muxed  ;
        assign entry_inv_req_valid_i    [i] = inv_req_valid_i    & update_inv_ready_o;
        assign entry_inv_req_i          [i] = inv_req_i         ;
        assign entry_csr_fctl_gxl_i     [i] = csr_fctl_gxl_i    ;
        assign lu_hit_list              [i] = entry_lookup_ack_o[i].hit  ;

        iommu_acd_micro_tlb_entry #(
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = logic,
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = logic,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = logic,
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = logic,
        /*parameter type         */ .TAG_TYPE                   (microtlb_tag_t             ), // = logic,
        /*parameter type         */ .CONTENT_TYPE               (microtlb_content_t         ), // = logic,
        /*parameter type         */ .INV_TAG_TYPE               (microtlb_inv_tag_t         ), // = logic,
        /*parameter */ .SPARE_PARAM                (0                          )  // = 0
        ) u_micro_tlb_entry(                                                                
        /*input  logic                                       */ .clk                        (clk                        ),
        /*input  logic                                       */ .rstn                       (rstn                       ),
        /*input  logic                                       */ .lookup_req_valid_i         (entry_lookup_req_valid_i   [i]),
        /*input  LOOKUP_REQ_TYPE                             */ .lookup_req_i               (entry_lookup_req_i         [i]),
        /*output logic                                       */ .lookup_ack_valid_o         (entry_lookup_ack_valid_o   [i]),
        /*input  logic                                       */ .lookup_ack_ready_i         (entry_lookup_ack_ready_i   [i]),
        /*output LOOKUP_ACK_TYPE                             */ .lookup_ack_o               (entry_lookup_ack_o         [i]),
        /*input  logic                                       */ .update_req_valid_i         (entry_update_req_valid_i   [i] & entry_update_req_ready),
        /*input  UPDATE_REQ_TYPE                             */ .update_req_i               (entry_update_req_i         [i]),
        /*input  logic                                       */ .inv_req_valid_i            (entry_inv_req_valid_i      [i]),
        /*input  INVALID_REQ_TYPE                            */ .inv_req_i                  (entry_inv_req_i            [i]),
        /*input  logic                                       */ .csr_fctl_gxl_i             (entry_csr_fctl_gxl_i       [i]),
        /*output logic                                       */ .valid_o                    (entry_valid_o              [i]),
        /*output TAG_TYPE                                    */ .hit_tag_o                  (entry_hit_tag_o            [i]),
        /*output INV_TAG_TYPE                                */ .inv_tag_o                  (entry_inv_tag_o            [i]),
        /*output CONTENT_TYPE                                */ .content_o                  (entry_content_o            [i]),
        /*input  logic                                       */ .spare_i                    (1'b0                       ) 
        );
    end
endgenerate
//}}}

`ifdef IOMMU_IDBG
//=== IDBG {{{
    iommu_acd_mon_unit_microtlb #(
    /*parameter */ .MICRO_TLB_IDX_WIDTH        (MICRO_TLB_IDX_WIDTH        ), // = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    /*parameter type         */ .MICROTLB_TAG_T             (microtlb_tag_t             ), // = logic,
    /*parameter type         */ .MICROTLB_INV_TAG_T         (microtlb_inv_tag_t         ), // = logic,
    /*parameter type         */ .MICROTLB_CONTENT_T         (microtlb_content_t         )  // = logic,
    ) U_idbg(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .idbg_go_i                  (idbg_intf_m_i.idbg_go      ),
    /*output logic                                      */  .idbg_busy_o                (idbg_intf_s_o.idbg_busy    ),
    /*input  logic [7:0]                                */  .idbg_opcode_i              (idbg_intf_m_i.idbg_opcode  ),
    /*input  logic [31:0]                               */  .idbg_dat_i                 (idbg_intf_m_i.idbg_datw    ),
    /*output logic [31:0]                               */  .idbg_dat_o                 (idbg_intf_s_o.idbg_datr    ),
    /*output logic                                      */  .idbg_datv_o                (idbg_intf_s_o.idbg_datv    ),
    /*input  logic [MICRO_TLB_DEPTH-1:0]                */  .entry_valid_i              (entry_valid_o              ),
    /*input  MICROTLB_TAG_T   [MICRO_TLB_DEPTH-1:0]     */  .entry_hit_tag_i            (entry_hit_tag_o            ),
    /*input  MICROTLB_INV_TAG_T [MICRO_TLB_DEPTH-1:0]   */  .entry_inv_tag_i            (entry_inv_tag_o            ),
    /*input  MICROTLB_CONTENT_T [MICRO_TLB_DEPTH-1:0]   */  .entry_content_i            (entry_content_o            ),
    /*input  logic                                      */  .param_in                   (1'b0                       )
    );
//}}}
`endif

endmodule
//}}}



////////////////////////////////////////////////////////
// iommu_acd_micro_tlb_entry
////////////////////////////////////////////////////////
module iommu_acd_micro_tlb_entry #( //{{{
//{{{ PARAM
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter type          TAG_TYPE                    = iommu_acd_pkg::microtlb_tag_t,
    parameter type          CONTENT_TYPE                = iommu_acd_pkg::microtlb_content_t,
    parameter type          INV_TAG_TYPE                = iommu_acd_pkg::microtlb_inv_tag_t,
    parameter  SPARE_PARAM                 = 0     
//}}}
)(                                                      
//{{{ IO                                                
    input  logic                                        clk                 ,
    input  logic                                        rstn                ,
    // LOOKUP                                           
    input  logic                                        lookup_req_valid_i  ,
    input  LOOKUP_REQ_TYPE                              lookup_req_i        ,
    output logic                                        lookup_ack_valid_o  ,
    input  logic                                        lookup_ack_ready_i  ,
    output LOOKUP_ACK_TYPE                              lookup_ack_o        ,
    // UPDATE                                           
    input  logic                                        update_req_valid_i  ,
    input  UPDATE_REQ_TYPE                              update_req_i        ,
    //output logic                                        update_do_o         ,
    // INV                                              
    input  logic                                        inv_req_valid_i     ,
    input  INVALID_REQ_TYPE                             inv_req_i           ,
    //                                                  
    input  logic                                        csr_fctl_gxl_i      ,
    output logic                                        valid_o             ,
    output TAG_TYPE                                     hit_tag_o           ,
    output INV_TAG_TYPE                                 inv_tag_o           ,
    output CONTENT_TYPE                                 content_o           ,
    input  logic                                        spare_i              
//}}}                                                   
);                                                      
//=== Declare {{{                                       
    logic [63:12]                                       iova_i;
    logic [23:0]                                        device_id_i;
    logic [19:0]                                        process_id_i;
    logic                                               process_id_valid_i;
    logic                                               is_translated_i;

    logic                                               valid;
    TAG_TYPE                                            hit_tag;
    INV_TAG_TYPE                                        inv_tag;
    CONTENT_TYPE                                        content;
                                                        
    logic                                               device_id_hit;
    logic                                               process_id_hit;
    logic                                               translated_hit;
    logic                                               va_hit;
    logic                                               vpn0_hit;
    logic                                               vpn1_hit;
    logic                                               vpn2_hit;
    logic                                               vpn3_hit;
    logic                                               vpn4_hit;
    logic                                               tag_hit;
                                                        
    logic [21:12]                                       vpn0_sv32;
    logic [31:22]                                       vpn1_sv32;
    logic [21:12]                                       vpn0_sv32x4;
    logic [33:22]                                       vpn1_sv32x4;
    logic [33:12]                                       ppn_sv32;
                                                        
    logic [20:12]                                       vpn0_sv39;
    logic [29:21]                                       vpn1_sv39;
    logic [38:30]                                       vpn2_sv39;
    logic [20:12]                                       vpn0_sv39x4;
    logic [29:21]                                       vpn1_sv39x4;
    logic [40:30]                                       vpn2_sv39x4;    //
    logic [55:12]                                       ppn_sv39;
                                                        
    logic [20:12]                                       vpn0_sv48;
    logic [29:21]                                       vpn1_sv48;
    logic [38:30]                                       vpn2_sv48;
    logic [47:39]                                       vpn3_sv48;
    logic [20:12]                                       vpn0_sv48x4;
    logic [29:21]                                       vpn1_sv48x4;
    logic [38:30]                                       vpn2_sv48x4;
    logic [49:39]                                       vpn3_sv48x4;    //
    logic [55:12]                                       ppn_sv48;
                                                        
    logic [20:12]                                       vpn0_sv57;
    logic [29:21]                                       vpn1_sv57;
    logic [38:30]                                       vpn2_sv57;
    logic [47:39]                                       vpn3_sv57;
    logic [56:48]                                       vpn4_sv57;
    logic [20:12]                                       vpn0_sv57x4;
    logic [29:21]                                       vpn1_sv57x4;
    logic [38:30]                                       vpn2_sv57x4;
    logic [47:39]                                       vpn3_sv57x4;
    logic [58:48]                                       vpn4_sv57x4;
    logic [55:12]                                       ppn_sv57;

    logic [63:57]                                       vpn5_sv;
    logic [63:59]                                       vpn5_svx4;
                                                        
    logic                                               s1_bare;
    logic                                               s1_sv32;
    logic                                               s1_sv39;
    logic                                               s1_sv48;
    logic                                               s1_sv57;
    logic                                               s2_bare;
    logic                                               s2_sv32x4;
    logic                                               s2_sv39x4;
    logic                                               s2_sv48x4;
    logic                                               s2_sv57x4;
    logic                                               s1_2m;
    logic                                               s2_2m;
    logic                                               is_2m;
    logic                                               s1_1g;
    logic                                               s2_1g;
    logic                                               is_1g;
    logic                                               s1_512g;
    logic                                               s2_512g;
    logic                                               is_512g;
                                                        
    logic                                               vma_i;
    logic                                               gvma_i;
    logic [63:12]                                       inv_addr_i;
    logic                                               inv_av_i;
    logic [19:0]                                        inv_pscid_i;
    logic                                               inv_pscv_i;
    logic [15:0]                                        inv_gscid_i;
    logic                                               inv_gv_i;
                                                        
    logic                                               inval_ddt_i;
    logic                                               inval_pdt_i;
    logic [19:0]                                        inv_process_id_i;
    logic [23:0]                                        inv_device_id_i;
    logic                                               inv_dv_i;
                                                        
    logic [63:12]                                       inv_tag_gpa;
                                                        
    logic [21:12]                                       inv_vpn0_sv32;
    logic [31:22]                                       inv_vpn1_sv32;
    logic [21:12]                                       inv_vpn0_sv32x4;
    logic [33:22]                                       inv_vpn1_sv32x4;
                                                        
    logic [20:12]                                       inv_vpn0_sv39;
    logic [29:21]                                       inv_vpn1_sv39;
    logic [38:30]                                       inv_vpn2_sv39;
    logic [20:12]                                       inv_vpn0_sv39x4;
    logic [29:21]                                       inv_vpn1_sv39x4;
    logic [40:30]                                       inv_vpn2_sv39x4;    //
                                                        
    logic [20:12]                                       inv_vpn0_sv48;
    logic [29:21]                                       inv_vpn1_sv48;
    logic [38:30]                                       inv_vpn2_sv48;
    logic [47:39]                                       inv_vpn3_sv48;
    logic [20:12]                                       inv_vpn0_sv48x4;
    logic [29:21]                                       inv_vpn1_sv48x4;
    logic [38:30]                                       inv_vpn2_sv48x4;
    logic [49:39]                                       inv_vpn3_sv48x4;    //
                                                        
    logic [20:12]                                       inv_vpn0_sv57;
    logic [29:21]                                       inv_vpn1_sv57;
    logic [38:30]                                       inv_vpn2_sv57;
    logic [47:39]                                       inv_vpn3_sv57;
    logic [56:48]                                       inv_vpn4_sv57;
    logic [20:12]                                       inv_vpn0_sv57x4;
    logic [29:21]                                       inv_vpn1_sv57x4;
    logic [38:30]                                       inv_vpn2_sv57x4;
    logic [47:39]                                       inv_vpn3_sv57x4;
    logic [58:48]                                       inv_vpn4_sv57x4;
                                                        
    logic                                               inv_device_id_hit;
    logic                                               inv_process_id_hit;
                                                        
    logic                                               inv_pscid_hit;
    logic                                               inv_gscid_hit;
    logic                                               inv_addr_hit;
    logic                                               inv_vpn0_hit;
    logic                                               inv_vpn1_hit;
    logic                                               inv_vpn2_hit;
    logic                                               inv_vpn3_hit;
    logic                                               inv_vpn4_hit;
                                                        
    logic                                               update_req_valid;
//}}}

//=== Main Code {{{{
//=== {{{
    assign valid_o                      = valid;
    assign hit_tag_o                    = hit_tag;
    assign inv_tag_o                    = inv_tag;
    assign content_o                    = content;

    assign device_id_i                  = lookup_req_i.device_id;
    assign process_id_i                 = lookup_req_i.process_id;
    assign process_id_valid_i           = lookup_req_i.process_id_valid;
    assign is_translated_i              = lookup_req_i.is_translated;
                                        
    assign iova_i                       = lookup_req_i.va;
    assign vpn0_sv32                    = iova_i[21:12];
    assign vpn1_sv32                    = iova_i[31:22];
    assign vpn0_sv32x4                  = iova_i[21:12];
    assign vpn1_sv32x4                  = iova_i[33:22];
    assign vpn0_sv39                    = iova_i[20:12];
    assign vpn1_sv39                    = iova_i[29:21];
    assign vpn2_sv39                    = iova_i[38:30];
    assign vpn0_sv39x4                  = iova_i[20:12];
    assign vpn1_sv39x4                  = iova_i[29:21];
    assign vpn2_sv39x4                  = iova_i[40:30];
    assign vpn0_sv48                    = iova_i[20:12];
    assign vpn1_sv48                    = iova_i[29:21];
    assign vpn2_sv48                    = iova_i[38:30];
    assign vpn3_sv48                    = iova_i[47:39];
    assign vpn0_sv48x4                  = iova_i[20:12];
    assign vpn1_sv48x4                  = iova_i[29:21];
    assign vpn2_sv48x4                  = iova_i[38:30];
    assign vpn3_sv48x4                  = iova_i[49:39];
    assign vpn0_sv57                    = iova_i[20:12];
    assign vpn1_sv57                    = iova_i[29:21];
    assign vpn2_sv57                    = iova_i[38:30];
    assign vpn3_sv57                    = iova_i[47:39];
    assign vpn4_sv57                    = iova_i[56:48];
    assign vpn0_sv57x4                  = iova_i[20:12];
    assign vpn1_sv57x4                  = iova_i[29:21];
    assign vpn2_sv57x4                  = iova_i[38:30];
    assign vpn3_sv57x4                  = iova_i[47:39];
    assign vpn4_sv57x4                  = iova_i[58:48];
    assign vpn5_sv                      = iova_i[63:57];
    assign vpn5_svx4                    = iova_i[63:59];
                                        
    assign s1_bare                      = content.S1MODE=='d0;
    assign s1_sv32                      = content.SXL ? content.S1MODE=='d8 : 1'b0;
    assign s1_sv39                      = content.SXL ? 1'b0                : content.S1MODE=='d8;
    assign s1_sv48                      = content.SXL ? 1'b0                : content.S1MODE=='d9;
    assign s1_sv57                      = content.SXL ? 1'b0                : content.S1MODE=='d10;
    assign s2_bare                      = content.S2MODE=='d0;
    assign s2_sv32x4                    = csr_fctl_gxl_i ? content.S2MODE=='d8 : 1'b0;
    assign s2_sv39x4                    = csr_fctl_gxl_i ? 1'b0                : content.S2MODE=='d8;
    assign s2_sv48x4                    = csr_fctl_gxl_i ? 1'b0                : content.S2MODE=='d9;
    assign s2_sv57x4                    = csr_fctl_gxl_i ? 1'b0                : content.S2MODE=='d10;
    assign s1_2m                        = content.S1SIZE == 'b01;
    assign s2_2m                        = content.S2SIZE == 'b01;
    assign is_2m                        = (~s1_bare & ~s2_bare) ? ((s2_2m & (s1_2m | s1_1g | s1_512g)) | (s1_2m & (s2_2m | s2_1g | s2_512g))) :
                                                                  ((s2_2m & ~s2_bare) | (s1_2m & ~s1_bare));
    assign s1_1g                        = content.S1SIZE == 'b10;
    assign s2_1g                        = content.S2SIZE == 'b10;
    assign is_1g                        = (~s1_bare & ~s2_bare) ? ((s2_1g & (s1_1g | s1_512g)) | (s1_1g & (s2_1g | s2_512g))) :
                                                                  ((s2_1g & ~s2_bare) | (s1_1g & ~s1_bare));
    assign s1_512g                      = content.S1SIZE == 'b11;
    assign s2_512g                      = content.S2SIZE == 'b11;
    assign is_512g                      = (~s1_bare & ~s2_bare) ? (s2_512g & s1_512g) :
                                                                  ((s2_512g & ~s2_bare) | (s1_512g & ~s1_bare));

//}}}

//=== LOOKUP {{{
//    assign device_id_hit    = ~valid_o ? 1'b0 : (device_id_i     == hit_tag.device_id);
//    assign process_id_hit   = ~valid_o ? 1'b0 : (hit_tag.process_id_valid ? ((process_id_i==hit_tag.process_id) & process_id_valid_i) : ~process_id_valid_i);
//    assign translated_hit   = ~valid_o ? 1'b0 : (is_translated_i == hit_tag.is_translated);
//
//    assign vpn0_hit         = ~valid_o ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   & (vpn0_sv32   == hit_tag.va[21:12]))  | (vpn0_sv39   == hit_tag.va[20:12])) :
//                              ~s2_bare ? ((s2_sv32x4 & (vpn0_sv32x4 == hit_tag.va[21:12]))  | (vpn0_sv39x4 == hit_tag.va[20:12])) :
//                              1'b1;
//    assign vpn1_hit         = ~valid_o ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   & (vpn1_sv32   == hit_tag.va[33:22]))  | (vpn1_sv39   == hit_tag.va[29:21])) :
//                              ~s2_bare ? ((s2_sv32x4 & (vpn1_sv32x4 == hit_tag.va[33:22]))  | (vpn1_sv39x4 == hit_tag.va[29:21])) :
//                              1'b1;
//    assign vpn2_hit         = ~valid_o ? 1'b0 :
//                              ~s1_bare ? ( s1_sv32                                          | (vpn2_sv39   == hit_tag.va[38:30])) :
//                              ~s2_bare ? ( s2_sv32x4                                        | (s2_sv39x4 ? (vpn2_sv39x4 == hit_tag.va[40:30]) : (vpn2_sv48x4 == hit_tag.va[38:30]))) :
//                              1'b1;
//    assign vpn3_hit         = ~valid_o ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   | s1_sv39)                             | (vpn3_sv48   == hit_tag.va[47:39])) :
//                              ~s2_bare ? ((s2_sv32x4 | s2_sv39x4)                           | (s2_sv48x4 ? (vpn3_sv48x4 == hit_tag.va[49:39]) : (vpn3_sv57x4 == hit_tag.va[47:39]))) :
//                              1'b1;
//    assign vpn4_hit         = ~valid_o ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   | s1_sv39 | s1_sv48)                   | (vpn4_sv57   == hit_tag.va[56:48])) :
//                              ~s2_bare ? ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)               | (vpn4_sv57x4 == hit_tag.va[58:48])) :
//                              1'b1;
//    assign va_hit           = (is_2m | vpn0_hit) & (is_1g | vpn1_hit) & (is_512g | vpn2_hit) & vpn3_hit & vpn4_hit;
//    assign tag_hit          = va_hit & translated_hit & process_id_hit & device_id_hit;
    iommu_acd_micro_tlb_tag_hit U_tag_hit(
    /*input  logic[21:12]               */  .vpn0_sv32                  (vpn0_sv32                  ),
    /*input  logic[31:22]               */  .vpn1_sv32                  (vpn1_sv32                  ),
    /*input  logic[21:12]               */  .vpn0_sv32x4                (vpn0_sv32x4                ),
    /*input  logic[33:22]               */  .vpn1_sv32x4                (vpn1_sv32x4                ),
    /*input  logic[20:12]               */  .vpn0_sv39                  (vpn0_sv39                  ),
    /*input  logic[29:21]               */  .vpn1_sv39                  (vpn1_sv39                  ),
    /*input  logic[38:30]               */  .vpn2_sv39                  (vpn2_sv39                  ),
    /*input  logic[20:12]               */  .vpn0_sv39x4                (vpn0_sv39x4                ),
    /*input  logic[29:21]               */  .vpn1_sv39x4                (vpn1_sv39x4                ),
    /*input  logic[40:30]               */  .vpn2_sv39x4                (vpn2_sv39x4                ),
    /*input  logic[20:12]               */  .vpn0_sv48                  (vpn0_sv48                  ),
    /*input  logic[29:21]               */  .vpn1_sv48                  (vpn1_sv48                  ),
    /*input  logic[38:30]               */  .vpn2_sv48                  (vpn2_sv48                  ),
    /*input  logic[47:39]               */  .vpn3_sv48                  (vpn3_sv48                  ),
    /*input  logic[20:12]               */  .vpn0_sv48x4                (vpn0_sv48x4                ),
    /*input  logic[29:21]               */  .vpn1_sv48x4                (vpn1_sv48x4                ),
    /*input  logic[38:30]               */  .vpn2_sv48x4                (vpn2_sv48x4                ),
    /*input  logic[49:39]               */  .vpn3_sv48x4                (vpn3_sv48x4                ),
    /*input  logic[20:12]               */  .vpn0_sv57                  (vpn0_sv57                  ),
    /*input  logic[29:21]               */  .vpn1_sv57                  (vpn1_sv57                  ),
    /*input  logic[38:30]               */  .vpn2_sv57                  (vpn2_sv57                  ),
    /*input  logic[47:39]               */  .vpn3_sv57                  (vpn3_sv57                  ),
    /*input  logic[56:48]               */  .vpn4_sv57                  (vpn4_sv57                  ),
    /*input  logic[20:12]               */  .vpn0_sv57x4                (vpn0_sv57x4                ),
    /*input  logic[29:21]               */  .vpn1_sv57x4                (vpn1_sv57x4                ),
    /*input  logic[38:30]               */  .vpn2_sv57x4                (vpn2_sv57x4                ),
    /*input  logic[47:39]               */  .vpn3_sv57x4                (vpn3_sv57x4                ),
    /*input  logic[58:48]               */  .vpn4_sv57x4                (vpn4_sv57x4                ),
    /*input  logic[63:57]               */  .vpn5_sv                    (vpn5_sv                    ),
    /*input  logic[63:59]               */  .vpn5_svx4                  (vpn5_svx4                  ),
    /*input  logic [23:0]               */  .device_id_i                (device_id_i                ),
    /*input  logic                      */  .process_id_valid_i         (process_id_valid_i         ),
    /*input  logic [19:0]               */  .process_id_i               (process_id_i               ),
    /*input  logic                      */  .is_translated_i            (is_translated_i            ),
    /*input  logic                      */  .valid_i                    (valid_o                    ),
    /*input  logic [23:0]               */  .hit_tag_device_id          (hit_tag.device_id          ),
    /*input  logic                      */  .hit_tag_process_id_valid   (hit_tag.process_id_valid   ),
    /*input  logic [19:0]               */  .hit_tag_process_id         (hit_tag.process_id         ),
    /*input  logic                      */  .hit_tag_is_translated      (hit_tag.is_translated      ),
    /*input  logic [63:12]              */  .hit_tag_va                 (hit_tag.va                 ),
    /*input  logic                      */  .content_n                  (content.N                  ),
    /*input  logic                      */  .s1_bare                    (s1_bare                    ),
    /*input  logic                      */  .s1_sv32                    (s1_sv32                    ),
    /*input  logic                      */  .s1_sv39                    (s1_sv39                    ),
    /*input  logic                      */  .s1_sv48                    (s1_sv48                    ),
    /*input  logic                      */  .s2_bare                    (s2_bare                    ),
    /*input  logic                      */  .s2_sv32x4                  (s2_sv32x4                  ),
    /*input  logic                      */  .s2_sv39x4                  (s2_sv39x4                  ),
    /*input  logic                      */  .s2_sv48x4                  (s2_sv48x4                  ),
    /*input  logic                      */  .is_2m                      (is_2m                      ),
    /*input  logic                      */  .is_1g                      (is_1g                      ),
    /*input  logic                      */  .is_512g                    (is_512g                    ),
    /*output logic                      */  .tag_hit                    (tag_hit                    ) 
    );

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            lookup_ack_valid_o  <= 1'b0;
            lookup_ack_o        <= 'd0;
        end
        else begin
            if(lookup_ack_valid_o & ~lookup_ack_ready_i)
                lookup_ack_valid_o <= 1'b1;
            else begin
                if(lookup_req_valid_i)
                    lookup_ack_valid_o <= 1'b1;
                else
                    lookup_ack_valid_o <= 1'b0;
            end

            if(lookup_req_valid_i) begin
                lookup_ack_o.idx        <= lookup_req_i.idx ;
                lookup_ack_o.hit        <= tag_hit          ;
                lookup_ack_o.PBMT       <= content.PBMT     ;
                lookup_ack_o.GPPN       <= inv_tag.GPPN     ;
                lookup_ack_o.ENATS      <= content.ENATS    ;
                lookup_ack_o.T2GPA      <= content.T2GPA    ;
                lookup_ack_o.DTF        <= content.DTF      ;
                lookup_ack_o.PDTV       <= content.PDTV     ;
                lookup_ack_o.DPE        <= content.DPE      ;
                lookup_ack_o.SXL        <= content.SXL      ;
                lookup_ack_o.ENS        <= content.ENS      ;
                lookup_ack_o.SUM        <= content.SUM      ;
                lookup_ack_o.S1_D       <= content.S1_D     ;
                lookup_ack_o.S2_D       <= content.S2_D     ;
                lookup_ack_o.SADE       <= content.SADE     ;
                lookup_ack_o.GADE       <= content.GADE     ;
                lookup_ack_o.N          <= content.N        ;
//                lookup_ack_o.S1_PERM_D  <= content.S1_PERM_D;
//                lookup_ack_o.S1_PERM_A  <= content.S1_PERM_A;
                lookup_ack_o.S1_PERM    <= content.S1_PERM  ;       // G U X W R
//                lookup_ack_o.S2_PERM_D  <= content.S2_PERM_D;
//                lookup_ack_o.S2_PERM_A  <= content.S2_PERM_A;
                lookup_ack_o.S2_PERM    <= content.S2_PERM  ;       // G U X W R
                lookup_ack_o.S1SIZE     <= content.S1SIZE   ;       // 00:4K; 01:2M; 10:1G; 11:512G
                lookup_ack_o.S2SIZE     <= content.S2SIZE   ;       // 
                lookup_ack_o.S1MODE     <= content.S1MODE   ;
                lookup_ack_o.S2MODE     <= content.S2MODE   ;
                lookup_ack_o.PDTMODE    <= content.PDTMODE  ;
                lookup_ack_o.GSCID      <= inv_tag.GSCID    ;
                lookup_ack_o.PSCID      <= inv_tag.PSCID    ;
                lookup_ack_o.PPN        <= content.PPN      ;       // no svnapot ppn[15:12] handle in tlb_cache, which is done in tlb_queue
            end
        end
    end
//}}}

//=== UPDATE & INV {{{
    iommu_acd_micro_tlb_inv_hit #(
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = logic,
    /*parameter */ .SPARE_PARAM                (0                          ) // = 0,
    ) U_inv_hit(
    /*input  logic                                       */ .inv_req_valid_i            (inv_req_valid_i    ),
    /*input  INVALID_REQ_TYPE                            */ .inv_req_i                  (inv_req_i          ),
    /*input  logic                                       */ .s1_bare                    (s1_bare            ),
    /*input  logic                                       */ .s1_sv32                    (s1_sv32            ),
    /*input  logic                                       */ .s1_sv39                    (s1_sv39            ),
    /*input  logic                                       */ .s1_sv48                    (s1_sv48            ),
    /*input  logic                                       */ .s1_sv57                    (s1_sv57            ),
    /*input  logic                                       */ .s2_bare                    (s2_bare            ),
    /*input  logic                                       */ .s2_sv32x4                  (s2_sv32x4          ),
    /*input  logic                                       */ .s2_sv39x4                  (s2_sv39x4          ),
    /*input  logic                                       */ .s2_sv48x4                  (s2_sv48x4          ),
    /*input  logic                                       */ .s2_sv57x4                  (s2_sv57x4          ),
    /*input  logic                                       */ .s1_2m                      (s1_2m              ),
    /*input  logic                                       */ .s2_2m                      (s2_2m              ),
    /*input  logic                                       */ .s1_1g                      (s1_1g              ),
    /*input  logic                                       */ .s2_1g                      (s2_1g              ),
    /*input  logic                                       */ .s1_512g                    (s1_512g            ),
    /*input  logic                                       */ .s2_512g                    (s2_512g            ),
    /*input  logic [23:0]                                */ .hit_tag_device_id          (hit_tag.device_id  ),
    /*input  logic [19:0]                                */ .hit_tag_process_id         (hit_tag.process_id ),
    /*input  logic [15:0]                                */ .inv_tag_GSCID              (inv_tag.GSCID      ),
    /*input  logic [19:0]                                */ .inv_tag_PSCID              (inv_tag.PSCID      ),
    /*input  logic [63:12]                               */ .hit_tag_va                 (hit_tag.va         ),
    /*input  logic [61:12]                               */ .inv_tag_GPPN               (inv_tag.GPPN       ),
    /*input  logic                                       */ .content_n                  (content.N          ),
    /*input  logic                                       */ .content_G                  (content.S1_PERM[4] ),
    /*output logic                                       */ .inv_hit                    (inv_hit            ),
    /*input  logic                                       */ .spare_in                   (1'b0               )
    );

    //===
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            valid   <= 1'b0;
            hit_tag <= 'd0;
            inv_tag <= 'd0;
            content <= 'd0;
        end
        else begin
            if(inv_hit)
                valid <= 1'b0;
            else if(update_req_valid_i)
                valid <= 1'b1;

            if(update_req_valid_i) begin
                hit_tag.is_translated   <= update_req_i.is_translated   ;
                hit_tag.process_id_valid<= update_req_i.process_id_valid;
                hit_tag.process_id      <= update_req_i.process_id      ;
                hit_tag.device_id       <= update_req_i.device_id       ;
                hit_tag.va              <= update_req_i.va              ;
                content.PBMT            <= update_req_i.PBMT            ;
                inv_tag.GPPN            <= update_req_i.GPPN            ;
                inv_tag.PSCID           <= update_req_i.PSCID           ;
                inv_tag.GSCID           <= update_req_i.GSCID           ;
                content.ENATS           <= update_req_i.ENATS           ;
                content.T2GPA           <= update_req_i.T2GPA           ;
                content.DTF             <= update_req_i.DTF             ;
                content.PDTV            <= update_req_i.PDTV            ;
                content.DPE             <= update_req_i.DPE             ;
                content.SXL             <= update_req_i.SXL             ;
                content.ENS             <= update_req_i.ENS             ;
                content.SUM             <= update_req_i.SUM             ;
                content.S1_D            <= update_req_i.S1_D            ;
                content.S2_D            <= update_req_i.S2_D            ;
                content.SADE            <= update_req_i.SADE            ;
                content.GADE            <= update_req_i.GADE            ;
                content.N               <= update_req_i.N               ;
//                content.S1_PERM_D       <= update_req_i.S1_PERM_D       ;
//                content.S1_PERM_A       <= update_req_i.S1_PERM_A       ;
                content.S1_PERM         <= update_req_i.S1_PERM         ;        // G U X W R
//                content.S2_PERM_D       <= update_req_i.S2_PERM_D       ;
//                content.S2_PERM_A       <= update_req_i.S2_PERM_A       ;
                content.S2_PERM         <= update_req_i.S2_PERM         ;        // G U X W R
                content.S1SIZE          <= update_req_i.S1SIZE          ;
                content.S2SIZE          <= update_req_i.S2SIZE          ;
                content.S1MODE          <= update_req_i.S1MODE          ;
                content.S2MODE          <= update_req_i.S2MODE          ;
                content.PDTMODE         <= update_req_i.PDTMODE         ;
                content.PPN             <= update_req_i.PPN             ;
            end
        end
    end


    //assign update_device_id_hit     = ~valid_o ? 1'b0 : (update_device_id_i     == hit_tag.device_id);
    //assign update_process_id_hit    = ~valid_o ? 1'b0 : (update_process_id_i    == hit_tag.process_id) & (update_process_id_valid_i == hit_tag.process_id_valid);
    //assign update_translated_hit    = ~valid_o ? 1'b0 : (update_is_translated_i == hit_tag.is_translated);
    //
    //assign update_vpn0_hit          = ~valid_o ? 1'b0 :
    //                                  ~s1_bare ? ((s1_sv32   & (update_vpn0_sv32   == hit_tag.va[21:12]))  | (update_vpn0_sv39   == hit_tag.va[20:12])) :
    //                                  ~s2_bare ? ((s2_sv32x4 & (update_vpn0_sv32x4 == hit_tag.va[21:12]))  | (update_vpn0_sv39x4 == hit_tag.va[20:12])) :
    //                                  1'b0;
    //assign update_vpn1_hit          = ~valid_o ? 1'b0 :
    //                                  ~s1_bare ? ((s1_sv32   & (update_vpn1_sv32   == hit_tag.va[33:22]))  | (update_vpn1_sv39   == hit_tag.va[29:21])) :
    //                                  ~s2_bare ? ((s2_sv32x4 & (update_vpn1_sv32x4 == hit_tag.va[33:22]))  | (update_vpn1_sv39x4 == hit_tag.va[29:21])) :
    //                                  1'b0;
    //assign update_vpn2_hit          = ~valid_o ? 1'b0 :
    //                                  ~s1_bare ? ( s1_sv32                                          | (update_vpn2_sv39   == hit_tag.va[38:30])) :
    //                                  ~s2_bare ? ( s2_sv32x4                                        | (s2_sv39x4 ? (update_vpn2_sv39x4 == hit_tag.va[40:30]) : (update_vpn2_sv48x4 == hit_tag.va[38:30]))) :
    //                                  1'b0;
    //assign update_vpn3_hit          = ~valid_o ? 1'b0 :
    //                                  ~s1_bare ? ((s1_sv32   | s1_sv39)                             | (update_vpn3_sv48   == hit_tag.va[47:39])) :
    //                                  ~s2_bare ? ((s2_sv32x4 | s2_sv39x4)                           | (s2_sv48x4 ? (update_vpn3_sv48x4 == hit_tag.va[49:39]) : (update_vpn3_sv57x4 == hit_tag.va[47:39]))) :
    //                                  1'b0;
    //assign update_vpn4_hit          = ~valid_o ? 1'b0 :
    //                                  ~s1_bare ? ((s1_sv32   | s1_sv39 | s1_sv48)                   | (update_vpn4_sv57   == hit_tag.va[56:48])) :
    //                                  ~s2_bare ? ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)               | (update_vpn4_sv57x4 == hit_tag.va[58:48])) :
    //                                  1'b0;
    //assign update_va_hit            = (is_2m | update_vpn0_hit) & (is_1g | update_vpn1_hit) & (is_512g | update_vpn2_hit) & update_vpn3_hit & update_vpn4_hit;
    //assign update_tag_hit           = update_va_hit & update_translated_hit & update_process_id_hit & update_device_id_hit;
    //assign update_req_valid         = update_req_valid_i & (~update_tag_hit);
    //assign update_do_o              = update_req_valid;
//}}}
//}}}

endmodule
//}}}




