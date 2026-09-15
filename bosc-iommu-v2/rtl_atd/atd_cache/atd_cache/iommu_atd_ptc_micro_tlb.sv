////////////////////////////////////////////////////////////
// iommu_atd_s2ptc_micro_tlb
////////////////////////////////////////////////////////////
module iommu_atd_ptc_micro_tlb #( //{{{
    parameter   CACHE_TYPE                  = 0, // 0:S2PTC, 1:S1PTC
    parameter   TLB_QIDX_WIDTH              = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter   MICRO_TLB_IDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    parameter   MICRO_TLB_DEPTH             = 2**MICRO_TLB_IDX_WIDTH,
    parameter type          LOOKUP_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter type          MICROTLB_TAG_TYPE           = iommu_atd_cache_pkg::s2ptc_microtlb_tag_t,
    parameter type          MICROTLB_INV_TAG_TYPE       = iommu_atd_cache_pkg::s2ptc_microtlb_inv_tag_t,
    parameter type          MICROTLB_CONTENT_TYPE       = iommu_atd_cache_pkg::s2ptc_microtlb_content_t,
    parameter   SPARE_PARAM                 = 0
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
    //                                                  
    input  logic                                        spare_i
//}}}                                                   
);                                                      
//=== Declare {{{                                       
    logic            [MICRO_TLB_DEPTH-1:0]              entry_lookup_req_valid_i;
    LOOKUP_REQ_TYPE  [MICRO_TLB_DEPTH-1:0]              entry_lookup_req_i      ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_lookup_ack_valid_o;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_lookup_ack_ready_i;
    LOOKUP_ACK_TYPE                                     entry_lookup_ack_o[MICRO_TLB_DEPTH-1:0];
    logic            [MICRO_TLB_DEPTH-1:0]              entry_update_req_valid_i;
    UPDATE_REQ_TYPE  [MICRO_TLB_DEPTH-1:0]              entry_update_req_i      ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_inv_req_valid_i   ;
    INVALID_REQ_TYPE [MICRO_TLB_DEPTH-1:0]              entry_inv_req_i         ;
    logic            [MICRO_TLB_DEPTH-1:0]              entry_valid_o           ;
    MICROTLB_TAG_TYPE[MICRO_TLB_DEPTH-1:0]              entry_hit_tag_o         ;
    MICROTLB_INV_TAG_TYPE [MICRO_TLB_DEPTH-1:0]         entry_inv_tag_o         ;
    MICROTLB_CONTENT_TYPE [MICRO_TLB_DEPTH-1:0]         entry_content_o         ;

    logic                                               entry_update_req_ready;
                                                        
    logic            [MICRO_TLB_DEPTH-1:0]              entry_update_req_hit_o  ;           
    logic            [MICRO_TLB_DEPTH-1:0]              entry_update_req_hit_o_lvl_masked;  // only entry[K].lvl==update.lvl and entry_update_req_hit_o[K]==1, is true hit
                                                        
    logic [MICRO_TLB_DEPTH-1:0]                         swap_out_hit_list;
    logic [2:0] [MICRO_TLB_DEPTH-1:0]                   swap_out_hit_list_l;
    logic [MICRO_TLB_DEPTH-1:0]                         plru_idx;
    logic [MICRO_TLB_DEPTH-1:0]                         plru_idx_lvl_masked;    // when refill happen and do have entry matching the update, selec the highest lvl to swap-out
    logic [MICRO_TLB_DEPTH-1:0]                         lu_hit_list;
    logic [MICRO_TLB_DEPTH-1:0]                         lu_hit_masked;
                                                        
    logic                                               multi_hit_flag;
                                                        
    LOOKUP_REQ_TYPE                                     lookup_req_ff;
    LOOKUP_ACK_TYPE                                     lookup_ack;
    LOOKUP_ACK_TYPE [2:0]                               lookup_ack_l;
    logic           [2:0]                               lookup_ack_l_valid;
                                                        
    logic [MICRO_TLB_DEPTH-1:0]                         plru_list;
    logic                                               plru_valid;
                                                        
    logic                                               lookup_ack_ready;

    UPDATE_REQ_TYPE                                     update_req_muxed;
    
    logic                                               all_entry_valid;
    logic [MICRO_TLB_DEPTH-1:0] [$bits(MICROTLB_TAG_TYPE )-1:0]     mtlb_refill_tag;
    logic [MICRO_TLB_DEPTH-1:0] [$bits(MICROTLB_INV_TAG_TYPE)-1:0]  mtlb_refill_itag;
    logic [MICRO_TLB_DEPTH-1:0] [$bits(MICROTLB_CONTENT_TYPE)-1:0]  mtlb_refill_content;
    logic                       [$bits(MICROTLB_TAG_TYPE )-1:0]     mtlb_refill_tag_or;
    logic                       [$bits(MICROTLB_INV_TAG_TYPE)-1:0]  mtlb_refill_itag_or;
    logic                       [$bits(MICROTLB_CONTENT_TYPE)-1:0]  mtlb_refill_content_or;
    MICROTLB_TAG_TYPE                                               mtlb_refill_tag_or_t;
    MICROTLB_INV_TAG_TYPE                                           mtlb_refill_itag_or_t;
    MICROTLB_CONTENT_TYPE                                           mtlb_refill_content_or_t;
    MICROTLB_TAG_TYPE                                               mtlb_refill_tag_ori;
    MICROTLB_INV_TAG_TYPE                                           mtlb_refill_itag_ori;
    MICROTLB_CONTENT_TYPE                                           mtlb_refill_content_ori;

//}}}

//=== Main Code
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

// just store lvl0 by now       always@(*) begin
// just store lvl0 by now           mtlb_refill_tag_ori     = 'd0;
// just store lvl0 by now           mtlb_refill_itag_ori    = 'd0;
// just store lvl0 by now           mtlb_refill_content_ori = 'd0;
// just store lvl0 by now           for(int unsigned ori=0; ori<MICRO_TLB_DEPTH; ori++) begin
// just store lvl0 by now               if(plru_idx[ori]==1'b1) begin
// just store lvl0 by now                   mtlb_refill_tag_ori     = entry_hit_tag_o[ori];
// just store lvl0 by now                   mtlb_refill_itag_ori    = entry_inv_tag_o[ori];
// just store lvl0 by now                   mtlb_refill_content_ori = entry_content_o[ori];
// just store lvl0 by now               end
// just store lvl0 by now           end
// just store lvl0 by now       end
// just store lvl0 by now   // check if any entry hit with this original swap-out entry, but lvl higher than it
// just store lvl0 by now   genvar sout;
// just store lvl0 by now   generate
// just store lvl0 by now       for(sout=0; sout<MICRO_TLB_DEPTH; sout++) begin : swap_out_hit_gen
// just store lvl0 by now           if(CACHE_TYPE==0) begin : s2ptc_tag_hit_gen
// just store lvl0 by now               assign update_process_id_i = 'd0;
// just store lvl0 by now               iommu_atd_dtc_micro_tlb_tag_hit #(
// just store lvl0 by now               /*parameter  */ .CACHE_TYPE (CACHE_TYPE                 )  // = 0, // 0:DDTC, 1:PDTC
// just store lvl0 by now               ) U_update_tag_hit(
// just store lvl0 by now               /*input  logic [23:0]               */  .device_id_i                (mtlb_refill_tag_ori.device_id  ),
// just store lvl0 by now               /*input  logic [19:0]               */  .process_id_i               (20'd0                          ),
// just store lvl0 by now               /*input  logic                      */  .valid_i                    (entry_valid_o[sout]            ),
// just store lvl0 by now               /*input  logic [23:0]               */  .hit_tag_device_id          (entry_hit_tag_o[sout].device_id),
// just store lvl0 by now               /*input  logic [19:0]               */  .hit_tag_process_id         (20'd0                          ),
// just store lvl0 by now               /*input  logic [1:0]                */  .content_lvl                (entry_content_o[sout].lvl      ),
// just store lvl0 by now               /*output logic                      */  .tag_hit                    (swap_out_hit_list[sout]        ) 
// just store lvl0 by now               );
// just store lvl0 by now           end
// just store lvl0 by now           else begin : s1ptc_tag_hit_gen
// just store lvl0 by now               assign update_process_id_i = update_req_i.process_id;
// just store lvl0 by now               iommu_atd_dtc_micro_tlb_tag_hit #(
// just store lvl0 by now               /*parameter  */ .CACHE_TYPE (CACHE_TYPE                 )  // = 0, // 0:DDTC, 1:PDTC
// just store lvl0 by now               ) U_update_tag_hit(
// just store lvl0 by now               /*input  logic [23:0]               */  .device_id_i                (mtlb_refill_tag_ori.device_id  ),
// just store lvl0 by now               /*input  logic [19:0]               */  .process_id_i               (mtlb_refill_tag_ori.process_id ),
// just store lvl0 by now               /*input  logic                      */  .valid_i                    (entry_valid_o[sout]            ),
// just store lvl0 by now               /*input  logic [23:0]               */  .hit_tag_device_id          (entry_hit_tag_o[sout].device_id),
// just store lvl0 by now               /*input  logic [19:0]               */  .hit_tag_process_id         (entry_hit_tag_o[sout].process_id),
// just store lvl0 by now               /*input  logic [1:0]                */  .content_lvl                (entry_content_o[sout].lvl      ),
// just store lvl0 by now               /*output logic                      */  .tag_hit                    (swap_out_hit_list[sout]        ) 
// just store lvl0 by now               );
// just store lvl0 by now           end
// just store lvl0 by now       end
// just store lvl0 by now   endgenerate
// just store lvl0 by now       
// just store lvl0 by now       always@(*) begin
// just store lvl0 by now           swap_out_hit_list_l[0] = 'd0;
// just store lvl0 by now           for(int sohl0=0; sohl0<MICRO_TLB_DEPTH; sohl0++) begin
// just store lvl0 by now               if(swap_out_hit_list[sohl0] & entry_content_o[sohl0].lvl=='d0)
// just store lvl0 by now                   swap_out_hit_list_l[0][sohl0] = 1'b1;
// just store lvl0 by now           end
// just store lvl0 by now       end
// just store lvl0 by now   
// just store lvl0 by now       always@(*) begin
// just store lvl0 by now           swap_out_hit_list_l[1] = 'd0;
// just store lvl0 by now           for(int sohl1=0; sohl1<MICRO_TLB_DEPTH; sohl1++) begin
// just store lvl0 by now               if(swap_out_hit_list[sohl1] & entry_content_o[sohl1].lvl=='d1)
// just store lvl0 by now                   swap_out_hit_list_l[1][sohl1] = 1'b1;
// just store lvl0 by now           end
// just store lvl0 by now       end
// just store lvl0 by now   
// just store lvl0 by now       always@(*) begin
// just store lvl0 by now           swap_out_hit_list_l[2] = 'd0;
// just store lvl0 by now           for(int sohl2=0; sohl2<MICRO_TLB_DEPTH; sohl2++) begin
// just store lvl0 by now               if(swap_out_hit_list[sohl2] & entry_content_o[sohl2].lvl=='d2)
// just store lvl0 by now                   swap_out_hit_list_l[2][sohl2] = 1'b1;
// just store lvl0 by now           end
// just store lvl0 by now       end
// just store lvl0 by now   
// just store lvl0 by now       always@(*) begin
// just store lvl0 by now           plru_idx_lvl_masked = plru_idx;
// just store lvl0 by now           if(all_entry_valid) begin
// just store lvl0 by now               if(swap_out_hit_list_l[2]!='d0) begin
// just store lvl0 by now                   plru_idx_lvl_masked = swap_out_hit_list_l[2];
// just store lvl0 by now               end
// just store lvl0 by now               else if(swap_out_hit_list_l[1]!='d0) begin
// just store lvl0 by now                   plru_idx_lvl_masked = swap_out_hit_list_l[1];
// just store lvl0 by now               end
// just store lvl0 by now               else if(swap_out_hit_list_l[0]!='d0) begin
// just store lvl0 by now                   plru_idx_lvl_masked = swap_out_hit_list_l[0];
// just store lvl0 by now               end
// just store lvl0 by now               else
// just store lvl0 by now                   plru_idx_lvl_masked = plru_idx;
// just store lvl0 by now           end
// just store lvl0 by now       end
assign plru_idx_lvl_masked = plru_idx;

// just store lvl0 by now    genvar eurh_masked;
// just store lvl0 by now    generate
// just store lvl0 by now        for(eurh_masked=0; eurh_masked<MICRO_TLB_DEPTH; eurh_masked++) begin : entry_update_req_hit_o_lvl_maksed_gen
// just store lvl0 by now            assign entry_update_req_hit_o_lvl_masked[eurh_masked] = entry_update_req_hit_o[eurh_masked] & (entry_content_o[eurh_masked].lvl==update_req_muxed.lvl);
// just store lvl0 by now        end
// just store lvl0 by now    endgenerate
assign entry_update_req_hit_o_lvl_masked = entry_update_req_hit_o;

genvar ss;
generate
    for(ss=0; ss<MICRO_TLB_DEPTH; ss++) begin : entry_updte_gen
        iommu_atd_ptc_micro_tlb_entry_update_wrap #(
        /*parameter type         */ .CACHE_TYPE         (CACHE_TYPE             ), // = 0, 0:DDTC, 1:PDTC
        /*parameter type         */ .UPDATE_REQ_TYPE    (UPDATE_REQ_TYPE        ), // = logic,
        /*parameter type         */ .TAG_TYPE           (MICROTLB_TAG_TYPE      ), // = logic,
        /*parameter type         */ .CONTENT_TYPE       (MICROTLB_CONTENT_TYPE  ), // = logic,
        /*parameter type         */ .INV_TAG_TYPE       (MICROTLB_INV_TAG_TYPE  ), // = logic,
        /*parameter  */ .IDX_WIDTH          (MICRO_TLB_IDX_WIDTH    )  //= 3,
        ) U_entry_updte(
        /*input  TAG_TYPE                            */ .hit_tag_i              (entry_hit_tag_o                    [ss]),
        /*input  INV_TAG_TYPE                        */ .inv_tag_i              (entry_inv_tag_o                    [ss]),
        /*input  CONTENT_TYPE                        */ .content_i              (entry_content_o                    [ss]),
//        /*input  logic                               */ .update_req_valid_i ((mtlb_update_req_valid_i & mtlb_update_req_ready_o) | (update_req_valid_i & update_inv_ready_o)),
        /*input  logic                               */ .update_req_valid_i     (mtlb_update_req_valid_i | update_req_valid_i),
        /*input  UPDATE_REQ_TYPE                     */ .update_req_i           (update_req_muxed                       ),
        /*input  logic [DEPTH-1:0]                   */ .valid_i                (entry_valid_o                          ),
        /*input  logic [IDX_WIDTH-1:0]               */ .tag_i                  (MICRO_TLB_IDX_WIDTH'(ss)               ),
        /*input  logic [DEPTH-1:0]                   */ .plru_idx_i             (plru_idx_lvl_masked                    ),
        /*output logic                               */ .update_o               (entry_update_req_valid_i           [ss]),
        /*output logic                               */ .update_req_hit_o       (entry_update_req_hit_o             [ss]),
        /*input  logic [DEPTH-1:0]                   */ .update_req_hit_list_i  (entry_update_req_hit_o_lvl_masked      ),
        /*input  logic                               */ .csr_fctl_gxl_i         (csr_fctl_gxl_i                         ),
        /*input  logic                               */ .spare_in               (1'b0                                   ) 
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
    // when update with all entry valid, an entry will be swapout and refilled to maintlb, if maintlb updcabin is full, backpressure as ready
    assign all_entry_valid = &entry_valid_o;
    assign entry_update_req_ready = all_entry_valid ? mtlb_refill_req_ready_i : 1'b1;
    assign mtlb_update_req_ready_o= entry_update_req_ready;
//}}}

//=== mtlb refill {{{
    assign mtlb_refill_req_valid_o = all_entry_valid & (|entry_update_req_valid_i);
//genvar rfil;
//generate
//    for(rfil=0; rfil<MICRO_TLB_DEPTH; rfil++) begin : mtlb_refill_struct_gen
//        assign mtlb_refill_tag    [rfil] = entry_update_req_valid_i[rfil] ? entry_hit_tag_o[rfil] : 'd0;
//        assign mtlb_refill_itag   [rfil] = entry_update_req_valid_i[rfil] ? entry_inv_tag_o[rfil] : 'd0;
//        assign mtlb_refill_content[rfil] = entry_update_req_valid_i[rfil] ? entry_content_o[rfil] : 'd0;
//    end
//endgenerate
//    assign mtlb_refill_tag_or                   = |mtlb_refill_tag;
//    assign mtlb_refill_itag_or                  = |mtlb_refill_itag;
//    assign mtlb_refill_content_or               = |mtlb_refill_content;
//    assign mtlb_refill_tag_or_t                 = mtlb_refill_tag_or;
//    assign mtlb_refill_itag_or_t                = mtlb_refill_itag_or;
//    assign mtlb_refill_content_or_t             = mtlb_refill_content_or;
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

generate
    if(CACHE_TYPE==0) begin : s2ptc_mtlb_refill_req_o_con
        assign mtlb_refill_req_o.idx                = {3'b000, 1'b1, TLB_QIDX_WIDTH'(0)};
        assign mtlb_refill_req_o.lvl                = mtlb_refill_content_or_t.lvl;
        assign mtlb_refill_req_o.prefetched         = 1'b0;
        assign mtlb_refill_req_o.addr               = mtlb_refill_tag_or_t.addr;
        assign mtlb_refill_req_o.gscid              = mtlb_refill_tag_or_t.gscid;
        assign mtlb_refill_req_o.N                  = mtlb_refill_content_or_t.N;
        assign mtlb_refill_req_o.PBMT               = mtlb_refill_content_or_t.PBMT;
        assign mtlb_refill_req_o.D                  = mtlb_refill_content_or_t.D;
        assign mtlb_refill_req_o.A                  = mtlb_refill_content_or_t.A;
        assign mtlb_refill_req_o.PPN                = mtlb_refill_content_or_t.PPN;
        assign mtlb_refill_req_o.PERM               = mtlb_refill_content_or_t.PERM;
        assign mtlb_refill_req_o.V                  = mtlb_refill_content_or_t.V;
    end
    else begin : s1ptc_mtlb_refill_req_o_con
        assign mtlb_refill_req_o.idx                = {3'b000, 1'b1, TLB_QIDX_WIDTH'(0)};
        assign mtlb_refill_req_o.lvl                = mtlb_refill_content_or_t.lvl;
        assign mtlb_refill_req_o.prefetched         = 1'b0;
        assign mtlb_refill_req_o.addr               = mtlb_refill_tag_or_t.addr;
        assign mtlb_refill_req_o.gv                 = mtlb_refill_tag_or_t.gv;
        assign mtlb_refill_req_o.gscid              = mtlb_refill_tag_or_t.gscid;
        assign mtlb_refill_req_o.pscid              = mtlb_refill_tag_or_t.pscid;
        assign mtlb_refill_req_o.sxl                = mtlb_refill_tag_or_t.sxl;
        assign mtlb_refill_req_o.N                  = mtlb_refill_content_or_t.N;
        assign mtlb_refill_req_o.PBMT               = mtlb_refill_content_or_t.PBMT;
        assign mtlb_refill_req_o.D                  = mtlb_refill_content_or_t.D;
        assign mtlb_refill_req_o.A                  = mtlb_refill_content_or_t.A;
        assign mtlb_refill_req_o.PPN                = mtlb_refill_content_or_t.PPN;
        assign mtlb_refill_req_o.PERM               = mtlb_refill_content_or_t.PERM;
        assign mtlb_refill_req_o.V                  = mtlb_refill_content_or_t.V;
    end
endgenerate
//}}}

//=== multi_hit {{{
    assign lu_hit_masked = lu_hit_list & entry_lookup_ack_valid_o;
    iommu_multi_bit_1_check#(MICRO_TLB_DEPTH) U_multi_hit_check(.din(lu_hit_masked), .ok(multi_hit_flag));
// just store lvl0 by now    // in DDTC/PDTC may hit multi lvl entry, so no multi_fault gen anymore
// just store lvl0 by now    //    assign multi_hit_fault_o = multi_hit_check_i ? multi_hit_flag : 0;
// just store lvl0 by now        assign multi_hit_fault_o = 'd0;
    assign multi_hit_fault_o = multi_hit_check_i ? multi_hit_flag : 'd0;
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

// just store lvl0 by now        always@(*) begin
// just store lvl0 by now            lookup_ack_l[0]       = 'd0;
// just store lvl0 by now            lookup_ack_l_valid[0] = 'd0;
// just store lvl0 by now            for(int unsigned ll0=0; ll0<MICRO_TLB_DEPTH; ll0++) begin
// just store lvl0 by now                if(lu_hit_masked[ll0] & entry_lookup_ack_o[ll0].lvl=='d0) begin
// just store lvl0 by now                    lookup_ack_l[0]       = entry_lookup_ack_o[ll0];
// just store lvl0 by now                    lookup_ack_l_valid[0] = 1'b1;
// just store lvl0 by now                end
// just store lvl0 by now            end
// just store lvl0 by now        end
// just store lvl0 by now    
// just store lvl0 by now        always@(*) begin
// just store lvl0 by now            lookup_ack_l[1]       = 'd0;
// just store lvl0 by now            lookup_ack_l_valid[1] = 'd0;
// just store lvl0 by now            for(int unsigned ll1=0; ll1<MICRO_TLB_DEPTH; ll1++) begin
// just store lvl0 by now                if(lu_hit_masked[ll1] & entry_lookup_ack_o[ll1].lvl=='d1) begin
// just store lvl0 by now                    lookup_ack_l[1]       = entry_lookup_ack_o[ll1];
// just store lvl0 by now                    lookup_ack_l_valid[1] = 1'b1;
// just store lvl0 by now                end
// just store lvl0 by now            end
// just store lvl0 by now        end
// just store lvl0 by now    
// just store lvl0 by now        always@(*) begin
// just store lvl0 by now            lookup_ack_l[2]       = 'd0;
// just store lvl0 by now            lookup_ack_l_valid[2] = 'd0;
// just store lvl0 by now            for(int unsigned ll2=0; ll2<MICRO_TLB_DEPTH; ll2++) begin
// just store lvl0 by now                if(lu_hit_masked[ll2] & entry_lookup_ack_o[ll2].lvl=='d2) begin
// just store lvl0 by now                    lookup_ack_l[2]       = entry_lookup_ack_o[ll2];
// just store lvl0 by now                    lookup_ack_l_valid[2] = 1'b1;
// just store lvl0 by now                end
// just store lvl0 by now            end
// just store lvl0 by now        end

generate
//    if(CACHE_TYPE==0) begin : s2ptc_lookup_ack_o_gen
        always@(*) begin
            lookup_ack_o        = 'd0;
            lookup_ack          = 'd0;
            for(int unsigned kk=0; kk<MICRO_TLB_DEPTH; kk++) begin
// just sotre lvl0 by now                for(int unsigned kk=0; kk<=2; kk++) begin
                if(lu_hit_masked[kk]) begin
// jsut store lvl0 by now                    if(lookup_ack_l_valid[2-kk]) begin
                    lookup_ack                      = entry_lookup_ack_o[kk];
// just store lvl0 by now                        lookup_ack                      = lookup_ack_l[2-kk]     ;
                    lookup_ack_o.idx                = lookup_req_ff.idx     ;
                    lookup_ack_o.lvl                = lookup_ack.lvl        ;
                    lookup_ack_o.hit                = lookup_ack.hit        ;
                    lookup_ack_o.prefetched         = 1'b0                  ;
//                    lookup_ack_o.pte_position       = lookup_ack.lvl=='d0 ? lookup_req_ff.addr[14:12] :
//                                                      lookup_ack.lvl=='d1 ? lookup_req_ff.addr[23:21] :
//                                                      lookup_ack.lvl=='d2 ? lookup_req_ff.addr[32:30] :
//                                                      lookup_ack.lvl=='d3 ? lookup_req_ff.addr[41:39] :
//                                                      lookup_ack.lvl=='d4 ? lookup_req_ff.addr[50:48] : 'd0;
                    lookup_ack_o.pte_position       = lookup_ack.pte_position;
                    lookup_ack_o.N                  = lookup_ack.N          ;
                    lookup_ack_o.PBMT               = lookup_ack.PBMT       ;
                    lookup_ack_o.D                  = lookup_ack.D          ;
                    lookup_ack_o.A                  = lookup_ack.A          ;
                    lookup_ack_o.PPN                = lookup_ack.PPN        ;
                    lookup_ack_o.PERM               = lookup_ack.PERM       ;
                    lookup_ack_o.V                  = lookup_ack.V          ;
                end
            end
        end
//    end
//    else begin : s1ptc_lookup_ack_o_gen
//        always@(*) begin
//            lookup_ack_o        = 'd0;
//            lookup_ack          = 'd0;
//            for(int unsigned kk=0; kk<MICRO_TLB_DEPTH; kk++) begin
//                if(lu_hit_masked[kk]) begin
//                    lookup_ack                      = entry_lookup_ack_o[kk];
//                    lookup_ack_o.idx                = lookup_req_ff.idx     ;
//                    lookup_ack_o.lvl                = lookup_ack.lvl        ;
//                    lookup_ack_o.hit                = lookup_ack.hit        ;
//                    lookup_ack_o.prefetched         = 1'b0                  ;
//                    lookup_ack_o.pte_position       = lookup_ack.lvl=='d0 ? lookup_req_ff.addr[14:12] :
//                                                      lookup_ack.lvl=='d1 ? (entry_content_o[kk].sxl==1'b1 ? lookup_req_ff.addr[24:22] : lookup_req_ff.addr[23:21]) :
//                                                      lookup_ack.lvl=='d2 ? lookup_req_ff.addr[32:30] :
//                                                      lookup_ack.lvl=='d3 ? lookup_req_ff.addr[41:39] :
//                                                      lookup_ack.lvl=='d4 ? lookup_req_ff.addr[50:48] : 'd0;
//                    lookup_ack_o.N                  = lookup_ack.N          ;
//                    lookup_ack_o.D                  = lookup_ack.D          ;
//                    lookup_ack_o.A                  = lookup_ack.A          ;
//                    lookup_ack_o.PPN                = lookup_ack.PPN        ;
//                    lookup_ack_o.PERM               = lookup_ack.PERM       ;
//                    lookup_ack_o.V                  = lookup_ack.V          ;
//                end
//            end
//        end
//    end
endgenerate

    assign mtlb_lookup_req_valid_o  = (|entry_lookup_ack_valid_o) & (lu_hit_masked=='d0);
    assign mtlb_lookup_req_o        = lookup_req_ff;

    assign lookup_ack_ready         = mtlb_lookup_req_valid_o ? mtlb_lookup_req_ready_i : lookup_ack_ready_i;
//}}}

//=== {{{ entry inst
genvar i;
generate
    for(i=0; i<MICRO_TLB_DEPTH; i++) begin : micro_tlb_entry_gen
        assign entry_lookup_req_valid_i [i] = lookup_req_valid_i & lookup_req_ready_o;
        assign entry_lookup_req_i       [i] = lookup_req_i      ;
        assign entry_lookup_ack_ready_i [i] = lookup_ack_ready  ;
        assign entry_update_req_i       [i] = update_req_muxed  ;
        assign entry_inv_req_valid_i    [i] = inv_req_valid_i    & update_inv_ready_o;
        assign entry_inv_req_i          [i] = inv_req_i         ;
        assign lu_hit_list              [i] = entry_lookup_ack_o[i].hit  ;

        iommu_atd_ptc_micro_tlb_entry #(
        /*parameter  */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:S2PTC, 1:S1PTC
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::s2ptc_lookup_req_t,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::s2ptc_lookup_ack_t,
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::s2ptc_update_req_t,
        /*parameter type         */ .TAG_TYPE                   (MICROTLB_TAG_TYPE          ), // = iommu_atd_cache_pkg::s2ptc_microtlb_tag_t,
        /*parameter type         */ .CONTENT_TYPE               (MICROTLB_CONTENT_TYPE      ), // = iommu_atd_cache_pkg::s2ptc_microtlb_content_t,
        /*parameter type         */ .INV_TAG_TYPE               (MICROTLB_INV_TAG_TYPE      ), // = iommu_atd_cache_pkg::s2ptc_microtlb_inv_tag_t,
        /*parameter  */ .SPARE_PARAM                (0                          )  // = 0
        ) u_micro_tlb_entry(                                                                
        /*input  logic                                       */ .clk                        (clk                            ),
        /*input  logic                                       */ .rstn                       (rstn                           ),
        /*input  logic                                       */ .lookup_req_valid_i         (entry_lookup_req_valid_i    [i]),
        /*input  LOOKUP_REQ_TYPE                             */ .lookup_req_i               (entry_lookup_req_i          [i]),
        /*output logic                                       */ .lookup_ack_valid_o         (entry_lookup_ack_valid_o    [i]),
        /*input  logic                                       */ .lookup_ack_ready_i         (entry_lookup_ack_ready_i    [i]),
        /*output LOOKUP_ACK_TYPE                             */ .lookup_ack_o               (entry_lookup_ack_o          [i]),
        /*input  logic                                       */ .update_req_valid_i         (entry_update_req_valid_i    [i] & entry_update_req_ready),
        /*input  UPDATE_REQ_TYPE                             */ .update_req_i               (entry_update_req_i          [i]),
        /*input  logic                                       */ .inv_req_valid_i            (entry_inv_req_valid_i       [i]),
        /*input  INVALID_REQ_TYPE                            */ .inv_req_i                  (entry_inv_req_i             [i]),
        /*output logic                                       */ .valid_o                    (entry_valid_o               [i]),
        /*output TAG_TYPE                                    */ .hit_tag_o                  (entry_hit_tag_o             [i]),
        /*output INV_TAG_TYPE                                */ .inv_tag_o                  (entry_inv_tag_o             [i]),
        /*output CONTENT_TYPE                                */ .content_o                  (entry_content_o             [i]),
        /*input  logic                                       */ .csr_fctl_gxl_i             (csr_fctl_gxl_i                 ),
        /*input  logic                                       */ .spare_i                    (1'b0                           ) 
        );
    end
endgenerate
//}}}

endmodule
//}}}



////////////////////////////////////////////////////////////
// iommu_atd_ptc_micro_tlb_entry
////////////////////////////////////////////////////////////
module iommu_atd_ptc_micro_tlb_entry #( //{{{
//{{{ PARAM
    parameter   CACHE_TYPE                  = 0, // 0:S2PTC, 1:S1PTC
    parameter type          INVALID_REQ_TYPE            = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter type          TAG_TYPE                    = iommu_atd_cache_pkg::s2ptc_microtlb_tag_t,
    parameter type          CONTENT_TYPE                = iommu_atd_cache_pkg::s2ptc_microtlb_content_t,
    parameter type          INV_TAG_TYPE                = iommu_atd_cache_pkg::s2ptc_microtlb_inv_tag_t,
    parameter   SPARE_PARAM                 = 0     
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
    output logic                                        valid_o             ,
    output TAG_TYPE                                     hit_tag_o           ,
    output INV_TAG_TYPE                                 inv_tag_o           ,
    output CONTENT_TYPE                                 content_o           ,
    //
    input  logic                                        csr_fctl_gxl_i      ,
    input  logic                                        spare_i              
//}}}                                                   
);
//=== Declare === {{{
    logic                                               gv_i;
    logic [15:0]                                        gscid_i;
    logic [19:0]                                        pscid_i;
    logic [63:12]                                       addr_i;

    logic                                               valid;
    TAG_TYPE                                            hit_tag;
    INV_TAG_TYPE                                        inv_tag;
    CONTENT_TYPE                                        content;

    logic                                               tag_hit, inv_hit;
//}}}

//=== Main Code === {{{
//=== {{{
    assign valid_o                      = valid;
    assign hit_tag_o                    = hit_tag;
    assign inv_tag_o                    = inv_tag;
    assign content_o                    = content;

generate
    if(CACHE_TYPE=='d0) begin : s2ptc_gv_i_gen
        assign gv_i = 1'b1;
    end
    else begin : s1ptc_gv_i_gen
        assign gv_i = lookup_req_i.gv;
    end
endgenerate

    assign gscid_i                      = lookup_req_i.gscid;
    assign addr_i                       = lookup_req_i.addr;
//}}}

//=== LOOKUP {{{
generate
    if(CACHE_TYPE==0) begin : s2ptc_tag_hit_gen
        assign pscid_i = 'd0;
        iommu_atd_ptc_micro_tlb_tag_hit #(
        /*parameter  */ .CACHE_TYPE (CACHE_TYPE                 )
        ) U_tag_hit(
        /*input  logic                      */  .gv_i                       (gv_i                       ),
        /*input  logic [15:0]               */  .gscid_i                    (gscid_i                    ),
        /*input  logic [19:0]               */  .pscid_i                    (pscid_i                    ),
        /*input  logic [63:12]              */  .addr_i                     (addr_i                     ),
        /*input  logic                      */  .valid_i                    (valid_o                    ),
        /*input  logic                      */  .hit_tag_gv                 (1'b1                       ),
        /*input  logic [15:0]               */  .hit_tag_gscid              (hit_tag.gscid              ),
        /*input  logic [19:0]               */  .hit_tag_pscid              (20'd0                      ),
        /*input  logic [63:12]              */  .hit_tag_addr               (hit_tag.addr               ),
        /*input  logic                      */  .hit_tag_sxl                (1'b0                       ),
        /*input  logic                      */  .csr_gxl                    (csr_fctl_gxl_i             ),
        /*input  logic [2:0]                */  .content_lvl                (content.lvl                ),
        /*input  logic                      */  .content_n                  (content.N                  ),
        /*output logic                      */  .tag_hit                    (tag_hit                    )

        );
    end
    else begin : s1ptc_tag_hit_gen
        assign pscid_i = lookup_req_i.pscid;
        iommu_atd_ptc_micro_tlb_tag_hit #(
        /*parameter  */ .CACHE_TYPE (CACHE_TYPE                 )
        ) U_tag_hit(
        /*input  logic                      */  .gv_i                       (gv_i                       ),
        /*input  logic [15:0]               */  .gscid_i                    (gscid_i                    ),
        /*input  logic [19:0]               */  .pscid_i                    (pscid_i                    ),
        /*input  logic [63:12]              */  .addr_i                     (addr_i                     ),
        /*input  logic                      */  .valid_i                    (valid_o                    ),
        /*input  logic                      */  .hit_tag_gv                 (hit_tag.gv                 ),
        /*input  logic [15:0]               */  .hit_tag_gscid              (hit_tag.gscid              ),
        /*input  logic [19:0]               */  .hit_tag_pscid              (hit_tag.pscid              ),
        /*input  logic [63:12]              */  .hit_tag_addr               (hit_tag.addr               ),
        /*input  logic                      */  .hit_tag_sxl                (hit_tag.sxl                ),
        /*input  logic                      */  .csr_gxl                    (csr_fctl_gxl_i             ),
        /*input  logic [2:0]                */  .content_lvl                (content.lvl                ),
        /*input  logic                      */  .content_n                  (content.N                  ),
        /*output logic                      */  .tag_hit                    (tag_hit                    )
        );
    end
endgenerate

generate
    if(CACHE_TYPE==0) begin : s2ptc_lookup_ack_o_gen
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
                    lookup_ack_o.idx                <= lookup_req_i.idx ;
                    lookup_ack_o.lvl                <= content.lvl      ;
                    lookup_ack_o.hit                <= tag_hit          ;
                    lookup_ack_o.prefetched         <= 1'b0             ;
                    lookup_ack_o.pte_position       <= content.lvl=='d0 ? lookup_req_i.addr[15:12] :
                                                       content.lvl=='d1 ? (csr_fctl_gxl_i ? {{1'b0, lookup_req_i.addr[24:22]}} : {1'b0, lookup_req_i.addr[23:21]}) :
                                                       content.lvl=='d2 ? {1'b0, lookup_req_i.addr[32:30]} :
                                                       content.lvl=='d3 ? {1'b0, lookup_req_i.addr[41:39]} :
                                                       content.lvl=='d4 ? {1'b0, lookup_req_i.addr[50:48]} : 4'd0;
                    lookup_ack_o.N                  <= content.N        ;
                    lookup_ack_o.PBMT               <= content.PBMT     ;
                    lookup_ack_o.D                  <= content.D        ;
                    lookup_ack_o.A                  <= content.A        ;
                    lookup_ack_o.PPN                <= content.PPN      ;
                    lookup_ack_o.PERM               <= content.PERM     ;
                    lookup_ack_o.V                  <= content.V        ;
                end
            end
        end
    end
    else begin : s1ptc_lookup_ack_o_gen
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
                    lookup_ack_o.idx                <= lookup_req_i.idx ;
                    lookup_ack_o.lvl                <= content.lvl      ;
                    lookup_ack_o.hit                <= tag_hit          ;
                    lookup_ack_o.prefetched         <= 1'b0             ;
                    lookup_ack_o.pte_position       <= content.lvl=='d0 ? lookup_req_i.addr[15:12] :
                                                       content.lvl=='d1 ? (hit_tag.sxl==1'b1 ? {1'b0, lookup_req_i.addr[24:22]} : {1'b0, lookup_req_i.addr[23:21]}) :
                                                       content.lvl=='d2 ? {1'b0, lookup_req_i.addr[32:30]} :
                                                       content.lvl=='d3 ? {1'b0, lookup_req_i.addr[41:39]} :
                                                       content.lvl=='d4 ? {1'b0, lookup_req_i.addr[50:48]} : 4'd0;
                    lookup_ack_o.N                  <= content.N        ;
                    lookup_ack_o.PBMT               <= content.PBMT     ;
                    lookup_ack_o.D                  <= content.D        ;
                    lookup_ack_o.A                  <= content.A        ;
                    lookup_ack_o.PPN                <= content.PPN      ;
                    lookup_ack_o.PERM               <= content.PERM     ;
                    lookup_ack_o.V                  <= content.V        ;
                end
            end
        end
    end
endgenerate
//}}}

//=== UPDATE & INV {{{
generate
    if(CACHE_TYPE==0) begin : s2ptc_inv_hit_gen
        iommu_atd_ptc_micro_tlb_inv_hit #(
        /*parameter  */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVVALID_REQ_TYPE,
        /*parameter  */ .SPARE_PARAM                (0                          )  // = 0,
        ) U_inv_hit(
        /*input  logic                                      */  .inv_req_valid_i            (inv_req_valid_i    ),
        /*input  INVALID_REQ_TYPE                           */  .inv_req_i                  (inv_req_i          ),
        /*input  logic                                      */  .hit_tag_gv                 (1'b1               ),
        /*input  logic [15:0]                               */  .hit_tag_gscid              (hit_tag.gscid      ),
        /*input  logic [19:0]                               */  .hit_tag_pscid              (20'd0              ),
        /*input  logic [63:12]                              */  .hit_tag_addr               (hit_tag.addr       ),
        /*input  logic                                      */  .hit_tag_sxl                (1'b0               ),
        /*input  logic                                      */  .csr_gxl                    (csr_fctl_gxl_i     ),
        /*input  logic [2:0]                                */  .content_lvl                (content.lvl        ),
        /*input  logic                                      */  .content_g                  (1'b0               ),
        /*input  logic                                      */  .content_leaf               (content.PERM[0] | content.PERM[2]),
        /*input  logic                                      */  .content_n                  (content.N          ),
        /*output logic                                      */  .inv_hit                    (inv_hit            ),
        /*input  logic                                      */  .spare_in                   (1'b0               )
        );
    end
    else begin : s1ptc_inv_hit_gen
        iommu_atd_ptc_micro_tlb_inv_hit #(
        /*parameter  */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVVALID_REQ_TYPE,
        /*parameter  */ .SPARE_PARAM                (0                          )  // = 0,
        ) U_inv_hit(
        /*input  logic                                      */  .inv_req_valid_i            (inv_req_valid_i    ),
        /*input  INVALID_REQ_TYPE                           */  .inv_req_i                  (inv_req_i          ),
        /*input  logic                                      */  .hit_tag_gv                 (hit_tag.gv         ),
        /*input  logic [15:0]                               */  .hit_tag_gscid              (hit_tag.gscid      ),
        /*input  logic [19:0]                               */  .hit_tag_pscid              (hit_tag.pscid      ),
        /*input  logic [63:12]                              */  .hit_tag_addr               (hit_tag.addr       ),
        /*input  logic                                      */  .hit_tag_sxl                (hit_tag.sxl        ),
        /*input  logic                                      */  .csr_gxl                    (csr_fctl_gxl_i     ),
        /*input  logic [2:0]                                */  .content_lvl                (content.lvl        ),
        /*input  logic                                      */  .content_n                  (content.N          ),
        /*input  logic                                      */  .content_g                  (content.PERM[4]    ),
        /*input  logic                                      */  .content_leaf               (content.PERM[0] | content.PERM[2]),
        /*output logic                                      */  .inv_hit                    (inv_hit            ),
        /*input  logic                                      */  .spare_in                   (1'b0               )
        );
    end
endgenerate

//===
generate
    if(CACHE_TYPE==0) begin : s2ptc_inv_upd_gen
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
                    hit_tag.prefetched      <= 1'b0                 ;
                    hit_tag.addr            <= update_req_i.addr    ;
                    hit_tag.gscid           <= update_req_i.gscid   ;
                    inv_tag.rsv             <= 'd0                  ;
                    content.lvl             <= update_req_i.lvl     ;
                    content.N               <= update_req_i.N       ;
                    content.PBMT            <= update_req_i.PBMT    ;
                    content.D               <= update_req_i.D       ;
                    content.A               <= update_req_i.A       ;
                    content.PPN             <= update_req_i.PPN     ;
                    content.PERM            <= update_req_i.PERM    ;
                    content.V               <= update_req_i.V       ;
                end
            end
        end
    end
    else begin : s1ptc_inv_upd_gen
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
                    hit_tag.prefetched      <= 1'b0                 ;
                    hit_tag.addr            <= update_req_i.addr    ;
                    hit_tag.gscid           <= update_req_i.gscid   ;
                    hit_tag.pscid           <= update_req_i.pscid   ;
                    hit_tag.sxl             <= update_req_i.sxl     ;
                    hit_tag.gv              <= update_req_i.gv      ;
                    inv_tag.rsv             <= 'd0                  ;
                    content.lvl             <= update_req_i.lvl     ;
                    content.N               <= update_req_i.N       ;
                    content.PBMT            <= update_req_i.PBMT    ;
                    content.D               <= update_req_i.D       ;
                    content.A               <= update_req_i.A       ;
                    content.PPN             <= update_req_i.PPN     ;
                    content.PERM            <= update_req_i.PERM    ;
                    content.V               <= update_req_i.V       ;
                end
            end
        end
    end
endgenerate
//}}}

//}}}

endmodule
//}}}




