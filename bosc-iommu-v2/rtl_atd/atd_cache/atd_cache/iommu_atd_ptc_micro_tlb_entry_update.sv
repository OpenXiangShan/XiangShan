////////////////////////////////////////////////////////////////////////////
// iommu_atd_dtc_micro_tlb_entry_update
// check if all bits in valid_i below tag_i is 1 and tag_i's bit is 0
// for example
//     tag_i == 3
//     if(valid_i[2:0] == 3'b111 && valid_i[3] == 1'b0) update_o = 1;
//     else                                             update_o = 0;
// if all entry is occypid
//     check if the plru_idx is match with tag_i, if match, uppdate_o = 1
////////////////////////////////////////////////////////////////////////////
module iommu_atd_ptc_micro_tlb_entry_update #( //{{{
    parameter   IDX_WIDTH       = 3,
    parameter   DEPTH           = 2**IDX_WIDTH
)(
    input  logic [DEPTH-1:0]                valid_i,
    input  logic                            update_i,
    input  logic [IDX_WIDTH-1:0]            tag_i,
    input  logic [DEPTH-1:0]                plru_idx_i,
    output logic                            update_o
);
    logic             all_valid;
    logic [DEPTH-1:0] tmp_vec;

    assign all_valid = &valid_i;

    always@(*) begin
        for(int unsigned i = 0; i < DEPTH; i++) begin
            if(i < tag_i)
                tmp_vec[i] = valid_i[i];
            else
                tmp_vec[i] = 1'b1;
        end
    end

    assign update_o = (all_valid ? plru_idx_i[tag_i] :                      // when all entry occpuied, update indicated by plru
                                   ((&tmp_vec) & (~valid_i[tag_i]))         // otherwise, select an empty entry
                       ) & update_i;

endmodule


module iommu_atd_ptc_micro_tlb_entry_update_wrap #(
    parameter   CACHE_TYPE          = 0, // 0:S2PTC, 1:S1PTC
    parameter type          UPDATE_REQ_TYPE     = logic,
    parameter type          TAG_TYPE            = logic,
    parameter type          CONTENT_TYPE        = logic,
    parameter type          INV_TAG_TYPE        = logic,
    parameter   IDX_WIDTH           = 3,
    parameter   DEPTH               = 2**IDX_WIDTH
)(
    input  TAG_TYPE                             hit_tag_i,
    input  INV_TAG_TYPE                         inv_tag_i,
    input  CONTENT_TYPE                         content_i,
    input  logic                                update_req_valid_i,
    input  UPDATE_REQ_TYPE                      update_req_i,
    input  logic [DEPTH-1:0]                    valid_i,
    input  logic [IDX_WIDTH-1:0]                tag_i,
    input  logic [DEPTH-1:0]                    plru_idx_i,
    output logic                                update_o,
    output logic                                update_req_hit_o,
    input  logic [DEPTH-1:0]                    update_req_hit_list_i,
    input  logic                                csr_fctl_gxl_i,
    input  logic                                spare_in             
);
//=== Declare {{{
    logic                                       valid_o;
    logic                                       update_gv_i;
    logic [15:0]                                update_gscid_i;
    logic [19:0]                                update_pscid_i;
    logic [63:12]                               update_addr_i;
    logic                                       update_tag_hit;
//}}}

//=== Main Code {{{
    assign valid_o          = valid_i[tag_i];
    
generate
    if(CACHE_TYPE==0) begin : s2ptc_gv_i_gen
        assign update_gv_i      = 1'b1;
    end
    else begin : s1ptc_gv_i_gen
        assign update_gv_i      = update_req_i.gv;
    end
endgenerate

    assign update_gscid_i   = update_req_i.gscid;
    assign update_addr_i    = update_req_i.addr;

generate
    if(CACHE_TYPE==0) begin : s2ptc_tag_hit_gen
        assign update_pscid_i = 'd0;
        iommu_atd_ptc_micro_tlb_tag_hit #(
        /*parameter  */ .CACHE_TYPE (CACHE_TYPE             )  // = 0, // 0:S2PTC, 1:S1PTC
        ) U_update_tag_hit(
        /*input  logic                      */  .gv_i                   (update_gv_i                ),
        /*input  logic [23:0]               */  .gscid_i                (update_gscid_i             ),
        /*input  logic [19:0]               */  .pscid_i                (update_pscid_i             ),
        /*input  logic [63:12]              */  .addr_i                 (update_addr_i              ),
        /*input  logic                      */  .valid_i                (valid_o                    ),
        /*input  logic                      */  .hit_tag_gv             (1'b1                       ),
        /*input  logic [15:0]               */  .hit_tag_gscid          (hit_tag_i.gscid            ),
        /*input  logic [19:0]               */  .hit_tag_pscid          (20'd0                      ),
        /*input  logic [63:12]              */  .hit_tag_addr           (hit_tag_i.addr             ),
        /*input  logic                      */  .hit_tag_sxl            (1'b0                       ),
        /*input  logic                      */  .csr_gxl                (csr_fctl_gxl_i             ),
        /*input  logic [1:0]                */  .content_lvl            (content_i.lvl              ),
        /*input  logic                      */  .content_n              (content_i.N                ),
        /*output logic                      */  .tag_hit                (update_tag_hit             ) 
        );
        assign update_req_hit_o         = update_tag_hit;
    end
    else begin : s1ptc_tag_hit_gen
        assign update_pscid_i = update_req_i.pscid;
        iommu_atd_ptc_micro_tlb_tag_hit #(
        /*parameter  */ .CACHE_TYPE (CACHE_TYPE                 )  // = 0, // 0:S2PTC, 1:S1PTC
        ) U_update_tag_hit(
        /*input  logic                      */  .gv_i                   (update_gv_i                ),
        /*input  logic [23:0]               */  .gscid_i                (update_gscid_i             ),
        /*input  logic [19:0]               */  .pscid_i                (update_pscid_i             ),
        /*input  logic [63:12]              */  .addr_i                 (update_addr_i              ),
        /*input  logic                      */  .valid_i                (valid_o                    ),
        /*input  logic                      */  .hit_tag_gv             (hit_tag_i.gv               ),
        /*input  logic [15:0]               */  .hit_tag_gscid          (hit_tag_i.gscid            ),
        /*input  logic [19:0]               */  .hit_tag_pscid          (hit_tag_i.pscid            ),
        /*input  logic [63:12]              */  .hit_tag_addr           (hit_tag_i.addr             ),
        /*input  logic                      */  .hit_tag_sxl            (hit_tag_i.sxl              ),
        /*input  logic                      */  .csr_gxl                (csr_fctl_gxl_i             ),
        /*input  logic [1:0]                */  .content_lvl            (content_i.lvl              ),
        /*input  logic                      */  .content_n              (content_i.N                ),
        /*output logic                      */  .tag_hit                (update_tag_hit             ) 
        );
        assign update_req_hit_o         = update_tag_hit;
    end
endgenerate
//}}}


iommu_atd_dtc_micro_tlb_entry_update #(
    /*parameter  */ .IDX_WIDTH      (IDX_WIDTH)  // = 3,
) U_update(
    /*input  logic [DEPTH-1:0]               */ .valid_i        (valid_i                                        ),
    /*input  logic                           */ .update_i       (update_req_valid_i & ~(|update_req_hit_list_i) ),
    /*input  logic [IDX_WIDTH-1:0]           */ .tag_i          (tag_i                                          ),
    /*input  logic [DEPTH-1:0]               */ .plru_idx_i     (plru_idx_i                                     ),
    /*output logic                           */ .update_o       (update_o                                       ) 
);

endmodule
//}}}



