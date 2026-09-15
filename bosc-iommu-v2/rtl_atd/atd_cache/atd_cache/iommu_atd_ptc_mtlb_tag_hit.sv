module iommu_atd_ptc_mtlb_tag_hit #(
    parameter   CACHE_TYPE      = 0, // 0:S2PTC, 1:S1PTC
    parameter   BANK_TYPE       = 3'b000,
    parameter type          INVALID_REQ_TYPE= iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          BANK_REQ_TYPE   = iommu_atd_cache_pkg::s2ptc_bank_req_t,
    parameter type          MTLB_TAG_TYPE   = iommu_atd_cache_pkg::s2ptc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE  = iommu_atd_cache_pkg::s2ptc_mtlb_itag_t,
    parameter   SPARE_PARAM     = 1'b0
)(
    input  BANK_REQ_TYPE                    req_i,
    input  MTLB_TAG_TYPE                    tag_i,
    input  MTLB_ITAG_TYPE                   itag_i,
    output logic                            tag_hit_o,
    output logic                            inv_hit_o,
    input  logic                            csr_fctl_gxl,
    input  logic                            spare_in
);
//=== Declare {{{
    logic                                   req_is_lkp, req_is_upd, req_is_inv;
    logic                                   gv_i;
    logic [15:0]                            gscid_i;
    logic [19:0]                            pscid_i;
    logic [63:12]                           addr_i;
    logic                                   valid_i;
    logic                                   hit_tag_gv;
    logic [15:0]                            hit_tag_gscid;
    logic [19:0]                            hit_tag_pscid;
    logic [63:12]                           hit_tag_addr;
    logic                                   hit_tag_sxl;
    logic                                   hit_tag_g;
    logic                                   hit_tag_n;
    logic                                   hit_tag_leaf;
    logic                                   tag_hit;

//}}}

//=== MainCode {{{
    assign req_is_lkp       = req_i.typ==3'b001;
    assign req_is_upd       = req_i.typ==3'b010;
    assign req_is_inv       = req_i.typ==3'b100;

//=== VPN {{{
generate
    if(CACHE_TYPE==0) begin : s2ptc_did_pid_got
        assign gv_i         = 'd1;
        assign gscid_i      = req_is_lkp ? req_i.lkp.req.gscid      :
                              req_is_upd ? req_i.upd.req.gscid      : 'd0;
        assign pscid_i      = 'd0;
        assign addr_i       = req_is_lkp ? req_i.lkp.req.addr       :
                              req_is_upd ? req_i.upd.req.addr       : 'd0;
    end
    else begin : s1ptc_did_pid_got
        assign gv_i         = req_is_lkp ? req_i.lkp.req.gv         :
                              req_is_upd ? req_i.upd.req.gv         : 'd0;
        assign gscid_i      = req_is_lkp ? req_i.lkp.req.gscid      :
                              req_is_upd ? req_i.upd.req.gscid      : 'd0;
        assign pscid_i      = req_is_lkp ? req_i.lkp.req.pscid      :
                              req_is_upd ? req_i.upd.req.pscid      : 'd0;
        assign addr_i       = req_is_lkp ? req_i.lkp.req.addr       :
                              req_is_upd ? req_i.upd.req.addr       : 'd0;
    end
endgenerate
//}}}
//=== TAG {{{
    assign valid_i          = tag_i.valid;

generate
    if(CACHE_TYPE==0) begin : s2ptc_hit_tag_gv_gen
        assign hit_tag_gv   = 1'b1;
    end
    else begin : s1ptc_hit_tag_gv_gen
        assign hit_tag_gv   = tag_i.gv;
    end
endgenerate

    assign hit_tag_gscid    = tag_i.gscid;
    assign hit_tag_addr     = tag_i.addr;
    assign hit_tag_g        = tag_i.g;
    assign hit_tag_n        = tag_i.n;
    assign hit_tag_leaf     = tag_i.leaf;
//}}}

//=== tag hit {{{
generate
    if(CACHE_TYPE==0) begin : s2ptc_tag_hit_gen
        assign hit_tag_pscid= 'd0;
        assign hit_tag_sxl  = 'b0;
    end
    else begin : s1ptc_tag_hit_gen
        assign hit_tag_pscid= tag_i.pscid;
        assign hit_tag_sxl  = tag_i.sxl;
    end
endgenerate

    iommu_atd_ptc_micro_tlb_tag_hit #(
    .CACHE_TYPE(CACHE_TYPE)
    ) U_tag_hit(
    /*input  logic                                  */  .gv_i                   (gv_i                   ),
    /*input  logic [15:0]                           */  .gscid_i                (gscid_i                ),
    /*input  logic [19:0]                           */  .pscid_i                (pscid_i                ),
    /*input  logic [63:12]                          */  .addr_i                 (addr_i                 ),
    /*input  logic                                  */  .valid_i                (valid_i                ),
    /*input  logic                                  */  .hit_tag_gv             (hit_tag_gv             ),
    /*input  logic [15:0]                           */  .hit_tag_gscid          (hit_tag_gscid          ),
    /*input  logic [19:0]                           */  .hit_tag_pscid          (hit_tag_pscid          ),
    /*input  logic [63:12]                          */  .hit_tag_addr           (hit_tag_addr           ),
    /*input  logic                                  */  .hit_tag_sxl            (hit_tag_sxl            ),
    /*input  logic                                  */  .csr_gxl                (csr_fctl_gxl           ),
    /*input  logic [2:0]                            */  .content_lvl            (3'(BANK_TYPE)          ),
    /*input  logic                                  */  .content_n              (hit_tag_n              ),
    /*output logic                                  */  .tag_hit                (tag_hit                ) 
    );

//}}}

//=== inv hit {{{
    iommu_atd_ptc_micro_tlb_inv_hit #(
    .CACHE_TYPE(CACHE_TYPE),
    /*parameter type         */ .INVALID_REQ_TYPE       (INVALID_REQ_TYPE       ), // = logic,
    /*parameter  */ .SPARE_PARAM            (0                      )  // = 0
    ) U_inv_hit(
    /*input  logic                                  */  .inv_req_valid_i        (1'b1                   ),
    /*input  INVALID_REQ_TYPE                       */  .inv_req_i              (req_i.inv.req          ),
    /*input  logic                                  */  .hit_tag_gv             (hit_tag_gv             ),
    /*input  logic [15:0]                           */  .hit_tag_gscid          (hit_tag_gscid          ),
    /*input  logic [19:0]                           */  .hit_tag_pscid          (hit_tag_pscid          ),
    /*input  logic [63:12]                          */  .hit_tag_addr           (hit_tag_addr           ),
    /*input  logic                                  */  .hit_tag_sxl            (hit_tag_sxl            ),
    /*input  logic                                  */  .csr_gxl                (csr_fctl_gxl           ),
    /*input  logic [2:0]                            */  .content_lvl            (3'(BANK_TYPE)          ),
    /*input  logic                                  */  .content_g              (hit_tag_g              ),
    /*input  logic                                  */  .content_n              (hit_tag_n              ),
    /*input  logic                                  */  .content_leaf           (hit_tag_leaf           ),
    /*output logic                                  */  .inv_hit                (inv_hit                ),
    /*input  logic                                  */  .spare_in               (1'b0                   ) 
    );
//}}}

//}}}

    assign tag_hit_o = tag_hit & (req_is_lkp | req_is_upd);
    assign inv_hit_o = inv_hit & req_is_inv;

endmodule
