module iommu_atd_dtc_mtlb_tag_hit #(
    parameter               CACHE_TYPE      = 0, // 0:DDTC, 1:PDTC
    parameter               BANK_TYPE       = 2'b00,
    parameter type          INVALID_REQ_TYPE= iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          BANK_REQ_TYPE   = iommu_atd_cache_pkg::ddtc_bank_req_t,
    parameter type          MTLB_TAG_TYPE   = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE  = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter               SPARE_PARAM     = 1'b0
)(
    input  BANK_REQ_TYPE                    req_i,
    input  MTLB_TAG_TYPE                    tag_i,
    input  MTLB_ITAG_TYPE                   itag_i,
    output logic                            tag_hit_o,
    output logic                            inv_hit_o,
    input  logic                            spare_in
);
//=== Declare {{{
    logic                                   req_is_lkp, req_is_upd, req_is_inv;
    logic [23:0]                            device_id_i;
    logic [19:0]                            process_id_i;
    logic                                   valid_i;
    logic [23:0]                            hit_tag_device_id;
    logic [19:0]                            hit_tag_process_id;
    logic                                   tag_hit;

//}}}

//=== MainCode {{{
    assign req_is_lkp       = req_i.typ==3'b001;
    assign req_is_upd       = req_i.typ==3'b010;
    assign req_is_inv       = req_i.typ==3'b100;

//=== VPN {{{
generate
    if(CACHE_TYPE==0) begin : ddtc_did_pid_got
        assign device_id_i      = req_is_lkp ? req_i.lkp.req.device_id        :
                                  req_is_upd ? req_i.upd.req.device_id        : 'd0;
        assign process_id_i     = 'd0;
    end
    else begin : pdtc_did_pid_got
        assign device_id_i      = req_is_lkp ? req_i.lkp.req.device_id        :
                                  req_is_upd ? req_i.upd.req.device_id        : 'd0;
        assign process_id_i     = req_is_lkp ? req_i.lkp.req.process_id       :
                                  req_is_upd ? req_i.upd.req.process_id       : 'd0;

    end
endgenerate
//}}}
//=== TAG {{{
    assign valid_i                  = tag_i.valid;
    assign hit_tag_device_id        = tag_i.device_id;
//}}}

//=== tag hit {{{
generate
    if(CACHE_TYPE==0) begin : ddtc_tag_hit_gen
        assign hit_tag_process_id = 'd0;
    end
    else begin
        assign hit_tag_process_id = tag_i.process_id;
    end
endgenerate
    iommu_atd_dtc_micro_tlb_tag_hit #(
    .CACHE_TYPE(CACHE_TYPE)
    ) U_tag_hit(
    /*input  logic [23:0]                           */  .device_id_i                (device_id_i                ),
    /*input  logic [19:0]                           */  .process_id_i               (process_id_i               ),
    /*input  logic                                  */  .valid_i                    (valid_i                    ),
    /*input  logic [23:0]                           */  .hit_tag_device_id          (hit_tag_device_id          ),
    /*input  logic [19:0]                           */  .hit_tag_process_id         (hit_tag_process_id         ),
    /*input  logic [1:0]                            */  .content_lvl                (2'(BANK_TYPE)              ),
    /*output logic                                  */  .tag_hit                    (tag_hit                    ) 
    );

//}}}

//=== inv hit {{{
    iommu_atd_dtc_micro_tlb_inv_hit #( //{{{
    .CACHE_TYPE(CACHE_TYPE),
    /*parameter type         */ .INVALID_REQ_TYPE       (INVALID_REQ_TYPE           ), // = logic,
    /*parameter */ .SPARE_PARAM            (0                          )  // = 0
    ) U_inv_hit(
    /*input  logic                                  */  .inv_req_valid_i            (1'b1                       ),
    /*input  INVALID_REQ_TYPE                       */  .inv_req_i                  (req_i.inv.req              ),
    /*input  logic [23:0]                           */  .hit_tag_device_id          (hit_tag_device_id          ),
    /*input  logic [19:0]                           */  .hit_tag_process_id         (hit_tag_process_id         ),
    /*input  logic [1:0]                            */  .content_lvl                (2'(BANK_TYPE)              ),
    /*output logic                                  */  .inv_hit                    (inv_hit                    ),
    /*input  logic                                  */  .spare_in                   (1'b0                       ) 
    );
//}}}

//}}}

    assign tag_hit_o = tag_hit & (req_is_lkp | req_is_upd);
    assign inv_hit_o = inv_hit & req_is_inv;

endmodule
