////////////////////////////////////////////////////////////////////////////
// iommu_atd_dtc_micro_tlb_inv_hit
////////////////////////////////////////////////////////////////////////////
module iommu_atd_dtc_micro_tlb_inv_hit #( //{{{
    parameter               CACHE_TYPE                  = 0, // 0:DDTC, 1:PDTC
    parameter type          INVALID_REQ_TYPE            = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter               SPARE_PARAM                 = 0
)(
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    input  logic [23:0]                                 hit_tag_device_id,
    input  logic [19:0]                                 hit_tag_process_id,
    input  logic [1:0]                                  content_lvl,
    output logic                                        inv_hit,
    input  logic                                        spare_in           
);
    logic                                               inval_ddt_i,inval_pdt_i;
    logic [23:0]                                        inv_device_id_i;
    logic                                               inv_dv_i;
    logic [19:0]                                        inv_process_id_i;
//    logic                                               device_id_inv_hit,process_id_inv_hit;

    assign inval_ddt_i      = inv_req_i.itype == iommu_atd_cache_pkg::INVTYPE_INVALID_DDT;
    assign inval_pdt_i      = inv_req_i.itype == iommu_atd_cache_pkg::INVTYPE_INVALID_PDT;
    assign inv_device_id_i  = inv_req_i.did_gscid;
    assign inv_dv_i         = inv_req_i.dv_gv;
    assign inv_process_id_i = inv_req_i.pid_pscid;

//    assign device_id_inv_hit  =  ~inv_dv_i 
//                               | (inv_device_id_i == hit_tag_device_id);
//    assign process_id_inv_hit = inv_process_id_i == hit_tag_process_id;

generate
    if(CACHE_TYPE==0) begin : ddtc_inv_hit_gen
        assign inv_hit      = inv_req_valid_i &                                             // if DV==0, invalid all DDT and PDT for all devices; if DV==1, invalid leaf level DDT identified by DID
                              inval_ddt_i &
                              (
                                 ~inv_dv_i |
                                 (inv_device_id_i==hit_tag_device_id & content_lvl=='d0)
                              );
    end
    else begin
        assign inv_hit      = inv_req_valid_i &
                              (
                                (inval_ddt_i &                                              // if DV==0, invalid all DDT and PDT for all devices; if DV==1, invalid leaf level DDT identified by DID
                                    (
                                     ~inv_dv_i |
                                     (inv_device_id_i==hit_tag_device_id)
                                    )
                                ) |
                                (inval_pdt_i &                                              // The command (INVAL_PDT) invalidates cached leaf PDT entry for the specified PID and DID
                                    (inv_device_id_i==hit_tag_device_id) &
                                    (inv_process_id_i==hit_tag_process_id) &
                                    content_lvl=='d0
                                )
                              );
    end
endgenerate



endmodule
//}}}




