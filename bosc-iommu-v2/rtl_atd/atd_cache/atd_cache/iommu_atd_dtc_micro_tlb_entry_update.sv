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
module iommu_atd_dtc_micro_tlb_entry_update #( //{{{
    parameter  IDX_WIDTH       = 3,
    parameter  DEPTH           = 2**IDX_WIDTH
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


module iommu_atd_dtc_micro_tlb_entry_update_wrap #(
    parameter               CACHE_TYPE          = 0, // 0:DDTC, 1:PDTC
    parameter type          UPDATE_REQ_TYPE     = logic,
    parameter type          TAG_TYPE            = logic,
    parameter type          CONTENT_TYPE        = logic,
    parameter type          INV_TAG_TYPE        = logic,
    parameter               IDX_WIDTH           = 3,
    parameter               DEPTH               = 2**IDX_WIDTH
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
    input  logic                                spare_in             
);
//=== Declare {{{
    logic                                       valid_o;
    logic [23:0]                                update_device_id_i;
    logic [19:0]                                update_process_id_i;
    logic                                       update_tag_hit;
//}}}

//=== Main Code {{{
    assign valid_o                  = valid_i[tag_i];
    
    assign update_device_id_i       = update_req_i.device_id;

generate
    if(CACHE_TYPE==0) begin : ddtc_tag_hit_gen
        assign update_process_id_i = 'd0;
        iommu_atd_dtc_micro_tlb_tag_hit #(
        /*parameter */ .CACHE_TYPE (CACHE_TYPE                 )  // = 0, // 0:DDTC, 1:PDTC
        ) U_update_tag_hit(
        /*input  logic [23:0]               */  .device_id_i                (update_device_id_i         ),
        /*input  logic [19:0]               */  .process_id_i               (20'd0                      ),
        /*input  logic                      */  .valid_i                    (valid_o                    ),
        /*input  logic [23:0]               */  .hit_tag_device_id          (hit_tag_i.device_id        ),
        /*input  logic [19:0]               */  .hit_tag_process_id         (20'd0                      ),
        /*input  logic [1:0]                */  .content_lvl                (content_i.lvl              ),
        /*output logic                      */  .tag_hit                    (update_tag_hit             ) 
        );
        assign update_req_hit_o         = update_tag_hit;
    end
    else begin : pdtc_tag_hit_gen
        assign update_process_id_i = update_req_i.process_id;
        iommu_atd_dtc_micro_tlb_tag_hit #(
        /*parameter */ .CACHE_TYPE (CACHE_TYPE                 )  // = 0, // 0:DDTC, 1:PDTC
        ) U_update_tag_hit(
        /*input  logic [23:0]               */  .device_id_i                (update_device_id_i         ),
        /*input  logic [19:0]               */  .process_id_i               (update_process_id_i        ),
        /*input  logic                      */  .valid_i                    (valid_o                    ),
        /*input  logic [23:0]               */  .hit_tag_device_id          (hit_tag_i.device_id        ),
        /*input  logic [19:0]               */  .hit_tag_process_id         (hit_tag_i.process_id       ),
        /*input  logic [1:0]                */  .content_lvl                (content_i.lvl              ),
        /*output logic                      */  .tag_hit                    (update_tag_hit             ) 
        );
        assign update_req_hit_o         = update_tag_hit;
    end
endgenerate
//}}}


iommu_atd_dtc_micro_tlb_entry_update #(
    /*parameter */ .IDX_WIDTH      (IDX_WIDTH)  // = 3,
) U_update(
    /*input  logic [DEPTH-1:0]               */ .valid_i        (valid_i                                        ),
    /*input  logic                           */ .update_i       (update_req_valid_i & ~(|update_req_hit_list_i) ),
    /*input  logic [IDX_WIDTH-1:0]           */ .tag_i          (tag_i                                          ),
    /*input  logic [DEPTH-1:0]               */ .plru_idx_i     (plru_idx_i                                     ),
    /*output logic                           */ .update_o       (update_o                                       ) 
);

endmodule
//}}}



