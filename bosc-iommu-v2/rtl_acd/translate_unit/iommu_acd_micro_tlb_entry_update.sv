////////////////////////////////////////////////////////////////////////////
// iommu_acd_micro_tlb_entry_update
// check if all bits in valid_i below tag_i is 1 and tag_i's bit is 0
// for example
//     tag_i == 3
//     if(valid_i[2:0] == 3'b111 && valid_i[3] == 1'b0) update_o = 1;
//     else                                             update_o = 0;
// if all entry is occypid
//     check if the plru_idx is match with tag_i, if match, uppdate_o = 1
////////////////////////////////////////////////////////////////////////////
module iommu_acd_micro_tlb_entry_update #( //{{{
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


module iommu_acd_micro_tlb_entry_update_wrap #(
    parameter type          UPDATE_REQ_TYPE     = iommu_acd_pkg::update_req_t,
    parameter type          TAG_TYPE            = iommu_acd_pkg::microtlb_tag_t,
    parameter type          CONTENT_TYPE        = iommu_acd_pkg::microtlb_content_t,
    parameter type          INV_TAG_TYPE        = iommu_acd_pkg::microtlb_inv_tag_t,
    parameter   IDX_WIDTH           = 3,
    parameter   DEPTH               = 2**IDX_WIDTH
)(
    input  TAG_TYPE                             hit_tag_i           ,
    input  INV_TAG_TYPE                         inv_tag_i           ,
    input  CONTENT_TYPE                         content_i           ,
    input  logic                                update_req_valid_i  ,
    input  UPDATE_REQ_TYPE                      update_req_i        ,
    input  logic [DEPTH-1:0]                    valid_i             ,
    input  logic [IDX_WIDTH-1:0]                tag_i               ,
    input  logic [DEPTH-1:0]                    plru_idx_i          ,
    output logic                                update_o            ,
    input  logic                                csr_fctl_gxl_i      ,
    output logic                                update_req_hit_o    ,
    input  logic [DEPTH-1:0]                    update_req_hit_list_i,
    input  logic                                spare_in             
);
//=== Declare {{{
    logic                                       valid_o;

    logic [63:12]                               update_iova_i;
    logic [23:0]                                update_device_id_i;
    logic [19:0]                                update_process_id_i;
    logic                                       update_process_id_valid_i;
    logic                                       update_is_translated_i;

    logic                                       update_device_id_hit;
    logic                                       update_process_id_hit;
    logic                                       update_translated_hit;
    logic                                       update_va_hit;
    logic                                       update_vpn0_hit;
    logic                                       update_vpn1_hit;
    logic                                       update_vpn2_hit;
    logic                                       update_vpn3_hit;
    logic                                       update_vpn4_hit;
    logic                                       update_tag_hit;

    logic [21:12]                               update_vpn0_sv32;
    logic [31:22]                               update_vpn1_sv32;
    logic [21:12]                               update_vpn0_sv32x4;
    logic [33:22]                               update_vpn1_sv32x4;
    logic [33:12]                               update_ppn_sv32;

    logic [20:12]                               update_vpn0_sv39;
    logic [29:21]                               update_vpn1_sv39;
    logic [38:30]                               update_vpn2_sv39;
    logic [20:12]                               update_vpn0_sv39x4;
    logic [29:21]                               update_vpn1_sv39x4;
    logic [40:30]                               update_vpn2_sv39x4;    //
    logic [55:12]                               update_ppn_sv39;

    logic [20:12]                               update_vpn0_sv48;
    logic [29:21]                               update_vpn1_sv48;
    logic [38:30]                               update_vpn2_sv48;
    logic [47:39]                               update_vpn3_sv48;
    logic [20:12]                               update_vpn0_sv48x4;
    logic [29:21]                               update_vpn1_sv48x4;
    logic [38:30]                               update_vpn2_sv48x4;
    logic [49:39]                               update_vpn3_sv48x4;    //
    logic [55:12]                               update_ppn_sv48;

    logic [20:12]                               update_vpn0_sv57;
    logic [29:21]                               update_vpn1_sv57;
    logic [38:30]                               update_vpn2_sv57;
    logic [47:39]                               update_vpn3_sv57;
    logic [56:48]                               update_vpn4_sv57;
    logic [20:12]                               update_vpn0_sv57x4;
    logic [29:21]                               update_vpn1_sv57x4;
    logic [38:30]                               update_vpn2_sv57x4;
    logic [47:39]                               update_vpn3_sv57x4;
    logic [58:48]                               update_vpn4_sv57x4;
    logic [55:12]                               update_ppn_sv57;

    logic [63:57]                               update_vpn5_sv;
    logic [63:59]                               update_vpn5_svx4;

    logic                                       s1_bare;
    logic                                       s1_sv32;
    logic                                       s1_sv39;
    logic                                       s1_sv48;
    logic                                       s1_sv57;
    logic                                       s2_bare;
    logic                                       s2_sv32x4;
    logic                                       s2_sv39x4;
    logic                                       s2_sv48x4;
    logic                                       s2_sv57x4;
    logic                                       s1_2m;
    logic                                       s2_2m;
    logic                                       is_2m;
    logic                                       s1_1g;
    logic                                       s2_1g;
    logic                                       is_1g;
    logic                                       s1_512g;
    logic                                       s2_512g;
    logic                                       is_512g;
//}}}

//=== Main Code {{{
    assign valid_o                  = valid_i[tag_i];


    assign update_device_id_i       = update_req_i.device_id;
    assign update_process_id_i      = update_req_i.process_id;
    assign update_process_id_valid_i= update_req_i.process_id_valid;
    assign update_is_translated_i   = update_req_i.is_translated;

    assign update_iova_i            = update_req_i.va;
    assign update_vpn0_sv32         = update_iova_i[21:12];
    assign update_vpn1_sv32         = update_iova_i[31:22];
    assign update_vpn0_sv32x4       = update_iova_i[21:12];
    assign update_vpn1_sv32x4       = update_iova_i[33:22];
    assign update_vpn0_sv39         = update_iova_i[20:12];
    assign update_vpn1_sv39         = update_iova_i[29:21];
    assign update_vpn2_sv39         = update_iova_i[38:30];
    assign update_vpn0_sv39x4       = update_iova_i[20:12];
    assign update_vpn1_sv39x4       = update_iova_i[29:21];
    assign update_vpn2_sv39x4       = update_iova_i[40:30];
    assign update_vpn0_sv48         = update_iova_i[20:12];
    assign update_vpn1_sv48         = update_iova_i[29:21];
    assign update_vpn2_sv48         = update_iova_i[38:30];
    assign update_vpn3_sv48         = update_iova_i[47:39];
    assign update_vpn0_sv48x4       = update_iova_i[20:12];
    assign update_vpn1_sv48x4       = update_iova_i[29:21];
    assign update_vpn2_sv48x4       = update_iova_i[38:30];
    assign update_vpn3_sv48x4       = update_iova_i[49:39];
    assign update_vpn0_sv57         = update_iova_i[20:12];
    assign update_vpn1_sv57         = update_iova_i[29:21];
    assign update_vpn2_sv57         = update_iova_i[38:30];
    assign update_vpn3_sv57         = update_iova_i[47:39];
    assign update_vpn4_sv57         = update_iova_i[56:48];
    assign update_vpn0_sv57x4       = update_iova_i[20:12];
    assign update_vpn1_sv57x4       = update_iova_i[29:21];
    assign update_vpn2_sv57x4       = update_iova_i[38:30];
    assign update_vpn3_sv57x4       = update_iova_i[47:39];
    assign update_vpn4_sv57x4       = update_iova_i[58:48];
    assign update_vpn5_sv           = update_iova_i[63:57];
    assign update_vpn5_svx4         = update_iova_i[63:59];

    assign s1_bare                  = content_i.S1MODE=='d0;
    assign s1_sv32                  = content_i.SXL ? content_i.S1MODE=='d8 : 1'b0;
    assign s1_sv39                  = content_i.SXL ? 1'b0                : content_i.S1MODE=='d8;
    assign s1_sv48                  = content_i.SXL ? 1'b0                : content_i.S1MODE=='d9;
    assign s1_sv57                  = content_i.SXL ? 1'b0                : content_i.S1MODE=='d10;
    assign s2_bare                  = content_i.S2MODE=='d0;
    assign s2_sv32x4                = csr_fctl_gxl_i ? content_i.S2MODE=='d8 : 1'b0;
    assign s2_sv39x4                = csr_fctl_gxl_i ? 1'b0                : content_i.S2MODE=='d8;
    assign s2_sv48x4                = csr_fctl_gxl_i ? 1'b0                : content_i.S2MODE=='d9;
    assign s2_sv57x4                = csr_fctl_gxl_i ? 1'b0                : content_i.S2MODE=='d10;
    assign s1_2m                    = content_i.S1SIZE == 'b01;
    assign s2_2m                    = content_i.S2SIZE == 'b01;
    assign is_2m                    = (~s1_bare & ~s2_bare) ? ((s2_2m & (s1_2m | s1_1g | s1_512g)) | (s1_2m & (s2_2m | s2_1g | s2_512g))) :
                                                              ((s2_2m & ~s2_bare) | (s1_2m & ~s1_bare));
    assign s1_1g                    = content_i.S1SIZE == 'b10;
    assign s2_1g                    = content_i.S2SIZE == 'b10;
    assign is_1g                    = (~s1_bare & ~s2_bare) ? ((s2_1g & (s1_1g | s1_512g)) | (s1_1g & (s2_1g | s2_512g))) :
                                                              ((s2_1g & ~s2_bare) | (s1_1g & ~s1_bare));
    assign s1_512g                  = content_i.S1SIZE == 'b11;
    assign s2_512g                  = content_i.S2SIZE == 'b11;
    assign is_512g                  = (~s1_bare & ~s2_bare) ? (s2_512g & s1_512g) :
                                                              ((s2_512g & ~s2_bare) | (s1_512g & ~s1_bare));

// tag_hit logic fix, 20241114
//    assign update_device_id_hit     = ~valid_o ? 1'b0 : (update_device_id_i     == hit_tag_i.device_id);
//    assign update_process_id_hit    = ~valid_o ? 1'b0 : (update_process_id_i    == hit_tag_i.process_id) & (update_process_id_valid_i == hit_tag_i.process_id_valid);
//    assign update_translated_hit    = ~valid_o ? 1'b0 : (update_is_translated_i == hit_tag_i.is_translated);
//    
//    assign update_vpn0_hit          = ~valid_o ? 1'b0 :
//                                      ~s1_bare ? ((s1_sv32   & (update_vpn0_sv32   == hit_tag_i.va[21:12]))  | (update_vpn0_sv39   == hit_tag_i.va[20:12])) :
//                                      ~s2_bare ? ((s2_sv32x4 & (update_vpn0_sv32x4 == hit_tag_i.va[21:12]))  | (update_vpn0_sv39x4 == hit_tag_i.va[20:12])) :
//                                      1'b0;
//    assign update_vpn1_hit          = ~valid_o ? 1'b0 :
//                                      ~s1_bare ? ((s1_sv32   & (update_vpn1_sv32   == hit_tag_i.va[33:22]))  | (update_vpn1_sv39   == hit_tag_i.va[29:21])) :
//                                      ~s2_bare ? ((s2_sv32x4 & (update_vpn1_sv32x4 == hit_tag_i.va[33:22]))  | (update_vpn1_sv39x4 == hit_tag_i.va[29:21])) :
//                                      1'b0;
//    assign update_vpn2_hit          = ~valid_o ? 1'b0 :
//                                      ~s1_bare ? ( s1_sv32                                          | (update_vpn2_sv39   == hit_tag_i.va[38:30])) :
//                                      ~s2_bare ? ( s2_sv32x4                                        | (s2_sv39x4 ? (update_vpn2_sv39x4 == hit_tag_i.va[40:30]) : (update_vpn2_sv48x4 == hit_tag_i.va[38:30]))) :
//                                      1'b0;
//    assign update_vpn3_hit          = ~valid_o ? 1'b0 :
//                                      ~s1_bare ? ((s1_sv32   | s1_sv39)                             | (update_vpn3_sv48   == hit_tag_i.va[47:39])) :
//                                      ~s2_bare ? ((s2_sv32x4 | s2_sv39x4)                           | (s2_sv48x4 ? (update_vpn3_sv48x4 == hit_tag_i.va[49:39]) : (update_vpn3_sv57x4 == hit_tag_i.va[47:39]))) :
//                                      1'b0;
//    assign update_vpn4_hit          = ~valid_o ? 1'b0 :
//                                      ~s1_bare ? ((s1_sv32   | s1_sv39 | s1_sv48)                   | (update_vpn4_sv57   == hit_tag_i.va[56:48])) :
//                                      ~s2_bare ? ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)               | (update_vpn4_sv57x4 == hit_tag_i.va[58:48])) :
//                                      1'b0;
//    assign update_va_hit            = (is_2m | update_vpn0_hit) & (is_1g | update_vpn1_hit) & (is_512g | update_vpn2_hit) & update_vpn3_hit & update_vpn4_hit;
//    assign update_tag_hit           = update_va_hit & update_translated_hit & update_process_id_hit & update_device_id_hit;
    iommu_acd_micro_tlb_tag_hit #(
    ) U_update_tag_hit(
    /*input  logic[21:12]               */  .vpn0_sv32                  (update_vpn0_sv32           ),
    /*input  logic[31:22]               */  .vpn1_sv32                  (update_vpn1_sv32           ),
    /*input  logic[21:12]               */  .vpn0_sv32x4                (update_vpn0_sv32x4         ),
    /*input  logic[33:22]               */  .vpn1_sv32x4                (update_vpn1_sv32x4         ),
    /*input  logic[20:12]               */  .vpn0_sv39                  (update_vpn0_sv39           ),
    /*input  logic[29:21]               */  .vpn1_sv39                  (update_vpn1_sv39           ),
    /*input  logic[38:30]               */  .vpn2_sv39                  (update_vpn2_sv39           ),
    /*input  logic[20:12]               */  .vpn0_sv39x4                (update_vpn0_sv39x4         ),
    /*input  logic[29:21]               */  .vpn1_sv39x4                (update_vpn1_sv39x4         ),
    /*input  logic[40:30]               */  .vpn2_sv39x4                (update_vpn2_sv39x4         ),
    /*input  logic[20:12]               */  .vpn0_sv48                  (update_vpn0_sv48           ),
    /*input  logic[29:21]               */  .vpn1_sv48                  (update_vpn1_sv48           ),
    /*input  logic[38:30]               */  .vpn2_sv48                  (update_vpn2_sv48           ),
    /*input  logic[47:39]               */  .vpn3_sv48                  (update_vpn3_sv48           ),
    /*input  logic[20:12]               */  .vpn0_sv48x4                (update_vpn0_sv48x4         ),
    /*input  logic[29:21]               */  .vpn1_sv48x4                (update_vpn1_sv48x4         ),
    /*input  logic[38:30]               */  .vpn2_sv48x4                (update_vpn2_sv48x4         ),
    /*input  logic[49:39]               */  .vpn3_sv48x4                (update_vpn3_sv48x4         ),
    /*input  logic[20:12]               */  .vpn0_sv57                  (update_vpn0_sv57           ),
    /*input  logic[29:21]               */  .vpn1_sv57                  (update_vpn1_sv57           ),
    /*input  logic[38:30]               */  .vpn2_sv57                  (update_vpn2_sv57           ),
    /*input  logic[47:39]               */  .vpn3_sv57                  (update_vpn3_sv57           ),
    /*input  logic[56:48]               */  .vpn4_sv57                  (update_vpn4_sv57           ),
    /*input  logic[20:12]               */  .vpn0_sv57x4                (update_vpn0_sv57x4         ),
    /*input  logic[29:21]               */  .vpn1_sv57x4                (update_vpn1_sv57x4         ),
    /*input  logic[38:30]               */  .vpn2_sv57x4                (update_vpn2_sv57x4         ),
    /*input  logic[47:39]               */  .vpn3_sv57x4                (update_vpn3_sv57x4         ),
    /*input  logic[58:48]               */  .vpn4_sv57x4                (update_vpn4_sv57x4         ),
    /*input  logic[63:57]               */  .vpn5_sv                    (update_vpn5_sv             ),
    /*input  logic[63:59]               */  .vpn5_svx4                  (update_vpn5_svx4           ),
    /*input  logic [23:0]               */  .device_id_i                (update_device_id_i         ),
    /*input  logic                      */  .process_id_valid_i         (update_process_id_valid_i  ),
    /*input  logic [19:0]               */  .process_id_i               (update_process_id_i        ),
    /*input  logic                      */  .is_translated_i            (update_is_translated_i     ),
    /*input  logic                      */  .valid_i                    (valid_o                    ),
    /*input  logic [23:0]               */  .hit_tag_device_id          (hit_tag_i.device_id        ),
    /*input  logic                      */  .hit_tag_process_id_valid   (hit_tag_i.process_id_valid ),
    /*input  logic [19:0]               */  .hit_tag_process_id         (hit_tag_i.process_id       ),
    /*input  logic                      */  .hit_tag_is_translated      (hit_tag_i.is_translated    ),
    /*input  logic [63:12]              */  .hit_tag_va                 (hit_tag_i.va               ),
    /*input  logic                      */  .content_n                  (content_i.N                ),
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
    /*output logic                      */  .tag_hit                    (update_tag_hit             ) 
    );
    assign update_req_hit_o         = update_tag_hit;
//}}}


iommu_acd_micro_tlb_entry_update #(
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



