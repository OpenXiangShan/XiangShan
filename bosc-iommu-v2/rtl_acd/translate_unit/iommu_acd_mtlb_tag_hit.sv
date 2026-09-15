module iommu_acd_mtlb_tag_hit #(
    parameter   BANK_TYPE       = 2'b00,
    parameter type          BANK_REQ_TYPE   = iommu_acd_pkg::bank_req_t,
    parameter type          MTLB_TAG_TYPE   = iommu_acd_pkg::mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE  = iommu_acd_pkg::mtlb_itag_t,
    parameter   SPARE_PARAM     = 1'b0
)(
    input  BANK_REQ_TYPE                    req_i,
    input  MTLB_TAG_TYPE                    tag_i,
    input  MTLB_ITAG_TYPE                   itag_i,
    input  logic                            csr_fctl_gxl_i,
    output logic                            tag_hit_o,
    output logic                            inv_hit_o,
    input  logic                            spare_in
);
//=== Declare {{{
    logic                                   req_is_lkp, req_is_upd, req_is_inv;

    logic [63:12]                           iova_i;
    logic [23:0]                            device_id_i;
    logic [19:0]                            process_id_i;
    logic                                   process_id_valid_i;
    logic                                   is_translated_i;

    logic[21:12]                            vpn0_sv32;
    logic[31:22]                            vpn1_sv32;
    logic[21:12]                            vpn0_sv32x4;
    logic[33:22]                            vpn1_sv32x4;
    logic[20:12]                            vpn0_sv39;
    logic[29:21]                            vpn1_sv39;
    logic[38:30]                            vpn2_sv39;
    logic[20:12]                            vpn0_sv39x4;
    logic[29:21]                            vpn1_sv39x4;
    logic[40:30]                            vpn2_sv39x4;
    logic[20:12]                            vpn0_sv48;
    logic[29:21]                            vpn1_sv48;
    logic[38:30]                            vpn2_sv48;
    logic[47:39]                            vpn3_sv48;
    logic[20:12]                            vpn0_sv48x4;
    logic[29:21]                            vpn1_sv48x4;
    logic[38:30]                            vpn2_sv48x4;
    logic[49:39]                            vpn3_sv48x4;
    logic[20:12]                            vpn0_sv57;
    logic[29:21]                            vpn1_sv57;
    logic[38:30]                            vpn2_sv57;
    logic[47:39]                            vpn3_sv57;
    logic[56:48]                            vpn4_sv57;
    logic[20:12]                            vpn0_sv57x4;
    logic[29:21]                            vpn1_sv57x4;
    logic[38:30]                            vpn2_sv57x4;
    logic[47:39]                            vpn3_sv57x4;
    logic[58:48]                            vpn4_sv57x4;
    logic[63:57]                            vpn5_sv;
    logic[63:59]                            vpn5_svx4;

    logic                                   valid_i;
    logic [23:0]                            hit_tag_device_id;
    logic                                   hit_tag_process_id_valid;
    logic [19:0]                            hit_tag_process_id;
    logic                                   hit_tag_is_translated;
    logic [63:12]                           hit_tag_va;
    logic                                   content_n;

    logic                                   s1_bare;
    logic                                   s1_sv32;
    logic                                   s1_sv39;
    logic                                   s1_sv48;
    logic                                   s2_bare;
    logic                                   s2_sv32x4;
    logic                                   s2_sv39x4;
    logic                                   s2_sv48x4;

    logic                                   s1_2m, s1_1g, s1_512g, s2_2m, s2_1g, s2_512g;
    logic                                   is_2m;
    logic                                   is_ig;
    logic                                   is_512g;

    logic                                   tag_hit;

    logic device_id_hit, process_id_hit, translated_hit, vpn0_hit, vpn1_hit, vpn2_hit, vpn3_hit, vpn4_hit, va_hit;

    logic                                   inv_req_valid_i;

    logic [15:0]                            inv_tag_GSCID;
    logic [19:0]                            inv_tag_PSCID;
    logic [61:12]                           inv_tag_GPPN;
    logic                                   content_G;

    logic [63:12]                           inv_tag_gpa;
    logic [63:12]                           inv_tag_va;
    logic                                   inv_vpn0_hit;
    logic                                   inv_vpn1_hit;
    logic                                   inv_vpn2_hit;
    logic                                   inv_vpn3_hit;
    logic                                   inv_vpn4_hit;
    logic                                   addr_inv_hit;
    logic                                   inv_hit;

    logic [63:12]                           inv_addr_i     ;
    logic [21:12]                           inv_vpn0_sv32  ;
    logic [31:22]                           inv_vpn1_sv32  ;
    logic [21:12]                           inv_vpn0_sv32x4;
    logic [33:22]                           inv_vpn1_sv32x4;
    logic [20:12]                           inv_vpn0_sv39  ;
    logic [29:21]                           inv_vpn1_sv39  ;
    logic [38:30]                           inv_vpn2_sv39  ;
    logic [20:12]                           inv_vpn0_sv39x4;
    logic [29:21]                           inv_vpn1_sv39x4;
    logic [40:30]                           inv_vpn2_sv39x4;
    logic [20:12]                           inv_vpn0_sv48  ;
    logic [29:21]                           inv_vpn1_sv48  ;
    logic [38:30]                           inv_vpn2_sv48  ;
    logic [47:39]                           inv_vpn3_sv48  ;
    logic [20:12]                           inv_vpn0_sv48x4;
    logic [29:21]                           inv_vpn1_sv48x4;
    logic [38:30]                           inv_vpn2_sv48x4;
    logic [49:39]                           inv_vpn3_sv48x4;
    logic [20:12]                           inv_vpn0_sv57  ;
    logic [29:21]                           inv_vpn1_sv57  ;
    logic [38:30]                           inv_vpn2_sv57  ;
    logic [47:39]                           inv_vpn3_sv57  ;
    logic [56:48]                           inv_vpn4_sv57  ;
    logic [20:12]                           inv_vpn0_sv57x4;
    logic [29:21]                           inv_vpn1_sv57x4;
    logic [38:30]                           inv_vpn2_sv57x4;
    logic [47:39]                           inv_vpn3_sv57x4;
    logic [58:48]                           inv_vpn4_sv57x4;
    
    logic                                   inval_ddt_i       ;
    logic                                   inval_pdt_i       ;
    logic [19:0]                            inv_process_id_i  ;
    logic [23:0]                            inv_device_id_i   ;
    logic                                   inv_dv_i          ;
    logic                                   device_id_inv_hit ;
    logic                                   process_id_inv_hit;
    logic                                   vma_i             ;
    logic                                   gvma_i            ;
    logic                                   inv_av_i          ;
    logic [19:0]                            inv_pscid_i       ;
    logic                                   inv_pscv_i        ;
    logic [15:0]                            inv_gscid_i       ;
    logic                                   inv_gv_i          ;
    logic                                   gscid_inv_hit     ;
    logic                                   pscid_inv_hit     ;

//}}}

//=== MainCode {{{
    assign req_is_lkp       = req_i.typ==3'b001;
    assign req_is_upd       = req_i.typ==3'b010;
    assign req_is_inv       = req_i.typ==3'b100;

//=== VPN {{{
    assign device_id_i        = req_is_lkp ? req_i.lkp.req.device_id        :
                                req_is_upd ? req_i.upd.req.device_id        : 'd0;
    assign process_id_i       = req_is_lkp ? req_i.lkp.req.process_id       :
                                req_is_upd ? req_i.upd.req.process_id       : 'd0;
    assign process_id_valid_i = req_is_lkp ? req_i.lkp.req.process_id_valid :
                                req_is_upd ? req_i.upd.req.process_id_valid : 'd0;
    assign is_translated_i    = req_is_lkp ? req_i.lkp.req.is_translated    :
                                req_is_upd ? req_i.upd.req.is_translated    : 'd0;
    assign iova_i             = req_is_lkp ? req_i.lkp.req.va               :
                                req_is_upd ? req_i.upd.req.va               : 'd0;

    assign vpn0_sv32   = iova_i[21:12];
    assign vpn1_sv32   = iova_i[31:22];
    assign vpn0_sv32x4 = iova_i[21:12];
    assign vpn1_sv32x4 = iova_i[33:22];
    assign vpn0_sv39   = iova_i[20:12];
    assign vpn1_sv39   = iova_i[29:21];
    assign vpn2_sv39   = iova_i[38:30];
    assign vpn0_sv39x4 = iova_i[20:12];
    assign vpn1_sv39x4 = iova_i[29:21];
    assign vpn2_sv39x4 = iova_i[40:30];
    assign vpn0_sv48   = iova_i[20:12];
    assign vpn1_sv48   = iova_i[29:21];
    assign vpn2_sv48   = iova_i[38:30];
    assign vpn3_sv48   = iova_i[47:39];
    assign vpn0_sv48x4 = iova_i[20:12];
    assign vpn1_sv48x4 = iova_i[29:21];
    assign vpn2_sv48x4 = iova_i[38:30];
    assign vpn3_sv48x4 = iova_i[49:39];
    assign vpn0_sv57   = iova_i[20:12];
    assign vpn1_sv57   = iova_i[29:21];
    assign vpn2_sv57   = iova_i[38:30];
    assign vpn3_sv57   = iova_i[47:39];
    assign vpn4_sv57   = iova_i[56:48];
    assign vpn0_sv57x4 = iova_i[20:12];
    assign vpn1_sv57x4 = iova_i[29:21];
    assign vpn2_sv57x4 = iova_i[38:30];
    assign vpn3_sv57x4 = iova_i[47:39];
    assign vpn4_sv57x4 = iova_i[58:48];
    assign vpn5_sv     = iova_i[63:57];
    assign vpn5_svx4   = iova_i[63:59];
//}}}
//=== TAG {{{
    assign valid_i                  = tag_i.valid;
    assign hit_tag_device_id        = tag_i.device_id;
    assign hit_tag_process_id_valid = tag_i.process_id_valid;
    assign hit_tag_process_id       = tag_i.process_id;
    assign hit_tag_is_translated    = tag_i.is_translated;
    assign hit_tag_va               = tag_i.va;
    assign content_n                = tag_i.N;
//}}}
//=== bare {{{
    assign s1_bare     = tag_i.S1MODE=='d0;
    assign s1_sv32     = tag_i.SXL      ? tag_i.S1MODE=='d8 : 1'b0;
    assign s1_sv39     = tag_i.SXL      ? 1'b0              : tag_i.S1MODE=='d8;
    assign s1_sv48     = tag_i.SXL      ? 1'b0              : tag_i.S1MODE=='d9;
    assign s1_sv57     = tag_i.SXL      ? 1'b0              : tag_i.S1MODE=='d10;
    assign s2_bare     = tag_i.S2MODE=='d0;
    assign s2_sv32x4   = csr_fctl_gxl_i ? tag_i.S2MODE=='d8 : 1'b0;
    assign s2_sv39x4   = csr_fctl_gxl_i ? 1'b0              : tag_i.S2MODE=='d8;
    assign s2_sv48x4   = csr_fctl_gxl_i ? 1'b0              : tag_i.S2MODE=='d9;
    assign s2_sv57x4   = csr_fctl_gxl_i ? 1'b0              : tag_i.S2MODE=='d10;
//}}}
//=== size {{{
    assign s1_2m       = tag_i.S1SIZE == 'b01;
    assign s2_2m       = tag_i.S2SIZE == 'b01;
    assign is_2m       = (~s1_bare & ~s2_bare) ? ((s2_2m & (s1_2m | s1_1g | s1_512g)) | (s1_2m & (s2_2m | s2_1g | s2_512g))) :
                                                 ((s2_2m & ~s2_bare) | (s1_2m & ~s1_bare));
    assign s1_1g       = tag_i.S1SIZE == 'b10;
    assign s2_1g       = tag_i.S2SIZE == 'b10;
    assign is_1g       = (~s1_bare & ~s2_bare) ? ((s2_1g & (s1_1g | s1_512g)) | (s1_1g & (s2_1g | s2_512g))) :
                                                 ((s2_1g & ~s2_bare) | (s1_1g & ~s1_bare));
    assign s1_512g     = tag_i.S1SIZE == 'b11;
    assign s2_512g     = tag_i.S2SIZE == 'b11;
    assign is_512g     = (~s1_bare & ~s2_bare) ? (s2_512g & s1_512g) :
                                                 ((s2_512g & ~s2_bare) | (s1_512g & ~s1_bare));
//    assign is_2m   = (BANK_TYPE==2'b01);
//    assign is_1g   = (BANK_TYPE==2'b10);
//    assign is_512g = (BANK_TYPE==2'b11);
//}}}
//=== lkp hit {{{
//        assign device_id_hit    = ~valid_i ? 1'b0 : (device_id_i     == hit_tag_device_id);
//        assign process_id_hit   = ~valid_i ? 1'b0 : (hit_tag_process_id_valid ? ((process_id_i==hit_tag_process_id) & process_id_valid_i) : ~process_id_valid_i);
//        assign translated_hit   = ~valid_i ? 1'b0 : (is_translated_i == hit_tag_is_translated);
//        
//        assign vpn0_hit         = ~valid_i ? 1'b0 :
//                                  ~s1_bare ? (content_n ? ((s1_sv32   & (vpn0_sv32[21:16]   == hit_tag_va[21:16])) | (vpn0_sv39[20:16]   == hit_tag_va[20:16])) :
//                                                          ((s1_sv32   & (vpn0_sv32          == hit_tag_va[21:12])) | (vpn0_sv39          == hit_tag_va[20:12]))
//                                             ) :
//                                  ~s2_bare ? (content_n ? ((s2_sv32x4 & (vpn0_sv32x4[21:16] == hit_tag_va[21:16])) | (vpn0_sv39x4[20:16] == hit_tag_va[20:12])) :
//                                                          ((s2_sv32x4 & (vpn0_sv32x4        == hit_tag_va[21:12])) | (vpn0_sv39x4        == hit_tag_va[20:12]))
//                                             ) :
//                                  1'b1;
//        assign vpn1_hit         = ~valid_i ? 1'b0 :
//                                  ~s1_bare ? ((s1_sv32   & (vpn1_sv32   == hit_tag_va[33:22]))  | (vpn1_sv39   == hit_tag_va[29:21])) :
//                                  ~s2_bare ? ((s2_sv32x4 & (vpn1_sv32x4 == hit_tag_va[33:22]))  | (vpn1_sv39x4 == hit_tag_va[29:21])) :
//                                  1'b1;
//        assign vpn2_hit         = ~valid_i ? 1'b0 :
//                                  ~s1_bare ? ( s1_sv32                                          | (vpn2_sv39   == hit_tag_va[38:30])) :
//                                  ~s2_bare ? ( s2_sv32x4                                        | (s2_sv39x4 ? (vpn2_sv39x4 == hit_tag_va[40:30]) : (vpn2_sv48x4 == hit_tag_va[38:30]))) :
//                                  1'b1;
//        assign vpn3_hit         = ~valid_i ? 1'b0 :
//                                  ~s1_bare ? ((s1_sv32   | s1_sv39)                             | (vpn3_sv48   == hit_tag_va[47:39])) :
//                                  ~s2_bare ? ((s2_sv32x4 | s2_sv39x4)                           | (s2_sv48x4 ? (vpn3_sv48x4 == hit_tag_va[49:39]) : (vpn3_sv57x4 == hit_tag_va[47:39]))) :
//                                  1'b1;
//        assign vpn4_hit         = ~valid_i ? 1'b0 :
//                                  ~s1_bare ? ((s1_sv32   | s1_sv39 | s1_sv48)                   | (vpn4_sv57   == hit_tag_va[56:48])) :
//                                  ~s2_bare ? ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)               | (vpn4_sv57x4 == hit_tag_va[58:48])) :
//                                  1'b1;
//    //  va_hit bug, when is_512g, vpn0 and vpn1 need not hit, 20241114
//    //    assign va_hit           = (is_2m | vpn0_hit) & (is_1g | vpn1_hit) & (is_512g | vpn2_hit) & vpn3_hit & vpn4_hit;
//        assign va_hit           = ((is_2m | is_1g | is_512g) | vpn0_hit) & // vpn0
//                                  ((        is_1g | is_512g) | vpn1_hit) & // vpn1
//                                  ((                is_512g) | vpn2_hit) & // vpn2
//                                                               vpn3_hit  & // vpn3
//                                                               vpn4_hit;   // vpn4
//        assign tag_hit          = va_hit & translated_hit & process_id_hit & device_id_hit;
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
    /*input  logic                      */  .valid_i                    (valid_i                    ),
    /*input  logic [23:0]               */  .hit_tag_device_id          (hit_tag_device_id          ),
    /*input  logic                      */  .hit_tag_process_id_valid   (hit_tag_process_id_valid   ),
    /*input  logic [19:0]               */  .hit_tag_process_id         (hit_tag_process_id         ),
    /*input  logic                      */  .hit_tag_is_translated      (hit_tag_is_translated      ),
    /*input  logic [63:12]              */  .hit_tag_va                 (hit_tag_va                 ),
    /*input  logic                      */  .content_n                  (content_n                  ),
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

//}}}
//=== inv hit {{{
//        assign inv_req_valid_i  = 1'b1;
//    
        assign inv_tag_GSCID    = itag_i.GSCID;
        assign inv_tag_PSCID    = itag_i.PSCID;
        assign inv_tag_GPPN     = itag_i.GPPN;
        assign content_G        = itag_i.G;
//    
//        assign inv_addr_i       = req_i.inv.req.addr;
//        assign inv_vpn0_sv32    = inv_addr_i[21:12];
//        assign inv_vpn1_sv32    = inv_addr_i[31:22];
//        assign inv_vpn0_sv32x4  = inv_addr_i[21:12];
//        assign inv_vpn1_sv32x4  = inv_addr_i[33:22];
//        assign inv_vpn0_sv39    = inv_addr_i[20:12];
//        assign inv_vpn1_sv39    = inv_addr_i[29:21];
//        assign inv_vpn2_sv39    = inv_addr_i[38:30];
//        assign inv_vpn0_sv39x4  = inv_addr_i[20:12];
//        assign inv_vpn1_sv39x4  = inv_addr_i[29:21];
//        assign inv_vpn2_sv39x4  = inv_addr_i[40:30];
//        assign inv_vpn0_sv48    = inv_addr_i[20:12];
//        assign inv_vpn1_sv48    = inv_addr_i[29:21];
//        assign inv_vpn2_sv48    = inv_addr_i[38:30];
//        assign inv_vpn3_sv48    = inv_addr_i[47:39];
//        assign inv_vpn0_sv48x4  = inv_addr_i[20:12];
//        assign inv_vpn1_sv48x4  = inv_addr_i[29:21];
//        assign inv_vpn2_sv48x4  = inv_addr_i[38:30];
//        assign inv_vpn3_sv48x4  = inv_addr_i[49:39];
//        assign inv_vpn0_sv57    = inv_addr_i[20:12];
//        assign inv_vpn1_sv57    = inv_addr_i[29:21];
//        assign inv_vpn2_sv57    = inv_addr_i[38:30];
//        assign inv_vpn3_sv57    = inv_addr_i[47:39];
//        assign inv_vpn4_sv57    = inv_addr_i[56:48];
//        assign inv_vpn0_sv57x4  = inv_addr_i[20:12];
//        assign inv_vpn1_sv57x4  = inv_addr_i[29:21];
//        assign inv_vpn2_sv57x4  = inv_addr_i[38:30];
//        assign inv_vpn3_sv57x4  = inv_addr_i[47:39];
//        assign inv_vpn4_sv57x4  = inv_addr_i[58:48];
//        //===                   
//        assign inval_ddt_i      = req_i.inv.req.itype == iommu_acd_pkg::INVTYPE_INVALID_DDT;
//        assign inval_pdt_i      = req_i.inv.req.itype == iommu_acd_pkg::INVTYPE_INVALID_PDT;
//        assign inv_process_id_i = req_i.inv.req.pid_pscid;
//        assign inv_device_id_i  = req_i.inv.req.did_gscid;
//        assign inv_dv_i           = req_i.inv.req.dv_gv;
//        assign device_id_inv_hit  =  ~inv_dv_i
//                                   | (inv_device_id_i == hit_tag_device_id);
//        assign process_id_inv_hit = (inv_process_id_i == hit_tag_process_id);
//        //===                     
//        assign vma_i            = req_i.inv.req.itype == iommu_acd_pkg::INVTYPE_VMA;
//        assign gvma_i           = req_i.inv.req.itype == iommu_acd_pkg::INVTYPE_GVMA;
//        assign inv_av_i         = req_i.inv.req.av;
//        assign inv_pscid_i      = req_i.inv.req.pid_pscid;
//        assign inv_pscv_i       = req_i.inv.req.pscv;
//        assign inv_gscid_i      = req_i.inv.req.did_gscid[15:0];
//        assign inv_gv_i         = req_i.inv.req.dv_gv;
//        assign gscid_inv_hit    =  ~inv_gv_i                      // if VMA and GV==0, no GSCID matching required; if GVMA and GV==0, no GSCID matching required
//                                 | (inv_gscid_i == inv_tag_GSCID);// if VMA and GV==1, GSCID match required; if GVMA and GV==1, GSCID matching required
//        assign pscid_inv_hit    = ~inv_pscv_i                                     // if VMA and PSCV==0, invalid for all host address spaces
//                                 | (~content_G & (inv_pscid_i == inv_tag_PSCID)); // if VMA and PSCV==1, invalid host address spaces identified by PSCID, escept for entries containing Global mappings
//        //===
//        always@(*) begin
//            inv_tag_gpa = (~s2_bare & s1_bare)  ? hit_tag_va            :       // S2_Only translation, GPN is stored in hit_tag_va
//                          (~s2_bare & ~s1_bare) ? {2'b0, inv_tag_GPPN}  :       // S1S2_nested translation, GPN is stored in ing_tag_GPN
//                                                  'd0;
//    //        if(~s1_bare) begin          // S1 not Bare, so inv_tag.GPPN is valid GPA
//    //                                    // S2 may also Bare, then no inv_vpn*_hit need and inv_tag_gpa will be ignore
//    //            if(is_2m) begin         // 2M or SV32-4M, no match GPN[0] for inv_hit
//    //                if(s1_sv32) inv_tag_gpa[21:12] = inv_vpn0_sv32[21:12];
//    //                else        inv_tag_gpa[20:12] = inv_vpn0_sv39[20:12];
//    //            end
//    //            else if(is_1g) begin    // 1G , no match GPN[1] for inv_hit
//    //                if(s1_sv32) inv_tag_gpa[31:12] = inv_vpn0_;
//    //                else        inv_tag_gpa[29:12] = {inv_vpn1_sv39[29:21], inv_vpn0_sv39[20:12]};
//    //            end
//    //            else if(is_512g) begin  // 512G, gpa[2:0] from iova
//    //                inv_tag_gpa[38:12]  = hit_tag_va[38:12];
//    //            end
//    //        end
//    //        else                        // S1 is Bare, inv_tag.GPPN is not valid GPA, the GPA is IOVA and stored in hit_tag
//    //                                    // if S2 is also Bare, nomater use hit_tag_va, as inv_vpn?_hit will not asserted and the inv_tag_gpa is useless when s2_bare active
//    //            inv_tag_gpa = hit_tag_va;
//    
//            if(s2_2m) begin         // S2 is 2M/4M pagesize, no GPN[0] matching for inv_hit
//                if(s2_sv32x4) inv_tag_gpa[21:12] = inv_vpn0_sv32x4;
//                else          inv_tag_gpa[20:12] = inv_vpn0_sv39x4;
//            end
//            else if(s2_1g) begin
//                inv_tag_gpa[20:12] = inv_vpn0_sv39x4;
//                inv_tag_gpa[29:21] = inv_vpn1_sv39x4;
//            end
//            else if(s2_512g) begin
//                inv_tag_gpa[20:12] = inv_vpn0_sv39x4;
//                inv_tag_gpa[29:21] = inv_vpn1_sv39x4;
//                inv_tag_gpa[38:30] = inv_vpn2_sv48x4;
//            end
//            else if(content_n) begin  // 4K pagesize with Svnapot active
//                inv_tag_gpa[15:12] = inv_vpn0_sv39x4[15:12];
//            end
//        end
//    
//        always@(*) begin
//            inv_tag_va = hit_tag_va;
//            if(s1_2m) begin         // S1 is 2M/4M pagesize, no VPN[0] matching for inv_hit
//                if(s1_sv32)   inv_tag_va[21:12] = inv_vpn0_sv32;
//                else          inv_tag_va[20:12] = inv_vpn0_sv39;
//            end
//            else if(s1_1g) begin
//                inv_tag_va[20:12] = inv_vpn0_sv39;
//                inv_tag_va[29:21] = inv_vpn1_sv39;
//            end
//            else if(s1_512g) begin
//                inv_tag_va[20:12] = inv_vpn0_sv39;
//                inv_tag_va[29:21] = inv_vpn1_sv39;
//                inv_tag_va[38:30] = inv_vpn2_sv48;
//            end
//            else if(content_n) begin  // 4K pagesize with Svnapot active
//                inv_tag_va[15:12] = inv_vpn0_sv39[15:12];
//            end
//        end
//        //===
//        // when S1 is Bare, inv_vpn*_hit will always 0 for VMA
//        // when S2 is Bare, inv_vpn*_hit will always 0 for GVMA
//        always@(*) begin
//            inv_vpn0_hit = 1'b0;
//            inv_vpn1_hit = 1'b0;
//            inv_vpn2_hit = 1'b0;
//            inv_vpn3_hit = 1'b0;
//            inv_vpn4_hit = 1'b0;
//            if(gvma_i) begin
//                inv_vpn0_hit = ((s2_sv32x4 & (inv_vpn0_sv32x4 == inv_tag_gpa[21:12]))   | (~s2_sv32x4 & (inv_vpn0_sv39x4 == inv_tag_gpa[20:12]))                                            );
//                inv_vpn1_hit = ((s2_sv32x4 & (inv_vpn1_sv32x4 == inv_tag_gpa[33:22]))   | (~s2_sv32x4 & (inv_vpn1_sv39x4 == inv_tag_gpa[29:21]))                                            );
//                inv_vpn2_hit = ( s2_sv32x4                                              | ( s2_sv39x4 ? (inv_vpn2_sv39x4 == inv_tag_gpa[40:30]) : (inv_vpn2_sv48x4 == inv_tag_gpa[38:30]))  );
//                inv_vpn3_hit = ((s2_sv32x4 | s2_sv39x4)                                 | ( s2_sv48x4 ? (inv_vpn3_sv48x4 == inv_tag_gpa[49:39]) : (inv_vpn3_sv57x4 == inv_tag_gpa[47:39]))  );
//                inv_vpn4_hit = ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)                     | (inv_vpn4_sv57x4 == inv_tag_gpa[58:48]));
//            end
//            else if(vma_i) begin
//                inv_vpn0_hit = ((s1_sv32   & (inv_vpn0_sv32   == inv_tag_va[21:12] ))   | (~s1_sv32   & (inv_vpn0_sv39   == inv_tag_va[20:12] ))    );
//                inv_vpn1_hit = ((s1_sv32   & (inv_vpn1_sv32   == inv_tag_va[31:22] ))   | (~s1_sv32   & (inv_vpn1_sv39   == inv_tag_va[29:21] ))    );
//                inv_vpn2_hit = ( s1_sv32                                                | (inv_vpn2_sv39 == inv_tag_va[38:30]));
//                inv_vpn3_hit = ((s1_sv32   | s1_sv39)                                   | (inv_vpn3_sv48 == inv_tag_va[47:39]));
//                inv_vpn4_hit = ((s1_sv32   | s1_sv39   | s1_sv48  )                     | (inv_vpn4_sv57 == inv_tag_va[56:48]));
//            end
//        end
//        assign addr_inv_hit =  ~inv_av_i 
//                             | (gvma_i & ~inv_gv_i)                                                             // when GV==0, AV in GVMA is ignored, invalid information cached from any level of the second stage page table, for all VM address space
//                             | (inv_vpn0_hit & inv_vpn1_hit & inv_vpn2_hit & inv_vpn3_hit & inv_vpn4_hit);
//        assign inv_hit      = inv_req_valid_i & (
//                                  (vma_i       & gscid_inv_hit     & pscid_inv_hit & addr_inv_hit & ~s1_bare)   // when AV==1, only invalid entries contains first-stage leaf PTE, but there's only leaf PTE in ACD
//                                | (gvma_i      & gscid_inv_hit                     & addr_inv_hit & ~s2_bare)   // when AV==1, only invlaid leaf second PTE, but there's only leaf PTE in ACD
//                                | (inval_ddt_i & device_id_inv_hit                                          )   // if DV==0, invalid all DDT and PDT for all devices; if DV==1, invalid leaf level DDT identified by DID, but there's only leaf DDT in ACD
//                                | (inval_pdt_i & device_id_inv_hit & process_id_inv_hit                     )   // if INVALD_PDT, invalid leaf PDT for the specified  PID & DID, but there's only leaf PDT in ACD
//                              );  
//    
    iommu_acd_micro_tlb_inv_hit #( //{{{
    /*parameter type         */ .INVALID_REQ_TYPE           (iommu_acd_pkg::INVALID_REQ_TYPE), // = logic,
    /*parameter  */ .SPARE_PARAM                (0                              )  // = 0
    ) U_inv_hit(
    /*input  logic                                      */  .inv_req_valid_i                (1'b1                           ),
    /*input  INVALID_REQ_TYPE                           */  .inv_req_i                      (req_i.inv.req                  ),
    /*input  logic                                      */  .s1_bare                        (s1_bare                        ),
    /*input  logic                                      */  .s1_sv32                        (s1_sv32                        ),
    /*input  logic                                      */  .s1_sv39                        (s1_sv39                        ),
    /*input  logic                                      */  .s1_sv48                        (s1_sv48                        ),
    /*input  logic                                      */  .s1_sv57                        (s1_sv57                        ),
    /*input  logic                                      */  .s2_bare                        (s2_bare                        ),
    /*input  logic                                      */  .s2_sv32x4                      (s2_sv32x4                      ),
    /*input  logic                                      */  .s2_sv39x4                      (s2_sv39x4                      ),
    /*input  logic                                      */  .s2_sv48x4                      (s2_sv48x4                      ),
    /*input  logic                                      */  .s2_sv57x4                      (s2_sv57x4                      ),
    /*input  logic                                      */  .s1_2m                          (s1_2m                          ),
    /*input  logic                                      */  .s2_2m                          (s2_2m                          ),
    /*input  logic                                      */  .s1_1g                          (s1_1g                          ),
    /*input  logic                                      */  .s2_1g                          (s2_1g                          ),
    /*input  logic                                      */  .s1_512g                        (s1_512g                        ),
    /*input  logic                                      */  .s2_512g                        (s2_512g                        ),
    /*input  logic [23:0]                               */  .hit_tag_device_id              (hit_tag_device_id              ),
    /*input  logic [19:0]                               */  .hit_tag_process_id             (hit_tag_process_id             ),
    /*input  logic [15:0]                               */  .inv_tag_GSCID                  (inv_tag_GSCID                  ),
    /*input  logic [19:0]                               */  .inv_tag_PSCID                  (inv_tag_PSCID                  ),
    /*input  logic [63:12]                              */  .hit_tag_va                     (hit_tag_va                     ),
    /*input  logic [61:12]                              */  .inv_tag_GPPN                   (inv_tag_GPPN                   ),
    /*input  logic                                      */  .content_n                      (content_n                      ),
    /*input  logic                                      */  .content_G                      (content_G                      ),
    /*output logic                                      */  .inv_hit                        (inv_hit                        ),
    /*input  logic                                      */  .spare_in                       (1'b0                           ) 
    );
//}}}

//}}}

    assign tag_hit_o = tag_hit & (req_is_lkp | req_is_upd);
    assign inv_hit_o = inv_hit & req_is_inv;

endmodule
