module iommu_acd_micro_tlb_tag_hit #(
    parameter               SPARE_PARAM = 1'b0
)(
    input  logic[21:12]                 vpn0_sv32  ,
    input  logic[31:22]                 vpn1_sv32  ,
    input  logic[21:12]                 vpn0_sv32x4,
    input  logic[33:22]                 vpn1_sv32x4,
    input  logic[20:12]                 vpn0_sv39  ,
    input  logic[29:21]                 vpn1_sv39  ,
    input  logic[38:30]                 vpn2_sv39  ,
    input  logic[20:12]                 vpn0_sv39x4,
    input  logic[29:21]                 vpn1_sv39x4,
    input  logic[40:30]                 vpn2_sv39x4,
    input  logic[20:12]                 vpn0_sv48  ,
    input  logic[29:21]                 vpn1_sv48  ,
    input  logic[38:30]                 vpn2_sv48  ,
    input  logic[47:39]                 vpn3_sv48  ,
    input  logic[20:12]                 vpn0_sv48x4,
    input  logic[29:21]                 vpn1_sv48x4,
    input  logic[38:30]                 vpn2_sv48x4,
    input  logic[49:39]                 vpn3_sv48x4,
    input  logic[20:12]                 vpn0_sv57  ,
    input  logic[29:21]                 vpn1_sv57  ,
    input  logic[38:30]                 vpn2_sv57  ,
    input  logic[47:39]                 vpn3_sv57  ,
    input  logic[56:48]                 vpn4_sv57  ,
    input  logic[20:12]                 vpn0_sv57x4,
    input  logic[29:21]                 vpn1_sv57x4,
    input  logic[38:30]                 vpn2_sv57x4,
    input  logic[47:39]                 vpn3_sv57x4,
    input  logic[58:48]                 vpn4_sv57x4,
    input  logic[63:57]                 vpn5_sv,
    input  logic[63:59]                 vpn5_svx4,
    input  logic [23:0]                 device_id_i,
    input  logic                        process_id_valid_i,
    input  logic [19:0]                 process_id_i,
    input  logic                        is_translated_i,
    input  logic                        valid_i,
    input  logic [23:0]                 hit_tag_device_id,
    input  logic                        hit_tag_process_id_valid,
    input  logic [19:0]                 hit_tag_process_id,
    input  logic                        hit_tag_is_translated,
    input  logic [63:12]                hit_tag_va,
    input  logic                        content_n,
    input  logic                        s1_bare,
    input  logic                        s1_sv32,
    input  logic                        s1_sv39,
    input  logic                        s1_sv48,
    input  logic                        s2_bare,
    input  logic                        s2_sv32x4,
    input  logic                        s2_sv39x4,
    input  logic                        s2_sv48x4,
    input  logic                        is_2m,
    input  logic                        is_1g,
    input  logic                        is_512g,
    output logic                        tag_hit
);
    logic device_id_hit, process_id_hit, translated_hit, vpn0_hit, vpn1_hit, vpn2_hit, vpn3_hit, vpn4_hit, va_hit, vpn5_hit;
    logic s1_sv32_s2;

    assign device_id_hit    = ~valid_i ? 1'b0 : (device_id_i     == hit_tag_device_id);
    assign process_id_hit   = ~valid_i ? 1'b0 : (hit_tag_process_id_valid ? ((process_id_i==hit_tag_process_id) & process_id_valid_i) : ~process_id_valid_i);
    assign translated_hit   = ~valid_i ? 1'b0 : (is_translated_i == hit_tag_is_translated);
    
//    assign vpn0_hit         = ~valid_i ? 1'b0 :
//                              ~s1_bare ? (content_n ? ((s1_sv32   & (vpn0_sv32[21:16]   == hit_tag_va[21:16])) | (vpn0_sv39[20:16]   == hit_tag_va[20:16])) :
//                                                      ((s1_sv32   & (vpn0_sv32          == hit_tag_va[21:12])) | (vpn0_sv39          == hit_tag_va[20:12]))
//                                         ) :
//                              ~s2_bare ? (content_n ? ((s2_sv32x4 & (vpn0_sv32x4[21:16] == hit_tag_va[21:16])) | (vpn0_sv39x4[20:16] == hit_tag_va[20:12])) :
//                                                      ((s2_sv32x4 & (vpn0_sv32x4        == hit_tag_va[21:12])) | (vpn0_sv39x4        == hit_tag_va[20:12]))
//                                         ) :
//                              1'b1;
//    assign vpn1_hit         = ~valid_i ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   & (vpn1_sv32   == hit_tag_va[33:22])) | (vpn1_sv39   == hit_tag_va[29:21])) :
//                              ~s2_bare ? ((s2_sv32x4 & (vpn1_sv32x4 == hit_tag_va[33:22])) | (vpn1_sv39x4 == hit_tag_va[29:21])) :
//                              1'b1;
//    assign vpn2_hit         = ~valid_i ? 1'b0 :
//                              ~s1_bare ? ( s1_sv32                                         | (vpn2_sv39   == hit_tag_va[38:30])) :
//                              ~s2_bare ? ( s2_sv32x4                                       | (s2_sv39x4 ? (vpn2_sv39x4 == hit_tag_va[40:30]) : (vpn2_sv48x4 == hit_tag_va[38:30]))) :
//                              1'b1;
//    assign vpn3_hit         = ~valid_i ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   | s1_sv39)                            | (vpn3_sv48   == hit_tag_va[47:39])) :
//                              ~s2_bare ? ((s2_sv32x4 | s2_sv39x4)                          | (s2_sv48x4 ? (vpn3_sv48x4 == hit_tag_va[49:39]) : (vpn3_sv57x4 == hit_tag_va[47:39]))) :
//                              1'b1;
//    assign vpn4_hit         = ~valid_i ? 1'b0 :
//                              ~s1_bare ? ((s1_sv32   | s1_sv39 | s1_sv48)                  | (vpn4_sv57   == hit_tag_va[56:48])) :
//                              ~s2_bare ? ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)              | (vpn4_sv57x4 == hit_tag_va[58:48])) :
//                              1'b1;
    assign vpn0_hit         = ~valid_i ? 1'b0 :
                              ~s1_bare ? (content_n ? ((s1_sv32   & (vpn0_sv32[21:16]   == hit_tag_va[21:16])) | (~s1_sv32   & (vpn0_sv39[20:16]   == hit_tag_va[20:16]))   ) :
                                                      ((s1_sv32   & (vpn0_sv32[21:12]   == hit_tag_va[21:12])) | (~s1_sv32   & (vpn0_sv39[20:12]   == hit_tag_va[20:12]))   )
                                         ) :
                              ~s2_bare ? (content_n ? ((s2_sv32x4 & (vpn0_sv32x4[21:16] == hit_tag_va[21:16])) | (~s2_sv32x4 & (vpn0_sv39x4[20:16] == hit_tag_va[20:16]))   ) :
                                                      ((s2_sv32x4 & (vpn0_sv32x4[21:12] == hit_tag_va[21:12])) | (~s2_sv32x4 & (vpn0_sv39x4[20:12] == hit_tag_va[20:12]))   )
                                         ) :
                              1'b1;
    assign vpn1_hit         = ~valid_i ? 1'b0 :
// when S1 is sv32(4M) but S2 is not sv32x4(sv39/48/57x4), VPN0[21] is not used in S1 translation and is translated bypass to GPN
// but located in GPN1[21], which is used in translation with 2M/1G/512G S2-pagesize
// so if S1 is sv32 but S2 is not sv32x4(sv39/48/57x4), VPN0[21] should also involved in vpn1_hit calculation
// if s2 is sv32x4, VPN0[21] need not involved in vpn1_hit calculation, as VPN0[21] IS
//                              ~s1_bare ? (             (s1_sv32   & (vpn1_sv32[31:22]   == hit_tag_va[31:22])) | (~s1_sv32   & (vpn1_sv39[29:21]   == hit_tag_va[29:21]))   ) :
//                                                          sv32 and sv32x4                                             not-sv32 or sv32-with-not-sv32x4
                              ~s1_bare ? (             (s1_sv32_s2& (vpn1_sv32[31:22]   == hit_tag_va[31:22])) | (             (vpn1_sv39[29:21]   == hit_tag_va[29:21]))   ) :
                              ~s2_bare ? (             (s2_sv32x4 & (vpn1_sv32x4[33:22] == hit_tag_va[33:22])) | (~s2_sv32x4 & (vpn1_sv39x4[29:21] == hit_tag_va[29:21]))   ) :
                              1'b1;
    assign s1_sv32_s2       = s1_sv32 & ~s2_bare & s2_sv32x4;   // S1 is sv32 AND S2 is sv32x4
    assign vpn2_hit         = ~valid_i ? 1'b0 :
                              ~s1_bare ? (                          (vpn2_sv39[38:30]   == hit_tag_va[38:30])                                                               ) :
                              ~s2_bare ? (             (s2_sv39x4 & (vpn2_sv39x4[40:30] == hit_tag_va[40:30])) | (~s2_sv39x4 & (vpn2_sv48x4[38:30] == hit_tag_va[38:30]))   ) :
                              1'b1;
    assign vpn3_hit         = ~valid_i ? 1'b0 :
                              ~s1_bare ? (                          (vpn3_sv48[47:39]   == hit_tag_va[47:39])                                                               ) :
                              ~s2_bare ? (             (s2_sv48x4 & (vpn3_sv48x4[49:39] == hit_tag_va[49:39])) | (~s2_sv48x4 & (vpn3_sv57x4[47:39] == hit_tag_va[47:39]))   ) :
                              1'b1;
    assign vpn4_hit         = ~valid_i ? 1'b0 :
                              ~s1_bare ? (                          (vpn4_sv57[56:48]   == hit_tag_va[56:48])                                                               ) :
                              ~s2_bare ? (                          (vpn4_sv57x4[58:48] == hit_tag_va[58:48])                                                               ) :
                              1'b1;
    assign vpn5_hit         = ~valid_i ? 1'b0 :
                              ~s1_bare ? (                          (vpn5_sv[63:57]     == hit_tag_va[63:57])                                                               ) :
                              ~s2_bare ? (                          (vpn5_svx4[63:59]   == hit_tag_va[63:59])                                                               ) :
                              1'b1;
// va_hit bug, when is_512g, vpn0 and vpn1 need not hit, 20241114
//    assign va_hit           = (is_2m | vpn0_hit) & (is_1g | vpn1_hit) & (is_512g | vpn2_hit) & vpn3_hit & vpn4_hit;
    assign va_hit           = ((is_2m | is_1g | is_512g) | vpn0_hit) & // vpn0
                              ((        is_1g | is_512g) | vpn1_hit) & // vpn1
                              ((                is_512g) | vpn2_hit) & // vpn2
                                                           vpn3_hit  & // vpn3
                                                           vpn4_hit  & // vpn4
                                                           vpn5_hit;   // vpn5
    assign tag_hit          = va_hit & translated_hit & process_id_hit & device_id_hit;

endmodule
