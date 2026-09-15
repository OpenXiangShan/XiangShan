module iommu_acd_ptwackfilter #(//{{{
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter   INV_IDX_WIDTH               = iommu_acd_pkg::INV_IDX_WIDTH,
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // PTW ACK
    input  logic                                        ptw_ack_valid_i,
    input  PTW_ACK_TYPE                                 ptw_ack_i,
    input  PTW_REQ_TYPE                                 ptw_req_i,
    // INV REQ
    input  logic [INV_INFLY_NUM-1:0]                    inv_req_valid_i,
    input  INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]         inv_req_i,
    // PTW ACK OUT
    output logic                                        ptw_ack_valid_o,
    output PTW_ACK_TYPE                                 ptw_ack_o,
    //
    input  logic                                        csr_fctl_gxl_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    logic [INV_INFLY_NUM-1:0]                           inv_hit;
    logic                                               translate_fail, translate_deny, translate_succ, translate_ncah;
//}}}



//=== MainCode {{{
    assign translate_fail = (ptw_ack_i.opcode==2'b10);
    assign translate_deny = (ptw_ack_i.opcode==2'b01);
    assign translate_succ = (ptw_ack_i.opcode==2'b00);
    assign translate_ncah = (ptw_ack_i.opcode==2'b11);

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ptw_ack_valid_o <= 1'b0;
            ptw_ack_o       <= 'd0;
        end
        else begin
            if(ptw_ack_valid_i) begin
                if(translate_fail | translate_ncah) begin   // FAIL or NON-CACHE, just bypss ptw_ack out
                    ptw_ack_valid_o <= 1'b1;
                    ptw_ack_o       <= ptw_ack_i;
                end
                else begin                                  // check if inv hit
                    if(|inv_hit) begin
                        ptw_ack_valid_o <= 1'b1;
                        ptw_ack_o.idx       <= ptw_ack_i.idx    ;
                        ptw_ack_o.opcode    <= {1'b1, ptw_ack_i.opcode[1:0]}; // if inv hit, change to NON-CACHE type
                        ptw_ack_o.MRIF      <= ptw_ack_i.MRIF   ;
                        ptw_ack_o.PBMT      <= ptw_ack_i.PBMT   ;
                        ptw_ack_o.GPPN      <= ptw_ack_i.GPPN   ;
                        ptw_ack_o.ENATS     <= ptw_ack_i.ENATS  ;
                        ptw_ack_o.T2GPA     <= ptw_ack_i.T2GPA  ;
                        ptw_ack_o.DTF       <= ptw_ack_i.DTF    ;
                        ptw_ack_o.PDTV      <= ptw_ack_i.PDTV   ;
                        ptw_ack_o.DPE       <= ptw_ack_i.DPE    ;
                        ptw_ack_o.SXL       <= ptw_ack_i.SXL    ;
                        ptw_ack_o.ENS       <= ptw_ack_i.ENS    ;
                        ptw_ack_o.SUM       <= ptw_ack_i.SUM    ;
                        ptw_ack_o.S1_D      <= ptw_ack_i.S1_D   ;
                        ptw_ack_o.S2_D      <= ptw_ack_i.S2_D   ;
                        ptw_ack_o.SADE      <= ptw_ack_i.SADE   ;
                        ptw_ack_o.GADE      <= ptw_ack_i.GADE   ;
                        ptw_ack_o.N         <= ptw_ack_i.N      ;
//                        ptw_ack_o.S1_PERM_D <= ptw_ack_i.S1_PERM_D;
//                        ptw_ack_o.S1_PERM_A <= ptw_ack_i.S1_PERM_A;
                        ptw_ack_o.S1_PERM   <= ptw_ack_i.S1_PERM;
//                        ptw_ack_o.S2_PERM_D <= ptw_ack_i.S2_PERM_D;
//                        ptw_ack_o.S2_PERM_A <= ptw_ack_i.S2_PERM_A;
                        ptw_ack_o.S2_PERM   <= ptw_ack_i.S2_PERM;
                        ptw_ack_o.S1SIZE    <= ptw_ack_i.S1SIZE ;
                        ptw_ack_o.S2SIZE    <= ptw_ack_i.S2SIZE ;
                        ptw_ack_o.S1MODE    <= ptw_ack_i.S1MODE ;
                        ptw_ack_o.S2MODE    <= ptw_ack_i.S2MODE ;
                        ptw_ack_o.PDTMODE   <= ptw_ack_i.PDTMODE;
                        ptw_ack_o.GSCID     <= ptw_ack_i.GSCID  ;
                        ptw_ack_o.PSCID     <= ptw_ack_i.PSCID  ;
                        ptw_ack_o.PPN       <= ptw_ack_i.PPN    ;
                    end
                    else begin
                        ptw_ack_valid_o <= 1'b1;
                        ptw_ack_o       <= ptw_ack_i;
                    end
                end
            end
            else begin
                ptw_ack_valid_o <= 1'b0;
            end
        end
    end
//}}}



//=== hit inst {{{
genvar ih;
generate
    for(ih=0; ih<INV_INFLY_NUM; ih++) begin : inv_hit_gen
        iommu_paf_tag_hit #(
        /*parameter type         */ .PTW_REQ_TYPE               (PTW_REQ_TYPE               ), // = iommu_acd_pkg::PTW_REQ_TYPE,
        /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE               ), // = iommu_acd_pkg::PTW_ACK_TYPE,
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
        /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 1'b0
        ) U_hit(
        /*input  PTW_REQ_TYPE                               */  .ptw_req_i                  (ptw_req_i              ),
        /*input  PTW_ACK_TYPE                               */  .ptw_ack_i                  (ptw_ack_i              ),
        /*input  logic                                      */  .inv_req_valid_i            (inv_req_valid_i    [ih]),
        /*input  INVALID_REQ_TYPE                           */  .inv_req_i                  (inv_req_i          [ih]),
        /*input  logic                                      */  .csr_fctl_gxl_i             (csr_fctl_gxl_i         ),
        /*output logic                                      */  .inv_hit_o                  (inv_hit            [ih]),
        /*input  logic                                      */  .spare_in                   (1'b0)
        );
    end
endgenerate

//}}}
endmodule
//}}}




module iommu_paf_tag_hit #(//{{{
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter   SPARE_PARAM                 = 1'b0
)(
//{{{ IO
    input  PTW_REQ_TYPE                                 ptw_req_i,
    input  PTW_ACK_TYPE                                 ptw_ack_i,
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    input  logic                                        csr_fctl_gxl_i,
    output logic                                        inv_hit_o,
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    logic [23:0]                                        hit_tag_device_id;
    logic                                               hit_tag_process_id_valid;
    logic [19:0]                                        hit_tag_process_id;
    logic                                               hit_tag_is_translated;
    logic [63:12]                                       hit_tag_va;

    logic                                               s1_bare;
    logic                                               s1_sv32;
    logic                                               s1_sv39;
    logic                                               s1_sv48;
    logic                                               s2_bare;
    logic                                               s2_sv32x4;
    logic                                               s2_sv39x4;
    logic                                               s2_sv48x4;
                                                        
    logic                                               s1_2m, s1_1g, s1_512g, s2_2m, s2_1g, s2_512g;
    logic                                               is_2m;
    logic                                               is_ig;
    logic                                               is_512g;
                                                        
    logic [15:0]                                        inv_tag_GSCID;
    logic [19:0]                                        inv_tag_PSCID;
    logic [61:12]                                       inv_tag_GPPN;
    logic                                               content_G;
                                                        
    logic [63:12]                                       inv_tag_gpa;
    logic                                               inv_vpn0_hit;
    logic                                               inv_vpn1_hit;
    logic                                               inv_vpn2_hit;
    logic                                               inv_vpn3_hit;
    logic                                               inv_vpn4_hit;
    logic                                               addr_inv_hit;
    logic                                               inv_hit;
                                                        
    logic [63:12]                                       inv_addr_i     ;
    logic [21:12]                                       inv_vpn0_sv32  ;
    logic [31:22]                                       inv_vpn1_sv32  ;
    logic [21:12]                                       inv_vpn0_sv32x4;
    logic [33:22]                                       inv_vpn1_sv32x4;
    logic [20:12]                                       inv_vpn0_sv39  ;
    logic [29:21]                                       inv_vpn1_sv39  ;
    logic [38:30]                                       inv_vpn2_sv39  ;
    logic [20:12]                                       inv_vpn0_sv39x4;
    logic [29:21]                                       inv_vpn1_sv39x4;
    logic [40:30]                                       inv_vpn2_sv39x4;
    logic [20:12]                                       inv_vpn0_sv48  ;
    logic [29:21]                                       inv_vpn1_sv48  ;
    logic [38:30]                                       inv_vpn2_sv48  ;
    logic [47:39]                                       inv_vpn3_sv48  ;
    logic [20:12]                                       inv_vpn0_sv48x4;
    logic [29:21]                                       inv_vpn1_sv48x4;
    logic [38:30]                                       inv_vpn2_sv48x4;
    logic [49:39]                                       inv_vpn3_sv48x4;
    logic [20:12]                                       inv_vpn0_sv57  ;
    logic [29:21]                                       inv_vpn1_sv57  ;
    logic [38:30]                                       inv_vpn2_sv57  ;
    logic [47:39]                                       inv_vpn3_sv57  ;
    logic [56:48]                                       inv_vpn4_sv57  ;
    logic [20:12]                                       inv_vpn0_sv57x4;
    logic [29:21]                                       inv_vpn1_sv57x4;
    logic [38:30]                                       inv_vpn2_sv57x4;
    logic [47:39]                                       inv_vpn3_sv57x4;
    logic [58:48]                                       inv_vpn4_sv57x4;
                                                        
    logic                                               inval_ddt_i       ;
    logic                                               inval_pdt_i       ;
    logic [19:0]                                        inv_process_id_i  ;
    logic [23:0]                                        inv_device_id_i   ;
    logic                                               inv_dv_i          ;
    logic                                               device_id_inv_hit ;
    logic                                               process_id_inv_hit;
    logic                                               vma_i             ;
    logic                                               gvma_i            ;
    logic                                               inv_av_i          ;
    logic [19:0]                                        inv_pscid_i       ;
    logic                                               inv_pscv_i        ;
    logic [15:0]                                        inv_gscid_i       ;
    logic                                               inv_gv_i          ;
    logic                                               gscid_inv_hit     ;
    logic                                               pscid_inv_hit     ;

//}}}

//=== MainCode {{{
//=== TAG {{{
    assign hit_tag_device_id        = ptw_req_i.device_id;
    assign hit_tag_process_id_valid = ptw_req_i.process_id_valid;
    assign hit_tag_process_id       = ptw_req_i.process_id;
    assign hit_tag_is_translated    = ptw_req_i.is_translated;
    assign hit_tag_va               = ptw_req_i.va;
//}}}
//=== bare {{{
    assign s1_bare                  = ptw_ack_i.S1MODE=='d0;
    assign s1_sv32                  = ptw_ack_i.SXL      ? ptw_ack_i.S1MODE=='d8 : 1'b0;
    assign s1_sv39                  = ptw_ack_i.SXL      ? 1'b0              : ptw_ack_i.S1MODE=='d8;
    assign s1_sv48                  = ptw_ack_i.SXL      ? 1'b0              : ptw_ack_i.S1MODE=='d9;
    assign s1_sv57                  = ptw_ack_i.SXL      ? 1'b0              : ptw_ack_i.S1MODE=='d10;
    assign s2_bare                  = ptw_ack_i.S2MODE=='d0;
    assign s2_sv32x4                = csr_fctl_gxl_i ? ptw_ack_i.S2MODE=='d8 : 1'b0;
    assign s2_sv39x4                = csr_fctl_gxl_i ? 1'b0              : ptw_ack_i.S2MODE=='d0;
    assign s2_sv48x4                = csr_fctl_gxl_i ? 1'b0              : ptw_ack_i.S2MODE=='d0;
    assign s2_sv57x4                = csr_fctl_gxl_i ? 1'b0              : ptw_ack_i.S2MODE=='d0;
//}}}                               
//=== size {{{                      
    assign s1_2m                    = ptw_ack_i.S1SIZE == 'b01;
    assign s2_2m                    = ptw_ack_i.S2SIZE == 'b01;
    assign is_2m                    = (~s1_bare & ~s2_bare) ? ((s2_2m & (s1_2m | s1_1g | s1_512g)) | (s1_2m & (s2_2m | s2_1g | s2_512g))) :
                                                              ((s2_2m & ~s2_bare) | (s1_2m & ~s1_bare));
    assign s1_1g                    = ptw_ack_i.S1SIZE == 'b10;
    assign s2_1g                    = ptw_ack_i.S2SIZE == 'b10;
    assign is_1g                    = (~s1_bare & ~s2_bare) ? ((s2_1g & (s1_1g | s1_512g)) | (s1_1g & (s2_1g | s2_512g))) :
                                                              ((s2_1g & ~s2_bare) | (s1_1g & ~s1_bare));
    assign s1_512g                  = ptw_ack_i.S1SIZE == 'b11;
    assign s2_512g                  = ptw_ack_i.S2SIZE == 'b11;
    assign is_512g                  = (~s1_bare & ~s2_bare) ? (s2_512g & s1_512g) :
                                                 ((s2_512g & ~s2_bare) | (s1_512g & ~s1_bare));
//}}}
//=== inv hit {{{
    assign inv_tag_GSCID            = ptw_ack_i.GSCID;
    assign inv_tag_PSCID            = ptw_ack_i.PSCID;
    assign inv_tag_GPPN             = ptw_ack_i.GPPN;
    assign content_G                = ptw_ack_i.S1_PERM[4];
                                    
    assign inv_addr_i               = inv_req_i.addr;
    assign inv_vpn0_sv32            = inv_addr_i[21:12];
    assign inv_vpn1_sv32            = inv_addr_i[31:22];
    assign inv_vpn0_sv32x4          = inv_addr_i[21:12];
    assign inv_vpn1_sv32x4          = inv_addr_i[33:22];
    assign inv_vpn0_sv39            = inv_addr_i[20:12];
    assign inv_vpn1_sv39            = inv_addr_i[29:21];
    assign inv_vpn2_sv39            = inv_addr_i[38:30];
    assign inv_vpn0_sv39x4          = inv_addr_i[20:12];
    assign inv_vpn1_sv39x4          = inv_addr_i[29:21];
    assign inv_vpn2_sv39x4          = inv_addr_i[40:30];
    assign inv_vpn0_sv48            = inv_addr_i[20:12];
    assign inv_vpn1_sv48            = inv_addr_i[29:21];
    assign inv_vpn2_sv48            = inv_addr_i[38:30];
    assign inv_vpn3_sv48            = inv_addr_i[47:39];
    assign inv_vpn0_sv48x4          = inv_addr_i[20:12];
    assign inv_vpn1_sv48x4          = inv_addr_i[29:21];
    assign inv_vpn2_sv48x4          = inv_addr_i[38:30];
    assign inv_vpn3_sv48x4          = inv_addr_i[49:39];
    assign inv_vpn0_sv57            = inv_addr_i[20:12];
    assign inv_vpn1_sv57            = inv_addr_i[29:21];
    assign inv_vpn2_sv57            = inv_addr_i[38:30];
    assign inv_vpn3_sv57            = inv_addr_i[47:39];
    assign inv_vpn4_sv57            = inv_addr_i[56:48];
    assign inv_vpn0_sv57x4          = inv_addr_i[20:12];
    assign inv_vpn1_sv57x4          = inv_addr_i[29:21];
    assign inv_vpn2_sv57x4          = inv_addr_i[38:30];
    assign inv_vpn3_sv57x4          = inv_addr_i[47:39];
    assign inv_vpn4_sv57x4          = inv_addr_i[58:48];
    //===                           
    assign inval_ddt_i              = inv_req_i.itype == iommu_acd_pkg::INVTYPE_INVALID_DDT;
    assign inval_pdt_i              = inv_req_i.itype == iommu_acd_pkg::INVTYPE_INVALID_PDT;
    assign inv_process_id_i         = inv_req_i.pid_pscid;
    assign inv_device_id_i          = inv_req_i.did_gscid;
    assign inv_dv_i                 = inv_req_i.dv_gv;
    assign device_id_inv_hit        = (inv_device_id_i  == hit_tag_device_id);
    assign process_id_inv_hit       = (inv_process_id_i == hit_tag_process_id);
    //===                           
    assign vma_i                    = inv_req_i.itype == iommu_acd_pkg::INVTYPE_VMA;
    assign gvma_i                   = inv_req_i.itype == iommu_acd_pkg::INVTYPE_GVMA;
    assign inv_av_i                 = inv_req_i.av;
    assign inv_pscid_i              = inv_req_i.pid_pscid;
    assign inv_pscv_i               = inv_req_i.pscv;
    assign inv_gscid_i              = inv_req_i.did_gscid[15:0];
    assign inv_gv_i                 = inv_req_i.dv_gv;
    assign gscid_inv_hit            =  ~inv_gv_i                      // if VMA and GV==0, no GSCID matching required; if GVMA and GV==0, no GSCID matching required
                                     | (inv_gscid_i == inv_tag_GSCID);// if VMA and GV==1, GSCID match required; if GVMA and GV==1, GSCID matching required
    assign pscid_inv_hit            = ~inv_pscv_i                                     // if VMA and PSCV==0, invalid for all host address spaces
                                     | (~content_G & (inv_pscid_i == inv_tag_PSCID)); // if VMA and PSCV==1, invalid host address spaces identified by PSCID, escept for entries containing Global mappings
    //===
    always@(*) begin
        inv_tag_gpa = {2'b0, inv_tag_GPPN};
        if(~s1_bare) begin          // S1 not Bare, so inv_tag.GPPN is valid GPA
            if(is_2m) begin         // 2M or SV32-4M, gpa[0] from iova
                if(s1_sv32) inv_tag_gpa[21:12] = hit_tag_va[21:12];
                else        inv_tag_gpa[20:12] = hit_tag_va[20:12];
            end
            else if(is_1g) begin    // 1G or SV32-4G, gpa[1:0] from iova
                if(s1_sv32) inv_tag_gpa[33:12] = hit_tag_va[33:12];
                else        inv_tag_gpa[29:12] = hit_tag_va[29:12];
            end
            else if(is_512g) begin  // 512G, gpa[2:0] from iova
                inv_tag_gpa[38:12]  = hit_tag_va[38:12];
            end
        end
        else                        // S1 is Bare, inv_tag.GPPN is not valid GPA, the GPA is IOVA and stored in hit_tag
            inv_tag_gpa = hit_tag_va;
    end
    //===
    always@(*) begin
        inv_vpn0_hit = 1'b0;
        inv_vpn1_hit = 1'b0;
        inv_vpn2_hit = 1'b0;
        inv_vpn3_hit = 1'b0;
        inv_vpn4_hit = 1'b0;
        if(gvma_i & ~s2_bare) begin
            inv_vpn0_hit = ((s2_sv32x4 & (inv_vpn0_sv32x4 == inv_tag_gpa[21:12]))   | (inv_vpn0_sv39x4 == inv_tag_gpa[20:12]));
            inv_vpn1_hit = ((s2_sv32x4 & (inv_vpn1_sv32x4 == inv_tag_gpa[33:22]))   | (inv_vpn1_sv39x4 == inv_tag_gpa[29:21]));
            inv_vpn2_hit = ( s2_sv32x4                                              | (s2_sv39x4 ? (inv_vpn2_sv39x4 == inv_tag_gpa[40:30]) : (inv_vpn2_sv48x4 == inv_tag_gpa[38:30])));
            inv_vpn3_hit = ((s2_sv32x4 | s2_sv39x4)                                 | (s2_sv48x4 ? (inv_vpn3_sv48x4 == inv_tag_gpa[49:39]) : (inv_vpn3_sv57x4 == inv_tag_gpa[47:39])));
            inv_vpn4_hit = ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)                     | (inv_vpn4_sv57x4 == inv_tag_gpa[58:48]));
        end
        else if(vma_i & ~s1_bare) begin
            inv_vpn0_hit = ((s1_sv32   & (inv_vpn0_sv32   == hit_tag_va[21:12]))    | (inv_vpn0_sv39 == hit_tag_va[20:12]));
            inv_vpn1_hit = ((s1_sv32   & (inv_vpn1_sv32   == hit_tag_va[31:22]))    | (inv_vpn1_sv39 == hit_tag_va[29:21]));
            inv_vpn2_hit = ( s1_sv32                                                | (inv_vpn2_sv39 == hit_tag_va[38:30]));
            inv_vpn3_hit = ((s1_sv32   | s1_sv39)                                   | (inv_vpn3_sv48 == hit_tag_va[47:39]));
            inv_vpn4_hit = ((s1_sv32   | s1_sv39 | s1_sv48)                         | (inv_vpn4_sv57 == hit_tag_va[56:48]));
        end
    end
    assign addr_inv_hit = (inv_vpn0_hit & inv_vpn1_hit & inv_vpn2_hit & inv_vpn3_hit & inv_vpn4_hit);
    assign inv_hit      = inv_req_valid_i & (
                              (vma_i       & gscid_inv_hit     & pscid_inv_hit & (~inv_av_i | addr_inv_hit) )   // when AV==1, only invalid entries contains first-stage leaf PTE, but there's only leaf PTE in ACD
                            | (gvma_i      & gscid_inv_hit                     & (~inv_av_i | addr_inv_hit) )   // when AV==1, only invlaid leaf second PTE, but there's only leaf PTE in ACD
                            | (inval_ddt_i & (~inv_dv_i | device_id_inv_hit)                                )   // if DV==0, invalid all DDT and PDT for all devices; if DV==1, invalid leaf level DDT identified by DID, but there's only leaf DDT in ACD
                            | (inval_pdt_i & device_id_inv_hit & process_id_inv_hit                         )
                          );  // if INVALD_PDT, invalid leaf PDT for the specified  PID & DID, but there's only leaf PDT in ACD

//}}}

//}}}

    assign inv_hit_o = inv_hit;

endmodule
//}}}
