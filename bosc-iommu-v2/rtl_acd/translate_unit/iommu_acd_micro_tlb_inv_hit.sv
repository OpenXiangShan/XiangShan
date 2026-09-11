////////////////////////////////////////////////////////////////////////////
// iommu_acd_micro_tlb_inv_hit
////////////////////////////////////////////////////////////////////////////
module iommu_acd_micro_tlb_inv_hit import iommu_acd_pkg::*; #( //{{{
    parameter type          INVALID_REQ_TYPE            = logic,
    parameter   SPARE_PARAM  = 0
)(
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    input  logic                                        s1_bare,
    input  logic                                        s1_sv32,
    input  logic                                        s1_sv39,
    input  logic                                        s1_sv48,
    input  logic                                        s1_sv57,
    input  logic                                        s2_bare,
    input  logic                                        s2_sv32x4,
    input  logic                                        s2_sv39x4,
    input  logic                                        s2_sv48x4,
    input  logic                                        s2_sv57x4,
    input  logic                                        s1_2m,
    input  logic                                        s2_2m,
    input  logic                                        s1_1g,
    input  logic                                        s2_1g,
    input  logic                                        s1_512g,
    input  logic                                        s2_512g,
    input  logic [23:0]                                 hit_tag_device_id,
    input  logic [19:0]                                 hit_tag_process_id,
    input  logic [15:0]                                 inv_tag_GSCID,
    input  logic [19:0]                                 inv_tag_PSCID,
    input  logic [63:12]                                hit_tag_va,
    input  logic [61:12]                                inv_tag_GPPN,
    input  logic                                        content_n,
    input  logic                                        content_G,
    output logic                                        inv_hit,
    input  logic                                        spare_in           
);

    logic [63:12]                                       inv_tag_gpa;
    logic [63:12]                                       inv_tag_va;
    logic                                               inv_vpn0_hit;
    logic                                               inv_vpn1_hit;
    logic                                               inv_vpn2_hit;
    logic                                               inv_vpn3_hit;
    logic                                               inv_vpn4_hit;
    logic                                               addr_inv_hit;
                                                        
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

    assign inv_addr_i         = inv_req_i.addr;
    assign inv_vpn0_sv32      = inv_addr_i[21:12];
    assign inv_vpn1_sv32      = inv_addr_i[31:22];
    assign inv_vpn0_sv32x4    = inv_addr_i[21:12];
    assign inv_vpn1_sv32x4    = inv_addr_i[33:22];
    assign inv_vpn0_sv39      = inv_addr_i[20:12];
    assign inv_vpn1_sv39      = inv_addr_i[29:21];
    assign inv_vpn2_sv39      = inv_addr_i[38:30];
    assign inv_vpn0_sv39x4    = inv_addr_i[20:12];
    assign inv_vpn1_sv39x4    = inv_addr_i[29:21];
    assign inv_vpn2_sv39x4    = inv_addr_i[40:30];
    assign inv_vpn0_sv48      = inv_addr_i[20:12];
    assign inv_vpn1_sv48      = inv_addr_i[29:21];
    assign inv_vpn2_sv48      = inv_addr_i[38:30];
    assign inv_vpn3_sv48      = inv_addr_i[47:39];
    assign inv_vpn0_sv48x4    = inv_addr_i[20:12];
    assign inv_vpn1_sv48x4    = inv_addr_i[29:21];
    assign inv_vpn2_sv48x4    = inv_addr_i[38:30];
    assign inv_vpn3_sv48x4    = inv_addr_i[49:39];
    assign inv_vpn0_sv57      = inv_addr_i[20:12];
    assign inv_vpn1_sv57      = inv_addr_i[29:21];
    assign inv_vpn2_sv57      = inv_addr_i[38:30];
    assign inv_vpn3_sv57      = inv_addr_i[47:39];
    assign inv_vpn4_sv57      = inv_addr_i[56:48];
    assign inv_vpn0_sv57x4    = inv_addr_i[20:12];
    assign inv_vpn1_sv57x4    = inv_addr_i[29:21];
    assign inv_vpn2_sv57x4    = inv_addr_i[38:30];
    assign inv_vpn3_sv57x4    = inv_addr_i[47:39];
    assign inv_vpn4_sv57x4    = inv_addr_i[58:48];
    //===                     
    assign inval_ddt_i        = inv_req_i.itype == INVTYPE_INVALID_DDT;
    assign inval_pdt_i        = inv_req_i.itype == INVTYPE_INVALID_PDT;
    assign inv_process_id_i   = inv_req_i.pid_pscid;
    assign inv_device_id_i    = inv_req_i.did_gscid;
    assign inv_dv_i           = inv_req_i.dv_gv;
    assign device_id_inv_hit  =  ~inv_dv_i 
                               | (inv_device_id_i == hit_tag_device_id);
    assign process_id_inv_hit = (inv_process_id_i == hit_tag_process_id);
    //===                     
    assign vma_i              = inv_req_i.itype == INVTYPE_VMA;
    assign gvma_i             = inv_req_i.itype == INVTYPE_GVMA;
    assign inv_av_i           = inv_req_i.av;
    assign inv_pscid_i        = inv_req_i.pid_pscid;
    assign inv_pscv_i         = inv_req_i.pscv;
    assign inv_gscid_i        = inv_req_i.did_gscid[15:0];
    assign inv_gv_i           = inv_req_i.dv_gv;
// GSICD hit should concern S2MODE is BARE or not, PSCID hit should concern S1MODE is Bare or not when VMA.PSCV==1, 20251110
//    assign gscid_inv_hit      =  ~inv_gv_i                      // if VMA and GV==0, no GSCID matching required; if GVMA and GV==0, no GSCID matching required
//                               | (inv_gscid_i == inv_tag_GSCID);// if VMA and GV==1, GSCID match required; if GVMA and GV==1, GSCID matching required
    assign gscid_inv_hit      =  (vma_i & (   (~inv_gv_i &  s2_bare)                                    // if VMA and GV==0, no GSCID matching required;    (Only flush S1Only translation, 20251110)
                                            | ( inv_gv_i & ~s2_bare & (inv_gscid_i == inv_tag_GSCID))   // if VMA and GV==1, GSCID match required;          (Not  flush S1Only translation, 20251110)
                                          )
                                  )
                                |
                                 (gvma_i& (
                                              (~inv_gv_i & ~s2_bare)                                    // if GVMA and GV==0, no GSCID matching required;   (Not  flush S1only translation. 20251110)
                                            | ( inv_gv_i & ~s2_bare & (inv_gscid_i == inv_tag_GSCID))   // if GVMA and GV==0, GSCID match required;         (Not  flush S1only translation. 20251110)
                                           )
                                  );

    assign pscid_inv_hit      =  ~inv_pscv_i                                                // if VMA and PSCV==0, invalid for all host address spaces.                                                             (No  S1MOE & S2MODE concern)
                               | (~content_G & ~s1_bare & (inv_pscid_i == inv_tag_PSCID));  // if VMA and PSCV==1, invalid host address spaces identified by PSCID, escept for entries containing Global mappings.  (Not flush S2Only, 20251110)
    //===
    always@(*) begin
        inv_tag_gpa = (~s2_bare & s1_bare)  ? hit_tag_va            :       // S2_Only translation, GPN is stored in hit_tag_va
                      (~s2_bare & ~s1_bare) ? {2'b0, inv_tag_GPPN}  :       // S1S2_nested translation, GPN is stored in ing_tag_GPN
                                              'd0;
//        if(~s1_bare) begin          // S1 not Bare, so inv_tag.GPPN is valid GPA
//                                    // S2 may also Bare, then no inv_vpn*_hit need and inv_tag_gpa will be ignore
//            if(is_2m) begin         // 2M or SV32-4M, no match GPN[0] for inv_hit
//                if(s1_sv32) inv_tag_gpa[21:12] = inv_vpn0_sv32[21:12];
//                else        inv_tag_gpa[20:12] = inv_vpn0_sv39[20:12];
//            end
//            else if(is_1g) begin    // 1G , no match GPN[1] for inv_hit
//                if(s1_sv32) inv_tag_gpa[31:12] = inv_vpn0_;
//                else        inv_tag_gpa[29:12] = {inv_vpn1_sv39[29:21], inv_vpn0_sv39[20:12]};
//            end
//            else if(is_512g) begin  // 512G, gpa[2:0] from iova
//                inv_tag_gpa[38:12]  = hit_tag_va[38:12];
//            end
//        end
//        else                        // S1 is Bare, inv_tag.GPPN is not valid GPA, the GPA is IOVA and stored in hit_tag
//                                    // if S2 is also Bare, nomater use hit_tag_va, as inv_vpn?_hit will not asserted and the inv_tag_gpa is useless when s2_bare active
//            inv_tag_gpa = hit_tag_va;

        if(s2_2m) begin         // S2 is 2M/4M pagesize, no GPN[0] matching for inv_hit
            if(s2_sv32x4) inv_tag_gpa[21:12] = inv_vpn0_sv32x4;
            else          inv_tag_gpa[20:12] = inv_vpn0_sv39x4;
        end
        else if(s2_1g) begin
            inv_tag_gpa[20:12] = inv_vpn0_sv39x4;
            inv_tag_gpa[29:21] = inv_vpn1_sv39x4;
        end
        else if(s2_512g) begin
            inv_tag_gpa[20:12] = inv_vpn0_sv39x4;
            inv_tag_gpa[29:21] = inv_vpn1_sv39x4;
            inv_tag_gpa[38:30] = inv_vpn2_sv48x4;
        end
        else if(content_n) begin  // 4K pagesize with Svnapot active
            inv_tag_gpa[15:12] = inv_vpn0_sv39x4[15:12];
        end
    end

    always@(*) begin
        inv_tag_va = hit_tag_va;
        if(s1_2m) begin         // S1 is 2M/4M pagesize, no VPN[0] matching for inv_hit
            if(s1_sv32)   inv_tag_va[21:12] = inv_vpn0_sv32;
            else          inv_tag_va[20:12] = inv_vpn0_sv39;
        end
        else if(s1_1g) begin
            inv_tag_va[20:12] = inv_vpn0_sv39;
            inv_tag_va[29:21] = inv_vpn1_sv39;
        end
        else if(s1_512g) begin
            inv_tag_va[20:12] = inv_vpn0_sv39;
            inv_tag_va[29:21] = inv_vpn1_sv39;
            inv_tag_va[38:30] = inv_vpn2_sv48;
        end
        else if(content_n) begin  // 4K pagesize with Svnapot active
            inv_tag_va[15:12] = inv_vpn0_sv39[15:12];
        end
    end
    //===
    always@(*) begin
        inv_vpn0_hit = 1'b0;
        inv_vpn1_hit = 1'b0;
        inv_vpn2_hit = 1'b0;
        inv_vpn3_hit = 1'b0;
        inv_vpn4_hit = 1'b0;
        if(gvma_i) begin
            inv_vpn0_hit = ((s2_sv32x4 & (inv_vpn0_sv32x4 == inv_tag_gpa[21:12]))   | (~s2_sv32x4 & (inv_vpn0_sv39x4 == inv_tag_gpa[20:12]))                                            );
            inv_vpn1_hit = ((s2_sv32x4 & (inv_vpn1_sv32x4 == inv_tag_gpa[33:22]))   | (~s2_sv32x4 & (inv_vpn1_sv39x4 == inv_tag_gpa[29:21]))                                            );
            inv_vpn2_hit = ( s2_sv32x4                                              | ( s2_sv39x4 ? (inv_vpn2_sv39x4 == inv_tag_gpa[40:30]) : (inv_vpn2_sv48x4 == inv_tag_gpa[38:30]))  );
            inv_vpn3_hit = ((s2_sv32x4 | s2_sv39x4)                                 | ( s2_sv48x4 ? (inv_vpn3_sv48x4 == inv_tag_gpa[49:39]) : (inv_vpn3_sv57x4 == inv_tag_gpa[47:39]))  );
            inv_vpn4_hit = ((s2_sv32x4 | s2_sv39x4 | s2_sv48x4)                     | (inv_vpn4_sv57x4 == inv_tag_gpa[58:48]));
        end
        else if(vma_i) begin
            inv_vpn0_hit = ((s1_sv32   & (inv_vpn0_sv32   == inv_tag_va[21:12] ))   | (~s1_sv32   & (inv_vpn0_sv39   == inv_tag_va[20:12] ))    );
            inv_vpn1_hit = ((s1_sv32   & (inv_vpn1_sv32   == inv_tag_va[31:22] ))   | (~s1_sv32   & (inv_vpn1_sv39   == inv_tag_va[29:21] ))    );
            inv_vpn2_hit = ( s1_sv32                                                | (inv_vpn2_sv39 == inv_tag_va[38:30]));
            inv_vpn3_hit = ((s1_sv32   | s1_sv39  )                                 | (inv_vpn3_sv48 == inv_tag_va[47:39]));
            inv_vpn4_hit = ((s1_sv32   | s1_sv39   | s1_sv48  )                     | (inv_vpn4_sv57 == inv_tag_va[56:48]));
        end
    end
    assign addr_inv_hit =  ~inv_av_i 
                         | (gvma_i & ~inv_gv_i)                                                             // when GV==0, AV in GVMA is ignored, invalid information cached from any level of the second stage page table, for all VM address space
                         | (inv_vpn0_hit & inv_vpn1_hit & inv_vpn2_hit & inv_vpn3_hit & inv_vpn4_hit);
    assign inv_hit      = inv_req_valid_i & (
                              (vma_i       & gscid_inv_hit     & pscid_inv_hit & addr_inv_hit & ~s1_bare)   // when AV==1, only invalid entries contains first-stage leaf PTE, but there's only leaf PTE in ACD
                            | (gvma_i      & gscid_inv_hit                     & addr_inv_hit & ~s2_bare)   // when AV==1, only invlaid leaf second PTE, but there's only leaf PTE in ACD
                            | (inval_ddt_i & device_id_inv_hit                                          )   // if DV==0, invalid all DDT and PDT for all devices; if DV==1, invalid leaf level DDT identified by DID, but there's only leaf DDT in ACD
                            | (inval_pdt_i & device_id_inv_hit & process_id_inv_hit                     )   // if INVALD_PDT, invalid leaf PDT for the specified  PID & DID, but there's only leaf PDT in ACD
                          );  



endmodule
//}}}




