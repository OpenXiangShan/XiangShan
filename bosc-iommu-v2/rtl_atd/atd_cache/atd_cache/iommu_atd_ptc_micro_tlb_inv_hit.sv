////////////////////////////////////////////////////////////////////////////
// iommu_atd_ptc_micro_tlb_inv_hit
////////////////////////////////////////////////////////////////////////////
module iommu_atd_ptc_micro_tlb_inv_hit #( //{{{
    parameter   CACHE_TYPE                  = 0, // 0:S2PTC, 1:S2PTC
    parameter type          INVALID_REQ_TYPE            = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter   SPARE_PARAM                 = 0
)(
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    input  logic                                        hit_tag_gv,
    input  logic [15:0]                                 hit_tag_gscid,
    input  logic [19:0]                                 hit_tag_pscid,
    input  logic [63:12]                                hit_tag_addr,
    input  logic                                        hit_tag_sxl,
    input  logic                                        csr_gxl,
    input  logic [2:0]                                  content_lvl,
    input  logic                                        content_n,
    input  logic                                        content_g,
    input  logic                                        content_leaf,
    output logic                                        inv_hit,
    input  logic                                        spare_in           
);
    logic                                               gvma_i,vma_i;
    logic [15:0]                                        inv_gscid_i;
    logic                                               inv_gv_i;
    logic [19:0]                                        inv_pscid_i;
    logic                                               inv_pv_i;
    logic [63:12]                                       inv_addr_i;
    logic                                               inv_av_i;

    logic [20:12] vpn0;
    logic [29:21] vpn1;
    logic [38:30] vpn2;
    logic [47:39] vpn3;
    logic [56:48] vpn4;
    logic [63:57] vpn5;
    logic [21:12] vpn0_sv32;
    logic [33:22] vpn1_sv32;

    logic [20:12] gpn0;
    logic [29:21] gpn1;
    logic [40:30] gpn2;
    logic [49:39] gpn3;
    logic [58:48] gpn4;
    logic [63:59] gpn5;
    logic [21:12] gpn0_sv32x4;
    logic [33:22] gpn1_sv32x4;

    logic gscid_hit, pscid_hit;
    logic vpn0_hit, vpn1_hit, vpn2_hit, vpn3_hit, vpn4_hit, vpn5_hit;

    assign vpn0          = inv_addr_i[20:12];
    assign vpn1          = inv_addr_i[29:21];
    assign vpn2          = inv_addr_i[38:30];
    assign vpn3          = inv_addr_i[47:39];
    assign vpn4          = inv_addr_i[56:48];
    assign vpn5          = inv_addr_i[63:57];
    assign vpn0_sv32     = inv_addr_i[21:12];
    assign vpn1_sv32     = inv_addr_i[33:22];

    assign gpn0          = inv_addr_i[20:12];
    assign gpn1          = inv_addr_i[29:21];
    assign gpn2          = inv_addr_i[40:30];
    assign gpn3          = inv_addr_i[49:39];
    assign gpn4          = inv_addr_i[58:48];
    assign gpn5          = inv_addr_i[63:59];
    assign gpn0_sv32x4   = inv_addr_i[21:12];
    assign gpn1_sv32x4   = inv_addr_i[33:22];

    assign gvma_i       = inv_req_i.itype == iommu_atd_cache_pkg::INVTYPE_GVMA;
    assign vma_i        = inv_req_i.itype == iommu_atd_cache_pkg::INVTYPE_VMA;
    assign inv_gscid_i  = inv_req_i.did_gscid[15:0];
    assign inv_gv_i     = inv_req_i.dv_gv;
    assign inv_pscid_i  = inv_req_i.pid_pscid;
    assign inv_pv_i     = inv_req_i.pscv;
    assign inv_addr_i   = inv_req_i.addr;
    assign inv_av_i     = inv_req_i.av;

generate
    if(CACHE_TYPE=='d0) begin : s2ptc_gscid_hit_gen
        assign gscid_hit    = inv_gscid_i==hit_tag_gscid;
    end
    else begin : s1ptc_gscid_hit_gen
        assign gscid_hit    = hit_tag_gv ? (inv_gscid_i==hit_tag_gscid) : 1'b1;
    end
endgenerate

    assign pscid_hit    = inv_pscid_i==hit_tag_pscid;

generate
    if(CACHE_TYPE=='d0) begin : s2ptc_vpn_hit_gen
        always@(*) begin
            vpn0_hit = 1'b0;
            vpn1_hit = 1'b0;
            vpn2_hit = 1'b0;
            vpn3_hit = 1'b0;
            vpn4_hit = 1'b0;
            vpn5_hit = 1'b0;
            if(csr_gxl) begin   // sv32x4
                vpn0_hit = content_lvl>'d0 ? 1'b1 :              // for pagesize>4k, no need vpn0 matching
                           (content_n ? (gpn0_sv32x4[21:16]==hit_tag_addr[21:16]) :(gpn0_sv32x4==hit_tag_addr[21:12]));
                vpn1_hit = (gpn1_sv32x4==hit_tag_addr[33:22]);
                vpn2_hit = (gpn2       ==hit_tag_addr[40:30]);
                vpn3_hit = (gpn3       ==hit_tag_addr[49:39]);
                vpn4_hit = (gpn4       ==hit_tag_addr[58:48]);
                vpn5_hit = (gpn5       ==hit_tag_addr[63:59]);
            end
            else begin          // sv39/48/57x4
                vpn0_hit = content_lvl>'d0 ? 1'b1 :              // for pagesize>4k, no need vpn0 matching
                           (content_n ? (gpn0[20:16]       ==hit_tag_addr[20:16]) : (gpn0       ==hit_tag_addr[20:12]));
                vpn1_hit = content_lvl>'d1 ? 1'b1 :              // for pagesize>2M, no need vpn1 matching
                           (gpn1       ==hit_tag_addr[29:21]);
                vpn2_hit = content_lvl>'d2 ? 1'b1 :              // for pagesize>1G, no need vpn1 matching
                           (gpn2       ==hit_tag_addr[40:30]);
                vpn3_hit = content_lvl>'d3 ? 1'b1 :              // for pagesize>512G, no need vpn1 matching
                           (gpn3       ==hit_tag_addr[49:39]);
                vpn4_hit = content_lvl>'d4 ? 1'b1 :              // for pagesize>256T, no need vpn1 matching
                           (gpn4       ==hit_tag_addr[58:48]);
                vpn5_hit = (gpn5       ==hit_tag_addr[63:59]);
            end
        end
    end
    else begin : s1ptc_vpn_hit_gen
        always@(*) begin
            vpn0_hit = 1'b0;
            vpn1_hit = 1'b0;
            vpn2_hit = 1'b0;
            vpn3_hit = 1'b0;
            vpn4_hit = 1'b0;
            vpn5_hit = 1'b0;
            if(hit_tag_sxl) begin // sv32
                vpn0_hit = content_lvl>'d0 ? 1'b1 :              // for pagesize>4k, no need vpn0 matching
                           (content_n ? (vpn0_sv32[21:16]  ==hit_tag_addr[21:16]) : (vpn0_sv32  ==hit_tag_addr[21:12]));
                vpn1_hit = (vpn1_sv32  ==hit_tag_addr[33:22]);
                vpn2_hit = (vpn2       ==hit_tag_addr[38:30]);
                vpn3_hit = (vpn3       ==hit_tag_addr[47:39]);
                vpn4_hit = (vpn4       ==hit_tag_addr[56:48]);
                vpn5_hit = (vpn5       ==hit_tag_addr[63:57]);
            end
            else begin
                vpn0_hit = content_lvl>'d0 ? 1'b1 :              // for pagesize>4k, no need vpn0 matching
                           (content_n ? (vpn0[20:16]       ==hit_tag_addr[20:16]) : (vpn0       ==hit_tag_addr[20:12]));
                vpn1_hit = content_lvl>'d1 ? 1'b1 :              // for pagesize>2M, no need vpn1 matching
                           (vpn1       ==hit_tag_addr[29:21]);
                vpn2_hit = content_lvl>'d2 ? 1'b1 :              // for pagesize>1G, no need vpn1 matching
                           (vpn2       ==hit_tag_addr[38:30]);
                vpn3_hit = content_lvl>'d3 ? 1'b1 :              // for pagesize>512G, no need vpn1 matching
                           (vpn3       ==hit_tag_addr[47:39]);
                vpn4_hit = content_lvl>'d4 ? 1'b1 :              // for pagesize>256T, no need vpn1 matching
                           (vpn4       ==hit_tag_addr[56:48]);
                vpn5_hit = (vpn5       ==hit_tag_addr[63:57]);
            end
        end
    end
endgenerate
    
    assign addr_hit = vpn0_hit & vpn1_hit & vpn2_hit & vpn3_hit & vpn4_hit & vpn5_hit;


generate
    if(CACHE_TYPE==0) begin : s2ptc_inv_hit_gen
        assign inv_hit      = inv_req_valid_i &
                              gvma_i &
                              (                                                                     // GV AV
                                (~inv_gv_i                                                      ) | //  0  ? , invalidates information cached from any level of the second-stage page table, for all VM address spaces
                                ( inv_gv_i & ~inv_av_i & gscid_hit                              ) | //  1  0 , invalidates information cached from any level of the second-stage page table, but only for VM address spaces identified by the GSCID operand
                                ( inv_gv_i &  inv_av_i & gscid_hit &  addr_hit & content_leaf   )   //  1  1 , invalidates information cached from leaf second-stage page table entries corresponding to the guest-physical-address in ADDR operand, for only VM address space identified by GSCID operand
                              );
    end
    else begin : s1ptc_inv_hit_gen
        assign inv_hit      = inv_req_valid_i &
                              vma_i &
                              (                                                                                                         // GV AV PSCV
                                (~inv_gv_i & ~inv_av_i & ~inv_pv_i                                                                  ) | //  0  0  0    , invalidates all address-translation cache entries, including those that contain global mapping, for all host address spaces.
                                (~inv_gv_i & ~inv_av_i &  inv_pv_i             & pscid_hit                            & ~content_g  ) | //  0  0  1    , invalidates all address-translation cache entries for the host address space identified by PSCID operand, except for entries containing global mapping.
                                (~inv_gv_i &  inv_av_i & ~inv_pv_i                         & addr_hit  & content_leaf               ) | //  0  1  0    , invalidates all address-translation cache entries that contain first-stage leaf page table entries, including those that contain global mapping, corresponding to the IOVA in ADDR operand, for all host address space
                                (~inv_gv_i &  inv_av_i &  inv_pv_i             & pscid_hit & addr_hit  & content_leaf & ~content_g  ) | //  0  1  1    , invalidates all address-translation cache entries that contain first-stage leaf page table entries, corresponding to the IOVA in ADDR operand and that match the host address space identified by PSCID operand. except for entries containging global mappings.
                                ( inv_gv_i & ~inv_av_i & ~inv_pv_i & gscid_hit                                                      ) | //  1  0  0    , invalidates all address-translation cache entries, including those that contain global mappings, for all VM address spaces associated with GSCID operand.
                                ( inv_gv_i & ~inv_av_i &  inv_pv_i & gscid_hit & pscid_hit                            & ~content_g  ) | //  1  0  1    , invalidates all address-translation cache entries for the VM address space identiried by PSCID and GSCID operands, excepts for entries containg global mappings.
                                ( inv_gv_i &  inv_av_i & ~inv_pv_i & gscid_hit             & addr_hit  & content_leaf               ) | //  1  1  0    , invalidates all address-translation cache entries that contain first-stage leaf page table entries, including those containg global mappings, corresponding to the IOVA in ADDR operand, for all VM address spaces associated with the GSCID operand.
                                ( inv_gv_i &  inv_av_i &  inv_pv_i & gscid_hit & pscid_hit & addr_hit  & content_leaf & ~content_g  )   //  1  1  1    , invalidates all address-translation cache entries that contain first-stage leaf page table entries corresponding to the IOVA in ADDR operand, for the VM address space identified by PSCID and GSCID operands, except for entries containing global mapping.
                              );
    end
endgenerate



endmodule
//}}}




