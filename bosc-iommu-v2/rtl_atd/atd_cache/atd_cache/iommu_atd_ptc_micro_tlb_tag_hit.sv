module iommu_atd_ptc_micro_tlb_tag_hit #(
    parameter   CACHE_TYPE  = 0, // 0:S2PTC, 1:S1PTC
    parameter   SPARE_PARAM = 1'b0
)(
    input  logic                        gv_i,
    input  logic [15:0]                 gscid_i,
    input  logic [19:0]                 pscid_i,
    input  logic [63:12]                addr_i,
    input  logic                        valid_i,
    input  logic                        hit_tag_gv,
    input  logic [15:0]                 hit_tag_gscid,
    input  logic [19:0]                 hit_tag_pscid,
    input  logic [63:12]                hit_tag_addr,
    input  logic                        hit_tag_sxl,
    input  logic                        csr_gxl,
    input  logic [2:0]                  content_lvl,
    input  logic                        content_n,
    output logic                        tag_hit
);
    logic gscid_hit, pscid_hit, addr_hit;

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

    logic vpn0_hit, vpn1_hit, vpn2_hit, vpn3_hit, vpn4_hit, vpn5_hit;

    assign vpn0          = addr_i[20:12];
    assign vpn1          = addr_i[29:21];
    assign vpn2          = addr_i[38:30];
    assign vpn3          = addr_i[47:39];
    assign vpn4          = addr_i[56:48];
    assign vpn5          = addr_i[63:57];
    assign vpn0_sv32     = addr_i[21:12];
    assign vpn1_sv32     = addr_i[33:22];

    assign gpn0          = addr_i[20:12];
    assign gpn1          = addr_i[29:21];
    assign gpn2          = addr_i[40:30];
    assign gpn3          = addr_i[49:39];
    assign gpn4          = addr_i[58:48];
    assign gpn5          = addr_i[63:59];
    assign gpn0_sv32x4   = addr_i[21:12];
    assign gpn1_sv32x4   = addr_i[33:22];

generate
    if(CACHE_TYPE=='d1) begin : s1ptc_gscid_hit_gen
        assign gscid_hit = (
                            ((gscid_i==hit_tag_gscid) & (gv_i & hit_tag_gv)) |   // S2 not BARE
                            (gv_i==1'b0 & hit_tag_gv==1'b0)
                           );
    end
    else begin : s2ptc_gscid_hit_gen
        assign gscid_hit = (gscid_i==hit_tag_gscid); // always match gscid in S2PTC
    end

endgenerate

    assign pscid_hit = CACHE_TYPE==0 ? 1'b1 :   // S2PTC, no pscid matching
                       (pscid_i==hit_tag_pscid);


generate
    if(CACHE_TYPE=='d0) begin : s2ptc_vpn_hit_gen
        always@(*) begin
            vpn0_hit = 1'b0;
            vpn1_hit = 1'b0;
            vpn2_hit = 1'b0;
            vpn3_hit = 1'b0;
            vpn4_hit = 1'b0;
            if(csr_gxl) begin   // sv32x4
                vpn0_hit = content_lvl>'d0 ? 1'b1 :              // for pagesize>4k, no need vpn0 matching
                           (content_n ? (gpn0_sv32x4[21:16]==hit_tag_addr[21:16]) : (gpn0_sv32x4==hit_tag_addr[21:12]));
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
    
    assign tag_hit  = addr_hit & gscid_hit & pscid_hit & valid_i;


endmodule
