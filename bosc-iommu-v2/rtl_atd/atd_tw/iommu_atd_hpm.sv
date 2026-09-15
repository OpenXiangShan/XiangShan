///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   rv_iommu_hpm.sv
//Version       :   2.0
//Author        :   yangyy
//Description   :
//      xxx     :
//      xxxx    :
//      xx      :
//Increase: 2.1
//
//          2.2
///*********************************************//

module iommu_atd_hpm (
    input                           iommu_clk,
    input                           iommu_rstn,

    //between cdw and HPM
    input                           ddt_walk_i,
    input                           pdt_walk_i,
    input   [23:0]                  cdw_did_i,
    input                           cdw_pv_i,
    input   [19:0]                  cdw_pid_i,
    //between ptw and HPM
    input                           ptw_s1_walk_i,
    input                           ptw_s2_walk_i,
    input   [15:0]                  ptw_gscid_i,
    input   [19:0]                  ptw_pscid_i,

    input                           atd_pmip_clr_i,
    input                           atd_iocntinh_i,
    input   [63:0]                  atd_hpmctr_i,
    input   [63:0]                  atd_hpmevt_i,

    output                          atd_hpm_rvalid_o,
    output                          atd_iocntovf_o,
    output  [63:0]                  atd_hpmctr_o
);

    localparam  logic   [3:0]   DDTW    = 4'd5;
    localparam  logic   [3:0]   PDTW    = 4'd6;
    localparam  logic   [3:0]   S1_PTW  = 4'd7;
    localparam  logic   [3:0]   S2_PTW  = 4'd8;

typedef struct packed {
    logic                           evt_of;
    logic                           idt;
    logic                           dv_gscv;
    logic                           pv_pscv;
    logic [23:0]                    did_gscid;
    logic [19:0]                    pid_pscid;
    logic                           dmask;
    logic [14:0]                    eventID;
} iohpmevt_t;


    iohpmevt_t                      iohpmevt;
    logic                           atd_iocntinh;	
    logic                           atd_hpmctr_en;
    logic [63:0]                    atd_hpmctr;
    logic                           atd_hpm_rvalid;
    
    logic [19:0]                    pid_pscid_masked;
    logic [23:0]                    did_gscid_masked;
    logic [19:0]                    pid_masked;
    logic [23:0]                    did_masked;
    logic [19:0]                    pscid_masked;
    logic [15:0]                    gscid_masked;

    logic [19:0]                    pid_mask_bits;
    logic [23:0]                    did_mask_bits;

    
assign  iohpmevt = atd_hpmevt_i;

always@(*) begin
    pid_mask_bits = 20'hfffff;
    for(int unsigned i=20; i>0; i--) begin
        if(iohpmevt.dmask & (iohpmevt.pid_pscid[i-1]==1'b0))
            pid_mask_bits = 20'hfffff << i;
    end
end

always@(*) begin
    did_mask_bits = 24'hffffff;
    for(int unsigned j=24; j>0; j--) begin
        if(iohpmevt.dmask & (iohpmevt.did_gscid[j-1]==1'b0))
            did_mask_bits = 24'hffffff << j;
    end
end

assign pid_pscid_masked = iohpmevt.pid_pscid & pid_mask_bits;
assign did_gscid_masked = iohpmevt.did_gscid & did_mask_bits;

assign pid_masked     = cdw_pid_i & pid_mask_bits;
assign did_masked     = cdw_did_i & did_mask_bits;
assign pscid_masked   = ptw_pscid_i & pid_mask_bits;
assign gscid_masked   = ptw_gscid_i & did_mask_bits[15:0];
    

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_hpmctr_en <= 1'b0;
    else if (atd_iocntinh_i)
        atd_hpmctr_en <= 1'b0;
    else
        case ({iohpmevt.idt,iohpmevt.dv_gscv,iohpmevt.pv_pscv})
        3'b000,3'b100:
            if ((ddt_walk_i && (iohpmevt.eventID == DDTW)) || (pdt_walk_i && cdw_pv_i && (iohpmevt.eventID == PDTW)) ||
                (ptw_s1_walk_i && (iohpmevt.eventID == S1_PTW)) || (ptw_s2_walk_i && (iohpmevt.eventID == S2_PTW)))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        3'b001:
            if (pdt_walk_i && cdw_pv_i && (pid_masked == pid_pscid_masked) && (iohpmevt.eventID == PDTW))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        3'b010:
            if (ddt_walk_i && (did_masked == did_gscid_masked) && (iohpmevt.eventID == DDTW))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        3'b011:
            if (pdt_walk_i && (iohpmevt.eventID == PDTW) && (did_masked == did_gscid_masked) && cdw_pv_i && (pid_masked == pid_pscid_masked))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        3'b101:
            if (ptw_s1_walk_i && (pscid_masked == pid_pscid_masked) && (iohpmevt.eventID == S1_PTW))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        3'b110:
            if (ptw_s2_walk_i && (gscid_masked == did_gscid_masked[15:0]) && (iohpmevt.eventID == S2_PTW))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        3'b111:
            if (ptw_s1_walk_i && ptw_s2_walk_i && (iohpmevt.eventID == S2_PTW) && (gscid_masked == did_gscid_masked[15:0]) && (pscid_masked == pid_pscid_masked))
                atd_hpmctr_en <= 1'b1;
            else
                atd_hpmctr_en <= 1'b0;
        default:
            atd_hpmctr_en <= 1'b0;
        endcase
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_iocntinh <= 1'b0;
    else 
        atd_iocntinh <= atd_iocntinh_i;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_hpmctr <= 64'd0;
//        atd_hpmctr <= 64'hffff_ffff_ffff_fffe;//test interrupt
    else if ((atd_iocntinh && !atd_iocntinh_i) || (atd_pmip_clr_i && atd_hpmctr[63]))
        atd_hpmctr <= atd_hpmctr_i;
    else if (!atd_iocntinh_i && atd_hpmctr_en)
        atd_hpmctr <= atd_hpmctr + 1;
    else
        atd_hpmctr <= atd_hpmctr;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_hpm_rvalid <= 1'b0;
    else if ((iohpmevt.eventID == DDTW) || (iohpmevt.eventID == PDTW) || (iohpmevt.eventID == S1_PTW) || (iohpmevt.eventID == S2_PTW))
        atd_hpm_rvalid <= 1'b1;
    else
        atd_hpm_rvalid <= 1'b0;
    end

assign atd_hpm_rvalid_o = atd_hpm_rvalid; 
assign atd_iocntovf_o = atd_hpmctr[63] && !iohpmevt.evt_of;
assign atd_hpmctr_o = atd_hpmctr;


endmodule
