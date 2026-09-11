///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_tw_apb.v
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
module iommu_tw_apb
(
    input                           iommu_clk,
    input                           iommu_rstn,

    input                           iommu_penable_i,
    input                           iommu_apb_wen_i,
    input  [11:0]                   iommu_apb_addr_i,
    input  [31:0]                   iommu_apb_wdata_i,
    input                           iommu_apb_ren_i,

    output [31:0]                   iommu_apb_rdata_o,
    output                          iommu_apb_ready_o,
    output                          iommu_apb_slverr_o,

    output      [63:0]              iommu_caps_o,
    output      [3:0]               iommu_ddtp_mode_o,
    output      [43:0]              iommu_ddtp_ppn_o,
    output      [11:0]              iommu_fctl_o,

    output                          iommu_ipsr_pmip_clr_o,
    output                          iommu_hpmcnt_cy_o,
    output      [30:0]              iommu_iocntinh_o,
    output      [63:0]              iommu_hpmctr_o[30:0],
    output      [63:0]              iommu_hpmevt_o[30:0]
    );


//***************************************************************//
//signal defination
//***************************************************************//
    localparam [63:0]   iommu_caps  = 64'h1f8_c7e68610;

    logic                   iommu_apb_ready;
    logic                   iommu_apb_wen;
    logic                   iommu_apb_ren;
    logic                   iommu_apb_en;
    logic [31:0]            iommu_apb_rdata;

    logic [11:0]            iommu_fctl;
    logic [47:0]            iommu_ddtp;
    logic [31:0]            iommu_iocntinh;

    logic                   iommu_ipsr_pmip_clr;
    logic [63:0]            iommu_hpmcycles;
    logic [63:0]            iommu_hpmctr[30:0];
    logic [63:0]            iommu_hpmevt[30:0];


assign iommu_caps_o = iommu_caps;
assign iommu_ddtp_mode_o = iommu_ddtp[3:0];
assign iommu_ddtp_ppn_o = iommu_ddtp[47:4];
assign iommu_fctl_o = iommu_fctl;

assign iommu_ipsr_pmip_clr_o = iommu_ipsr_pmip_clr;
assign iommu_iocntinh_o = iommu_iocntinh[31:1];
assign iommu_hpmctr_o = iommu_hpmctr;
assign iommu_hpmevt_o = iommu_hpmevt;


assign iommu_apb_rdata_o = iommu_apb_rdata;
assign iommu_apb_ready_o = iommu_apb_ready;
assign iommu_apb_slverr_o = iommu_apb_wen || iommu_apb_ren;

assign iommu_apb_wen = iommu_apb_wen_i && iommu_penable_i && (
                            (iommu_apb_addr_i == 12'h014) ||
                            (iommu_apb_addr_i == 12'h2b0));//empty

assign iommu_apb_ren = iommu_apb_ren_i && iommu_penable_i && (
                            (iommu_apb_addr_i == 12'h000) ||
                            (iommu_apb_addr_i == 12'h004) ||
                            (iommu_apb_addr_i == 12'h014) ||
                            (iommu_apb_addr_i == 12'h060) ||
                            (iommu_apb_addr_i == 12'h064));


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_apb_en <= 1'b0;
    else
        iommu_apb_en <= iommu_apb_wen || iommu_apb_ren;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_apb_ready <= 1'b0;
    else if ((iommu_apb_wen && !iommu_apb_en) || (iommu_apb_ren && !iommu_apb_en))
        iommu_apb_ready <= 1'b1;
    else
        iommu_apb_ready <= 1'b0;
end


//***********************************************************//
//system config
//***********************************************************//
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        iommu_fctl <= 12'd0;
        iommu_ddtp <= 48'd0;
        iommu_ipsr_pmip_clr <= 1'b0;
        iommu_iocntinh <= 32'd0;
        for (int i = 0;i < 31; i++) begin
            iommu_hpmctr[i] <= 64'd0;
            iommu_hpmevt[i] <= 64'd0;
        end
    end else if (iommu_apb_wen_i)
        case (iommu_apb_addr_i)
            12'h008:
            begin
                iommu_fctl[2:0] <= iommu_apb_wdata_i[2:0];
                iommu_fctl[11:3] <= iommu_apb_wdata_i[24:16];
            end
            12'h010:
            begin
                iommu_ddtp[3:0] <= iommu_apb_wdata_i[3:0];
                iommu_ddtp[25:4] <= iommu_apb_wdata_i[31:10];
            end
            12'h014:
                iommu_ddtp[47:26] <= iommu_apb_wdata_i[21:0];
            12'h054:
                iommu_ipsr_pmip_clr <= iommu_apb_wdata_i[2];
            12'h05c: iommu_iocntinh <= iommu_apb_wdata_i;
            12'h068 : iommu_hpmctr[0][31:0]  <= iommu_apb_wdata_i;
            12'h070 : iommu_hpmctr[1][31:0]  <= iommu_apb_wdata_i;
            12'h078 : iommu_hpmctr[2][31:0]  <= iommu_apb_wdata_i;
            12'h080 : iommu_hpmctr[3][31:0]  <= iommu_apb_wdata_i;
            12'h088 : iommu_hpmctr[4][31:0]  <= iommu_apb_wdata_i;
            12'h090 : iommu_hpmctr[5][31:0]  <= iommu_apb_wdata_i;
            12'h098 : iommu_hpmctr[6][31:0]  <= iommu_apb_wdata_i;
            12'h0a0 : iommu_hpmctr[7][31:0]  <= iommu_apb_wdata_i;
            12'h0a8 : iommu_hpmctr[8][31:0]  <= iommu_apb_wdata_i;
            12'h0b0 : iommu_hpmctr[9][31:0]  <= iommu_apb_wdata_i;
            12'h0b8 : iommu_hpmctr[10][31:0] <= iommu_apb_wdata_i;
            12'h0c0 : iommu_hpmctr[11][31:0] <= iommu_apb_wdata_i;
            12'h0c8 : iommu_hpmctr[12][31:0] <= iommu_apb_wdata_i;
            12'h0d0 : iommu_hpmctr[13][31:0] <= iommu_apb_wdata_i;
            12'h0d8 : iommu_hpmctr[14][31:0] <= iommu_apb_wdata_i;
            12'h0e0 : iommu_hpmctr[15][31:0] <= iommu_apb_wdata_i;
            12'h0e8 : iommu_hpmctr[16][31:0] <= iommu_apb_wdata_i;
            12'h0f0 : iommu_hpmctr[17][31:0] <= iommu_apb_wdata_i;
            12'h0f8 : iommu_hpmctr[18][31:0] <= iommu_apb_wdata_i;
            12'h100 : iommu_hpmctr[19][31:0] <= iommu_apb_wdata_i;
            12'h108 : iommu_hpmctr[20][31:0] <= iommu_apb_wdata_i;
            12'h110 : iommu_hpmctr[21][31:0] <= iommu_apb_wdata_i;
            12'h118 : iommu_hpmctr[22][31:0] <= iommu_apb_wdata_i;
            12'h120 : iommu_hpmctr[23][31:0] <= iommu_apb_wdata_i;
            12'h128 : iommu_hpmctr[24][31:0] <= iommu_apb_wdata_i;
            12'h130 : iommu_hpmctr[25][31:0] <= iommu_apb_wdata_i;
            12'h138 : iommu_hpmctr[26][31:0] <= iommu_apb_wdata_i;
            12'h140 : iommu_hpmctr[27][31:0] <= iommu_apb_wdata_i;
            12'h148 : iommu_hpmctr[28][31:0] <= iommu_apb_wdata_i;
            12'h150 : iommu_hpmctr[29][31:0] <= iommu_apb_wdata_i;
            12'h158 : iommu_hpmctr[30][31:0] <= iommu_apb_wdata_i;
            12'h06c : iommu_hpmctr[0][63:32]  <= iommu_apb_wdata_i;
            12'h074 : iommu_hpmctr[1][63:32]  <= iommu_apb_wdata_i;
            12'h07c : iommu_hpmctr[2][63:32]  <= iommu_apb_wdata_i;
            12'h084 : iommu_hpmctr[3][63:32]  <= iommu_apb_wdata_i;
            12'h08c : iommu_hpmctr[4][63:32]  <= iommu_apb_wdata_i;
            12'h094 : iommu_hpmctr[5][63:32]  <= iommu_apb_wdata_i;
            12'h09c : iommu_hpmctr[6][63:32]  <= iommu_apb_wdata_i;
            12'h0a4 : iommu_hpmctr[7][63:32]  <= iommu_apb_wdata_i;
            12'h0ac : iommu_hpmctr[8][63:32]  <= iommu_apb_wdata_i;
            12'h0b4 : iommu_hpmctr[9][63:32]  <= iommu_apb_wdata_i;
            12'h0bc : iommu_hpmctr[10][63:32] <= iommu_apb_wdata_i;
            12'h0c4 : iommu_hpmctr[11][63:32] <= iommu_apb_wdata_i;
            12'h0cc : iommu_hpmctr[12][63:32] <= iommu_apb_wdata_i;
            12'h0d4 : iommu_hpmctr[13][63:32] <= iommu_apb_wdata_i;
            12'h0dc : iommu_hpmctr[14][63:32] <= iommu_apb_wdata_i;
            12'h0e4 : iommu_hpmctr[15][63:32] <= iommu_apb_wdata_i;
            12'h0ec : iommu_hpmctr[16][63:32] <= iommu_apb_wdata_i;
            12'h0f4 : iommu_hpmctr[17][63:32] <= iommu_apb_wdata_i;
            12'h0fc : iommu_hpmctr[18][63:32] <= iommu_apb_wdata_i;
            12'h104 : iommu_hpmctr[19][63:32] <= iommu_apb_wdata_i;
            12'h10c : iommu_hpmctr[20][63:32] <= iommu_apb_wdata_i;
            12'h114 : iommu_hpmctr[21][63:32] <= iommu_apb_wdata_i;
            12'h11c : iommu_hpmctr[22][63:32] <= iommu_apb_wdata_i;
            12'h124 : iommu_hpmctr[23][63:32] <= iommu_apb_wdata_i;
            12'h12c : iommu_hpmctr[24][63:32] <= iommu_apb_wdata_i;
            12'h134 : iommu_hpmctr[25][63:32] <= iommu_apb_wdata_i;
            12'h13c : iommu_hpmctr[26][63:32] <= iommu_apb_wdata_i;
            12'h144 : iommu_hpmctr[27][63:32] <= iommu_apb_wdata_i;
            12'h14c : iommu_hpmctr[28][63:32] <= iommu_apb_wdata_i;
            12'h154 : iommu_hpmctr[29][63:32] <= iommu_apb_wdata_i;
            12'h15c : iommu_hpmctr[30][63:32] <= iommu_apb_wdata_i;
            12'h160 : iommu_hpmevt[0][31:0]   <= iommu_apb_wdata_i;
            12'h168 : iommu_hpmevt[1][31:0]   <= iommu_apb_wdata_i;
            12'h170 : iommu_hpmevt[2][31:0]   <= iommu_apb_wdata_i;
            12'h178 : iommu_hpmevt[3][31:0]   <= iommu_apb_wdata_i;
            12'h180 : iommu_hpmevt[4][31:0]   <= iommu_apb_wdata_i;
            12'h188 : iommu_hpmevt[5][31:0]   <= iommu_apb_wdata_i;
            12'h190 : iommu_hpmevt[6][31:0]   <= iommu_apb_wdata_i;
            12'h198 : iommu_hpmevt[7][31:0]   <= iommu_apb_wdata_i;
            12'h1a0 : iommu_hpmevt[8][31:0]   <= iommu_apb_wdata_i;
            12'h1a8 : iommu_hpmevt[9][31:0]   <= iommu_apb_wdata_i;
            12'h1b0 : iommu_hpmevt[10][31:0]  <= iommu_apb_wdata_i;
            12'h1b8 : iommu_hpmevt[11][31:0]  <= iommu_apb_wdata_i;
            12'h1c0 : iommu_hpmevt[12][31:0]  <= iommu_apb_wdata_i;
            12'h1c8 : iommu_hpmevt[13][31:0]  <= iommu_apb_wdata_i;
            12'h1d0 : iommu_hpmevt[14][31:0]  <= iommu_apb_wdata_i;
            12'h1d8 : iommu_hpmevt[15][31:0]  <= iommu_apb_wdata_i;
            12'h1e0 : iommu_hpmevt[16][31:0]  <= iommu_apb_wdata_i;
            12'h1e8 : iommu_hpmevt[17][31:0]  <= iommu_apb_wdata_i;
            12'h1f0 : iommu_hpmevt[18][31:0]  <= iommu_apb_wdata_i;
            12'h1f8 : iommu_hpmevt[19][31:0]  <= iommu_apb_wdata_i;
            12'h200 : iommu_hpmevt[20][31:0]  <= iommu_apb_wdata_i;
            12'h208 : iommu_hpmevt[21][31:0]  <= iommu_apb_wdata_i;
            12'h210 : iommu_hpmevt[22][31:0]  <= iommu_apb_wdata_i;
            12'h218 : iommu_hpmevt[23][31:0]  <= iommu_apb_wdata_i;
            12'h220 : iommu_hpmevt[24][31:0]  <= iommu_apb_wdata_i;
            12'h228 : iommu_hpmevt[25][31:0]  <= iommu_apb_wdata_i;
            12'h230 : iommu_hpmevt[26][31:0]  <= iommu_apb_wdata_i;
            12'h238 : iommu_hpmevt[27][31:0]  <= iommu_apb_wdata_i;
            12'h240 : iommu_hpmevt[28][31:0]  <= iommu_apb_wdata_i;
            12'h248 : iommu_hpmevt[29][31:0]  <= iommu_apb_wdata_i;
            12'h250 : iommu_hpmevt[30][31:0]  <= iommu_apb_wdata_i;
            12'h164 : iommu_hpmevt[0][63:32]  <= iommu_apb_wdata_i;
            12'h16c : iommu_hpmevt[1][63:32]  <= iommu_apb_wdata_i;
            12'h174 : iommu_hpmevt[2][63:32]  <= iommu_apb_wdata_i;
            12'h17c : iommu_hpmevt[3][63:32]  <= iommu_apb_wdata_i;
            12'h184 : iommu_hpmevt[4][63:32]  <= iommu_apb_wdata_i;
            12'h18c : iommu_hpmevt[5][63:32]  <= iommu_apb_wdata_i;
            12'h194 : iommu_hpmevt[6][63:32]  <= iommu_apb_wdata_i;
            12'h19c : iommu_hpmevt[7][63:32]  <= iommu_apb_wdata_i;
            12'h1a4 : iommu_hpmevt[8][63:32]  <= iommu_apb_wdata_i;
            12'h1ac : iommu_hpmevt[9][63:32]  <= iommu_apb_wdata_i;
            12'h1b4 : iommu_hpmevt[10][63:32] <= iommu_apb_wdata_i;
            12'h1bc : iommu_hpmevt[11][63:32] <= iommu_apb_wdata_i;
            12'h1c4 : iommu_hpmevt[12][63:32] <= iommu_apb_wdata_i;
            12'h1cc : iommu_hpmevt[13][63:32] <= iommu_apb_wdata_i;
            12'h1d4 : iommu_hpmevt[14][63:32] <= iommu_apb_wdata_i;
            12'h1dc : iommu_hpmevt[15][63:32] <= iommu_apb_wdata_i;
            12'h1e4 : iommu_hpmevt[16][63:32] <= iommu_apb_wdata_i;
            12'h1ec : iommu_hpmevt[17][63:32] <= iommu_apb_wdata_i;
            12'h1f4 : iommu_hpmevt[18][63:32] <= iommu_apb_wdata_i;
            12'h1fc : iommu_hpmevt[19][63:32] <= iommu_apb_wdata_i;
            12'h204 : iommu_hpmevt[20][63:32] <= iommu_apb_wdata_i;
            12'h20c : iommu_hpmevt[21][63:32] <= iommu_apb_wdata_i;
            12'h214 : iommu_hpmevt[22][63:32] <= iommu_apb_wdata_i;
            12'h21c : iommu_hpmevt[23][63:32] <= iommu_apb_wdata_i;
            12'h224 : iommu_hpmevt[24][63:32] <= iommu_apb_wdata_i;
            12'h22c : iommu_hpmevt[25][63:32] <= iommu_apb_wdata_i;
            12'h234 : iommu_hpmevt[26][63:32] <= iommu_apb_wdata_i;
            12'h23c : iommu_hpmevt[27][63:32] <= iommu_apb_wdata_i;
            12'h244 : iommu_hpmevt[28][63:32] <= iommu_apb_wdata_i;
            12'h24c : iommu_hpmevt[29][63:32] <= iommu_apb_wdata_i;
            12'h254 : iommu_hpmevt[30][63:32] <= iommu_apb_wdata_i;           
            default:
                begin
                    iommu_fctl <= iommu_fctl;
                    iommu_ddtp <= iommu_ddtp;
                    iommu_ipsr_pmip_clr <= iommu_ipsr_pmip_clr;
                    iommu_iocntinh <= iommu_iocntinh;
                end
            endcase
    else
        iommu_ipsr_pmip_clr <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_apb_rdata <= 32'd0;
    else if (iommu_apb_ren_i)
        case (iommu_apb_addr_i)
            12'h000:
                iommu_apb_rdata <= iommu_caps[31:0];//RO
            12'h004:
                iommu_apb_rdata <= iommu_caps[63:32];//RO
            12'h014:
                iommu_apb_rdata <= {10'd0,iommu_ddtp[47:26]};
            12'h060: iommu_apb_rdata <= iommu_hpmcycles[31:0];
            12'h064: iommu_apb_rdata <= iommu_hpmcycles[63:32];
            default: iommu_apb_rdata <= 32'd0;
        endcase
    else
        iommu_apb_rdata <= 32'd0;
end


assign iommu_hpmcnt_cy_o = iommu_hpmcycles[63];

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_hpmcycles <= 64'd0;
    else if (iommu_iocntinh[0] || iommu_ipsr_pmip_clr)
        iommu_hpmcycles <= 64'd0;
    else
        iommu_hpmcycles <= iommu_hpmcycles + 1'b1;
end


endmodule
