///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_sw_apb.v
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
module iommu_sw_apb
(
    input                           iommu_clk,
    input                           iommu_rstn,

    input                           iommu_penable_i,
    input                           iommu_apb_wen_i,
    input  [11:0]                   iommu_apb_addr_i,
    input  [31:0]                  iommu_apb_wdata_i,
    input                           iommu_apb_ren_i,

    output [31:0]                   iommu_apb_rdata_o,
    output                          iommu_apb_ready_o,
    output                          iommu_apb_slverr_o,

    input       [31:0]              iommu_cqh_i,//RO
    input       [31:0]              iommu_fqt_i,//RO
    input       [31:0]              iommu_pqt_i,//RO
    input       [5:0]               iommu_cqcsr_i,
    input       [3:0]               iommu_fqcsr_i,
    input       [3:0]               iommu_pqcsr_i,
    input       [3:0]               iommu_ipsr_i,

    output      [4:0]               iommu_cqb_size_o,
    output      [43:0]              iommu_cqb_ppn_o,
    output      [31:0]              iommu_cqt_o,
    output      [4:0]               iommu_fqb_size_o,
    output      [43:0]              iommu_fqb_ppn_o,
    output      [31:0]              iommu_fqh_o,
    output      [4:0]               iommu_pqb_size_o,
    output      [43:0]              iommu_pqb_ppn_o,
    output      [31:0]              iommu_pqh_o,
    output      [5:0]               iommu_cqcsr_o,
    output      [3:0]               iommu_fqcsr_o,
    output      [3:0]               iommu_pqcsr_o,
    output      [3:0]               iommu_ipsr_o,
    output      [15:0]              iommu_icvec_o,

    output      [55:0]              iommu_msi_addr_o[15:0],
    output      [31:0]              iommu_msi_data_o[15:0],
    output      [15:0]              iommu_msi_mask_o
    );



//***************************************************************//
//signal defination
//***************************************************************//
    logic                   iommu_apb_ready;
    logic                   iommu_apb_wen;
    logic                   iommu_apb_ren;
    logic                   iommu_apb_en;
    logic [31:0]            iommu_apb_rdata;

    logic [48:0]            iommu_cqb;
    logic [31:0]            iommu_cqt;
    logic [48:0]            iommu_fqb;
    logic [31:0]            iommu_fqh;
    logic [48:0]            iommu_pqb;
    logic [31:0]            iommu_pqh;
    logic [31:0]            iommu_cqcsr;
    logic [31:0]            iommu_fqcsr;
    logic [31:0]            iommu_pqcsr;
    logic [31:0]            iommu_ipsr;
    logic [63:0]            iommu_icvec;
    logic [55:0]            iommu_msi_addr[15:0];//56
    logic [31:0]            iommu_msi_data[15:0];//32
    logic [15:0]            iommu_msi_mask;


assign iommu_cqb_size_o = iommu_cqb[4:0];
assign iommu_cqb_ppn_o = iommu_cqb[48:5];
assign iommu_cqt_o = iommu_cqt;
assign iommu_fqb_size_o = iommu_fqb[4:0];
assign iommu_fqb_ppn_o = iommu_fqb[48:5];
assign iommu_fqh_o = iommu_fqh;
assign iommu_pqb_size_o = iommu_pqb[4:0];
assign iommu_pqb_ppn_o = iommu_pqb[48:5];
assign iommu_pqh_o = iommu_pqh;
assign iommu_cqcsr_o = {iommu_cqcsr[11:8],iommu_cqcsr[1:0]};
assign iommu_fqcsr_o = {iommu_fqcsr[9:8],iommu_fqcsr[1:0]};
assign iommu_pqcsr_o = {iommu_pqcsr[9:8],iommu_pqcsr[1:0]};
assign iommu_ipsr_o = iommu_ipsr[3:0];
assign iommu_icvec_o = iommu_icvec[15:0];
assign iommu_msi_addr_o = iommu_msi_addr;
assign iommu_msi_data_o = iommu_msi_data;
assign iommu_msi_mask_o = iommu_msi_mask;


assign iommu_apb_rdata_o = iommu_apb_rdata;
assign iommu_apb_ready_o = iommu_apb_ready;
assign iommu_apb_slverr_o = iommu_apb_wen || iommu_apb_ren;

assign iommu_apb_wen = iommu_apb_wen_i && iommu_penable_i && (
                            ((iommu_apb_addr_i >= 12'h300) && (iommu_apb_addr_i <= 12'h3fc)) ||
                            (iommu_apb_addr_i == 12'h018) || 
                            (iommu_apb_addr_i == 12'h01c) || 
                            (iommu_apb_addr_i == 12'h024) || 
                            (iommu_apb_addr_i == 12'h028) || 
                            (iommu_apb_addr_i == 12'h02c) || 
                            (iommu_apb_addr_i == 12'h030) || 
                            (iommu_apb_addr_i == 12'h038) || 
                            (iommu_apb_addr_i == 12'h03c) || 
                            (iommu_apb_addr_i == 12'h040) || 
                            (iommu_apb_addr_i == 12'h048) || 
                            (iommu_apb_addr_i == 12'h04c) || 
                            (iommu_apb_addr_i == 12'h050) || 
                            (iommu_apb_addr_i == 12'h2f8) || 
                            (iommu_apb_addr_i == 12'h2fc));

assign iommu_apb_ren = iommu_apb_ren_i && iommu_penable_i && (
                            ((iommu_apb_addr_i >= 12'h300) && (iommu_apb_addr_i <= 12'h3fc)) ||
                            (iommu_apb_addr_i == 12'h018) || 
                            (iommu_apb_addr_i == 12'h01c) ||
                            (iommu_apb_addr_i == 12'h020) ||
                            (iommu_apb_addr_i == 12'h024) || 
                            (iommu_apb_addr_i == 12'h028) || 
                            (iommu_apb_addr_i == 12'h02c) || 
                            (iommu_apb_addr_i == 12'h030) ||
                            (iommu_apb_addr_i == 12'h034) ||
                            (iommu_apb_addr_i == 12'h038) || 
                            (iommu_apb_addr_i == 12'h03c) || 
                            (iommu_apb_addr_i == 12'h040) ||
                            (iommu_apb_addr_i == 12'h044) ||
                            (iommu_apb_addr_i == 12'h048) || 
                            (iommu_apb_addr_i == 12'h04c) || 
                            (iommu_apb_addr_i == 12'h050) || 
                            (iommu_apb_addr_i == 12'h054) || 
                            (iommu_apb_addr_i == 12'h2f8) || 
                            (iommu_apb_addr_i == 12'h2fc));

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
//for (i = 0;i < 16; i++) begin
//12'h300+i*16: iommu_msi_addr[i][31:0] <= iommu_apb_wdata_i;
//12'h304+i*16: iommu_msi_addr[i][55:32] <= iommu_apb_wdata_i[23:0];
//12'h308+i*16: iommu_msi_data[i] <= iommu_apb_wdata_i;
//12'h30c+i*16: iommu_msi_mask[i] <= iommu_apb_wdata_i[0];
//end
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        iommu_cqb <= 49'd0;
        iommu_cqt <= 32'd0;
        iommu_fqb <= 49'd0;
        iommu_fqh <= 32'd0;
        iommu_pqb <= 49'd0;
        iommu_pqh <= 32'd0;
        iommu_cqcsr <= 32'd0;
        iommu_fqcsr <= 32'd0;
        iommu_pqcsr <= 32'd0;
        iommu_ipsr <= 32'd0;
        iommu_icvec <= 64'd0;
        for (int i = 0;i < 16; i++) begin
            iommu_msi_addr[i] <= 56'd0;
            iommu_msi_data[i] <= 32'd0;
            iommu_msi_mask[i] <= 1'b0;
        end
    end else if (iommu_apb_wen_i) begin
        if (iommu_apb_addr_i >= 12'h300 && iommu_apb_addr_i <= 12'h3fc) begin
            case (iommu_apb_addr_i[3:0])
                4'h0: iommu_msi_addr[(iommu_apb_addr_i[7:0] - 8'h00) >> 4][31:0] <= iommu_apb_wdata_i;
                4'h4: iommu_msi_addr[(iommu_apb_addr_i[7:0] - 8'h04) >> 4][55:32] <= iommu_apb_wdata_i[23:0];
                4'h8: iommu_msi_data[(iommu_apb_addr_i[7:0] - 8'h08) >> 4] <= iommu_apb_wdata_i;
                4'hc: iommu_msi_mask[(iommu_apb_addr_i[7:0] - 8'h0c) >> 4] <= iommu_apb_wdata_i[0];
                default: 
                    for (int i = 0;i < 16; i++) begin
                        iommu_msi_addr[i] <= iommu_msi_addr[i];
                        iommu_msi_data[i] <= iommu_msi_data[i];
                        iommu_msi_mask[i] <= iommu_msi_mask[i];
                    end
            endcase
        end else case (iommu_apb_addr_i)
            12'h018:
            begin
                iommu_cqb[4:0] <= iommu_apb_wdata_i[4:0];
                iommu_cqb[26:5] <= iommu_apb_wdata_i[31:10];
            end
            12'h01c:
                iommu_cqb[48:27] <= iommu_apb_wdata_i[21:0];
            12'h024:
                iommu_cqt <= iommu_apb_wdata_i;
            12'h028:
            begin
                iommu_fqb[4:0] <= iommu_apb_wdata_i[4:0];
                iommu_fqb[26:5] <= iommu_apb_wdata_i[31:10];
            end
            12'h02c:
                iommu_fqb[48:27] <= iommu_apb_wdata_i[21:0];
            12'h030:
                iommu_fqh <= iommu_apb_wdata_i;
            12'h038:
            begin
                iommu_pqb[4:0] <= iommu_apb_wdata_i[4:0];
                iommu_pqb[26:5] <= iommu_apb_wdata_i[31:10];
            end
            12'h03c:
                iommu_pqb[48:27] <= iommu_apb_wdata_i[21:0];
            12'h040:
                iommu_pqh <= iommu_apb_wdata_i;
            12'h048:
            begin
                iommu_cqcsr[11:0] <= iommu_apb_wdata_i[11:0];
                //iommu_cqcsr[8] <= iommu_apb_wdata_i[8] ? 1'b0 : iommu_cqcsr[8];
                //iommu_cqcsr[9] <= iommu_apb_wdata_i[9] ? 1'b0 : iommu_cqcsr[9];
                //iommu_cqcsr[10] <= iommu_apb_wdata_i[10] ? 1'b0 : iommu_cqcsr[10];
                //iommu_cqcsr[11] <= iommu_apb_wdata_i[11] ? 1'b0 : iommu_cqcsr[11];
                iommu_cqcsr[15:12] <= iommu_apb_wdata_i[15:12];
                iommu_cqcsr[17:16] <= 2'd0;
                iommu_cqcsr[31:18] <= iommu_apb_wdata_i[31:18];
            end
            12'h04c:
            begin
                iommu_fqcsr[9:0] <= iommu_apb_wdata_i[9:0];
                //iommu_fqcsr[8] <= iommu_apb_wdata_i[8] ? 1'b0 : iommu_fqcsr[8];
                //iommu_fqcsr[9] <= iommu_apb_wdata_i[9] ? 1'b0 : iommu_fqcsr[9];
                iommu_fqcsr[15:10] <= iommu_apb_wdata_i[15:10];
                iommu_fqcsr[17:16] <= 2'd0;
                iommu_fqcsr[31:18] <= iommu_apb_wdata_i[31:18];
            end
            12'h050:
            begin
                iommu_pqcsr[9:0] <= iommu_apb_wdata_i[9:0];
                //iommu_pqcsr[8] <= iommu_apb_wdata_i[8] ? 1'b0 : iommu_pqcsr[8];
                //iommu_pqcsr[9] <= iommu_apb_wdata_i[9] ? 1'b0 : iommu_pqcsr[9];
                iommu_pqcsr[15:10] <= iommu_apb_wdata_i[15:10];
                iommu_pqcsr[17:16] <= 2'd0;
                iommu_pqcsr[31:18] <= iommu_apb_wdata_i[31:18];
            end
            12'h054:
            begin
                iommu_ipsr[3:0] <= iommu_apb_wdata_i[3:0];
//              iommu_ipsr[0] <= iommu_apb_wdata_i[0] ? 1'b0 : iommu_ipsr[0];
//              iommu_ipsr[1] <= iommu_apb_wdata_i[1] ? 1'b0 : iommu_ipsr[1];
//              iommu_ipsr[2] <= iommu_apb_wdata_i[2] ? 1'b0 : iommu_ipsr[2];
//              iommu_ipsr[3] <= iommu_apb_wdata_i[3] ? 1'b0 : iommu_ipsr[3];
                iommu_ipsr[31:4] <= 28'd0;
            end
            12'h2f8: iommu_icvec[31:0] <= iommu_apb_wdata_i;
            12'h2fc: iommu_icvec[63:32] <= iommu_apb_wdata_i;
            default:
            begin
                iommu_cqb <= iommu_cqb;
                iommu_cqt <= iommu_cqt;
                iommu_fqb <= iommu_fqb;
                iommu_fqh <= iommu_fqh;
                iommu_pqb <= iommu_pqb;
                iommu_pqh <= iommu_pqh;
                iommu_cqcsr <= iommu_cqcsr;
                iommu_fqcsr <= iommu_fqcsr;
                iommu_pqcsr <= iommu_pqcsr;
                iommu_ipsr <= iommu_ipsr;
                iommu_icvec <= iommu_icvec;
                for (int i = 0;i < 16; i++) begin
                    iommu_msi_addr[i] <= iommu_msi_addr[i];
                    iommu_msi_data[i] <= iommu_msi_data[i];
                    iommu_msi_mask[i] <= iommu_msi_mask[i];
                end
            end
        endcase
    end else begin
        iommu_cqcsr[11:8] <= 4'd0;
        iommu_fqcsr[9:8] <= 2'd0;
        iommu_pqcsr[9:8] <= 2'd0;
        iommu_ipsr[3:0] <= 4'd0;
    end
end


//for (j = 0;j < 16;j++) begin
//12'h300+j*16: iommu_apb_rdata <= iommu_msi_addr[j][31:0];
//12'h304+j*16: iommu_apb_rdata <= {18'd0,iommu_msi_addr[j][13:0]};
//12'h308+j*16: iommu_apb_rdata <= iommu_msi_data[j];
//12'h30c+j*16: iommu_apb_rdata <= {31'd0,iommu_msi_mask[j]};
//end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        iommu_apb_rdata <= 32'd0;
    end else if (iommu_apb_ren_i) begin
        if (iommu_apb_addr_i >= 12'h300 && iommu_apb_addr_i <= 12'h3fc)
            case (iommu_apb_addr_i[3:0])
                4'h0: iommu_apb_rdata <= iommu_msi_addr[(iommu_apb_addr_i[7:0] - 8'h00) >> 4][31:0];
                4'h4: iommu_apb_rdata <= {18'd0, iommu_msi_addr[(iommu_apb_addr_i[7:0] - 8'h04) >> 4][13:0]};
                4'h8: iommu_apb_rdata <= iommu_msi_data[(iommu_apb_addr_i[7:0] - 8'h08) >> 4];
                4'hc: iommu_apb_rdata <= {31'd0, iommu_msi_mask[(iommu_apb_addr_i[7:0] - 8'h0c) >> 4]};
                default: iommu_apb_rdata <= 32'd0;
            endcase
        else case (iommu_apb_addr_i)
            12'h018:
                iommu_apb_rdata <= {iommu_cqb[26:5],5'd0,iommu_cqb[4:0]};
            12'h01c:
                iommu_apb_rdata <= {10'd0,iommu_cqb[48:27]};
            12'h020:
                iommu_apb_rdata <= iommu_cqh_i;//RO
            12'h024:
                iommu_apb_rdata <= iommu_cqt;
            12'h028:
                iommu_apb_rdata <= {iommu_fqb[26:5],5'd0,iommu_fqb[4:0]};
            12'h02c:
                iommu_apb_rdata <= {10'd0,iommu_fqb[48:27]};
            12'h030:
                iommu_apb_rdata <= iommu_fqh;
            12'h034:
                iommu_apb_rdata <= iommu_fqt_i;//RO
            12'h038:
                iommu_apb_rdata <= {iommu_pqb[26:5],5'd0,iommu_pqb[4:0]};
            12'h03c:
                iommu_apb_rdata <= {10'd0,iommu_pqb[48:27]};
            12'h040:
                iommu_apb_rdata <= iommu_pqh;
            12'h044:
                iommu_apb_rdata <= iommu_pqt_i;//RO
            12'h048:
            begin
                iommu_apb_rdata[7:0] <= iommu_cqcsr[7:0];
                iommu_apb_rdata[11:8] <= iommu_cqcsr_i[3:0];//RO
                iommu_apb_rdata[15:12] <= iommu_cqcsr[15:12];
                iommu_apb_rdata[17:16] <= iommu_cqcsr_i[5:4];//RO
                iommu_apb_rdata[31:18] <= iommu_cqcsr[31:18];
            end
            12'h04c:
            begin
                iommu_apb_rdata[7:0] <= iommu_fqcsr[7:0];
                iommu_apb_rdata[9:8] <= iommu_fqcsr_i[1:0];
                iommu_apb_rdata[15:10] <= iommu_fqcsr[15:10];
                iommu_apb_rdata[17:16] <= iommu_fqcsr_i[3:2];//RO
                iommu_apb_rdata[31:18] <= iommu_fqcsr[31:18];
            end
            12'h050:
            begin
                iommu_apb_rdata[7:0] <= iommu_pqcsr[7:0];
                iommu_apb_rdata[9:8] <= iommu_pqcsr_i[1:0];
                iommu_apb_rdata[15:10] <= iommu_pqcsr[15:10];
                iommu_apb_rdata[17:16] <= iommu_pqcsr_i[3:2];//RO
                iommu_apb_rdata[31:18] <= iommu_pqcsr[31:18];
            end
            12'h054:
                iommu_apb_rdata <= {28'd0,iommu_ipsr_i};
            12'h2f8: iommu_apb_rdata <= iommu_icvec[31:0];
            12'h2fc: iommu_apb_rdata <= iommu_icvec[63:32];
            default:iommu_apb_rdata <= 32'd0;
        endcase
    end
    else
        iommu_apb_rdata <= 32'd0;
end


endmodule
