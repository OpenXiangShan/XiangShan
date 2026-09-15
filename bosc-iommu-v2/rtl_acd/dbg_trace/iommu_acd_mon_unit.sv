/////////////////////////////////////////////////////
// iommu_acd_mon_unit
//  generate monitor unit design
/////////////////////////////////////////////////////
module iommu_acd_mon_unit_microtlb #(//{{{
//{{{ PARAM
    parameter   MICRO_TLB_IDX_WIDTH         = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    parameter   MICRO_TLB_DEPTH             = 2**MICRO_TLB_IDX_WIDTH,
    parameter type          MICROTLB_TAG_T              = iommu_acd_pkg::microtlb_tag_t,
    parameter type          MICROTLB_INV_TAG_T          = iommu_acd_pkg::microtlb_inv_tag_t,
    parameter type          MICROTLB_CONTENT_T          = iommu_acd_pkg::microtlb_content_t,
    parameter   SPARE_PARAM                 = 0
//}}}
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
                                                        
    input  logic                                        idbg_go_i,
    output logic                                        idbg_busy_o,
    input  logic [7:0]                                  idbg_opcode_i,
    input  logic [31:0]                                 idbg_dat_i,
    output logic [31:0]                                 idbg_dat_o,
    output logic                                        idbg_datv_o,
                                                        
    input  logic [MICRO_TLB_DEPTH-1:0]                  entry_valid_i,
    input  MICROTLB_TAG_T   [MICRO_TLB_DEPTH-1:0]       entry_hit_tag_i,
    input  MICROTLB_INV_TAG_T [MICRO_TLB_DEPTH-1:0]     entry_inv_tag_i,
    input  MICROTLB_CONTENT_T [MICRO_TLB_DEPTH-1:0]     entry_content_i,
                                                        
    input  logic                                        param_in
//}}}
);
//=== Declare === {{{
    localparam HIT_TAG_WIDTH = $bits(MICROTLB_TAG_T);
    localparam INV_TAG_WIDTH = $bits(MICROTLB_INV_TAG_T);
    localparam CONTENT_WIDTH = $bits(MICROTLB_CONTENT_T);
    localparam TOTAL_WIDTH   = ((HIT_TAG_WIDTH + INV_TAG_WIDTH + CONTENT_WIDTH + 1) < 32) ? 32 : (HIT_TAG_WIDTH + INV_TAG_WIDTH + CONTENT_WIDTH + 1);
    localparam IDBG_LENGTH   = (TOTAL_WIDTH%32==0) ? (TOTAL_WIDTH/32) : (TOTAL_WIDTH/32 + 1);

    localparam S_IDLE = 2'b00;
    localparam S_BUSY = 2'b01;

    logic [MICRO_TLB_IDX_WIDTH-1:0]                     idbg_entry_sel;

    logic [TOTAL_WIDTH-1:0]                             idbg_dat_muxed, idbg_dat_muxed_shifted;
    logic [31:0]                                        idbg_dat;
    logic [(TOTAL_WIDTH/32)+6:0]                        shift_value;

    logic [1:0] cs, ns;
    logic [(TOTAL_WIDTH/32):0]                          cnt;
//}}}

//=== Main Code === {{{
// FSM-Stage1
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
// FSM-Stage2
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(idbg_go_i)
                ns = S_BUSY;
            else
                ns = S_IDLE;
        end
        S_BUSY: begin
            if(cnt==IDBG_LENGTH)
                ns = S_IDLE;
            else
                ns = S_BUSY;
        end
        default: ns = cs;
        endcase
    end

    assign idbg_entry_sel = idbg_dat_i[MICRO_TLB_IDX_WIDTH-1:0];
    assign idbg_dat_muxed         = {
                                     entry_valid_i[idbg_entry_sel],
                                     entry_hit_tag_i[idbg_entry_sel],
                                     entry_inv_tag_i[idbg_entry_sel],
                                     entry_content_i[idbg_entry_sel]
                                     };
    always@(*) begin
        shift_value            = ((TOTAL_WIDTH/32)+6+1)'((IDBG_LENGTH-1-cnt) << 5);
        idbg_dat_muxed_shifted = idbg_dat_muxed >> shift_value;
        idbg_dat               = idbg_dat_muxed_shifted[31:0];
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            idbg_dat_o  <= 'd0;
            idbg_datv_o <= 'd0;
            cnt         <= 'd0;
        end
        else begin
            if(ns==S_BUSY) begin
                idbg_dat_o  <= idbg_dat;
                idbg_datv_o <= 1'b1;
                cnt         <= cnt + 'd1;
            end
            else begin
                idbg_datv_o <= 1'b0;
                cnt         <= 'd0;
            end
        end
    end

    assign idbg_busy_o = (cs != S_IDLE);
//}}}

endmodule//}}}



module iommu_acd_mon_unit_tlb_queue #(//{{{
//{{{ PARAM
    parameter   TRANS_QIDX_WIDTH            = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter   SPARE_PARAM                 = 0
//}}}
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
                                                        
    input  logic                                        idbg_go_i,
    output logic                                        idbg_busy_o,
    input  logic [7:0]                                  idbg_opcode_i,
    input  logic [31:0]                                 idbg_dat_i,
    output logic [31:0]                                 idbg_dat_o,
    output logic                                        idbg_datv_o,
                                                        
    input  logic [TLB_QUEUE_DEPTH-1:0]                  entry_valid_i,
    input  logic [3:0]                                  entry_info_fsm_i             [0:TLB_QUEUE_DEPTH-1],
    input  logic [TRANS_QIDX_WIDTH  :0]                 entry_info_tidx_i            [0:TLB_QUEUE_DEPTH-1],
    input  logic                                        entry_info_priv_i            [0:TLB_QUEUE_DEPTH-1],
    input  logic                                        entry_info_ext_i             [0:TLB_QUEUE_DEPTH-1],
    input  logic                                        entry_info_wr_i              [0:TLB_QUEUE_DEPTH-1],
    input  logic                                        entry_info_is_translated_i   [0:TLB_QUEUE_DEPTH-1],
    input  logic                                        entry_info_process_id_valid_i[0:TLB_QUEUE_DEPTH-1],
    input  logic [19:0]                                 entry_info_process_id_i      [0:TLB_QUEUE_DEPTH-1],
    input  logic [23:0]                                 entry_info_device_id_i       [0:TLB_QUEUE_DEPTH-1],
    input  logic [63:12]                                entry_info_va_i              [0:TLB_QUEUE_DEPTH-1],
                                                        
    input  logic                                        param_in
//}}}
);
//=== Declare === {{{
    localparam TOTAL_WIDTH = 4 + TRANS_QIDX_WIDTH+1 + 5 + 20 + 24 + 52 + 1;
    localparam IDBG_LENGTH = (TOTAL_WIDTH%32==0) ? (TOTAL_WIDTH/32) : (TOTAL_WIDTH/32 + 1);

    localparam S_IDLE = 2'b00;
    localparam S_BUSY = 2'b01;

    logic [TLB_QIDX_WIDTH-1:0]                          idbg_entry_sel;

    logic [TOTAL_WIDTH-1:0]                             idbg_dat_muxed, idbg_dat_muxed_shifted;
    logic [31:0]                                        idbg_dat;

    logic [1:0] cs, ns;
    logic [(TOTAL_WIDTH/32):0]                          cnt;
    logic [(TOTAL_WIDTH/32)+6:0]                        shift_value;
//}}}

//=== Main Code === {{{
// FSM-Stage1
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
// FSM-Stage2
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(idbg_go_i)
                ns = S_BUSY;
            else
                ns = S_IDLE;
        end
        S_BUSY: begin
            if(cnt==IDBG_LENGTH)
                ns = S_IDLE;
            else
                ns = S_BUSY;
        end
        default: ns = cs;
        endcase
    end

    assign idbg_entry_sel = idbg_dat_i[TLB_QIDX_WIDTH-1:0];
    assign idbg_dat_muxed = {
                                entry_valid_i                [idbg_entry_sel],
                                entry_info_fsm_i             [idbg_entry_sel],
                                entry_info_tidx_i            [idbg_entry_sel],
                                entry_info_priv_i            [idbg_entry_sel],
                                entry_info_ext_i             [idbg_entry_sel],
                                entry_info_wr_i              [idbg_entry_sel],
                                entry_info_is_translated_i   [idbg_entry_sel],
                                entry_info_process_id_valid_i[idbg_entry_sel],
                                entry_info_process_id_i      [idbg_entry_sel],
                                entry_info_device_id_i       [idbg_entry_sel],
                                entry_info_va_i              [idbg_entry_sel]
                                };
    always@(*) begin
        shift_value            = ((TOTAL_WIDTH/32)+6+1)'((IDBG_LENGTH-1-cnt) << 5);
        idbg_dat_muxed_shifted = idbg_dat_muxed >> shift_value;
        idbg_dat               = idbg_dat_muxed_shifted[31:0];
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            idbg_dat_o  <= 'd0;
            idbg_datv_o <= 'd0;
            cnt         <= 'd0;
        end
        else begin
            if(ns==S_BUSY) begin
                idbg_dat_o  <= idbg_dat;
                idbg_datv_o <= 1'b1;
                cnt         <= cnt + 'd1;
            end
            else begin
                idbg_datv_o <= 1'b0;
                cnt         <= 'd0;
            end
        end
    end
    
    assign idbg_busy_o = (cs != S_IDLE);
//}}}

endmodule//}}}



module iommu_acd_mon_unit_mtlb_ram #(//{{{
//{{{ PARAM
    parameter   BANK_4K_IDX_WIDTH           = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_IDX_WIDTH           = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_IDX_WIDTH           = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_IDX_WIDTH           = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    parameter   BANK_4K_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    parameter   BANK_2M_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    parameter   BANK_1G_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    parameter   BANK_0T_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    parameter   BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH,
    parameter   BANK_4K_WAY_NUM             = 2**BANK_4K_WAY_IDX_WIDTH,
    parameter   BANK_2M_WAY_NUM             = 2**BANK_2M_WAY_IDX_WIDTH,
    parameter   BANK_1G_WAY_NUM             = 2**BANK_1G_WAY_IDX_WIDTH,
    parameter   BANK_0T_WAY_NUM             = 2**BANK_0T_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_T                  = logic,
    parameter type          MTLB_ITAG_T                 = logic,
    parameter type          MTLB_UTAG_T                 = logic,
    parameter type          MTLB_DAT_T                  = logic,
    parameter   ECC_WIDTH                   = 7,
    parameter   TRAM_DAT_WIDTH              = $bits(MTLB_TAG_T)+ECC_WIDTH+1,
    parameter   IRAM_DAT_WIDTH              = $bits(MTLB_ITAG_T)+ECC_WIDTH+1,
    parameter   DRAM_DAT_WIDTH              = $bits(MTLB_DAT_T)+ECC_WIDTH+1,
    parameter   SPARE_PARAM                 = 0 
//}}}
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
                                                        
    input  logic                                        idbg_go_i,
    output logic                                        idbg_busy_o,
    input  logic [7:0]                                  idbg_opcode_i,
    input  logic [31:0]                                 idbg_dat_i,
    output logic [31:0]                                 idbg_dat_o,
    output logic                                        idbg_datv_o,
                                                        
    output logic                                                                        idbg_ongoing_o,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                mtlb_b00_tram_cs_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                mtlb_b00_tram_wr_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]    mtlb_b00_tram_addr_o ,
    output logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b00_tram_wdata_o,
    input  logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b00_tram_rdata_i,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                mtlb_b00_iram_cs_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                mtlb_b00_iram_wr_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]    mtlb_b00_iram_addr_o ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b00_iram_wdata_o,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b00_iram_rdata_i,
    output logic [BANK_4K_NUM-1:0]                                                      mtlb_b00_uram_cs_o   ,
    output logic [BANK_4K_NUM-1:0]                                                      mtlb_b00_uram_wr_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                          mtlb_b00_uram_addr_o ,
    output MTLB_UTAG_T    [BANK_4K_NUM-1:0]                                             mtlb_b00_uram_wdata_o,
    input  MTLB_UTAG_T    [BANK_4K_NUM-1:0]                                             mtlb_b00_uram_rdata_i,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                mtlb_b00_dram_cs_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                mtlb_b00_dram_wr_o   ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]    mtlb_b00_dram_addr_o ,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b00_dram_wdata_o,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b00_dram_rdata_i,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                mtlb_b01_tram_cs_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                mtlb_b01_tram_wr_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]    mtlb_b01_tram_addr_o ,
    output logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b01_tram_wdata_o,
    input  logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b01_tram_rdata_i,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                mtlb_b01_iram_cs_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                mtlb_b01_iram_wr_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]    mtlb_b01_iram_addr_o ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b01_iram_wdata_o,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b01_iram_rdata_i,
    output logic [BANK_2M_NUM-1:0]                                                      mtlb_b01_uram_cs_o   ,
    output logic [BANK_2M_NUM-1:0]                                                      mtlb_b01_uram_wr_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                          mtlb_b01_uram_addr_o ,
    output MTLB_UTAG_T    [BANK_2M_NUM-1:0]                                             mtlb_b01_uram_wdata_o,
    input  MTLB_UTAG_T    [BANK_2M_NUM-1:0]                                             mtlb_b01_uram_rdata_i,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                mtlb_b01_dram_cs_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                mtlb_b01_dram_wr_o   ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]    mtlb_b01_dram_addr_o ,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b01_dram_wdata_o,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b01_dram_rdata_i,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                mtlb_b10_tram_cs_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                mtlb_b10_tram_wr_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]    mtlb_b10_tram_addr_o ,
    output logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b10_tram_wdata_o,
    input  logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b10_tram_rdata_i,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                mtlb_b10_iram_cs_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                mtlb_b10_iram_wr_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]    mtlb_b10_iram_addr_o ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b10_iram_wdata_o,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b10_iram_rdata_i,
    output logic [BANK_1G_NUM-1:0]                                                      mtlb_b10_uram_cs_o   ,
    output logic [BANK_1G_NUM-1:0]                                                      mtlb_b10_uram_wr_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                          mtlb_b10_uram_addr_o ,
    output MTLB_UTAG_T    [BANK_1G_NUM-1:0]                                             mtlb_b10_uram_wdata_o,
    input  MTLB_UTAG_T    [BANK_1G_NUM-1:0]                                             mtlb_b10_uram_rdata_i,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                mtlb_b10_dram_cs_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                mtlb_b10_dram_wr_o   ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]    mtlb_b10_dram_addr_o ,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b10_dram_wdata_o,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b10_dram_rdata_i,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                mtlb_b11_tram_cs_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                mtlb_b11_tram_wr_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]    mtlb_b11_tram_addr_o ,
    output logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b11_tram_wdata_o,
    input  logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b11_tram_rdata_i,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                mtlb_b11_iram_cs_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                mtlb_b11_iram_wr_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]    mtlb_b11_iram_addr_o ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b11_iram_wdata_o,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           mtlb_b11_iram_rdata_i,
    output logic [BANK_0T_NUM-1:0]                                                      mtlb_b11_uram_cs_o   ,
    output logic [BANK_0T_NUM-1:0]                                                      mtlb_b11_uram_wr_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                          mtlb_b11_uram_addr_o ,
    output MTLB_UTAG_T    [BANK_0T_NUM-1:0]                                             mtlb_b11_uram_wdata_o,
    input  MTLB_UTAG_T    [BANK_0T_NUM-1:0]                                             mtlb_b11_uram_rdata_i,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                mtlb_b11_dram_cs_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                mtlb_b11_dram_wr_o   ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]    mtlb_b11_dram_addr_o ,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b11_dram_wdata_o,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           mtlb_b11_dram_rdata_i,
                                                        
    input  logic                                        param_in
//}}}
);
//=== Declare === {{{
    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_4K_IDX_WIDTH > BANK_2M_IDX_WIDTH) ? BANK_4K_IDX_WIDTH : BANK_2M_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = (BANK_1G_IDX_WIDTH > BANK_0T_IDX_WIDTH) ? BANK_1G_IDX_WIDTH : BANK_0T_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;
    localparam MAX_BANK_NUM         = 2**MAX_BANK_IDX_WIDTH;

    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_4K_SET_IDX_WIDTH > BANK_2M_SET_IDX_WIDTH) ? BANK_4K_SET_IDX_WIDTH : BANK_2M_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = (BANK_1G_SET_IDX_WIDTH > BANK_0T_SET_IDX_WIDTH) ? BANK_1G_SET_IDX_WIDTH : BANK_0T_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;
    localparam MAX_BANK_SET_NUM         = 2**MAX_BANK_SET_IDX_WIDTH;

    localparam MAX_BANK_WAY_IDX_WIDTH_0 = (BANK_4K_WAY_IDX_WIDTH > BANK_2M_WAY_IDX_WIDTH) ? BANK_4K_WAY_IDX_WIDTH : BANK_2M_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH_1 = (BANK_1G_WAY_IDX_WIDTH > BANK_0T_WAY_IDX_WIDTH) ? BANK_1G_WAY_IDX_WIDTH : BANK_0T_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH   = (MAX_BANK_WAY_IDX_WIDTH_0 > MAX_BANK_WAY_IDX_WIDTH_1) ? MAX_BANK_WAY_IDX_WIDTH_0 : MAX_BANK_WAY_IDX_WIDTH_1;
    localparam MAX_BANK_WAY_NUM         = 2**MAX_BANK_WAY_IDX_WIDTH;

    localparam TOTAL_WIDTH = ((TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH) < 32) ? 32 : (TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH);
    localparam IDBG_LENGTH = (TOTAL_WIDTH%32==0) ? (TOTAL_WIDTH/32) : (TOTAL_WIDTH/32 + 1);

    localparam S_IDLE = 3'b000;
    localparam S_BUSY = 3'b001;
    localparam S_WAIT = 3'b100;
    localparam S_WAIT1= 3'b101;

    logic [3-1:0]                                       idbg_bank_sel;
    logic [MAX_BANK_IDX_WIDTH-1:0]                      idbg_subbank_sel;
    logic [MAX_BANK_SET_IDX_WIDTH-1:0]                  idbg_bankset_sel;
    logic [MAX_BANK_WAY_IDX_WIDTH-1:0]                  idbg_bankway_sel;

    logic [TOTAL_WIDTH-1:0]                             idbg_dat_muxed, idbg_dat_muxed_shifted;
    logic [31:0]                                        idbg_dat;

    logic [2:0] cs, ns;
    logic [(TOTAL_WIDTH/32):0]                          cnt;
    logic [(TOTAL_WIDTH/32)+6:0]                        shift_value;
//}}}

//=== Main Code === {{{
// FSM-Stage1
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
// FSM-Stage2
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(idbg_go_i & idbg_opcode_i=={5'd0, S_BUSY})
                ns = S_BUSY;
            else if(idbg_go_i)
                ns = S_WAIT;
            else
                ns = S_IDLE;
        end
        S_BUSY: begin
            if(cnt==IDBG_LENGTH)
                ns = S_WAIT;
            else
                ns = S_BUSY;
        end
        S_WAIT: ns = S_WAIT1;
        S_WAIT1:ns = S_IDLE;
        default:ns = cs;
        endcase
    end
// FSM-Stage3
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            idbg_bank_sel   <= 'd0;
        else begin
            if(cs==S_IDLE & idbg_go_i & idbg_opcode_i==8'b0000_0100)
                idbg_bank_sel   <= idbg_dat_i[2:0];
        end
    end
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            idbg_subbank_sel<= 'd0;
        else begin
            if(cs==S_IDLE & idbg_go_i & idbg_opcode_i==8'b0000_0101)
                idbg_subbank_sel<= idbg_dat_i[MAX_BANK_IDX_WIDTH-1:0];
        end
    end
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            idbg_bankset_sel<= 'd0;
        else begin
            if(cs==S_IDLE & idbg_go_i & idbg_opcode_i==8'b0000_0110)
                idbg_bankset_sel<= idbg_dat_i[MAX_BANK_SET_IDX_WIDTH-1:0];
        end
    end
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            idbg_bankway_sel<= 'd0;
        else begin
            if(cs==S_IDLE & idbg_go_i & idbg_opcode_i==8'b0000_0111)
                idbg_bankway_sel<= idbg_dat_i[MAX_BANK_WAY_IDX_WIDTH-1:0];
        end
    end

genvar i0, i1; //{{{
generate
    for(i0=0; i0<BANK_4K_NUM; i0++) begin : gen_4k_ram_bank
        always@(*) begin
            if(idbg_bank_sel!=3'd0 | cs!=S_BUSY) begin
                mtlb_b00_uram_cs_o[i0]      = 1'b1;
            end
            else if(idbg_subbank_sel==i0) begin
                mtlb_b00_uram_cs_o[i0]      = 1'b0;
            end
            else begin
                mtlb_b00_uram_cs_o[i0]      = 1'b1;
            end
            
            mtlb_b00_uram_wr_o[i0]          = 1'b1;
            mtlb_b00_uram_addr_o[i0]        = idbg_bankset_sel[BANK_4K_SET_IDX_WIDTH-1:0];
            mtlb_b00_uram_wdata_o[i0]       = 'd0;
        end

        for(i1=0; i1<BANK_4K_WAY_NUM; i1++) begin : gen_4k_ram_set
            always@(*) begin
                if(idbg_bank_sel!=3'd0 | cs!=S_BUSY) begin
                    mtlb_b00_tram_cs_o[i0][i1]  = 1'b1;
                    mtlb_b00_iram_cs_o[i0][i1]  = 1'b1;
                    mtlb_b00_dram_cs_o[i0][i1]  = 1'b1;
                end
                else if(idbg_subbank_sel==i0 & idbg_bankway_sel==i1) begin
                    mtlb_b00_tram_cs_o[i0][i1]  = 1'b0;
                    mtlb_b00_iram_cs_o[i0][i1]  = 1'b0;
                    mtlb_b00_dram_cs_o[i0][i1]  = 1'b0;
                end
                else begin
                    mtlb_b00_tram_cs_o[i0][i1]  = 1'b1;
                    mtlb_b00_iram_cs_o[i0][i1]  = 1'b1;
                    mtlb_b00_dram_cs_o[i0][i1]  = 1'b1;
                end
                
                mtlb_b00_tram_wr_o[i0][i1]      = 1'b1;
                mtlb_b00_iram_wr_o[i0][i1]      = 1'b1;
                mtlb_b00_dram_wr_o[i0][i1]      = 1'b1;
                
                mtlb_b00_tram_addr_o[i0][i1]    = idbg_bankset_sel[BANK_4K_SET_IDX_WIDTH-1:0];
                mtlb_b00_iram_addr_o[i0][i1]    = idbg_bankset_sel[BANK_4K_SET_IDX_WIDTH-1:0];
                mtlb_b00_dram_addr_o[i0][i1]    = idbg_bankset_sel[BANK_4K_SET_IDX_WIDTH-1:0];
                
                mtlb_b00_tram_wdata_o[i0][i1]   = 'd0;
                mtlb_b00_iram_wdata_o[i0][i1]   = 'd0;
                mtlb_b00_dram_wdata_o[i0][i1]   = 'd0;
            end
        end
    end
endgenerate //}}}

genvar j0, j1; //{{{
generate
    for(j0=0; j0<BANK_2M_NUM; j0++) begin : gen_2m_ram_bank
        always@(*) begin
            if(idbg_bank_sel!=3'd1 | cs!=S_BUSY) begin
                mtlb_b01_uram_cs_o[j0]      = 1'b1;
            end
            else if(idbg_subbank_sel==j0) begin
                mtlb_b01_uram_cs_o[j0]      = 1'b0;
            end
            else begin
                mtlb_b01_uram_cs_o[j0]      = 1'b1;
            end
            
            mtlb_b01_uram_wr_o[j0]          = 1'b1;
            mtlb_b01_uram_addr_o[j0]        = idbg_bankset_sel[BANK_2M_SET_IDX_WIDTH-1:0];
            mtlb_b01_uram_wdata_o[j0]       = 'd0;
        end

        for(j1=0; j1<BANK_2M_WAY_NUM; j1++) begin : gen_4k_ram_set
            always@(*) begin
                if(idbg_bank_sel!=3'd1 | cs!=S_BUSY) begin
                    mtlb_b01_tram_cs_o[j0][j1]  = 1'b1;
                    mtlb_b01_iram_cs_o[j0][j1]  = 1'b1;
                    mtlb_b01_dram_cs_o[j0][j1]  = 1'b1;
                end
                else if(idbg_subbank_sel==j0 & idbg_bankway_sel==j1) begin
                    mtlb_b01_tram_cs_o[j0][j1]  = 1'b0;
                    mtlb_b01_iram_cs_o[j0][j1]  = 1'b0;
                    mtlb_b01_dram_cs_o[j0][j1]  = 1'b0;
                end
                else begin
                    mtlb_b01_tram_cs_o[j0][j1]  = 1'b1;
                    mtlb_b01_iram_cs_o[j0][j1]  = 1'b1;
                    mtlb_b01_dram_cs_o[j0][j1]  = 1'b1;
                end
                
                mtlb_b01_tram_wr_o[j0][j1]      = 1'b1;
                mtlb_b01_iram_wr_o[j0][j1]      = 1'b1;
                mtlb_b01_dram_wr_o[j0][j1]      = 1'b1;
                
                mtlb_b01_tram_addr_o[j0][j1]    = idbg_bankset_sel[BANK_2M_SET_IDX_WIDTH-1:0];
                mtlb_b01_iram_addr_o[j0][j1]    = idbg_bankset_sel[BANK_2M_SET_IDX_WIDTH-1:0];
                mtlb_b01_dram_addr_o[j0][j1]    = idbg_bankset_sel[BANK_2M_SET_IDX_WIDTH-1:0];
                
                mtlb_b01_tram_wdata_o[j0][j1]   = 'd0;
                mtlb_b01_iram_wdata_o[j0][j1]   = 'd0;
                mtlb_b01_dram_wdata_o[j0][j1]   = 'd0;
            end
        end
    end
endgenerate //}}}

genvar m0, m1; //{{{
generate
    for(m0=0; m0<BANK_1G_NUM; m0++) begin : gen_1g_ram_bank
        always@(*) begin
            if(idbg_bank_sel!=3'd2 | cs!=S_BUSY) begin
                mtlb_b10_uram_cs_o[m0]      = 1'b1;
            end
            else if(idbg_subbank_sel==m0) begin
                mtlb_b10_uram_cs_o[m0]      = 1'b0;
            end
            else begin
                mtlb_b10_uram_cs_o[m0]      = 1'b1;
            end
            
            mtlb_b10_uram_wr_o[m0]          = 1'b1;
            mtlb_b10_uram_addr_o[m0]        = idbg_bankset_sel[BANK_1G_SET_IDX_WIDTH-1:0];
            mtlb_b10_uram_wdata_o[m0]       = 'd0;
        end

        for(m1=0; m1<BANK_1G_WAY_NUM; m1++) begin : gen_4k_ram_set
            always@(*) begin
                if(idbg_bank_sel!=3'd2 | cs!=S_BUSY) begin
                    mtlb_b10_tram_cs_o[m0][m1]  = 1'b1;
                    mtlb_b10_iram_cs_o[m0][m1]  = 1'b1;
                    mtlb_b10_dram_cs_o[m0][m1]  = 1'b1;
                end
                else if(idbg_subbank_sel==m0 & idbg_bankway_sel==m1) begin
                    mtlb_b10_tram_cs_o[m0][m1]  = 1'b0;
                    mtlb_b10_iram_cs_o[m0][m1]  = 1'b0;
                    mtlb_b10_dram_cs_o[m0][m1]  = 1'b0;
                end
                else begin
                    mtlb_b10_tram_cs_o[m0][m1]  = 1'b1;
                    mtlb_b10_iram_cs_o[m0][m1]  = 1'b1;
                    mtlb_b10_dram_cs_o[m0][m1]  = 1'b1;
                end
                
                mtlb_b10_tram_wr_o[m0][m1]      = 1'b1;
                mtlb_b10_iram_wr_o[m0][m1]      = 1'b1;
                mtlb_b10_dram_wr_o[m0][m1]      = 1'b1;
                
                mtlb_b10_tram_addr_o[m0][m1]    = idbg_bankset_sel[BANK_1G_SET_IDX_WIDTH-1:0];
                mtlb_b10_iram_addr_o[m0][m1]    = idbg_bankset_sel[BANK_1G_SET_IDX_WIDTH-1:0];
                mtlb_b10_dram_addr_o[m0][m1]    = idbg_bankset_sel[BANK_1G_SET_IDX_WIDTH-1:0];
                
                mtlb_b10_tram_wdata_o[m0][m1]   = 'd0;
                mtlb_b10_iram_wdata_o[m0][m1]   = 'd0;
                mtlb_b10_dram_wdata_o[m0][m1]   = 'd0;
            end
        end
    end
endgenerate //}}}

genvar n0, n1; //{{{
generate
    for(n0=0; n0<BANK_0T_NUM; n0++) begin : gen_0t_ram_bank
        always@(*) begin
            if(idbg_bank_sel!=3'd3 | cs!=S_BUSY) begin
                mtlb_b11_uram_cs_o[n0]      = 1'b1;
            end
            else if(idbg_subbank_sel==n0) begin
                mtlb_b11_uram_cs_o[n0]      = 1'b0;
            end
            else begin
                mtlb_b11_uram_cs_o[n0]      = 1'b1;
            end
            
            mtlb_b11_uram_wr_o[n0]          = 1'b1;
            mtlb_b11_uram_addr_o[n0]        = idbg_bankset_sel[BANK_0T_SET_IDX_WIDTH-1:0];
            mtlb_b11_uram_wdata_o[n0]       = 'd0;
        end

        for(n1=0; n1<BANK_0T_WAY_NUM; n1++) begin : gen_4k_ram_set
            always@(*) begin
                if(idbg_bank_sel!=3'd3 | cs!=S_BUSY) begin
                    mtlb_b11_tram_cs_o[n0][n1]  = 1'b1;
                    mtlb_b11_iram_cs_o[n0][n1]  = 1'b1;
                    mtlb_b11_dram_cs_o[n0][n1]  = 1'b1;
                end
                else if(idbg_subbank_sel==n0 & idbg_bankway_sel==n1) begin
                    mtlb_b11_tram_cs_o[n0][n1]  = 1'b0;
                    mtlb_b11_iram_cs_o[n0][n1]  = 1'b0;
                    mtlb_b11_dram_cs_o[n0][n1]  = 1'b0;
                end
                else begin
                    mtlb_b11_tram_cs_o[n0][n1]  = 1'b1;
                    mtlb_b11_iram_cs_o[n0][n1]  = 1'b1;
                    mtlb_b11_dram_cs_o[n0][n1]  = 1'b1;
                end
                
                mtlb_b11_tram_wr_o[n0][n1]      = 1'b1;
                mtlb_b11_iram_wr_o[n0][n1]      = 1'b1;
                mtlb_b11_dram_wr_o[n0][n1]      = 1'b1;
                
                mtlb_b11_tram_addr_o[n0][n1]    = idbg_bankset_sel[BANK_0T_SET_IDX_WIDTH-1:0];
                mtlb_b11_iram_addr_o[n0][n1]    = idbg_bankset_sel[BANK_0T_SET_IDX_WIDTH-1:0];
                mtlb_b11_dram_addr_o[n0][n1]    = idbg_bankset_sel[BANK_0T_SET_IDX_WIDTH-1:0];
                
                mtlb_b11_tram_wdata_o[n0][n1]   = 'd0;
                mtlb_b11_iram_wdata_o[n0][n1]   = 'd0;
                mtlb_b11_dram_wdata_o[n0][n1]   = 'd0;
            end
        end
    end
endgenerate //}}}

generate
    if(TOTAL_WIDTH > (TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH)) begin //< 32) ? 32 : (TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH);
        assign idbg_dat_muxed = idbg_bank_sel==3'd0 ? { {(TOTAL_WIDTH-(TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH)){1'b0}},
                                                        mtlb_b00_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b00_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b00_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b00_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                idbg_bank_sel==3'd1 ? { {(TOTAL_WIDTH-(TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH)){1'b0}},
                                                        mtlb_b01_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b01_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b01_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b01_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                idbg_bank_sel==3'd2 ? { {(TOTAL_WIDTH-(TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH)){1'b0}},
                                                        mtlb_b10_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b10_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b10_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b10_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                idbg_bank_sel==3'd3 ? { {(TOTAL_WIDTH-(TRAM_DAT_WIDTH+IRAM_DAT_WIDTH+$bits(MTLB_UTAG_T)+DRAM_DAT_WIDTH)){1'b0}},
                                                        mtlb_b11_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b11_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b11_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b11_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                                        'hdeadbeef;
    end
    else begin
        assign idbg_dat_muxed = idbg_bank_sel==3'd0 ? {
                                                        mtlb_b00_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b00_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b00_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b00_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                idbg_bank_sel==3'd1 ? {
                                                        mtlb_b01_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b01_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b01_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b01_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                idbg_bank_sel==3'd2 ? {
                                                        mtlb_b10_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b10_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b10_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b10_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                idbg_bank_sel==3'd3 ? {
                                                        mtlb_b11_uram_rdata_i[idbg_subbank_sel],
                                                        mtlb_b11_tram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b11_iram_rdata_i[idbg_subbank_sel][idbg_bankway_sel],
                                                        mtlb_b11_dram_rdata_i[idbg_subbank_sel][idbg_bankway_sel]
                                                        } :
                                                        'hdeadbeef;
    end
endgenerate

    always@(*) begin
        shift_value            = ((TOTAL_WIDTH/32)+6+1)'((IDBG_LENGTH-(cnt-2)-1) << 5);
        idbg_dat_muxed_shifted = idbg_dat_muxed >> shift_value;
        idbg_dat               = idbg_dat_muxed_shifted[31:0];
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cnt <= 'd0;
        else begin
            if(ns==S_BUSY | ns==S_WAIT)
                cnt <= cnt + 'd1;
            else if(ns==S_IDLE)
                cnt <= 'd0;
        end
    end

    assign idbg_busy_o = (cs!=S_IDLE);

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            idbg_datv_o <= 'd0;
            idbg_dat_o  <= 'd0;
        end
        else begin
            if((cs==S_BUSY | cs==S_WAIT) & (cnt>='d2)) begin
                idbg_datv_o <= 1'b1;
                idbg_dat_o  <= idbg_dat;
            end
            else begin
                idbg_datv_o <= 1'b0;
            end
        end
    end

    assign idbg_ongoing_o = (cs!=S_IDLE);
//}}}

endmodule//}}}



module iommu_acd_mon_unit_translate_intf #(//{{{
    parameter type          TRANSLATE_REQ_TYPE          = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE          = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter   RAM_IDX_WIDTH               = 6,
    parameter   SPARE_PARAM                 = 0
)(//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    //                                                  
    input  logic                                        idbg_go_i,
    output logic                                        idbg_busy_o,
    input  logic [7:0]                                  idbg_opcode_i,
    input  logic [31:0]                                 idbg_dat_i,
    output logic [31:0]                                 idbg_dat_o,
    output logic                                        idbg_datv_o,
    input  logic [63:0]                                 idbg_timer_i,
    //                                                  
    input  logic                                        translate_req_valid_i,
    input  TRANSLATE_REQ_TYPE                           translate_req_i,
    input  logic                                        translate_ack_valid_i,
    input  TRANSLATE_ACK_TYPE                           translate_ack_i,
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
    localparam TOTAL_WIDTH = $bits(translate_req_i) > $bits(translate_ack_i) ?  $bits(translate_req_i) + 64 : $bits(translate_ack_i) + 64;
    localparam IDBG_LENGTH = (TOTAL_WIDTH%32==0) ? (TOTAL_WIDTH/32) : (TOTAL_WIDTH/32 + 1);

    typedef struct packed {
        TRANSLATE_REQ_TYPE  req;
        logic [63:0]        scnt;
    } reqram_t;

    typedef struct packed {
        TRANSLATE_ACK_TYPE  ack;
        logic [63:0]        scnt;
    } ackram_t;

    localparam S_IDLE = 3'b000;
    localparam S_BUSY = 3'b001;
    localparam S_WAIT = 3'b100;
    localparam S_WAIT1= 3'b101;
    logic [2:0] cs, ns;
    logic [(TOTAL_WIDTH/32)+1:0]                        cnt;
    logic [(TOTAL_WIDTH/32)+6:0]                        shift_value;

    logic                                               req_fl_priv     ;
    logic                                               req_fl_ext      ;
    logic [1:0]                                         req_fl_wr       ;
    logic                                               req_fl_atst     ;
    logic                                               req_fl_pv       ;
    logic [19:0]                                        req_fl_pid      ;
    logic [23:0]                                        req_fl_did      ;
    logic [63:12]                                       req_fl_va       ;
    logic                                               req_fl_mask_priv;
    logic                                               req_fl_mask_ext ;
    logic [1:0]                                         req_fl_mask_wr  ;
    logic                                               req_fl_mask_atst;
    logic                                               req_fl_mask_pv  ;
    logic [19:0]                                        req_fl_mask_pid ;
    logic [23:0]                                        req_fl_mask_did ;
    logic [63:12]                                       req_fl_mask_va  ;
    logic                                               req_fl_math_priv;
    logic                                               req_fl_math_ext ;
    logic                                               req_fl_math_wr  ;
    logic                                               req_fl_math_atst;
    logic                                               req_fl_math_pv  ;
    logic                                               req_fl_math_pid ;
    logic                                               req_fl_math_did ;
    logic                                               req_fl_math_va  ;

    logic                                               req_masked_valid;
    logic                                               dorecord;

    logic                                               req_ram_cs;
    logic                                               req_ram_wr;
    reqram_t                                            req_ram_out;
    logic [RAM_IDX_WIDTH-1:0]                           req_wr_addr;
    logic [RAM_IDX_WIDTH-1:0]                           req_ram_addr;

    logic [1:0]                                         ack_fl_resp;
    logic [63:12]                                       ack_fl_pa;
    logic [1:0]                                         ack_fl_mask_resp;
    logic [63:12]                                       ack_fl_mask_pa;
    logic                                               ack_fl_math_resp;
    logic                                               ack_fl_math_pa;

    logic                                               ack_masked_valid;

    logic                                               ack_ram_cs;
    logic                                               ack_ram_wr;
    ackram_t                                            ack_ram_out;
    logic [RAM_IDX_WIDTH-1:0]                           ack_wr_addr;
    logic [RAM_IDX_WIDTH-1:0]                           ack_ram_addr;

    logic [RAM_IDX_WIDTH-1:0]                           rd_addr;
    logic                                               rd_sel;

    logic [TOTAL_WIDTH-1:0]                             idbg_dat_muxed, idbg_dat_muxed_shifted;
    logic [31:0]                                        idbg_dat;

//}}}

//=== Main Code === {{{
//=== cfg {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            dorecord  <= 1'b0;
        end
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d1))
                dorecord <= 1'b1;
            else if(idbg_go_i & (idbg_opcode_i=='d2))
                dorecord <= 1'b0;
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            rd_addr <= 'd0;
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d29))
                rd_addr <= idbg_dat_i[RAM_IDX_WIDTH-1:0];
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            rd_sel  <= 'd0;
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d30))
                rd_sel  <= idbg_dat_i[0];
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            req_fl_priv     <= 'd0;
            req_fl_ext      <= 'd0;
            req_fl_wr       <= 'd0;
            req_fl_atst     <= 'd0;
            req_fl_pv       <= 'd0;
            req_fl_pid      <= 'd0;

            req_fl_mask_priv     <= 'd0;
            req_fl_mask_ext      <= 'd0;
            req_fl_mask_wr       <= 'd0;
            req_fl_mask_atst     <= 'd0;
            req_fl_mask_pv       <= 'd0;
            req_fl_mask_pid      <= 'd0;
        end
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d3)) begin
                req_fl_priv     <= idbg_dat_i[0];
                req_fl_ext      <= idbg_dat_i[1];
                req_fl_wr       <= idbg_dat_i[3:2];
                req_fl_atst     <= idbg_dat_i[4];
                req_fl_pv       <= idbg_dat_i[5];
                req_fl_pid      <= idbg_dat_i[25:6];
            end

            if(idbg_go_i & (idbg_opcode_i=='d13)) begin
                req_fl_mask_priv     <= idbg_dat_i[0];
                req_fl_mask_ext      <= idbg_dat_i[1];
                req_fl_mask_wr       <= idbg_dat_i[3:2];
                req_fl_mask_atst     <= idbg_dat_i[4];
                req_fl_mask_pv       <= idbg_dat_i[5];
                req_fl_mask_pid      <= idbg_dat_i[25:6];
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            req_fl_did      <= 'd0;
            req_fl_mask_did      <= 'd0;
        end
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d4)) begin
                req_fl_did      <= idbg_dat_i[23:0];
            end

            if(idbg_go_i & (idbg_opcode_i=='d14)) begin
                req_fl_mask_did      <= idbg_dat_i[23:0];
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            req_fl_va       <= 'd0;
            req_fl_mask_va      <= 'd0;
        end
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d5)) begin
                req_fl_va   <= {req_fl_va[63:44], idbg_dat_i[31:0]};
            end
            else if(idbg_go_i & (idbg_opcode_i=='d6)) begin
                req_fl_va   <= {idbg_dat_i[19:0], req_fl_va[43:12]};
            end

            if(idbg_go_i & (idbg_opcode_i=='d15)) begin
                req_fl_mask_va   <= {req_fl_mask_va[63:44], idbg_dat_i[31:0]};
            end
            else if(idbg_go_i & (idbg_opcode_i=='d16)) begin
                req_fl_mask_va   <= {idbg_dat_i[19:0], req_fl_mask_va[43:12]};
            end

        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ack_fl_pa       <= 'd0;
            ack_fl_mask_pa      <= 'd0;
            ack_fl_resp     <= 'd0;
            ack_fl_mask_resp    <= 'd0;
        end
        else begin
            if(idbg_go_i & (idbg_opcode_i=='d7)) begin
                ack_fl_pa   <= {ack_fl_pa[63:44], idbg_dat_i[31:0]};
            end
            else if(idbg_go_i & (idbg_opcode_i=='d8)) begin
                ack_fl_pa   <= {idbg_dat_i[19:0], ack_fl_pa[43:12]};
                ack_fl_resp <= idbg_dat_i[21:20];
            end

            if(idbg_go_i & (idbg_opcode_i=='d17)) begin
                ack_fl_mask_pa   <= {ack_fl_mask_pa[63:44], idbg_dat_i[31:0]};
            end
            else if(idbg_go_i & (idbg_opcode_i=='d18)) begin
                ack_fl_mask_pa   <= {idbg_dat_i[19:0], ack_fl_mask_pa[43:12]};
                ack_fl_mask_resp <= idbg_dat_i[21:20];
            end
        end
    end
//}}}

//=== req mon {{{
    assign req_fl_math_priv = ((req_fl_mask_priv & translate_req_i.priv              ) == req_fl_priv);
    assign req_fl_math_ext  = ((req_fl_mask_ext  & translate_req_i.ext               ) == req_fl_ext );
    assign req_fl_math_wr   =   req_fl_wr==2'b00 ? 1'b1 :
                                req_fl_wr==2'b01 ? (translate_req_i.wr==1'b1) :
                                req_fl_wr==2'b10 ? (translate_req_i.wr==1'b0) :
                                                   1'b1;
    assign req_fl_math_atst = ((req_fl_mask_atst & translate_req_i.is_translated     ) == req_fl_atst);
    assign req_fl_math_pv   = ((req_fl_mask_pv   & translate_req_i.process_id_valid  ) == req_fl_pv  );
    assign req_fl_math_pid  = ((req_fl_mask_pid  & translate_req_i.process_id        ) == req_fl_pid );
    assign req_fl_math_did  = ((req_fl_mask_did  & translate_req_i.device_id         ) == req_fl_did );
    assign req_fl_math_va   = ((req_fl_mask_va   & translate_req_i.va                ) == req_fl_va  );

    assign req_masked_valid = translate_req_valid_i &
                              req_fl_math_priv      &
                              req_fl_math_ext       &
                              req_fl_math_wr        &
                              req_fl_math_atst      &
                              req_fl_math_pv        &
                              req_fl_math_pid       &
                              req_fl_math_did       &
                              req_fl_math_va        &
                              1'b1;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            req_wr_addr <= 'd0;
        else begin
            if(~req_ram_cs & ~req_ram_wr)
                req_wr_addr <= req_wr_addr + 'd1;
        end
    end

    assign req_ram_cs   = dorecord ?  ~req_masked_valid : ~((cs==S_BUSY) & (rd_sel=='d0));
    assign req_ram_wr   = dorecord ?  1'b0 : 1'b1;
    assign req_ram_addr = dorecord ?  req_wr_addr : rd_addr;

    iommu_acd_tlb_ram_wrap #(
    /*parameter */ .RAM_WIDTH   ($bits(reqram_t)                ), //= 64,
    /*parameter */ .ADDR_WIDTH  (RAM_IDX_WIDTH                  )  //= 6
    ) U_req_mon_ram (
        .CLK                    (clk                            ),
        .D                      ({translate_req_i, idbg_timer_i}),
        .Q                      (req_ram_out                    ),
        .CEN                    (req_ram_cs                     ),
        .WEN                    (req_ram_wr                     ),
        .A                      (req_ram_addr                   )
    );

//}}}

//=== ack mon {{{
    assign ack_fl_math_pa   = ((ack_fl_mask_pa   & translate_ack_i.pa               ) == ack_fl_pa  );
    assign ack_fl_math_resp = ((ack_fl_mask_resp & translate_ack_i.resp             ) == ack_fl_resp);

    assign ack_masked_valid = translate_ack_valid_i &
                              ack_fl_math_pa        &
                              ack_fl_math_resp      &
                              1'b1;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            ack_wr_addr <= 'd0;
        else begin
            if(~ack_ram_cs & ~ack_ram_wr)
                ack_wr_addr <= ack_wr_addr + 'd1;
        end
    end

    assign ack_ram_cs   = dorecord ?  ~ack_masked_valid : ~((cs==S_BUSY) & rd_sel=='d1);
    assign ack_ram_wr   = dorecord ?  1'b0 : 1'b1;
    assign ack_ram_addr = dorecord ?  ack_wr_addr : rd_addr;

    iommu_acd_tlb_ram_wrap #(
    /*parameter */ .RAM_WIDTH   ($bits(ackram_t)                ), //= 64,
    /*parameter */ .ADDR_WIDTH  (RAM_IDX_WIDTH                  )  //= 6
    ) U_ack_mon_ram (
        .CLK                    (clk                            ),
        .D                      ({translate_ack_i, idbg_timer_i}),
        .Q                      (ack_ram_out                    ),
        .CEN                    (ack_ram_cs                     ),
        .WEN                    (ack_ram_wr                     ),
        .A                      (ack_ram_addr                   )
    );

//}}}

// FSM-Stage1
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
// FSM-Stage2
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(idbg_go_i & idbg_opcode_i=='d31)
                ns = S_BUSY;
            else if(idbg_go_i)
                ns = S_WAIT;
            else
                ns = S_IDLE;
        end
        S_BUSY: begin
            if(cnt==IDBG_LENGTH)
                ns = S_WAIT;
            else
                ns = S_BUSY;
        end
        S_WAIT: ns = S_WAIT1;
        S_WAIT1:ns = S_IDLE;
        default:ns = cs;
        endcase
    end
// FSM-Stage3
    assign idbg_dat_muxed = rd_sel ? ack_ram_out : req_ram_out;

    always@(*) begin
        shift_value            = (IDBG_LENGTH-(cnt-2)-1) << 5;
        idbg_dat_muxed_shifted = idbg_dat_muxed >> shift_value;
        idbg_dat               = idbg_dat_muxed_shifted[31:0];
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cnt <= 'd0;
        else begin
            if(ns==S_BUSY | ns==S_WAIT)
                cnt <= cnt + 'd1;
            else if(ns==S_IDLE)
                cnt <= 'd0;
        end
    end

    assign idbg_busy_o = (cs!=S_IDLE);

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            idbg_datv_o <= 'd0;
            idbg_dat_o  <= 'd0;
        end
        else begin
            if((cs==S_BUSY | cs==S_WAIT) & (cnt>='d2)) begin
                idbg_datv_o <= 1'b1;
                idbg_dat_o  <= idbg_dat;
            end
            else begin
                idbg_datv_o <= 1'b0;
            end
        end
    end

//}}}

endmodule//}}}
