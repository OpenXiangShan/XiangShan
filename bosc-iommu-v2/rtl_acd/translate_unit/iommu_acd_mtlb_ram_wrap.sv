////////////////////////////////////////////////////
// iommu_acd_mtlb_ram_wrap
////////////////////////////////////////////////////
module iommu_acd_mtlb_ram_wrap #(//{{{
    parameter   BANK_4K_IDX_WIDTH                                           = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_IDX_WIDTH                                           = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_IDX_WIDTH                                           = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_IDX_WIDTH                                           = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_IDX_WIDTH                                       = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_IDX_WIDTH                                       = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_IDX_WIDTH                                       = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_IDX_WIDTH                                       = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    parameter   BANK_4K_WAY_IDX_WIDTH                                       = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    parameter   BANK_2M_WAY_IDX_WIDTH                                       = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    parameter   BANK_1G_WAY_IDX_WIDTH                                       = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    parameter   BANK_0T_WAY_IDX_WIDTH                                       = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    parameter   BANK_4K_NUM                                                 = 2**BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_NUM                                                 = 2**BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_NUM                                                 = 2**BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_NUM                                                 = 2**BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_NUM                                             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_NUM                                             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_NUM                                             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_NUM                                             = 2**BANK_0T_SET_IDX_WIDTH,
    parameter   BANK_4K_WAY_NUM                                             = 2**BANK_4K_WAY_IDX_WIDTH,
    parameter   BANK_2M_WAY_NUM                                             = 2**BANK_2M_WAY_IDX_WIDTH,
    parameter   BANK_1G_WAY_NUM                                             = 2**BANK_1G_WAY_IDX_WIDTH,
    parameter   BANK_0T_WAY_NUM                                             = 2**BANK_0T_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                                               = iommu_acd_pkg::mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                                              = iommu_acd_pkg::mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                                              = iommu_acd_pkg::mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                                               = iommu_acd_pkg::mtlb_dat_t,
    parameter   ECC_WIDTH                                                   = 7,
    parameter   TRAM_DAT_WIDTH                                              = $bits(MTLB_TAG_TYPE)+ECC_WIDTH+1,
    parameter   IRAM_DAT_WIDTH                                              = $bits(MTLB_ITAG_TYPE)+ECC_WIDTH+1,
    parameter   DRAM_DAT_WIDTH                                              = $bits(MTLB_DAT_TYPE)+ECC_WIDTH+1,
    parameter   SPARE_PARAM                                                 = 0
)(
//=== IO {{{
    input  logic                                                                        clk,
    input  logic                                                                        rstn,
    // RAM                                                                              
    output logic                                                                        ram_initial_done_o,

    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                b00_tram_cs_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                b00_tram_wr_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]    b00_tram_addr_i,
    input  logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b00_tram_wdata_i,
    output logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b00_tram_rdata_o,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                b00_iram_cs_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                b00_iram_wr_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]    b00_iram_addr_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b00_iram_wdata_i,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b00_iram_rdata_o,
    input  logic [BANK_4K_NUM-1:0]                                                      b00_uram_cs_i,
    input  logic [BANK_4K_NUM-1:0]                                                      b00_uram_wr_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                          b00_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                             b00_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                             b00_uram_rdata_o,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                b00_dram_cs_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                                b00_dram_wr_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]    b00_dram_addr_i,
    input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b00_dram_wdata_i,
    output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b00_dram_rdata_o,
                                                                                        
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                b01_tram_cs_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                b01_tram_wr_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]    b01_tram_addr_i,
    input  logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b01_tram_wdata_i,
    output logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b01_tram_rdata_o,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                b01_iram_cs_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                b01_iram_wr_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]    b01_iram_addr_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b01_iram_wdata_i,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b01_iram_rdata_o,
    input  logic [BANK_2M_NUM-1:0]                                                      b01_uram_cs_i,
    input  logic [BANK_2M_NUM-1:0]                                                      b01_uram_wr_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                          b01_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                             b01_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                             b01_uram_rdata_o,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                b01_dram_cs_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                                b01_dram_wr_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]    b01_dram_addr_i,
    input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b01_dram_wdata_i,
    output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b01_dram_rdata_o,
                                                                                        
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                b10_tram_cs_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                b10_tram_wr_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]    b10_tram_addr_i,
    input  logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b10_tram_wdata_i,
    output logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b10_tram_rdata_o,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                b10_iram_cs_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                b10_iram_wr_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]    b10_iram_addr_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b10_iram_wdata_i,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b10_iram_rdata_o,
    input  logic [BANK_1G_NUM-1:0]                                                      b10_uram_cs_i,
    input  logic [BANK_1G_NUM-1:0]                                                      b10_uram_wr_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                          b10_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                             b10_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                             b10_uram_rdata_o,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                b10_dram_cs_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                                b10_dram_wr_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]    b10_dram_addr_i,
    input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b10_dram_wdata_i,
    output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b10_dram_rdata_o,
                                                                                        
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                b11_tram_cs_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                b11_tram_wr_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]    b11_tram_addr_i,
    input  logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b11_tram_wdata_i,
    output logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b11_tram_rdata_o,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                b11_iram_cs_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                b11_iram_wr_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]    b11_iram_addr_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b11_iram_wdata_i,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b11_iram_rdata_o,
    input  logic [BANK_0T_NUM-1:0]                                                      b11_uram_cs_i,
    input  logic [BANK_0T_NUM-1:0]                                                      b11_uram_wr_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                          b11_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                             b11_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                             b11_uram_rdata_o,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                b11_dram_cs_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                                b11_dram_wr_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]    b11_dram_addr_i,
    input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b11_dram_wdata_i,
    output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]           b11_dram_rdata_o,
    //                                                                                  
    input  logic                                                                        spare_in
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

    logic [MAX_BANK_SET_IDX_WIDTH-1:0]  ram_initial_scnt;
    logic                               ram_initial_done, ram_initial_done_ff;

    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               b00_tram_cs_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               b00_tram_wr_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   b00_tram_addr_mux;
    logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         b00_tram_wdata_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               b00_iram_cs_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               b00_iram_wr_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   b00_iram_addr_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b00_iram_wdata_mux;
    logic [BANK_4K_NUM-1:0]                                                     b00_uram_cs_mux;
    logic [BANK_4K_NUM-1:0]                                                     b00_uram_wr_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                         b00_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                            b00_uram_wdata_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               b00_dram_cs_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               b00_dram_wr_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   b00_dram_addr_mux;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          b00_dram_wdata_mux;

    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               b01_tram_cs_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               b01_tram_wr_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   b01_tram_addr_mux;
    logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         b01_tram_wdata_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               b01_iram_cs_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               b01_iram_wr_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   b01_iram_addr_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b01_iram_wdata_mux;
    logic [BANK_2M_NUM-1:0]                                                     b01_uram_cs_mux;
    logic [BANK_2M_NUM-1:0]                                                     b01_uram_wr_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                         b01_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                            b01_uram_wdata_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               b01_dram_cs_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               b01_dram_wr_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   b01_dram_addr_mux;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          b01_dram_wdata_mux;

    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               b10_tram_cs_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               b10_tram_wr_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   b10_tram_addr_mux;
    logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         b10_tram_wdata_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               b10_iram_cs_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               b10_iram_wr_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   b10_iram_addr_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b10_iram_wdata_mux;
    logic [BANK_1G_NUM-1:0]                                                     b10_uram_cs_mux;
    logic [BANK_1G_NUM-1:0]                                                     b10_uram_wr_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                         b10_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                            b10_uram_wdata_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               b10_dram_cs_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               b10_dram_wr_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   b10_dram_addr_mux;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          b10_dram_wdata_mux;

    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               b11_tram_cs_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               b11_tram_wr_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   b11_tram_addr_mux;
    logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         b11_tram_wdata_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               b11_iram_cs_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               b11_iram_wr_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   b11_iram_addr_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b11_iram_wdata_mux;
    logic [BANK_0T_NUM-1:0]                                                     b11_uram_cs_mux;
    logic [BANK_0T_NUM-1:0]                                                     b11_uram_wr_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                         b11_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                            b11_uram_wdata_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               b11_dram_cs_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               b11_dram_wr_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   b11_dram_addr_mux;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          b11_dram_wdata_mux;

//}}}

//=== MainCode === {{{

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ram_initial_scnt <= 'd0;
        end
        else if(~(ram_initial_done | ram_initial_done_ff)) begin
            ram_initial_scnt <= ram_initial_scnt+'d1;
        end
    end
    
    assign ram_initial_done = ( ram_initial_scnt=={MAX_BANK_SET_IDX_WIDTH{1'b1}} );
    
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            ram_initial_done_ff <= 1'b0;
        else if(ram_initial_done)
            ram_initial_done_ff <= 1'b1;
    end
    
    assign ram_initial_done_o = ram_initial_done_ff;
//}}}

//=== RAM Inst {{{
//=== 4K {{{
genvar r4k_b, r4k_w;
generate
    for(r4k_b=0; r4k_b<BANK_4K_NUM; r4k_b++) begin : ram4k_inst_gen
        for(r4k_w=0; r4k_w<BANK_4K_WAY_NUM; r4k_w++) begin : way4k_inst_gen
            assign b00_tram_cs_mux[r4k_b][r4k_w]    = ram_initial_done_ff ? b00_tram_cs_i[r4k_b][r4k_w]    : 'd0;
            assign b00_tram_wr_mux[r4k_b][r4k_w]    = ram_initial_done_ff ? b00_tram_wr_i[r4k_b][r4k_w]    : 'd0;
            assign b00_tram_addr_mux[r4k_b][r4k_w]  = ram_initial_done_ff ? b00_tram_addr_i[r4k_b][r4k_w]  : ram_initial_scnt[BANK_4K_SET_IDX_WIDTH-1:0];
            assign b00_tram_wdata_mux[r4k_b][r4k_w] = ram_initial_done_ff ? b00_tram_wdata_i[r4k_b][r4k_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH                  ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_4K_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b00_tram_wdata_mux[r4k_b][r4k_w] ),
                            .Q                      (b00_tram_rdata_o[r4k_b][r4k_w] ),
                            .CEN                    (b00_tram_cs_mux[r4k_b][r4k_w]    ),
                            .WEN                    (b00_tram_wr_mux[r4k_b][r4k_w]    ),
                            .A                      (b00_tram_addr_mux[r4k_b][r4k_w]  ) 
            );

            assign b00_iram_cs_mux[r4k_b][r4k_w]    = ram_initial_done_ff ? b00_iram_cs_i[r4k_b][r4k_w]    : 'd0;
            assign b00_iram_wr_mux[r4k_b][r4k_w]    = ram_initial_done_ff ? b00_iram_wr_i[r4k_b][r4k_w]    : 'd0;
            assign b00_iram_addr_mux[r4k_b][r4k_w]  = ram_initial_done_ff ? b00_iram_addr_i[r4k_b][r4k_w]  : ram_initial_scnt[BANK_4K_SET_IDX_WIDTH-1:0];
            assign b00_iram_wdata_mux[r4k_b][r4k_w] = ram_initial_done_ff ? b00_iram_wdata_i[r4k_b][r4k_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (IRAM_DAT_WIDTH                ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_4K_SET_IDX_WIDTH          )  // = 6
            ) U_iram(
                            .CLK                    (clk                            ),
                            .D                      (b00_iram_wdata_mux[r4k_b][r4k_w] ),
                            .Q                      (b00_iram_rdata_o[r4k_b][r4k_w] ),
                            .CEN                    (b00_iram_cs_mux[r4k_b][r4k_w]    ),
                            .WEN                    (b00_iram_wr_mux[r4k_b][r4k_w]    ),
                            .A                      (b00_iram_addr_mux[r4k_b][r4k_w]  ) 
            );

            assign b00_dram_cs_mux[r4k_b][r4k_w]    = ram_initial_done_ff ? b00_dram_cs_i[r4k_b][r4k_w]    : 'd0;
            assign b00_dram_wr_mux[r4k_b][r4k_w]    = ram_initial_done_ff ? b00_dram_wr_i[r4k_b][r4k_w]    : 'd0;
            assign b00_dram_addr_mux[r4k_b][r4k_w]  = ram_initial_done_ff ? b00_dram_addr_i[r4k_b][r4k_w]  : ram_initial_scnt[BANK_4K_SET_IDX_WIDTH-1:0];
            assign b00_dram_wdata_mux[r4k_b][r4k_w] = ram_initial_done_ff ? b00_dram_wdata_i[r4k_b][r4k_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (DRAM_DAT_WIDTH                 ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_4K_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b00_dram_wdata_mux[r4k_b][r4k_w] ),
                            .Q                      (b00_dram_rdata_o[r4k_b][r4k_w] ),
                            .CEN                    (b00_dram_cs_mux[r4k_b][r4k_w]    ),
                            .WEN                    (b00_dram_wr_mux[r4k_b][r4k_w]    ),
                            .A                      (b00_dram_addr_mux[r4k_b][r4k_w]  ) 
            );
        end
            assign b00_uram_cs_mux[r4k_b]    = ram_initial_done_ff ? b00_uram_cs_i[r4k_b]    : 'd0;
            assign b00_uram_wr_mux[r4k_b]    = ram_initial_done_ff ? b00_uram_wr_i[r4k_b]    : 'd0;
            assign b00_uram_addr_mux[r4k_b]  = ram_initial_done_ff ? b00_uram_addr_i[r4k_b]  : ram_initial_scnt[BANK_4K_SET_IDX_WIDTH-1:0];
            assign b00_uram_wdata_mux[r4k_b] = ram_initial_done_ff ? b00_uram_wdata_i[r4k_b] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_4K_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b00_uram_wdata_mux[r4k_b]        ),
                            .Q                      (b00_uram_rdata_o[r4k_b]        ),
                            .CEN                    (b00_uram_cs_mux[r4k_b]           ),
                            .WEN                    (b00_uram_wr_mux[r4k_b]           ),
                            .A                      (b00_uram_addr_mux[r4k_b]         ) 
            );
    end
endgenerate
//}}}

//=== 2M {{{
genvar r2m_b, r2m_w;
generate
    for(r2m_b=0; r2m_b<BANK_2M_NUM; r2m_b++) begin : ram2m_inst_gen
        for(r2m_w=0; r2m_w<BANK_2M_WAY_NUM; r2m_w++) begin : way2m_inst_gen
            assign b01_tram_cs_mux[r2m_b][r2m_w]    = ram_initial_done_ff ? b01_tram_cs_i[r2m_b][r2m_w]    : 'd0;
            assign b01_tram_wr_mux[r2m_b][r2m_w]    = ram_initial_done_ff ? b01_tram_wr_i[r2m_b][r2m_w]    : 'd0;
            assign b01_tram_addr_mux[r2m_b][r2m_w]  = ram_initial_done_ff ? b01_tram_addr_i[r2m_b][r2m_w]  : ram_initial_scnt[BANK_2M_SET_IDX_WIDTH-1:0];
            assign b01_tram_wdata_mux[r2m_b][r2m_w] = ram_initial_done_ff ? b01_tram_wdata_i[r2m_b][r2m_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH                  ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_2M_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b01_tram_wdata_mux[r2m_b][r2m_w] ),
                            .Q                      (b01_tram_rdata_o[r2m_b][r2m_w] ),
                            .CEN                    (b01_tram_cs_mux[r2m_b][r2m_w]    ),
                            .WEN                    (b01_tram_wr_mux[r2m_b][r2m_w]    ),
                            .A                      (b01_tram_addr_mux[r2m_b][r2m_w]  ) 
            );

            assign b01_iram_cs_mux[r2m_b][r2m_w]    = ram_initial_done_ff ? b01_iram_cs_i[r2m_b][r2m_w]    : 'd0;
            assign b01_iram_wr_mux[r2m_b][r2m_w]    = ram_initial_done_ff ? b01_iram_wr_i[r2m_b][r2m_w]    : 'd0;
            assign b01_iram_addr_mux[r2m_b][r2m_w]  = ram_initial_done_ff ? b01_iram_addr_i[r2m_b][r2m_w]  : ram_initial_scnt[BANK_2M_SET_IDX_WIDTH-1:0];
            assign b01_iram_wdata_mux[r2m_b][r2m_w] = ram_initial_done_ff ? b01_iram_wdata_i[r2m_b][r2m_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (IRAM_DAT_WIDTH                ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_2M_SET_IDX_WIDTH          )  // = 6
            ) U_iram(
                            .CLK                    (clk                            ),
                            .D                      (b01_iram_wdata_mux[r2m_b][r2m_w] ),
                            .Q                      (b01_iram_rdata_o[r2m_b][r2m_w] ),
                            .CEN                    (b01_iram_cs_mux[r2m_b][r2m_w]    ),
                            .WEN                    (b01_iram_wr_mux[r2m_b][r2m_w]    ),
                            .A                      (b01_iram_addr_mux[r2m_b][r2m_w]  ) 
            );

            assign b01_dram_cs_mux[r2m_b][r2m_w]    = ram_initial_done_ff ? b01_dram_cs_i[r2m_b][r2m_w]    : 'd0;
            assign b01_dram_wr_mux[r2m_b][r2m_w]    = ram_initial_done_ff ? b01_dram_wr_i[r2m_b][r2m_w]    : 'd0;
            assign b01_dram_addr_mux[r2m_b][r2m_w]  = ram_initial_done_ff ? b01_dram_addr_i[r2m_b][r2m_w]  : ram_initial_scnt[BANK_2M_SET_IDX_WIDTH-1:0];
            assign b01_dram_wdata_mux[r2m_b][r2m_w] = ram_initial_done_ff ? b01_dram_wdata_i[r2m_b][r2m_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (DRAM_DAT_WIDTH                 ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_2M_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b01_dram_wdata_mux[r2m_b][r2m_w] ),
                            .Q                      (b01_dram_rdata_o[r2m_b][r2m_w] ),
                            .CEN                    (b01_dram_cs_mux[r2m_b][r2m_w]    ),
                            .WEN                    (b01_dram_wr_mux[r2m_b][r2m_w]    ),
                            .A                      (b01_dram_addr_mux[r2m_b][r2m_w]  ) 
            );
        end
            assign b01_uram_cs_mux[r2m_b]    = ram_initial_done_ff ? b01_uram_cs_i[r2m_b]    : 'd0;
            assign b01_uram_wr_mux[r2m_b]    = ram_initial_done_ff ? b01_uram_wr_i[r2m_b]    : 'd0;
            assign b01_uram_addr_mux[r2m_b]  = ram_initial_done_ff ? b01_uram_addr_i[r2m_b]  : ram_initial_scnt[BANK_2M_SET_IDX_WIDTH-1:0];
            assign b01_uram_wdata_mux[r2m_b] = ram_initial_done_ff ? b01_uram_wdata_i[r2m_b] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_2M_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b01_uram_wdata_mux[r2m_b]        ),
                            .Q                      (b01_uram_rdata_o[r2m_b]        ),
                            .CEN                    (b01_uram_cs_mux[r2m_b]           ),
                            .WEN                    (b01_uram_wr_mux[r2m_b]           ),
                            .A                      (b01_uram_addr_mux[r2m_b]         ) 
            );
    end
endgenerate
//}}}

//=== 1G {{{
genvar r1g_b, r1g_w;
generate
    for(r1g_b=0; r1g_b<BANK_1G_NUM; r1g_b++) begin : ram1g_inst_gen
        for(r1g_w=0; r1g_w<BANK_1G_WAY_NUM; r1g_w++) begin : way1g_inst_gen
            assign b10_tram_cs_mux[r1g_b][r1g_w]    = ram_initial_done_ff ? b10_tram_cs_i[r1g_b][r1g_w]    : 'd0;
            assign b10_tram_wr_mux[r1g_b][r1g_w]    = ram_initial_done_ff ? b10_tram_wr_i[r1g_b][r1g_w]    : 'd0;
            assign b10_tram_addr_mux[r1g_b][r1g_w]  = ram_initial_done_ff ? b10_tram_addr_i[r1g_b][r1g_w]  : ram_initial_scnt[BANK_1G_SET_IDX_WIDTH-1:0];
            assign b10_tram_wdata_mux[r1g_b][r1g_w] = ram_initial_done_ff ? b10_tram_wdata_i[r1g_b][r1g_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH                  ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_1G_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b10_tram_wdata_mux[r1g_b][r1g_w] ),
                            .Q                      (b10_tram_rdata_o[r1g_b][r1g_w] ),
                            .CEN                    (b10_tram_cs_mux[r1g_b][r1g_w]    ),
                            .WEN                    (b10_tram_wr_mux[r1g_b][r1g_w]    ),
                            .A                      (b10_tram_addr_mux[r1g_b][r1g_w]  ) 
            );

            assign b10_iram_cs_mux[r1g_b][r1g_w]    = ram_initial_done_ff ? b10_iram_cs_i[r1g_b][r1g_w]    : 'd0;
            assign b10_iram_wr_mux[r1g_b][r1g_w]    = ram_initial_done_ff ? b10_iram_wr_i[r1g_b][r1g_w]    : 'd0;
            assign b10_iram_addr_mux[r1g_b][r1g_w]  = ram_initial_done_ff ? b10_iram_addr_i[r1g_b][r1g_w]  : ram_initial_scnt[BANK_1G_SET_IDX_WIDTH-1:0];
            assign b10_iram_wdata_mux[r1g_b][r1g_w] = ram_initial_done_ff ? b10_iram_wdata_i[r1g_b][r1g_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (IRAM_DAT_WIDTH                ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_1G_SET_IDX_WIDTH          )  // = 6
            ) U_iram(
                            .CLK                    (clk                            ),
                            .D                      (b10_iram_wdata_mux[r1g_b][r1g_w] ),
                            .Q                      (b10_iram_rdata_o[r1g_b][r1g_w] ),
                            .CEN                    (b10_iram_cs_mux[r1g_b][r1g_w]    ),
                            .WEN                    (b10_iram_wr_mux[r1g_b][r1g_w]    ),
                            .A                      (b10_iram_addr_mux[r1g_b][r1g_w]  ) 
            );

            assign b10_dram_cs_mux[r1g_b][r1g_w]    = ram_initial_done_ff ? b10_dram_cs_i[r1g_b][r1g_w]    : 'd0;
            assign b10_dram_wr_mux[r1g_b][r1g_w]    = ram_initial_done_ff ? b10_dram_wr_i[r1g_b][r1g_w]    : 'd0;
            assign b10_dram_addr_mux[r1g_b][r1g_w]  = ram_initial_done_ff ? b10_dram_addr_i[r1g_b][r1g_w]  : ram_initial_scnt[BANK_1G_SET_IDX_WIDTH-1:0];
            assign b10_dram_wdata_mux[r1g_b][r1g_w] = ram_initial_done_ff ? b10_dram_wdata_i[r1g_b][r1g_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (DRAM_DAT_WIDTH                 ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_1G_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b10_dram_wdata_mux[r1g_b][r1g_w] ),
                            .Q                      (b10_dram_rdata_o[r1g_b][r1g_w] ),
                            .CEN                    (b10_dram_cs_mux[r1g_b][r1g_w]    ),
                            .WEN                    (b10_dram_wr_mux[r1g_b][r1g_w]    ),
                            .A                      (b10_dram_addr_mux[r1g_b][r1g_w]  ) 
            );
        end
            assign b10_uram_cs_mux[r1g_b]    = ram_initial_done_ff ? b10_uram_cs_i[r1g_b]    : 'd0;
            assign b10_uram_wr_mux[r1g_b]    = ram_initial_done_ff ? b10_uram_wr_i[r1g_b]    : 'd0;
            assign b10_uram_addr_mux[r1g_b]  = ram_initial_done_ff ? b10_uram_addr_i[r1g_b]  : ram_initial_scnt[BANK_1G_SET_IDX_WIDTH-1:0];
            assign b10_uram_wdata_mux[r1g_b] = ram_initial_done_ff ? b10_uram_wdata_i[r1g_b] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_1G_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b10_uram_wdata_mux[r1g_b]        ),
                            .Q                      (b10_uram_rdata_o[r1g_b]        ),
                            .CEN                    (b10_uram_cs_mux[r1g_b]           ),
                            .WEN                    (b10_uram_wr_mux[r1g_b]           ),
                            .A                      (b10_uram_addr_mux[r1g_b]         ) 
            );
    end
endgenerate
//}}}

//=== 0T {{{
genvar r0t_b, r0t_w;
generate
    for(r0t_b=0; r0t_b<BANK_0T_NUM; r0t_b++) begin : ram0t_inst_gen
        for(r0t_w=0; r0t_w<BANK_0T_WAY_NUM; r0t_w++) begin : way0t_inst_gen
            assign b11_tram_cs_mux[r0t_b][r0t_w]    = ram_initial_done_ff ? b11_tram_cs_i[r0t_b][r0t_w]    : 'd0;
            assign b11_tram_wr_mux[r0t_b][r0t_w]    = ram_initial_done_ff ? b11_tram_wr_i[r0t_b][r0t_w]    : 'd0;
            assign b11_tram_addr_mux[r0t_b][r0t_w]  = ram_initial_done_ff ? b11_tram_addr_i[r0t_b][r0t_w]  : ram_initial_scnt[BANK_0T_SET_IDX_WIDTH-1:0];
            assign b11_tram_wdata_mux[r0t_b][r0t_w] = ram_initial_done_ff ? b11_tram_wdata_i[r0t_b][r0t_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH                  ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_0T_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b11_tram_wdata_mux[r0t_b][r0t_w] ),
                            .Q                      (b11_tram_rdata_o[r0t_b][r0t_w] ),
                            .CEN                    (b11_tram_cs_mux[r0t_b][r0t_w]    ),
                            .WEN                    (b11_tram_wr_mux[r0t_b][r0t_w]    ),
                            .A                      (b11_tram_addr_mux[r0t_b][r0t_w]  ) 
            );

            assign b11_iram_cs_mux[r0t_b][r0t_w]    = ram_initial_done_ff ? b11_iram_cs_i[r0t_b][r0t_w]    : 'd0;
            assign b11_iram_wr_mux[r0t_b][r0t_w]    = ram_initial_done_ff ? b11_iram_wr_i[r0t_b][r0t_w]    : 'd0;
            assign b11_iram_addr_mux[r0t_b][r0t_w]  = ram_initial_done_ff ? b11_iram_addr_i[r0t_b][r0t_w]  : ram_initial_scnt[BANK_0T_SET_IDX_WIDTH-1:0];
            assign b11_iram_wdata_mux[r0t_b][r0t_w] = ram_initial_done_ff ? b11_iram_wdata_i[r0t_b][r0t_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (IRAM_DAT_WIDTH                ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_0T_SET_IDX_WIDTH          )  // = 6
            ) U_iram(
                            .CLK                    (clk                            ),
                            .D                      (b11_iram_wdata_mux[r0t_b][r0t_w] ),
                            .Q                      (b11_iram_rdata_o[r0t_b][r0t_w] ),
                            .CEN                    (b11_iram_cs_mux[r0t_b][r0t_w]    ),
                            .WEN                    (b11_iram_wr_mux[r0t_b][r0t_w]    ),
                            .A                      (b11_iram_addr_mux[r0t_b][r0t_w]  ) 
            );

            assign b11_dram_cs_mux[r0t_b][r0t_w]    = ram_initial_done_ff ? b11_dram_cs_i[r0t_b][r0t_w]    : 'd0;
            assign b11_dram_wr_mux[r0t_b][r0t_w]    = ram_initial_done_ff ? b11_dram_wr_i[r0t_b][r0t_w]    : 'd0;
            assign b11_dram_addr_mux[r0t_b][r0t_w]  = ram_initial_done_ff ? b11_dram_addr_i[r0t_b][r0t_w]  : ram_initial_scnt[BANK_0T_SET_IDX_WIDTH-1:0];
            assign b11_dram_wdata_mux[r0t_b][r0t_w] = ram_initial_done_ff ? b11_dram_wdata_i[r0t_b][r0t_w] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (DRAM_DAT_WIDTH                 ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_0T_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b11_dram_wdata_mux[r0t_b][r0t_w] ),
                            .Q                      (b11_dram_rdata_o[r0t_b][r0t_w] ),
                            .CEN                    (b11_dram_cs_mux[r0t_b][r0t_w]    ),
                            .WEN                    (b11_dram_wr_mux[r0t_b][r0t_w]    ),
                            .A                      (b11_dram_addr_mux[r0t_b][r0t_w]  ) 
            );
        end
            assign b11_uram_cs_mux[r0t_b]    = ram_initial_done_ff ? b11_uram_cs_i[r0t_b]    : 'd0;
            assign b11_uram_wr_mux[r0t_b]    = ram_initial_done_ff ? b11_uram_wr_i[r0t_b]    : 'd0;
            assign b11_uram_addr_mux[r0t_b]  = ram_initial_done_ff ? b11_uram_addr_i[r0t_b]  : ram_initial_scnt[BANK_0T_SET_IDX_WIDTH-1:0];
            assign b11_uram_wdata_mux[r0t_b] = ram_initial_done_ff ? b11_uram_wdata_i[r0t_b] : 'd0;
            iommu_acd_tlb_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_0T_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b11_uram_wdata_mux[r0t_b]        ),
                            .Q                      (b11_uram_rdata_o[r0t_b]        ),
                            .CEN                    (b11_uram_cs_mux[r0t_b]           ),
                            .WEN                    (b11_uram_wr_mux[r0t_b]           ),
                            .A                      (b11_uram_addr_mux[r0t_b]         ) 
            );
    end
endgenerate
//}}}

//}}}

endmodule//}}}
