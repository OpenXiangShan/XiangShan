module iommu_atd_ptc_mtlb_ram_wrap #(
    parameter   BANK_L0_IDX_WIDTH                                           = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_IDX_WIDTH                                           = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_IDX_WIDTH                                           = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    parameter   BANK_L3_IDX_WIDTH                                           = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    parameter   BANK_L4_IDX_WIDTH                                           = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    parameter   BANK_L0_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L3_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    parameter   BANK_L4_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    parameter   BANK_L0_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    parameter   BANK_L1_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    parameter   BANK_L2_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    parameter   BANK_L3_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    parameter   BANK_L4_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    parameter   BANK_L0_NUM                                                 = 2**BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_NUM                                                 = 2**BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_NUM                                                 = 2**BANK_L2_IDX_WIDTH,
    parameter   BANK_L3_NUM                                                 = 2**BANK_L3_IDX_WIDTH,
    parameter   BANK_L4_NUM                                                 = 2**BANK_L4_IDX_WIDTH,
    parameter   BANK_L0_SET_NUM                                             = 2**BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_NUM                                             = 2**BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_NUM                                             = 2**BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L3_SET_NUM                                             = 2**BANK_L3_SET_IDX_WIDTH,
    parameter   BANK_L4_SET_NUM                                             = 2**BANK_L4_SET_IDX_WIDTH,
    parameter   BANK_L0_WAY_NUM                                             = 2**BANK_L0_WAY_IDX_WIDTH,
    parameter   BANK_L1_WAY_NUM                                             = 2**BANK_L1_WAY_IDX_WIDTH,
    parameter   BANK_L2_WAY_NUM                                             = 2**BANK_L2_WAY_IDX_WIDTH,
    parameter   BANK_L3_WAY_NUM                                             = 2**BANK_L3_WAY_IDX_WIDTH,
    parameter   BANK_L4_WAY_NUM                                             = 2**BANK_L4_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                                               = iommu_atd_cache_pkg::s2ptc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                                              = iommu_atd_cache_pkg::s2ptc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                                              = iommu_atd_cache_pkg::s2ptc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                                               = iommu_atd_cache_pkg::s2ptc_mtlb_dat_t_l,
    parameter type          MTLB_DAT_TYPE_NL                                            = iommu_atd_cache_pkg::s2ptc_mtlb_dat_t_nl,
    parameter   TECC_WIDTH                                                  = 5,
    parameter   LDECC_WIDTH                                                 = 9,
    parameter   TRAM_DAT_WIDTH                                              = $bits(MTLB_TAG_TYPE)+TECC_WIDTH+1,
    parameter   IRAM_DAT_WIDTH                                              = $bits(MTLB_ITAG_TYPE)+TECC_WIDTH+1,
    parameter   LDRAM_DAT_WIDTH                                             = $bits(MTLB_DAT_TYPE)+LDECC_WIDTH+1,
    parameter   NLDECC_WIDTH                                                = 6,
    parameter   NLDRAM_DAT_WIDTH                                            = $bits(MTLB_DAT_TYPE_NL)+NLDECC_WIDTH+1,
    parameter   BANK_L4_EMPTY                                               = 0,
    parameter   SPARE_PARAM                                                 = 0
)(
//=== IO {{{
    input  logic                                                                        clk,
    input  logic                                                                        rstn,
    // RAM
    output logic                                                                        ram_initial_done_o,

    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_tram_cs_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_tram_wr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]    b00_tram_addr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b00_tram_wdata_i,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b00_tram_rdata_o,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_iram_cs_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_iram_wr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]    b00_iram_addr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b00_iram_wdata_i,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b00_iram_rdata_o,
    input  logic [BANK_L0_NUM-1:0]                                                      b00_uram_cs_i,
    input  logic [BANK_L0_NUM-1:0]                                                      b00_uram_wr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]                          b00_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                             b00_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                             b00_uram_rdata_o,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_dram_cs_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_dram_wr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]    b00_dram_addr_i,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]          b00_dram_wdata_i,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]          b00_dram_rdata_o,

    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_tram_cs_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_tram_wr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]    b01_tram_addr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b01_tram_wdata_i,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b01_tram_rdata_o,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_iram_cs_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_iram_wr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]    b01_iram_addr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b01_iram_wdata_i,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b01_iram_rdata_o,
    input  logic [BANK_L1_NUM-1:0]                                                      b01_uram_cs_i,
    input  logic [BANK_L1_NUM-1:0]                                                      b01_uram_wr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]                          b01_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                             b01_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                             b01_uram_rdata_o,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_dram_cs_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_dram_wr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]    b01_dram_addr_i,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b01_dram_wdata_i,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b01_dram_rdata_o,

    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_tram_cs_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_tram_wr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]    b10_tram_addr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b10_tram_wdata_i,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b10_tram_rdata_o,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_iram_cs_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_iram_wr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]    b10_iram_addr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b10_iram_wdata_i,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b10_iram_rdata_o,
    input  logic [BANK_L2_NUM-1:0]                                                      b10_uram_cs_i,
    input  logic [BANK_L2_NUM-1:0]                                                      b10_uram_wr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]                          b10_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                             b10_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                             b10_uram_rdata_o,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_dram_cs_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_dram_wr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]    b10_dram_addr_i,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b10_dram_wdata_i,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b10_dram_rdata_o,

    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                                b11_tram_cs_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                                b11_tram_wr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]    b11_tram_addr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b11_tram_wdata_i,
    output logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b11_tram_rdata_o,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                                b11_iram_cs_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                                b11_iram_wr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]    b11_iram_addr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b11_iram_wdata_i,
    output logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b11_iram_rdata_o,
    input  logic [BANK_L3_NUM-1:0]                                                      b11_uram_cs_i,
    input  logic [BANK_L3_NUM-1:0]                                                      b11_uram_wr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]                          b11_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_L3_NUM-1:0]                                             b11_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_L3_NUM-1:0]                                             b11_uram_rdata_o,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                                b11_dram_cs_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                                b11_dram_wr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]    b11_dram_addr_i,
    input  logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b11_dram_wdata_i,
    output logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b11_dram_rdata_o,

    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                                b04_tram_cs_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                                b04_tram_wr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]    b04_tram_addr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b04_tram_wdata_i,
    output logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b04_tram_rdata_o,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                                b04_iram_cs_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                                b04_iram_wr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]    b04_iram_addr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b04_iram_wdata_i,
    output logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b04_iram_rdata_o,
    input  logic [BANK_L4_NUM-1:0]                                                      b04_uram_cs_i,
    input  logic [BANK_L4_NUM-1:0]                                                      b04_uram_wr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]                          b04_uram_addr_i,
    input  MTLB_UTAG_TYPE [BANK_L4_NUM-1:0]                                             b04_uram_wdata_i,
    output MTLB_UTAG_TYPE [BANK_L4_NUM-1:0]                                             b04_uram_rdata_o,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                                b04_dram_cs_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                                b04_dram_wr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]    b04_dram_addr_i,
    input  logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b04_dram_wdata_i,
    output logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b04_dram_rdata_o,

    //                                                                                  
    input  logic                                                                        spare_in
//}}}
);
//=== Declare {{{
    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_L0_IDX_WIDTH > BANK_L1_IDX_WIDTH) ? BANK_L0_IDX_WIDTH : BANK_L1_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = (BANK_L2_IDX_WIDTH > BANK_L3_IDX_WIDTH) ? BANK_L2_IDX_WIDTH : BANK_L3_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_2 =  BANK_L4_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_01= (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_2 > MAX_BANK_IDX_WIDTH_01)? MAX_BANK_IDX_WIDTH_2 : MAX_BANK_IDX_WIDTH_01;
    localparam MAX_BANK_NUM         = 2**MAX_BANK_IDX_WIDTH;
    
    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_L0_SET_IDX_WIDTH > BANK_L1_SET_IDX_WIDTH) ? BANK_L0_SET_IDX_WIDTH : BANK_L1_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = (BANK_L2_SET_IDX_WIDTH > BANK_L3_SET_IDX_WIDTH) ? BANK_L2_SET_IDX_WIDTH : BANK_L3_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_2 =  BANK_L4_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_01= (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_2 > MAX_BANK_SET_IDX_WIDTH_01)? MAX_BANK_SET_IDX_WIDTH_2 : MAX_BANK_SET_IDX_WIDTH_01;
    localparam MAX_BANK_SET_NUM         = 2**MAX_BANK_SET_IDX_WIDTH;
    

    logic [MAX_BANK_SET_IDX_WIDTH-1:0]  ram_initial_scnt;
    logic                               ram_initial_done, ram_initial_done_ff;

    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               b00_tram_cs_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               b00_tram_wr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]   b00_tram_addr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b00_tram_wdata_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               b00_iram_cs_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               b00_iram_wr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]   b00_iram_addr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b00_iram_wdata_mux;
    logic [BANK_L0_NUM-1:0]                                                     b00_uram_cs_mux;
    logic [BANK_L0_NUM-1:0]                                                     b00_uram_wr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]                         b00_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                            b00_uram_wdata_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               b00_dram_cs_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               b00_dram_wr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]   b00_dram_addr_mux;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]         b00_dram_wdata_mux;

    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               b01_tram_cs_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               b01_tram_wr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]   b01_tram_addr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b01_tram_wdata_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               b01_iram_cs_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               b01_iram_wr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]   b01_iram_addr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b01_iram_wdata_mux;
    logic [BANK_L1_NUM-1:0]                                                     b01_uram_cs_mux;
    logic [BANK_L1_NUM-1:0]                                                     b01_uram_wr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]                         b01_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                            b01_uram_wdata_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               b01_dram_cs_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               b01_dram_wr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]   b01_dram_addr_mux;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        b01_dram_wdata_mux;

    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               b10_tram_cs_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               b10_tram_wr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]   b10_tram_addr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b10_tram_wdata_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               b10_iram_cs_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               b10_iram_wr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]   b10_iram_addr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b10_iram_wdata_mux;
    logic [BANK_L2_NUM-1:0]                                                     b10_uram_cs_mux;
    logic [BANK_L2_NUM-1:0]                                                     b10_uram_wr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]                         b10_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                            b10_uram_wdata_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               b10_dram_cs_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               b10_dram_wr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]   b10_dram_addr_mux;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        b10_dram_wdata_mux;

    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                               b11_tram_cs_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                               b11_tram_wr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]   b11_tram_addr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b11_tram_wdata_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                               b11_iram_cs_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                               b11_iram_wr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]   b11_iram_addr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b11_iram_wdata_mux;
    logic [BANK_L3_NUM-1:0]                                                     b11_uram_cs_mux;
    logic [BANK_L3_NUM-1:0]                                                     b11_uram_wr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]                         b11_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_L3_NUM-1:0]                                            b11_uram_wdata_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                               b11_dram_cs_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0]                               b11_dram_wr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [BANK_L3_SET_IDX_WIDTH-1:0]   b11_dram_addr_mux;
    logic [BANK_L3_NUM-1:0] [BANK_L3_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        b11_dram_wdata_mux;

    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                               b04_tram_cs_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                               b04_tram_wr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]   b04_tram_addr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          b04_tram_wdata_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                               b04_iram_cs_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                               b04_iram_wr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]   b04_iram_addr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          b04_iram_wdata_mux;
    logic [BANK_L4_NUM-1:0]                                                     b04_uram_cs_mux;
    logic [BANK_L4_NUM-1:0]                                                     b04_uram_wr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]                         b04_uram_addr_mux;
    MTLB_UTAG_TYPE [BANK_L4_NUM-1:0]                                            b04_uram_wdata_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                               b04_dram_cs_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0]                               b04_dram_wr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [BANK_L4_SET_IDX_WIDTH-1:0]   b04_dram_addr_mux;
    logic [BANK_L4_NUM-1:0] [BANK_L4_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        b04_dram_wdata_mux;

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
//=== L0 {{{
genvar r0_b, r0_w;
generate
    for(r0_b=0; r0_b<BANK_L0_NUM; r0_b++) begin : raml0_gen
        for(r0_w=0; r0_w<BANK_L0_WAY_NUM; r0_w++) begin : wayl0_gen
            assign b00_tram_cs_mux[r0_b][r0_w]    = ram_initial_done_ff ? b00_tram_cs_i[r0_b][r0_w]    : 'd0;
            assign b00_tram_wr_mux[r0_b][r0_w]    = ram_initial_done_ff ? b00_tram_wr_i[r0_b][r0_w]    : 'd0;
            assign b00_tram_addr_mux[r0_b][r0_w]  = ram_initial_done_ff ? b00_tram_addr_i[r0_b][r0_w]  : ram_initial_scnt[BANK_L0_SET_IDX_WIDTH-1:0];
            assign b00_tram_wdata_mux[r0_b][r0_w] = ram_initial_done_ff ? b00_tram_wdata_i[r0_b][r0_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L0_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b00_tram_wdata_mux[r0_b][r0_w] ),
                            .Q                      (b00_tram_rdata_o[r0_b][r0_w] ),
                            .CEN                    (b00_tram_cs_mux[r0_b][r0_w]    ),
                            .WEN                    (b00_tram_wr_mux[r0_b][r0_w]    ),
                            .A                      (b00_tram_addr_mux[r0_b][r0_w]  ) 
            );

            assign b00_iram_cs_mux[r0_b][r0_w]    = ram_initial_done_ff ? b00_iram_cs_i[r0_b][r0_w]    : 'd0;
            assign b00_iram_wr_mux[r0_b][r0_w]    = ram_initial_done_ff ? b00_iram_wr_i[r0_b][r0_w]    : 'd0;
            assign b00_iram_addr_mux[r0_b][r0_w]  = ram_initial_done_ff ? b00_iram_addr_i[r0_b][r0_w]  : ram_initial_scnt[BANK_L0_SET_IDX_WIDTH-1:0];
            assign b00_iram_wdata_mux[r0_b][r0_w] = ram_initial_done_ff ? b00_iram_wdata_i[r0_b][r0_w] : 'd0;
            //iommu_atd_cache_ram_wrap #(
            ///*parameter */  .RAM_WIDTH              ($bits(MTLB_ITAG_TYPE)          ), // = 64,
            ///*parameter */  .ADDR_WIDTH             (BANK_L0_SET_IDX_WIDTH          )  // = 6
            //) U_iram(
            //                .CLK                    (clk                            ),
            //                .D                      (b00_iram_wdata_mux[r0_b][r0_w] ),
            //                .Q                      (b00_iram_rdata_o[r0_b][r0_w] ),
            //                .CEN                    (b00_iram_cs_mux[r0_b][r0_w]    ),
            //                .WEN                    (b00_iram_wr_mux[r0_b][r0_w]    ),
            //                .A                      (b00_iram_addr_mux[r0_b][r0_w]  ) 
            //);
            assign b00_iram_rdata_o[r0_b][r0_w] = 'd0;

            assign b00_dram_cs_mux[r0_b][r0_w]    = ram_initial_done_ff ? b00_dram_cs_i[r0_b][r0_w]    : 'd0;
            assign b00_dram_wr_mux[r0_b][r0_w]    = ram_initial_done_ff ? b00_dram_wr_i[r0_b][r0_w]    : 'd0;
            assign b00_dram_addr_mux[r0_b][r0_w]  = ram_initial_done_ff ? b00_dram_addr_i[r0_b][r0_w]  : ram_initial_scnt[BANK_L0_SET_IDX_WIDTH-1:0];
            assign b00_dram_wdata_mux[r0_b][r0_w] = ram_initial_done_ff ? b00_dram_wdata_i[r0_b][r0_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (LDRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L0_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b00_dram_wdata_mux[r0_b][r0_w] ),
                            .Q                      (b00_dram_rdata_o[r0_b][r0_w] ),
                            .CEN                    (b00_dram_cs_mux[r0_b][r0_w]    ),
                            .WEN                    (b00_dram_wr_mux[r0_b][r0_w]    ),
                            .A                      (b00_dram_addr_mux[r0_b][r0_w]  ) 
            );
        end
            assign b00_uram_cs_mux[r0_b]    = ram_initial_done_ff ? b00_uram_cs_i[r0_b]    : 'd0;
            assign b00_uram_wr_mux[r0_b]    = ram_initial_done_ff ? b00_uram_wr_i[r0_b]    : 'd0;
            assign b00_uram_addr_mux[r0_b]  = ram_initial_done_ff ? b00_uram_addr_i[r0_b]  : ram_initial_scnt[BANK_L0_SET_IDX_WIDTH-1:0];
            assign b00_uram_wdata_mux[r0_b] = ram_initial_done_ff ? b00_uram_wdata_i[r0_b] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L0_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b00_uram_wdata_mux[r0_b]        ),
                            .Q                      (b00_uram_rdata_o[r0_b]        ),
                            .CEN                    (b00_uram_cs_mux[r0_b]           ),
                            .WEN                    (b00_uram_wr_mux[r0_b]           ),
                            .A                      (b00_uram_addr_mux[r0_b]         ) 
            );
    end
endgenerate
//}}}

//=== L1 {{{
genvar r1_b, r1_w;
generate
    for(r1_b=0; r1_b<BANK_L1_NUM; r1_b++) begin : raml1_gen
        for(r1_w=0; r1_w<BANK_L1_WAY_NUM; r1_w++) begin : wayl1_gen
            assign b01_tram_cs_mux[r1_b][r1_w]    = ram_initial_done_ff ? b01_tram_cs_i[r1_b][r1_w]    : 'd0;
            assign b01_tram_wr_mux[r1_b][r1_w]    = ram_initial_done_ff ? b01_tram_wr_i[r1_b][r1_w]    : 'd0;
            assign b01_tram_addr_mux[r1_b][r1_w]  = ram_initial_done_ff ? b01_tram_addr_i[r1_b][r1_w]  : ram_initial_scnt[BANK_L1_SET_IDX_WIDTH-1:0];
            assign b01_tram_wdata_mux[r1_b][r1_w] = ram_initial_done_ff ? b01_tram_wdata_i[r1_b][r1_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L1_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b01_tram_wdata_mux[r1_b][r1_w] ),
                            .Q                      (b01_tram_rdata_o[r1_b][r1_w] ),
                            .CEN                    (b01_tram_cs_mux[r1_b][r1_w]    ),
                            .WEN                    (b01_tram_wr_mux[r1_b][r1_w]    ),
                            .A                      (b01_tram_addr_mux[r1_b][r1_w]  ) 
            );

            assign b01_iram_cs_mux[r1_b][r1_w]    = ram_initial_done_ff ? b01_iram_cs_i[r1_b][r1_w]    : 'd0;
            assign b01_iram_wr_mux[r1_b][r1_w]    = ram_initial_done_ff ? b01_iram_wr_i[r1_b][r1_w]    : 'd0;
            assign b01_iram_addr_mux[r1_b][r1_w]  = ram_initial_done_ff ? b01_iram_addr_i[r1_b][r1_w]  : ram_initial_scnt[BANK_L1_SET_IDX_WIDTH-1:0];
            assign b01_iram_wdata_mux[r1_b][r1_w] = ram_initial_done_ff ? b01_iram_wdata_i[r1_b][r1_w] : 'd0;
            //iommu_atd_cache_ram_wrap #(
            ///*parameter */  .RAM_WIDTH              ($bits(MTLB_ITAG_TYPE)          ), // = 64,
            ///*parameter */  .ADDR_WIDTH             (BANK_L1_SET_IDX_WIDTH          )  // = 6
            //) U_iram(
            //                .CLK                    (clk                            ),
            //                .D                      (b01_iram_wdata_mux[r1_b][r1_w] ),
            //                .Q                      (b01_iram_rdata_o[r1_b][r1_w] ),
            //                .CEN                    (b01_iram_cs_mux[r1_b][r1_w]    ),
            //                .WEN                    (b01_iram_wr_mux[r1_b][r1_w]    ),
            //                .A                      (b01_iram_addr_mux[r1_b][r1_w]  ) 
            //);
            assign b01_iram_rdata_o[r1_b][r1_w] = 'd0;

            assign b01_dram_cs_mux[r1_b][r1_w]    = ram_initial_done_ff ? b01_dram_cs_i[r1_b][r1_w]    : 'd0;
            assign b01_dram_wr_mux[r1_b][r1_w]    = ram_initial_done_ff ? b01_dram_wr_i[r1_b][r1_w]    : 'd0;
            assign b01_dram_addr_mux[r1_b][r1_w]  = ram_initial_done_ff ? b01_dram_addr_i[r1_b][r1_w]  : ram_initial_scnt[BANK_L1_SET_IDX_WIDTH-1:0];
            assign b01_dram_wdata_mux[r1_b][r1_w] = ram_initial_done_ff ? b01_dram_wdata_i[r1_b][r1_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (NLDRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L1_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b01_dram_wdata_mux[r1_b][r1_w] ),
                            .Q                      (b01_dram_rdata_o[r1_b][r1_w] ),
                            .CEN                    (b01_dram_cs_mux[r1_b][r1_w]    ),
                            .WEN                    (b01_dram_wr_mux[r1_b][r1_w]    ),
                            .A                      (b01_dram_addr_mux[r1_b][r1_w]  ) 
            );
        end
            assign b01_uram_cs_mux[r1_b]    = ram_initial_done_ff ? b01_uram_cs_i[r1_b]    : 'd0;
            assign b01_uram_wr_mux[r1_b]    = ram_initial_done_ff ? b01_uram_wr_i[r1_b]    : 'd0;
            assign b01_uram_addr_mux[r1_b]  = ram_initial_done_ff ? b01_uram_addr_i[r1_b]  : ram_initial_scnt[BANK_L1_SET_IDX_WIDTH-1:0];
            assign b01_uram_wdata_mux[r1_b] = ram_initial_done_ff ? b01_uram_wdata_i[r1_b] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L1_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b01_uram_wdata_mux[r1_b]        ),
                            .Q                      (b01_uram_rdata_o[r1_b]        ),
                            .CEN                    (b01_uram_cs_mux[r1_b]           ),
                            .WEN                    (b01_uram_wr_mux[r1_b]           ),
                            .A                      (b01_uram_addr_mux[r1_b]         ) 
            );
    end
endgenerate
//}}}

//=== L2 {{{
genvar r2_b, r2_w;
generate
    for(r2_b=0; r2_b<BANK_L2_NUM; r2_b++) begin : raml2_gen
        for(r2_w=0; r2_w<BANK_L2_WAY_NUM; r2_w++) begin : wayl2_gen
            assign b10_tram_cs_mux[r2_b][r2_w]    = ram_initial_done_ff ? b10_tram_cs_i[r2_b][r2_w]    : 'd0;
            assign b10_tram_wr_mux[r2_b][r2_w]    = ram_initial_done_ff ? b10_tram_wr_i[r2_b][r2_w]    : 'd0;
            assign b10_tram_addr_mux[r2_b][r2_w]  = ram_initial_done_ff ? b10_tram_addr_i[r2_b][r2_w]  : ram_initial_scnt[BANK_L2_SET_IDX_WIDTH-1:0];
            assign b10_tram_wdata_mux[r2_b][r2_w] = ram_initial_done_ff ? b10_tram_wdata_i[r2_b][r2_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L2_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b10_tram_wdata_mux[r2_b][r2_w] ),
                            .Q                      (b10_tram_rdata_o[r2_b][r2_w] ),
                            .CEN                    (b10_tram_cs_mux[r2_b][r2_w]    ),
                            .WEN                    (b10_tram_wr_mux[r2_b][r2_w]    ),
                            .A                      (b10_tram_addr_mux[r2_b][r2_w]  ) 
            );

            assign b10_iram_cs_mux[r2_b][r2_w]    = ram_initial_done_ff ? b10_iram_cs_i[r2_b][r2_w]    : 'd0;
            assign b10_iram_wr_mux[r2_b][r2_w]    = ram_initial_done_ff ? b10_iram_wr_i[r2_b][r2_w]    : 'd0;
            assign b10_iram_addr_mux[r2_b][r2_w]  = ram_initial_done_ff ? b10_iram_addr_i[r2_b][r2_w]  : ram_initial_scnt[BANK_L2_SET_IDX_WIDTH-1:0];
            assign b10_iram_wdata_mux[r2_b][r2_w] = ram_initial_done_ff ? b10_iram_wdata_i[r2_b][r2_w] : 'd0;
            //iommu_atd_cache_ram_wrap #(
            ///*parameter */  .RAM_WIDTH              ($bits(MTLB_ITAG_TYPE)          ), // = 64,
            ///*parameter */  .ADDR_WIDTH             (BANK_L2_SET_IDX_WIDTH          )  // = 6
            //) U_iram(
            //                .CLK                    (clk                            ),
            //                .D                      (b10_iram_wdata_mux[r2_b][r2_w] ),
            //                .Q                      (b10_iram_rdata_o[r2_b][r2_w] ),
            //                .CEN                    (b10_iram_cs_mux[r2_b][r2_w]    ),
            //                .WEN                    (b10_iram_wr_mux[r2_b][r2_w]    ),
            //                .A                      (b10_iram_addr_mux[r2_b][r2_w]  ) 
            //);
            assign b10_iram_rdata_o[r2_b][r2_w] = 'd0;

            assign b10_dram_cs_mux[r2_b][r2_w]    = ram_initial_done_ff ? b10_dram_cs_i[r2_b][r2_w]    : 'd0;
            assign b10_dram_wr_mux[r2_b][r2_w]    = ram_initial_done_ff ? b10_dram_wr_i[r2_b][r2_w]    : 'd0;
            assign b10_dram_addr_mux[r2_b][r2_w]  = ram_initial_done_ff ? b10_dram_addr_i[r2_b][r2_w]  : ram_initial_scnt[BANK_L2_SET_IDX_WIDTH-1:0];
            assign b10_dram_wdata_mux[r2_b][r2_w] = ram_initial_done_ff ? b10_dram_wdata_i[r2_b][r2_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (NLDRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L2_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b10_dram_wdata_mux[r2_b][r2_w] ),
                            .Q                      (b10_dram_rdata_o[r2_b][r2_w] ),
                            .CEN                    (b10_dram_cs_mux[r2_b][r2_w]    ),
                            .WEN                    (b10_dram_wr_mux[r2_b][r2_w]    ),
                            .A                      (b10_dram_addr_mux[r2_b][r2_w]  ) 
            );
        end
            assign b10_uram_cs_mux[r2_b]    = ram_initial_done_ff ? b10_uram_cs_i[r2_b]    : 'd0;
            assign b10_uram_wr_mux[r2_b]    = ram_initial_done_ff ? b10_uram_wr_i[r2_b]    : 'd0;
            assign b10_uram_addr_mux[r2_b]  = ram_initial_done_ff ? b10_uram_addr_i[r2_b]  : ram_initial_scnt[BANK_L2_SET_IDX_WIDTH-1:0];
            assign b10_uram_wdata_mux[r2_b] = ram_initial_done_ff ? b10_uram_wdata_i[r2_b] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L2_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b10_uram_wdata_mux[r2_b]        ),
                            .Q                      (b10_uram_rdata_o[r2_b]        ),
                            .CEN                    (b10_uram_cs_mux[r2_b]           ),
                            .WEN                    (b10_uram_wr_mux[r2_b]           ),
                            .A                      (b10_uram_addr_mux[r2_b]         ) 
            );
    end
endgenerate
//}}}

//=== L3 {{{
genvar r3_b, r3_w;
generate
    for(r3_b=0; r3_b<BANK_L3_NUM; r3_b++) begin : raml3_gen
        for(r3_w=0; r3_w<BANK_L3_WAY_NUM; r3_w++) begin : wayl3_gen
            assign b11_tram_cs_mux[r3_b][r3_w]    = ram_initial_done_ff ? b11_tram_cs_i[r3_b][r3_w]    : 'd0;
            assign b11_tram_wr_mux[r3_b][r3_w]    = ram_initial_done_ff ? b11_tram_wr_i[r3_b][r3_w]    : 'd0;
            assign b11_tram_addr_mux[r3_b][r3_w]  = ram_initial_done_ff ? b11_tram_addr_i[r3_b][r3_w]  : ram_initial_scnt[BANK_L3_SET_IDX_WIDTH-1:0];
            assign b11_tram_wdata_mux[r3_b][r3_w] = ram_initial_done_ff ? b11_tram_wdata_i[r3_b][r3_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L3_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b11_tram_wdata_mux[r3_b][r3_w] ),
                            .Q                      (b11_tram_rdata_o[r3_b][r3_w] ),
                            .CEN                    (b11_tram_cs_mux[r3_b][r3_w]    ),
                            .WEN                    (b11_tram_wr_mux[r3_b][r3_w]    ),
                            .A                      (b11_tram_addr_mux[r3_b][r3_w]  ) 
            );

            assign b11_iram_cs_mux[r3_b][r3_w]    = ram_initial_done_ff ? b11_iram_cs_i[r3_b][r3_w]    : 'd0;
            assign b11_iram_wr_mux[r3_b][r3_w]    = ram_initial_done_ff ? b11_iram_wr_i[r3_b][r3_w]    : 'd0;
            assign b11_iram_addr_mux[r3_b][r3_w]  = ram_initial_done_ff ? b11_iram_addr_i[r3_b][r3_w]  : ram_initial_scnt[BANK_L3_SET_IDX_WIDTH-1:0];
            assign b11_iram_wdata_mux[r3_b][r3_w] = ram_initial_done_ff ? b11_iram_wdata_i[r3_b][r3_w] : 'd0;
            //iommu_atd_cache_ram_wrap #(
            ///*parameter */  .RAM_WIDTH              ($bits(MTLB_ITAG_TYPE)          ), // = 64,
            ///*parameter */  .ADDR_WIDTH             (BANK_L3_SET_IDX_WIDTH          )  // = 6
            //) U_iram(
            //                .CLK                    (clk                            ),
            //                .D                      (b11_iram_wdata_mux[r3_b][r3_w] ),
            //                .Q                      (b11_iram_rdata_o[r3_b][r3_w] ),
            //                .CEN                    (b11_iram_cs_mux[r3_b][r3_w]    ),
            //                .WEN                    (b11_iram_wr_mux[r3_b][r3_w]    ),
            //                .A                      (b11_iram_addr_mux[r3_b][r3_w]  ) 
            //);
            assign b11_iram_rdata_o[r3_b][r3_w] = 'd0;

            assign b11_dram_cs_mux[r3_b][r3_w]    = ram_initial_done_ff ? b11_dram_cs_i[r3_b][r3_w]    : 'd0;
            assign b11_dram_wr_mux[r3_b][r3_w]    = ram_initial_done_ff ? b11_dram_wr_i[r3_b][r3_w]    : 'd0;
            assign b11_dram_addr_mux[r3_b][r3_w]  = ram_initial_done_ff ? b11_dram_addr_i[r3_b][r3_w]  : ram_initial_scnt[BANK_L3_SET_IDX_WIDTH-1:0];
            assign b11_dram_wdata_mux[r3_b][r3_w] = ram_initial_done_ff ? b11_dram_wdata_i[r3_b][r3_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (NLDRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L3_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b11_dram_wdata_mux[r3_b][r3_w] ),
                            .Q                      (b11_dram_rdata_o[r3_b][r3_w] ),
                            .CEN                    (b11_dram_cs_mux[r3_b][r3_w]    ),
                            .WEN                    (b11_dram_wr_mux[r3_b][r3_w]    ),
                            .A                      (b11_dram_addr_mux[r3_b][r3_w]  ) 
            );
        end
            assign b11_uram_cs_mux[r3_b]    = ram_initial_done_ff ? b11_uram_cs_i[r3_b]    : 'd0;
            assign b11_uram_wr_mux[r3_b]    = ram_initial_done_ff ? b11_uram_wr_i[r3_b]    : 'd0;
            assign b11_uram_addr_mux[r3_b]  = ram_initial_done_ff ? b11_uram_addr_i[r3_b]  : ram_initial_scnt[BANK_L3_SET_IDX_WIDTH-1:0];
            assign b11_uram_wdata_mux[r3_b] = ram_initial_done_ff ? b11_uram_wdata_i[r3_b] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L3_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b11_uram_wdata_mux[r3_b]        ),
                            .Q                      (b11_uram_rdata_o[r3_b]        ),
                            .CEN                    (b11_uram_cs_mux[r3_b]           ),
                            .WEN                    (b11_uram_wr_mux[r3_b]           ),
                            .A                      (b11_uram_addr_mux[r3_b]         ) 
            );
    end
endgenerate
//}}}

//=== L4 {{{
genvar r4_b, r4_w;
generate
    if(BANK_L4_EMPTY=='d1) begin : empty_l4_gen
        assign b04_tram_rdata_o = 'd0;
        assign b04_dram_rdata_o = 'd0;
        assign b04_uram_rdata_o = 'd0;
    end
    else begin : non_empty_l4_bank_gen
    for(r4_b=0; r4_b<BANK_L4_NUM; r4_b++) begin : raml4_gen
        for(r4_w=0; r4_w<BANK_L4_WAY_NUM; r4_w++) begin : wayl4_gen
            assign b04_tram_cs_mux[r4_b][r4_w]    = ram_initial_done_ff ? b04_tram_cs_i[r4_b][r4_w]    : 'd0;
            assign b04_tram_wr_mux[r4_b][r4_w]    = ram_initial_done_ff ? b04_tram_wr_i[r4_b][r4_w]    : 'd0;
            assign b04_tram_addr_mux[r4_b][r4_w]  = ram_initial_done_ff ? b04_tram_addr_i[r4_b][r4_w]  : ram_initial_scnt[BANK_L4_SET_IDX_WIDTH-1:0];
            assign b04_tram_wdata_mux[r4_b][r4_w] = ram_initial_done_ff ? b04_tram_wdata_i[r4_b][r4_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (TRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L4_SET_IDX_WIDTH          )  // = 6
            ) U_tram(
                            .CLK                    (clk                            ),
                            .D                      (b04_tram_wdata_mux[r4_b][r4_w] ),
                            .Q                      (b04_tram_rdata_o[r4_b][r4_w] ),
                            .CEN                    (b04_tram_cs_mux[r4_b][r4_w]    ),
                            .WEN                    (b04_tram_wr_mux[r4_b][r4_w]    ),
                            .A                      (b04_tram_addr_mux[r4_b][r4_w]  ) 
            );

            assign b04_iram_cs_mux[r4_b][r4_w]    = ram_initial_done_ff ? b04_iram_cs_i[r4_b][r4_w]    : 'd0;
            assign b04_iram_wr_mux[r4_b][r4_w]    = ram_initial_done_ff ? b04_iram_wr_i[r4_b][r4_w]    : 'd0;
            assign b04_iram_addr_mux[r4_b][r4_w]  = ram_initial_done_ff ? b04_iram_addr_i[r4_b][r4_w]  : ram_initial_scnt[BANK_L4_SET_IDX_WIDTH-1:0];
            assign b04_iram_wdata_mux[r4_b][r4_w] = ram_initial_done_ff ? b04_iram_wdata_i[r4_b][r4_w] : 'd0;
            //iommu_atd_cache_ram_wrap #(
            ///*parameter */  .RAM_WIDTH              ($bits(MTLB_ITAG_TYPE)          ), // = 64,
            ///*parameter */  .ADDR_WIDTH             (BANK_L4_SET_IDX_WIDTH          )  // = 6
            //) U_iram(
            //                .CLK                    (clk                            ),
            //                .D                      (b04_iram_wdata_mux[r4_b][r4_w] ),
            //                .Q                      (b04_iram_rdata_o[r4_b][r4_w] ),
            //                .CEN                    (b04_iram_cs_mux[r4_b][r4_w]    ),
            //                .WEN                    (b04_iram_wr_mux[r4_b][r4_w]    ),
            //                .A                      (b04_iram_addr_mux[r4_b][r4_w]  ) 
            //);
            assign b04_iram_rdata_o[r4_b][r4_w] = 'd0;

            assign b04_dram_cs_mux[r4_b][r4_w]    = ram_initial_done_ff ? b04_dram_cs_i[r4_b][r4_w]    : 'd0;
            assign b04_dram_wr_mux[r4_b][r4_w]    = ram_initial_done_ff ? b04_dram_wr_i[r4_b][r4_w]    : 'd0;
            assign b04_dram_addr_mux[r4_b][r4_w]  = ram_initial_done_ff ? b04_dram_addr_i[r4_b][r4_w]  : ram_initial_scnt[BANK_L4_SET_IDX_WIDTH-1:0];
            assign b04_dram_wdata_mux[r4_b][r4_w] = ram_initial_done_ff ? b04_dram_wdata_i[r4_b][r4_w] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              (NLDRAM_DAT_WIDTH           ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L4_SET_IDX_WIDTH          )  // = 6
            ) U_dram(
                            .CLK                    (clk                            ),
                            .D                      (b04_dram_wdata_mux[r4_b][r4_w] ),
                            .Q                      (b04_dram_rdata_o[r4_b][r4_w] ),
                            .CEN                    (b04_dram_cs_mux[r4_b][r4_w]    ),
                            .WEN                    (b04_dram_wr_mux[r4_b][r4_w]    ),
                            .A                      (b04_dram_addr_mux[r4_b][r4_w]  ) 
            );
        end
            assign b04_uram_cs_mux[r4_b]    = ram_initial_done_ff ? b04_uram_cs_i[r4_b]    : 'd0;
            assign b04_uram_wr_mux[r4_b]    = ram_initial_done_ff ? b04_uram_wr_i[r4_b]    : 'd0;
            assign b04_uram_addr_mux[r4_b]  = ram_initial_done_ff ? b04_uram_addr_i[r4_b]  : ram_initial_scnt[BANK_L4_SET_IDX_WIDTH-1:0];
            assign b04_uram_wdata_mux[r4_b] = ram_initial_done_ff ? b04_uram_wdata_i[r4_b] : 'd0;
            iommu_atd_cache_ram_wrap #(
            /*parameter */  .RAM_WIDTH              ($bits(MTLB_UTAG_TYPE)          ), // = 64,
            /*parameter */  .ADDR_WIDTH             (BANK_L4_SET_IDX_WIDTH          )  // = 6
            ) U_uram(
                            .CLK                    (clk                            ),
                            .D                      (b04_uram_wdata_mux[r4_b]        ),
                            .Q                      (b04_uram_rdata_o[r4_b]        ),
                            .CEN                    (b04_uram_cs_mux[r4_b]           ),
                            .WEN                    (b04_uram_wr_mux[r4_b]           ),
                            .A                      (b04_uram_addr_mux[r4_b]         ) 
            );
    end
    end
endgenerate
//}}}

//}}}

endmodule //}}}
