////////////////////////////////////////////////////////////////////////////////////////
// iommu_atd_dtc_main_tlb
////////////////////////////////////////////////////////////////////////////////////////
module iommu_atd_dtc_main_tlb #( //{{{
    parameter  CACHE_TYPE                                                  = 0, // 0:DDTC, 1:PDTC
    parameter type          INVALID_REQ_TYPE                                            = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                                             = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                                             = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                                             = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter               CABIN_LKP_IDX_WIDTH                                         = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter               CABIN_UPD_IDX_WIDTH                                         = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_INV_IDX_WIDTH                                         = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter               BANK_L0_IDX_WIDTH                                           = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    parameter               BANK_L1_IDX_WIDTH                                           = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    parameter               BANK_L2_IDX_WIDTH                                           = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    parameter               BANK_L0_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter               BANK_L1_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter               BANK_L2_SET_IDX_WIDTH                                       = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter               BANK_L0_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    parameter               BANK_L1_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    parameter               BANK_L2_WAY_IDX_WIDTH                                       = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    parameter               CABIN_LKP_NUM                                               = 2**CABIN_LKP_IDX_WIDTH,
    parameter               CABIN_UPD_NUM                                               = 2**CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_INV_NUM                                               = 2**CABIN_INV_IDX_WIDTH,
    parameter               BANK_L0_NUM                                                 = 2**BANK_L0_IDX_WIDTH,
    parameter               BANK_L1_NUM                                                 = 2**BANK_L1_IDX_WIDTH,
    parameter               BANK_L2_NUM                                                 = 2**BANK_L2_IDX_WIDTH,
    parameter               BANK_L0_SET_NUM                                             = 2**BANK_L0_SET_IDX_WIDTH,
    parameter               BANK_L1_SET_NUM                                             = 2**BANK_L1_SET_IDX_WIDTH,
    parameter               BANK_L2_SET_NUM                                             = 2**BANK_L2_SET_IDX_WIDTH,
    parameter               BANK_L0_WAY_NUM                                             = 2**BANK_L0_WAY_IDX_WIDTH,
    parameter               BANK_L1_WAY_NUM                                             = 2**BANK_L1_WAY_IDX_WIDTH,
    parameter               BANK_L2_WAY_NUM                                             = 2**BANK_L2_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                                               = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                                              = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                                              = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                                               = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    parameter type          MTLB_DAT_TYPE_NL                                            = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl,
    parameter               TECC_WIDTH                                                  = 5,
    parameter               LDECC_WIDTH                                                 = 9,
    parameter               TRAM_DAT_WIDTH                                              = $bits(MTLB_TAG_TYPE)+TECC_WIDTH+1,
    parameter               IRAM_DAT_WIDTH                                              = $bits(MTLB_ITAG_TYPE)+TECC_WIDTH+1,
    parameter               LDRAM_DAT_WIDTH                                             = $bits(MTLB_DAT_TYPE)+LDECC_WIDTH+1,
    parameter               NLDECC_WIDTH                                                = 6,
    parameter               NLDRAM_DAT_WIDTH                                            = $bits(MTLB_DAT_TYPE_NL)+NLDECC_WIDTH+1,
    parameter               SPARE_PARAM                                                 = 0
)(
//=== IO {{{
    input  logic                                                                        clk,
    input  logic                                                                        rstn,
    // LOOKUP INPUT                                                                     
    input  logic                                                                        lookup_req_valid_i,
    output logic                                                                        lookup_req_ready_o,
    input  LOOKUP_REQ_TYPE                                                              lookup_req_i,
    // UPDATE INPUT                                                                     
    input  logic                                                                        update_req_valid_i,
    output logic                                                                        update_req_ready_o,
    input  UPDATE_REQ_TYPE                                                              update_req_i,
    // REFILL INPUT                                                                     
    input  logic                                                                        refill_req_valid_i,
    output logic                                                                        refill_req_ready_o,
    input  UPDATE_REQ_TYPE                                                              refill_req_i,
    // INV INPUT                                                                        
    input  logic                                                                        invalid_req_valid_i,
    output logic                                                                        invalid_req_ready_o,
    input  INVALID_REQ_TYPE                                                             invalid_req_i,
    // LOOKUP ACK                                                                       
    output logic                                                                        lookup_ack_valid_o,
    input  logic                                                                        lookup_ack_ready_i,
    output UPDATE_REQ_TYPE                                                              lookup_ack_o,
    output logic                                                                        lookup_ack_hit_o,
    // UPDATE ACK                                                                       
    output logic                                                                        update_ack_valid_o,
    output UPDATE_REQ_TYPE                                                              update_ack_o,       // only idx used by now, other constant 0
    // INV ACK                                                                          
    output logic                                                                        invalid_ack_valid_o,
    output INVALID_REQ_TYPE                                                             invalid_ack_o,          // only idx used by now, other constant 0
    // INV REQ TO microTLB                                                              
    output logic                                                                        invalid_req_valid_o,
    input  logic                                                                        invalid_req_ready_i,
    output INVALID_REQ_TYPE                                                             invalid_req_o,
    // RAM
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_tram_cs_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_tram_wr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]    b00_tram_addr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b00_tram_wdata_o,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b00_tram_rdata_i,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_iram_cs_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_iram_wr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]    b00_iram_addr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b00_iram_wdata_o,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b00_iram_rdata_i,
    output logic [BANK_L0_NUM-1:0]                                                      b00_uram_cs_o,
    output logic [BANK_L0_NUM-1:0]                                                      b00_uram_wr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]                          b00_uram_addr_o,
    output MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                             b00_uram_wdata_o,
    input  MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                             b00_uram_rdata_i,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_dram_cs_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                                b00_dram_wr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]    b00_dram_addr_o,
    output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]          b00_dram_wdata_o,
    input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]          b00_dram_rdata_i,

    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_tram_cs_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_tram_wr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]    b01_tram_addr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b01_tram_wdata_o,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b01_tram_rdata_i,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_iram_cs_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_iram_wr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]    b01_iram_addr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b01_iram_wdata_o,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b01_iram_rdata_i,
    output logic [BANK_L1_NUM-1:0]                                                      b01_uram_cs_o,
    output logic [BANK_L1_NUM-1:0]                                                      b01_uram_wr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]                          b01_uram_addr_o,
    output MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                             b01_uram_wdata_o,
    input  MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                             b01_uram_rdata_i,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_dram_cs_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                                b01_dram_wr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]    b01_dram_addr_o,
    output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b01_dram_wdata_o,
    input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b01_dram_rdata_i,

    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_tram_cs_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_tram_wr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]    b10_tram_addr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b10_tram_wdata_o,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]           b10_tram_rdata_i,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_iram_cs_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_iram_wr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]    b10_iram_addr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b10_iram_wdata_o,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]           b10_iram_rdata_i,
    output logic [BANK_L2_NUM-1:0]                                                      b10_uram_cs_o,
    output logic [BANK_L2_NUM-1:0]                                                      b10_uram_wr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]                          b10_uram_addr_o,
    output MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                             b10_uram_wdata_o,
    input  MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                             b10_uram_rdata_i,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_dram_cs_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                                b10_dram_wr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]    b10_dram_addr_o,
    output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b10_dram_wdata_o,
    input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]         b10_dram_rdata_i,
    // CFG                                                                              
    input  logic                                                                        multi_hit_check_i,
    output logic [3:0]                                                                  multi_hit_fault_o,
    output logic [1:0]                                                                  ecc_err_o,
    //                                                                                  
    input  logic                                                                        spare_in
//}}}
);
//=== Declare === {{{
    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_L0_IDX_WIDTH > BANK_L1_IDX_WIDTH) ? BANK_L0_IDX_WIDTH : BANK_L1_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = BANK_L2_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;
    localparam MAX_BANK_NUM         = 2**MAX_BANK_IDX_WIDTH;

    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_L0_SET_IDX_WIDTH > BANK_L1_SET_IDX_WIDTH) ? BANK_L0_SET_IDX_WIDTH : BANK_L1_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = BANK_L2_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;
    localparam MAX_BANK_SET_NUM     = 2**MAX_BANK_SET_IDX_WIDTH;

    // lkp_req to bank
    typedef struct packed {
        LOOKUP_REQ_TYPE                                 req;            // LOOKUP_REQ message
        logic [MAX_BANK_IDX_WIDTH-1:0]                  bank_idx;       // sub_bank idx
        logic [MAX_BANK_SET_IDX_WIDTH-1:0]              bank_set_idx;   // ram addr
    } bank_lkp_req_t;

    typedef struct packed {
        UPDATE_REQ_TYPE                                 ack;
        logic                                           hit;
    } bank_lkp_ack_t;

    typedef struct packed {
        logic [CABIN_LKP_NUM-1:0]                       valid;
        logic [CABIN_LKP_NUM-1:0]                       ready;
        bank_lkp_req_t  [CABIN_LKP_NUM-1:0]             req;
    } lkp2bank_req_grp_t;

    typedef struct packed {
        logic [CABIN_LKP_NUM-1:0]                       valid;
        bank_lkp_ack_t [CABIN_LKP_NUM-1:0]              ack;
    } bank2lkp_ack_grp_t;


    // upd_req to bank
    typedef struct packed {
        UPDATE_REQ_TYPE                                 req;            // LOOKUP_REQ message
        logic [MAX_BANK_IDX_WIDTH-1:0]                  bank_idx;       // sub_bank idx
        logic [MAX_BANK_SET_IDX_WIDTH-1:0]              bank_set_idx;   // ram addr
    } bank_upd_req_t;

    typedef struct packed {
        UPDATE_REQ_TYPE                                 ack;
    } bank_upd_ack_t;

    typedef struct packed {
        logic [CABIN_UPD_NUM-1:0]                       valid;
        logic [CABIN_UPD_NUM-1:0]                       ready;
        bank_upd_req_t  [CABIN_UPD_NUM-1:0]             req;
    } upd2bank_req_grp_t;

    typedef struct packed {
        logic [CABIN_UPD_NUM-1:0]                       valid;
        bank_upd_ack_t [CABIN_UPD_NUM-1:0]              ack;
    } bank2upd_ack_grp_t;


    // inv_req to bank
    typedef struct packed {
        INVALID_REQ_TYPE                                req;            // LOOKUP_REQ message
        logic                                           bank_idx_val;
        logic [MAX_BANK_IDX_WIDTH-1:0]                  bank_idx;       // sub_bank idx
        logic [MAX_BANK_SET_IDX_WIDTH-1:0]              bank_set_idx;   // ram addr
    } bank_inv_req_t;

    typedef struct packed {
        INVALID_REQ_TYPE                                ack;
    } bank_inv_ack_t;

    typedef struct packed {
        logic [CABIN_INV_NUM-1:0]                       valid;
        logic [CABIN_INV_NUM-1:0]                       ready;
        bank_inv_req_t  [CABIN_INV_NUM-1:0]             req;
    } inv2bank_req_grp_t;

    typedef struct packed {
        logic [CABIN_INV_NUM-1:0]                       valid;
        bank_inv_ack_t [CABIN_INV_NUM-1:0]              ack;
    } bank2inv_ack_grp_t;

    logic                                               lkp_lookup_req_valid_i  ;
    logic                                               lkp_lookup_req_ready_o  ;
    LOOKUP_REQ_TYPE                                     lkp_lookup_req_i        ;
    logic                                               lkp_lookup_ack_valid_o  ;
    logic                                               lkp_lookup_ack_ready_i  ;
    UPDATE_REQ_TYPE                                     lkp_lookup_ack_o        ;
    logic                                               lkp_lookup_ack_hit_o    ;
    logic [2:0]             [CABIN_LKP_NUM-1:0]         lkp_lkp2bank_req_valid_o;
    logic [2:0]             [CABIN_LKP_NUM-1:0]         lkp_lkp2bank_req_ready_i;
    bank_lkp_req_t    [2:0] [CABIN_LKP_NUM-1:0]         lkp_lkp2bank_req_o      ;
    logic [2:0]             [CABIN_LKP_NUM-1:0]         lkp_bank2lkp_ack_valid_i;
    bank_lkp_ack_t    [2:0] [CABIN_LKP_NUM-1:0]         lkp_bank2lkp_ack_i      ;
    logic [2:0]                                         lkp_bank_valid_i        ;

    lkp2bank_req_grp_t [2:0]                            lkp2bank_req;
    bank2lkp_ack_grp_t [2:0]                            bank2lkp_ack;

    logic                                               upd_update_req_valid_i  ;
    logic                                               upd_update_req_ready_o  ;
    UPDATE_REQ_TYPE                                     upd_update_req_i        ;
    logic                                               upd_update_ack_valid_o  ;
    logic                                               upd_update_ack_ready_i  ;
    UPDATE_REQ_TYPE                                     upd_update_ack_o        ;
    logic [2:0]             [CABIN_UPD_NUM-1:0]         upd_upd2bank_req_valid_o;
    logic [2:0]             [CABIN_UPD_NUM-1:0]         upd_upd2bank_req_ready_i;
    bank_upd_req_t    [2:0] [CABIN_UPD_NUM-1:0]         upd_upd2bank_req_o      ;
    logic [2:0]             [CABIN_UPD_NUM-1:0]         upd_bank2upd_ack_valid_i;
    bank_upd_ack_t    [2:0] [CABIN_UPD_NUM-1:0]         upd_bank2upd_ack_i      ;

    upd2bank_req_grp_t [2:0]                            upd2bank_req;
    bank2upd_ack_grp_t [2:0]                            bank2upd_ack;

    logic                                               inv_invalid_req_valid_i ;
    logic                                               inv_invalid_req_ready_o ;
    INVALID_REQ_TYPE                                    inv_invalid_req_i       ;
    logic                                               inv_invalid_ack_valid_o ;
    logic                                               inv_invalid_ack_ready_i ;
    INVALID_REQ_TYPE                                    inv_invalid_ack_o       ;
    logic             [2:0] [CABIN_INV_NUM-1:0]         inv_inv2bank_req_valid_o;
    logic             [2:0] [CABIN_INV_NUM-1:0]         inv_inv2bank_req_ready_i;
    bank_inv_req_t    [2:0] [CABIN_INV_NUM-1:0]         inv_inv2bank_req_o      ;
    logic             [2:0] [CABIN_INV_NUM-1:0]         inv_bank2inv_ack_valid_i;
    bank_inv_ack_t    [2:0] [CABIN_INV_NUM-1:0]         inv_bank2inv_ack_i      ;
    logic                                               inv_invalid_req_valid_o ;
    logic                                               inv_invalid_req_ready_i ;
    INVALID_REQ_TYPE                                    inv_invalid_req_o       ;


    inv2bank_req_grp_t [2:0]                            inv2bank_req;
    bank2inv_ack_grp_t [2:0]                            bank2inv_ack;

    logic [1:0]                                         upd_cabin_in_arb_valid_mux;
    logic [1:0]                                         upd_cabin_in_arb_ready_mux;
    UPDATE_REQ_TYPE [1:0]                               upd_cabin_in_arb_data_mux;

    logic [3-1:0] [1:0]                                 b_ecc_err_o;
    logic [1:0] [3-1:0]                                 ecc_err_o_int;

//}}}

//=== Main Code === {{{
    assign multi_hit_fault_o[3] = 'd0;

    assign lkp_bank_valid_i = 4'b1111;

    always@(*) begin
        for(int unsigned ei=0; ei<3; ei++) begin
            ecc_err_o_int[0][ei] = b_ecc_err_o[ei][0];
            ecc_err_o_int[1][ei] = b_ecc_err_o[ei][1];
        end
    end

    assign ecc_err_o[0] = |ecc_err_o_int[0];
    assign ecc_err_o[1] = |ecc_err_o_int[1];

//}}}

//=== LKP_CABIN_WRAP inst {{{
    assign lkp_lookup_req_valid_i   = lookup_req_valid_i    ;
    assign lookup_req_ready_o       = lkp_lookup_req_ready_o;
    assign lkp_lookup_req_i         = lookup_req_i          ;
    assign lookup_ack_valid_o       = lkp_lookup_ack_valid_o;
//    assign lkp_lookup_ack_ready_i   = 1'b1                  ;
    assign lkp_lookup_ack_ready_i   = lookup_ack_ready_i    ;
    assign lookup_ack_o             = lkp_lookup_ack_o      ;
    assign lookup_ack_hit_o         = lkp_lookup_ack_hit_o  ;
    iommu_atd_dtc_mtlb_lkp_cabin_wrap #(
    /*parameter type         */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE          (bank_lkp_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE          (bank_lkp_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter */              .CABIN_IDX_WIDTH            (CABIN_LKP_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    /*parameter */              .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    /*parameter */              .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    /*parameter */              .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    /*parameter */              .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter */              .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter */              .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter */              .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_lkp_cabin_wrap (                                                      
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .lookup_req_valid_i         (lkp_lookup_req_valid_i     ),
    /*output logic                                      */  .lookup_req_ready_o         (lkp_lookup_req_ready_o     ),
    /*input  LOOKUP_REQ_TYPE                            */  .lookup_req_i               (lkp_lookup_req_i           ),
    /*output logic                                      */  .lookup_ack_valid_o         (lkp_lookup_ack_valid_o     ),
    /*input  logic                                      */  .lookup_ack_ready_i         (lkp_lookup_ack_ready_i     ),
    /*output UPDATE_REQ_TYPE                            */  .lookup_ack_o               (lkp_lookup_ack_o           ),
    /*output logic                                      */  .lookup_ack_hit_o           (lkp_lookup_ack_hit_o       ),
    /*output logic [2:0]             [CABIN_NUM-1:0]    */  .lkp2bank_req_valid_o       (lkp_lkp2bank_req_valid_o   ),
    /*input  logic [2:0]             [CABIN_NUM-1:0]    */  .lkp2bank_req_ready_i       (lkp_lkp2bank_req_ready_i   ),
    /*output BANK_LKP_REQ_TYPE [2:0] [CABIN_NUM-1:0]    */  .lkp2bank_req_o             (lkp_lkp2bank_req_o         ),
    /*input  logic [2:0]             [CABIN_NUM-1:0]    */  .bank2lkp_ack_valid_i       (lkp_bank2lkp_ack_valid_i   ),
    /*input  BANK_LKP_ACK_TYPE [2:0] [CABIN_NUM-1:0]    */  .bank2lkp_ack_i             (lkp_bank2lkp_ack_i         ),
    /*input  logic [2:0]                                */  .bank_valid_i               (lkp_bank_valid_i           ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );

genvar lkp2bank_b, lkp2bank_l;
generate
    for(lkp2bank_b=0; lkp2bank_b<=2; lkp2bank_b++) begin : lkp2bank_req_connect_b_gen
        assign lkp2bank_req[lkp2bank_b].valid       = lkp_lkp2bank_req_valid_o[lkp2bank_b];
        assign lkp_lkp2bank_req_ready_i[lkp2bank_b] = lkp2bank_req[lkp2bank_b].ready;
        assign lkp2bank_req[lkp2bank_b].req         = lkp_lkp2bank_req_o[lkp2bank_b];
    end
endgenerate

genvar bank2lkp_b, bank2lkp_l;
generate
    for(bank2lkp_b=0; bank2lkp_b<=2; bank2lkp_b++) begin : bank2lkp_req_connect_b_gen
        assign lkp_bank2lkp_ack_valid_i[bank2lkp_b] =  bank2lkp_ack[bank2lkp_b].valid;
        assign lkp_bank2lkp_ack_i[bank2lkp_b]       =  bank2lkp_ack[bank2lkp_b].ack;
    end
endgenerate

//}}}

//=== UPD_CABIN_WRAP inst {{{
    

//    assign upd_update_req_valid_i   = update_req_valid_i    ;
//    assign update_req_ready_o       = upd_update_req_ready_o;
//    assign upd_update_req_i         = update_req_i          ;
    assign upd_cabin_in_arb_valid_mux[1]= update_req_valid_i;
    assign update_req_ready_o           = upd_cabin_in_arb_ready_mux[1];
    assign upd_cabin_in_arb_data_mux[1] = update_req_i;

    assign upd_cabin_in_arb_valid_mux[0]= refill_req_valid_i;
    assign refill_req_ready_o           = upd_cabin_in_arb_ready_mux[0];
    assign upd_cabin_in_arb_data_mux[0] = refill_req_i;

    iommu_acd_bus_handler_trans_arb #(
    /*parameter */              .ARB_TYPE       (0                  ), // = 0,
    /*parameter */              .REQ_NUM        (2                  ), // = 2,
    /*parameter type         */ .DATA_TYPE      (UPDATE_REQ_TYPE    ), // = logic,
    /*parameter */              .AXIVLDRDY      (1                  )  // = 1
    ) U_upd_cabin_in_arb(                                          
    /*input  logic                          */  .clk                (clk                        ),
    /*input  logic                          */  .rstn               (rstn                       ),
    /*input  logic [REQ_NUM-1:0]            */  .req_i              (upd_cabin_in_arb_valid_mux ),
    /*input  logic [REQ_NUM-1:0]            */  .req_prior_i        (2'(0)                      ),
    /*input  DATA_TYPE [REQ_NUM-1:0]        */  .data_i             (upd_cabin_in_arb_data_mux  ),
    /*output logic [REQ_NUM-1:0]            */  .gnt_o              (upd_cabin_in_arb_ready_mux ),
    /*output logic                          */  .req_o              (upd_update_req_valid_i     ),
    /*output DATA_TYPE                      */  .data_o             (upd_update_req_i           ),
    /*input  logic                          */  .gnt_i              (upd_update_req_ready_o     ) 
    );


    assign update_ack_valid_o       = upd_update_ack_valid_o;
    assign upd_update_ack_ready_i   = 1'b1                  ;
    assign update_ack_o             = upd_update_ack_o      ;
    iommu_atd_dtc_mtlb_upd_cabin_wrap #(
    /*parameter type         */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE          (bank_upd_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE          (bank_upd_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter */              .CABIN_IDX_WIDTH            (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    /*parameter */              .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    /*parameter */              .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    /*parameter */              .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    /*parameter */              .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter */              .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter */              .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter */              .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_upd_cabin_wrap (                                                      
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .update_req_valid_i         (upd_update_req_valid_i     ),
    /*output logic                                      */  .update_req_ready_o         (upd_update_req_ready_o     ),
    /*input  UPDATE_REQ_TYPE                            */  .update_req_i               (upd_update_req_i           ),
    /*output logic                                      */  .update_ack_valid_o         (upd_update_ack_valid_o     ),
    /*input  logic                                      */  .update_ack_ready_i         (upd_update_ack_ready_i     ),
    /*output UPDATE_REQ_TYPE                            */  .update_ack_o               (upd_update_ack_o           ),
    /*output logic [2:0]             [CABIN_NUM-1:0]    */  .upd2bank_req_valid_o       (upd_upd2bank_req_valid_o   ),
    /*input  logic [2:0]             [CABIN_NUM-1:0]    */  .upd2bank_req_ready_i       (upd_upd2bank_req_ready_i   ),
    /*output BANK_UPD_REQ_TYPE [2:0] [CABIN_NUM-1:0]    */  .upd2bank_req_o             (upd_upd2bank_req_o         ),
    /*input  logic [2:0]             [CABIN_NUM-1:0]    */  .bank2upd_ack_valid_i       (upd_bank2upd_ack_valid_i   ),
    /*input  BANK_UPD_ACK_TYPE [2:0] [CABIN_NUM-1:0]    */  .bank2upd_ack_i             (upd_bank2upd_ack_i         ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );

genvar upd2bank_b, upd2bank_l;
generate
    for(upd2bank_b=0; upd2bank_b<=2; upd2bank_b++) begin : upd2bank_req_connect_b_gen
        assign upd2bank_req[upd2bank_b].valid       = upd_upd2bank_req_valid_o[upd2bank_b];
        assign upd_upd2bank_req_ready_i[upd2bank_b] = upd2bank_req[upd2bank_b].ready;
        assign upd2bank_req[upd2bank_b].req         = upd_upd2bank_req_o[upd2bank_b];
    end
endgenerate

genvar bank2upd_b, bank2upd_l;
generate
    for(bank2upd_b=0; bank2upd_b<=2; bank2upd_b++) begin : bank2upd_req_connect_b_gen
        assign upd_bank2upd_ack_valid_i[bank2upd_b] =  bank2upd_ack[bank2upd_b].valid;
        assign upd_bank2upd_ack_i[bank2upd_b]       =  bank2upd_ack[bank2upd_b].ack;
    end
endgenerate

//}}}

//=== INV_CABIN_WRAP inst {{{
    assign inv_invalid_req_valid_i  = invalid_req_valid_i    ;
    assign invalid_req_ready_o      = inv_invalid_req_ready_o;
    assign inv_invalid_req_i        = invalid_req_i          ;
    assign invalid_ack_valid_o      = inv_invalid_ack_valid_o;
    assign inv_invalid_ack_ready_i  = 1'b1                   ;
    assign invalid_ack_o            = inv_invalid_ack_o      ;
    assign invalid_req_valid_o      = inv_invalid_req_valid_o;
    assign inv_invalid_req_ready_i  = invalid_req_ready_i    ;
    assign invalid_req_o            = inv_invalid_req_o      ;

    iommu_atd_dtc_mtlb_inv_cabin_wrap #(
    /*parameter type         */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .BANK_INV_REQ_TYPE          (bank_inv_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE          (bank_inv_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */              .CABIN_IDX_WIDTH            (CABIN_INV_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    /*parameter */              .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    /*parameter */              .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    /*parameter */              .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    /*parameter */              .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter */              .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter */              .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter */              .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_inv_cabin_wrap(                                                      
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .invalid_req_valid_i        (inv_invalid_req_valid_i    ),
    /*output logic                                      */  .invalid_req_ready_o        (inv_invalid_req_ready_o    ),
    /*input  INVALID_REQ_TYPE                           */  .invalid_req_i              (inv_invalid_req_i          ),
    /*output logic                                      */  .invalid_ack_valid_o        (inv_invalid_ack_valid_o    ),
    /*input  logic                                      */  .invalid_ack_ready_i        (inv_invalid_ack_ready_i    ),
    /*output INVALID_REQ_TYPE                           */  .invalid_ack_o              (inv_invalid_ack_o          ),
    /*output logic             [2:0] [CABIN_NUM-1:0]    */  .inv2bank_req_valid_o       (inv_inv2bank_req_valid_o   ),
    /*input  logic             [2:0] [CABIN_NUM-1:0]    */  .inv2bank_req_ready_i       (inv_inv2bank_req_ready_i   ),
    /*output BANK_INV_REQ_TYPE [2:0] [CABIN_NUM-1:0]    */  .inv2bank_req_o             (inv_inv2bank_req_o         ),
    /*input  logic             [2:0] [CABIN_NUM-1:0]    */  .bank2inv_ack_valid_i       (inv_bank2inv_ack_valid_i   ),
    /*input  BANK_INV_ACK_TYPE [2:0] [CABIN_NUM-1:0]    */  .bank2inv_ack_i             (inv_bank2inv_ack_i         ),
    /*output logic                                      */  .invalid_req_valid_o        (inv_invalid_req_valid_o    ),
    /*input  logic                                      */  .invalid_req_ready_i        (inv_invalid_req_ready_i    ),
    /*output INVALID_REQ_TYPE                           */  .invalid_req_o              (inv_invalid_req_o          ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );

genvar inv2bank_b, inv2bank_l;
generate
    for(inv2bank_b=0; inv2bank_b<=2; inv2bank_b++) begin : inv2bank_req_connect_b_gen
        assign inv2bank_req[inv2bank_b].valid       = inv_inv2bank_req_valid_o[inv2bank_b];
        assign inv_inv2bank_req_ready_i[inv2bank_b] = inv2bank_req[inv2bank_b].ready;
        assign inv2bank_req[inv2bank_b].req         = inv_inv2bank_req_o[inv2bank_b];
    end
endgenerate

genvar bank2inv_b, bank2inv_l;
generate
    for(bank2inv_b=0; bank2inv_b<=2; bank2inv_b++) begin : bank2inv_req_connect_b_gen
        assign inv_bank2inv_ack_valid_i[bank2inv_b] =  bank2inv_ack[bank2inv_b].valid;
        assign inv_bank2inv_ack_i[bank2inv_b]       =  bank2inv_ack[bank2inv_b].ack;
    end
endgenerate

//}}}

//=== BANK inst {{{
    iommu_atd_dtc_mtlb_bank_top #(
    /*parameter type         */ .CACHE_TYPE                                         (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter */ .BANK_TYPE                                          (2'b00                      ), // = 2'b00,
    /*parameter type         */ .INVALID_REQ_TYPE                                   (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                                    (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                                    (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                                    (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                                  (bank_lkp_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                                  (bank_lkp_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                                  (bank_upd_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                                  (bank_upd_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                                  (bank_inv_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                                  (bank_inv_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */              .CABIN_LKP_IDX_WIDTH                                (CABIN_LKP_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    /*parameter */              .CABIN_UPD_IDX_WIDTH                                (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    /*parameter */              .CABIN_INV_IDX_WIDTH                                (CABIN_INV_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    /*parameter */              .BANK_IDX_WIDTH                                     (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    /*parameter */              .BANK_SET_IDX_WIDTH                                 (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    /*parameter */              .BANK_WAY_IDX_WIDTH                                 (BANK_L0_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE                                      (MTLB_TAG_TYPE              ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                                     (MTLB_ITAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                                     (MTLB_UTAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                                      (MTLB_DAT_TYPE              ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    /*parameter */              .TECC_WIDTH                                         (TECC_WIDTH                 ), // = 5,
    /*parameter */              .DECC_WIDTH                                         (LDECC_WIDTH                ), // = 9,
    /*parameter */              .SPARE_PARAM                                        (1'b0                       )  // = 0
    ) U_mtlb_bank_l0(
    /*input  logic                                                              */  .clk                        (clk                        ),
    /*input  logic                                                              */  .rstn                       (rstn                       ),
    /*input  logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_req_valid_i            (lkp2bank_req[0].valid      ),
    /*output logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_req_ready_o            (lkp2bank_req[0].ready      ),
    /*input  BANK_LKP_REQ_TYPE [CABIN_LKP_NUM-1:0]                              */  .lkp_req_i                  (lkp2bank_req[0].req        ),
    /*input  logic [CABIN_UPD_NUM-1:0]                                          */  .upd_req_valid_i            (upd2bank_req[0].valid      ),
    /*output logic [CABIN_UPD_NUM-1:0]                                          */  .upd_req_ready_o            (upd2bank_req[0].ready      ),
    /*input  BANK_UPD_REQ_TYPE [CABIN_UPD_NUM-1:0]                              */  .upd_req_i                  (upd2bank_req[0].req        ),
    /*input  logic [CABIN_INV_NUM-1:0]                                          */  .inv_req_valid_i            (inv2bank_req[0].valid      ),
    /*output logic [CABIN_INV_NUM-1:0]                                          */  .inv_req_ready_o            (inv2bank_req[0].ready      ),
    /*input  BANK_INV_REQ_TYPE [CABIN_INV_NUM-1:0]                              */  .inv_req_i                  (inv2bank_req[0].req        ),
    /*output logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_ack_valid_o            (bank2lkp_ack[0].valid      ),
    /*output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]                              */  .lkp_ack_o                  (bank2lkp_ack[0].ack        ),
    /*output logic [CABIN_UPD_NUM-1:0]                                          */  .upd_ack_valid_o            (bank2upd_ack[0].valid      ),
    /*output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]                              */  .upd_ack_o                  (bank2upd_ack[0].ack        ),
    /*output logic [CABIN_INV_NUM-1:0]                                          */  .inv_ack_valid_o            (bank2inv_ack[0].valid      ),
    /*output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]                              */  .inv_ack_o                  (bank2inv_ack[0].ack        ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .tram_cs_o                  (b00_tram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .tram_wr_o                  (b00_tram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .tram_addr_o                (b00_tram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]       */  .tram_wdata_o               (b00_tram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]       */  .tram_rdata_i               (b00_tram_rdata_i           ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .iram_cs_o                  (b00_iram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .iram_wr_o                  (b00_iram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .iram_addr_o                (b00_iram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]       */  .iram_wdata_o               (b00_iram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]       */  .iram_rdata_i               (b00_iram_rdata_i           ),
    /*output logic [BANK_NUM-1:0]                                               */  .uram_cs_o                  (b00_uram_cs_o              ),
    /*output logic [BANK_NUM-1:0]                                               */  .uram_wr_o                  (b00_uram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]                      */  .uram_addr_o                (b00_uram_addr_o            ),
    /*output MTLB_UTAG_TYPE [BANK_NUM-1:0]                                      */  .uram_wdata_o               (b00_uram_wdata_o           ),
    /*input  MTLB_UTAG_TYPE [BANK_NUM-1:0]                                      */  .uram_rdata_i               (b00_uram_rdata_i           ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .dram_cs_o                  (b00_dram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .dram_wr_o                  (b00_dram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .dram_addr_o                (b00_dram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]       */  .dram_wdata_o               (b00_dram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]       */  .dram_rdata_i               (b00_dram_rdata_i           ),
    /*input  logic                                                              */  .multi_hit_check_i          (multi_hit_check_i          ),
    /*output logic                                                              */  .multi_hit_fault_o          (multi_hit_fault_o[0]       ),
    /*output logic [1:0]                                                        */  .ecc_err_o                  (b_ecc_err_o[0]             ),
    /*input  logic                                                              */  .spare_in                   (1'b0                       ) 
    );

    iommu_atd_dtc_mtlb_bank_top #(
    /*parameter type         */ .CACHE_TYPE                                         (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter */              .BANK_TYPE                                          (2'b01                      ), // = 2'b00,
    /*parameter type         */ .INVALID_REQ_TYPE                                   (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                                    (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                                    (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                                    (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                                  (bank_lkp_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                                  (bank_lkp_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                                  (bank_upd_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                                  (bank_upd_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                                  (bank_inv_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                                  (bank_inv_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */              .CABIN_LKP_IDX_WIDTH                                (CABIN_LKP_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    /*parameter */              .CABIN_UPD_IDX_WIDTH                                (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    /*parameter */              .CABIN_INV_IDX_WIDTH                                (CABIN_INV_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    /*parameter */              .BANK_IDX_WIDTH                                     (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    /*parameter */              .BANK_SET_IDX_WIDTH                                 (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    /*parameter */              .BANK_WAY_IDX_WIDTH                                 (BANK_L1_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE                                      (MTLB_TAG_TYPE              ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                                     (MTLB_ITAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                                     (MTLB_UTAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                                      (MTLB_DAT_TYPE_NL           ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl,
    /*parameter */              .TECC_WIDTH                                         (TECC_WIDTH                 ), // = 5,
    /*parameter */              .DECC_WIDTH                                         (NLDECC_WIDTH               ), // = 9,
    /*parameter */              .SPARE_PARAM                                        (1'b0                       )  // = 0
    ) U_mtlb_bank_l1(
    /*input  logic                                                              */  .clk                        (clk                        ),
    /*input  logic                                                              */  .rstn                       (rstn                       ),
    /*input  logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_req_valid_i            (lkp2bank_req[1].valid      ),
    /*output logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_req_ready_o            (lkp2bank_req[1].ready      ),
    /*input  BANK_LKP_REQ_TYPE [CABIN_LKP_NUM-1:0]                              */  .lkp_req_i                  (lkp2bank_req[1].req        ),
    /*input  logic [CABIN_UPD_NUM-1:0]                                          */  .upd_req_valid_i            (upd2bank_req[1].valid      ),
    /*output logic [CABIN_UPD_NUM-1:0]                                          */  .upd_req_ready_o            (upd2bank_req[1].ready      ),
    /*input  BANK_UPD_REQ_TYPE [CABIN_UPD_NUM-1:0]                              */  .upd_req_i                  (upd2bank_req[1].req        ),
    /*input  logic [CABIN_INV_NUM-1:0]                                          */  .inv_req_valid_i            (inv2bank_req[1].valid      ),
    /*output logic [CABIN_INV_NUM-1:0]                                          */  .inv_req_ready_o            (inv2bank_req[1].ready      ),
    /*input  BANK_INV_REQ_TYPE [CABIN_INV_NUM-1:0]                              */  .inv_req_i                  (inv2bank_req[1].req        ),
    /*output logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_ack_valid_o            (bank2lkp_ack[1].valid      ),
    /*output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]                              */  .lkp_ack_o                  (bank2lkp_ack[1].ack        ),
    /*output logic [CABIN_UPD_NUM-1:0]                                          */  .upd_ack_valid_o            (bank2upd_ack[1].valid      ),
    /*output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]                              */  .upd_ack_o                  (bank2upd_ack[1].ack        ),
    /*output logic [CABIN_INV_NUM-1:0]                                          */  .inv_ack_valid_o            (bank2inv_ack[1].valid      ),
    /*output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]                              */  .inv_ack_o                  (bank2inv_ack[1].ack        ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .tram_cs_o                  (b01_tram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .tram_wr_o                  (b01_tram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .tram_addr_o                (b01_tram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]       */  .tram_wdata_o               (b01_tram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]       */  .tram_rdata_i               (b01_tram_rdata_i           ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .iram_cs_o                  (b01_iram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .iram_wr_o                  (b01_iram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .iram_addr_o                (b01_iram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]       */  .iram_wdata_o               (b01_iram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]       */  .iram_rdata_i               (b01_iram_rdata_i           ),
    /*output logic [BANK_NUM-1:0]                                               */  .uram_cs_o                  (b01_uram_cs_o              ),
    /*output logic [BANK_NUM-1:0]                                               */  .uram_wr_o                  (b01_uram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]                      */  .uram_addr_o                (b01_uram_addr_o            ),
    /*output MTLB_UTAG_TYPE [BANK_NUM-1:0]                                      */  .uram_wdata_o               (b01_uram_wdata_o           ),
    /*input  MTLB_UTAG_TYPE [BANK_NUM-1:0]                                      */  .uram_rdata_i               (b01_uram_rdata_i           ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .dram_cs_o                  (b01_dram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .dram_wr_o                  (b01_dram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .dram_addr_o                (b01_dram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]       */  .dram_wdata_o               (b01_dram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]       */  .dram_rdata_i               (b01_dram_rdata_i           ),
    /*input  logic                                                              */  .multi_hit_check_i          (multi_hit_check_i          ),
    /*output logic                                                              */  .multi_hit_fault_o          (multi_hit_fault_o[1]       ),
    /*output logic [1:0]                                                        */  .ecc_err_o                  (b_ecc_err_o[1]             ),
    /*input  logic                                                              */  .spare_in                   (1'b0                       ) 
    );

    iommu_atd_dtc_mtlb_bank_top #(
    /*parameter type         */ .CACHE_TYPE                                         (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter */              .BANK_TYPE                                          (2'b10                      ), // = 2'b00,
    /*parameter type         */ .INVALID_REQ_TYPE                                   (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                                    (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                                    (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                                    (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                                  (bank_lkp_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                                  (bank_lkp_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                                  (bank_upd_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                                  (bank_upd_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                                  (bank_inv_req_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                                  (bank_inv_ack_t             ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */              .CABIN_LKP_IDX_WIDTH                                (CABIN_LKP_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    /*parameter */              .CABIN_UPD_IDX_WIDTH                                (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    /*parameter */              .CABIN_INV_IDX_WIDTH                                (CABIN_INV_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    /*parameter */              .BANK_IDX_WIDTH                                     (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    /*parameter */              .BANK_SET_IDX_WIDTH                                 (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    /*parameter */              .BANK_WAY_IDX_WIDTH                                 (BANK_L2_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE                                      (MTLB_TAG_TYPE              ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                                     (MTLB_ITAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                                     (MTLB_UTAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                                      (MTLB_DAT_TYPE_NL           ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl,
    /*parameter */              .TECC_WIDTH                                         (TECC_WIDTH                 ), // = 5,
    /*parameter */              .DECC_WIDTH                                         (NLDECC_WIDTH               ), // = 9,
    /*parameter */              .SPARE_PARAM                                        (1'b0                       )  // = 0
    ) U_mtlb_bank_l2(
    /*input  logic                                                              */  .clk                        (clk                        ),
    /*input  logic                                                              */  .rstn                       (rstn                       ),
    /*input  logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_req_valid_i            (lkp2bank_req[2].valid      ),
    /*output logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_req_ready_o            (lkp2bank_req[2].ready      ),
    /*input  BANK_LKP_REQ_TYPE [CABIN_LKP_NUM-1:0]                              */  .lkp_req_i                  (lkp2bank_req[2].req        ),
    /*input  logic [CABIN_UPD_NUM-1:0]                                          */  .upd_req_valid_i            (upd2bank_req[2].valid      ),
    /*output logic [CABIN_UPD_NUM-1:0]                                          */  .upd_req_ready_o            (upd2bank_req[2].ready      ),
    /*input  BANK_UPD_REQ_TYPE [CABIN_UPD_NUM-1:0]                              */  .upd_req_i                  (upd2bank_req[2].req        ),
    /*input  logic [CABIN_INV_NUM-1:0]                                          */  .inv_req_valid_i            (inv2bank_req[2].valid      ),
    /*output logic [CABIN_INV_NUM-1:0]                                          */  .inv_req_ready_o            (inv2bank_req[2].ready      ),
    /*input  BANK_INV_REQ_TYPE [CABIN_INV_NUM-1:0]                              */  .inv_req_i                  (inv2bank_req[2].req        ),
    /*output logic [CABIN_LKP_NUM-1:0]                                          */  .lkp_ack_valid_o            (bank2lkp_ack[2].valid      ),
    /*output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]                              */  .lkp_ack_o                  (bank2lkp_ack[2].ack        ),
    /*output logic [CABIN_UPD_NUM-1:0]                                          */  .upd_ack_valid_o            (bank2upd_ack[2].valid      ),
    /*output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]                              */  .upd_ack_o                  (bank2upd_ack[2].ack        ),
    /*output logic [CABIN_INV_NUM-1:0]                                          */  .inv_ack_valid_o            (bank2inv_ack[2].valid      ),
    /*output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]                              */  .inv_ack_o                  (bank2inv_ack[2].ack        ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .tram_cs_o                  (b10_tram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .tram_wr_o                  (b10_tram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .tram_addr_o                (b10_tram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]       */  .tram_wdata_o               (b10_tram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]       */  .tram_rdata_i               (b10_tram_rdata_i           ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .iram_cs_o                  (b10_iram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .iram_wr_o                  (b10_iram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .iram_addr_o                (b10_iram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]       */  .iram_wdata_o               (b10_iram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]       */  .iram_rdata_i               (b10_iram_rdata_i           ),
    /*output logic [BANK_NUM-1:0]                                               */  .uram_cs_o                  (b10_uram_cs_o              ),
    /*output logic [BANK_NUM-1:0]                                               */  .uram_wr_o                  (b10_uram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]                      */  .uram_addr_o                (b10_uram_addr_o            ),
    /*output MTLB_UTAG_TYPE [BANK_NUM-1:0]                                      */  .uram_wdata_o               (b10_uram_wdata_o           ),
    /*input  MTLB_UTAG_TYPE [BANK_NUM-1:0]                                      */  .uram_rdata_i               (b10_uram_rdata_i           ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .dram_cs_o                  (b10_dram_cs_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            */  .dram_wr_o                  (b10_dram_wr_o              ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   */  .dram_addr_o                (b10_dram_addr_o            ),
    /*output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]       */  .dram_wdata_o               (b10_dram_wdata_o           ),
    /*input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]       */  .dram_rdata_i               (b10_dram_rdata_i           ),
    /*input  logic                                                              */  .multi_hit_check_i          (multi_hit_check_i          ),
    /*output logic                                                              */  .multi_hit_fault_o          (multi_hit_fault_o[2]       ),
    /*output logic [1:0]                                                        */  .ecc_err_o                  (b_ecc_err_o[2]             ),
    /*input  logic                                                              */  .spare_in                   (1'b0                       ) 
    );

//}}}

endmodule
//}}}



