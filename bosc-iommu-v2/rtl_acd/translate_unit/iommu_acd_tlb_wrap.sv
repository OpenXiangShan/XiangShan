/////////////////////////////////////////////////////////////
// iommu_acd_tlb_wrap
/////////////////////////////////////////////////////////////
module iommu_acd_tlb_wrap #(
//{{{ PARAM
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter   INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter   MICRO_TLB_IDX_WIDTH         = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    parameter   CABIN_LKP_IDX_WIDTH         = iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    parameter   CABIN_UPD_IDX_WIDTH         = iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    parameter   CABIN_INV_IDX_WIDTH         = iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
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
    parameter   CABIN_LKP_NUM               = 2**CABIN_LKP_IDX_WIDTH,
    parameter   CABIN_UPD_NUM               = 2**CABIN_UPD_IDX_WIDTH,
    parameter   CABIN_INV_NUM               = 2**CABIN_INV_IDX_WIDTH,
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
    parameter   ECC_ENABLE                  = 1,
    parameter   SPARE_PARAM                 = 0 
//}}}
)(
//IO {{{
    input  logic                                        clk,
    input  logic                                        rstn,
    // LOOKUP                                           
    input  logic                                        lookup_req_valid_i,
    output logic                                        lookup_req_ready_o,
    input  LOOKUP_REQ_TYPE                              lookup_req_i,
    output logic                                        lookup_ack_valid_o,
    output LOOKUP_ACK_TYPE                              lookup_ack_o,
    // UPDATE                                           
    input  logic                                        update_req_valid_i,
    output logic                                        update_req_ready_o,
    input  UPDATE_REQ_TYPE                              update_req_i,
    output logic                                        update_ack_valid_o,
    output UPDATE_REQ_TYPE                              update_ack_o,
    // INV                                              
    input  logic                                        invalid_req_valid_i,
    output logic                                        invalid_req_ready_o,
    input  INVALID_REQ_TYPE                             invalid_req_i,
    output logic                                        invalid_ack_valid_o,
    output INVALID_REQ_TYPE                             invalid_ack_o,
    //
    output logic                                        ram_initial_done_o,
    // CFG                                              
    input  logic                                        csr_fctl_gxl_i,
    input  logic                                        multi_hit_check_i,
    output logic                                        multi_hit_fault_o,
    output logic [1:0]                                  ecc_err_o,
//    // HPM
//    input  logic [3:0]                                  hpm_cnt_inhibit_i,
//    output logic [63:0]                                 hpm_cnt_o[3:0],
`ifdef IOMMU_IDBG
    // IDBG
    input  iommu_acd_pkg::IDBG_TYPE_M                   idbg_intf_m_i[1:0],
    output iommu_acd_pkg::IDBG_TYPE_S                   idbg_intf_s_o[1:0],
`endif
    // INTERNAL INV                                     
    input  logic                                        tlbq_invalid_req_valid_i,
    output logic                                        tlbq_invalid_req_ready_o,
    input  INVALID_REQ_TYPE                             tlbq_invalid_req_i,
    output logic                                        tlbq_invalid_ack_valid_o,
    output INVALID_REQ_TYPE                             tlbq_invalid_ack_o,
    //                                                  
    input  logic                                        spare_in             
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


    typedef struct packed {                             
        logic                                           valid;
        logic                                           N;
        logic                                           SXL;
        logic [3:0]                                     S1MODE;
        logic [3:0]                                     S2MODE;
        logic [1:0]                                     S1SIZE;
        logic [1:0]                                     S2SIZE;
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } mtlb_tag_t;                                       

    typedef struct packed {
        logic [61:12]                                   GPPN;
        logic [19:0]                                    PSCID;
        logic [15:0]                                    GSCID;
        logic                                           G;
    } mtlb_itag_t;

    typedef struct packed {
        logic [MAX_BANK_WAY_NUM-2:0]                    plru_list;
    } mtlb_utag_t;

    typedef struct packed {                             
        logic [1:0]                                     PBMT;
        logic                                           ENATS;
        logic                                           T2GPA;
        logic                                           DTF;
        logic                                           PDTV;
        logic                                           DPE;
        logic                                           ENS;
        logic                                           SUM;
        logic                                           S1_D;
        logic                                           S2_D;
        logic                                           SADE;
        logic                                           GADE;
//        logic                                           N;
//        logic [15:0]                                    S1_PERM_D;
//        logic [15:0]                                    S1_PERM_A;
        logic [4:0]                                     S1_PERM;
//        logic [15:0]                                    S2_PERM_D;
//        logic [15:0]                                    S2_PERM_A;
        logic [4:0]                                     S2_PERM;
        logic [3:0]                                     PDTMODE;
        logic [63:12]                                   PPN;
    } mtlb_dat_t;                                       

    localparam ECC_WIDTH = (ECC_ENABLE==0) ? 0 : 7;//iommu_acd_pkg::cal_ecc_width($bits(mtlb_tag_t), $bits(mtlb_itag_t), $bits(mtlb_dat_t));
    localparam TRAM_DAT_WIDTH = $bits(mtlb_tag_t)+ECC_WIDTH+1;
    localparam IRAM_DAT_WIDTH = $bits(mtlb_itag_t)+ECC_WIDTH+1;
    localparam DRAM_DAT_WIDTH = $bits(mtlb_dat_t)+ECC_WIDTH+1;

    logic                                               ftlb_lookup_req_valid_i     ;
    logic                                               ftlb_lookup_req_ready_o     ;
    LOOKUP_REQ_TYPE                                     ftlb_lookup_req_i           ;
    logic                                               ftlb_lookup_ack_valid_o     ;
    logic                                               ftlb_lookup_ack_ready_i     ;
    LOOKUP_ACK_TYPE                                     ftlb_lookup_ack_o           ;
    logic                                               ftlb_update_req_valid_i     ;
    UPDATE_REQ_TYPE                                     ftlb_update_req_i           ;
    logic                                               ftlb_mtlb_update_req_valid_i;
    logic                                               ftlb_mtlb_update_req_ready_o;
    UPDATE_REQ_TYPE                                     ftlb_mtlb_update_req_i      ;
    logic                                               ftlb_inv_req_valid_i        ;
    INVALID_REQ_TYPE                                    ftlb_inv_req_i              ;
    logic                                               ftlb_update_inv_ready_o     ;
    logic                                               ftlb_mtlb_lookup_req_valid_o;
    logic                                               ftlb_mtlb_lookup_req_ready_i;
    LOOKUP_REQ_TYPE                                     ftlb_mtlb_lookup_req_o      ;
    logic                                               ftlb_mtlb_refill_req_valid_o;
    logic                                               ftlb_mtlb_refill_req_ready_i;
    UPDATE_REQ_TYPE                                     ftlb_mtlb_refill_req_o      ;
    logic                                               ftlb_multi_hit_fault_o      ;


    logic                                               mtlb_lookup_req_valid_i ;
    logic                                               mtlb_lookup_req_ready_o ;
    LOOKUP_REQ_TYPE                                     mtlb_lookup_req_i       ;
    logic                                               mtlb_update_req_valid_i ;
    logic                                               mtlb_update_req_ready_o ;
    UPDATE_REQ_TYPE                                     mtlb_update_req_i       ;
    logic                                               mtlb_invalid_req_valid_i;
    logic                                               mtlb_invalid_req_ready_o;
    INVALID_REQ_TYPE                                    mtlb_invalid_req_i      ;
    logic                                               mtlb_lookup_ack_valid_o ;
    UPDATE_REQ_TYPE                                     mtlb_lookup_ack_o       ;
    logic                                               mtlb_lookup_ack_hit_o   ;
    logic                                               mtlb_update_ack_valid_o ;
    UPDATE_REQ_TYPE                                     mtlb_update_ack_o       ;
    logic                                               mtlb_invalid_ack_valid_o;
    INVALID_REQ_TYPE                                    mtlb_invalid_ack_o      ;
    logic                                               mtlb_invalid_req_valid_o;
    logic                                               mtlb_invalid_req_ready_i;
    INVALID_REQ_TYPE                                    mtlb_invalid_req_o      ;

    logic [1:0]                                         mtlb_lookup_ack_valid_demux;
    logic [1:0]                                         mtlb_lookup_ack_ready_demux;

    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               mtlb_b00_tram_cs_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               mtlb_b00_tram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   mtlb_b00_tram_addr_o ;
    //mtlb_tag_t    [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0]                      mtlb_b00_tram_wdata_o;
    //mtlb_tag_t    [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0]                      mtlb_b00_tram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b00_tram_wdata_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b00_tram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               mtlb_b00_iram_cs_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               mtlb_b00_iram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   mtlb_b00_iram_addr_o ;
    //mtlb_itag_t    [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                      mtlb_b00_iram_wdata_o;
    //mtlb_itag_t    [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                      mtlb_b00_iram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b00_iram_wdata_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b00_iram_rdata_i;
    logic [BANK_4K_NUM-1:0]                                                     mtlb_b00_uram_cs_o   ;
    logic [BANK_4K_NUM-1:0]                                                     mtlb_b00_uram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                         mtlb_b00_uram_addr_o ;
    mtlb_utag_t    [BANK_4K_NUM-1:0]                                            mtlb_b00_uram_wdata_o;
    mtlb_utag_t    [BANK_4K_NUM-1:0]                                            mtlb_b00_uram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               mtlb_b00_dram_cs_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               mtlb_b00_dram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   mtlb_b00_dram_addr_o ;
    //mtlb_dat_t    [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                       mtlb_b00_dram_wdata_o;
    //mtlb_dat_t    [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                       mtlb_b00_dram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b00_dram_wdata_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b00_dram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               mtlb_b01_tram_cs_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               mtlb_b01_tram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   mtlb_b01_tram_addr_o ;
    //mtlb_tag_t    [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0]                      mtlb_b01_tram_wdata_o;
    //mtlb_tag_t    [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0]                      mtlb_b01_tram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b01_tram_wdata_o;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b01_tram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               mtlb_b01_iram_cs_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               mtlb_b01_iram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   mtlb_b01_iram_addr_o ;
    //mtlb_itag_t    [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                      mtlb_b01_iram_wdata_o;
    //mtlb_itag_t    [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                      mtlb_b01_iram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b01_iram_wdata_o;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b01_iram_rdata_i;
    logic [BANK_2M_NUM-1:0]                                                     mtlb_b01_uram_cs_o   ;
    logic [BANK_2M_NUM-1:0]                                                     mtlb_b01_uram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                         mtlb_b01_uram_addr_o ;
    mtlb_utag_t    [BANK_2M_NUM-1:0]                                            mtlb_b01_uram_wdata_o;
    mtlb_utag_t    [BANK_2M_NUM-1:0]                                            mtlb_b01_uram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               mtlb_b01_dram_cs_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               mtlb_b01_dram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   mtlb_b01_dram_addr_o ;
    //mtlb_dat_t    [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                       mtlb_b01_dram_wdata_o;
    //mtlb_dat_t    [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                       mtlb_b01_dram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b01_dram_wdata_o;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b01_dram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               mtlb_b10_tram_cs_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               mtlb_b10_tram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   mtlb_b10_tram_addr_o ;
    //mtlb_tag_t    [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0]                      mtlb_b10_tram_wdata_o;
    //mtlb_tag_t    [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0]                      mtlb_b10_tram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b10_tram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b10_tram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               mtlb_b10_iram_cs_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               mtlb_b10_iram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   mtlb_b10_iram_addr_o ;
    //mtlb_itag_t    [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                      mtlb_b10_iram_wdata_o;
    //mtlb_itag_t    [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                      mtlb_b10_iram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b10_iram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b10_iram_rdata_i;
    logic [BANK_1G_NUM-1:0]                                                     mtlb_b10_uram_cs_o   ;
    logic [BANK_1G_NUM-1:0]                                                     mtlb_b10_uram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                         mtlb_b10_uram_addr_o ;
    mtlb_utag_t    [BANK_1G_NUM-1:0]                                            mtlb_b10_uram_wdata_o;
    mtlb_utag_t    [BANK_1G_NUM-1:0]                                            mtlb_b10_uram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               mtlb_b10_dram_cs_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               mtlb_b10_dram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   mtlb_b10_dram_addr_o ;
    //mtlb_dat_t    [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                       mtlb_b10_dram_wdata_o;
    //mtlb_dat_t    [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                       mtlb_b10_dram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b10_dram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b10_dram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               mtlb_b11_tram_cs_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               mtlb_b11_tram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   mtlb_b11_tram_addr_o ;
    //mtlb_tag_t    [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0]                      mtlb_b11_tram_wdata_o;
    //mtlb_tag_t    [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0]                      mtlb_b11_tram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b11_tram_wdata_o;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b11_tram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               mtlb_b11_iram_cs_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               mtlb_b11_iram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   mtlb_b11_iram_addr_o ;
    //mtlb_itag_t    [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                      mtlb_b11_iram_wdata_o;
    //mtlb_itag_t    [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                      mtlb_b11_iram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b11_iram_wdata_o;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b11_iram_rdata_i;
    logic [BANK_0T_NUM-1:0]                                                     mtlb_b11_uram_cs_o   ;
    logic [BANK_0T_NUM-1:0]                                                     mtlb_b11_uram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                         mtlb_b11_uram_addr_o ;
    mtlb_utag_t    [BANK_0T_NUM-1:0]                                            mtlb_b11_uram_wdata_o;
    mtlb_utag_t    [BANK_0T_NUM-1:0]                                            mtlb_b11_uram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               mtlb_b11_dram_cs_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               mtlb_b11_dram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   mtlb_b11_dram_addr_o ;
    //mtlb_dat_t    [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                       mtlb_b11_dram_wdata_o;
    //mtlb_dat_t    [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                       mtlb_b11_dram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b11_dram_wdata_o;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          mtlb_b11_dram_rdata_i;

    logic                                           ram_initial_done;
    
    logic                                                                       idbg_ongoing_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               idbg_mtlb_b00_tram_cs_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               idbg_mtlb_b00_tram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   idbg_mtlb_b00_tram_addr_o ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b00_tram_wdata_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b00_tram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               idbg_mtlb_b00_iram_cs_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               idbg_mtlb_b00_iram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   idbg_mtlb_b00_iram_addr_o ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b00_iram_wdata_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b00_iram_rdata_i;
    logic [BANK_4K_NUM-1:0]                                                     idbg_mtlb_b00_uram_cs_o   ;
    logic [BANK_4K_NUM-1:0]                                                     idbg_mtlb_b00_uram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                         idbg_mtlb_b00_uram_addr_o ;
    mtlb_utag_t    [BANK_4K_NUM-1:0]                                            idbg_mtlb_b00_uram_wdata_o;
    mtlb_utag_t    [BANK_4K_NUM-1:0]                                            idbg_mtlb_b00_uram_rdata_i;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               idbg_mtlb_b00_dram_cs_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                               idbg_mtlb_b00_dram_wr_o   ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]   idbg_mtlb_b00_dram_addr_o ;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b00_dram_wdata_o;
    logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b00_dram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               idbg_mtlb_b01_tram_cs_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               idbg_mtlb_b01_tram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   idbg_mtlb_b01_tram_addr_o ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b01_tram_wdata_o;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b01_tram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               idbg_mtlb_b01_iram_cs_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               idbg_mtlb_b01_iram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   idbg_mtlb_b01_iram_addr_o ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b01_iram_wdata_o;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b01_iram_rdata_i;
    logic [BANK_2M_NUM-1:0]                                                     idbg_mtlb_b01_uram_cs_o   ;
    logic [BANK_2M_NUM-1:0]                                                     idbg_mtlb_b01_uram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                         idbg_mtlb_b01_uram_addr_o ;
    mtlb_utag_t    [BANK_2M_NUM-1:0]                                            idbg_mtlb_b01_uram_wdata_o;
    mtlb_utag_t    [BANK_2M_NUM-1:0]                                            idbg_mtlb_b01_uram_rdata_i;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               idbg_mtlb_b01_dram_cs_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                               idbg_mtlb_b01_dram_wr_o   ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]   idbg_mtlb_b01_dram_addr_o ;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b01_dram_wdata_o;
    logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b01_dram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               idbg_mtlb_b10_tram_cs_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               idbg_mtlb_b10_tram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   idbg_mtlb_b10_tram_addr_o ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b10_tram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b10_tram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               idbg_mtlb_b10_iram_cs_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               idbg_mtlb_b10_iram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   idbg_mtlb_b10_iram_addr_o ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b10_iram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b10_iram_rdata_i;
    logic [BANK_1G_NUM-1:0]                                                     idbg_mtlb_b10_uram_cs_o   ;
    logic [BANK_1G_NUM-1:0]                                                     idbg_mtlb_b10_uram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                         idbg_mtlb_b10_uram_addr_o ;
    mtlb_utag_t    [BANK_1G_NUM-1:0]                                            idbg_mtlb_b10_uram_wdata_o;
    mtlb_utag_t    [BANK_1G_NUM-1:0]                                            idbg_mtlb_b10_uram_rdata_i;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               idbg_mtlb_b10_dram_cs_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                               idbg_mtlb_b10_dram_wr_o   ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]   idbg_mtlb_b10_dram_addr_o ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b10_dram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b10_dram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               idbg_mtlb_b11_tram_cs_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               idbg_mtlb_b11_tram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   idbg_mtlb_b11_tram_addr_o ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b11_tram_wdata_o;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          idbg_mtlb_b11_tram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               idbg_mtlb_b11_iram_cs_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               idbg_mtlb_b11_iram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   idbg_mtlb_b11_iram_addr_o ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b11_iram_wdata_o;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          idbg_mtlb_b11_iram_rdata_i;
    logic [BANK_0T_NUM-1:0]                                                     idbg_mtlb_b11_uram_cs_o   ;
    logic [BANK_0T_NUM-1:0]                                                     idbg_mtlb_b11_uram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                         idbg_mtlb_b11_uram_addr_o ;
    mtlb_utag_t    [BANK_0T_NUM-1:0]                                            idbg_mtlb_b11_uram_wdata_o;
    mtlb_utag_t    [BANK_0T_NUM-1:0]                                            idbg_mtlb_b11_uram_rdata_i;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               idbg_mtlb_b11_dram_cs_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                               idbg_mtlb_b11_dram_wr_o   ;
    logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]   idbg_mtlb_b11_dram_addr_o ;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b11_dram_wdata_o;
    logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]          idbg_mtlb_b11_dram_rdata_i;

    logic            [1:0]                                                      invalid_req_valid_i_mux;
    logic            [1:0]                                                      invalid_req_ready_o_mux;
    INVALID_REQ_TYPE [1:0]                                                      invalid_req_i_mux;
    logic            [1:0]                                                      invalid_ack_valid_o_mux;
    INVALID_REQ_TYPE [1:0]                                                      invalid_ack_o_mux;

    logic                                                                       invalid_req_valid_i_int;
    logic                                                                       invalid_req_ready_o_int;
    INVALID_REQ_TYPE                                                            invalid_req_i_int;
    logic                                                                       invalid_ack_valid_o_int;
    INVALID_REQ_TYPE                                                            invalid_ack_o_int;

//}}}

//=== Main Code === {{{
    assign ram_initial_done_o = ram_initial_done;

//=== ftlb2mtlb cont {{{
    assign ftlb_lookup_ack_ready_i      = mtlb_lookup_ack_valid_o ? 1'b0 : 1'b1;

//    assign ftlb_mtlb_update_req_valid_i = mtlb_lookup_ack_valid_o & mtlb_lookup_ack_hit_o;
    iommu_acd_mtlb_ready_mux U_mtlb_lookup_ack_demux(
    /*input  logic                  */  .clk        (clk                        ),
    /*input  logic                  */  .rstn       (rstn                       ),
    /*input  logic                  */  .valid_i    (mtlb_lookup_ack_valid_o    ),
    /*output logic                  */  .ready_o    (mtlb_lookup_ack_ready_i    ),
    /*output logic [NUM-1:0]        */  .valid_o    (mtlb_lookup_ack_valid_demux),
    /*input  logic [NUM-1:0]        */  .ready_i    (mtlb_lookup_ack_ready_demux)
    );
    assign ftlb_mtlb_update_req_valid_i = mtlb_lookup_ack_valid_demux[1] & mtlb_lookup_ack_hit_o;
    assign mtlb_lookup_ack_ready_demux[1]=ftlb_mtlb_update_req_valid_i ? ftlb_mtlb_update_req_ready_o : 1'b1;
    assign ftlb_mtlb_update_req_i       = mtlb_lookup_ack_o;

    assign ftlb_inv_req_valid_i         = mtlb_invalid_req_valid_o;
    assign mtlb_invalid_req_ready_i     = ftlb_update_inv_ready_o;
    assign ftlb_inv_req_i               = mtlb_invalid_req_o;

    assign mtlb_lookup_req_valid_i      = ftlb_mtlb_lookup_req_valid_o;
    assign ftlb_mtlb_lookup_req_ready_i = mtlb_lookup_req_ready_o;
    assign mtlb_lookup_req_i            = ftlb_mtlb_lookup_req_o;
    
    assign multi_hit_fault_o            = 'd0;
//}}}

//=== lookup ack mux {{{
    assign lookup_ack_valid_o       = mtlb_lookup_ack_valid_demux[0] ?  1'b1 : ftlb_lookup_ack_valid_o;
    assign lookup_ack_o.idx         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.idx       : ftlb_lookup_ack_o.idx     ;
    assign lookup_ack_o.hit         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_hit_o       : ftlb_lookup_ack_o.hit     ;
    assign lookup_ack_o.PBMT        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.PBMT      : ftlb_lookup_ack_o.PBMT    ;
    assign lookup_ack_o.GPPN        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.GPPN      : ftlb_lookup_ack_o.GPPN    ;
    assign lookup_ack_o.ENATS       = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.ENATS     : ftlb_lookup_ack_o.ENATS   ;
    assign lookup_ack_o.T2GPA       = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.T2GPA     : ftlb_lookup_ack_o.T2GPA   ;
    assign lookup_ack_o.DTF         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.DTF       : ftlb_lookup_ack_o.DTF     ;
    assign lookup_ack_o.PDTV        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.PDTV      : ftlb_lookup_ack_o.PDTV    ;
    assign lookup_ack_o.DPE         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.DPE       : ftlb_lookup_ack_o.DPE     ;
    assign lookup_ack_o.SXL         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.SXL       : ftlb_lookup_ack_o.SXL     ;
    assign lookup_ack_o.ENS         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.ENS       : ftlb_lookup_ack_o.ENS     ;
    assign lookup_ack_o.SUM         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.SUM       : ftlb_lookup_ack_o.SUM     ;
    assign lookup_ack_o.S1_D        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S1_D      : ftlb_lookup_ack_o.S1_D    ;
    assign lookup_ack_o.S2_D        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S2_D      : ftlb_lookup_ack_o.S2_D    ;
    assign lookup_ack_o.SADE        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.SADE      : ftlb_lookup_ack_o.SADE    ;
    assign lookup_ack_o.GADE        = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.GADE      : ftlb_lookup_ack_o.GADE    ;
    assign lookup_ack_o.N           = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.N         : ftlb_lookup_ack_o.N       ;
//    assign lookup_ack_o.S1_PERM_D = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S1_PERM_D : ftlb_lookup_ack_o.S1_PERM_D;
//    assign lookup_ack_o.S1_PERM_A = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S1_PERM_A : ftlb_lookup_ack_o.S1_PERM_A;
    assign lookup_ack_o.S1_PERM     = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S1_PERM   : ftlb_lookup_ack_o.S1_PERM ;
//    assign lookup_ack_o.S2_PERM_D = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S2_PERM_D : ftlb_lookup_ack_o.S2_PERM_D;
//    assign lookup_ack_o.S2_PERM_A = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S2_PERM_A : ftlb_lookup_ack_o.S2_PERM_A;
    assign lookup_ack_o.S2_PERM     = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S2_PERM   : ftlb_lookup_ack_o.S2_PERM ;
    assign lookup_ack_o.S1SIZE      = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S1SIZE    : ftlb_lookup_ack_o.S1SIZE  ;
    assign lookup_ack_o.S2SIZE      = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S2SIZE    : ftlb_lookup_ack_o.S2SIZE  ;
    assign lookup_ack_o.S1MODE      = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S1MODE    : ftlb_lookup_ack_o.S1MODE  ;
    assign lookup_ack_o.S2MODE      = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.S2MODE    : ftlb_lookup_ack_o.S2MODE  ;
    assign lookup_ack_o.PDTMODE     = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.PDTMODE   : ftlb_lookup_ack_o.PDTMODE ;
    assign lookup_ack_o.PPN         = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.PPN       : ftlb_lookup_ack_o.PPN     ;
    assign lookup_ack_o.GSCID       = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.GSCID     : ftlb_lookup_ack_o.GSCID   ;
    assign lookup_ack_o.PSCID       = mtlb_lookup_ack_valid_demux[0] ?  mtlb_lookup_ack_o.PSCID     : ftlb_lookup_ack_o.PSCID   ;
    assign mtlb_lookup_ack_ready_demux[0] = 1'b1;
//}}}

//=== update ack {{{
    assign update_ack_valid_o = mtlb_update_ack_valid_o & (mtlb_update_ack_o.idx[TLB_QIDX_WIDTH]!=1'b1);
    assign update_ack_o       = mtlb_update_ack_o;
//}}}

//=== invalid_req_arb === {{{
    assign invalid_req_valid_i_mux[0]   = tlbq_invalid_req_valid_i;
    assign tlbq_invalid_req_ready_o     = invalid_req_ready_o_mux[0];
    assign invalid_req_i_mux[0]         = tlbq_invalid_req_i;

    assign invalid_req_valid_i_mux[1]   = invalid_req_valid_i;
    assign invalid_req_ready_o          = invalid_req_ready_o_mux[1];
    assign invalid_req_i_mux[1]         = invalid_req_i;

    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (2                          ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (INVALID_REQ_TYPE           ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_entry_invalid_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (invalid_req_valid_i_mux    ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   (2'b0                       ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (invalid_req_i_mux          ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (invalid_req_ready_o_mux    ),
    /*output logic                    */ .req_o         (invalid_req_valid_i_int    ),
    /*output DATA_TYPE                */ .data_o        (invalid_req_i_int          ),
    /*input  logic                    */ .gnt_i         (invalid_req_ready_o_int    ) 
    );

//}}}

//=== invalid ack {{{
    assign invalid_ack_valid_o      = mtlb_invalid_ack_valid_o & ~mtlb_invalid_ack_o.idx[INTERNAL_INV_IDX_WIDTH];
    assign invalid_ack_o            = mtlb_invalid_ack_o;

    assign tlbq_invalid_ack_valid_o = mtlb_invalid_ack_valid_o &  mtlb_invalid_ack_o.idx[INTERNAL_INV_IDX_WIDTH];
    assign tlbq_invalid_ack_o       = mtlb_invalid_ack_o;
//}}}

//}}}

//=== microTLB inst === {{{
    assign ftlb_lookup_req_valid_i = lookup_req_valid_i & ram_initial_done;
    assign lookup_req_ready_o      = ftlb_lookup_req_ready_o & ram_initial_done;
    assign ftlb_lookup_req_i       = lookup_req_i;
    assign ftlb_update_req_valid_i = 1'b0;
    assign ftlb_update_req_i       = 'd0;

    iommu_acd_micro_tlb #(
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH                 ), // = iommu_acd_pkg::TLB_QIDX_WIDTH
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE               ), // = logic,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (MICRO_TLB_IDX_WIDTH            ), // = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE                ), // = logic,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE                ), // = logic,
    /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE                ), // = logic,
    /*parameter  */ .SPARE_PARAM                ()  // = 0
    ) U_ftlb(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .lookup_req_valid_i             (ftlb_lookup_req_valid_i        ),
    /*output logic                                      */  .lookup_req_ready_o             (ftlb_lookup_req_ready_o        ),
    /*input  LOOKUP_REQ_TYPE                            */  .lookup_req_i                   (ftlb_lookup_req_i              ),
    /*output logic                                      */  .lookup_ack_valid_o             (ftlb_lookup_ack_valid_o        ),
    /*input  logic                                      */  .lookup_ack_ready_i             (ftlb_lookup_ack_ready_i        ),
    /*output LOOKUP_ACK_TYPE                            */  .lookup_ack_o                   (ftlb_lookup_ack_o              ),
    /*input  logic                                      */  .update_req_valid_i             (ftlb_update_req_valid_i        ),
    /*input  UPDATE_REQ_TYPE                            */  .update_req_i                   (ftlb_update_req_i              ),
    /*input  logic                                      */  .mtlb_update_req_valid_i        (ftlb_mtlb_update_req_valid_i   ),
    /*output logic                                      */  .mtlb_update_req_ready_o        (ftlb_mtlb_update_req_ready_o   ),
    /*input  UPDATE_REQ_TYPE                            */  .mtlb_update_req_i              (ftlb_mtlb_update_req_i         ),
    /*input  logic                                      */  .inv_req_valid_i                (ftlb_inv_req_valid_i           ),
    /*input  INVALID_REQ_TYPE                           */  .inv_req_i                      (ftlb_inv_req_i                 ),
    /*output logic                                      */  .update_inv_ready_o             (ftlb_update_inv_ready_o        ),
    /*output logic                                      */  .mtlb_lookup_req_valid_o        (ftlb_mtlb_lookup_req_valid_o   ),
    /*input  logic                                      */  .mtlb_lookup_req_ready_i        (ftlb_mtlb_lookup_req_ready_i   ),
    /*output LOOKUP_REQ_TYPE                            */  .mtlb_lookup_req_o              (ftlb_mtlb_lookup_req_o         ),
    /*output logic                                      */  .mtlb_refill_req_valid_o        (ftlb_mtlb_refill_req_valid_o   ),
    /*input  logic                                      */  .mtlb_refill_req_ready_i        (ftlb_mtlb_refill_req_ready_i   ),
    /*output UPDATE_REQ_TYPE                            */  .mtlb_refill_req_o              (ftlb_mtlb_refill_req_o         ),
    /*input  logic                                      */  .csr_fctl_gxl_i                 (csr_fctl_gxl_i                 ),
    /*input  logic                                      */  .multi_hit_check_i              (multi_hit_check_i              ),
    /*output logic                                      */  .multi_hit_fault_o              (ftlb_multi_hit_fault_o         ),
`ifdef IOMMU_IDBG
    /*input  iommu_acd_pkg::IDBG_TYPE_M                 */  .idbg_intf_m_i                  (idbg_intf_m_i               [0]),
    /*output iommu_acd_pkg::IDBG_TYPE_S                 */  .idbg_intf_s_o                  (idbg_intf_s_o               [0]),
`endif
    /*input  logic                                      */  .spare_i                        (1'b0                           ) 
);
//}}}

//=== mainTLB inst === {{{
    assign mtlb_update_req_valid_i  = update_req_valid_i & ram_initial_done;
    assign update_req_ready_o       = mtlb_update_req_ready_o & ram_initial_done;
    assign mtlb_update_req_i        = update_req_i;

    assign mtlb_invalid_req_valid_i = invalid_req_valid_i_int & ram_initial_done;
    assign invalid_req_ready_o_int  = mtlb_invalid_req_ready_o & ram_initial_done;
    assign mtlb_invalid_req_i       = invalid_req_i_int;

    iommu_acd_main_tlb #(
    /*parameter type         */ .INVALID_REQ_TYPE                                           (INVALID_REQ_TYPE               ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                                            (LOOKUP_REQ_TYPE                ), // = iommu_acd_pkg::lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                                            (LOOKUP_ACK_TYPE                ), // = iommu_acd_pkg::lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                                            (UPDATE_REQ_TYPE                ), // = iommu_acd_pkg::update_req_t,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH                                        (CABIN_LKP_IDX_WIDTH            ), // = iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH                                        (CABIN_UPD_IDX_WIDTH            ), // = iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    /*parameter  */ .CABIN_INV_IDX_WIDTH                                        (CABIN_INV_IDX_WIDTH            ), // = iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
    /*parameter  */ .BANK_4K_IDX_WIDTH                                          (BANK_4K_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    /*parameter  */ .BANK_2M_IDX_WIDTH                                          (BANK_2M_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    /*parameter  */ .BANK_1G_IDX_WIDTH                                          (BANK_1G_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    /*parameter  */ .BANK_0T_IDX_WIDTH                                          (BANK_0T_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    /*parameter  */ .BANK_4K_SET_IDX_WIDTH                                      (BANK_4K_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    /*parameter  */ .BANK_2M_SET_IDX_WIDTH                                      (BANK_2M_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    /*parameter  */ .BANK_1G_SET_IDX_WIDTH                                      (BANK_1G_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    /*parameter  */ .BANK_0T_SET_IDX_WIDTH                                      (BANK_0T_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    /*parameter  */ .BANK_4K_WAY_IDX_WIDTH                                      (BANK_4K_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_2M_WAY_IDX_WIDTH                                      (BANK_2M_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_1G_WAY_IDX_WIDTH                                      (BANK_1G_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_0T_WAY_IDX_WIDTH                                      (BANK_0T_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE                                              (mtlb_tag_t                     ), // = iommu_acd_pkg::mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                                             (mtlb_itag_t                    ), // = iommu_acd_pkg::mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                                             (mtlb_utag_t                    ), // = iommu_acd_pkg::mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                                              (mtlb_dat_t                     ), // = iommu_acd_pkg::mtlb_dat_t,
    /*parameter  */ .ECC_WIDTH                                                  (ECC_WIDTH                      ), // = 7,
    /*parameter  */ .SPARE_PARAM                                                ()  // = 0
    ) U_mtlb(
    /*input  logic                                                                      */  .clk                            (clk                                ),
    /*input  logic                                                                      */  .rstn                           (rstn                               ),
    /*input  logic                                                                      */  .lookup_req_valid_i             (mtlb_lookup_req_valid_i            ),
    /*output logic                                                                      */  .lookup_req_ready_o             (mtlb_lookup_req_ready_o            ),
    /*input  LOOKUP_REQ_TYPE                                                            */  .lookup_req_i                   (mtlb_lookup_req_i                  ),
    /*input  logic                                                                      */  .update_req_valid_i             (mtlb_update_req_valid_i            ),
    /*output logic                                                                      */  .update_req_ready_o             (mtlb_update_req_ready_o            ),
    /*input  UPDATE_REQ_TYPE                                                            */  .update_req_i                   (mtlb_update_req_i                  ),
    /*input  logic                                                                      */  .refill_req_valid_i             (ftlb_mtlb_refill_req_valid_o       ),
    /*output logic                                                                      */  .refill_req_ready_o             (ftlb_mtlb_refill_req_ready_i       ),
    /*input  UPDATE_REQ_TYPE                                                            */  .refill_req_i                   (ftlb_mtlb_refill_req_o             ),
    /*input  logic                                                                      */  .invalid_req_valid_i            (mtlb_invalid_req_valid_i           ),
    /*output logic                                                                      */  .invalid_req_ready_o            (mtlb_invalid_req_ready_o           ),
    /*input  INVALID_REQ_TYPE                                                           */  .invalid_req_i                  (mtlb_invalid_req_i                 ),
    /*output logic                                                                      */  .lookup_ack_valid_o             (mtlb_lookup_ack_valid_o            ),
    /*input  logic                                                                      */  .lookup_ack_ready_i             (mtlb_lookup_ack_ready_i            ),
    /*output UPDATE_REQ_TYPE                                                            */  .lookup_ack_o                   (mtlb_lookup_ack_o                  ),
    /*output logic                                                                      */  .lookup_ack_hit_o               (mtlb_lookup_ack_hit_o              ),
    /*output logic                                                                      */  .update_ack_valid_o             (mtlb_update_ack_valid_o            ),
    /*output UPDATE_REQ_TYPE                                                            */  .update_ack_o                   (mtlb_update_ack_o                  ),       // only idx used by now, other constant 0
    /*output logic                                                                      */  .invalid_ack_valid_o            (mtlb_invalid_ack_valid_o           ),
    /*output INVALID_REQ_TYPE                                                           */  .invalid_ack_o                  (mtlb_invalid_ack_o                 ),       // only idx used by now, other constant 0
    /*output logic                                                                      */  .invalid_req_valid_o            (mtlb_invalid_req_valid_o           ),
    /*input  logic                                                                      */  .invalid_req_ready_i            (mtlb_invalid_req_ready_i           ),
    /*output INVALID_REQ_TYPE                                                           */  .invalid_req_o                  (mtlb_invalid_req_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_tram_cs_o                  (mtlb_b00_tram_cs_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_tram_wr_o                  (mtlb_b00_tram_wr_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .b00_tram_addr_o                (mtlb_b00_tram_addr_o               ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b00_tram_wdata_o               (mtlb_b00_tram_wdata_o              ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b00_tram_rdata_i               (mtlb_b00_tram_rdata_i              ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_iram_cs_o                  (mtlb_b00_iram_cs_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_iram_wr_o                  (mtlb_b00_iram_wr_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .b00_iram_addr_o                (mtlb_b00_iram_addr_o               ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_wdata_o               (mtlb_b00_iram_wdata_o              ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_rdata_i               (mtlb_b00_iram_rdata_i              ),
    /*output logic [BANK_4K_NUM-1:0]                                                    */  .b00_uram_cs_o                  (mtlb_b00_uram_cs_o                 ),
    /*output logic [BANK_4K_NUM-1:0]                                                    */  .b00_uram_wr_o                  (mtlb_b00_uram_wr_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                        */  .b00_uram_addr_o                (mtlb_b00_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                           */  .b00_uram_wdata_o               (mtlb_b00_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                           */  .b00_uram_rdata_i               (mtlb_b00_uram_rdata_i              ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_dram_cs_o                  (mtlb_b00_dram_cs_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_dram_wr_o                  (mtlb_b00_dram_wr_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .b00_dram_addr_o                (mtlb_b00_dram_addr_o               ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b00_dram_wdata_o               (mtlb_b00_dram_wdata_o              ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b00_dram_rdata_i               (mtlb_b00_dram_rdata_i              ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_tram_cs_o                  (mtlb_b01_tram_cs_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_tram_wr_o                  (mtlb_b01_tram_wr_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .b01_tram_addr_o                (mtlb_b01_tram_addr_o               ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b01_tram_wdata_o               (mtlb_b01_tram_wdata_o              ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b01_tram_rdata_i               (mtlb_b01_tram_rdata_i              ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_iram_cs_o                  (mtlb_b01_iram_cs_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_iram_wr_o                  (mtlb_b01_iram_wr_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .b01_iram_addr_o                (mtlb_b01_iram_addr_o               ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_wdata_o               (mtlb_b01_iram_wdata_o              ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_rdata_i               (mtlb_b01_iram_rdata_i              ),
    /*output logic [BANK_2M_NUM-1:0]                                                    */  .b01_uram_cs_o                  (mtlb_b01_uram_cs_o                 ),
    /*output logic [BANK_2M_NUM-1:0]                                                    */  .b01_uram_wr_o                  (mtlb_b01_uram_wr_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                        */  .b01_uram_addr_o                (mtlb_b01_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                           */  .b01_uram_wdata_o               (mtlb_b01_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                           */  .b01_uram_rdata_i               (mtlb_b01_uram_rdata_i              ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_dram_cs_o                  (mtlb_b01_dram_cs_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_dram_wr_o                  (mtlb_b01_dram_wr_o                 ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .b01_dram_addr_o                (mtlb_b01_dram_addr_o               ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b01_dram_wdata_o               (mtlb_b01_dram_wdata_o              ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b01_dram_rdata_i               (mtlb_b01_dram_rdata_i              ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_tram_cs_o                  (mtlb_b10_tram_cs_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_tram_wr_o                  (mtlb_b10_tram_wr_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .b10_tram_addr_o                (mtlb_b10_tram_addr_o               ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b10_tram_wdata_o               (mtlb_b10_tram_wdata_o              ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b10_tram_rdata_i               (mtlb_b10_tram_rdata_i              ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_iram_cs_o                  (mtlb_b10_iram_cs_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_iram_wr_o                  (mtlb_b10_iram_wr_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .b10_iram_addr_o                (mtlb_b10_iram_addr_o               ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_wdata_o               (mtlb_b10_iram_wdata_o              ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_rdata_i               (mtlb_b10_iram_rdata_i              ),
    /*output logic [BANK_1G_NUM-1:0]                                                    */  .b10_uram_cs_o                  (mtlb_b10_uram_cs_o                 ),
    /*output logic [BANK_1G_NUM-1:0]                                                    */  .b10_uram_wr_o                  (mtlb_b10_uram_wr_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                        */  .b10_uram_addr_o                (mtlb_b10_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                           */  .b10_uram_wdata_o               (mtlb_b10_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                           */  .b10_uram_rdata_i               (mtlb_b10_uram_rdata_i              ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_dram_cs_o                  (mtlb_b10_dram_cs_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_dram_wr_o                  (mtlb_b10_dram_wr_o                 ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .b10_dram_addr_o                (mtlb_b10_dram_addr_o               ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b10_dram_wdata_o               (mtlb_b10_dram_wdata_o              ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b10_dram_rdata_i               (mtlb_b10_dram_rdata_i              ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_tram_cs_o                  (mtlb_b11_tram_cs_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_tram_wr_o                  (mtlb_b11_tram_wr_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .b11_tram_addr_o                (mtlb_b11_tram_addr_o               ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b11_tram_wdata_o               (mtlb_b11_tram_wdata_o              ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b11_tram_rdata_i               (mtlb_b11_tram_rdata_i              ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_iram_cs_o                  (mtlb_b11_iram_cs_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_iram_wr_o                  (mtlb_b11_iram_wr_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .b11_iram_addr_o                (mtlb_b11_iram_addr_o               ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b11_iram_wdata_o               (mtlb_b11_iram_wdata_o              ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b11_iram_rdata_i               (mtlb_b11_iram_rdata_i              ),
    /*output logic [BANK_0T_NUM-1:0]                                                    */  .b11_uram_cs_o                  (mtlb_b11_uram_cs_o                 ),
    /*output logic [BANK_0T_NUM-1:0]                                                    */  .b11_uram_wr_o                  (mtlb_b11_uram_wr_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                        */  .b11_uram_addr_o                (mtlb_b11_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                           */  .b11_uram_wdata_o               (mtlb_b11_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                           */  .b11_uram_rdata_i               (mtlb_b11_uram_rdata_i              ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_dram_cs_o                  (mtlb_b11_dram_cs_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_dram_wr_o                  (mtlb_b11_dram_wr_o                 ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .b11_dram_addr_o                (mtlb_b11_dram_addr_o               ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b11_dram_wdata_o               (mtlb_b11_dram_wdata_o              ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b11_dram_rdata_i               (mtlb_b11_dram_rdata_i              ),
    /*input  logic                                                                      */  .csr_fctl_gxl_i                 (csr_fctl_gxl_i                     ),
    /*input  logic                                                                      */  .multi_hit_check_i              (multi_hit_check_i                  ),
    /*output logic [3:0]                                                                */  .multi_hit_fault_o              (                                   ),
    /*output logic [1:0]                                                                */  .ecc_err_o                      (ecc_err_o                          ),
    /*input  logic                                                                      */  .spare_in                       (1'b0                               )
    );

//}}}

//=== MTLB RAM inst === {{{
    iommu_acd_mtlb_ram_wrap #(
    /*parameter  */ .BANK_4K_IDX_WIDTH          (BANK_4K_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    /*parameter  */ .BANK_2M_IDX_WIDTH          (BANK_2M_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    /*parameter  */ .BANK_1G_IDX_WIDTH          (BANK_1G_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    /*parameter  */ .BANK_0T_IDX_WIDTH          (BANK_0T_IDX_WIDTH              ), // = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    /*parameter  */ .BANK_4K_SET_IDX_WIDTH      (BANK_4K_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    /*parameter  */ .BANK_2M_SET_IDX_WIDTH      (BANK_2M_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    /*parameter  */ .BANK_1G_SET_IDX_WIDTH      (BANK_1G_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    /*parameter  */ .BANK_0T_SET_IDX_WIDTH      (BANK_0T_SET_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    /*parameter  */ .BANK_4K_WAY_IDX_WIDTH      (BANK_4K_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_2M_WAY_IDX_WIDTH      (BANK_2M_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_1G_WAY_IDX_WIDTH      (BANK_1G_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_0T_WAY_IDX_WIDTH      (BANK_0T_WAY_IDX_WIDTH          ), // = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE              (mtlb_tag_t                     ), // = iommu_acd_pkg::mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE             (mtlb_itag_t                    ), // = iommu_acd_pkg::mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE             (mtlb_utag_t                    ), // = iommu_acd_pkg::mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE              (mtlb_dat_t                     ), // = iommu_acd_pkg::mtlb_dat_t,
    /*parameter type         */ .ECC_WIDTH                  (ECC_WIDTH                      ), // = 7,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0
    ) U_mtlb_ram(                                                                                      
    /*input  logic                                      */  .clk                            (clk                    ),
    /*input  logic                                      */  .rstn                           (rstn                   ),
    /*output logic                                      */  .ram_initial_done_o             (ram_initial_done       ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_tram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b00_tram_cs_o    : mtlb_b00_tram_cs_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_tram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b00_tram_wr_o    : mtlb_b00_tram_wr_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .b00_tram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b00_tram_addr_o  : mtlb_b00_tram_addr_o   ),
    /*input  logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b00_tram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b00_tram_wdata_o : mtlb_b00_tram_wdata_o  ),
    /*output logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b00_tram_rdata_o       (mtlb_b00_tram_rdata_i  ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_iram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b00_iram_cs_o    : mtlb_b00_iram_cs_o     ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_iram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b00_iram_wr_o    : mtlb_b00_iram_wr_o     ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .b00_iram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b00_iram_addr_o  : mtlb_b00_iram_addr_o   ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b00_iram_wdata_o : mtlb_b00_iram_wdata_o  ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_rdata_o       (mtlb_b00_iram_rdata_i  ),
    /*input  logic [BANK_4K_NUM-1:0]                                                    */  .b00_uram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b00_uram_cs_o    : mtlb_b00_uram_cs_o     ),
    /*input  logic [BANK_4K_NUM-1:0]                                                    */  .b00_uram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b00_uram_wr_o    : mtlb_b00_uram_wr_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                        */  .b00_uram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b00_uram_addr_o  : mtlb_b00_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                           */  .b00_uram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b00_uram_wdata_o : mtlb_b00_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_4K_NUM-1:0]                                           */  .b00_uram_rdata_o       (mtlb_b00_uram_rdata_i  ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_dram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b00_dram_cs_o    : mtlb_b00_dram_cs_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .b00_dram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b00_dram_wr_o    : mtlb_b00_dram_wr_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .b00_dram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b00_dram_addr_o  : mtlb_b00_dram_addr_o   ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b00_dram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b00_dram_wdata_o : mtlb_b00_dram_wdata_o  ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b00_dram_rdata_o       (mtlb_b00_dram_rdata_i  ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_tram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b01_tram_cs_o    : mtlb_b01_tram_cs_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_tram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b01_tram_wr_o    : mtlb_b01_tram_wr_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .b01_tram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b01_tram_addr_o  : mtlb_b01_tram_addr_o   ),
    /*input  logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b01_tram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b01_tram_wdata_o : mtlb_b01_tram_wdata_o  ),
    /*output logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b01_tram_rdata_o       (mtlb_b01_tram_rdata_i  ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_iram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b01_iram_cs_o    : mtlb_b01_iram_cs_o     ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_iram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b01_iram_wr_o    : mtlb_b01_iram_wr_o     ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .b01_iram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b01_iram_addr_o  : mtlb_b01_iram_addr_o   ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b01_iram_wdata_o : mtlb_b01_iram_wdata_o  ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_rdata_o       (mtlb_b01_iram_rdata_i  ),
    /*input  logic [BANK_2M_NUM-1:0]                                                    */  .b01_uram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b01_uram_cs_o    : mtlb_b01_uram_cs_o     ),
    /*input  logic [BANK_2M_NUM-1:0]                                                    */  .b01_uram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b01_uram_wr_o    : mtlb_b01_uram_wr_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                        */  .b01_uram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b01_uram_addr_o  : mtlb_b01_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                           */  .b01_uram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b01_uram_wdata_o : mtlb_b01_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_2M_NUM-1:0]                                           */  .b01_uram_rdata_o       (mtlb_b01_uram_rdata_i  ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_dram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b01_dram_cs_o    : mtlb_b01_dram_cs_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .b01_dram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b01_dram_wr_o    : mtlb_b01_dram_wr_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .b01_dram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b01_dram_addr_o  : mtlb_b01_dram_addr_o   ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b01_dram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b01_dram_wdata_o : mtlb_b01_dram_wdata_o  ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b01_dram_rdata_o       (mtlb_b01_dram_rdata_i  ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_tram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b10_tram_cs_o    : mtlb_b10_tram_cs_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_tram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b10_tram_wr_o    : mtlb_b10_tram_wr_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .b10_tram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b10_tram_addr_o  : mtlb_b10_tram_addr_o   ),
    /*input  logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b10_tram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b10_tram_wdata_o : mtlb_b10_tram_wdata_o  ),
    /*output logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b10_tram_rdata_o       (mtlb_b10_tram_rdata_i  ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_iram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b10_iram_cs_o    : mtlb_b10_iram_cs_o     ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_iram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b10_iram_wr_o    : mtlb_b10_iram_wr_o     ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .b10_iram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b10_iram_addr_o  : mtlb_b10_iram_addr_o   ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b10_iram_wdata_o : mtlb_b10_iram_wdata_o  ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_rdata_o       (mtlb_b10_iram_rdata_i  ),
    /*input  logic [BANK_1G_NUM-1:0]                                                    */  .b10_uram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b10_uram_cs_o    : mtlb_b10_uram_cs_o     ),
    /*input  logic [BANK_1G_NUM-1:0]                                                    */  .b10_uram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b10_uram_wr_o    : mtlb_b10_uram_wr_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                        */  .b10_uram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b10_uram_addr_o  : mtlb_b10_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                           */  .b10_uram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b10_uram_wdata_o : mtlb_b10_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_1G_NUM-1:0]                                           */  .b10_uram_rdata_o       (mtlb_b10_uram_rdata_i  ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_dram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b10_dram_cs_o    : mtlb_b10_dram_cs_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .b10_dram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b10_dram_wr_o    : mtlb_b10_dram_wr_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .b10_dram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b10_dram_addr_o  : mtlb_b10_dram_addr_o   ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b10_dram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b10_dram_wdata_o : mtlb_b10_dram_wdata_o  ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b10_dram_rdata_o       (mtlb_b10_dram_rdata_i  ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_tram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b11_tram_cs_o    : mtlb_b11_tram_cs_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_tram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b11_tram_wr_o    : mtlb_b11_tram_wr_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .b11_tram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b11_tram_addr_o  : mtlb_b11_tram_addr_o   ),
    /*input  logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b11_tram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b11_tram_wdata_o : mtlb_b11_tram_wdata_o  ),
    /*output logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .b11_tram_rdata_o       (mtlb_b11_tram_rdata_i  ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_iram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b11_iram_cs_o    : mtlb_b11_iram_cs_o     ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_iram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b11_iram_wr_o    : mtlb_b11_iram_wr_o     ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .b11_iram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b11_iram_addr_o  : mtlb_b11_iram_addr_o   ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b11_iram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b11_iram_wdata_o : mtlb_b11_iram_wdata_o  ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b11_iram_rdata_o       (mtlb_b11_iram_rdata_i  ),
    /*input  logic [BANK_0T_NUM-1:0]                                                    */  .b11_uram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b11_uram_cs_o    : mtlb_b11_uram_cs_o     ),
    /*input  logic [BANK_0T_NUM-1:0]                                                    */  .b11_uram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b11_uram_wr_o    : mtlb_b11_uram_wr_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                        */  .b11_uram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b11_uram_addr_o  : mtlb_b11_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                           */  .b11_uram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b11_uram_wdata_o : mtlb_b11_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_0T_NUM-1:0]                                           */  .b11_uram_rdata_o       (mtlb_b11_uram_rdata_i  ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_dram_cs_i          (idbg_ongoing_o ? idbg_mtlb_b11_dram_cs_o    : mtlb_b11_dram_cs_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .b11_dram_wr_i          (idbg_ongoing_o ? idbg_mtlb_b11_dram_wr_o    : mtlb_b11_dram_wr_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .b11_dram_addr_i        (idbg_ongoing_o ? idbg_mtlb_b11_dram_addr_o  : mtlb_b11_dram_addr_o   ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b11_dram_wdata_i       (idbg_ongoing_o ? idbg_mtlb_b11_dram_wdata_o : mtlb_b11_dram_wdata_o  ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .b11_dram_rdata_o       (mtlb_b11_dram_rdata_i  ),
    /*input  logic                                                                      */  .spare_in               (1'b0                   )
    );

//}}}

//=== HPM === {{{
//    iommu_acd_hpm_tlb_wrap U_hpm(
//    /*input  logic                  */  .clk                        (clk                        ),
//    /*input  logic                  */  .rstn                       (rstn                       ),
//    /*input  logic                  */  .lookup_req_valid_i         (lookup_req_valid_i         ),
//    /*input  logic                  */  .lookup_req_ready_i         (lookup_req_ready_o         ),
//    /*input  logic                  */  .lookup_ack_valid_i         (lookup_ack_valid_o         ),
//    /*input  logic                  */  .lookup_ack_hit_i           (lookup_ack_o.hit           ),
//    /*input  logic                  */  .ftlb_lookup_ack_valid_i    (ftlb_lookup_ack_valid_o    ),
//    /*input  logic                  */  .ftlb_lookup_ack_ready_i    (ftlb_lookup_ack_ready_i    ),
//    /*input  logic [3:0]            */  .hpm_cnt_inhibit_i          (hpm_cnt_inhibit_i          ),
//    /*output logic [63:0]           */  .hpm_cnt_o                  (hpm_cnt_o                  ), //[3:0],
//    /*input  logic                  */  .spare_in                   (1'b0                       )
//    );
//
//}}}

`ifdef IOMMU_IDBG
//=== IDBG === {{{
    assign idbg_mtlb_b00_tram_rdata_i = mtlb_b00_tram_rdata_i;
    assign idbg_mtlb_b00_iram_rdata_i = mtlb_b00_iram_rdata_i;
    assign idbg_mtlb_b00_uram_rdata_i = mtlb_b00_uram_rdata_i;
    assign idbg_mtlb_b00_dram_rdata_i = mtlb_b00_dram_rdata_i;
    assign idbg_mtlb_b01_tram_rdata_i = mtlb_b01_tram_rdata_i;
    assign idbg_mtlb_b01_iram_rdata_i = mtlb_b01_iram_rdata_i;
    assign idbg_mtlb_b01_uram_rdata_i = mtlb_b01_uram_rdata_i;
    assign idbg_mtlb_b01_dram_rdata_i = mtlb_b01_dram_rdata_i;
    assign idbg_mtlb_b10_tram_rdata_i = mtlb_b10_tram_rdata_i;
    assign idbg_mtlb_b10_iram_rdata_i = mtlb_b10_iram_rdata_i;
    assign idbg_mtlb_b10_uram_rdata_i = mtlb_b10_uram_rdata_i;
    assign idbg_mtlb_b10_dram_rdata_i = mtlb_b10_dram_rdata_i;
    assign idbg_mtlb_b11_tram_rdata_i = mtlb_b11_tram_rdata_i;
    assign idbg_mtlb_b11_iram_rdata_i = mtlb_b11_iram_rdata_i;
    assign idbg_mtlb_b11_uram_rdata_i = mtlb_b11_uram_rdata_i;
    assign idbg_mtlb_b11_dram_rdata_i = mtlb_b11_dram_rdata_i;

    iommu_acd_mon_unit_mtlb_ram #(
    /*parameter  */ .BANK_4K_IDX_WIDTH                                          (BANK_4K_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    /*parameter  */ .BANK_2M_IDX_WIDTH                                          (BANK_2M_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    /*parameter  */ .BANK_1G_IDX_WIDTH                                          (BANK_1G_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    /*parameter  */ .BANK_0T_IDX_WIDTH                                          (BANK_0T_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    /*parameter  */ .BANK_4K_SET_IDX_WIDTH                                      (BANK_4K_SET_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    /*parameter  */ .BANK_2M_SET_IDX_WIDTH                                      (BANK_2M_SET_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    /*parameter  */ .BANK_1G_SET_IDX_WIDTH                                      (BANK_1G_SET_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    /*parameter  */ .BANK_0T_SET_IDX_WIDTH                                      (BANK_0T_SET_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    /*parameter  */ .BANK_4K_WAY_IDX_WIDTH                                      (BANK_4K_WAY_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_2M_WAY_IDX_WIDTH                                      (BANK_2M_WAY_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_1G_WAY_IDX_WIDTH                                      (BANK_1G_WAY_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_0T_WAY_IDX_WIDTH                                      (BANK_0T_WAY_IDX_WIDTH  ), //= iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_T                                                 (mtlb_tag_t             ), //= logic,
    /*parameter type         */ .MTLB_ITAG_T                                                (mtlb_itag_t            ), //= logic,
    /*parameter type         */ .MTLB_UTAG_T                                                (mtlb_utag_t            ), //= logic,
    /*parameter type         */ .MTLB_DAT_T                                                 (mtlb_dat_t             ), //= logic,
    /*parameter  */ .ECC_WIDTH                                                  (ECC_WIDTH              ), // = 7,
    /*parameter  */ .SPARE_PARAM                                                (1'b0                   )  //= 0 
    ) U_idbg(
    /*input  logic                                                                      */  .clk                    (clk                            ),
    /*input  logic                                                                      */  .rstn                   (rstn                           ),
    /*input  logic                                                                      */  .idbg_go_i              (idbg_intf_m_i[1].idbg_go       ),
    /*output logic                                                                      */  .idbg_busy_o            (idbg_intf_s_o[1].idbg_busy     ),
    /*input  logic [7:0]                                                                */  .idbg_opcode_i          (idbg_intf_m_i[1].idbg_opcode   ),
    /*input  logic [31:0]                                                               */  .idbg_dat_i             (idbg_intf_m_i[1].idbg_datw     ),
    /*output logic [31:0]                                                               */  .idbg_dat_o             (idbg_intf_s_o[1].idbg_datr     ),
    /*output logic                                                                      */  .idbg_datv_o            (idbg_intf_s_o[1].idbg_datv     ),
    /*output logic                                                                      */  .idbg_ongoing_o         (idbg_ongoing_o                 ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .mtlb_b00_tram_cs_o     (idbg_mtlb_b00_tram_cs_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .mtlb_b00_tram_wr_o     (idbg_mtlb_b00_tram_wr_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .mtlb_b00_tram_addr_o   (idbg_mtlb_b00_tram_addr_o      ),
    /*output logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b00_tram_wdata_o  (idbg_mtlb_b00_tram_wdata_o     ),
    /*input  logic [BANK_4K_NUM-1:0]  [BANK_4K_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b00_tram_rdata_i  (idbg_mtlb_b00_tram_rdata_i     ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .mtlb_b00_iram_cs_o     (idbg_mtlb_b00_iram_cs_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .mtlb_b00_iram_wr_o     (idbg_mtlb_b00_iram_wr_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .mtlb_b00_iram_addr_o   (idbg_mtlb_b00_iram_addr_o      ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b00_iram_wdata_o  (idbg_mtlb_b00_iram_wdata_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b00_iram_rdata_i  (idbg_mtlb_b00_iram_rdata_i     ),
    /*output logic [BANK_4K_NUM-1:0]                                                    */  .mtlb_b00_uram_cs_o     (idbg_mtlb_b00_uram_cs_o        ),
    /*output logic [BANK_4K_NUM-1:0]                                                    */  .mtlb_b00_uram_wr_o     (idbg_mtlb_b00_uram_wr_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]                        */  .mtlb_b00_uram_addr_o   (idbg_mtlb_b00_uram_addr_o      ),
    /*output MTLB_UTAG_T    [BANK_4K_NUM-1:0]                                           */  .mtlb_b00_uram_wdata_o  (idbg_mtlb_b00_uram_wdata_o     ),
    /*input  MTLB_UTAG_T    [BANK_4K_NUM-1:0]                                           */  .mtlb_b00_uram_rdata_i  (idbg_mtlb_b00_uram_rdata_i     ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .mtlb_b00_dram_cs_o     (idbg_mtlb_b00_dram_cs_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0]                              */  .mtlb_b00_dram_wr_o     (idbg_mtlb_b00_dram_wr_o        ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [BANK_4K_SET_IDX_WIDTH-1:0]  */  .mtlb_b00_dram_addr_o   (idbg_mtlb_b00_dram_addr_o      ),
    /*output logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b00_dram_wdata_o  (idbg_mtlb_b00_dram_wdata_o     ),
    /*input  logic [BANK_4K_NUM-1:0] [BANK_4K_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b00_dram_rdata_i  (idbg_mtlb_b00_dram_rdata_i     ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .mtlb_b01_tram_cs_o     (idbg_mtlb_b01_tram_cs_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .mtlb_b01_tram_wr_o     (idbg_mtlb_b01_tram_wr_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .mtlb_b01_tram_addr_o   (idbg_mtlb_b01_tram_addr_o      ),
    /*output logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b01_tram_wdata_o  (idbg_mtlb_b01_tram_wdata_o     ),
    /*input  logic [BANK_2M_NUM-1:0]  [BANK_2M_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b01_tram_rdata_i  (idbg_mtlb_b01_tram_rdata_i     ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .mtlb_b01_iram_cs_o     (idbg_mtlb_b01_iram_cs_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .mtlb_b01_iram_wr_o     (idbg_mtlb_b01_iram_wr_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .mtlb_b01_iram_addr_o   (idbg_mtlb_b01_iram_addr_o      ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b01_iram_wdata_o  (idbg_mtlb_b01_iram_wdata_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b01_iram_rdata_i  (idbg_mtlb_b01_iram_rdata_i     ),
    /*output logic [BANK_2M_NUM-1:0]                                                    */  .mtlb_b01_uram_cs_o     (idbg_mtlb_b01_uram_cs_o        ),
    /*output logic [BANK_2M_NUM-1:0]                                                    */  .mtlb_b01_uram_wr_o     (idbg_mtlb_b01_uram_wr_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]                        */  .mtlb_b01_uram_addr_o   (idbg_mtlb_b01_uram_addr_o      ),
    /*output MTLB_UTAG_T    [BANK_2M_NUM-1:0]                                           */  .mtlb_b01_uram_wdata_o  (idbg_mtlb_b01_uram_wdata_o     ),
    /*input  MTLB_UTAG_T    [BANK_2M_NUM-1:0]                                           */  .mtlb_b01_uram_rdata_i  (idbg_mtlb_b01_uram_rdata_i     ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .mtlb_b01_dram_cs_o     (idbg_mtlb_b01_dram_cs_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0]                              */  .mtlb_b01_dram_wr_o     (idbg_mtlb_b01_dram_wr_o        ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [BANK_2M_SET_IDX_WIDTH-1:0]  */  .mtlb_b01_dram_addr_o   (idbg_mtlb_b01_dram_addr_o      ),
    /*output logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b01_dram_wdata_o  (idbg_mtlb_b01_dram_wdata_o     ),
    /*input  logic [BANK_2M_NUM-1:0] [BANK_2M_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b01_dram_rdata_i  (idbg_mtlb_b01_dram_rdata_i     ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .mtlb_b10_tram_cs_o     (idbg_mtlb_b10_tram_cs_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .mtlb_b10_tram_wr_o     (idbg_mtlb_b10_tram_wr_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .mtlb_b10_tram_addr_o   (idbg_mtlb_b10_tram_addr_o      ),
    /*output logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b10_tram_wdata_o  (idbg_mtlb_b10_tram_wdata_o     ),
    /*input  logic [BANK_1G_NUM-1:0]  [BANK_1G_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b10_tram_rdata_i  (idbg_mtlb_b10_tram_rdata_i     ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .mtlb_b10_iram_cs_o     (idbg_mtlb_b10_iram_cs_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .mtlb_b10_iram_wr_o     (idbg_mtlb_b10_iram_wr_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .mtlb_b10_iram_addr_o   (idbg_mtlb_b10_iram_addr_o      ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b10_iram_wdata_o  (idbg_mtlb_b10_iram_wdata_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b10_iram_rdata_i  (idbg_mtlb_b10_iram_rdata_i     ),
    /*output logic [BANK_1G_NUM-1:0]                                                    */  .mtlb_b10_uram_cs_o     (idbg_mtlb_b10_uram_cs_o        ),
    /*output logic [BANK_1G_NUM-1:0]                                                    */  .mtlb_b10_uram_wr_o     (idbg_mtlb_b10_uram_wr_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]                        */  .mtlb_b10_uram_addr_o   (idbg_mtlb_b10_uram_addr_o      ),
    /*output MTLB_UTAG_T    [BANK_1G_NUM-1:0]                                           */  .mtlb_b10_uram_wdata_o  (idbg_mtlb_b10_uram_wdata_o     ),
    /*input  MTLB_UTAG_T    [BANK_1G_NUM-1:0]                                           */  .mtlb_b10_uram_rdata_i  (idbg_mtlb_b10_uram_rdata_i     ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .mtlb_b10_dram_cs_o     (idbg_mtlb_b10_dram_cs_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0]                              */  .mtlb_b10_dram_wr_o     (idbg_mtlb_b10_dram_wr_o        ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [BANK_1G_SET_IDX_WIDTH-1:0]  */  .mtlb_b10_dram_addr_o   (idbg_mtlb_b10_dram_addr_o      ),
    /*output logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b10_dram_wdata_o  (idbg_mtlb_b10_dram_wdata_o     ),
    /*input  logic [BANK_1G_NUM-1:0] [BANK_1G_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b10_dram_rdata_i  (idbg_mtlb_b10_dram_rdata_i     ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .mtlb_b11_tram_cs_o     (idbg_mtlb_b11_tram_cs_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .mtlb_b11_tram_wr_o     (idbg_mtlb_b11_tram_wr_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .mtlb_b11_tram_addr_o   (idbg_mtlb_b11_tram_addr_o      ),
    /*output logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b11_tram_wdata_o  (idbg_mtlb_b11_tram_wdata_o     ),
    /*input  logic [BANK_0T_NUM-1:0]  [BANK_0T_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]        */  .mtlb_b11_tram_rdata_i  (idbg_mtlb_b11_tram_rdata_i     ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .mtlb_b11_iram_cs_o     (idbg_mtlb_b11_iram_cs_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .mtlb_b11_iram_wr_o     (idbg_mtlb_b11_iram_wr_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .mtlb_b11_iram_addr_o   (idbg_mtlb_b11_iram_addr_o      ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b11_iram_wdata_o  (idbg_mtlb_b11_iram_wdata_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .mtlb_b11_iram_rdata_i  (idbg_mtlb_b11_iram_rdata_i     ),
    /*output logic [BANK_0T_NUM-1:0]                                                    */  .mtlb_b11_uram_cs_o     (idbg_mtlb_b11_uram_cs_o        ),
    /*output logic [BANK_0T_NUM-1:0]                                                    */  .mtlb_b11_uram_wr_o     (idbg_mtlb_b11_uram_wr_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]                        */  .mtlb_b11_uram_addr_o   (idbg_mtlb_b11_uram_addr_o      ),
    /*output MTLB_UTAG_T    [BANK_0T_NUM-1:0]                                           */  .mtlb_b11_uram_wdata_o  (idbg_mtlb_b11_uram_wdata_o     ),
    /*input  MTLB_UTAG_T    [BANK_0T_NUM-1:0]                                           */  .mtlb_b11_uram_rdata_i  (idbg_mtlb_b11_uram_rdata_i     ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .mtlb_b11_dram_cs_o     (idbg_mtlb_b11_dram_cs_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0]                              */  .mtlb_b11_dram_wr_o     (idbg_mtlb_b11_dram_wr_o        ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [BANK_0T_SET_IDX_WIDTH-1:0]  */  .mtlb_b11_dram_addr_o   (idbg_mtlb_b11_dram_addr_o      ),
    /*output logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b11_dram_wdata_o  (idbg_mtlb_b11_dram_wdata_o     ),
    /*input  logic [BANK_0T_NUM-1:0] [BANK_0T_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         */  .mtlb_b11_dram_rdata_i  (idbg_mtlb_b11_dram_rdata_i     ),
    /*input  logic                                                                      */  .param_in               (1'b0                           )
);
//}}}
`else
assign idbg_ongoing_o = 1'b0;

//{{{
    assign idbg_mtlb_b00_tram_rdata_i = mtlb_b00_tram_rdata_i;
    assign idbg_mtlb_b00_iram_rdata_i = mtlb_b00_iram_rdata_i;
    assign idbg_mtlb_b00_uram_rdata_i = mtlb_b00_uram_rdata_i;
    assign idbg_mtlb_b00_dram_rdata_i = mtlb_b00_dram_rdata_i;
    assign idbg_mtlb_b01_tram_rdata_i = mtlb_b01_tram_rdata_i;
    assign idbg_mtlb_b01_iram_rdata_i = mtlb_b01_iram_rdata_i;
    assign idbg_mtlb_b01_uram_rdata_i = mtlb_b01_uram_rdata_i;
    assign idbg_mtlb_b01_dram_rdata_i = mtlb_b01_dram_rdata_i;
    assign idbg_mtlb_b10_tram_rdata_i = mtlb_b10_tram_rdata_i;
    assign idbg_mtlb_b10_iram_rdata_i = mtlb_b10_iram_rdata_i;
    assign idbg_mtlb_b10_uram_rdata_i = mtlb_b10_uram_rdata_i;
    assign idbg_mtlb_b10_dram_rdata_i = mtlb_b10_dram_rdata_i;
    assign idbg_mtlb_b11_tram_rdata_i = mtlb_b11_tram_rdata_i;
    assign idbg_mtlb_b11_iram_rdata_i = mtlb_b11_iram_rdata_i;
    assign idbg_mtlb_b11_uram_rdata_i = mtlb_b11_uram_rdata_i;
    assign idbg_mtlb_b11_dram_rdata_i = mtlb_b11_dram_rdata_i;
//}}}

genvar i0, i1; //{{{
generate
    for(i0=0; i0<BANK_4K_NUM; i0++) begin : gen_4k_ram_bank
        assign idbg_mtlb_b00_uram_cs_o[i0]       = 1'b1;
        assign idbg_mtlb_b00_uram_wr_o[i0]       = 1'b1;
        assign idbg_mtlb_b00_uram_addr_o[i0]     = 'd0;
        assign idbg_mtlb_b00_uram_wdata_o[i0]    = 'd0;
        for(i1=0; i1<BANK_4K_WAY_NUM; i1++) begin : gen_4k_ram_set
            assign idbg_mtlb_b00_tram_cs_o[i0][i1]   = 1'b1;
            assign idbg_mtlb_b00_iram_cs_o[i0][i1]   = 1'b1;
            assign idbg_mtlb_b00_dram_cs_o[i0][i1]   = 1'b1;
            assign idbg_mtlb_b00_tram_wr_o[i0][i1]   = 1'b1;
            assign idbg_mtlb_b00_iram_wr_o[i0][i1]   = 1'b1;
            assign idbg_mtlb_b00_dram_wr_o[i0][i1]   = 1'b1;
            assign idbg_mtlb_b00_tram_addr_o[i0][i1] = 'd0;
            assign idbg_mtlb_b00_iram_addr_o[i0][i1] = 'd0;
            assign idbg_mtlb_b00_dram_addr_o[i0][i1] = 'd0;
            assign idbg_mtlb_b00_tram_wdata_o[i0][i1]= 'd0;
            assign idbg_mtlb_b00_iram_wdata_o[i0][i1]= 'd0;
            assign idbg_mtlb_b00_dram_wdata_o[i0][i1]= 'd0;
        end
    end
endgenerate //}}}

genvar j0, j1; //{{{
generate
    for(j0=0; j0<BANK_2M_NUM; j0++) begin : gen_2m_ram_bank
        assign idbg_mtlb_b01_uram_cs_o[j0]       = 1'b1;
        assign idbg_mtlb_b01_uram_wr_o[j0]       = 1'b1;
        assign idbg_mtlb_b01_uram_addr_o[j0]     = 'd0;
        assign idbg_mtlb_b01_uram_wdata_o[j0]    = 'd0;
        for(j1=0; j1<BANK_2M_WAY_NUM; j1++) begin : gen_2m_ram_set
            assign idbg_mtlb_b01_tram_cs_o[j0][j1]   = 1'b1;
            assign idbg_mtlb_b01_iram_cs_o[j0][j1]   = 1'b1;
            assign idbg_mtlb_b01_dram_cs_o[j0][j1]   = 1'b1;
            assign idbg_mtlb_b01_tram_wr_o[j0][j1]   = 1'b1;
            assign idbg_mtlb_b01_iram_wr_o[j0][j1]   = 1'b1;
            assign idbg_mtlb_b01_dram_wr_o[j0][j1]   = 1'b1;
            assign idbg_mtlb_b01_tram_addr_o[j0][j1] = 'd0;
            assign idbg_mtlb_b01_iram_addr_o[j0][j1] = 'd0;
            assign idbg_mtlb_b01_dram_addr_o[j0][j1] = 'd0;
            assign idbg_mtlb_b01_tram_wdata_o[j0][j1]= 'd0;
            assign idbg_mtlb_b01_iram_wdata_o[j0][j1]= 'd0;
            assign idbg_mtlb_b01_dram_wdata_o[j0][j1]= 'd0;
        end
    end
endgenerate //}}}

genvar m0, m1; //{{{
generate
    for(m0=0; m0<BANK_1G_NUM; m0++) begin : gen_1g_ram_bank
        assign idbg_mtlb_b10_uram_cs_o[m0]       = 1'b1;
        assign idbg_mtlb_b10_uram_wr_o[m0]       = 1'b1;
        assign idbg_mtlb_b10_uram_addr_o[m0]     = 'd0;
        assign idbg_mtlb_b10_uram_wdata_o[m0]    = 'd0;
        for(m1=0; m1<BANK_1G_WAY_NUM; m1++) begin : gen_1g_ram_set
            assign idbg_mtlb_b10_tram_cs_o[m0][m1]   = 1'b1;
            assign idbg_mtlb_b10_iram_cs_o[m0][m1]   = 1'b1;
            assign idbg_mtlb_b10_dram_cs_o[m0][m1]   = 1'b1;
            assign idbg_mtlb_b10_tram_wr_o[m0][m1]   = 1'b1;
            assign idbg_mtlb_b10_iram_wr_o[m0][m1]   = 1'b1;
            assign idbg_mtlb_b10_dram_wr_o[m0][m1]   = 1'b1;
            assign idbg_mtlb_b10_tram_addr_o[m0][m1] = 'd0;
            assign idbg_mtlb_b10_iram_addr_o[m0][m1] = 'd0;
            assign idbg_mtlb_b10_dram_addr_o[m0][m1] = 'd0;
            assign idbg_mtlb_b10_tram_wdata_o[m0][m1]= 'd0;
            assign idbg_mtlb_b10_iram_wdata_o[m0][m1]= 'd0;
            assign idbg_mtlb_b10_dram_wdata_o[m0][m1]= 'd0;
        end
    end
endgenerate //}}}

genvar n0, n1; //{{{
generate
    for(n0=0; n0<BANK_0T_NUM; n0++) begin : gen_0t_ram_bank
        assign idbg_mtlb_b11_uram_cs_o[n0]       = 1'b1;
        assign idbg_mtlb_b11_uram_wr_o[n0]       = 1'b1;
        assign idbg_mtlb_b11_uram_addr_o[n0]     = 'd0;
        assign idbg_mtlb_b11_uram_wdata_o[n0]    = 'd0;
        for(n1=0; n1<BANK_0T_WAY_NUM; n1++) begin : gen_0t_ram_set
            assign idbg_mtlb_b11_tram_cs_o[n0][n1]   = 1'b1;
            assign idbg_mtlb_b11_iram_cs_o[n0][n1]   = 1'b1;
            assign idbg_mtlb_b11_dram_cs_o[n0][n1]   = 1'b1;
            assign idbg_mtlb_b11_tram_wr_o[n0][n1]   = 1'b1;
            assign idbg_mtlb_b11_iram_wr_o[n0][n1]   = 1'b1;
            assign idbg_mtlb_b11_dram_wr_o[n0][n1]   = 1'b1;
            assign idbg_mtlb_b11_tram_addr_o[n0][n1] = 'd0;
            assign idbg_mtlb_b11_iram_addr_o[n0][n1] = 'd0;
            assign idbg_mtlb_b11_dram_addr_o[n0][n1] = 'd0;
            assign idbg_mtlb_b11_tram_wdata_o[n0][n1]= 'd0;
            assign idbg_mtlb_b11_iram_wdata_o[n0][n1]= 'd0;
            assign idbg_mtlb_b11_dram_wdata_o[n0][n1]= 'd0;
        end
    end
endgenerate //}}}
`endif

endmodule




