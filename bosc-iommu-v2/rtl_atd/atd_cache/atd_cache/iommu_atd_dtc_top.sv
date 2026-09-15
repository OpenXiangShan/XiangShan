////////////////////////////////////////////////////////////////////////////////
// iommu_atd_ddtc_top
////////////////////////////////////////////////////////////////////////////////
module iommu_atd_dtc_top #(
//{{{ PARAM
    parameter   CACHE_TYPE                                          = 0, // 0:DDTC, 1:PDTC
    parameter   TLB_QIDX_WIDTH                                      = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH                                     = 2**TLB_QIDX_WIDTH,
    parameter type          INVALID_REQ_TYPE                                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          MTLB_TAG_TYPE                                       = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                                      = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                                      = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE_NL                                    = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl,
    parameter type          MTLB_DAT_TYPE_L                                     = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    parameter type          MICROTLB_TAG_TYPE                                   = iommu_atd_cache_pkg::ddtc_microtlb_tag_t,
    parameter type          MICROTLB_INV_TAG_TYPE                               = iommu_atd_cache_pkg::ddtc_microtlb_inv_tag_t,
    parameter type          MICROTLB_CONTENT_TYPE                               = iommu_atd_cache_pkg::ddtc_microtlb_content_t,
    parameter   MICRO_TLB_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH,
    parameter   CABIN_LKP_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter   CABIN_UPD_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter   CABIN_INV_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter   BANK_L0_IDX_WIDTH                                   = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_IDX_WIDTH                                   = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_IDX_WIDTH                                   = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    parameter   BANK_L0_SET_IDX_WIDTH                               = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_IDX_WIDTH                               = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_IDX_WIDTH                               = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L0_WAY_IDX_WIDTH                               = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    parameter   BANK_L1_WAY_IDX_WIDTH                               = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    parameter   BANK_L2_WAY_IDX_WIDTH                               = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    parameter   CABIN_LKP_NUM                                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter   CABIN_UPD_NUM                                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter   CABIN_INV_NUM                                       = 2**CABIN_INV_IDX_WIDTH,
    parameter   BANK_L0_NUM                                         = 2**BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_NUM                                         = 2**BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_NUM                                         = 2**BANK_L2_IDX_WIDTH,
    parameter   BANK_L0_SET_NUM                                     = 2**BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_NUM                                     = 2**BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_NUM                                     = 2**BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L0_WAY_NUM                                     = 2**BANK_L0_WAY_IDX_WIDTH,
    parameter   BANK_L1_WAY_NUM                                     = 2**BANK_L1_WAY_IDX_WIDTH,
    parameter   BANK_L2_WAY_NUM                                     = 2**BANK_L2_WAY_IDX_WIDTH,
    parameter   TECC_WIDTH                                          = 5,
    parameter   LDECC_WIDTH                                         = 9,
    parameter   NLDECC_WIDTH                                        = 6,
    parameter   SPARE_PARAM                                         = 0 
//}}}
)(
//IO {{{
    input  logic                                                                clk,
    input  logic                                                                rstn,
    // LOOKUP                                                                   
    input  logic                                                                lookup_req_valid_i,
    output logic                                                                lookup_req_ready_o,
    input  LOOKUP_REQ_TYPE                                                      lookup_req_i,
    output logic                                                                lookup_ack_valid_o,
    output LOOKUP_ACK_TYPE                                                      lookup_ack_o,
    // UPDATE                                                                   
    input  logic                                                                update_req_valid_i,
    output logic                                                                update_req_ready_o,
    input  UPDATE_REQ_TYPE                                                      update_req_i,
    output logic                                                                update_ack_valid_o,
    output UPDATE_REQ_TYPE                                                      update_ack_o,
    // INV                                                                      
    input  logic                                                                invalid_req_valid_i,
    output logic                                                                invalid_req_ready_o,
    input  INVALID_REQ_TYPE                                                     invalid_req_i,
    output logic                                                                invalid_ack_valid_o,
    output INVALID_REQ_TYPE                                                     invalid_ack_o,
    //                                                                          
    output logic                                                                ram_initial_done_o,
    // CFG                                                                      
    input  logic                                                                multi_hit_check_i,
    output logic                                                                multi_hit_fault_o,
    output logic [1:0]                                                          ecc_err_o,
    //                                                                          
    input  logic                                                                spare_in
//}}}
);
//=== Declare === {{{
    localparam  TRAM_DAT_WIDTH                                     = $bits(MTLB_TAG_TYPE)+TECC_WIDTH+1;
    localparam  IRAM_DAT_WIDTH                                     = $bits(MTLB_ITAG_TYPE)+TECC_WIDTH+1;
    localparam  LDRAM_DAT_WIDTH                                    = $bits(MTLB_DAT_TYPE_L)+LDECC_WIDTH+1;
    localparam  NLDRAM_DAT_WIDTH                                   = $bits(MTLB_DAT_TYPE_NL)+NLDECC_WIDTH+1;

    logic                                                                       ftlb_lookup_req_valid_i;
    logic                                                                       ftlb_lookup_req_ready_o;
    LOOKUP_REQ_TYPE                                                             ftlb_lookup_req_i;
    logic                                                                       ftlb_lookup_ack_valid_o;
    logic                                                                       ftlb_lookup_ack_ready_i;
    LOOKUP_ACK_TYPE                                                             ftlb_lookup_ack_o;
    logic                                                                       ftlb_update_req_valid_i;
    UPDATE_REQ_TYPE                                                             ftlb_update_req_i;
    logic                                                                       ftlb_mtlb_update_req_valid_i;
    logic                                                                       ftlb_mtlb_update_req_ready_o;
    UPDATE_REQ_TYPE                                                             ftlb_mtlb_update_req_i;
    logic                                                                       ftlb_inv_req_valid_i;
    INVALID_REQ_TYPE                                                            ftlb_inv_req_i;
    logic                                                                       ftlb_update_inv_ready_o;
    logic                                                                       ftlb_mtlb_lookup_req_valid_o;
    logic                                                                       ftlb_mtlb_lookup_req_ready_i;
    LOOKUP_REQ_TYPE                                                             ftlb_mtlb_lookup_req_o;
    logic                                                                       ftlb_mtlb_refill_req_valid_o;
    logic                                                                       ftlb_mtlb_refill_req_ready_i;
    UPDATE_REQ_TYPE                                                             ftlb_mtlb_refill_req_o;
    logic                                                                       ftlb_multi_hit_fault_o;
    logic                                                                       mtlb_lookup_req_valid_i;
    logic                                                                       mtlb_lookup_req_ready_o;
    LOOKUP_REQ_TYPE                                                             mtlb_lookup_req_i;
    logic                                                                       mtlb_update_req_valid_i;
    logic                                                                       mtlb_update_req_ready_o;
    UPDATE_REQ_TYPE                                                             mtlb_update_req_i;
    logic                                                                       mtlb_invalid_req_valid_i;
    logic                                                                       mtlb_invalid_req_ready_o;
    INVALID_REQ_TYPE                                                            mtlb_invalid_req_i;
    logic                                                                       mtlb_lookup_ack_valid_o;
    logic                                                                       mtlb_lookup_ack_ready_i;
    UPDATE_REQ_TYPE                                                             mtlb_lookup_ack_o;
    logic                                                                       mtlb_lookup_ack_hit_o;
    logic                                                                       mtlb_update_ack_valid_o;
    UPDATE_REQ_TYPE                                                             mtlb_update_ack_o;
    logic                                                                       mtlb_invalid_ack_valid_o;
    INVALID_REQ_TYPE                                                            mtlb_invalid_ack_o;
    logic                                                                       mtlb_invalid_req_valid_o;
    logic                                                                       mtlb_invalid_req_ready_i;
    INVALID_REQ_TYPE                                                            mtlb_invalid_req_o;
                                                                                
    logic [1:0]                                                                 mtlb_lookup_ack_valid_demux;
    logic [1:0]                                                                 mtlb_lookup_ack_ready_demux;

    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               mtlb_b00_tram_cs_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               mtlb_b00_tram_wr_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]   mtlb_b00_tram_addr_o ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b00_tram_wdata_o;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b00_tram_rdata_i;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               mtlb_b00_iram_cs_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               mtlb_b00_iram_wr_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]   mtlb_b00_iram_addr_o ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b00_iram_wdata_o;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b00_iram_rdata_i;
    logic [BANK_L0_NUM-1:0]                                                     mtlb_b00_uram_cs_o   ;
    logic [BANK_L0_NUM-1:0]                                                     mtlb_b00_uram_wr_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]                         mtlb_b00_uram_addr_o ;
    MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                            mtlb_b00_uram_wdata_o;
    MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                            mtlb_b00_uram_rdata_i;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               mtlb_b00_dram_cs_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                               mtlb_b00_dram_wr_o   ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]   mtlb_b00_dram_addr_o ;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]         mtlb_b00_dram_wdata_o;
    logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [LDRAM_DAT_WIDTH-1:0]         mtlb_b00_dram_rdata_i;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               mtlb_b01_tram_cs_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               mtlb_b01_tram_wr_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]   mtlb_b01_tram_addr_o ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b01_tram_wdata_o;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b01_tram_rdata_i;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               mtlb_b01_iram_cs_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               mtlb_b01_iram_wr_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]   mtlb_b01_iram_addr_o ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b01_iram_wdata_o;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b01_iram_rdata_i;
    logic [BANK_L1_NUM-1:0]                                                     mtlb_b01_uram_cs_o   ;
    logic [BANK_L1_NUM-1:0]                                                     mtlb_b01_uram_wr_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]                         mtlb_b01_uram_addr_o ;
    MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                            mtlb_b01_uram_wdata_o;
    MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                            mtlb_b01_uram_rdata_i;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               mtlb_b01_dram_cs_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                               mtlb_b01_dram_wr_o   ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]   mtlb_b01_dram_addr_o ;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        mtlb_b01_dram_wdata_o;
    logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        mtlb_b01_dram_rdata_i;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               mtlb_b10_tram_cs_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               mtlb_b10_tram_wr_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]   mtlb_b10_tram_addr_o ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b10_tram_wdata_o;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]          mtlb_b10_tram_rdata_i;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               mtlb_b10_iram_cs_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               mtlb_b10_iram_wr_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]   mtlb_b10_iram_addr_o ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b10_iram_wdata_o;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]          mtlb_b10_iram_rdata_i;
    logic [BANK_L2_NUM-1:0]                                                     mtlb_b10_uram_cs_o   ;
    logic [BANK_L2_NUM-1:0]                                                     mtlb_b10_uram_wr_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]                         mtlb_b10_uram_addr_o ;
    MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                            mtlb_b10_uram_wdata_o;
    MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                            mtlb_b10_uram_rdata_i;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               mtlb_b10_dram_cs_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                               mtlb_b10_dram_wr_o   ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]   mtlb_b10_dram_addr_o ;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        mtlb_b10_dram_wdata_o;
    logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]        mtlb_b10_dram_rdata_i;

    logic                                                                       ram_initial_done;

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
    assign ftlb_mtlb_update_req_valid_i = mtlb_lookup_ack_valid_demux[1] & mtlb_lookup_ack_hit_o & mtlb_lookup_ack_o.lvl=='d0;
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
generate
    if(CACHE_TYPE=='d0) begin : ddtc_lookup_ack_con
        assign lookup_ack_valid_o           = mtlb_lookup_ack_valid_demux[0] ? 1'b1                                     : ftlb_lookup_ack_valid_o           ;
        assign lookup_ack_o.idx             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.idx[TLB_QIDX_WIDTH-1:0]: ftlb_lookup_ack_o.idx             ;
        assign lookup_ack_o.lvl             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.lvl                    : ftlb_lookup_ack_o.lvl             ;
        assign lookup_ack_o.hit             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_hit_o                    : ftlb_lookup_ack_o.hit             ;
        assign lookup_ack_o.prefetched      = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.prefetched             : 1'b0                              ;
        assign lookup_ack_o.msi_addr_pattern= mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.msi_addr_pattern       : ftlb_lookup_ack_o.msi_addr_pattern;
        assign lookup_ack_o.msi_addr_mask   = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.msi_addr_mask          : ftlb_lookup_ack_o.msi_addr_mask   ;
        assign lookup_ack_o.msipip_mode     = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.msipip_mode            : ftlb_lookup_ack_o.msipip_mode     ;
        assign lookup_ack_o.msipip_ppn      = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.msipip_ppn             : ftlb_lookup_ack_o.msipip_ppn      ;
        assign lookup_ack_o.fsc_mode        = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.fsc_mode               : ftlb_lookup_ack_o.fsc_mode        ;
        assign lookup_ack_o.fsc_ppn         = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.fsc_ppn                : ftlb_lookup_ack_o.fsc_ppn         ;
        assign lookup_ack_o.PSCID           = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.PSCID                  : ftlb_lookup_ack_o.PSCID           ;
        assign lookup_ack_o.S2MODE          = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.S2MODE                 : ftlb_lookup_ack_o.S2MODE          ;
        assign lookup_ack_o.GSCID           = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.GSCID                  : ftlb_lookup_ack_o.GSCID           ;
        assign lookup_ack_o.S2PPN           = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.S2PPN                  : ftlb_lookup_ack_o.S2PPN           ;
        assign lookup_ack_o.SXL             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.SXL                    : ftlb_lookup_ack_o.SXL             ;
        assign lookup_ack_o.SBE             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.SBE                    : ftlb_lookup_ack_o.SBE             ;
        assign lookup_ack_o.DPE             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.DPE                    : ftlb_lookup_ack_o.DPE             ;
        assign lookup_ack_o.SADE            = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.SADE                   : ftlb_lookup_ack_o.SADE            ;
        assign lookup_ack_o.GADE            = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.GADE                   : ftlb_lookup_ack_o.GADE            ;
        assign lookup_ack_o.PRPR            = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.PRPR                   : ftlb_lookup_ack_o.PRPR            ;
        assign lookup_ack_o.PDTV            = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.PDTV                   : ftlb_lookup_ack_o.PDTV            ;
        assign lookup_ack_o.DTF             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.DTF                    : ftlb_lookup_ack_o.DTF             ;
        assign lookup_ack_o.T2GPA           = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.T2GPA                  : ftlb_lookup_ack_o.T2GPA           ;
        assign lookup_ack_o.EN_PRI          = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.EN_PRI                 : ftlb_lookup_ack_o.EN_PRI          ;
        assign lookup_ack_o.EN_ATS          = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.EN_ATS                 : ftlb_lookup_ack_o.EN_ATS          ;
        assign lookup_ack_o.V               = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.V                      : ftlb_lookup_ack_o.V               ;
        assign mtlb_lookup_ack_ready_demux[0]=1'b1;
    end
    else begin : pdtc_lookup_ack_con
        assign lookup_ack_valid_o           = mtlb_lookup_ack_valid_demux[0] ? 1'b1                                     : ftlb_lookup_ack_valid_o           ;
        assign lookup_ack_o.idx             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.idx[TLB_QIDX_WIDTH-1:0]: ftlb_lookup_ack_o.idx             ;
        assign lookup_ack_o.lvl             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.lvl                    : ftlb_lookup_ack_o.lvl             ;
        assign lookup_ack_o.hit             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_hit_o                    : ftlb_lookup_ack_o.hit             ;
        assign lookup_ack_o.prefetched      = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.prefetched             : 1'b0                              ;
        assign lookup_ack_o.fsc_mode        = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.fsc_mode               : ftlb_lookup_ack_o.fsc_mode        ;
        assign lookup_ack_o.fsc_ppn         = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.fsc_ppn                : ftlb_lookup_ack_o.fsc_ppn         ;
        assign lookup_ack_o.PSCID           = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.PSCID                  : ftlb_lookup_ack_o.PSCID           ;
        assign lookup_ack_o.SUM             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.SUM                    : ftlb_lookup_ack_o.SUM             ;
        assign lookup_ack_o.ENS             = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.ENS                    : ftlb_lookup_ack_o.ENS             ;
        assign lookup_ack_o.V               = mtlb_lookup_ack_valid_demux[0] ? mtlb_lookup_ack_o.V                      : ftlb_lookup_ack_o.V               ;
        assign mtlb_lookup_ack_ready_demux[0]=1'b1;
    end
endgenerate
//}}}

//=== update ack {{{
    assign update_ack_valid_o = mtlb_update_ack_valid_o;
    assign update_ack_o       = mtlb_update_ack_o;
//}}}

//=== invalid ack {{{
    assign invalid_ack_valid_o = mtlb_invalid_ack_valid_o;
    assign invalid_ack_o       = mtlb_invalid_ack_o;
//}}}

//}}}

//=== microTLB inst === {{{
    assign ftlb_lookup_req_valid_i = lookup_req_valid_i & ram_initial_done;
    assign lookup_req_ready_o      = ftlb_lookup_req_ready_o & ram_initial_done;
    assign ftlb_lookup_req_i       = lookup_req_i;
    assign ftlb_update_req_valid_i = 1'b0;
    assign ftlb_update_req_i       = 'd0;

    iommu_atd_dtc_micro_tlb #(
    /*parameter  */ .CACHE_TYPE                 (CACHE_TYPE                     ), // = 0, 0:DDTC, 1:PDTC
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH                 ), // = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE               ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (MICRO_TLB_IDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH,
    /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE                ), // = iommu_acd_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE                ), // = iommu_acd_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE                ), // = iommu_acd_pkg::ddtc_update_req_t,
    /*parameter type         */ .MICROTLB_TAG_TYPE          (MICROTLB_TAG_TYPE              ), // = iommu_atd_cache_pkg::ddtc_microtlb_tag_t,
    /*parameter type         */ .MICROTLB_INV_TAG_TYPE      (MICROTLB_INV_TAG_TYPE          ), // = iommu_atd_cache_pkg::ddtc_microtlb_inv_tag_t,
    /*parameter type         */ .MICROTLB_CONTENT_TYPE      (MICROTLB_CONTENT_TYPE          ), // = iommu_atd_cache_pkg::ddtc_microtlb_content_t,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0
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
    /*input  logic                                      */  .multi_hit_check_i              (multi_hit_check_i              ),
    /*output logic                                      */  .multi_hit_fault_o              (ftlb_multi_hit_fault_o         ),
    /*input  logic                                      */  .spare_i                        (1'b0                           ) 
);
//}}}

//=== mainTLB inst === {{{
    assign mtlb_update_req_valid_i = update_req_valid_i & ram_initial_done;
    assign update_req_ready_o      = mtlb_update_req_ready_o & ram_initial_done;
    assign mtlb_update_req_i       = update_req_i;

    assign mtlb_invalid_req_valid_i= invalid_req_valid_i & ram_initial_done;
    assign invalid_req_ready_o     = mtlb_invalid_req_ready_o & ram_initial_done;
    assign mtlb_invalid_req_i      = invalid_req_i;

    iommu_atd_dtc_main_tlb #(
    /*parameter  */ .CACHE_TYPE                                                 (CACHE_TYPE                     ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter type         */ .INVALID_REQ_TYPE                                           (INVALID_REQ_TYPE               ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                                            (LOOKUP_REQ_TYPE                ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                                            (LOOKUP_ACK_TYPE                ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                                            (UPDATE_REQ_TYPE                ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH                                        (CABIN_LKP_IDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH                                        (CABIN_UPD_IDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    /*parameter  */ .CABIN_INV_IDX_WIDTH                                        (CABIN_INV_IDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    /*parameter  */ .BANK_L0_IDX_WIDTH                                          (BANK_L0_IDX_WIDTH              ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    /*parameter  */ .BANK_L1_IDX_WIDTH                                          (BANK_L1_IDX_WIDTH              ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    /*parameter  */ .BANK_L2_IDX_WIDTH                                          (BANK_L2_IDX_WIDTH              ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    /*parameter  */ .BANK_L0_SET_IDX_WIDTH                                      (BANK_L0_SET_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L1_SET_IDX_WIDTH                                      (BANK_L1_SET_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L2_SET_IDX_WIDTH                                      (BANK_L2_SET_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L0_WAY_IDX_WIDTH                                      (BANK_L0_WAY_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L1_WAY_IDX_WIDTH                                      (BANK_L1_WAY_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L2_WAY_IDX_WIDTH                                      (BANK_L2_WAY_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE                                              (MTLB_TAG_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                                             (MTLB_ITAG_TYPE                 ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                                             (MTLB_UTAG_TYPE                 ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                                              (MTLB_DAT_TYPE_L                ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    /*parameter type         */ .MTLB_DAT_TYPE_NL                                           (MTLB_DAT_TYPE_NL               ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl\
    /*parameter  */ .TECC_WIDTH                                                 (TECC_WIDTH                     ), // = 5,
    /*parameter  */ .LDECC_WIDTH                                                (LDECC_WIDTH                    ), // = 9,
    /*parameter  */ .NLDECC_WIDTH                                               (NLDECC_WIDTH                   ), // = 6,
    /*parameter  */ .SPARE_PARAM                                                (1'b0                           )  // = 0
    ) U_mtlb(
    /*input  logic                                                                      */  .clk                            (clk                            ),
    /*input  logic                                                                      */  .rstn                           (rstn                           ),
    /*input  logic                                                                      */  .lookup_req_valid_i             (mtlb_lookup_req_valid_i        ),
    /*output logic                                                                      */  .lookup_req_ready_o             (mtlb_lookup_req_ready_o        ),
    /*input  LOOKUP_REQ_TYPE                                                            */  .lookup_req_i                   (mtlb_lookup_req_i              ),
    /*input  logic                                                                      */  .update_req_valid_i             (mtlb_update_req_valid_i        ),
    /*output logic                                                                      */  .update_req_ready_o             (mtlb_update_req_ready_o        ),
    /*input  UPDATE_REQ_TYPE                                                            */  .update_req_i                   (mtlb_update_req_i              ),
    /*input  logic                                                                      */  .refill_req_valid_i             (ftlb_mtlb_refill_req_valid_o   ),
    /*output logic                                                                      */  .refill_req_ready_o             (ftlb_mtlb_refill_req_ready_i   ),
    /*input  UPDATE_REQ_TYPE                                                            */  .refill_req_i                   (ftlb_mtlb_refill_req_o         ),
    /*input  logic                                                                      */  .invalid_req_valid_i            (mtlb_invalid_req_valid_i       ),
    /*output logic                                                                      */  .invalid_req_ready_o            (mtlb_invalid_req_ready_o       ),
    /*input  INVALID_REQ_TYPE                                                           */  .invalid_req_i                  (mtlb_invalid_req_i             ),
    /*output logic                                                                      */  .lookup_ack_valid_o             (mtlb_lookup_ack_valid_o        ),
    /*input  logic                                                                      */  .lookup_ack_ready_i             (mtlb_lookup_ack_ready_i        ),
    /*output UPDATE_REQ_TYPE                                                            */  .lookup_ack_o                   (mtlb_lookup_ack_o              ),
    /*output logic                                                                      */  .lookup_ack_hit_o               (mtlb_lookup_ack_hit_o          ),
    /*output logic                                                                      */  .update_ack_valid_o             (mtlb_update_ack_valid_o        ),
    /*output UPDATE_REQ_TYPE                                                            */  .update_ack_o                   (mtlb_update_ack_o              ),       // only idx used by now, other constant 0
    /*output logic                                                                      */  .invalid_ack_valid_o            (mtlb_invalid_ack_valid_o       ),
    /*output INVALID_REQ_TYPE                                                           */  .invalid_ack_o                  (mtlb_invalid_ack_o             ),       // only idx used by now, other constant 0
    /*output logic                                                                      */  .invalid_req_valid_o            (mtlb_invalid_req_valid_o       ),
    /*input  logic                                                                      */  .invalid_req_ready_i            (mtlb_invalid_req_ready_i       ),
    /*output INVALID_REQ_TYPE                                                           */  .invalid_req_o                  (mtlb_invalid_req_o             ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_tram_cs_o                  (mtlb_b00_tram_cs_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_tram_wr_o                  (mtlb_b00_tram_wr_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]  */  .b00_tram_addr_o                (mtlb_b00_tram_addr_o               ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b00_tram_wdata_o               (mtlb_b00_tram_wdata_o              ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b00_tram_rdata_i               (mtlb_b00_tram_rdata_i              ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_iram_cs_o                  (mtlb_b00_iram_cs_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_iram_wr_o                  (mtlb_b00_iram_wr_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]  */  .b00_iram_addr_o                (mtlb_b00_iram_addr_o               ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_wdata_o               (mtlb_b00_iram_wdata_o              ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_rdata_i               (mtlb_b00_iram_rdata_i              ),
    /*output logic [BANK_L0_NUM-1:0]                                                    */  .b00_uram_cs_o                  (mtlb_b00_uram_cs_o                 ),
    /*output logic [BANK_L0_NUM-1:0]                                                    */  .b00_uram_wr_o                  (mtlb_b00_uram_wr_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]                        */  .b00_uram_addr_o                (mtlb_b00_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                           */  .b00_uram_wdata_o               (mtlb_b00_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                           */  .b00_uram_rdata_i               (mtlb_b00_uram_rdata_i              ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_dram_cs_o                  (mtlb_b00_dram_cs_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_dram_wr_o                  (mtlb_b00_dram_wr_o                 ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]  */  .b00_dram_addr_o                (mtlb_b00_dram_addr_o               ),
    /*output MTLB_DAT_TYPE [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                      */  .b00_dram_wdata_o               (mtlb_b00_dram_wdata_o              ),
    /*input  MTLB_DAT_TYPE [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                      */  .b00_dram_rdata_i               (mtlb_b00_dram_rdata_i              ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_tram_cs_o                  (mtlb_b01_tram_cs_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_tram_wr_o                  (mtlb_b01_tram_wr_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]  */  .b01_tram_addr_o                (mtlb_b01_tram_addr_o               ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b01_tram_wdata_o               (mtlb_b01_tram_wdata_o              ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b01_tram_rdata_i               (mtlb_b01_tram_rdata_i              ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_iram_cs_o                  (mtlb_b01_iram_cs_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_iram_wr_o                  (mtlb_b01_iram_wr_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]  */  .b01_iram_addr_o                (mtlb_b01_iram_addr_o               ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_wdata_o               (mtlb_b01_iram_wdata_o              ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_rdata_i               (mtlb_b01_iram_rdata_i              ),
    /*output logic [BANK_L1_NUM-1:0]                                                    */  .b01_uram_cs_o                  (mtlb_b01_uram_cs_o                 ),
    /*output logic [BANK_L1_NUM-1:0]                                                    */  .b01_uram_wr_o                  (mtlb_b01_uram_wr_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]                        */  .b01_uram_addr_o                (mtlb_b01_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                           */  .b01_uram_wdata_o               (mtlb_b01_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                           */  .b01_uram_rdata_i               (mtlb_b01_uram_rdata_i              ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_dram_cs_o                  (mtlb_b01_dram_cs_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_dram_wr_o                  (mtlb_b01_dram_wr_o                 ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]  */  .b01_dram_addr_o                (mtlb_b01_dram_addr_o               ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b01_dram_wdata_o               (mtlb_b01_dram_wdata_o              ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b01_dram_rdata_i               (mtlb_b01_dram_rdata_i              ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_tram_cs_o                  (mtlb_b10_tram_cs_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_tram_wr_o                  (mtlb_b10_tram_wr_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]  */  .b10_tram_addr_o                (mtlb_b10_tram_addr_o               ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b10_tram_wdata_o               (mtlb_b10_tram_wdata_o              ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b10_tram_rdata_i               (mtlb_b10_tram_rdata_i              ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_iram_cs_o                  (mtlb_b10_iram_cs_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_iram_wr_o                  (mtlb_b10_iram_wr_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]  */  .b10_iram_addr_o                (mtlb_b10_iram_addr_o               ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_wdata_o               (mtlb_b10_iram_wdata_o              ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_rdata_i               (mtlb_b10_iram_rdata_i              ),
    /*output logic [BANK_L2_NUM-1:0]                                                    */  .b10_uram_cs_o                  (mtlb_b10_uram_cs_o                 ),
    /*output logic [BANK_L2_NUM-1:0]                                                    */  .b10_uram_wr_o                  (mtlb_b10_uram_wr_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]                        */  .b10_uram_addr_o                (mtlb_b10_uram_addr_o               ),
    /*output MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                           */  .b10_uram_wdata_o               (mtlb_b10_uram_wdata_o              ),
    /*input  MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                           */  .b10_uram_rdata_i               (mtlb_b10_uram_rdata_i              ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_dram_cs_o                  (mtlb_b10_dram_cs_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_dram_wr_o                  (mtlb_b10_dram_wr_o                 ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]  */  .b10_dram_addr_o                (mtlb_b10_dram_addr_o               ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b10_dram_wdata_o               (mtlb_b10_dram_wdata_o              ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b10_dram_rdata_i               (mtlb_b10_dram_rdata_i              ),
    /*input  logic                                                                      */  .multi_hit_check_i              (multi_hit_check_i                  ),
    /*output logic [3:0]                                                                */  .multi_hit_fault_o              (                                   ),
    /*output logic [1:0]                                                                */  .ecc_err_o                      (ecc_err_o                          ),
    /*input  logic                                                                      */  .spare_in                       (1'b0                               )
    );

//}}}

//=== MTLB RAM inst {{{
    iommu_atd_dtc_mtlb_ram_wrap #(
    /*parameter  */ .BANK_L0_IDX_WIDTH                                          (BANK_L0_IDX_WIDTH              ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    /*parameter  */ .BANK_L1_IDX_WIDTH                                          (BANK_L1_IDX_WIDTH              ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    /*parameter  */ .BANK_L2_IDX_WIDTH                                          (BANK_L2_IDX_WIDTH              ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    /*parameter  */ .BANK_L0_SET_IDX_WIDTH                                      (BANK_L0_SET_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L1_SET_IDX_WIDTH                                      (BANK_L1_SET_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L2_SET_IDX_WIDTH                                      (BANK_L2_SET_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L0_WAY_IDX_WIDTH                                      (BANK_L0_WAY_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L1_WAY_IDX_WIDTH                                      (BANK_L1_WAY_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L2_WAY_IDX_WIDTH                                      (BANK_L2_WAY_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter type         */ .MTLB_TAG_TYPE                                              (MTLB_TAG_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                                             (MTLB_ITAG_TYPE                 ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                                             (MTLB_UTAG_TYPE                 ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                                              (MTLB_DAT_TYPE_L                ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    /*parameter type         */ .MTLB_DAT_TYPE_NL                                           (MTLB_DAT_TYPE_NL               ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl,
    /*parameter  */ .TECC_WIDTH                                                 (TECC_WIDTH                     ), // = 5,
    /*parameter  */ .LDECC_WIDTH                                                (LDECC_WIDTH                    ), // = 9,
    /*parameter  */ .NLDECC_WIDTH                                               (NLDECC_WIDTH                   ), // = 6,
    /*parameter  */ .SPARE_PARAM                                                (1'b0                           )  // = 0
    ) U_mtlb_ram(                                                                           
    /*input  logic                                                                      */  .clk                            (clk                    ),
    /*input  logic                                                                      */  .rstn                           (rstn                   ),
    /*output logic                                                                      */  .ram_initial_done_o             (ram_initial_done       ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_tram_cs_i          (mtlb_b00_tram_cs_o     ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_tram_wr_i          (mtlb_b00_tram_wr_o     ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]  */  .b00_tram_addr_i        (mtlb_b00_tram_addr_o   ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b00_tram_wdata_i       (mtlb_b00_tram_wdata_o  ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b00_tram_rdata_o       (mtlb_b00_tram_rdata_i  ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_iram_cs_i          (mtlb_b00_iram_cs_o     ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_iram_wr_i          (mtlb_b00_iram_wr_o     ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]  */  .b00_iram_addr_i        (mtlb_b00_iram_addr_o   ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_wdata_i       (mtlb_b00_iram_wdata_o  ),
    /*output logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b00_iram_rdata_o       (mtlb_b00_iram_rdata_i  ),
    /*input  logic [BANK_L0_NUM-1:0]                                                    */  .b00_uram_cs_i          (mtlb_b00_uram_cs_o     ),
    /*input  logic [BANK_L0_NUM-1:0]                                                    */  .b00_uram_wr_i          (mtlb_b00_uram_wr_o     ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]                        */  .b00_uram_addr_i        (mtlb_b00_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                           */  .b00_uram_wdata_i       (mtlb_b00_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_L0_NUM-1:0]                                           */  .b00_uram_rdata_o       (mtlb_b00_uram_rdata_i  ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_dram_cs_i          (mtlb_b00_dram_cs_o     ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                              */  .b00_dram_wr_i          (mtlb_b00_dram_wr_o     ),
    /*input  logic [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0] [BANK_L0_SET_IDX_WIDTH-1:0]  */  .b00_dram_addr_i        (mtlb_b00_dram_addr_o   ),
    /*input  MTLB_DAT_TYPE [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                      */  .b00_dram_wdata_i       (mtlb_b00_dram_wdata_o  ),
    /*output MTLB_DAT_TYPE [BANK_L0_NUM-1:0] [BANK_L0_WAY_NUM-1:0]                      */  .b00_dram_rdata_o       (mtlb_b00_dram_rdata_i  ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_tram_cs_i          (mtlb_b01_tram_cs_o     ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_tram_wr_i          (mtlb_b01_tram_wr_o     ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]  */  .b01_tram_addr_i        (mtlb_b01_tram_addr_o   ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b01_tram_wdata_i       (mtlb_b01_tram_wdata_o  ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b01_tram_rdata_o       (mtlb_b01_tram_rdata_i  ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_iram_cs_i          (mtlb_b01_iram_cs_o     ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_iram_wr_i          (mtlb_b01_iram_wr_o     ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]  */  .b01_iram_addr_i        (mtlb_b01_iram_addr_o   ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_wdata_i       (mtlb_b01_iram_wdata_o  ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b01_iram_rdata_o       (mtlb_b01_iram_rdata_i  ),
    /*input  logic [BANK_L1_NUM-1:0]                                                    */  .b01_uram_cs_i          (mtlb_b01_uram_cs_o     ),
    /*input  logic [BANK_L1_NUM-1:0]                                                    */  .b01_uram_wr_i          (mtlb_b01_uram_wr_o     ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]                        */  .b01_uram_addr_i        (mtlb_b01_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                           */  .b01_uram_wdata_i       (mtlb_b01_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_L1_NUM-1:0]                                           */  .b01_uram_rdata_o       (mtlb_b01_uram_rdata_i  ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_dram_cs_i          (mtlb_b01_dram_cs_o     ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0]                              */  .b01_dram_wr_i          (mtlb_b01_dram_wr_o     ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [BANK_L1_SET_IDX_WIDTH-1:0]  */  .b01_dram_addr_i        (mtlb_b01_dram_addr_o   ),
    /*input  logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b01_dram_wdata_i       (mtlb_b01_dram_wdata_o  ),
    /*output logic [BANK_L1_NUM-1:0] [BANK_L1_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b01_dram_rdata_o       (mtlb_b01_dram_rdata_i  ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_tram_cs_i          (mtlb_b10_tram_cs_o     ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_tram_wr_i          (mtlb_b10_tram_wr_o     ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]  */  .b10_tram_addr_i        (mtlb_b10_tram_addr_o   ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b10_tram_wdata_i       (mtlb_b10_tram_wdata_o  ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         */  .b10_tram_rdata_o       (mtlb_b10_tram_rdata_i  ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_iram_cs_i          (mtlb_b10_iram_cs_o     ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_iram_wr_i          (mtlb_b10_iram_wr_o     ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]  */  .b10_iram_addr_i        (mtlb_b10_iram_addr_o   ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_wdata_i       (mtlb_b10_iram_wdata_o  ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         */  .b10_iram_rdata_o       (mtlb_b10_iram_rdata_i  ),
    /*input  logic [BANK_L2_NUM-1:0]                                                    */  .b10_uram_cs_i          (mtlb_b10_uram_cs_o     ),
    /*input  logic [BANK_L2_NUM-1:0]                                                    */  .b10_uram_wr_i          (mtlb_b10_uram_wr_o     ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]                        */  .b10_uram_addr_i        (mtlb_b10_uram_addr_o   ),
    /*input  MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                           */  .b10_uram_wdata_i       (mtlb_b10_uram_wdata_o  ),
    /*output MTLB_UTAG_TYPE [BANK_L2_NUM-1:0]                                           */  .b10_uram_rdata_o       (mtlb_b10_uram_rdata_i  ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_dram_cs_i          (mtlb_b10_dram_cs_o     ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0]                              */  .b10_dram_wr_i          (mtlb_b10_dram_wr_o     ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [BANK_L2_SET_IDX_WIDTH-1:0]  */  .b10_dram_addr_i        (mtlb_b10_dram_addr_o   ),
    /*input  logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b10_dram_wdata_i       (mtlb_b10_dram_wdata_o  ),
    /*output logic [BANK_L2_NUM-1:0] [BANK_L2_WAY_NUM-1:0] [NLDRAM_DAT_WIDTH-1:0]       */  .b10_dram_rdata_o       (mtlb_b10_dram_rdata_i  ),
    /*input  logic                                                                      */  .spare_in                       (1'b0                   )
    );

//}}}
endmodule




