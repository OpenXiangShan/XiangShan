module integration_example #(
    parameter  INV_FIFO_IDX_WIDTH           = iommu_atd_cache_pkg::INV_IDX_WIDTH                ,
    parameter  DDTC_TLB_QIDX_WIDTH          = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH          ,
    parameter  DDTC_MICRO_TLB_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH     ,
    parameter  DDTC_CABIN_LKP_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH     ,
    parameter  DDTC_CABIN_UPD_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH     ,
    parameter  DDTC_CABIN_INV_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH     ,
    parameter  DDTC_BANK_L0_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH       ,
    parameter  DDTC_BANK_L1_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH       ,
    parameter  DDTC_BANK_L2_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH       ,
    parameter  DDTC_BANK_L0_SET_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH   ,
    parameter  DDTC_BANK_L1_SET_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH   ,
    parameter  DDTC_BANK_L2_SET_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH   ,
    parameter  DDTC_BANK_L0_WAY_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH   ,
    parameter  DDTC_BANK_L1_WAY_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH   ,
    parameter  DDTC_BANK_L2_WAY_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH   ,
    parameter  PDTC_TLB_QIDX_WIDTH          = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH          ,
    parameter  PDTC_MICRO_TLB_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH     ,
    parameter  PDTC_CABIN_LKP_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH     ,
    parameter  PDTC_CABIN_UPD_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH     ,
    parameter  PDTC_CABIN_INV_IDX_WIDTH     = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH     ,
    parameter  PDTC_BANK_L0_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH       ,
    parameter  PDTC_BANK_L1_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH       ,
    parameter  PDTC_BANK_L2_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH       ,
    parameter  PDTC_BANK_L0_SET_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH   ,
    parameter  PDTC_BANK_L1_SET_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH   ,
    parameter  PDTC_BANK_L2_SET_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH   ,
    parameter  PDTC_BANK_L0_WAY_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH   ,
    parameter  PDTC_BANK_L1_WAY_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH   ,
    parameter  PDTC_BANK_L2_WAY_IDX_WIDTH   = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH   ,
    parameter  S1PTC_TLB_QIDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH         ,
    parameter  S1PTC_MICRO_TLB_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH    ,
    parameter  S1PTC_CABIN_LKP_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH    ,
    parameter  S1PTC_CABIN_UPD_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH    ,
    parameter  S1PTC_CABIN_INV_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH    ,
    parameter  S1PTC_BANK_L0_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH      ,
    parameter  S1PTC_BANK_L1_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH      ,
    parameter  S1PTC_BANK_L2_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH      ,
    parameter  S1PTC_BANK_L3_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH      ,
    parameter  S1PTC_BANK_L4_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH      ,
    parameter  S1PTC_BANK_L0_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L1_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L2_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L3_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L4_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L0_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L1_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L2_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L3_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH  ,
    parameter  S1PTC_BANK_L4_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH  ,
    parameter  S2PTC_TLB_QIDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH         ,
    parameter  S2PTC_MICRO_TLB_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH    ,
    parameter  S2PTC_CABIN_LKP_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH    ,
    parameter  S2PTC_CABIN_UPD_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH    ,
    parameter  S2PTC_CABIN_INV_IDX_WIDTH    = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH    ,
    parameter  S2PTC_BANK_L0_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH      ,
    parameter  S2PTC_BANK_L1_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH      ,
    parameter  S2PTC_BANK_L2_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH      ,
    parameter  S2PTC_BANK_L3_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH      ,
    parameter  S2PTC_BANK_L4_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH      ,
    parameter  S2PTC_BANK_L0_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L1_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L2_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L3_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L4_SET_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L0_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L1_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L2_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L3_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH  ,
    parameter  S2PTC_BANK_L4_WAY_IDX_WIDTH  = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH  
)(
    input  logic                                    clk                            ,
    input  logic                                    rstn                           ,
    input  logic                                    invalid_req_valid_i            ,
    output logic                                    invalid_req_ready_o            ,
    input  logic [127:0]                            invalid_req_i                  ,
    input  logic                                    invalid_req_fence_i            ,
    output logic                                    cmd_fifo_empty_o               ,

    input  logic                                    ddtc_lookup_req_valid_i        ,
    output logic                                    ddtc_lookup_req_ready_o        ,
    input  logic [DDTC_TLB_QIDX_WIDTH-1:0]          ddtc_lookup_req_idx_i          ,
    input  logic [23:0]                             ddtc_lookup_req_device_id_i    ,
    output logic                                    ddtc_lookup_ack_valid_o        ,
    output logic                                    ddtc_lookup_ack_hit_o          ,
    output logic [DDTC_TLB_QIDX_WIDTH-1:0]          ddtc_lookup_ack_idx_o          ,
    output logic                                    ddtc_lookup_ack_prefetched_o   ,
    output logic [1:0]                              ddtc_lookup_ack_lvl_o          ,
    output logic [511:0]                            ddtc_lookup_ack_o              ,
    input  logic                                    ddtc_update_req_valid_i        ,
    output logic                                    ddtc_update_req_ready_o        ,
    input  logic [DDTC_TLB_QIDX_WIDTH-1:0]          ddtc_update_req_idx_i          ,
    input  logic [1:0]                              ddtc_update_req_lvl_i          ,
    input  logic                                    ddtc_update_req_prefetched_i   ,
    input  logic [511:0]                            ddtc_update_req_i              ,
    input  logic [23:0]                             ddtc_update_req_device_id_i    ,
    output logic                                    ddtc_update_ack_valid_o        ,
    output logic [DDTC_TLB_QIDX_WIDTH-1:0]          ddtc_update_ack_idx_o          ,

    input  logic                                    pdtc_lookup_req_valid_i        ,
    output logic                                    pdtc_lookup_req_ready_o        ,
    input  logic [PDTC_TLB_QIDX_WIDTH-1:0]          pdtc_lookup_req_idx_i          ,
    input  logic [23:0]                             pdtc_lookup_req_device_id_i    ,
    input  logic [19:0]                             pdtc_lookup_req_process_id_i   ,
    output logic                                    pdtc_lookup_ack_valid_o        ,
    output logic                                    pdtc_lookup_ack_hit_o          ,
    output logic [PDTC_TLB_QIDX_WIDTH-1:0]          pdtc_lookup_ack_idx_o          ,
    output logic                                    pdtc_lookup_ack_prefetched_o   ,
    output logic [1:0]                              pdtc_lookup_ack_lvl_o          ,
    output logic [127:0]                            pdtc_lookup_ack_o              ,
    input  logic                                    pdtc_update_req_valid_i        ,
    output logic                                    pdtc_update_req_ready_o        ,
    input  logic [PDTC_TLB_QIDX_WIDTH-1:0]          pdtc_update_req_idx_i          ,
    input  logic [1:0]                              pdtc_update_req_lvl_i          ,
    input  logic                                    pdtc_update_req_prefetched_i   ,
    input  logic [127:0]                            pdtc_update_req_i              ,
    input  logic [23:0]                             pdtc_update_req_device_id_i    ,
    input  logic [19:0]                             pdtc_update_req_process_id_i   ,
    output logic                                    pdtc_update_ack_valid_o        ,
    output logic [PDTC_TLB_QIDX_WIDTH-1:0]          pdtc_update_ack_idx_o          ,

    input  logic                                    s1ptc_lookup_req_valid_i       ,
    output logic                                    s1ptc_lookup_req_ready_o       ,
    input  logic [S1PTC_TLB_QIDX_WIDTH-1:0]         s1ptc_lookup_req_idx_i         ,
    input  logic                                    s1ptc_lookup_req_gv_i           ,
    input  logic [15:0]                             s1ptc_lookup_req_gscid_i       ,
    input  logic [19:0]                             s1ptc_lookup_req_pscid_i       ,
    input  logic [63:12]                            s1ptc_lookup_req_va_i          ,
    output logic                                    s1ptc_lookup_ack_valid_o       ,
    output logic [S1PTC_TLB_QIDX_WIDTH-1:0]         s1ptc_lookup_ack_idx_o         ,
    output logic                                    s1ptc_lookup_ack_hit_o         ,
    output logic [2:0]                              s1ptc_lookup_ack_lvl_o         ,
    output logic                                    s1ptc_lookup_ack_prefetched_o  ,
    output logic [511:0]                            s1ptc_lookup_ack_o             ,
    output logic [63:0]                             s1ptc_lookup_ack_svnapot_o     ,
    input  logic                                    s1ptc_update_req_valid_i       ,
    output logic                                    s1ptc_update_req_ready_o       ,
    input  logic [S1PTC_TLB_QIDX_WIDTH-1:0]         s1ptc_update_req_idx_i         ,
    input  logic [2:0]                              s1ptc_update_req_lvl_i         ,
    input  logic                                    s1ptc_update_req_prefetched_i  ,
    input  logic [511:0]                            s1ptc_update_req_i             ,
    input  logic [63:0]                             s1ptc_update_req_svnapot_i     ,
    input  logic [19:0]                             s1ptc_update_req_pscid_i       ,
    input  logic                                    s1ptc_update_req_gv_i          ,
    input  logic [15:0]                             s1ptc_update_req_gscid_i       ,
    input  logic [63:12]                            s1ptc_update_req_va_i          ,
    input  logic                                    s1ptc_update_req_sxl_i         ,
    output logic                                    s1ptc_update_ack_valid_o       ,
    output logic [S1PTC_TLB_QIDX_WIDTH-1:0]         s1ptc_update_ack_idx_o         ,

    input  logic                                    s2ptc_lookup_req_valid_i       ,
    output logic                                    s2ptc_lookup_req_ready_o       ,
    input  logic [S2PTC_TLB_QIDX_WIDTH-1:0]         s2ptc_lookup_req_idx_i         ,
    input  logic [15:0]                             s2ptc_lookup_req_gscid_i       ,
    input  logic [63:12]                            s2ptc_lookup_req_gpa_i         ,
    output logic                                    s2ptc_lookup_ack_valid_o       ,
    output logic [S2PTC_TLB_QIDX_WIDTH-1:0]         s2ptc_lookup_ack_idx_o         ,
    output logic                                    s2ptc_lookup_ack_hit_o         ,
    output logic [2:0]                              s2ptc_lookup_ack_lvl_o         ,
    output logic                                    s2ptc_lookup_ack_prefetched_o  ,
    output logic [511:0]                            s2ptc_lookup_ack_o             ,
    output logic [63:0]                             s2ptc_lookup_ack_svnapot_o     ,
    input  logic                                    s2ptc_update_req_valid_i       ,
    output logic                                    s2ptc_update_req_ready_o       ,
    input  logic [S2PTC_TLB_QIDX_WIDTH-1:0]         s2ptc_update_req_idx_i         ,
    input  logic [2:0]                              s2ptc_update_req_lvl_i         ,
    input  logic                                    s2ptc_update_req_prefetched_i  ,
    input  logic [511:0]                            s2ptc_update_req_i             ,
    input  logic [63:0]                             s2ptc_update_req_svnapot_i     ,
    input  logic [15:0]                             s2ptc_update_req_gscid_i       ,
    input  logic [63:12]                            s2ptc_update_req_gpa_i         ,
    output logic                                    s2ptc_update_ack_valid_o       ,
    output logic [S2PTC_TLB_QIDX_WIDTH-1:0]         s2ptc_update_ack_idx_o         ,
    input  logic                                    csr_fctl_gxl_i                  
);

    logic [3:0]                                     inv_req_valid_o    ;
    logic [3:0]                                     inv_req_ready_i    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  inv_req_idx_o      ;
    logic [1:0]                                     inv_req_itype_o    ;        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    logic                                           inv_req_dv_gv_o    ;
    logic [23:0]                                    inv_req_did_gscid_o;
    logic                                           inv_req_pscv_o     ;
    logic [19:0]                                    inv_req_pid_pscid_o;
    logic                                           inv_req_av_o       ;
    logic [63:12]                                   inv_req_addr_o     ;
    logic [3:0]                                     inv_ack_valid_i    ;
    logic [3:0] [INV_FIFO_IDX_WIDTH-1:0]            inv_ack_idx_i      ;
                                                    
    logic                                           ddtc_invalid_req_valid_i    ;
    logic                                           ddtc_invalid_req_ready_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  ddtc_invalid_req_idx_i      ;
    logic [1:0]                                     ddtc_invalid_req_type_i     ;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    logic                                           ddtc_invalid_req_dv_gv_i    ;
    logic [23:0]                                    ddtc_invalid_req_did_gscid_i;
    logic                                           ddtc_invalid_req_pscv_i     ;
    logic [19:0]                                    ddtc_invalid_req_pid_pscid_i;
    logic                                           ddtc_invalid_req_av_i       ;
    logic [63:12]                                   ddtc_invalid_req_addr_i     ;
    logic                                           ddtc_invalid_ack_valid_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  ddtc_invalid_ack_idx_o      ;

    logic                                           pdtc_invalid_req_valid_i    ;
    logic                                           pdtc_invalid_req_ready_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  pdtc_invalid_req_idx_i      ;
    logic [1:0]                                     pdtc_invalid_req_type_i     ;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    logic                                           pdtc_invalid_req_dv_gv_i    ;
    logic [23:0]                                    pdtc_invalid_req_did_gscid_i;
    logic                                           pdtc_invalid_req_pscv_i     ;
    logic [19:0]                                    pdtc_invalid_req_pid_pscid_i;
    logic                                           pdtc_invalid_req_av_i       ;
    logic [63:12]                                   pdtc_invalid_req_addr_i     ;
    logic                                           pdtc_invalid_ack_valid_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  pdtc_invalid_ack_idx_o      ;

    logic                                           s1ptc_invalid_req_valid_i    ;
    logic                                           s1ptc_invalid_req_ready_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  s1ptc_invalid_req_idx_i      ;
    logic [1:0]                                     s1ptc_invalid_req_type_i     ;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    logic                                           s1ptc_invalid_req_dv_gv_i    ;
    logic [23:0]                                    s1ptc_invalid_req_did_gscid_i;
    logic                                           s1ptc_invalid_req_pscv_i     ;
    logic [19:0]                                    s1ptc_invalid_req_pid_pscid_i;
    logic                                           s1ptc_invalid_req_av_i       ;
    logic [63:12]                                   s1ptc_invalid_req_addr_i     ;
    logic                                           s1ptc_invalid_ack_valid_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  s1ptc_invalid_ack_idx_o      ;

    logic                                           s2ptc_invalid_req_valid_i    ;
    logic                                           s2ptc_invalid_req_ready_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  s2ptc_invalid_req_idx_i      ;
    logic [1:0]                                     s2ptc_invalid_req_type_i     ;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    logic                                           s2ptc_invalid_req_dv_gv_i    ;
    logic [23:0]                                    s2ptc_invalid_req_did_gscid_i;
    logic                                           s2ptc_invalid_req_pscv_i     ;
    logic [19:0]                                    s2ptc_invalid_req_pid_pscid_i;
    logic                                           s2ptc_invalid_req_av_i       ;
    logic [63:12]                                   s2ptc_invalid_req_addr_i     ;
    logic                                           s2ptc_invalid_ack_valid_o    ;
    logic [INV_FIFO_IDX_WIDTH-1:0]                  s2ptc_invalid_ack_idx_o      ;




    assign ddtc_invalid_req_valid_i     = inv_req_valid_o[0];
    assign pdtc_invalid_req_valid_i     = inv_req_valid_o[1];
    assign s1ptc_invalid_req_valid_i    = inv_req_valid_o[2];
    assign s2ptc_invalid_req_valid_i    = inv_req_valid_o[3];
    assign inv_req_ready_i              = {
                                            s2ptc_invalid_req_ready_o,
                                            s1ptc_invalid_req_ready_o,
                                            pdtc_invalid_req_ready_o,
                                            ddtc_invalid_req_ready_o
                                          };
    assign inv_ack_valid_i              = {
                                            s2ptc_invalid_ack_valid_o,
                                            s1ptc_invalid_ack_valid_o,
                                            pdtc_invalid_ack_valid_o,
                                            ddtc_invalid_ack_valid_o
                                          };
    assign inv_ack_idx_i                = {
                                            s2ptc_invalid_ack_idx_o,
                                            s1ptc_invalid_ack_idx_o,
                                            pdtc_invalid_ack_idx_o,
                                            ddtc_invalid_ack_idx_o
                                          };

    assign ddtc_invalid_req_idx_i       = inv_req_idx_o;
    assign ddtc_invalid_req_type_i      = inv_req_itype_o;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    assign ddtc_invalid_req_dv_gv_i     = inv_req_dv_gv_o;
    assign ddtc_invalid_req_did_gscid_i = inv_req_did_gscid_o;
    assign ddtc_invalid_req_pscv_i      = inv_req_pscv_o;
    assign ddtc_invalid_req_pid_pscid_i = inv_req_pid_pscid_o;
    assign ddtc_invalid_req_av_i        = inv_req_av_o;
    assign ddtc_invalid_req_addr_i      = inv_req_addr_o;

    assign pdtc_invalid_req_idx_i       = inv_req_idx_o;
    assign pdtc_invalid_req_type_i      = inv_req_itype_o;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    assign pdtc_invalid_req_dv_gv_i     = inv_req_dv_gv_o;
    assign pdtc_invalid_req_did_gscid_i = inv_req_did_gscid_o;
    assign pdtc_invalid_req_pscv_i      = inv_req_pscv_o;
    assign pdtc_invalid_req_pid_pscid_i = inv_req_pid_pscid_o;
    assign pdtc_invalid_req_av_i        = inv_req_av_o;
    assign pdtc_invalid_req_addr_i      = inv_req_addr_o;

    assign s1ptc_invalid_req_idx_i      = inv_req_idx_o;
    assign s1ptc_invalid_req_type_i     = inv_req_itype_o;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    assign s1ptc_invalid_req_dv_gv_i    = inv_req_dv_gv_o;
    assign s1ptc_invalid_req_did_gscid_i= inv_req_did_gscid_o;
    assign s1ptc_invalid_req_pscv_i     = inv_req_pscv_o;
    assign s1ptc_invalid_req_pid_pscid_i= inv_req_pid_pscid_o;
    assign s1ptc_invalid_req_av_i       = inv_req_av_o;
    assign s1ptc_invalid_req_addr_i     = inv_req_addr_o;

    assign s2ptc_invalid_req_idx_i      = inv_req_idx_o;
    assign s2ptc_invalid_req_type_i     = inv_req_itype_o;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    assign s2ptc_invalid_req_dv_gv_i    = inv_req_dv_gv_o;
    assign s2ptc_invalid_req_did_gscid_i= inv_req_did_gscid_o;
    assign s2ptc_invalid_req_pscv_i     = inv_req_pscv_o;
    assign s2ptc_invalid_req_pid_pscid_i= inv_req_pid_pscid_o;
    assign s2ptc_invalid_req_av_i       = inv_req_av_o;
    assign s2ptc_invalid_req_addr_i     = inv_req_addr_o;


    iommu_atd_inv_top #(
    /*parameter  */ .FIFO_IDX_WIDTH             (INV_FIFO_IDX_WIDTH             ), // = 5, //iommu_atd_cache_pkg::INV_IDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0
    ) U_inv(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .invalid_req_valid_i            (invalid_req_valid_i            ),
    /*output logic                                      */  .invalid_req_ready_o            (invalid_req_ready_o            ),
    /*input  logic [127:0]                              */  .invalid_req_i                  (invalid_req_i                  ),
    /*input  logic                                      */  .invalid_req_fence_i            (invalid_req_fence_i            ),
    /*output logic [3:0]                                */  .inv_req_valid_o                (inv_req_valid_o                ),
    /*input  logic [3:0]                                */  .inv_req_ready_i                (inv_req_ready_i                ),
    /*output logic [FIFO_IDX_WIDTH-1:0]                 */  .inv_req_idx_o                  (inv_req_idx_o                  ),
    /*output logic [2:0]                                */  .inv_req_itype_o                (inv_req_itype_o                ),        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    /*output logic                                      */  .inv_req_dv_gv_o                (inv_req_dv_gv_o                ),
    /*output logic [23:0]                               */  .inv_req_did_gscid_o            (inv_req_did_gscid_o            ),
    /*output logic                                      */  .inv_req_pscv_o                 (inv_req_pscv_o                 ),
    /*output logic [19:0]                               */  .inv_req_pid_pscid_o            (inv_req_pid_pscid_o            ),
    /*output logic                                      */  .inv_req_av_o                   (inv_req_av_o                   ),
    /*output logic [63:12]                              */  .inv_req_addr_o                 (inv_req_addr_o                 ),
    /*input  logic [3:0]                                */  .inv_ack_valid_i                (inv_ack_valid_i                ),
    /*input  logic [3:0] [FIFO_IDX_WIDTH-1:0]           */  .inv_ack_idx_i                  (inv_ack_idx_i                  ),
    /*output logic                                      */  .cmd_fifo_empty_o               (cmd_fifo_empty_o               ),
    /*input  logic                                      */  .spare_in                       (1'b0                           ) 
    );



    iommu_atd_ddtc_wrap #(
    /*parameter  */ .INV_IDX_WIDTH              (INV_FIFO_IDX_WIDTH             ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    /*parameter  */ .TLB_QIDX_WIDTH             (DDTC_TLB_QIDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (DDTC_MICRO_TLB_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH        (DDTC_CABIN_LKP_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH        (DDTC_CABIN_UPD_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    /*parameter  */ .CABIN_INV_IDX_WIDTH        (DDTC_CABIN_INV_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    /*parameter  */ .BANK_L0_IDX_WIDTH          (DDTC_BANK_L0_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    /*parameter  */ .BANK_L1_IDX_WIDTH          (DDTC_BANK_L1_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    /*parameter  */ .BANK_L2_IDX_WIDTH          (DDTC_BANK_L2_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    /*parameter  */ .BANK_L0_SET_IDX_WIDTH      (DDTC_BANK_L0_SET_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L1_SET_IDX_WIDTH      (DDTC_BANK_L1_SET_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L2_SET_IDX_WIDTH      (DDTC_BANK_L2_SET_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L0_WAY_IDX_WIDTH      (DDTC_BANK_L0_WAY_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L1_WAY_IDX_WIDTH      (DDTC_BANK_L1_WAY_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L2_WAY_IDX_WIDTH      (DDTC_BANK_L2_WAY_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0
    ) U_ddtc(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .invalid_req_valid_i            (ddtc_invalid_req_valid_i       ),
    /*output logic                                      */  .invalid_req_ready_o            (ddtc_invalid_req_ready_o       ),
    /*input  logic [INV_IDX_WIDTH-1:0]                  */  .invalid_req_idx_i              (ddtc_invalid_req_idx_i         ),
    /*input  logic [1:0]                                */  .invalid_req_type_i             (ddtc_invalid_req_type_i        ),      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    /*input  logic                                      */  .invalid_req_dv_gv_i            (ddtc_invalid_req_dv_gv_i       ),
    /*input  logic [23:0]                               */  .invalid_req_did_gscid_i        (ddtc_invalid_req_did_gscid_i   ),
    /*input  logic                                      */  .invalid_req_pscv_i             (ddtc_invalid_req_pscv_i        ),
    /*input  logic [19:0]                               */  .invalid_req_pid_pscid_i        (ddtc_invalid_req_pid_pscid_i   ),
    /*input  logic                                      */  .invalid_req_av_i               (ddtc_invalid_req_av_i          ),
    /*input  logic [63:12]                              */  .invalid_req_addr_i             (ddtc_invalid_req_addr_i        ),
    /*output logic                                      */  .invalid_ack_valid_o            (ddtc_invalid_ack_valid_o       ),
    /*output logic [INV_IDX_WIDTH-1:0]                  */  .invalid_ack_idx_o              (ddtc_invalid_ack_idx_o         ),
    /*input  logic                                      */  .lookup_req_valid_i             (ddtc_lookup_req_valid_i        ),
    /*output logic                                      */  .lookup_req_ready_o             (ddtc_lookup_req_ready_o        ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_req_idx_i               (ddtc_lookup_req_idx_i          ),
    /*input  logic [23:0]                               */  .lookup_req_device_id_i         (ddtc_lookup_req_device_id_i    ),
    /*output logic                                      */  .lookup_ack_valid_o             (ddtc_lookup_ack_valid_o        ),
    /*output logic                                      */  .lookup_ack_hit_o               (ddtc_lookup_ack_hit_o          ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_ack_idx_o               (ddtc_lookup_ack_idx_o          ),
    /*output logic                                      */  .lookup_ack_prefetched_o        (ddtc_lookup_ack_prefetched_o   ),
    /*output logic [1:0]                                */  .lookup_ack_lvl_o               (ddtc_lookup_ack_lvl_o          ),
    /*output logic [511:0]                              */  .lookup_ack_o                   (ddtc_lookup_ack_o              ),
    /*input  logic                                      */  .update_req_valid_i             (ddtc_update_req_valid_i        ),
    /*output logic                                      */  .update_req_ready_o             (ddtc_update_req_ready_o        ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .update_req_idx_i               (ddtc_update_req_idx_i          ),
    /*input  logic [1:0]                                */  .update_req_lvl_i               (ddtc_update_req_lvl_i          ),
    /*input  logic                                      */  .update_req_prefetched_i        (ddtc_update_req_prefetched_i   ),
    /*input  logic [511:0]                              */  .update_req_i                   (ddtc_update_req_i              ),
    /*input  logic [23:0]                               */  .update_req_device_id_i         (ddtc_update_req_device_id_i    ),
    /*output logic                                      */  .update_ack_valid_o             (ddtc_update_ack_valid_o        ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .update_ack_idx_o               (ddtc_update_ack_idx_o          ),
    /*input  logic                                      */  .multi_hit_check_i              (1'b1                           ),
    /*output logic                                      */  .multi_hit_fault_o              (                               ),
    /*output logic [1:0]                                */  .ecc_err_o                      (                               ),
    /*input  logic                                      */  .spare_in                       (1'b0                           ) 
    );



    iommu_atd_pdtc_wrap #(
    /*parameter  */ .INV_IDX_WIDTH              (INV_FIFO_IDX_WIDTH             ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    /*parameter  */ .TLB_QIDX_WIDTH             (PDTC_TLB_QIDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (PDTC_MICRO_TLB_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH        (PDTC_CABIN_LKP_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH        (PDTC_CABIN_UPD_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    /*parameter  */ .CABIN_INV_IDX_WIDTH        (PDTC_CABIN_INV_IDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    /*parameter  */ .BANK_L0_IDX_WIDTH          (PDTC_BANK_L0_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    /*parameter  */ .BANK_L1_IDX_WIDTH          (PDTC_BANK_L1_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    /*parameter  */ .BANK_L2_IDX_WIDTH          (PDTC_BANK_L2_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    /*parameter  */ .BANK_L0_SET_IDX_WIDTH      (PDTC_BANK_L0_SET_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L1_SET_IDX_WIDTH      (PDTC_BANK_L1_SET_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L2_SET_IDX_WIDTH      (PDTC_BANK_L2_SET_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L0_WAY_IDX_WIDTH      (PDTC_BANK_L0_WAY_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L1_WAY_IDX_WIDTH      (PDTC_BANK_L1_WAY_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L2_WAY_IDX_WIDTH      (PDTC_BANK_L2_WAY_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0
    ) U_pdtc(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .invalid_req_valid_i            (pdtc_invalid_req_valid_i       ),
    /*output logic                                      */  .invalid_req_ready_o            (pdtc_invalid_req_ready_o       ),
    /*input  logic [INV_IDX_WIDTH-1:0]                  */  .invalid_req_idx_i              (pdtc_invalid_req_idx_i         ),
    /*input  logic [1:0]                                */  .invalid_req_type_i             (pdtc_invalid_req_type_i        ),         // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    /*input  logic                                      */  .invalid_req_dv_gv_i            (pdtc_invalid_req_dv_gv_i       ),
    /*input  logic [23:0]                               */  .invalid_req_did_gscid_i        (pdtc_invalid_req_did_gscid_i   ),
    /*input  logic                                      */  .invalid_req_pscv_i             (pdtc_invalid_req_pscv_i        ),
    /*input  logic [19:0]                               */  .invalid_req_pid_pscid_i        (pdtc_invalid_req_pid_pscid_i   ),
    /*input  logic                                      */  .invalid_req_av_i               (pdtc_invalid_req_av_i          ),
    /*input  logic [63:12]                              */  .invalid_req_addr_i             (pdtc_invalid_req_addr_i        ),
    /*output logic                                      */  .invalid_ack_valid_o            (pdtc_invalid_ack_valid_o       ),
    /*output logic [INV_IDX_WIDTH-1:0]                  */  .invalid_ack_idx_o              (pdtc_invalid_ack_idx_o         ),
    /*input  logic                                      */  .lookup_req_valid_i             (pdtc_lookup_req_valid_i        ),
    /*output logic                                      */  .lookup_req_ready_o             (pdtc_lookup_req_ready_o        ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_req_idx_i               (pdtc_lookup_req_idx_i          ),
    /*input  logic [23:0]                               */  .lookup_req_device_id_i         (pdtc_lookup_req_device_id_i    ),
    /*input  logic [19:0]                               */  .lookup_req_process_id_i        (pdtc_lookup_req_process_id_i   ),
    /*output logic                                      */  .lookup_ack_valid_o             (pdtc_lookup_ack_valid_o        ),
    /*output logic                                      */  .lookup_ack_hit_o               (pdtc_lookup_ack_hit_o          ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_ack_idx_o               (pdtc_lookup_ack_idx_o          ),
    /*output logic                                      */  .lookup_ack_prefetched_o        (pdtc_lookup_ack_prefetched_o   ),
    /*output logic [1:0]                                */  .lookup_ack_lvl_o               (pdtc_lookup_ack_lvl_o          ),
    /*output logic [127:0]                              */  .lookup_ack_o                   (pdtc_lookup_ack_o              ),
    /*input  logic                                      */  .update_req_valid_i             (pdtc_update_req_valid_i        ),
    /*output logic                                      */  .update_req_ready_o             (pdtc_update_req_ready_o        ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .update_req_idx_i               (pdtc_update_req_idx_i          ),
    /*input  logic [1:0]                                */  .update_req_lvl_i               (pdtc_update_req_lvl_i          ),
    /*input  logic                                      */  .update_req_prefetched_i        (pdtc_update_req_prefetched_i   ),
    /*input  logic [127:0]                              */  .update_req_i                   (pdtc_update_req_i              ),
    /*input  logic [23:0]                               */  .update_req_device_id_i         (pdtc_update_req_device_id_i    ),
    /*input  logic [19:0]                               */  .update_req_process_id_i        (pdtc_update_req_process_id_i   ),
    /*output logic                                      */  .update_ack_valid_o             (pdtc_update_ack_valid_o        ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .update_ack_idx_o               (pdtc_update_ack_idx_o          ),
    /*input  logic                                      */  .multi_hit_check_i              (1'b1                           ),
    /*output logic                                      */  .multi_hit_fault_o              (                               ),
    /*output logic [1:0]                                */  .ecc_err_o                      (                               ),
    /*input  logic                                      */  .spare_in                       (1'b0                           ) 
    );



    iommu_atd_s1ptc_wrap #(
    /*parameter  */ .INV_IDX_WIDTH              (INV_FIFO_IDX_WIDTH             ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    /*parameter  */ .TLB_QIDX_WIDTH             (S1PTC_TLB_QIDX_WIDTH           ), // = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (S1PTC_MICRO_TLB_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH        (S1PTC_CABIN_LKP_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH        (S1PTC_CABIN_UPD_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    /*parameter  */ .CABIN_INV_IDX_WIDTH        (S1PTC_CABIN_INV_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    /*parameter  */ .BANK_L0_IDX_WIDTH          (S1PTC_BANK_L0_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    /*parameter  */ .BANK_L1_IDX_WIDTH          (S1PTC_BANK_L1_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    /*parameter  */ .BANK_L2_IDX_WIDTH          (S1PTC_BANK_L2_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    /*parameter  */ .BANK_L3_IDX_WIDTH          (S1PTC_BANK_L3_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    /*parameter  */ .BANK_L4_IDX_WIDTH          (S1PTC_BANK_L4_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    /*parameter  */ .BANK_L0_SET_IDX_WIDTH      (S1PTC_BANK_L0_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L1_SET_IDX_WIDTH      (S1PTC_BANK_L1_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L2_SET_IDX_WIDTH      (S1PTC_BANK_L2_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L3_SET_IDX_WIDTH      (S1PTC_BANK_L3_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L4_SET_IDX_WIDTH      (S1PTC_BANK_L4_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L0_WAY_IDX_WIDTH      (S1PTC_BANK_L0_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L1_WAY_IDX_WIDTH      (S1PTC_BANK_L1_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L2_WAY_IDX_WIDTH      (S1PTC_BANK_L2_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L3_WAY_IDX_WIDTH      (S1PTC_BANK_L3_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L4_WAY_IDX_WIDTH      (S1PTC_BANK_L4_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0 
    ) U_s1ptc(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .invalid_req_valid_i            (s1ptc_invalid_req_valid_i      ),
    /*output logic                                      */  .invalid_req_ready_o            (s1ptc_invalid_req_ready_o      ),
    /*input  logic [INV_IDX_WIDTH-1:0]                  */  .invalid_req_idx_i              (s1ptc_invalid_req_idx_i        ),
    /*input  logic [1:0]                                */  .invalid_req_type_i             (s1ptc_invalid_req_type_i       ),        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    /*input  logic                                      */  .invalid_req_dv_gv_i            (s1ptc_invalid_req_dv_gv_i      ),
    /*input  logic [23:0]                               */  .invalid_req_did_gscid_i        (s1ptc_invalid_req_did_gscid_i  ),
    /*input  logic                                      */  .invalid_req_pscv_i             (s1ptc_invalid_req_pscv_i       ),
    /*input  logic [19:0]                               */  .invalid_req_pid_pscid_i        (s1ptc_invalid_req_pid_pscid_i  ),
    /*input  logic                                      */  .invalid_req_av_i               (s1ptc_invalid_req_av_i         ),
    /*input  logic [63:12]                              */  .invalid_req_addr_i             (s1ptc_invalid_req_addr_i       ),
    /*output logic                                      */  .invalid_ack_valid_o            (s1ptc_invalid_ack_valid_o      ),
    /*output logic [INV_IDX_WIDTH-1:0]                  */  .invalid_ack_idx_o              (s1ptc_invalid_ack_idx_o        ),
    /*input  logic                                      */  .lookup_req_valid_i             (s1ptc_lookup_req_valid_i       ),
    /*output logic                                      */  .lookup_req_ready_o             (s1ptc_lookup_req_ready_o       ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_req_idx_i               (s1ptc_lookup_req_idx_i         ),
    /*input  logic                                      */  .lookup_req_gv_i                (s1ptc_lookup_req_gv_i          ),
    /*input  logic [15:0]                               */  .lookup_req_gscid_i             (s1ptc_lookup_req_gscid_i       ),
    /*input  logic [19:0]                               */  .lookup_req_pscid_i             (s1ptc_lookup_req_pscid_i       ),
    /*input  logic [63:12]                              */  .lookup_req_va_i                (s1ptc_lookup_req_va_i          ),
    /*output logic                                      */  .lookup_ack_valid_o             (s1ptc_lookup_ack_valid_o       ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_ack_idx_o               (s1ptc_lookup_ack_idx_o         ),
    /*output logic                                      */  .lookup_ack_hit_o               (s1ptc_lookup_ack_hit_o         ),
    /*output logic [2:0]                                */  .lookup_ack_lvl_o               (s1ptc_lookup_ack_lvl_o         ),
    /*output logic                                      */  .lookup_ack_prefetched_o        (s1ptc_lookup_ack_prefetched_o  ),
    /*output logic [511:0]                              */  .lookup_ack_o                   (s1ptc_lookup_ack_o             ),
    /*output logic [63:0]                               */  .lookup_ack_svnapot_o           (s1ptc_lookup_ack_svnapot_o     ),
    /*input  logic                                      */  .update_req_valid_i             (s1ptc_update_req_valid_i       ),
    /*output logic                                      */  .update_req_ready_o             (s1ptc_update_req_ready_o       ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .update_req_idx_i               (s1ptc_update_req_idx_i         ),
    /*input  logic [2:0]                                */  .update_req_lvl_i               (s1ptc_update_req_lvl_i         ),
    /*input  logic                                      */  .update_req_prefetched_i        (s1ptc_update_req_prefetched_i  ),
    /*input  logic [511:0]                              */  .update_req_i                   (s1ptc_update_req_i             ),
    /*input  logic [63:0]                               */  .update_req_svnapot_i           (s1ptc_update_req_svnapot_i     ),
    /*input  logic [19:0]                               */  .update_req_pscid_i             (s1ptc_update_req_pscid_i       ),
    /*input  logic                                      */  .update_req_gv_i                (s1ptc_update_req_gv_i          ),
    /*input  logic [15:0]                               */  .update_req_gscid_i             (s1ptc_update_req_gscid_i       ),
    /*input  logic [63:12]                              */  .update_req_va_i                (s1ptc_update_req_va_i          ),
    /*input  logic                                      */  .update_req_sxl_i               (s1ptc_update_req_sxl_i         ),
    /*output logic                                      */  .update_ack_valid_o             (s1ptc_update_ack_valid_o       ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .update_ack_idx_o               (s1ptc_update_ack_idx_o         ),
    /*input  logic                                      */  .multi_hit_check_i              (1'b1                           ),
    /*output logic                                      */  .multi_hit_fault_o              (                               ),
    /*output logic [1:0]                                */  .ecc_err_o                      (                               ),
    /*input  logic                                      */  .spare_in                       (1'b0                           ) 
    );



    iommu_atd_s2ptc_wrap #(
    /*parameter  */ .INV_IDX_WIDTH              (INV_FIFO_IDX_WIDTH             ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    /*parameter  */ .TLB_QIDX_WIDTH             (S2PTC_TLB_QIDX_WIDTH           ), // = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (S2PTC_MICRO_TLB_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH        (S2PTC_CABIN_LKP_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH        (S2PTC_CABIN_UPD_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    /*parameter  */ .CABIN_INV_IDX_WIDTH        (S2PTC_CABIN_INV_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    /*parameter  */ .BANK_L0_IDX_WIDTH          (S2PTC_BANK_L0_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    /*parameter  */ .BANK_L1_IDX_WIDTH          (S2PTC_BANK_L1_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    /*parameter  */ .BANK_L2_IDX_WIDTH          (S2PTC_BANK_L2_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    /*parameter  */ .BANK_L3_IDX_WIDTH          (S2PTC_BANK_L3_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    /*parameter  */ .BANK_L4_IDX_WIDTH          (S2PTC_BANK_L4_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    /*parameter  */ .BANK_L0_SET_IDX_WIDTH      (S2PTC_BANK_L0_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L1_SET_IDX_WIDTH      (S2PTC_BANK_L1_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L2_SET_IDX_WIDTH      (S2PTC_BANK_L2_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L3_SET_IDX_WIDTH      (S2PTC_BANK_L3_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L4_SET_IDX_WIDTH      (S2PTC_BANK_L4_SET_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    /*parameter  */ .BANK_L0_WAY_IDX_WIDTH      (S2PTC_BANK_L0_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L1_WAY_IDX_WIDTH      (S2PTC_BANK_L1_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L2_WAY_IDX_WIDTH      (S2PTC_BANK_L2_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L3_WAY_IDX_WIDTH      (S2PTC_BANK_L3_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_L4_WAY_IDX_WIDTH      (S2PTC_BANK_L4_WAY_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  // = 0 
    ) U_s2ptc(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .invalid_req_valid_i            (s2ptc_invalid_req_valid_i      ),
    /*output logic                                      */  .invalid_req_ready_o            (s2ptc_invalid_req_ready_o      ),
    /*input  logic [INV_IDX_WIDTH-1:0]                  */  .invalid_req_idx_i              (s2ptc_invalid_req_idx_i        ),
    /*input  logic [1:0]                                */  .invalid_req_type_i             (s2ptc_invalid_req_type_i       ),        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    /*input  logic                                      */  .invalid_req_dv_gv_i            (s2ptc_invalid_req_dv_gv_i      ),
    /*input  logic [23:0]                               */  .invalid_req_did_gscid_i        (s2ptc_invalid_req_did_gscid_i  ),
    /*input  logic                                      */  .invalid_req_pscv_i             (s2ptc_invalid_req_pscv_i       ),
    /*input  logic [19:0]                               */  .invalid_req_pid_pscid_i        (s2ptc_invalid_req_pid_pscid_i  ),
    /*input  logic                                      */  .invalid_req_av_i               (s2ptc_invalid_req_av_i         ),
    /*input  logic [63:12]                              */  .invalid_req_addr_i             (s2ptc_invalid_req_addr_i       ),
    /*output logic                                      */  .invalid_ack_valid_o            (s2ptc_invalid_ack_valid_o      ),
    /*output logic [INV_IDX_WIDTH-1:0]                  */  .invalid_ack_idx_o              (s2ptc_invalid_ack_idx_o        ),
    /*input  logic                                      */  .lookup_req_valid_i             (s2ptc_lookup_req_valid_i       ),
    /*output logic                                      */  .lookup_req_ready_o             (s2ptc_lookup_req_ready_o       ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_req_idx_i               (s2ptc_lookup_req_idx_i         ),
    /*input  logic [15:0]                               */  .lookup_req_gscid_i             (s2ptc_lookup_req_gscid_i       ),
    /*input  logic [63:12]                              */  .lookup_req_gpa_i               (s2ptc_lookup_req_gpa_i         ),
    /*output logic                                      */  .lookup_ack_valid_o             (s2ptc_lookup_ack_valid_o       ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .lookup_ack_idx_o               (s2ptc_lookup_ack_idx_o         ),
    /*output logic                                      */  .lookup_ack_hit_o               (s2ptc_lookup_ack_hit_o         ),
    /*output logic [2:0]                                */  .lookup_ack_lvl_o               (s2ptc_lookup_ack_lvl_o         ),
    /*output logic                                      */  .lookup_ack_prefetched_o        (s2ptc_lookup_ack_prefetched_o  ),
    /*output logic [511:0]                              */  .lookup_ack_o                   (s2ptc_lookup_ack_o             ),
    /*output logic [63:0]                               */  .lookup_ack_svnapot_o           (s2ptc_lookup_ack_svnapot_o     ),
    /*input  logic                                      */  .update_req_valid_i             (s2ptc_update_req_valid_i       ),
    /*output logic                                      */  .update_req_ready_o             (s2ptc_update_req_ready_o       ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .update_req_idx_i               (s2ptc_update_req_idx_i         ),
    /*input  logic [2:0]                                */  .update_req_lvl_i               (s2ptc_update_req_lvl_i         ),
    /*input  logic                                      */  .update_req_prefetched_i        (s2ptc_update_req_prefetched_i  ),
    /*input  logic [511:0]                              */  .update_req_i                   (s2ptc_update_req_i             ),
    /*input  logic [63:0]                               */  .update_req_svnapot_i           (s2ptc_update_req_svnapot_i     ),
    /*input  logic [15:0]                               */  .update_req_gscid_i             (s2ptc_update_req_gscid_i       ),
    /*input  logic [63:12]                              */  .update_req_gpa_i               (s2ptc_update_req_gpa_i         ),
    /*output logic                                      */  .update_ack_valid_o             (s2ptc_update_ack_valid_o       ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .update_ack_idx_o               (s2ptc_update_ack_idx_o         ),
    /*input  logic                                      */  .csr_fctl_gxl_i                 (csr_fctl_gxl_i                 ),
    /*input  logic                                      */  .multi_hit_check_i              (1'b1                           ),
    /*output logic                                      */  .multi_hit_fault_o              (                               ),
    /*output logic [1:0]                                */  .ecc_err_o                      (                               ),
    /*input  logic                                      */  .spare_in                       (1'b0                           ) 
    );



endmodule
