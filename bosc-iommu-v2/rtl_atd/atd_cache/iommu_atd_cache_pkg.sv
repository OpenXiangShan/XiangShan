package iommu_atd_cache_pkg;


localparam INVTYPE_INVALID_DDT                              = 2'b00;
localparam INVTYPE_INVALID_PDT                              = 2'b01;
localparam INVTYPE_VMA                                      = 2'b10;
localparam INVTYPE_GVMA                                     = 2'b11;




//////////////////////////////////////////////////////////////////////////////
// parametersized define default setting
//////////////////////////////////////////////////////////////////////////////
// iommu_atd_ddtc_wrap {{{
    parameter  INV_IDX_WIDTH               = 4;                    // should not bigger than 4
    parameter  INV_INFLY_NUM               = 2**INV_IDX_WIDTH;
    
    typedef struct packed {
        logic [INV_IDX_WIDTH-1:0]           idx;                // invalid cmd idx
        logic [1:0]                         itype;              // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
        logic                               dv_gv;
        logic [23:0]                        did_gscid;
        logic                               pscv;
        logic [19:0]                        pid_pscid;
        logic                               av;
        logic [63:12]                       addr;
    } INVALID_REQ_TYPE;
    
    
    parameter  DDTC_TLB_QIDX_WIDTH                 = 3;
    parameter  DDTC_MICRO_TLB_IDX_WIDTH            = 4;
    parameter  DDTC_CABIN_LKP_IDX_WIDTH            = 1;
    parameter  DDTC_CABIN_UPD_IDX_WIDTH            = 1;
    parameter  DDTC_CABIN_INV_IDX_WIDTH            = 1;
    parameter  DDTC_BANK_L0_IDX_WIDTH              = 2;
    parameter  DDTC_BANK_L1_IDX_WIDTH              = 1;
    parameter  DDTC_BANK_L2_IDX_WIDTH              = 1;
    parameter  DDTC_BANK_L0_SET_IDX_WIDTH          = 4;
    parameter  DDTC_BANK_L1_SET_IDX_WIDTH          = 2;
    parameter  DDTC_BANK_L2_SET_IDX_WIDTH          = 1;
    parameter  DDTC_BANK_L0_WAY_IDX_WIDTH          = 3;
    parameter  DDTC_BANK_L1_WAY_IDX_WIDTH          = 2;
    parameter  DDTC_BANK_L2_WAY_IDX_WIDTH          = 1;



// iommu_atd_ddtc_wrap
    typedef struct packed {
        logic [DDTC_TLB_QIDX_WIDTH-1:0]     idx;
        logic [23:0]                        device_id;
    } ddtc_lookup_req_t;
    
    typedef struct packed {
        logic [DDTC_TLB_QIDX_WIDTH-1:0]     idx;
        logic [1:0]                         lvl;
        logic                               hit;
        logic                               prefetched;
        logic [51:0]                        msi_addr_pattern;
        logic [51:0]                        msi_addr_mask;
        logic [3:0]                         msipip_mode;
        logic [43:0]                        msipip_ppn;
        logic [3:0]                         fsc_mode;
        logic [43:0]                        fsc_ppn;
        logic [19:0]                        PSCID;
        logic [3:0]                         S2MODE;
        logic [15:0]                        GSCID;
        logic [43:0]                        S2PPN;
        logic                               SXL;
        logic                               SBE;
        logic                               DPE;
        logic                               SADE;
        logic                               GADE;
        logic                               PRPR;
        logic                               PDTV;
        logic                               DTF;
        logic                               T2GPA;
        logic                               EN_PRI;
        logic                               EN_ATS;
        logic                               V;
    } ddtc_lookup_ack_t;
    
    typedef struct packed {
        logic [DDTC_TLB_QIDX_WIDTH:0]       idx;
        logic [1:0]                         lvl;
        logic                               prefetched;
        logic [23:0]                        device_id;
        logic [51:0]                        msi_addr_pattern;
        logic [51:0]                        msi_addr_mask;
        logic [3:0]                         msipip_mode;
        logic [43:0]                        msipip_ppn;
        logic [3:0]                         fsc_mode;
        logic [43:0]                        fsc_ppn;
        logic [19:0]                        PSCID;
        logic [3:0]                         S2MODE;
        logic [15:0]                        GSCID;
        logic [43:0]                        S2PPN;
        logic                               SXL;
        logic                               SBE;
        logic                               DPE;
        logic                               SADE;
        logic                               GADE;
        logic                               PRPR;
        logic                               PDTV;
        logic                               DTF;
        logic                               T2GPA;
        logic                               EN_PRI;
        logic                               EN_ATS;
        logic                               V;
    } ddtc_update_req_t;


    localparam  DDTC_CABIN_LKP_NUM               = 2**DDTC_CABIN_LKP_IDX_WIDTH;
    localparam  DDTC_CABIN_UPD_NUM               = 2**DDTC_CABIN_UPD_IDX_WIDTH;
    localparam  DDTC_CABIN_INV_NUM               = 2**DDTC_CABIN_INV_IDX_WIDTH;
    localparam  DDTC_BANK_L0_NUM                 = 2**DDTC_BANK_L0_IDX_WIDTH;
    localparam  DDTC_BANK_L1_NUM                 = 2**DDTC_BANK_L1_IDX_WIDTH;
    localparam  DDTC_BANK_L2_NUM                 = 2**DDTC_BANK_L2_IDX_WIDTH;
    localparam  DDTC_BANK_L0_SET_NUM             = 2**DDTC_BANK_L0_SET_IDX_WIDTH;
    localparam  DDTC_BANK_L1_SET_NUM             = 2**DDTC_BANK_L1_SET_IDX_WIDTH;
    localparam  DDTC_BANK_L2_SET_NUM             = 2**DDTC_BANK_L2_SET_IDX_WIDTH;
    localparam  DDTC_BANK_L0_WAY_NUM             = 2**DDTC_BANK_L0_WAY_IDX_WIDTH;
    localparam  DDTC_BANK_L1_WAY_NUM             = 2**DDTC_BANK_L1_WAY_IDX_WIDTH;
    localparam  DDTC_BANK_L2_WAY_NUM             = 2**DDTC_BANK_L2_WAY_IDX_WIDTH;

    localparam DDTC_MAX_BANK_IDX_WIDTH_0 = (DDTC_BANK_L0_IDX_WIDTH > DDTC_BANK_L1_IDX_WIDTH) ? DDTC_BANK_L0_IDX_WIDTH : DDTC_BANK_L1_IDX_WIDTH;
    localparam DDTC_MAX_BANK_IDX_WIDTH_1 = DDTC_BANK_L2_IDX_WIDTH;
    localparam DDTC_MAX_BANK_IDX_WIDTH   = (DDTC_MAX_BANK_IDX_WIDTH_0 > DDTC_MAX_BANK_IDX_WIDTH_1) ? DDTC_MAX_BANK_IDX_WIDTH_0 : DDTC_MAX_BANK_IDX_WIDTH_1;
    localparam DDTC_MAX_BANK_NUM         = 2**DDTC_MAX_BANK_IDX_WIDTH;
    
    localparam DDTC_MAX_BANK_SET_IDX_WIDTH_0 = (DDTC_BANK_L0_SET_IDX_WIDTH > DDTC_BANK_L1_SET_IDX_WIDTH) ? DDTC_BANK_L0_SET_IDX_WIDTH : DDTC_BANK_L1_SET_IDX_WIDTH;
    localparam DDTC_MAX_BANK_SET_IDX_WIDTH_1 = DDTC_BANK_L2_SET_IDX_WIDTH;
    localparam DDTC_MAX_BANK_SET_IDX_WIDTH   = (DDTC_MAX_BANK_SET_IDX_WIDTH_0 > DDTC_MAX_BANK_SET_IDX_WIDTH_1) ? DDTC_MAX_BANK_SET_IDX_WIDTH_0 : DDTC_MAX_BANK_SET_IDX_WIDTH_1;
    localparam DDTC_MAX_BANK_SET_NUM         = 2**DDTC_MAX_BANK_SET_IDX_WIDTH;
    
    localparam DDTC_MAX_BANK_WAY_IDX_WIDTH_0 = (DDTC_BANK_L0_WAY_IDX_WIDTH > DDTC_BANK_L1_WAY_IDX_WIDTH) ? DDTC_BANK_L0_WAY_IDX_WIDTH : DDTC_BANK_L1_WAY_IDX_WIDTH;
    localparam DDTC_MAX_BANK_WAY_IDX_WIDTH_1 = DDTC_BANK_L2_WAY_IDX_WIDTH;
    localparam DDTC_MAX_BANK_WAY_IDX_WIDTH   = (DDTC_MAX_BANK_WAY_IDX_WIDTH_0 > DDTC_MAX_BANK_WAY_IDX_WIDTH_1) ? DDTC_MAX_BANK_WAY_IDX_WIDTH_0 : DDTC_MAX_BANK_WAY_IDX_WIDTH_1;
    localparam DDTC_MAX_BANK_WAY_NUM         = 2**DDTC_MAX_BANK_WAY_IDX_WIDTH;
    
    typedef struct packed {                             
        logic                                           valid;
        logic                                           prefetched;
        logic [23:0]                                    device_id;
    } ddtc_mtlb_tag_t;                                       
    
    typedef struct packed {
        logic                                           rsv;
    } ddtc_mtlb_itag_t;
    
    typedef struct packed {
        logic [DDTC_MAX_BANK_WAY_NUM-2:0]               plru_list;
    } ddtc_mtlb_utag_t;
    
    typedef struct packed {                             
        logic [43:0]                                    fsc_ppn;
        logic                                           V;
    } ddtc_mtlb_dat_t_nl;                                     
    
    typedef struct packed {                             
        logic [51:0]                                    msi_addr_pattern;
        logic [51:0]                                    msi_addr_mask;
        logic [3:0]                                     msipip_mode;
        logic [43:0]                                    msipip_ppn;
        logic [3:0]                                     fsc_mode;
        logic [43:0]                                    fsc_ppn;
        logic [19:0]                                    PSCID;
        logic [3:0]                                     S2MODE;
        logic [15:0]                                    GSCID;
        logic [43:0]                                    S2PPN;
        logic                                           SXL;
        logic                                           SBE;
        logic                                           DPE;
        logic                                           SADE;
        logic                                           GADE;
        logic                                           PRPR;
        logic                                           PDTV;
        logic                                           DTF;
        logic                                           T2GPA;
        logic                                           EN_PRI;
        logic                                           EN_ATS;
        logic                                           V;
    } ddtc_mtlb_dat_t_l;                                     

    typedef struct packed {                             
        logic                                           prefetched;
        logic [23:0]                                    device_id;
    } ddtc_microtlb_tag_t;                              
                                                        
    typedef struct packed {                             
        logic                                           rsv;
    } ddtc_microtlb_inv_tag_t;                          
                                                        
    typedef struct packed {                             
        logic [1:0]                                     lvl;
        logic [51:0]                                    msi_addr_pattern;
        logic [51:0]                                    msi_addr_mask;
        logic [3:0]                                     msipip_mode;
        logic [43:0]                                    msipip_ppn;
        logic [3:0]                                     fsc_mode;
        logic [43:0]                                    fsc_ppn;
        logic [19:0]                                    PSCID;
        logic [3:0]                                     S2MODE;
        logic [15:0]                                    GSCID;
        logic [43:0]                                    S2PPN;
        logic                                           SXL;
        logic                                           SBE;
        logic                                           DPE;
        logic                                           SADE;
        logic                                           GADE;
        logic                                           PRPR;
        logic                                           PDTV;
        logic                                           DTF;
        logic                                           T2GPA;
        logic                                           EN_PRI;
        logic                                           EN_ATS;
        logic                                           V;               
    } ddtc_microtlb_content_t;                          

// iommu_atd_ddtc_main_tlb
    // lkp_req to bank
    typedef struct packed {
        ddtc_lookup_req_t                               req;            // LOOKUP_REQ message
        logic [DDTC_MAX_BANK_IDX_WIDTH-1:0]             bank_idx;       // sub_bank idx
        logic [DDTC_MAX_BANK_SET_IDX_WIDTH-1:0]         bank_set_idx;   // ram addr
    } ddtc_bank_lkp_req_t;

    typedef struct packed {
        ddtc_update_req_t                               ack;
        logic                                           hit;
    } ddtc_bank_lkp_ack_t;

    typedef struct packed {
        logic [DDTC_CABIN_LKP_NUM-1:0]                  valid;
        logic [DDTC_CABIN_LKP_NUM-1:0]                  ready;
        ddtc_bank_lkp_req_t  [DDTC_CABIN_LKP_NUM-1:0]   req;
    } ddtc_lkp2bank_req_grp_t;

    typedef struct packed {
        logic [DDTC_CABIN_LKP_NUM-1:0]                  valid;
        ddtc_bank_lkp_ack_t [DDTC_CABIN_LKP_NUM-1:0]    ack;
    } ddtc_bank2lkp_ack_grp_t;


    // upd_req to bank
    typedef struct packed {
        ddtc_update_req_t                               req;            // LOOKUP_REQ message
        logic [DDTC_MAX_BANK_IDX_WIDTH-1:0]             bank_idx;       // sub_bank idx
        logic [DDTC_MAX_BANK_SET_IDX_WIDTH-1:0]         bank_set_idx;   // ram addr
    } ddtc_bank_upd_req_t;

    typedef struct packed {
        ddtc_update_req_t                               ack;
    } ddtc_bank_upd_ack_t;

    typedef struct packed {
        logic [DDTC_CABIN_UPD_NUM-1:0]                  valid;
        logic [DDTC_CABIN_UPD_NUM-1:0]                  ready;
        ddtc_bank_upd_req_t  [DDTC_CABIN_UPD_NUM-1:0]   req;
    } ddtc_upd2bank_req_grp_t;

    typedef struct packed {
        logic [DDTC_CABIN_UPD_NUM-1:0]                  valid;
        ddtc_bank_upd_ack_t [DDTC_CABIN_UPD_NUM-1:0]    ack;
    } ddtc_bank2upd_ack_grp_t;


    // inv_req to bank
    typedef struct packed {
        INVALID_REQ_TYPE                                req;            // LOOKUP_REQ message
        logic                                           bank_idx_val;
        logic [DDTC_MAX_BANK_IDX_WIDTH-1:0]             bank_idx;       // sub_bank idx
        logic [DDTC_MAX_BANK_SET_IDX_WIDTH-1:0]         bank_set_idx;   // ram addr
    } ddtc_bank_inv_req_t;

    typedef struct packed {
        INVALID_REQ_TYPE                                ack;
    } ddtc_bank_inv_ack_t;

    typedef struct packed {
        logic [DDTC_CABIN_INV_NUM-1:0]                  valid;
        logic [DDTC_CABIN_INV_NUM-1:0]                  ready;
        ddtc_bank_inv_req_t  [DDTC_CABIN_INV_NUM-1:0]   req;
    } ddtc_inv2bank_req_grp_t;

    typedef struct packed {
        logic [DDTC_CABIN_INV_NUM-1:0]                  valid;
        ddtc_bank_inv_ack_t [DDTC_CABIN_INV_NUM-1:0]    ack;
    } ddtc_bank2inv_ack_grp_t;


// iommu_atd_ddtc_mtlb_bank
    typedef struct packed {
        logic [2:0]                                     typ; // 001: lkp, 010:upd, 100:inv
        logic [DDTC_CABIN_LKP_NUM-1:0]                  lidx;
        logic [DDTC_CABIN_UPD_NUM-1:0]                  uidx;
        logic [DDTC_CABIN_INV_NUM-1:0]                  iidx;
        ddtc_bank_lkp_req_t                             lkp;
        ddtc_bank_upd_req_t                             upd;
        ddtc_bank_inv_req_t                             inv;
    } ddtc_bank_req_t;
//}}}

// iommu_atd_s2ptc_wrap {{{
//    parameter  INV_IDX_WIDTH               = 4;                    // should not bigger than 4
//    parameter  INV_INFLY_NUM               = 2**INV_IDX_WIDTH;
//    
//    typedef struct packed {
//        logic [INV_IDX_WIDTH-1:0]           idx;                // invalid cmd idx
//        logic [1:0]                         itype;              // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
//        logic                               dv_gv;
//        logic [23:0]                        did_gscid;
//        logic                               pscv;
//        logic [19:0]                        pid_pscid;
//        logic                               av;
//        logic [63:12]                       addr;
//    } INVALID_REQ_TYPE;
//    
//    
    parameter  S2PTC_TLB_QIDX_WIDTH                 = 3;
    parameter  S2PTC_MICRO_TLB_IDX_WIDTH            = 5;
    parameter  S2PTC_CABIN_LKP_IDX_WIDTH            = 1;
    parameter  S2PTC_CABIN_UPD_IDX_WIDTH            = 1;
    parameter  S2PTC_CABIN_INV_IDX_WIDTH            = 1;
    parameter  S2PTC_BANK_L0_IDX_WIDTH              = 4;
    parameter  S2PTC_BANK_L1_IDX_WIDTH              = 3;
    parameter  S2PTC_BANK_L2_IDX_WIDTH              = 2;
    parameter  S2PTC_BANK_L3_IDX_WIDTH              = 1;
    parameter  S2PTC_BANK_L4_IDX_WIDTH              = 1;
    parameter  S2PTC_BANK_L0_SET_IDX_WIDTH          = 4;
    parameter  S2PTC_BANK_L1_SET_IDX_WIDTH          = 3;
    parameter  S2PTC_BANK_L2_SET_IDX_WIDTH          = 2;
    parameter  S2PTC_BANK_L3_SET_IDX_WIDTH          = 1;
    parameter  S2PTC_BANK_L4_SET_IDX_WIDTH          = 1;
    parameter  S2PTC_BANK_L0_WAY_IDX_WIDTH          = 5;
    parameter  S2PTC_BANK_L1_WAY_IDX_WIDTH          = 4;
    parameter  S2PTC_BANK_L2_WAY_IDX_WIDTH          = 3;
    parameter  S2PTC_BANK_L3_WAY_IDX_WIDTH          = 2;
    parameter  S2PTC_BANK_L4_WAY_IDX_WIDTH          = 1;



// iommu_atd_s2ptc_wrap
    typedef struct packed {
        logic [S2PTC_TLB_QIDX_WIDTH-1:0]    idx;
        logic [63:12]                       addr;
        logic [15:0]                        gscid;
//        logic [19:0]                        pscid;
    } s2ptc_lookup_req_t;
    
    typedef struct packed {
        logic [S2PTC_TLB_QIDX_WIDTH-1:0]    idx;
        logic [2:0]                         lvl;
        logic                               hit;
        logic                               prefetched;
        logic [3:0]                         pte_position;
        logic                               N;
        logic [1:0]                         PBMT;
//        logic [15:0]                        D;
//        logic [15:0]                        A;
        logic                               D;
        logic                               A;
        logic [55:12]                       PPN;
        logic [4:0]                         PERM;
        logic                               V;
    } s2ptc_lookup_ack_t;
    
    typedef struct packed {
        logic [S2PTC_TLB_QIDX_WIDTH+3:0]    idx;            // microtlb2maintlb refill  update will dirve the bit[S2PTC_TLB_QIDX_WIDTH] to 1, to distinguish from input UPDATE, the highest 3bit is used to distinguish 8 ptes in the 512bit req input
        logic [2:0]                         lvl;
        logic                               prefetched;
        logic [63:12]                       addr;
        logic [15:0]                        gscid;
//        logic [19:0]                        pscid;
//        logic                               sxl;
        logic                               N;
        logic [1:0]                         PBMT;
//        logic [15:0]                        D;
//        logic [15:0]                        A;
        logic                               D;
        logic                               A;
        logic [55:12]                       PPN;
        logic [4:0]                         PERM;
        logic                               V;
    } s2ptc_update_req_t;


    localparam  S2PTC_CABIN_LKP_NUM               = 2**S2PTC_CABIN_LKP_IDX_WIDTH;
    localparam  S2PTC_CABIN_UPD_NUM               = 2**S2PTC_CABIN_UPD_IDX_WIDTH;
    localparam  S2PTC_CABIN_INV_NUM               = 2**S2PTC_CABIN_INV_IDX_WIDTH;
    localparam  S2PTC_BANK_L0_NUM                 = 2**S2PTC_BANK_L0_IDX_WIDTH;
    localparam  S2PTC_BANK_L1_NUM                 = 2**S2PTC_BANK_L1_IDX_WIDTH;
    localparam  S2PTC_BANK_L2_NUM                 = 2**S2PTC_BANK_L2_IDX_WIDTH;
    localparam  S2PTC_BANK_L3_NUM                 = 2**S2PTC_BANK_L3_IDX_WIDTH;
    localparam  S2PTC_BANK_L4_NUM                 = 2**S2PTC_BANK_L4_IDX_WIDTH;
    localparam  S2PTC_BANK_L0_SET_NUM             = 2**S2PTC_BANK_L0_SET_IDX_WIDTH;
    localparam  S2PTC_BANK_L1_SET_NUM             = 2**S2PTC_BANK_L1_SET_IDX_WIDTH;
    localparam  S2PTC_BANK_L2_SET_NUM             = 2**S2PTC_BANK_L2_SET_IDX_WIDTH;
    localparam  S2PTC_BANK_L3_SET_NUM             = 2**S2PTC_BANK_L3_SET_IDX_WIDTH;
    localparam  S2PTC_BANK_L4_SET_NUM             = 2**S2PTC_BANK_L4_SET_IDX_WIDTH;
    localparam  S2PTC_BANK_L0_WAY_NUM             = 2**S2PTC_BANK_L0_WAY_IDX_WIDTH;
    localparam  S2PTC_BANK_L1_WAY_NUM             = 2**S2PTC_BANK_L1_WAY_IDX_WIDTH;
    localparam  S2PTC_BANK_L2_WAY_NUM             = 2**S2PTC_BANK_L2_WAY_IDX_WIDTH;
    localparam  S2PTC_BANK_L3_WAY_NUM             = 2**S2PTC_BANK_L3_WAY_IDX_WIDTH;
    localparam  S2PTC_BANK_L4_WAY_NUM             = 2**S2PTC_BANK_L4_WAY_IDX_WIDTH;

    localparam S2PTC_MAX_BANK_IDX_WIDTH_0 = (S2PTC_BANK_L0_IDX_WIDTH > S2PTC_BANK_L1_IDX_WIDTH) ? S2PTC_BANK_L0_IDX_WIDTH : S2PTC_BANK_L1_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_IDX_WIDTH_1 = (S2PTC_BANK_L2_IDX_WIDTH > S2PTC_BANK_L3_IDX_WIDTH) ? S2PTC_BANK_L2_IDX_WIDTH : S2PTC_BANK_L3_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_IDX_WIDTH_2 =  S2PTC_BANK_L4_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_IDX_WIDTH_01= (S2PTC_MAX_BANK_IDX_WIDTH_0 > S2PTC_MAX_BANK_IDX_WIDTH_1) ? S2PTC_MAX_BANK_IDX_WIDTH_0 : S2PTC_MAX_BANK_IDX_WIDTH_1;
    localparam S2PTC_MAX_BANK_IDX_WIDTH   = (S2PTC_MAX_BANK_IDX_WIDTH_2 > S2PTC_MAX_BANK_IDX_WIDTH_01)? S2PTC_MAX_BANK_IDX_WIDTH_2 : S2PTC_MAX_BANK_IDX_WIDTH_01;
    localparam S2PTC_MAX_BANK_NUM         = 2**S2PTC_MAX_BANK_IDX_WIDTH;
    
    localparam S2PTC_MAX_BANK_SET_IDX_WIDTH_0 = (S2PTC_BANK_L0_SET_IDX_WIDTH > S2PTC_BANK_L1_SET_IDX_WIDTH) ? S2PTC_BANK_L0_SET_IDX_WIDTH : S2PTC_BANK_L1_SET_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_SET_IDX_WIDTH_1 = (S2PTC_BANK_L2_SET_IDX_WIDTH > S2PTC_BANK_L3_SET_IDX_WIDTH) ? S2PTC_BANK_L2_SET_IDX_WIDTH : S2PTC_BANK_L3_SET_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_SET_IDX_WIDTH_2 =  S2PTC_BANK_L4_SET_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_SET_IDX_WIDTH_01= (S2PTC_MAX_BANK_SET_IDX_WIDTH_0 > S2PTC_MAX_BANK_SET_IDX_WIDTH_1) ? S2PTC_MAX_BANK_SET_IDX_WIDTH_0 : S2PTC_MAX_BANK_SET_IDX_WIDTH_1;
    localparam S2PTC_MAX_BANK_SET_IDX_WIDTH   = (S2PTC_MAX_BANK_SET_IDX_WIDTH_2 > S2PTC_MAX_BANK_SET_IDX_WIDTH_01)? S2PTC_MAX_BANK_SET_IDX_WIDTH_2 : S2PTC_MAX_BANK_SET_IDX_WIDTH_01;
    localparam S2PTC_MAX_BANK_SET_NUM         = 2**S2PTC_MAX_BANK_SET_IDX_WIDTH;
    
    localparam S2PTC_MAX_BANK_WAY_IDX_WIDTH_0 = (S2PTC_BANK_L0_WAY_IDX_WIDTH > S2PTC_BANK_L1_WAY_IDX_WIDTH) ? S2PTC_BANK_L0_WAY_IDX_WIDTH : S2PTC_BANK_L1_WAY_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_WAY_IDX_WIDTH_1 = (S2PTC_BANK_L2_WAY_IDX_WIDTH > S2PTC_BANK_L3_WAY_IDX_WIDTH) ? S2PTC_BANK_L2_WAY_IDX_WIDTH : S2PTC_BANK_L3_WAY_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_WAY_IDX_WIDTH_2 =  S2PTC_BANK_L4_WAY_IDX_WIDTH;
    localparam S2PTC_MAX_BANK_WAY_IDX_WIDTH_01= (S2PTC_MAX_BANK_WAY_IDX_WIDTH_0 > S2PTC_MAX_BANK_WAY_IDX_WIDTH_1) ? S2PTC_MAX_BANK_WAY_IDX_WIDTH_0 : S2PTC_MAX_BANK_WAY_IDX_WIDTH_1;
    localparam S2PTC_MAX_BANK_WAY_IDX_WIDTH   = (S2PTC_MAX_BANK_WAY_IDX_WIDTH_2 > S2PTC_MAX_BANK_WAY_IDX_WIDTH_01)? S2PTC_MAX_BANK_WAY_IDX_WIDTH_2 : S2PTC_MAX_BANK_WAY_IDX_WIDTH_01;
    localparam S2PTC_MAX_BANK_WAY_NUM         = 2**S2PTC_MAX_BANK_WAY_IDX_WIDTH;
    
    typedef struct packed {                             
        logic                                           valid;
        logic                                           prefetched;
        logic [63:12]                                   addr;
        logic [15:0]                                    gscid;
        logic                                           g;
        logic                                           n;
        logic                                           leaf;
//        logic [19:0]                                    pscid;
//        logic                                           sxl;
        logic [2:0]                                     lvl;        // all leaf-pte (with any level/pagesize), stored in level0 bank
    } s2ptc_mtlb_tag_t;                                       
    
    typedef struct packed {
        logic                                           rsv;
    } s2ptc_mtlb_itag_t;
    
    typedef struct packed {
        logic [S2PTC_MAX_BANK_WAY_NUM-2:0]              plru_list;
    } s2ptc_mtlb_utag_t;
    
    typedef struct packed {                             
        logic [1:0]                                     PBMT;
        logic                                           D;
        logic                                           A;
        logic [55:12]                                   PPN;
        logic [4:0]                                     PERM;
        logic                                           V;
    } s2ptc_mtlb_dat_t_nl;                                     
    
    typedef struct packed {                             
        logic                                           N;
        logic [1:0]                                     PBMT;
//        logic [15:0]                                    D;
//        logic [15:0]                                    A;
        logic                                           D;
        logic                                           A;
        logic [55:12]                                   PPN;
        logic [4:0]                                     PERM;
        logic                                           V;
    } s2ptc_mtlb_dat_t_l;                                     

    typedef struct packed {                             
        logic                                           prefetched;
        logic [63:12]                                   addr;
        logic [15:0]                                    gscid;
//        logic [19:0]                                    pscid;
//        logic                                           sxl;
    } s2ptc_microtlb_tag_t;                              
                                                        
    typedef struct packed {                             
        logic                                           rsv;
    } s2ptc_microtlb_inv_tag_t;                          
                                                        
    typedef struct packed {                             
        logic [2:0]                                     lvl;
        logic                                           N;
        logic [1:0]                                     PBMT;
//        logic [15:0]                                    D;
//        logic [15:0]                                    A;
        logic                                           D;
        logic                                           A;
        logic [55:12]                                   PPN;
        logic [4:0]                                     PERM;
        logic                                           V;               
    } s2ptc_microtlb_content_t;                          

// iommu_atd_s2ptc_main_tlb
    // lkp_req to bank
    typedef struct packed {
        s2ptc_lookup_req_t                              req;            // LOOKUP_REQ message
        logic [S2PTC_MAX_BANK_IDX_WIDTH-1:0]            bank_idx;       // sub_bank idx
        logic [S2PTC_MAX_BANK_SET_IDX_WIDTH-1:0]        bank_set_idx;   // ram addr
    } s2ptc_bank_lkp_req_t;

    typedef struct packed {
        s2ptc_update_req_t                              ack;
        logic                                           hit;
    } s2ptc_bank_lkp_ack_t;

    typedef struct packed {
        logic [S2PTC_CABIN_LKP_NUM-1:0]                 valid;
        logic [S2PTC_CABIN_LKP_NUM-1:0]                 ready;
        s2ptc_bank_lkp_req_t  [S2PTC_CABIN_LKP_NUM-1:0] req;
    } s2ptc_lkp2bank_req_grp_t;

    typedef struct packed {
        logic [S2PTC_CABIN_LKP_NUM-1:0]                 valid;
        s2ptc_bank_lkp_ack_t [S2PTC_CABIN_LKP_NUM-1:0]  ack;
    } s2ptc_bank2lkp_ack_grp_t;


    // upd_req to bank
    typedef struct packed {
        s2ptc_update_req_t                              req;            // LOOKUP_REQ message
        logic [S2PTC_MAX_BANK_IDX_WIDTH-1:0]            bank_idx;       // sub_bank idx
        logic [S2PTC_MAX_BANK_SET_IDX_WIDTH-1:0]        bank_set_idx;   // ram addr
    } s2ptc_bank_upd_req_t;

    typedef struct packed {
        s2ptc_update_req_t                              ack;
    } s2ptc_bank_upd_ack_t;

    typedef struct packed {
        logic [S2PTC_CABIN_UPD_NUM-1:0]                 valid;
        logic [S2PTC_CABIN_UPD_NUM-1:0]                 ready;
        s2ptc_bank_upd_req_t  [S2PTC_CABIN_UPD_NUM-1:0] req;
    } s2ptc_upd2bank_req_grp_t;

    typedef struct packed {
        logic [S2PTC_CABIN_UPD_NUM-1:0]                 valid;
        s2ptc_bank_upd_ack_t [S2PTC_CABIN_UPD_NUM-1:0]  ack;
    } s2ptc_bank2upd_ack_grp_t;


    // inv_req to bank
    typedef struct packed {
        INVALID_REQ_TYPE                                req;            // LOOKUP_REQ message
        logic                                           bank_idx_val;
        logic [S2PTC_MAX_BANK_IDX_WIDTH-1:0]            bank_idx;       // sub_bank idx
        logic [S2PTC_MAX_BANK_SET_IDX_WIDTH-1:0]        bank_set_idx;   // ram addr
    } s2ptc_bank_inv_req_t;

    typedef struct packed {
        INVALID_REQ_TYPE                                ack;
    } s2ptc_bank_inv_ack_t;

    typedef struct packed {
        logic [S2PTC_CABIN_INV_NUM-1:0]                 valid;
        logic [S2PTC_CABIN_INV_NUM-1:0]                 ready;
        s2ptc_bank_inv_req_t  [S2PTC_CABIN_INV_NUM-1:0] req;
    } s2ptc_inv2bank_req_grp_t;

    typedef struct packed {
        logic [S2PTC_CABIN_INV_NUM-1:0]                 valid;
        s2ptc_bank_inv_ack_t [S2PTC_CABIN_INV_NUM-1:0]  ack;
    } s2ptc_bank2inv_ack_grp_t;


// iommu_atd_s2ptc_mtlb_bank
    typedef struct packed {
        logic [2:0]                                     typ; // 001: lkp, 010:upd, 100:inv
        logic [S2PTC_CABIN_LKP_NUM-1:0]                 lidx;
        logic [S2PTC_CABIN_UPD_NUM-1:0]                 uidx;
        logic [S2PTC_CABIN_INV_NUM-1:0]                 iidx;
        s2ptc_bank_lkp_req_t                            lkp;
        s2ptc_bank_upd_req_t                            upd;
        s2ptc_bank_inv_req_t                            inv;
    } s2ptc_bank_req_t;
//}}}



endpackage
