//
//
//
module iommu_atd_ddtc_wrap #(
//{{{ PARAM
    parameter  INV_IDX_WIDTH               = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    parameter  TLB_QIDX_WIDTH              = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    parameter  MICRO_TLB_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    parameter  CABIN_LKP_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    parameter  CABIN_UPD_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    parameter  CABIN_INV_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    parameter  BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    parameter  BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    parameter  BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    parameter  BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter  BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter  BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter  BANK_L0_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    parameter  BANK_L1_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    parameter  BANK_L2_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    parameter  ECC_ENABLE                  = 1,
    parameter  SPARE_PARAM                 = 0
//}}}
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // invalid
    input  logic                                        invalid_req_valid_i,
    output logic                                        invalid_req_ready_o,
    input  logic [INV_IDX_WIDTH-1:0]                    invalid_req_idx_i,
    input  logic [1:0]                                  invalid_req_type_i,         // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    input  logic                                        invalid_req_dv_gv_i,
    input  logic [23:0]                                 invalid_req_did_gscid_i,
    input  logic                                        invalid_req_pscv_i,
    input  logic [19:0]                                 invalid_req_pid_pscid_i,
    input  logic                                        invalid_req_av_i,
    input  logic [63:12]                                invalid_req_addr_i,
    output logic                                        invalid_ack_valid_o,
    output logic [INV_IDX_WIDTH-1:0]                    invalid_ack_idx_o,
    // lookup
    input  logic                                        lookup_req_valid_i,
    output logic                                        lookup_req_ready_o,
    input  logic [TLB_QIDX_WIDTH-1:0]                   lookup_req_idx_i,
    input  logic [23:0]                                 lookup_req_device_id_i,
    output logic                                        lookup_ack_valid_o,
    output logic                                        lookup_ack_hit_o,
    output logic [TLB_QIDX_WIDTH-1:0]                   lookup_ack_idx_o,
    output logic                                        lookup_ack_prefetched_o,
    output logic [1:0]                                  lookup_ack_lvl_o,
    output logic [511:0]                                lookup_ack_o,
    // update
    input  logic                                        update_req_valid_i,
    output logic                                        update_req_ready_o,
    input  logic [TLB_QIDX_WIDTH-1:0]                   update_req_idx_i,
    input  logic [1:0]                                  update_req_lvl_i,
    input  logic                                        update_req_prefetched_i,
    input  logic [511:0]                                update_req_i,
    input  logic [23:0]                                 update_req_device_id_i,
    output logic                                        update_ack_valid_o,
    output logic [TLB_QIDX_WIDTH-1:0]                   update_ack_idx_o,
    //  
    input  logic                                        multi_hit_check_i,
    output logic                                        multi_hit_fault_o,
    output logic [1:0]                                  ecc_err_o,
    //
    input  logic                                        spare_in
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
    localparam MAX_BANK_SET_NUM         = 2**MAX_BANK_SET_IDX_WIDTH;

    localparam MAX_BANK_WAY_IDX_WIDTH_0 = (BANK_L0_WAY_IDX_WIDTH > BANK_L1_WAY_IDX_WIDTH) ? BANK_L0_WAY_IDX_WIDTH : BANK_L1_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH_1 = BANK_L2_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH   = (MAX_BANK_WAY_IDX_WIDTH_0 > MAX_BANK_WAY_IDX_WIDTH_1) ? MAX_BANK_WAY_IDX_WIDTH_0 : MAX_BANK_WAY_IDX_WIDTH_1;
    localparam MAX_BANK_WAY_NUM         = 2**MAX_BANK_WAY_IDX_WIDTH;

    localparam TECC_WIDTH   = (ECC_ENABLE==0) ? 0 : 5;
    localparam NLDECC_WIDTH = (ECC_ENABLE==0) ? 0 : 6;
    localparam LDECC_WIDTH  = (ECC_ENABLE==0) ? 0 : 9;

//{{{ struct
    typedef struct packed {
        logic [INV_IDX_WIDTH-1:0]           idx;            // invalid cmd idx
        logic [1:0]                         itype;          // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
        logic                               dv_gv;
        logic [23:0]                        did_gscid;
        logic                               pscv;
        logic [19:0]                        pid_pscid;
        logic                               av;
        logic [63:12]                       addr;
    } INVALID_REQ_TYPE;

    typedef struct packed {
        logic [TLB_QIDX_WIDTH-1:0]          idx;
        logic [23:0]                        device_id;
    } lookup_req_t;
    
    typedef struct packed {
        logic [TLB_QIDX_WIDTH-1:0]          idx;
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
    } lookup_ack_t;
    
    typedef struct packed {
        logic [TLB_QIDX_WIDTH:0]            idx;
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
    } update_req_t;

    typedef struct packed {                             
        logic                                           valid;
        logic                                           prefetched;
        logic [23:0]                                    device_id;
    } mtlb_tag_t;                                       

    typedef struct packed {
        logic                                           rsv;
    } mtlb_itag_t;

    typedef struct packed {
        logic [MAX_BANK_WAY_NUM-2:0]                    plru_list;
    } mtlb_utag_t;

    typedef struct packed {                             
        logic [43:0]                                    fsc_ppn;
        logic                                           V;
    } mtlb_dat_t_nl;                                     

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
    } mtlb_dat_t_l;                                     


    typedef struct packed {                             
        logic                                           prefetched;
        logic [23:0]                                    device_id;
    } microtlb_tag_t;                                            
                                                        
    typedef struct packed {                             
        logic                                           rsv;
    } microtlb_inv_tag_t;                                        
                                                        
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
    } microtlb_content_t;                                        

//}}}

    logic                                               tc_lookup_req_valid_i ;
    logic                                               tc_lookup_req_ready_o ;
    lookup_req_t                                        tc_lookup_req_i       ;
    logic                                               tc_lookup_ack_valid_o ;
    lookup_ack_t                                        tc_lookup_ack_o       ;
    logic                                               tc_update_req_valid_i ;
    logic                                               tc_update_req_ready_o ;
    update_req_t                                        tc_update_req_i       ;
    logic                                               tc_update_ack_valid_o ;
    update_req_t                                        tc_update_ack_o       ;
    logic                                               tc_invalid_req_valid_i;
    logic                                               tc_invalid_req_ready_o;
    INVALID_REQ_TYPE                                    tc_invalid_req_i      ;
    logic                                               tc_invalid_ack_valid_o;
    INVALID_REQ_TYPE                                    tc_invalid_ack_o      ;
    logic                                               tc_ram_initial_done_o ;

//}}}
    
//=== Main Code === {{{
    assign multi_hit_fault_o = 'd0;
//=== LOOKUP {{{
    assign tc_lookup_req_valid_i            = tc_ram_initial_done_o & lookup_req_valid_i;
    assign lookup_req_ready_o               = tc_ram_initial_done_o & tc_lookup_req_ready_o;
    assign tc_lookup_req_i.idx              = lookup_req_idx_i;
    assign tc_lookup_req_i.device_id        = lookup_req_device_id_i;
                                            
    assign lookup_ack_valid_o               = tc_lookup_ack_valid_o;
    assign lookup_ack_idx_o                 = tc_lookup_ack_o.idx;
    assign lookup_ack_hit_o                 = tc_lookup_ack_o.hit;
    assign lookup_ack_lvl_o                 = tc_lookup_ack_o.lvl;
    assign lookup_ack_prefetched_o          = tc_lookup_ack_o.prefetched;
    assign lookup_ack_o                     = (tc_lookup_ack_o.lvl=='d0) ? {  
                                                //reserved
                                                64'd0,
                                                //msi_addr_pattern
                                                12'd0,
                                                tc_lookup_ack_o.msi_addr_pattern,
                                                //msi_addr_mask
                                                12'd0,
                                                tc_lookup_ack_o.msi_addr_mask,
                                                //msiptp
                                                tc_lookup_ack_o.msipip_mode,
                                                16'd0,
                                                tc_lookup_ack_o.msipip_ppn,
                                                //fsc
                                                tc_lookup_ack_o.fsc_mode,
                                                16'd0,
                                                tc_lookup_ack_o.fsc_ppn,
                                                //ta
                                                32'd0,
                                                tc_lookup_ack_o.PSCID,
                                                12'd0,
                                                //iohgatp
                                                tc_lookup_ack_o.S2MODE,
                                                tc_lookup_ack_o.GSCID,
                                                tc_lookup_ack_o.S2PPN,
                                                // tc
                                                16'd0,
                                                16'd0,
                                                8'd0, 8'd0,
                                                4'b0,
                                                tc_lookup_ack_o.SXL,
                                                tc_lookup_ack_o.SBE,
                                                tc_lookup_ack_o.DPE,
                                                tc_lookup_ack_o.SADE,
                                                tc_lookup_ack_o.GADE,
                                                tc_lookup_ack_o.PRPR,
                                                tc_lookup_ack_o.PDTV,
                                                tc_lookup_ack_o.DTF,
                                                tc_lookup_ack_o.T2GPA,
                                                tc_lookup_ack_o.EN_PRI,
                                                tc_lookup_ack_o.EN_ATS,
                                                tc_lookup_ack_o.V
                                                }
                                                :
                                                {
                                                448'd0,
                                                10'd0,
                                                tc_lookup_ack_o.fsc_ppn,
                                                9'd0,
                                                tc_lookup_ack_o.V
                                                }
                                                ;

//}}}

//=== UPDATE {{{
    assign tc_update_req_valid_i            = tc_ram_initial_done_o & update_req_valid_i;
    assign update_req_ready_o               = tc_ram_initial_done_o & tc_update_req_ready_o;
    assign tc_update_req_i.idx              = {1'b0, update_req_idx_i};
    assign tc_update_req_i.lvl              = update_req_lvl_i;
    assign tc_update_req_i.prefetched       = update_req_prefetched_i;
    assign tc_update_req_i.device_id        = update_req_device_id_i;
    assign tc_update_req_i.msi_addr_pattern = update_req_lvl_i=='d0 ? update_req_i[384+51:384+0]    : 'd0;
    assign tc_update_req_i.msi_addr_mask    = update_req_lvl_i=='d0 ? update_req_i[320+51:320+0]    : 'd0;
    assign tc_update_req_i.msipip_mode      = update_req_lvl_i=='d0 ? update_req_i[256+63:256+60]   : 'd0;
    assign tc_update_req_i.msipip_ppn       = update_req_lvl_i=='d0 ? update_req_i[256+43:256+0]    : 'd0;
    assign tc_update_req_i.fsc_mode         = update_req_lvl_i=='d0 ? update_req_i[192+63:192+60]   : 'd0;
    assign tc_update_req_i.fsc_ppn          = update_req_lvl_i=='d0 ? update_req_i[192+43:192+0]    : update_req_i[53:10];
    assign tc_update_req_i.PSCID            = update_req_lvl_i=='d0 ? update_req_i[128+31:128+12]   : 'd0;
    assign tc_update_req_i.S2MODE           = update_req_lvl_i=='d0 ? update_req_i[64+63:64+60]     : 'd0;
    assign tc_update_req_i.GSCID            = update_req_lvl_i=='d0 ? update_req_i[64+59:64+44]     : 'd0;
    assign tc_update_req_i.S2PPN            = update_req_lvl_i=='d0 ? update_req_i[64+43:64+0]      : 'd0;
    assign tc_update_req_i.SXL              = update_req_lvl_i=='d0 ? update_req_i[11]              : 'd0;
    assign tc_update_req_i.SBE              = update_req_lvl_i=='d0 ? update_req_i[10]              : 'd0;
    assign tc_update_req_i.DPE              = update_req_lvl_i=='d0 ? update_req_i[9]               : 'd0;
    assign tc_update_req_i.SADE             = update_req_lvl_i=='d0 ? update_req_i[8]               : 'd0;
    assign tc_update_req_i.GADE             = update_req_lvl_i=='d0 ? update_req_i[7]               : 'd0;
    assign tc_update_req_i.PRPR             = update_req_lvl_i=='d0 ? update_req_i[6]               : 'd0;
    assign tc_update_req_i.PDTV             = update_req_lvl_i=='d0 ? update_req_i[5]               : 'd0;
    assign tc_update_req_i.DTF              = update_req_lvl_i=='d0 ? update_req_i[4]               : 'd0;
    assign tc_update_req_i.T2GPA            = update_req_lvl_i=='d0 ? update_req_i[3]               : 'd0;
    assign tc_update_req_i.EN_PRI           = update_req_lvl_i=='d0 ? update_req_i[2]               : 'd0;
    assign tc_update_req_i.EN_ATS           = update_req_lvl_i=='d0 ? update_req_i[1]               : 'd0;
    assign tc_update_req_i.V                = update_req_i[0];

    assign update_ack_valid_o               = tc_update_ack_valid_o & ~tc_update_ack_o.idx[TLB_QIDX_WIDTH];
    assign update_ack_idx_o                 = tc_update_ack_o.idx[TLB_QIDX_WIDTH-1:0];
//}}}

//=== INVALID {{{
    assign tc_invalid_req_valid_i           = tc_ram_initial_done_o & invalid_req_valid_i;
    assign invalid_req_ready_o              = tc_ram_initial_done_o & tc_invalid_req_ready_o;
    assign tc_invalid_req_i.idx             = invalid_req_idx_i;
    assign tc_invalid_req_i.itype           = invalid_req_type_i;
    assign tc_invalid_req_i.dv_gv           = invalid_req_dv_gv_i;
    assign tc_invalid_req_i.did_gscid       = invalid_req_did_gscid_i;
    assign tc_invalid_req_i.pscv            = invalid_req_pscv_i;
    assign tc_invalid_req_i.pid_pscid       = invalid_req_pid_pscid_i;
    assign tc_invalid_req_i.av              = invalid_req_av_i;
    assign tc_invalid_req_i.addr            = invalid_req_addr_i;
    assign invalid_ack_valid_o              = tc_invalid_ack_valid_o;
    assign invalid_ack_idx_o                = tc_invalid_ack_o.idx;
//}}}

//}}}

//=== Inst === {{{
    iommu_atd_dtc_top #(
    /*parameter */              .CACHE_TYPE                 (1'b0                   ), // = 0, // 0:DDTC, 1:PDTC0
    /*parameter */              .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE       ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE            (lookup_req_t           ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (lookup_ack_t           ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE            (update_req_t           ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .MTLB_TAG_TYPE              (mtlb_tag_t             ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE             (mtlb_itag_t            ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE             (mtlb_utag_t            ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE_NL           (mtlb_dat_t_nl          ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_nl,
    /*parameter type         */ .MTLB_DAT_TYPE_L            (mtlb_dat_t_l           ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    /*parameter type         */ .MICROTLB_TAG_TYPE          (microtlb_tag_t         ), // = iommu_atd_cache_pkg::ddtc_microtlb_tag_t,
    /*parameter type         */ .MICROTLB_INV_TAG_TYPE      (microtlb_inv_tag_t     ), // = iommu_atd_cache_pkg::ddtc_microtlb_inv_tag_t,
    /*parameter type         */ .MICROTLB_CONTENT_TYPE      (microtlb_content_t     ), // = iommu_atd_cache_pkg::ddtc_microtlb_content_t,
    /*parameter */              .MICRO_TLB_IDX_WIDTH        (MICRO_TLB_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH,
    /*parameter */              .CABIN_LKP_IDX_WIDTH        (CABIN_LKP_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    /*parameter */              .CABIN_UPD_IDX_WIDTH        (CABIN_UPD_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    /*parameter */              .CABIN_INV_IDX_WIDTH        (CABIN_INV_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    /*parameter */              .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    /*parameter */              .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    /*parameter */              .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    /*parameter */              .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter */              .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter */              .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter */              .BANK_L0_WAY_IDX_WIDTH      (BANK_L0_WAY_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter */              .BANK_L1_WAY_IDX_WIDTH      (BANK_L1_WAY_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter */              .BANK_L2_WAY_IDX_WIDTH      (BANK_L2_WAY_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter */              .TECC_WIDTH                 (TECC_WIDTH             ), //= 5,
    /*parameter */              .LDECC_WIDTH                (LDECC_WIDTH            ), //= 9,
    /*parameter */              .NLDECC_WIDTH               (NLDECC_WIDTH           ), //= 6,
    /*parameter */              .SPARE_PARAM                (1'b0                   )  // = 0 
    ) U_dtc_top(
    /*input  logic                                      */  .clk                    (clk                    ),
    /*input  logic                                      */  .rstn                   (rstn                   ),
    /*input  logic                                      */  .lookup_req_valid_i     (tc_lookup_req_valid_i  ),
    /*output logic                                      */  .lookup_req_ready_o     (tc_lookup_req_ready_o  ),
    /*input  LOOKUP_REQ_TYPE                            */  .lookup_req_i           (tc_lookup_req_i        ),
    /*output logic                                      */  .lookup_ack_valid_o     (tc_lookup_ack_valid_o  ),
    /*output LOOKUP_ACK_TYPE                            */  .lookup_ack_o           (tc_lookup_ack_o        ),
    /*input  logic                                      */  .update_req_valid_i     (tc_update_req_valid_i  ),
    /*output logic                                      */  .update_req_ready_o     (tc_update_req_ready_o  ),
    /*input  UPDATE_REQ_TYPE                            */  .update_req_i           (tc_update_req_i        ),
    /*output logic                                      */  .update_ack_valid_o     (tc_update_ack_valid_o  ),
    /*output UPDATE_REQ_TYPE                            */  .update_ack_o           (tc_update_ack_o        ),
    /*input  logic                                      */  .invalid_req_valid_i    (tc_invalid_req_valid_i ),
    /*output logic                                      */  .invalid_req_ready_o    (tc_invalid_req_ready_o ),
    /*input  INVALID_REQ_TYPE                           */  .invalid_req_i          (tc_invalid_req_i       ),
    /*output logic                                      */  .invalid_ack_valid_o    (tc_invalid_ack_valid_o ),
    /*output INVALID_REQ_TYPE                           */  .invalid_ack_o          (tc_invalid_ack_o       ),
    /*output logic                                      */  .ram_initial_done_o     (tc_ram_initial_done_o  ),
    /*input  logic                                      */  .multi_hit_check_i      (1'b1                   ),
    /*output logic                                      */  .multi_hit_fault_o      (                       ),
    /*output logic [1:0]                                */  .ecc_err_o              (ecc_err_o              ),
    /*input  logic                                      */  .spare_in               (1'b0                   ) 
    );

//}}}

endmodule
