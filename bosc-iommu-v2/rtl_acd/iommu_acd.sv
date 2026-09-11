
module iommu_acd #(
//{{{ PARAM
    parameter WBUF_FIFO_TYPE              = 1'b1, // 0:FF type, 1:RAM type
    parameter SLV_AW_REGSLICE             = iommu_acd_pkg::SLV_AW_REGSLICE,
    parameter SLV_W_REGSLICE              = iommu_acd_pkg::SLV_W_REGSLICE,
    parameter SLV_AR_REGSLICE             = iommu_acd_pkg::SLV_AR_REGSLICE,
    parameter SLV_R_REGSLICE              = iommu_acd_pkg::SLV_R_REGSLICE,
    parameter SLV_B_REGSLICE              = iommu_acd_pkg::SLV_B_REGSLICE,
    parameter MST_AW_REGSLICE             = iommu_acd_pkg::MST_AW_REGSLICE,
    parameter MST_W_REGSLICE              = iommu_acd_pkg::MST_W_REGSLICE,
    parameter MST_AR_REGSLICE             = iommu_acd_pkg::MST_AR_REGSLICE,
    parameter MST_R_REGSLICE              = iommu_acd_pkg::MST_R_REGSLICE,
    parameter MST_B_REGSLICE              = iommu_acd_pkg::MST_B_REGSLICE,
    parameter BUS_INFLY_TOKEN_WIDTH       = iommu_acd_pkg::BUS_INFLY_TOKEN_WIDTH,
    parameter BUS_INFLY_TOKEN_NUM         = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter BUS_ADDR_WIDTH              = 64,                               // should never change to any other value
    parameter BUS_DATA_WIDTH              = iommu_acd_pkg::BUS_DATA_WIDTH,
    parameter BUS_SIZE_WIDTH              = 3,                                // should nover change to any other value
    parameter BUS_STRB_WIDTH              = BUS_DATA_WIDTH/8,
    parameter BUS_ID_WIDTH                = iommu_acd_pkg::BUS_ID_WIDTH,
    parameter BUS_USER_WIDTH              = iommu_acd_pkg::BUS_USER_WIDTH,
    parameter BUS_LOOP_WIDTH              = iommu_acd_pkg::BUS_LOOP_WIDTH,
    parameter INV_IDX_WIDTH               = iommu_acd_pkg::INV_IDX_WIDTH,     // should not bigger than 4
    parameter INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter FAULT_TOKEN_WIDTH           = iommu_acd_pkg::FAULT_TOKEN_WIDTH, // should not bigger than 12
    parameter FAULT_INFLY_NUM             = 2**FAULT_TOKEN_WIDTH,
    parameter PTW_IDX_WIDTH               = iommu_acd_pkg::PTW_IDX_WIDTH,     // should not bigger than 8
    parameter PTW_INFLY_NUM               = 2**PTW_IDX_WIDTH,
    parameter TRANS_QIDX_WIDTH            = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter TRANS_QUEUE_DEPTH           = 2**TRANS_QIDX_WIDTH,
    parameter TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,    // should not bigger than PTW_IDX_WIDTH-1
    parameter TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter MICRO_TLB_IDX_WIDTH         = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    parameter CABIN_LKP_IDX_WIDTH         = iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    parameter CABIN_UPD_IDX_WIDTH         = iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    parameter CABIN_INV_IDX_WIDTH         = iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
    parameter BANK_4K_IDX_WIDTH           = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    parameter BANK_2M_IDX_WIDTH           = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    parameter BANK_1G_IDX_WIDTH           = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    parameter BANK_0T_IDX_WIDTH           = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    parameter BANK_4K_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    parameter BANK_2M_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    parameter BANK_1G_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    parameter BANK_0T_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    parameter BANK_4K_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    parameter BANK_2M_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    parameter BANK_1G_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    parameter BANK_0T_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    parameter CTIF_DATA_WIDTH             = 64,                               // can not change
    parameter CTIF_STRB_WIDTH             = CTIF_DATA_WIDTH/8,
    parameter CTIF_ID_WIDTH               = 4,
    parameter CTIF_DEST_WIDTH             = 4,
    parameter CTIF_USER_WIDTH             = 1,
    parameter SPARE_PARAM                 = 0
//}}}
)(
//{{{ IO
    input logic                                         clk,
    input logic                                         rstn,
    // AXI SLV Port                                     
    // AW
    input  logic                                        slv_awvalid_i,
    output logic                                        slv_awready_o,
    input  logic [BUS_ID_WIDTH-1:0]                     slv_awid_i,
    input  logic [64-1:0]                               slv_awaddr_i,
    input  logic [ 7:0]                                 slv_awlen_i,
    input  logic [BUS_SIZE_WIDTH-1:0]                   slv_awsize_i,
    input  logic [ 1:0]                                 slv_awburst_i,
    input  logic                                        slv_awlock_i,
    input  logic [ 3:0]                                 slv_awcache_i,
    input  logic [ 2:0]                                 slv_awprot_i,
    input  logic [ 3:0]                                 slv_awregion_i,
    input  logic [BUS_USER_WIDTH-1:0]                   slv_awuser_i,
    input  logic [ 3:0]                                 slv_awqos_i,
    input  logic [ 3:0]                                 slv_awsnoop_i,
    input  logic [ 1:0]                                 slv_awdomain_i,
//    input  logic [ 1:0]                                 slv_awbar_i,
    input  logic                                        slv_awidunq_i,
    input  logic [ 5:0]                                 slv_awatop_i,
    input  logic [BUS_LOOP_WIDTH-1:0]                   slv_awloop_i,
    input  logic [23:0]                                 slv_aw_device_id_i,
    input  logic [19:0]                                 slv_aw_process_id_i,
    input  logic                                        slv_aw_process_id_valid_i,
    input  logic                                        slv_aw_is_translated_i,
    // W
    input  logic                                        slv_wvalid_i,
    output logic                                        slv_wready_o,
    input  logic [BUS_DATA_WIDTH-1:0]                   slv_wdata_i,
    input  logic [BUS_STRB_WIDTH-1:0]                   slv_wstrb_i,
    input  logic                                        slv_wlast_i,
    input  logic [BUS_USER_WIDTH-1:0]                   slv_wuser_i,
    // B
    output logic                                        slv_bvalid_o,
    input  logic                                        slv_bready_i,
    output logic [BUS_ID_WIDTH-1:0]                     slv_bid_o,
    output logic [ 1:0]                                 slv_bresp_o,
    output logic [BUS_USER_WIDTH-1:0]                   slv_buser_o,
    output logic                                        slv_bidunq_o,
    output logic [BUS_LOOP_WIDTH-1:0]                   slv_bloop_o,
    // AR
    input  logic                                        slv_arvalid_i,
    output logic                                        slv_arready_o,
    input  logic [BUS_ID_WIDTH-1:0]                     slv_arid_i,
    input  logic [64-1:0]                               slv_araddr_i,
    input  logic [ 7:0]                                 slv_arlen_i,
    input  logic [BUS_SIZE_WIDTH-1:0]                   slv_arsize_i,
    input  logic [ 1:0]                                 slv_arburst_i,
    input  logic                                        slv_arlock_i,
    input  logic [ 3:0]                                 slv_arcache_i,
    input  logic [ 2:0]                                 slv_arprot_i,
    input  logic [ 3:0]                                 slv_arregion_i,
    input  logic [ BUS_USER_WIDTH-1:0]                  slv_aruser_i,
    input  logic [ 3:0]                                 slv_arqos_i,
    input  logic [ 3:0]                                 slv_arsnoop_i,
    input  logic [ 1:0]                                 slv_ardomain_i,
//    input  logic [ 1:0]                                 slv_arbar_i,
    input  logic                                        slv_aridunq_i,
//    input  logic [ 5:0]                                 slv_aratop_i, // no ATOP in AR
    input  logic [BUS_LOOP_WIDTH-1:0]                   slv_arloop_i,
    input  logic [23:0]                                 slv_ar_device_id_i,
    input  logic [19:0]                                 slv_ar_process_id_i,
    input  logic                                        slv_ar_process_id_valid_i,
    input  logic                                        slv_ar_is_translated_i,
    output logic                                        slv_rvalid_o,
    input  logic                                        slv_rready_i,
    output logic [BUS_ID_WIDTH-1:0]                     slv_rid_o,
    output logic [BUS_DATA_WIDTH-1:0]                   slv_rdata_o,
    output logic [ 1:0]                                 slv_rresp_o,
    output logic                                        slv_rlast_o,
    output logic [BUS_USER_WIDTH-1:0]                   slv_ruser_o,
    output logic                                        slv_ridunq_o,
    output logic [BUS_LOOP_WIDTH-1:0]                   slv_rloop_o,
    // AXI MST Port                                     
    // AW
    output logic                                        mst_awvalid_o,
    input  logic                                        mst_awready_i,
    output logic [BUS_ID_WIDTH-1:0]                     mst_awid_o,
    output logic [64-1:0]                               mst_awaddr_o,
    output logic [ 7:0]                                 mst_awlen_o,
    output logic [BUS_SIZE_WIDTH-1:0]                   mst_awsize_o,
    output logic [ 1:0]                                 mst_awburst_o,
    output logic                                        mst_awlock_o,
    output logic [ 3:0]                                 mst_awcache_o,
    output logic [ 2:0]                                 mst_awprot_o,
    output logic [ 3:0]                                 mst_awregion_o,
    output logic [BUS_USER_WIDTH-1:0]                   mst_awuser_o,
    output logic [ 3:0]                                 mst_awqos_o,
    output logic [ 3:0]                                 mst_awsnoop_o,
    output logic [ 1:0]                                 mst_awdomain_o,
//    output logic [ 1:0]                                 mst_awbar_o,
    output logic                                        mst_awidunq_o,
    output logic [ 5:0]                                 mst_awatop_o,
    output logic [BUS_LOOP_WIDTH-1:0]                   mst_awloop_o,
    // W
    output logic                                        mst_wvalid_o,
    input  logic                                        mst_wready_i,
    output logic [BUS_DATA_WIDTH-1:0]                   mst_wdata_o,
    output logic [BUS_STRB_WIDTH-1:0]                   mst_wstrb_o,
    output logic                                        mst_wlast_o,
    output logic [BUS_USER_WIDTH-1:0]                   mst_wuser_o,
    // B
    input  logic                                        mst_bvalid_i,
    output logic                                        mst_bready_o,
    input  logic [BUS_ID_WIDTH-1:0]                     mst_bid_i,
    input  logic [ 1:0]                                 mst_bresp_i,
    input  logic [BUS_USER_WIDTH-1:0]                   mst_buser_i,
    input  logic                                        mst_bidunq_i,
    input  logic [BUS_LOOP_WIDTH-1:0]                   mst_bloop_i,
    // AR
    output logic                                        mst_arvalid_o,
    input  logic                                        mst_arready_i,
    output logic [BUS_ID_WIDTH-1:0]                     mst_arid_o,
    output logic [64-1:0]                               mst_araddr_o,
    output logic [ 7:0]                                 mst_arlen_o,
    output logic [BUS_SIZE_WIDTH-1:0]                   mst_arsize_o,
    output logic [ 1:0]                                 mst_arburst_o,
    output logic                                        mst_arlock_o,
    output logic [ 3:0]                                 mst_arcache_o,
    output logic [ 2:0]                                 mst_arprot_o,
    output logic [ 3:0]                                 mst_arregion_o,
    output logic [ BUS_USER_WIDTH-1:0]                  mst_aruser_o,
    output logic [ 3:0]                                 mst_arqos_o,
    output logic [ 3:0]                                 mst_arsnoop_o,
    output logic [ 1:0]                                 mst_ardomain_o,
//    output logic [ 1:0]                                 mst_arbar_o,
    output logic                                        mst_aridunq_o,
    output logic [BUS_LOOP_WIDTH-1:0]                   mst_arloop_o,
    // R
    input  logic                                        mst_rvalid_i,
    output logic                                        mst_rready_o,
    input  logic [BUS_ID_WIDTH-1:0]                     mst_rid_i,
    input  logic [BUS_DATA_WIDTH-1:0]                   mst_rdata_i,
    input  logic [ 1:0]                                 mst_rresp_i,
    input  logic                                        mst_rlast_i,
    input  logic [BUS_USER_WIDTH-1:0]                   mst_ruser_i,
    input  logic                                        mst_ridunq_i,
    input  logic [BUS_LOOP_WIDTH-1:0]                   mst_rloop_i,
    // C2T                                              
    output logic                                        c2t_tvalid_o,
    input  logic                                        c2t_tready_i,
    output logic [CTIF_DATA_WIDTH-1:0]                  c2t_tdata_o,
    output logic [CTIF_STRB_WIDTH-1:0]                  c2t_tstrb_o,
    output logic [CTIF_STRB_WIDTH-1:0]                  c2t_tkeep_o,
    output logic                                        c2t_tlast_o,
    output logic [CTIF_ID_WIDTH-1:0]                    c2t_tid_o,
    output logic [CTIF_DEST_WIDTH-1:0]                  c2t_tdest_o,
    output logic [CTIF_USER_WIDTH-1:0]                  c2t_tuser_o,
    // T2C                                              
    input  logic                                        t2c_tvalid_i,
    output logic                                        t2c_tready_o,
    input  logic [CTIF_DATA_WIDTH-1:0]                  t2c_tdata_i,
    input  logic [CTIF_STRB_WIDTH-1:0]                  t2c_tstrb_i,
    input  logic [CTIF_STRB_WIDTH-1:0]                  t2c_tkeep_i,
    input  logic                                        t2c_tlast_i,
    input  logic [CTIF_ID_WIDTH-1:0]                    t2c_tid_i,
    input  logic [CTIF_DEST_WIDTH-1:0]                  t2c_tdest_i,
    input  logic [CTIF_USER_WIDTH-1:0]                  t2c_tuser_i,
    // TID and TDEST                                    
    input  logic [CTIF_ID_WIDTH-1:0]                    acd_tid_i,
    input  logic [CTIF_DEST_WIDTH-1:0]                  acd_tdest_i,
`ifdef IOMMU_IDBG
    // IDBG                                             
    input  logic                                        idbg_psel_i   ,
    input  logic                                        idbg_penable_i,
    output logic                                        idbg_pready_o ,
    input  logic                                        idbg_pwrite_i ,
    input  logic [11:0]                                 idbg_paddr_i  ,
    input  logic [31:0]                                 idbg_pwdata_i ,
    output logic [31:0]                                 idbg_prdata_o ,
    output logic                                        idbg_pslverr_o,
    input  logic [63:0]                                 idbg_scnt_i,
`endif
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
//=== internal struct define {{{
    typedef struct packed {
        logic [BUS_ID_WIDTH-1:0]                    axid    ;
        logic [BUS_ADDR_WIDTH-1:0]                  axaddr  ;
        logic [ 7:0]                                axlen   ;
        logic [BUS_SIZE_WIDTH-1:0]                  axsize  ;
        logic [ 1:0]                                axburst ;
        logic                                       axlock  ;
        logic [ 3:0]                                axcache ;
        logic [ 2:0]                                axprot  ;
        logic [ 3:0]                                axregion;
        logic [BUS_USER_WIDTH-1:0]                  axuser  ;
        logic [ 3:0]                                axqos   ;
        logic [ 3:0]                                axsnoop ;
        logic [ 1:0]                                axdomain;
        logic [ 1:0]                                axbar   ;
        logic                                       axidunq ;
        logic [ 5:0]                                axatop  ;
        logic [BUS_LOOP_WIDTH-1:0]                  axloop  ;
    } ch_ax_t;
    typedef struct packed {
        logic [BUS_DATA_WIDTH-1:0]                  wdata   ;
        logic [BUS_STRB_WIDTH-1:0]                  wstrb   ;
        logic                                       wlast   ;
        logic [BUS_USER_WIDTH-1:0]                  wuser   ;
    } ch_w_t;
    typedef struct packed {
        logic [BUS_ID_WIDTH-1:0]                    bid     ;
        logic [ 1:0]                                bresp   ;
        logic [BUS_USER_WIDTH-1:0]                  buser   ;
        logic                                       bidunq  ;
        logic [BUS_LOOP_WIDTH-1:0]                  bloop   ;
    } ch_b_t;
    typedef struct packed {
        logic [BUS_ID_WIDTH-1:0]                    rid     ;
        logic [BUS_DATA_WIDTH-1:0]                  rdata   ;
        logic [ 1:0]                                rresp   ;
        logic                                       rlast   ;
        logic [BUS_USER_WIDTH-1:0]                  ruser   ;
        logic                                       ridunq  ;
        logic [BUS_LOOP_WIDTH-1:0]                  rloop   ;
    } ch_r_t;


    localparam INTERNAL_INV_IDX_WIDTH = (TLB_QIDX_WIDTH > 4) ? TLB_QIDX_WIDTH : 4;

    typedef struct packed {
        logic [TRANS_QIDX_WIDTH:0]                      idx;            // bit[TRANS_QIDX_WIDTH]==1, indicates the debug request
        logic                                           priv;
        logic                                           ext;
        logic                                           wr;
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } TRANSLATE_REQ_TYPE ;
                                                        
    typedef struct packed {
        logic [TRANS_QIDX_WIDTH:0]                      idx;            // bit[TRANS_QIDX_WIDTH]==1, indicates the debug request
        logic [1:0]                                     resp;
        logic [1:0]                                     pbmt;
        logic                                           mrif;
        logic [10:0]                                    nid;
        logic [55:12]                                   nppn;
        logic [2:0]                                     trange;          // 3'b111 : iommu_bypass or S1/S2 both BARE; 3'b100: Svnapot64K; 3'b000: 4K; 3'b001: 2M/4M; 3'b010: 1G/4G; 3'b011:512G
        logic [63:12]                                   pa;
    } TRANSLATE_ACK_TYPE;
                                                        
    typedef struct packed {
        logic [INTERNAL_INV_IDX_WIDTH:0]                idx;                // invalid cmd idx, highest bit indicates tlb_queue's internal invalid_req or not
        logic [1:0]                                     itype;              // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
        logic                                           dv_gv;
        logic [23:0]                                    did_gscid;
        logic                                           pscv;
        logic [19:0]                                    pid_pscid;
        logic                                           av;
        logic [63:12]                                   addr;
    } INVALID_REQ_TYPE;
                                                        
    typedef struct packed {
        logic [TLB_QIDX_WIDTH:0]                        idx;            // bit[TLB_QIDX_WIDTH], indicates the debug request
        logic                                           priv;
        logic                                           ext;
        logic                                           wr;
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } PTW_REQ_TYPE;
                                                        
    typedef struct packed {
        logic [TLB_QIDX_WIDTH:0]                        idx;            // bit[TLB_QIDX_WIDTH], indicates the debug request
        logic [2:0]                                     opcode;         // bit[1:0] 00: translate&permission success 01: translate success but permission fail 10: translate fail, bit[2] 1: do not cache
        logic                                           MRIF;
        logic [1:0]                                     PBMT;
        logic [61:12]                                   GPPN;           // if MRIF=1: {NID[10:5], NPPN}
        logic                                           ENATS;
        logic                                           T2GPA;
        logic                                           DTF;
        logic                                           PDTV;
        logic                                           DPE;
        logic                                           SXL;
        logic                                           ENS;
        logic                                           SUM;
        logic                                           S1_D;
        logic                                           S2_D;
        logic                                           SADE;
        logic                                           GADE;
        logic                                           N;
//        logic [15:0]                                    S1_PERM_D;
//        logic [15:0]                                    S1_PERM_A;
        logic [4:0]                                     S1_PERM;        // G U X W R
//        logic [15:0]                                    S2_PERM_D;
//        logic [15:0]                                    S2_PERM_A;
        logic [4:0]                                     S2_PERM;        // G U X W R
        logic [1:0]                                     S1SIZE;         // 00:4K; 01:2M; 10:1G; 11:512G
        logic [1:0]                                     S2SIZE;         // 
        logic [3:0]                                     S1MODE;
        logic [3:0]                                     S2MODE;
        logic [3:0]                                     PDTMODE;
        logic [15:0]                                    GSCID;
        logic [19:0]                                    PSCID;
        logic [63:12]                                   PPN;            // if MRIF=1: {NID[4:0], MRIF_Addr}
    } PTW_ACK_TYPE;
                                                        
   typedef struct packed {
        logic [TLB_QUEUE_DEPTH-1:0]                     valid;
        logic [TLB_QUEUE_DEPTH-1:0]                     wr;
    } tlbq2inv_t;
                                                        
    typedef struct packed {
        PTW_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]              ptw_req;
    } ptw_req_grp_t;
                                                        
    typedef struct packed {
        logic [TLB_QIDX_WIDTH:0]                        idx;            // bit[TLB_QIDX_WIDTH] indicates the DEBUG request
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } lookup_req_t;
                                                        
    typedef struct packed {
        logic [TLB_QIDX_WIDTH:0]                        idx;            // bit[TLB_QIDX_WIDTH] indicates the DEBUG request
        logic                                           hit;
        logic [1:0]                                     PBMT;
        logic [61:12]                                   GPPN;
        logic                                           ENATS;
        logic                                           T2GPA;
        logic                                           DTF;
        logic                                           PDTV;
        logic                                           DPE;
        logic                                           SXL;
        logic                                           ENS;
        logic                                           SUM;
        logic                                           S1_D;
        logic                                           S2_D;
        logic                                           SADE;
        logic                                           GADE;
        logic                                           N;
//        logic [15:0]                                    S1_PERM_D;
//        logic [15:0]                                    S1_PERM_A;
        logic [4:0]                                     S1_PERM;        // G U X W R
//        logic [15:0]                                    S2_PERM_D;
//        logic [15:0]                                    S2_PERM_A;
        logic [4:0]                                     S2_PERM;        // G U X W R
        logic [1:0]                                     S1SIZE;         // 00:4K; 01:2M; 10:1G; 11:512G
        logic [1:0]                                     S2SIZE;         // 
        logic [3:0]                                     S1MODE;
        logic [3:0]                                     S2MODE;
        logic [3:0]                                     PDTMODE;
        logic [19:0]                                    PSCID;
        logic [15:0]                                    GSCID;
        logic [63:12]                                   PPN;
    } lookup_ack_t;                                     
    typedef struct packed {                             
        logic [TLB_QIDX_WIDTH:0]                        idx;
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [1:0]                                     PBMT;
        logic [61:12]                                   GPPN;
        logic                                           ENATS;
        logic                                           T2GPA;
        logic                                           DTF;
        logic                                           PDTV;
        logic                                           DPE;
        logic                                           SXL;
        logic                                           ENS;
        logic                                           SUM;
        logic                                           S1_D;
        logic                                           S2_D;
        logic                                           SADE;
        logic                                           GADE;
        logic                                           N;
//        logic [15:0]                                    S1_PERM_D;
//        logic [15:0]                                    S1_PERM_A;
        logic [4:0]                                     S1_PERM;        // G U X W R
//        logic [15:0]                                    S2_PERM_D;
//        logic [15:0]                                    S2_PERM_A;
        logic [4:0]                                     S2_PERM;        // G U X W R
        logic [1:0]                                     S1SIZE;         // 00:4K; 01:2M; 10:1G; 11:512G
        logic [1:0]                                     S2SIZE;         // 
        logic [3:0]                                     S1MODE;
        logic [3:0]                                     S2MODE;
        logic [3:0]                                     PDTMODE;
        logic [19:0]                                    PSCID;
        logic [15:0]                                    GSCID;
        logic [63:12]                                   PPN;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } update_req_t;                                     
//}}}
    // AXI PAYLD
    ch_ax_t                                             slv_awpayld_i, mst_awpayld_o;
    ch_w_t                                              slv_wpayld_i,  mst_wpayld_o;
    ch_ax_t                                             slv_arpayld_i, mst_arpayld_o;
    ch_b_t                                              slv_bpayld_o,  mst_bpayld_i;
    ch_r_t                                              slv_rpayld_o,  mst_rpayld_i;

    // BUS_HAND TO TRANS_UNIT
    logic                                               translate_req_valid;
    logic                                               translate_req_ready;
    TRANSLATE_REQ_TYPE                                  translate_req;
    logic                                               translate_ack_valid;
    TRANSLATE_ACK_TYPE                                  translate_ack;
    // PTW from queue                                   
    logic                                               ptw_req_valid;
    logic                                               ptw_req_ready;
    PTW_REQ_TYPE                                        ptw_req;
    logic                                               ptw_ack_valid;
    PTW_ACK_TYPE                                        ptw_ack;
    // FAULT from queue                                 
    logic                                               fault_rpt_valid;
    logic                                               fault_rpt_ready;
    iommu_acd_pkg::FAULT_RPT_TYPE                       fault_rpt;
    // INV to cache                                     
    logic                                               inv_tlbcache_req_valid;
    logic                                               inv_tlbcache_req_ready;
    INVALID_REQ_TYPE                                    inv_tlbcache_req;
    logic                                               inv_tlbcache_ack_valid;
    INVALID_REQ_TYPE                                    inv_tlbcache_ack;
                                                        
    logic                                               csr_fctl_gxl;
    logic [3:0]                                         csr_ddtp_iommu_mode;
    logic                                               multi_hit_check;
    logic                                               multi_hit_fault;
    // PTW and TC IF                                    
    logic                                               msg_pvalid_o;
    logic                                               msg_pready_i;
    logic [63:0]                                        msg_pwdata_o;
    logic                                               msg_pvalid_i;
    logic                                               msg_pready_o;
    logic [63:0]                                        msg_pdata_i;
    logic                                               msg_plast_i;
    // INV and TC IF                                    
    logic                                               msg_ivalid_o;
    logic                                               msg_iready_i;
    logic [63:0]                                        msg_iwdata_o;
    logic                                               msg_ivalid_i;
    logic                                               msg_iready_o;
    iommu_acd_pkg::MSG_INV_ACK_TYPE                     msg_idata_i;
    logic                                               msg_ilast_i;
    // FAULT and TC IF                                  
    logic                                               msg_fvalid_o;
    logic                                               msg_fready_i;
    iommu_acd_pkg::MSG_FAULT_ACK_TYPE                   msg_fwdata_o;
    logic                                               msg_fvalid_i;
    logic                                               msg_fready_o;
    logic [63:0]                                        msg_fdata_i;
    logic                                               msg_flast_i;
    // CFG and TC IF                                    
    logic                                               msg_cvalid_o;
    logic                                               msg_cready_i;
    iommu_acd_pkg::MSG_CFG_ACCESS_TYPE                  msg_cwdata_o;
    logic                                               msg_rvalid_i;
    logic                                               msg_rready_o;
    iommu_acd_pkg::MSG_CFG_ACK_TYPE                     msg_rdata_i;
    logic                                               msg_rlast_i;
    // DBG and TC IF                                    
    logic                                               msg_gvalid_o;
    logic                                               msg_gready_i;
    logic [63:0]                                        msg_gwdata_o;
    logic                                               msg_gvalid_i;
    logic                                               msg_gready_o;
    iommu_acd_pkg::MSG_DBG_ACK_TYPE                     msg_gdata_i;
    logic                                               msg_glast_i;



    tlbq2inv_t                                          qinfo2_inv;
    ptw_req_grp_t                                       ptwreqinfo2_ptw;

//    logic [63:12]                                       tr_req_iova_vpn;
//    logic [23:0]                                        tr_req_ctl_did;
//    logic                                               tr_req_ctl_pv;
//    logic [19:0]                                        tr_req_ctl_pid;
//    logic                                               tr_req_ctl_nw;
//    logic                                               tr_req_ctl_exe;
//    logic                                               tr_req_ctl_priv;
//    logic                                               tr_req_ctl_go;
//    logic                                               tr_req_finish;
//    logic [63:0]                                        tr_req_resp;

    logic                                               dbg_translate_req_valid;
    logic                                               dbg_translate_req_ready;
    TRANSLATE_REQ_TYPE                                  dbg_translate_req;
    logic                                               dbg_translate_ack_valid;
    TRANSLATE_ACK_TYPE                                  dbg_translate_ack;

    iommu_acd_pkg::RISCV_HPMEVT_TYPE                    riscv_hpmevt_intf[1:0];

    logic                                               iocountinh         [1:31];
    logic [14:0]                                        iohpmevt_eventid   [1:31];
    logic                                               iohpmevt_dmask     [1:31];
    logic [19:0]                                        iohpmevt_pid_pscid [1:31];
    logic [23:0]                                        iohpmevt_did_gscid [1:31];
    logic                                               iohpmevt_pv_pscv   [1:31];
    logic                                               iohpmevt_dv_gscv   [1:31];
    logic                                               iohpmevt_idt       [1:31];
    logic                                               iohpmevt_of        [1:31];
    logic [63:0]                                        iohpmctr_counter   [1:31];
    logic [63:0]                                        iohpmctr_counter_r [1:31];
    logic                                               iommu_ipsr_pmip_clr;
    
    logic                                               msg_nvalid_i;
    logic                                               msg_nready_o;
    iommu_acd_pkg::MSG_INT_TYPE                         msg_ndata_i ;
    logic                                               msg_nlast_i ;

    logic                                               mrif_credit_grant_valid;

`ifdef IOMMU_IDBG
    iommu_acd_pkg::IDBG_TYPE_M                          idbg_intf_m[3:0];
    iommu_acd_pkg::IDBG_TYPE_S                          idbg_intf_s[3:0];
`endif

    logic [BUS_INFLY_TOKEN_NUM-1:0]                     bh2inv_outstanding_list;
    logic [BUS_INFLY_TOKEN_NUM-1:0]                     bh2inv_outstanding_rw_list;

    logic [1:0]                                         trans_unit_ecc_err_o;
//}}}

//=== Inst === {{{
//=== BUS_HANDLER {{{
    assign slv_awpayld_i.axid       = slv_awid_i        ;
    assign slv_awpayld_i.axaddr     = slv_awaddr_i      ;
    assign slv_awpayld_i.axlen      = slv_awlen_i       ;
    assign slv_awpayld_i.axsize     = slv_awsize_i      ;
    assign slv_awpayld_i.axburst    = slv_awburst_i     ;
    assign slv_awpayld_i.axlock     = slv_awlock_i      ;
    assign slv_awpayld_i.axcache    = slv_awcache_i     ;
    assign slv_awpayld_i.axprot     = slv_awprot_i      ;
    assign slv_awpayld_i.axregion   = slv_awregion_i    ;
    assign slv_awpayld_i.axuser     = slv_awuser_i      ;
    assign slv_awpayld_i.axqos      = slv_awqos_i       ;
    assign slv_awpayld_i.axsnoop    = slv_awsnoop_i     ;
    assign slv_awpayld_i.axdomain   = slv_awdomain_i    ;
    assign slv_awpayld_i.axbar      = 'd0;
    assign slv_awpayld_i.axidunq    = slv_awidunq_i     ;
    assign slv_awpayld_i.axatop     = slv_awatop_i      ;
    assign slv_awpayld_i.axloop     = slv_awloop_i      ;
    assign slv_wpayld_i.wdata       = slv_wdata_i       ;
    assign slv_wpayld_i.wstrb       = slv_wstrb_i       ;
    assign slv_wpayld_i.wlast       = slv_wlast_i       ;
    assign slv_wpayld_i.wuser       = slv_wuser_i       ;
    assign slv_arpayld_i.axid       = slv_arid_i        ;
    assign slv_arpayld_i.axaddr     = slv_araddr_i      ;
    assign slv_arpayld_i.axlen      = slv_arlen_i       ;
    assign slv_arpayld_i.axsize     = slv_arsize_i      ;
    assign slv_arpayld_i.axburst    = slv_arburst_i     ;
    assign slv_arpayld_i.axlock     = slv_arlock_i      ;
    assign slv_arpayld_i.axcache    = slv_arcache_i     ;
    assign slv_arpayld_i.axprot     = slv_arprot_i      ;
    assign slv_arpayld_i.axregion   = slv_arregion_i    ;
    assign slv_arpayld_i.axuser     = slv_aruser_i      ;
    assign slv_arpayld_i.axqos      = slv_arqos_i       ;
    assign slv_arpayld_i.axsnoop    = slv_arsnoop_i     ;
    assign slv_arpayld_i.axdomain   = slv_ardomain_i    ;
    assign slv_arpayld_i.axbar      = 'd0;
    assign slv_arpayld_i.axidunq    = slv_aridunq_i     ;
    assign slv_arpayld_i.axatop     = 'd0;
    assign slv_arpayld_i.axloop     = slv_arloop_i      ;
    assign slv_bid_o                = slv_bpayld_o.bid  ;
    assign slv_bresp_o              = slv_bpayld_o.bresp;
    assign slv_buser_o              = slv_bpayld_o.buser;
    assign slv_bidunq_o             = slv_bpayld_o.bidunq;
    assign slv_bloop_o              = slv_bpayld_o.bloop;
    assign slv_rid_o                = slv_rpayld_o.rid  ;
    assign slv_rdata_o              = slv_rpayld_o.rdata;
    assign slv_rresp_o              = slv_rpayld_o.rresp;
    assign slv_rlast_o              = slv_rpayld_o.rlast;
    assign slv_ruser_o              = slv_rpayld_o.ruser;
    assign slv_ridunq_o             = slv_rpayld_o.ridunq;
    assign slv_rloop_o              = slv_rpayld_o.rloop;

    assign mst_awid_o               = mst_awpayld_o.axid    ;
    assign mst_awaddr_o             = mst_awpayld_o.axaddr  ;
    assign mst_awlen_o              = mst_awpayld_o.axlen   ;
    assign mst_awsize_o             = mst_awpayld_o.axsize  ;
    assign mst_awburst_o            = mst_awpayld_o.axburst ;
    assign mst_awlock_o             = mst_awpayld_o.axlock  ;
    assign mst_awcache_o            = mst_awpayld_o.axcache ;
    assign mst_awprot_o             = mst_awpayld_o.axprot  ;
    assign mst_awregion_o           = mst_awpayld_o.axregion;
    assign mst_awuser_o             = mst_awpayld_o.axuser  ;
    assign mst_awqos_o              = mst_awpayld_o.axqos   ;
    assign mst_awsnoop_o            = mst_awpayld_o.axsnoop ;
    assign mst_awdomain_o           = mst_awpayld_o.axdomain;
//    assign mst_awbar_o              = 'd0;
    assign mst_awidunq_o            = mst_awpayld_o.axidunq ;
    assign mst_awatop_o             = mst_awpayld_o.axatop  ;
    assign mst_awloop_o             = mst_awpayld_o.axloop  ;
    assign mst_wdata_o              = mst_wpayld_o.wdata    ;
    assign mst_wstrb_o              = mst_wpayld_o.wstrb    ;
    assign mst_wlast_o              = mst_wpayld_o.wlast    ;
    assign mst_wuser_o              = mst_wpayld_o.wuser    ;
    assign mst_arid_o               = mst_arpayld_o.axid    ;
    assign mst_araddr_o             = mst_arpayld_o.axaddr  ;
    assign mst_arlen_o              = mst_arpayld_o.axlen   ;
    assign mst_arsize_o             = mst_arpayld_o.axsize  ;
    assign mst_arburst_o            = mst_arpayld_o.axburst ;
    assign mst_arlock_o             = mst_arpayld_o.axlock  ;
    assign mst_arcache_o            = mst_arpayld_o.axcache ;
    assign mst_arprot_o             = mst_arpayld_o.axprot  ;
    assign mst_arregion_o           = mst_arpayld_o.axregion;
    assign mst_aruser_o             = mst_arpayld_o.axuser  ;
    assign mst_arqos_o              = mst_arpayld_o.axqos   ;
    assign mst_arsnoop_o            = mst_arpayld_o.axsnoop ;
    assign mst_ardomain_o           = mst_arpayld_o.axdomain;
//    assign mst_arbar_o              = 'd0;
    assign mst_aridunq_o            = mst_arpayld_o.axidunq ;
//    assign mst_aratop_o             = 'd0; // no ATOP in AR
    assign mst_arloop_o             = mst_arpayld_o.axloop  ;
    assign mst_bpayld_i.bid         = mst_bid_i             ;
    assign mst_bpayld_i.bresp       = mst_bresp_i           ;
    assign mst_bpayld_i.buser       = mst_buser_i           ;
    assign mst_bpayld_i.bidunq      = mst_bidunq_i          ;
    assign mst_bpayld_i.bloop       = mst_bloop_i           ;
    assign mst_rpayld_i.rid         = mst_rid_i             ;
    assign mst_rpayld_i.rdata       = mst_rdata_i           ;
    assign mst_rpayld_i.rresp       = mst_rresp_i           ;
    assign mst_rpayld_i.rlast       = mst_rlast_i           ;
    assign mst_rpayld_i.ruser       = mst_ruser_i           ;
    assign mst_rpayld_i.ridunq      = mst_ridunq_i          ;
    assign mst_rpayld_i.rloop       = mst_rloop_i           ;

    iommu_acd_bus_handler_top #(
    /*parameter  */ .WBUF_FIFO_TYPE                             (WBUF_FIFO_TYPE                             ), // = 1'b1, // 0:FF type, 1:RAM type
    /*parameter  */ .SLV_AW_REGSLICE                            (SLV_AW_REGSLICE                            ), // = 1,
    /*parameter  */ .SLV_W_REGSLICE                             (SLV_W_REGSLICE                             ), // = 1,
    /*parameter  */ .SLV_AR_REGSLICE                            (SLV_AR_REGSLICE                            ), // = 1,
    /*parameter  */ .SLV_R_REGSLICE                             (SLV_R_REGSLICE                             ), // = 1,
    /*parameter  */ .SLV_B_REGSLICE                             (SLV_B_REGSLICE                             ), // = 1,
    /*parameter  */ .MST_AW_REGSLICE                            (MST_AW_REGSLICE                            ), // = 1,
    /*parameter  */ .MST_W_REGSLICE                             (MST_W_REGSLICE                             ), // = 1,
    /*parameter  */ .MST_AR_REGSLICE                            (MST_AR_REGSLICE                            ), // = 1,
    /*parameter  */ .MST_R_REGSLICE                             (MST_R_REGSLICE                             ), // = 1,
    /*parameter  */ .MST_B_REGSLICE                             (MST_B_REGSLICE                             ), // = 1,
    /*parameter  */ .BUS_PROPERTY_BAR                           (0                                          ), // = 0,
    /*parameter  */ .TRANS_QUEUE_IDX_WIDTH                      (TRANS_QIDX_WIDTH                           ), // = 3,
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH                      (BUS_INFLY_TOKEN_WIDTH                      ), // = 6,
    /*parameter  */ .BUS_ADDR_WIDTH                             (64                                         ), // = 64,
    /*parameter  */ .BUS_DATA_WIDTH                             (BUS_DATA_WIDTH                             ), // = 128,
    /*parameter  */ .BUS_SIZE_WIDTH                             (BUS_SIZE_WIDTH                             ), // = 3,
    /*parameter  */ .BUS_STRB_WIDTH                             (BUS_STRB_WIDTH                             ), // = BUS_DATA_WIDTH/8,
    /*parameter  */ .BUS_ID_WIDTH                               (BUS_ID_WIDTH                               ), // = 8,
    /*parameter  */ .BUS_USER_WIDTH                             (BUS_USER_WIDTH                             ), // = 8,
    /*parameter  */ .BUS_LOOP_WIDTH                             (BUS_LOOP_WIDTH                             ), // = 1,
    /*parameter type         */ .BUS_CH_AX_TYPE                             (ch_ax_t                                    ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_W_TYPE                              (ch_w_t                                     ), // = iommu_acd_pkg::ch_w_t,
    /*parameter type         */ .BUS_CH_B_TYPE                              (ch_b_t                                     ), // = iommu_acd_pkg::ch_b_t,
    /*parameter type         */ .BUS_CH_R_TYPE                              (ch_r_t                                     ), // = iommu_acd_pkg::ch_r_t,
    /*parameter type         */ .TRANSLATE_REQ_TYPE                         (TRANSLATE_REQ_TYPE                         ), // = logic,
    /*parameter type         */ .TRANSLATE_ACK_TYPE                         (TRANSLATE_ACK_TYPE                         ), // = logic,
    /*parameter  */ .SPARE_PARAM                                (0)  //  = 0
    ) U_bus_handler(
    /*input logic                                                       */  .clk                                        (clk                                        ),
    /*input logic                                                       */  .rstn                                       (rstn                                       ),
    /*input  logic                                                      */  .slv_awvalid_i                              (slv_awvalid_i                              ),
    /*output logic                                                      */  .slv_awready_o                              (slv_awready_o                              ),
    /*input  BUS_CH_AX_TYPE                                             */  .slv_awpayld_i                              (slv_awpayld_i                              ),
    /*input  logic [23:0]                                               */  .slv_aw_device_id_i                         (slv_aw_device_id_i                         ),
    /*input  logic [19:0]                                               */  .slv_aw_process_id_i                        (slv_aw_process_id_i                        ),
    /*input  logic                                                      */  .slv_aw_process_id_valid_i                  (slv_aw_process_id_valid_i                  ),
    /*input  logic                                                      */  .slv_aw_is_translated_i                     (slv_aw_is_translated_i                     ),
    /*input  logic                                                      */  .slv_wvalid_i                               (slv_wvalid_i                               ),
    /*output logic                                                      */  .slv_wready_o                               (slv_wready_o                               ),
    /*input  BUS_CH_W_TYPE                                              */  .slv_wpayld_i                               (slv_wpayld_i                               ),
    /*output logic                                                      */  .slv_bvalid_o                               (slv_bvalid_o                               ),
    /*input  logic                                                      */  .slv_bready_i                               (slv_bready_i                               ),
    /*output BUS_CH_B_TYPE                                              */  .slv_bpayld_o                               (slv_bpayld_o                               ),
    /*input  logic                                                      */  .slv_arvalid_i                              (slv_arvalid_i                              ),
    /*output logic                                                      */  .slv_arready_o                              (slv_arready_o                              ),
    /*input  BUS_CH_AX_TYPE                                             */  .slv_arpayld_i                              (slv_arpayld_i                              ),
    /*input  logic [23:0]                                               */  .slv_ar_device_id_i                         (slv_ar_device_id_i                         ),
    /*input  logic [19:0]                                               */  .slv_ar_process_id_i                        (slv_ar_process_id_i                        ),
    /*input  logic                                                      */  .slv_ar_process_id_valid_i                  (slv_ar_process_id_valid_i                  ),
    /*input  logic                                                      */  .slv_ar_is_translated_i                     (slv_ar_is_translated_i                     ),
    /*output logic                                                      */  .slv_rvalid_o                               (slv_rvalid_o                               ),
    /*input  logic                                                      */  .slv_rready_i                               (slv_rready_i                               ),
    /*input  BUS_CH_R_TYPE                                              */  .slv_rpayld_o                               (slv_rpayld_o                               ),
    /*output logic                                                      */  .mst_awvalid_o                              (mst_awvalid_o                              ),
    /*input  logic                                                      */  .mst_awready_i                              (mst_awready_i                              ),
    /*output BUS_CH_AX_TYPE                                             */  .mst_awpayld_o                              (mst_awpayld_o                              ),
    /*output logic                                                      */  .mst_wvalid_o                               (mst_wvalid_o                               ),
    /*input  logic                                                      */  .mst_wready_i                               (mst_wready_i                               ),
    /*output BUS_CH_W_TYPE                                              */  .mst_wpayld_o                               (mst_wpayld_o                               ),
    /*input  logic                                                      */  .mst_bvalid_i                               (mst_bvalid_i                               ),
    /*output logic                                                      */  .mst_bready_o                               (mst_bready_o                               ),
    /*input  BUS_CH_B_TYPE                                              */  .mst_bpayld_i                               (mst_bpayld_i                               ),
    /*output logic                                                      */  .mst_arvalid_o                              (mst_arvalid_o                              ),
    /*input  logic                                                      */  .mst_arready_i                              (mst_arready_i                              ),
    /*output BUS_CH_AX_TYPE                                             */  .mst_arpayld_o                              (mst_arpayld_o                              ),
    /*input  logic                                                      */  .mst_rvalid_i                               (mst_rvalid_i                               ),
    /*output logic                                                      */  .mst_rready_o                               (mst_rready_o                               ),
    /*input  BUS_CH_R_TYPE                                              */  .mst_rpayld_i                               (mst_rpayld_i                               ),
    /*output logic                                                      */  .translate_req_valid_o                      (translate_req_valid                        ),
    /*input  logic                                                      */  .translate_req_ready_i                      (translate_req_ready                        ),
    /*output TRANSLATE_REQ_TYPE                                         */  .translate_req_o                            (translate_req                              ),
    /*input  logic                                                      */  .translate_ack_valid_i                      (translate_ack_valid                        ),
    /*input  TRANSLATE_ACK_TYPE                                         */  .translate_ack_i                            (translate_ack                              ),
    /*output logic                                                      */  .mrif_credit_grant_valid_o                  (mrif_credit_grant_valid                    ),
    /*output logic [BUS_INFLY_TOKEN_NUM-1:0]                            */  .bh2inv_outstanding_list_o                  (bh2inv_outstanding_list                    ),
    /*output logic [BUS_INFLY_TOKEN_NUM-1:0]                            */  .bh2inv_outstanding_rw_list_o               (bh2inv_outstanding_rw_list                 ),
    /*output iommu_acd_pkg::RISCV_HPMEVT_TYPE                           */  .riscv_hpmevt_intf_o                        (riscv_hpmevt_intf[0]                       ),
`ifdef IOMMU_IDBG
    /*input  iommu_acd_pkg::IDBG_TYPE_M                                 */  .idbg_intf_m_i                              (idbg_intf_m[3]                             ), //[2:0] 0: tlb_queue 1:microTLB 2:mainTLB_RAM
    /*output iommu_acd_pkg::IDBG_TYPE_S                                 */  .idbg_intf_s_o                              (idbg_intf_s[3]                             ), //[2:0]
    /*input  logic [63:0]                                               */  .idbg_scnt_i                                (idbg_scnt_i                                ),
`endif
    /*input  logic                                                      */  .spare_in                                   (1'b0                                       ) 
    );
//}}}

//=== TRANSLATE_UNIT {{{
    iommu_acd_translate_unit #(
    /*parameter  */ .TRANS_QIDX_WIDTH                           (TRANS_QIDX_WIDTH                           ), // = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    /*parameter  */ .TLB_QIDX_WIDTH                             (TLB_QIDX_WIDTH                             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter type         */ .TRANSLATE_REQ_TYPE                         (TRANSLATE_REQ_TYPE                         ), // = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    /*parameter type         */ .TRANSLATE_ACK_TYPE                         (TRANSLATE_ACK_TYPE                         ), // = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    /*parameter type         */ .PTW_REQ_TYPE                               (PTW_REQ_TYPE                               ), // = iommu_acd_pkg::PTW_REQ_TYPE,
    /*parameter type         */ .PTW_ACK_TYPE                               (PTW_ACK_TYPE                               ), // = iommu_acd_pkg::PTW_ACK_TYPE,
    /*parameter type         */ .FAULT_RPT_TYPE                             (iommu_acd_pkg::FAULT_RPT_TYPE              ), // = iommu_acd_pkg::FAULT_RPT_TYPE,
    /*parameter  */ .INTERNAL_INV_IDX_WIDTH                     (INTERNAL_INV_IDX_WIDTH                     ), // = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE                           (INVALID_REQ_TYPE                           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH                        (MICRO_TLB_IDX_WIDTH                        ), // = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH                        (CABIN_LKP_IDX_WIDTH                        ), // = iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH                        (CABIN_UPD_IDX_WIDTH                        ), // = iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    /*parameter  */ .CABIN_INV_IDX_WIDTH                        (CABIN_INV_IDX_WIDTH                        ), // = iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
    /*parameter  */ .BANK_4K_IDX_WIDTH                          (BANK_4K_IDX_WIDTH                          ), // = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    /*parameter  */ .BANK_2M_IDX_WIDTH                          (BANK_2M_IDX_WIDTH                          ), // = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    /*parameter  */ .BANK_1G_IDX_WIDTH                          (BANK_1G_IDX_WIDTH                          ), // = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    /*parameter  */ .BANK_0T_IDX_WIDTH                          (BANK_0T_IDX_WIDTH                          ), // = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    /*parameter  */ .BANK_4K_SET_IDX_WIDTH                      (BANK_4K_SET_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    /*parameter  */ .BANK_2M_SET_IDX_WIDTH                      (BANK_2M_SET_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    /*parameter  */ .BANK_1G_SET_IDX_WIDTH                      (BANK_1G_SET_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    /*parameter  */ .BANK_0T_SET_IDX_WIDTH                      (BANK_0T_SET_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    /*parameter  */ .BANK_4K_WAY_IDX_WIDTH                      (BANK_4K_WAY_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_2M_WAY_IDX_WIDTH                      (BANK_2M_WAY_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_1G_WAY_IDX_WIDTH                      (BANK_1G_WAY_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_0T_WAY_IDX_WIDTH                      (BANK_0T_WAY_IDX_WIDTH                      ), // = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    /*parameter  */ .INV_IDX_WIDTH                              (INV_IDX_WIDTH                              ), // = iommu_acd_pkg::INV_IDX_WIDTH,
    /*parameter type         */ .TLBQ2INV_TYPE                              (tlbq2inv_t                                 ), // = iommu_acd_pkg::tlbq2inv_t,
    /*parameter type         */ .PTW_REQ_GRP_TYPE                           (ptw_req_grp_t                              ), // = iommu_acd_pkg::ptw_req_grp_t,
    /*parameter type         */ .LOOKUP_REQ_TYPE                            (lookup_req_t                               ), // = iommu_acd_pkg::lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                            (lookup_ack_t                               ), // = iommu_acd_pkg::lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                            (update_req_t                               ), // = iommu_acd_pkg::update_req_t,
    /*parameter  */ .SPARE_PARAM                                (1'b0                                       )  // = 0 
    ) U_trans_unit(
    /*input  logic                                                      */  .clk                                        (clk                                        ),
    /*input  logic                                                      */  .rstn                                       (rstn                                       ),
    /*input  logic                                                      */  .translate_req_valid_i                      (translate_req_valid                        ),
    /*output logic                                                      */  .translate_req_ready_o                      (translate_req_ready                        ),
    /*input  TRANSLATE_REQ_TYPE                                         */  .translate_req_i                            (translate_req                              ),
    /*output logic                                                      */  .translate_ack_valid_o                      (translate_ack_valid                        ),
    /*output TRANSLATE_ACK_TYPE                                         */  .translate_ack_o                            (translate_ack                              ),
    /*input  logic                                                      */  .mrif_credit_grant_valid_i                  (mrif_credit_grant_valid                    ),
    /*output logic                                                      */  .ptw_req_valid_o                            (ptw_req_valid                              ),
    /*input  logic                                                      */  .ptw_req_ready_i                            (ptw_req_ready                              ),
    /*output PTW_REQ_TYPE                                               */  .ptw_req_o                                  (ptw_req                                    ),
    /*input  logic                                                      */  .ptw_ack_valid_i                            (ptw_ack_valid                              ),
    /*input  PTW_ACK_TYPE                                               */  .ptw_ack_i                                  (ptw_ack                                    ),
    /*output logic                                                      */  .fault_rpt_valid_o                          (fault_rpt_valid                            ),
    /*input  logic                                                      */  .fault_rpt_ready_i                          (fault_rpt_ready                            ),
    /*output FAULT_RPT_TYPE                                             */  .fault_rpt_o                                (fault_rpt                                  ),
    /*input  logic                                                      */  .inv_tlbcache_req_valid_i                   (inv_tlbcache_req_valid                     ),
    /*output logic                                                      */  .inv_tlbcache_req_ready_o                   (inv_tlbcache_req_ready                     ),
    /*input  INVALID_REQ_TYPE                                           */  .inv_tlbcache_req_i                         (inv_tlbcache_req                           ),
    /*output logic                                                      */  .inv_tlbcache_ack_valid_o                   (inv_tlbcache_ack_valid                     ),
    /*output INVALID_REQ_TYPE                                           */  .inv_tlbcache_ack_o                         (inv_tlbcache_ack                           ),
    /*output TLBQ2INV_TYPE                                              */  .qinfo2_inv_o                               (qinfo2_inv                                 ),
    /*output PTW_REQ_GRP_TYPE                                           */  .ptwreqinfo2_ptw_o                          (ptwreqinfo2_ptw                            ),
    /*input  logic                                                      */  .csr_fctl_gxl_i                             (csr_fctl_gxl                               ),
    /*input  logic [3:0]                                                */  .csr_ddtp_iommu_mode_i                      (csr_ddtp_iommu_mode                        ),
    /*input  logic                                                      */  .multi_hit_check_i                          (multi_hit_check                            ),
    /*output logic                                                      */  .multi_hit_fault_o                          (multi_hit_fault                            ),
    /*output logic [1:0]                                                */  .ecc_err_o                                  (trans_unit_ecc_err_o                       ),
    /*input  logic                                                      */  .dbg_translate_req_valid_i                  (dbg_translate_req_valid                    ),
    /*output logic                                                      */  .dbg_translate_req_ready_o                  (dbg_translate_req_ready                    ),
    /*input  TRANSLATE_REQ_TYPE                                         */  .dbg_translate_req_i                        (dbg_translate_req                          ),
    /*output logic                                                      */  .dbg_translate_ack_valid_o                  (dbg_translate_ack_valid                    ),
    /*output TRANSLATE_ACK_TYPE                                         */  .dbg_translate_ack_o                        (dbg_translate_ack                          ),
    /*output iommu_acd_pkg::RISCV_HPMEVT_TYPE                           */  .riscv_hpmevt_intf_o                        (riscv_hpmevt_intf[1]                       ),
`ifdef IOMMU_IDBG
    /*input  iommu_acd_pkg::IDBG_TYPE_M                                 */  .idbg_intf_m_i                              (idbg_intf_m[2:0]                           ), //[2:0] 0: tlb_queue 1:microTLB 2:mainTLB_RAM
    /*output iommu_acd_pkg::IDBG_TYPE_S                                 */  .idbg_intf_s_o                              (idbg_intf_s[2:0]                           ), //[2:0]
`endif
    /*input  logic                                                      */  .spare_in                                   (1'b0                                       ) 
    );
//}}}

//=== CFG_CTRL {{{
    iommu_acd_cfg_ctrl_wrap #(
    /*parameter  */ .TLB_QIDX_WIDTH                             (TLB_QIDX_WIDTH                             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH                      (BUS_INFLY_TOKEN_WIDTH                      ), // = 6,
    /*parameter type         */ .TLBQ2INV_TYPE                              (tlbq2inv_t                                 ), // = iommu_acd_pkg::tlbq2inv_t,
    /*parameter type         */ .PTW_REQ_GRP_TYPE                           (ptw_req_grp_t                              ), // = iommu_acd_pkg::ptw_req_grp_t,
    /*parameter  */ .INV_IDX_WIDTH                              (INV_IDX_WIDTH                              ), // = iommu_acd_pkg::INV_IDX_WIDTH,
    /*parameter  */ .FAULT_TOKEN_WIDTH                          (FAULT_TOKEN_WIDTH                          ), // = iommu_acd_pkg::FAULT_TOKEN_WIDTH,
    /*parameter  */ .PTW_IDX_WIDTH                              (PTW_IDX_WIDTH                              ), // = iommu_acd_pkg::PTW_IDX_WIDTH,
    /*parameter  */ .INTERNAL_INV_IDX_WIDTH                     (INTERNAL_INV_IDX_WIDTH                     ), //= iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE                           (INVALID_REQ_TYPE                           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .PTW_REQ_TYPE                               (PTW_REQ_TYPE                               ), // = iommu_acd_pkg::PTW_REQ_TYPE,
    /*parameter type         */ .PTW_ACK_TYPE                               (PTW_ACK_TYPE                               ), // = iommu_acd_pkg::PTW_ACK_TYPE,
    /*parameter  */ .SPARE_PARAM                                (1'b0                                       )  // = 0
    ) U_cfg_ctrl(
    /*input  logic                                                      */  .clk                                        (clk                                        ),
    /*input  logic                                                      */  .rstn                                       (rstn                                       ),
    /*input  logic                                                      */  .ptw_req_valid_i                            (ptw_req_valid                              ),
    /*output logic                                                      */  .ptw_req_ready_o                            (ptw_req_ready                              ),
    /*input  PTW_REQ_TYPE                                               */  .ptw_req_i                                  (ptw_req                                    ),
    /*output logic                                                      */  .ptw_ack_valid_o                            (ptw_ack_valid                              ),
    /*output PTW_ACK_TYPE                                               */  .ptw_ack_o                                  (ptw_ack                                    ),
    /*input  logic                                                      */  .msg_pvalid_i                               (msg_pvalid_o                               ),
    /*output logic                                                      */  .msg_pready_o                               (msg_pready_i                               ),
    /*input  logic [63:0]                                               */  .msg_pwdata_i                               (msg_pwdata_o                               ),
    /*output logic                                                      */  .msg_pvalid_o                               (msg_pvalid_i                               ),
    /*input  logic                                                      */  .msg_pready_i                               (msg_pready_o                               ),
    /*output logic [63:0]                                               */  .msg_pdata_o                                (msg_pdata_i                                ),
    /*output logic                                                      */  .msg_plast_o                                (msg_plast_i                                ),
    /*output logic                                                      */  .inv_tlbcache_req_valid_o                   (inv_tlbcache_req_valid                     ),
    /*input  logic                                                      */  .inv_tlbcache_req_ready_i                   (inv_tlbcache_req_ready                     ),
    /*output INVALID_REQ_TYPE                                           */  .inv_tlbcache_req_o                         (inv_tlbcache_req                           ),
    /*input  logic                                                      */  .inv_tlbcache_ack_valid_i                   (inv_tlbcache_ack_valid                     ),
    /*input  INVALID_REQ_TYPE                                           */  .inv_tlbcache_ack_i                         (inv_tlbcache_ack                           ),
    /*input  logic                                                      */  .msg_ivalid_i                               (msg_ivalid_o                               ),
    /*output logic                                                      */  .msg_iready_o                               (msg_iready_i                               ),
    /*input  logic [63:0]                                               */  .msg_iwdata_i                               (msg_iwdata_o                               ),
    /*output logic                                                      */  .msg_ivalid_o                               (msg_ivalid_i                               ),
    /*input  logic                                                      */  .msg_iready_i                               (msg_iready_o                               ),
    /*output MSG_INV_ACK_TYPE                                           */  .msg_idata_o                                (msg_idata_i                                ),
    /*output logic                                                      */  .msg_ilast_o                                (msg_ilast_i                                ),
    /*input  TLBQ2INV_TYPE                                              */  .qinfo2_inv_i                               (qinfo2_inv                                 ),
    /*input  PTW_REQ_GRP_TYPE                                           */  .ptwreqinfo2_ptw_i                          (ptwreqinfo2_ptw                            ),
    /*input  logic                                                      */  .fault_rpt_valid_i                          (fault_rpt_valid                            ),
    /*output logic                                                      */  .fault_rpt_ready_o                          (fault_rpt_ready                            ),
    /*input  FAULT_RPT_TYPE                                             */  .fault_rpt_i                                (fault_rpt                                  ),
    /*input  logic                                                      */  .msg_fvalid_i                               (msg_fvalid_o                               ),
    /*output logic                                                      */  .msg_fready_o                               (msg_fready_i                               ),
    /*input  MSG_FAULT_ACK_TYPE                                         */  .msg_fwdata_i                               (msg_fwdata_o                               ),
    /*output logic                                                      */  .msg_fvalid_o                               (msg_fvalid_i                               ),
    /*input  logic                                                      */  .msg_fready_i                               (msg_fready_o                               ),
    /*output logic [63:0]                                               */  .msg_fdata_o                                (msg_fdata_i                                ),
    /*output logic                                                      */  .msg_flast_o                                (msg_flast_i                                ),
    /*output logic                                                      */  .iommu_fctl_gxl_o                           (csr_fctl_gxl                               ),
    /*output logic                                                      */  .iommu_ipsr_pmip_clr_o                      (iommu_ipsr_pmip_clr                        ),
    /*output logic [3:0]                                                */  .iommu_ddtp_iommu_mode_o                    (csr_ddtp_iommu_mode                        ),
    /*output logic                                                      */  .acd_fault_ctrl_multi_hit_check_en_o        (multi_hit_check                            ),
    /*output logic                                                      */  .acd_fault_ctrl_selfdefine_fault_rpt_en_o   (                                           ),
    /*input  logic                                                      */  .int_multi_hit_check_fail_i                 (multi_hit_fault                            ),
    /*input  logic                                                      */  .msg_cvalid_i                               (msg_cvalid_o                               ),
    /*output logic                                                      */  .msg_cready_o                               (msg_cready_i                               ),
    /*input  MSG_CFG_ACCESS_TYPE                                        */  .msg_cwdata_i                               (msg_cwdata_o                               ),
    /*output logic                                                      */  .msg_rvalid_o                               (msg_rvalid_i                               ),
    /*input  logic                                                      */  .msg_rready_i                               (msg_rready_o                               ),
    /*output MSG_CFG_ACK_TYPE                                           */  .msg_rdata_o                                (msg_rdata_i                                ),
    /*output logic                                                      */  .msg_rlast_o                                (msg_rlast_i                                ),
//    /*output logic [63:12]                                              */  .tr_req_iova_vpn_o                          (tr_req_iova_vpn                            ),
//    /*output logic [23:0]                                               */  .tr_req_ctl_did_o                           (tr_req_ctl_did                             ),
//    /*output logic                                                      */  .tr_req_ctl_pv_o                            (tr_req_ctl_pv                              ),
//    /*output logic [19:0]                                               */  .tr_req_ctl_pid_o                           (tr_req_ctl_pid                             ),
//    /*output logic                                                      */  .tr_req_ctl_nw_o                            (tr_req_ctl_nw                              ),
//    /*output logic                                                      */  .tr_req_ctl_exe_o                           (tr_req_ctl_exe                             ),
//    /*output logic                                                      */  .tr_req_ctl_priv_o                          (tr_req_ctl_priv                            ),
//    /*output logic                                                      */  .tr_req_ctl_go_o                            (tr_req_ctl_go                              ),
//    /*input  logic                                                      */  .tr_req_finish_i                            (tr_req_finish                              ),
//    /*input  logic [63:0]                                               */  .tr_req_resp_i                              (tr_req_resp                                ),
    /*output logic                                                      */  .iocountinh_o                               (iocountinh                                 ), //[1:31],
    /*output logic [14:0]                                               */  .iohpmevt_eventid_o                         (iohpmevt_eventid                           ), //[1:31],
    /*output logic                                                      */  .iohpmevt_dmask_o                           (iohpmevt_dmask                             ), //[1:31],
    /*output logic [19:0]                                               */  .iohpmevt_pid_pscid_o                       (iohpmevt_pid_pscid                         ), //[1:31],
    /*output logic [23:0]                                               */  .iohpmevt_did_gscid_o                       (iohpmevt_did_gscid                         ), //[1:31],
    /*output logic                                                      */  .iohpmevt_pv_pscv_o                         (iohpmevt_pv_pscv                           ), //[1:31],
    /*output logic                                                      */  .iohpmevt_dv_gscv_o                         (iohpmevt_dv_gscv                           ), //[1:31],
    /*output logic                                                      */  .iohpmevt_idt_o                             (iohpmevt_idt                               ), //[1:31],
    /*output logic                                                      */  .iohpmctr_counter_o                         (iohpmctr_counter_r                         ), //[1:31],
    /*input  logic                                                      */  .iohpmevt_of_i                              (iohpmevt_of                                ), //[1:31],
    /*input  logic [63:0]                                               */  .iohpmctr_counter_i                         (iohpmctr_counter                           ), //[1:31],
    /*output logic                                                      */  .msg_nvalid_o                               (msg_nvalid_i                               ),
    /*input  logic                                                      */  .msg_nready_i                               (msg_nready_o                               ),
    /*output MSG_INT_TYPE                                               */  .msg_ndata_o                                (msg_ndata_i                                ),
    /*output logic                                                      */  .msg_nlast_o                                (msg_nlast_i                                ),
    /*input  logic [BUS_INFLY_TOKEN_NUM-1:0]                            */  .bh2inv_outstanding_list_i                  (bh2inv_outstanding_list                    ),
    /*input  logic [BUS_INFLY_TOKEN_NUM-1:0]                            */  .bh2inv_outstanding_rw_list_i               (bh2inv_outstanding_rw_list                 ),
    /*input  logic [1:0]                                                */  .trans_unit_ecc_err_i                       (trans_unit_ecc_err_o                       ),
    /*input  logic                                                      */  .spare_in                                   (1'b0                                       ) 
    );
//}}}

//=== T2C C2T IF {{{
    iommu_acd_tc_if #(
    /*parameter  */ .INV_IDX_WIDTH                              (INV_IDX_WIDTH                              ), // = 4,
    /*parameter  */ .FAULT_TOKEN_WIDTH                          (FAULT_TOKEN_WIDTH                          ), // = 5,
    /*parameter  */ .PTW_IDX_WIDTH                              (PTW_IDX_WIDTH                              ), // = 8,
    /*parameter  */ .CTIF_DATA_WIDTH                            (CTIF_DATA_WIDTH                            ), // = 64,
    /*parameter  */ .CTIF_STRB_WIDTH                            (CTIF_STRB_WIDTH                            ), // = CTIF_DATA_WIDTH/8,
    /*parameter  */ .CTIF_ID_WIDTH                              (CTIF_ID_WIDTH                              ), // = 4,
    /*parameter  */ .CTIF_DEST_WIDTH                            (CTIF_DEST_WIDTH                            ), // = 4,
    /*parameter  */ .CTIF_USER_WIDTH                            (CTIF_USER_WIDTH                            ), // = 1,
    /*parameter  */ .SPARE_PARAM                                (0                                          )  // = 0
    ) U_if(
    /*input  logic                                                      */  .clk                                        (clk                                        ),
    /*input  logic                                                      */  .rstn                                       (rstn                                       ),
    /*output logic                                                      */  .c2t_tvalid_o                               (c2t_tvalid_o                               ),
    /*input  logic                                                      */  .c2t_tready_i                               (c2t_tready_i                               ),
    /*output logic [CTIF_DATA_WIDTH-1:0]                                */  .c2t_tdata_o                                (c2t_tdata_o                                ),
    /*output logic [CTIF_STRB_WIDTH-1:0]                                */  .c2t_tstrb_o                                (c2t_tstrb_o                                ),
    /*output logic [CTIF_STRB_WIDTH-1:0]                                */  .c2t_tkeep_o                                (c2t_tkeep_o                                ),
    /*output logic                                                      */  .c2t_tlast_o                                (c2t_tlast_o                                ),
    /*output logic [CTIF_ID_WIDTH-1:0]                                  */  .c2t_tid_o                                  (c2t_tid_o                                  ),
    /*output logic [CTIF_DEST_WIDTH-1:0]                                */  .c2t_tdest_o                                (c2t_tdest_o                                ),
    /*output logic [CTIF_USER_WIDTH-1:0]                                */  .c2t_tuser_o                                (c2t_tuser_o                                ),
    /*input  logic                                                      */  .t2c_tvalid_i                               (t2c_tvalid_i                               ),
    /*output logic                                                      */  .t2c_tready_o                               (t2c_tready_o                               ),
    /*input  logic [CTIF_DATA_WIDTH-1:0]                                */  .t2c_tdata_i                                (t2c_tdata_i                                ),
    /*input  logic [CTIF_STRB_WIDTH-1:0]                                */  .t2c_tstrb_i                                (t2c_tstrb_i                                ),
    /*input  logic [CTIF_STRB_WIDTH-1:0]                                */  .t2c_tkeep_i                                (t2c_tkeep_i                                ),
    /*input  logic                                                      */  .t2c_tlast_i                                (t2c_tlast_i                                ),
    /*input  logic [CTIF_ID_WIDTH-1:0]                                  */  .t2c_tid_i                                  (t2c_tid_i                                  ),
    /*input  logic [CTIF_DEST_WIDTH-1:0]                                */  .t2c_tdest_i                                (t2c_tdest_i                                ),
    /*input  logic [CTIF_USER_WIDTH-1:0]                                */  .t2c_tuser_i                                (t2c_tuser_i                                ),
    /*output logic                                                      */  .msg_pvalid_o                               (msg_pvalid_o                               ),
    /*input  logic                                                      */  .msg_pready_i                               (msg_pready_i                               ),
    /*output logic [63:0]                                               */  .msg_pwdata_o                               (msg_pwdata_o                               ),
    /*input  logic                                                      */  .msg_pvalid_i                               (msg_pvalid_i                               ),
    /*output logic                                                      */  .msg_pready_o                               (msg_pready_o                               ),
    /*input  logic [63:0]                                               */  .msg_pdata_i                                (msg_pdata_i                                ),
    /*input  logic                                                      */  .msg_plast_i                                (msg_plast_i                                ),
    /*output logic                                                      */  .msg_ivalid_o                               (msg_ivalid_o                               ),
    /*input  logic                                                      */  .msg_iready_i                               (msg_iready_i                               ),
    /*output logic [63:0]                                               */  .msg_iwdata_o                               (msg_iwdata_o                               ),
    /*input  logic                                                      */  .msg_ivalid_i                               (msg_ivalid_i                               ),
    /*output logic                                                      */  .msg_iready_o                               (msg_iready_o                               ),
    /*input  MSG_INV_ACK_TYPE                                           */  .msg_idata_i                                (msg_idata_i                                ),
    /*input  logic                                                      */  .msg_ilast_i                                (msg_ilast_i                                ),
    /*output logic                                                      */  .msg_fvalid_o                               (msg_fvalid_o                               ),
    /*input  logic                                                      */  .msg_fready_i                               (msg_fready_i                               ),
    /*output MSG_FAULT_ACK_TYPE                                         */  .msg_fwdata_o                               (msg_fwdata_o                               ),
    /*input  logic                                                      */  .msg_fvalid_i                               (msg_fvalid_i                               ),
    /*output logic                                                      */  .msg_fready_o                               (msg_fready_o                               ),
    /*input  logic [63:0]                                               */  .msg_fdata_i                                (msg_fdata_i                                ),
    /*input  logic                                                      */  .msg_flast_i                                (msg_flast_i                                ),
    /*output logic                                                      */  .msg_cvalid_o                               (msg_cvalid_o                               ),
    /*input  logic                                                      */  .msg_cready_i                               (msg_cready_i                               ),
    /*output MSG_CFG_ACCESS_TYPE                                        */  .msg_cwdata_o                               (msg_cwdata_o                               ),
    /*input  logic                                                      */  .msg_rvalid_i                               (msg_rvalid_i                               ),
    /*output logic                                                      */  .msg_rready_o                               (msg_rready_o                               ),
    /*input  MSG_CFG_ACK_TYPE                                           */  .msg_rdata_i                                (msg_rdata_i                                ),
    /*input  logic                                                      */  .msg_rlast_i                                (msg_rlast_i                                ),
    /*input  logic                                                      */  .msg_nvalid_i                               (msg_nvalid_i                               ),
    /*output logic                                                      */  .msg_nready_o                               (msg_nready_o                               ),
    /*input  MSG_INT_TYPE                                               */  .msg_ndata_i                                (msg_ndata_i                                ),
    /*input  logic                                                      */  .msg_nlast_i                                (msg_nlast_i                                ),
    /*output logic                                                      */  .msg_gvalid_o                               (msg_gvalid_o                               ),
    /*input  logic                                                      */  .msg_gready_i                               (msg_gready_i                               ),
    /*output logic [63:0]                                               */  .msg_gwdata_o                               (msg_gwdata_o                               ),
    /*input  logic                                                      */  .msg_gvalid_i                               (msg_gvalid_i                               ),
    /*output logic                                                      */  .msg_gready_o                               (msg_gready_o                               ),
    /*input  MSG_DBG_ACK_TYPE                                           */  .msg_gdata_i                                (msg_gdata_i                                ),
    /*input  logic                                                      */  .msg_glast_i                                (msg_glast_i                                ),
    /*input  logic [CTIF_ID_WIDTH-1:0]                                  */  .acd_tid_i                                  (acd_tid_i                                  ),
    /*input  logic [CTIF_DEST_WIDTH-1:0]                                */  .acd_tdest_i                                (acd_tdest_i                                ),
    /*input  logic                                                      */  .spare_in                                   (1'b0                                       ) 
);
//}}}

//=== DBG  {{{
    iommu_acd_dbg_top #(
    /*parameter  */ .TRANS_QIDX_WIDTH                           (TRANS_QIDX_WIDTH                           ), // = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    /*parameter  */ .TLB_QIDX_WIDTH                             (TLB_QIDX_WIDTH                             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter type         */ .TRANSLATE_REQ_TYPE                         (TRANSLATE_REQ_TYPE                         ), // = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    /*parameter type         */ .TRANSLATE_ACK_TYPE                         (TRANSLATE_ACK_TYPE                         ), // = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    /*parameter  */ .SPARE_PARAM                                (1'b0                                       )  // = 0
    ) U_dbg_top(
    /*input  logic                                                      */  .clk                                        (clk                                        ),
    /*input  logic                                                      */  .rstn                                       (rstn                                       ),
//    /*input  logic [63:12]                                              */  .tr_req_iova_vpn_i                          (tr_req_iova_vpn                            ),
//    /*input  logic [23:0]                                               */  .tr_req_ctl_did_i                           (tr_req_ctl_did                             ),
//    /*input  logic                                                      */  .tr_req_ctl_pv_i                            (tr_req_ctl_pv                              ),
//    /*input  logic [19:0]                                               */  .tr_req_ctl_pid_i                           (tr_req_ctl_pid                             ),
//    /*input  logic                                                      */  .tr_req_ctl_nw_i                            (tr_req_ctl_nw                              ),
//    /*input  logic                                                      */  .tr_req_ctl_exe_i                           (tr_req_ctl_exe                             ),
//    /*input  logic                                                      */  .tr_req_ctl_priv_i                          (tr_req_ctl_priv                            ),
//    /*input  logic                                                      */  .tr_req_ctl_go_i                            (tr_req_ctl_go                              ),
//    /*output logic                                                      */  .tr_req_finish_o                            (tr_req_finish                              ),
//    /*output logic [63:0]                                               */  .tr_req_resp_o                              (tr_req_resp                                ),
    /*input  logic                                                      */  .msg_valid_i                                (msg_gvalid_o                               ),
    /*output logic                                                      */  .msg_ready_o                                (msg_gready_i                               ),
    /*input  logic [63:0]                                               */  .msg_wdata_i                                (msg_gwdata_o                               ),
    /*output logic                                                      */  .msg_valid_o                                (msg_gvalid_i                               ),
    /*input  logic                                                      */  .msg_ready_i                                (msg_gready_o                               ),
    /*output iommu_acd_pkg::MSG_DBG_ACK_TYPE                            */  .msg_data_o                                 (msg_gdata_i                                ),
    /*output logic                                                      */  .msg_last_o                                 (msg_glast_i                                ),
    /*output logic                                                      */  .dbg_translate_req_valid_o                  (dbg_translate_req_valid                    ),
    /*input  logic                                                      */  .dbg_translate_req_ready_i                  (dbg_translate_req_ready                    ),
    /*output TRANSLATE_REQ_TYPE                                         */  .dbg_translate_req_o                        (dbg_translate_req                          ),
    /*input  logic                                                      */  .dbg_translate_ack_valid_i                  (dbg_translate_ack_valid                    ),
    /*input  TRANSLATE_ACK_TYPE                                         */  .dbg_translate_ack_i                        (dbg_translate_ack                          ),
`ifdef IOMMU_IDBG
    /*input  logic                                                      */  .idbg_psel_i                                (idbg_psel_i                                ),
    /*input  logic                                                      */  .idbg_penable_i                             (idbg_penable_i                             ),
    /*output logic                                                      */  .idbg_pready_o                              (idbg_pready_o                              ),
    /*input  logic                                                      */  .idbg_pwrite_i                              (idbg_pwrite_i                              ),
    /*input  logic [11:0]                                               */  .idbg_paddr_i                               (idbg_paddr_i                               ),
    /*input  logic [31:0]                                               */  .idbg_pwdata_i                              (idbg_pwdata_i                              ),
    /*output logic [31:0]                                               */  .idbg_prdata_o                              (idbg_prdata_o                              ),
    /*output logic                                                      */  .idbg_pslverr_o                             (idbg_pslverr_o                             ),
    /*output iommu_acd_pkg::IDBG_TYPE_M                                 */  .idbg_intf_m_o                              (idbg_intf_m[3:0]                           ),  //[3:0] 0: tlb_queue 1:microTLB 2:mainTLB_RAM 3:TranslateIntf
    /*input  iommu_acd_pkg::IDBG_TYPE_S                                 */  .idbg_intf_s_i                              (idbg_intf_s[3:0]                           ),  //[3:0]
`endif
    /*input  logic                                                      */  .spare_in                                   (1'b0                                       )
    );

//}}}

//=== HPM {{{
    iommu_acd_hpm_top U_hpm_top(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .iocountinh_i               (iocountinh                 ), // [1:31],
    /*input  logic [14:0]                               */  .iohpmevt_eventid_i         (iohpmevt_eventid           ), // [1:31],
    /*input  logic                                      */  .iohpmevt_dmask_i           (iohpmevt_dmask             ), // [1:31],
    /*input  logic [19:0]                               */  .iohpmevt_pid_pscid_i       (iohpmevt_pid_pscid         ), // [1:31],
    /*input  logic [23:0]                               */  .iohpmevt_did_gscid_i       (iohpmevt_did_gscid         ), // [1:31],
    /*input  logic                                      */  .iohpmevt_pv_pscv_i         (iohpmevt_pv_pscv           ), // [1:31],
    /*input  logic                                      */  .iohpmevt_dv_gscv_i         (iohpmevt_dv_gscv           ), // [1:31],
    /*input  logic                                      */  .iohpmevt_idt_i             (iohpmevt_idt               ), // [1:31],
    /*input  logic                                      */  .iohpmctr_counter_i         (iohpmctr_counter_r         ), // [1:31],
    /*input  logic                                      */  .iommu_ipsr_pmip_clr_i      (iommu_ipsr_pmip_clr        ),
    /*output logic                                      */  .iohpmevt_of_o              (iohpmevt_of                ), // [1:31],
    /*output logic [63:0]                               */  .iohpmctr_counter_o         (iohpmctr_counter           ), // [1:31],
    /*input iommu_acd_pkg::RISCV_HPMEVT_TYPE            */  .riscv_hpmevt_intf_i        (riscv_hpmevt_intf          ), // [1:0],
    /*input  logic                                      */  .spare_in                   (1'b0                       )
    );
//}}}
//}}}

endmodule
