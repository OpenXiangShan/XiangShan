package iommu_acd_pkg;
////////////////////////////////////////////////////////////////////////
// global param setting
////////////////////////////////////////////////////////////////////////
                                                            
localparam IOMMU_MODE_BARE                                  = 3'd1;
localparam IOMMU_MODE_OFF                                   = 3'd0;
localparam IOMMU_MODE_2LVL                                  = 3'd3;
localparam IOMMU_MODE_1LVL                                  = 3'd2;
                                                            
localparam PDTMODE_BARE                                     = 4'd0;
localparam PDTMODE_PD8                                      = 4'd1;
localparam PDTMODE_PD17                                     = 4'd2;
localparam PDTMODE_PD20                                     = 4'd3;
                                                            
localparam FCODE_INSTRUCTION_PAGE_FAULT                     = 12'd12;
localparam FCODE_READ_PAGE_FAULT                            = 12'd13;
localparam FCODE_WRITE_AMO_PAGE_FAULT                       = 12'd15;
localparam FCODE_GUEST_INSTRUCTION_PAGE_FAULT               = 12'd20;
localparam FCODE_GUEST_READ_PAGE_FAULT                      = 12'd21;
localparam FCODE_GUEST_WRITE_AMO_PAGE_FAULT                 = 12'd23;
localparam FCODE_ALL_INBOUND_TRANSACTION_DISALLOWED         = 12'd256;
localparam FCODE_TRANSACTION_TYPE_DISALLOWED                = 12'd260;

localparam FTTYP_UNTRANSLATED_READ_FOR_EXECUTE_TRANSACTION  = 6'd1;
localparam FTTYP_UNTRANSLATED_READ_TRANSACTION              = 6'd2;
localparam FTTYP_UNTRANSLATED_WRITE_AMO_TRANSACTION         = 6'd3;
localparam FTTYP_TRANSLATED_READ_FOR_EXECUTE_TRANSACTION    = 6'd5;
localparam FTTYP_TRANSLATED_READ_TRANSACTION                = 6'd6;
localparam FTTYP_TRANSLATED_WRITE_AMO_TRANSACTION           = 6'd7;

localparam INVOPCODE_IOTINVAL                               = 6'h1;
localparam INVOPCODE_IOFENCE                                = 6'h2;
localparam INVOPCODE_IODIR                                  = 6'h3;
localparam INVFUNC3_VMA                                     = 3'h0;
localparam INVFUNC3_GVMA                                    = 3'h1;
localparam INVFUNC3_C                                       = 3'h0;
localparam INVFUNC3_INVAL_DDT                               = 3'h0;
localparam INVFUNC3_INVAL_PDT                               = 3'h1;

localparam INVTYPE_INVALID_DDT                              = 2'b00;
localparam INVTYPE_INVALID_PDT                              = 2'b01;
localparam INVTYPE_VMA                                      = 2'b10;
localparam INVTYPE_GVMA                                     = 2'b11;

localparam MSGCODE_CFG_ACCESS                               = 4'h6;
localparam MSGCODE_CFG_ACK                                  = 4'h6;
localparam MSGCODE_FAULT_PRT                                = 4'h8;
localparam MSGCODE_FAULT_ACK                                = 4'h8;
localparam MSGCODE_INV_ACK                                  = 4'h4;
localparam MSGCODE_INV_REQ                                  = 4'h4;
localparam MSGCODE_FENCE_ACK                                = 4'h5;
localparam MSGCODE_FENCE_REQ                                = 4'h5;
localparam MSGCODE_PTW_REQ                                  = 4'h2;
localparam MSGCODE_PTW_ACK                                  = 4'h2;
localparam MSGCODE_CONNECT_REQ                              = 4'h0;
localparam MSGCODE_CONNECT_ACK                              = 4'h0;
localparam MSGCODE_INTERRUPT                                = 4'he;
localparam MSGCODE_DBG                                      = 4'hf;


localparam HPMEVENTID_DONOTCOUNT                            = 14'd0;
localparam HPMEVENTID_UNTRANSLATED_REQUEST                  = 14'd1;
localparam HPMEVENTID_TRANSLATED_REQUEST                    = 14'd2;
localparam HPMEVENTID_ATS_TRANSLATION_REQUEST               = 14'd3;
localparam HPMEVENTID_TLB_MISS                              = 14'd4;
localparam HPMEVENTID_DEVICE_DIRECTORY_WALKS                = 14'd5;
localparam HPMEVENTID_PPPROCESS_DIRECTORY_WALKS             = 14'd6;
localparam HPMEVENTID_FIRST_STAGE_PT_WALKS                  = 14'd7;
localparam HPMEVENTID_SECOND_STAGE_PT_WALKS                 = 14'd8;



typedef struct packed {
    logic [63:0]                    ioval2;
    logic [63:0]                    ioval;
    logic [23:0]                    did;
    logic [5:0]                     ttyp;
    logic                           priv;
    logic                           pv;
    logic [19:0]                    pid;
    logic [11:0]                    cause;
} FAULT_RPT_TYPE;

typedef struct packed {
    logic [59:0]                    interrupts;
    logic [3:0]                     msg_code;
} MSG_INT_TYPE;

typedef struct packed {
    logic [31:0]                    cfg_data;
    logic [31:13]                   reserved;
    logic [9:2]                     cfg_addr;
    logic                           cfg_rw;
    logic [3:0]                     msg_code;
} MSG_CFG_ACCESS_TYPE;

typedef struct packed {
    logic [31:0]                    rdata;
    logic [31:4]                    reserved;
    logic [3:0]                     msg_code;
} MSG_CFG_ACK_TYPE;

typedef struct packed {
    FAULT_RPT_TYPE                  rpt;
    logic [63:4]                    reserved0;
    logic [3:0]                     msg_code;
} MSG_FAULT_RPT_TYPE;

typedef struct packed {
    logic [63:4]                    reserved;
    logic [3:0]                     msg_code;
} MSG_FAULT_ACK_TYPE;

typedef struct packed {
    logic [63:8]                    reserved;
    logic [3:0]                     idx;
    logic [3:0]                     code;
} MSG_INV_ACK_TYPE;

typedef struct packed {
    logic [63:12]                   va;
    logic [11:0]                    attr;
    logic [23:0]                    device_id;
    logic [19:0]                    process_id;
    logic                           pv;
    logic [18:13]                   reserved0;
    logic                           proto;
    logic [7:0]                     idx;
    logic [3:0]                     msg_code;
} MSG_PTW_REQ_TYPE;

typedef struct packed {
    logic [63:12]                   vpn;
    logic [23:0]                    did;
    logic                           pv;
    logic [19:0]                    pid;
    logic                           nw;
    logic                           exe;
    logic                           priv;
    logic                           go;
} MSG_DBG_REQ_TYPE;

typedef struct packed {
    logic [63:4]                    reserved;
    logic [3:0]                     msg_code;
} MSG_DBG_ACK_TYPE;

typedef struct packed {
    logic                           idbg_go;
    logic [7:0]                     idbg_opcode;
    logic [31:0]                    idbg_datw;
} IDBG_TYPE_M;

typedef struct packed {
    logic                           idbg_busy;
    logic                           idbg_datv;
    logic [31:0]                    idbg_datr;
} IDBG_TYPE_S;


typedef struct packed {
    logic                           valid;
    logic [23:0]                    did;
    logic                           pv;
    logic [19:0]                    pid;
    logic                           gv;
    logic [15:0]                    gscid;
    logic                           pscv;
    logic [19:0]                    pscid;
    logic [14:0]                    eventid;
} RISCV_HPMEVT_TYPE;

















//////////////////////////////////////////////////////////////////////////////
// parametersized define default setting
//////////////////////////////////////////////////////////////////////////////
    parameter   SLV_AW_REGSLICE             = 1;
    parameter   SLV_W_REGSLICE              = 1;
    parameter   SLV_AR_REGSLICE             = 1;
    parameter   SLV_R_REGSLICE              = 1;
    parameter   SLV_B_REGSLICE              = 1;
                            
    parameter   MST_AW_REGSLICE             = 1;
    parameter   MST_W_REGSLICE              = 1;
    parameter   MST_AR_REGSLICE             = 1;
    parameter   MST_R_REGSLICE              = 1;
    parameter   MST_B_REGSLICE              = 1;
                            
    parameter   BUS_INFLY_TOKEN_WIDTH       = 6;
    parameter   BUS_INFLY_TOKEN_NUM         = 2**BUS_INFLY_TOKEN_WIDTH;
                            
    parameter   BUS_ADDR_WIDTH              = 64;
    parameter   BUS_DATA_WIDTH              = 128;
    parameter   BUS_SIZE_WIDTH              = 3;
    parameter   BUS_STRB_WIDTH              = BUS_DATA_WIDTH/8;
    parameter   BUS_ID_WIDTH                = 8;
    parameter   BUS_USER_WIDTH              = 8;
    parameter   BUS_LOOP_WIDTH              = 1;
                            
    parameter   INV_IDX_WIDTH               = 2;                    // should not bigger than 4
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH;
    parameter   FAULT_TOKEN_WIDTH           = 5;                    // should not bigger than 12
    parameter   FAULT_INFLY_NUM             = 2**FAULT_TOKEN_WIDTH;
    parameter   PTW_IDX_WIDTH               = 4;                    // should not bigger than 12
    parameter   PTW_INFLY_NUM               = 2**PTW_IDX_WIDTH;

    parameter   TRANS_QIDX_WIDTH            = 3;
    parameter   TRANS_QUEUE_DEPTH           = 2**TRANS_QIDX_WIDTH;
    parameter   TLB_QIDX_WIDTH              = 3;
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH;    // should equal or less than PTW_INFLY_NUM
                            
    parameter   MICRO_TLB_IDX_WIDTH         = 5;
    parameter   DTC_WAY_IDX_WIDTH           = 2;
    parameter   DTC_SET_IDX_WIDTH           = 6;
    parameter   DTC_INV_DID_WIDTH           = 4;
    parameter   DTC_INV_PID_WIDTH           = 6;
    parameter   MAIN_TLB_INFIFO_DEPTH       = 4;
    parameter   PTC_WAY_IDX_WIDTH           = 2;
    parameter   PTC_4K_SET_IDX_WIDTH        = 6;
    parameter   PTC_2M_SET_IDX_WIDTH        = 6;
    parameter   PTC_1G_SET_IDX_WIDTH        = 6;
    parameter   PTC_512G_SET_IDX_WIDTH      = 6;
    parameter   PTC_INV_DID_WIDTH           = 4;
    parameter   PTC_INV_PID_WIDTH           = 6;
    parameter   PTC_INV_ADDR_WIDTH          = 8;
                            
    parameter   CTIF_DATA_WIDTH             = 64;                   // can not change
    parameter   CTIF_STRB_WIDTH             = CTIF_DATA_WIDTH/8;
    parameter   CTIF_ID_WIDTH               = 4;
    parameter   CTIF_DEST_WIDTH             = 4;
    parameter   CTIF_USER_WIDTH             = 1;
    
// iommu_acd
    typedef struct packed {
        logic [BUS_ID_WIDTH-1:0]                        axid    ;
        logic [BUS_ADDR_WIDTH-1:0]                      axaddr  ;
        logic [ 7:0]                                    axlen   ;
        logic [BUS_SIZE_WIDTH-1:0]                      axsize  ;
        logic [ 1:0]                                    axburst ;
        logic                                           axlock  ;
        logic [ 3:0]                                    axcache ;
        logic [ 2:0]                                    axprot  ;
        logic [ 3:0]                                    axregion;
        logic [BUS_USER_WIDTH-1:0]                      axuser  ;
        logic [ 3:0]                                    axqos   ;
        logic [ 3:0]                                    axsnoop ;
        logic [ 1:0]                                    axdomain;
        logic [ 1:0]                                    axbar   ;
        logic                                           axidunq ;
        logic [ 5:0]                                    axatop  ;
        logic [BUS_LOOP_WIDTH-1:0]                      axloop  ;
    } ch_ax_t;                                          
    typedef struct packed {                             
        logic [BUS_DATA_WIDTH-1:0]                      wdata   ;
        logic [BUS_STRB_WIDTH-1:0]                      wstrb   ;
        logic                                           wlast   ;
        logic [BUS_USER_WIDTH-1:0]                      wuser   ;
    } ch_w_t;                                           
    typedef struct packed {                             
        logic [BUS_ID_WIDTH-1:0]                        bid     ;
        logic [ 1:0]                                    bresp   ;
        logic [BUS_USER_WIDTH-1:0]                      buser   ;
        logic                                           bidunq  ;
        logic [BUS_LOOP_WIDTH-1:0]                      bloop   ;
    } ch_b_t;                                           
    typedef struct packed {                             
        logic [BUS_ID_WIDTH-1:0]                        rid     ;
        logic [BUS_DATA_WIDTH-1:0]                      rdata   ;
        logic [ 1:0]                                    rresp   ;
        logic                                           rlast   ;
        logic [BUS_USER_WIDTH-1:0]                      ruser   ;
        logic                                           ridunq  ;
        logic [BUS_LOOP_WIDTH-1:0]                      rloop   ;
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
        logic [2:0]                                     trange;          // 3'b111 : iommu_bypass or S1/S2 both BARE; 3'b100: Svnapot64K; 3'b000: 4K; 3'b001: 2M/4M; 3'b010: 1G/4G; 3'b011:512G; 3'b101: MRIF
        logic [63:12]                                   pa;
    } TRANSLATE_ACK_TYPE;                               
                                                        
    typedef struct packed {                             
        logic [INTERNAL_INV_IDX_WIDTH:0]                idx;            // invalid cmd idx, highest bit indicates tlb_queue's internal invalid_req or not
        logic [1:0]                                     itype;          // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
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
    } PTW_REQ_TYPE ;                                    
                                                        
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

// iommu_ack_translate_unit
    //parameter   TRANS_QIDX_WIDTH            = 3;
    //parameter   INV_IDX_WIDTH               = 4;    // should not bigger than 4
    //parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH;
    //parameter   TLB_QIDX_WIDTH              = 3;
    //parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH;
    //parameter   MICRO_TLB_IDX_WIDTH         = 6;
    //parameter   DTC_WAY_IDX_WIDTH           = 2;
    //parameter   DTC_SET_IDX_WIDTH           = 6;
    //parameter   DTC_INV_DID_WIDTH           = 4;
    //parameter   DTC_INV_PID_WIDTH           = 6;
    //parameter   MAIN_TLB_INFIFO_DEPTH       = 4;
    //parameter   PTC_WAY_IDX_WIDTH           = 2;
    //parameter   PTC_4K_SET_IDX_WIDTH        = 6;
    //parameter   PTC_2M_SET_IDX_WIDTH        = 6;
    //parameter   PTC_1G_SET_IDX_WIDTH        = 6;
    //parameter   PTC_512G_SET_IDX_WIDTH      = 6;
    //parameter   PTC_INV_DID_WIDTH           = 4;
    //parameter   PTC_INV_PID_WIDTH           = 6;
    //parameter   PTC_INV_ADDR_WIDTH          = 8;

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



// iommu_acd_bus_handler_trans_queue
    typedef struct packed {
    logic                               mrif    ;
    logic                               fault   ;
    logic                               wr      ;
    ch_ax_t                             axpayld ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token   ;
    } tqe_info_t;                                       // trans-queue-entry info type              // transaction info to store into queue_entry



// iommu_acd_main_tlb
    parameter   CABIN_LKP_IDX_WIDTH         = 1;
    parameter   CABIN_UPD_IDX_WIDTH         = 1;
    parameter   CABIN_INV_IDX_WIDTH         = 1;
    parameter   BANK_4K_IDX_WIDTH           = 3;
    parameter   BANK_2M_IDX_WIDTH           = 2;
    parameter   BANK_1G_IDX_WIDTH           = 1;
    parameter   BANK_0T_IDX_WIDTH           = 1;
    parameter   BANK_4K_SET_IDX_WIDTH       = 4;
    parameter   BANK_2M_SET_IDX_WIDTH       = 2;
    parameter   BANK_1G_SET_IDX_WIDTH       = 1;
    parameter   BANK_0T_SET_IDX_WIDTH       = 1;
    parameter   BANK_4K_WAY_IDX_WIDTH       = 4;
    parameter   BANK_2M_WAY_IDX_WIDTH       = 2;
    parameter   BANK_1G_WAY_IDX_WIDTH       = 1;
    parameter   BANK_0T_WAY_IDX_WIDTH       = 1;
    parameter   CABIN_LKP_NUM               = 2**CABIN_LKP_IDX_WIDTH;
    parameter   CABIN_UPD_NUM               = 2**CABIN_UPD_IDX_WIDTH;
    parameter   CABIN_INV_NUM               = 2**CABIN_INV_IDX_WIDTH;
    parameter   BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH;
    parameter   BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH;
    parameter   BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH;
    parameter   BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH;
    parameter   BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH;
    parameter   BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH;
    parameter   BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH;
    parameter   BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH;
    parameter   BANK_4K_WAY_NUM             = 2**BANK_4K_WAY_IDX_WIDTH;
    parameter   BANK_2M_WAY_NUM             = 2**BANK_2M_WAY_IDX_WIDTH;
    parameter   BANK_1G_WAY_NUM             = 2**BANK_1G_WAY_IDX_WIDTH;
    parameter   BANK_0T_WAY_NUM             = 2**BANK_0T_WAY_IDX_WIDTH;

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
        lookup_req_t                                    req;            // LOOKUP_REQ message
//        logic [CABIN_LKP_IDX_WIDTH-1:0]                 cabin_idx;      // cabin idx
        logic [MAX_BANK_IDX_WIDTH-1:0]                  bank_idx;       // sub_bank idx
        logic [MAX_BANK_SET_IDX_WIDTH-1:0]              bank_set_idx;   // ram addr
    } bank_lkp_req_t;

    typedef struct packed {
        update_req_t                                    ack;
        logic                                           hit;
//        logic [CABIN_LKP_IDX_WIDTH-1:0]                 cabin_idx;
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
        update_req_t                                    req;            // LOOKUP_REQ message
//        logic [CABIN_UPD_IDX_WIDTH-1:0]                 cabin_idx;      // cabin idx
        logic [MAX_BANK_IDX_WIDTH-1:0]                  bank_idx;       // sub_bank idx
        logic [MAX_BANK_SET_IDX_WIDTH-1:0]              bank_set_idx;   // ram addr
    } bank_upd_req_t;

    typedef struct packed {
        update_req_t                                    ack;
//        logic [CABIN_UPD_IDX_WIDTH-1:0]                 cabin_idx;
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
//        logic [CABIN_UPD_IDX_WIDTH-1:0]                 cabin_idx;    // cabin idx
        logic                                           bank_idx_val;
        logic [MAX_BANK_IDX_WIDTH-1:0]                  bank_idx;       // sub_bank idx
        logic [MAX_BANK_SET_IDX_WIDTH-1:0]              bank_set_idx;   // ram addr
    } bank_inv_req_t;

    typedef struct packed {
        INVALID_REQ_TYPE                                ack;
//        logic [CABIN_UPD_IDX_WIDTH-1:0]                 cabin_idx;
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


    typedef struct packed {
        logic [2:0]                                     typ; // 001: lkp, 010:upd, 100:inv
        logic [CABIN_LKP_NUM-1:0]                       lidx;
        logic [CABIN_UPD_NUM-1:0]                       uidx;
        logic [CABIN_INV_NUM-1:0]                       iidx;
        bank_lkp_req_t                                  lkp;
        bank_upd_req_t                                  upd;
        bank_inv_req_t                                  inv;
    } bank_req_t;

// iommu_ack_tlb_wrap
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


// iommu_acd_micro_tlb
    typedef struct packed {                             
        logic                                           is_translated;
        logic                                           process_id_valid;
        logic [19:0]                                    process_id;
        logic [23:0]                                    device_id;
        logic [63:12]                                   va;
    } microtlb_tag_t;                                            
                                                        
    typedef struct packed {                             
        logic [61:12]                                   GPPN;
        logic [19:0]                                    PSCID;
        logic [15:0]                                    GSCID;
    } microtlb_inv_tag_t;                                        
                                                        
    typedef struct packed {                             
        logic [1:0]                                     PBMT;
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
        logic [63:12]                                   PPN;
    } microtlb_content_t;                                        




















//////////////////////////////////////////////////////////////
// global function
//////////////////////////////////////////////////////////////
    function automatic integer p_col_check(input integer value);
        if((value&(value-1))==0)
            p_col_check = 1;
        else
            p_col_check = 0;
    endfunction

    function automatic integer p_bit_idx(input integer value);  // log2(value)
        integer cnt;
        integer val;
        begin
            cnt = 0;
            val = value;
            while(val > 1) begin
                val = val >> 1;
                cnt = cnt + 1;
            end
            p_bit_idx = cnt;
        end
    endfunction

    function automatic integer d_bit_idx(input integer value);
        integer cnt;
        begin
            cnt = 0;
            while((2**cnt)<value) begin
                cnt = cnt + 1;
            end
            d_bit_idx = value - cnt - 1;
        end
    endfunction

    function automatic integer d_col_cal(input integer value);
        d_col_cal = (value+1)+$clog2(1+(value+1)+$clog2(value+1));
    endfunction

    function automatic integer p_pos_check(input integer r, input integer c);
        integer log2c;
        begin
            log2c = p_bit_idx(c);
            if(r==log2c)
                p_pos_check = 1;
            else
                p_pos_check = 0;
        end
    endfunction







endpackage
