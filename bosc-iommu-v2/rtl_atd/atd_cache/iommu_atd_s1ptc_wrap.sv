//
//
//
module iommu_atd_s1ptc_wrap #(
//{{{ PARAM
    parameter  INV_IDX_WIDTH               = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    parameter  TLB_QIDX_WIDTH              = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    parameter  MICRO_TLB_IDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    parameter  CABIN_LKP_IDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_IDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_IDX_WIDTH         = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    parameter  BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    parameter  BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    parameter  BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    parameter  BANK_L3_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    parameter  BANK_L4_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    parameter  BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    parameter  BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    parameter  BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    parameter  BANK_L3_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    parameter  BANK_L4_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    parameter  BANK_L0_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    parameter  BANK_L1_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    parameter  BANK_L2_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    parameter  BANK_L3_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    parameter  BANK_L4_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
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
    input  logic [1:0]                                  invalid_req_type_i,        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
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
    input  logic                                        lookup_req_gv_i,
    input  logic [15:0]                                 lookup_req_gscid_i,
    input  logic [19:0]                                 lookup_req_pscid_i,
    input  logic [63:12]                                lookup_req_va_i,
    output logic                                        lookup_ack_valid_o,
    output logic [TLB_QIDX_WIDTH-1:0]                   lookup_ack_idx_o,
    output logic                                        lookup_ack_hit_o,
    output logic [2:0]                                  lookup_ack_lvl_o,
    output logic                                        lookup_ack_prefetched_o,
    output logic [511:0]                                lookup_ack_o,
    output logic [63:0]                                 lookup_ack_svnapot_o,
    // update
    input  logic                                        update_req_valid_i,
    output logic                                        update_req_ready_o,
    input  logic [TLB_QIDX_WIDTH-1:0]                   update_req_idx_i,
    input  logic [2:0]                                  update_req_lvl_i,
    input  logic                                        update_req_prefetched_i,
    input  logic [511:0]                                update_req_i,
    input  logic [63:0]                                 update_req_svnapot_i,
    input  logic [19:0]                                 update_req_pscid_i,
    input  logic                                        update_req_gv_i,
    input  logic [15:0]                                 update_req_gscid_i,
    input  logic [63:12]                                update_req_va_i,
    input  logic                                        update_req_sxl_i,
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
    localparam WORK_L4_IDX_WIDTH = BANK_L4_IDX_WIDTH=='d0 ? 'd1 : BANK_L4_IDX_WIDTH;

    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_L0_IDX_WIDTH > BANK_L1_IDX_WIDTH) ? BANK_L0_IDX_WIDTH : BANK_L1_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = (BANK_L2_IDX_WIDTH > BANK_L3_IDX_WIDTH) ? BANK_L2_IDX_WIDTH : BANK_L3_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_2 =  WORK_L4_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_01= (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_2 > MAX_BANK_IDX_WIDTH_01)? MAX_BANK_IDX_WIDTH_2 : MAX_BANK_IDX_WIDTH_01;
    localparam MAX_BANK_NUM         = 2**MAX_BANK_IDX_WIDTH;
    
    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_L0_SET_IDX_WIDTH > BANK_L1_SET_IDX_WIDTH) ? BANK_L0_SET_IDX_WIDTH : BANK_L1_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = (BANK_L2_SET_IDX_WIDTH > BANK_L3_SET_IDX_WIDTH) ? BANK_L2_SET_IDX_WIDTH : BANK_L3_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_2 =  BANK_L4_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_01= (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_2 > MAX_BANK_SET_IDX_WIDTH_01)? MAX_BANK_SET_IDX_WIDTH_2 : MAX_BANK_SET_IDX_WIDTH_01;
    localparam MAX_BANK_SET_NUM         = 2**MAX_BANK_SET_IDX_WIDTH;
    
    localparam MAX_BANK_WAY_IDX_WIDTH_0 = (BANK_L0_WAY_IDX_WIDTH > BANK_L1_WAY_IDX_WIDTH) ? BANK_L0_WAY_IDX_WIDTH : BANK_L1_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH_1 = (BANK_L2_WAY_IDX_WIDTH > BANK_L3_WAY_IDX_WIDTH) ? BANK_L2_WAY_IDX_WIDTH : BANK_L3_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH_2 =  BANK_L4_WAY_IDX_WIDTH;
    localparam MAX_BANK_WAY_IDX_WIDTH_01= (MAX_BANK_WAY_IDX_WIDTH_0 > MAX_BANK_WAY_IDX_WIDTH_1) ? MAX_BANK_WAY_IDX_WIDTH_0 : MAX_BANK_WAY_IDX_WIDTH_1;
    localparam MAX_BANK_WAY_IDX_WIDTH   = (MAX_BANK_WAY_IDX_WIDTH_2 > MAX_BANK_WAY_IDX_WIDTH_01)? MAX_BANK_WAY_IDX_WIDTH_2 : MAX_BANK_WAY_IDX_WIDTH_01;
    localparam MAX_BANK_WAY_NUM         = 2**MAX_BANK_WAY_IDX_WIDTH;

    localparam TECC_WIDTH   = (ECC_ENABLE==0) ? 0 : 7;
    localparam NLDECC_WIDTH = (ECC_ENABLE==0) ? 0 : 6;
    localparam LDECC_WIDTH  = (ECC_ENABLE==0) ? 0 : 6;

//{{{ struct
    typedef struct packed {
        logic [INV_IDX_WIDTH-1:0]                       idx;            // invalid cmd idx
        logic [1:0]                                     itype;          // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
        logic                                           dv_gv;
        logic [23:0]                                    did_gscid;
        logic                                           pscv;
        logic [19:0]                                    pid_pscid;
        logic                                           av;
        logic [63:12]                                   addr;
    } INVALID_REQ_TYPE;                                 
                                                        
    typedef struct packed {                             
        logic [TLB_QIDX_WIDTH-1:0]                      idx;
        logic [63:12]                                   addr;
        logic                                           gv;
        logic [15:0]                                    gscid;
        logic [19:0]                                    pscid;
    } lookup_req_t;                                     
                                                        
    typedef struct packed {                             
        logic [TLB_QIDX_WIDTH-1:0]                      idx;
        logic [2:0]                                     lvl;
        logic                                           hit;
        logic                                           prefetched;
        logic [3:0]                                     pte_position;
        logic                                           N;
        logic [1:0]                                     PBMT;
//        logic [15:0]                                    D;
//        logic [15:0]                                    A;
        logic                                           D;
        logic                                           A;
        logic [55:12]                                   PPN;
        logic [4:0]                                     PERM;
        logic                                           V;
    } lookup_ack_t;                                     
                                                        
    typedef struct packed {                                         // 3bit     1bit         
        logic [TLB_QIDX_WIDTH+3:0]                      idx;        //{pte_idx, refill_flag, tlb_qidx}
        logic [2:0]                                     lvl;
        logic                                           prefetched;
        logic [63:12]                                   addr;
        logic                                           gv;
        logic [15:0]                                    gscid;
        logic [19:0]                                    pscid;
        logic                                           sxl;
        logic                                           N;
        logic [1:0]                                     PBMT;
//        logic [15:0]                                    D;
//        logic [15:0]                                    A;
        logic                                           D;
        logic                                           A;
        logic [55:12]                                   PPN;
        logic [4:0]                                     PERM;
        logic                                           V;
    } update_req_t;

    typedef struct packed {                             
        logic                                           valid;
        logic                                           prefetched;
        logic [63:12]                                   addr;
        logic                                           gv;
        logic [15:0]                                    gscid;
        logic                                           g;
        logic                                           n;
        logic                                           leaf;
        logic [19:0]                                    pscid;
        logic                                           sxl;
        logic [2:0]                                     lvl;        // all leaf-pte (with any level/pagesize), stored in level0 bank
    } mtlb_tag_t;                                       
    
    typedef struct packed {
        logic                                           rsv;
    } mtlb_itag_t;
    
    typedef struct packed {
        logic [MAX_BANK_WAY_NUM-2:0]                    plru_list;
    } mtlb_utag_t;
    
    typedef struct packed {                             
        logic [1:0]                                     PBMT;
        logic                                           D;
        logic                                           A;
        logic [55:12]                                   PPN;
        logic [4:0]                                     PERM;
        logic                                           V;
    } mtlb_dat_t_nl;                                    
    
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
    } mtlb_dat_t_l;                                     

    typedef struct packed {                             
        logic                                           prefetched;
        logic [63:12]                                   addr;
        logic                                           gv;
        logic [15:0]                                    gscid;
        logic [19:0]                                    pscid;
        logic                                           sxl;
    } microtlb_tag_t;                                   
                                                        
    typedef struct packed {                             
        logic                                           rsv;
    } microtlb_inv_tag_t;                               
                                                        
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

    logic                                               tc_lookup_ack_is_leaf;

    logic [63:0]                                        update_pte;
    logic                                               update_pte_is_svnapot;
    logic [3:0]                                         update_pte_position;
//}}}
    
//=== Main Code === {{{
//=== LOOKUP {{{
    assign tc_lookup_req_valid_i        = tc_ram_initial_done_o & lookup_req_valid_i;
    assign lookup_req_ready_o           = tc_ram_initial_done_o & tc_lookup_req_ready_o;
    assign tc_lookup_req_i.idx          = lookup_req_idx_i;
    assign tc_lookup_req_i.addr         = lookup_req_va_i;
    assign tc_lookup_req_i.gv           = lookup_req_gv_i;
    assign tc_lookup_req_i.gscid        = lookup_req_gscid_i;
    assign tc_lookup_req_i.pscid        = lookup_req_pscid_i;

    assign lookup_ack_valid_o           = tc_lookup_ack_valid_o;
    assign lookup_ack_idx_o             = tc_lookup_ack_o.idx;
    assign lookup_ack_hit_o             = tc_lookup_ack_o.hit;
    assign lookup_ack_lvl_o             = tc_lookup_ack_o.lvl;
    assign lookup_ack_prefetched_o      = tc_lookup_ack_o.prefetched;
    assign tc_lookup_ack_is_leaf        = tc_lookup_ack_o.lvl=='d0 | (tc_lookup_ack_o.PERM[0] | tc_lookup_ack_o.PERM[2]);
genvar i;
generate
    for(i=0; i<8; i++) begin : lookup_ack_assign
        assign lookup_ack_o[i*64+63:i*64] = (tc_lookup_ack_is_leaf & tc_lookup_ack_o.N) ?    {
                                                                                            tc_lookup_ack_o.N,
                                                                                            tc_lookup_ack_o.PBMT,
                                                                                            7'b0,
                                                                                            tc_lookup_ack_o.PPN[55:12],
                                                                                            2'b0,                           // resv
//                                                                                            {tc_lookup_ack_o.pte_position[3] ? tc_lookup_ack_o.D[i+8] : tc_lookup_ack_o.D[i]},  // position[3]==1 indicates is pte8~15, otherwise is pte0~7
//                                                                                            {tc_lookup_ack_o.pte_position[3] ? tc_lookup_ack_o.A[i+8] : tc_lookup_ack_o.A[i]},
                                                                                            tc_lookup_ack_o.D,
                                                                                            tc_lookup_ack_o.A,
                                                                                            tc_lookup_ack_o.PERM,
                                                                                            tc_lookup_ack_o.V               // all PTE valid
                                                                                            } :
                                            (tc_lookup_ack_o.pte_position[2:0] != i) ?           64'b0 :
                                                                                            {
                                                                                            tc_lookup_ack_o.N,
                                                                                            tc_lookup_ack_o.PBMT,
                                                                                            7'b0,
                                                                                            tc_lookup_ack_o.PPN[55:12],
                                                                                            2'b0,                           // resv
//                                                                                            tc_lookup_ack_o.D[i],
//                                                                                            tc_lookup_ack_o.A[i],
                                                                                            tc_lookup_ack_o.D,
                                                                                            tc_lookup_ack_o.A,
                                                                                            tc_lookup_ack_o.PERM,
                                                                                            tc_lookup_ack_o.V               // only the location silce valid
                                                                                            };
    end
endgenerate
    assign lookup_ack_svnapot_o =  (tc_lookup_ack_is_leaf & tc_lookup_ack_o.N) ?             {
                                                                                            {16{tc_lookup_ack_o.PBMT}},
//                                                                                            tc_lookup_ack_o.D,
//                                                                                            tc_lookup_ack_o.A
                                                                                            {16{tc_lookup_ack_o.D}},
                                                                                            {16{tc_lookup_ack_o.A}}
                                                                                            } : 'd0;
//}}}

//=== UPDATE {{{
// gpf 20260214, add mutli update logic
//        assign update_pte_position = (update_req_lvl_i=='d4) ? {1'b0, update_req_va_i[50:48]} :
//                                     (update_req_lvl_i=='d3) ? {1'b0, update_req_va_i[41:39]} :
//                                     (update_req_lvl_i=='d2) ? {1'b0, update_req_va_i[32:30]} :
//                                     (update_req_lvl_i=='d1) ? (update_req_sxl_i==1'b1 ? {1'b0, update_req_va_i[24:22]} : {1'b0, update_req_va_i[23:21]}) :
//                                     (update_req_lvl_i=='d0) ? {1'b0, update_req_va_i[14:12]} : 4'b0;
//    
//        always@(*) begin
//                case(update_pte_position)
//                4'b0000: update_pte = update_req_i[63 :  0];
//                4'b0001: update_pte = update_req_i[127: 64];
//                4'b0010: update_pte = update_req_i[191:128];
//                4'b0011: update_pte = update_req_i[255:192];
//                4'b0100: update_pte = update_req_i[319:256];
//                4'b0101: update_pte = update_req_i[383:320];
//                4'b0110: update_pte = update_req_i[447:384];
//                4'b0111: update_pte = update_req_i[511:448];
//                default: update_pte = update_req_i[63 :  0];
//                endcase
//        end
//        assign update_pte_is_svnapot        = update_pte[63];
//    
//        assign tc_update_req_valid_i        = tc_ram_initial_done_o & update_req_valid_i;
//        assign update_req_ready_o           = tc_ram_initial_done_o & tc_update_req_ready_o;
//        assign tc_update_req_i.idx          = {1'b0, update_req_idx_i};
//        assign tc_update_req_i.lvl          = update_req_lvl_i;
//        assign tc_update_req_i.prefetched   = update_req_prefetched_i;
//        assign tc_update_req_i.addr         = update_req_va_i;
//        assign tc_update_req_i.gv           = update_req_gv_i;
//        assign tc_update_req_i.gscid        = update_req_gscid_i;
//        assign tc_update_req_i.pscid        = update_req_pscid_i;
//        assign tc_update_req_i.sxl          = update_req_sxl_i;
//        assign tc_update_req_i.N            = update_pte_is_svnapot;
//        assign tc_update_req_i.PBMT         = update_pte[62:61];
//    //    assign tc_update_req_i.D            = update_pte_is_svnapot ? update_req_svnapot_i[31:16] : {16{update_pte[7]}};//({15'b0, update_pte[7]} << update_pte_position);
//    //    assign tc_update_req_i.A            = update_pte_is_svnapot ? update_req_svnapot_i[15: 0] : {16{update_pte[6]}};//({15'b0, update_pte[6]} << update_pte_position);
//        assign tc_update_req_i.D            = update_pte[7];
//        assign tc_update_req_i.A            = update_pte[6];
//        assign tc_update_req_i.PPN          = update_pte[53:10];
//        assign tc_update_req_i.PERM         = update_pte[5:1];
//        assign tc_update_req_i.V            = update_pte[0];
//    
//        assign update_ack_valid_o           = tc_update_ack_valid_o & ~tc_update_ack_o.idx[TLB_QIDX_WIDTH]; // bit[TLB_QIDX_WIDTH] indicates if the update ack is for refill update_req
//        assign update_ack_idx_o             = tc_update_ack_o.idx[TLB_QIDX_WIDTH-1:0];
    iommu_atd_ptc_multiupdate #(
    /*parameter */              .CACHE_TYPE                 (1'b1                       ), // = 0, // 0:S2TC, 1:S1PTC
    /*parameter */              .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH             ), // = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    /*parameter */              .CABIN_UPD_IDX_WIDTH        (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    /*parameter type         */ .UPDATE_REQ_TYPE            (update_req_t               ), // = iommu_atd_cache_pkg::s2ptc_update_req_t,
    /*parameter */              .SPARE_PARAM                (0                          )  // = 0
    ) U_multi_update(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .update_req_valid_i         (update_req_valid_i         ),
    /*output logic                                      */  .update_req_ready_o         (update_req_ready_o         ),
    /*input  logic [TLB_QIDX_WIDTH-1:0]                 */  .update_req_idx_i           (update_req_idx_i           ),
    /*input  logic [2:0]                                */  .update_req_lvl_i           (update_req_lvl_i           ),
    /*input  logic                                      */  .update_req_prefetched_i    (update_req_prefetched_i    ),
    /*input  logic [511:0]                              */  .update_req_i               (update_req_i               ),
    /*input  logic [63:0]                               */  .update_req_svnapot_i       (update_req_svnapot_i       ),
    /*input  logic [19:0]                               */  .update_req_pscid_i         (update_req_pscid_i         ),
    /*input  logic                                      */  .update_req_gv_i            (update_req_gv_i            ),
    /*input  logic [15:0]                               */  .update_req_gscid_i         (update_req_gscid_i         ),
    /*input  logic [63:12]                              */  .update_req_va_i            (update_req_va_i            ),
    /*input  logic                                      */  .update_req_sxl_i           (update_req_sxl_i           ),
    /*output logic                                      */  .update_ack_valid_o         (update_ack_valid_o         ),
    /*output logic [TLB_QIDX_WIDTH-1:0]                 */  .update_ack_idx_o           (update_ack_idx_o           ),
    /*output logic                                      */  .tc_update_req_valid_o      (tc_update_req_valid_i      ),
    /*input  logic                                      */  .tc_update_req_ready_i      (tc_update_req_ready_o      ),
    /*output UPDATE_REQ_TYPE                            */  .tc_update_req_o            (tc_update_req_i            ),
    /*input  logic                                      */  .tc_update_ack_valid_i      (tc_update_ack_valid_o      ),
    /*input  UPDATE_REQ_TYPE                            */  .tc_update_ack_i            (tc_update_ack_o            ),
    /*input  logic                                      */  .tc_ram_initial_done_i      (tc_ram_initial_done_o      ),
    /*input  logic                                      */  .csr_fctl_gxl_i             (1'b0                       ),
    /*input  logic                                      */  .spare_in                   (1'b0                       )
    );

//}}}

//=== INVALID {{{
    assign tc_invalid_req_valid_i       = tc_ram_initial_done_o & invalid_req_valid_i;
    assign invalid_req_ready_o          = tc_ram_initial_done_o & tc_invalid_req_ready_o;
    assign tc_invalid_req_i.idx         = invalid_req_idx_i;
    assign tc_invalid_req_i.itype       = invalid_req_type_i;
    assign tc_invalid_req_i.dv_gv       = invalid_req_dv_gv_i;
    assign tc_invalid_req_i.did_gscid   = invalid_req_did_gscid_i;
    assign tc_invalid_req_i.pscv        = invalid_req_pscv_i;
    assign tc_invalid_req_i.pid_pscid   = invalid_req_pid_pscid_i;
    assign tc_invalid_req_i.av          = invalid_req_av_i;
    assign tc_invalid_req_i.addr        = invalid_req_addr_i;
    assign invalid_ack_valid_o          = tc_invalid_ack_valid_o;
    assign invalid_ack_idx_o            = tc_invalid_ack_o.idx;
//}}}

//}}}

//=== Inst === {{{

    iommu_atd_ptc_top #(
    /*parameter */              .CACHE_TYPE                 (1'b1                       ), // = 0, // 0:S2TC, 1:S1PTC
    /*parameter */              .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH+'d3         ), // = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE            (lookup_req_t               ), // = iommu_atd_cache_pkg::s2ptc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (lookup_ack_t               ), // = iommu_atd_cache_pkg::s2ptc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE            (update_req_t               ), // = iommu_atd_cache_pkg::s2ptc_update_req_t,
    /*parameter type         */ .MTLB_TAG_TYPE              (mtlb_tag_t                 ), // = iommu_atd_cache_pkg::s2ptc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE             (mtlb_itag_t                ), // = iommu_atd_cache_pkg::s2ptc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE             (mtlb_utag_t                ), // = iommu_atd_cache_pkg::s2ptc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE_NL           (mtlb_dat_t_nl              ), // = iommu_atd_cache_pkg::s2ptc_mtlb_dat_t_nl,
    /*parameter type         */ .MTLB_DAT_TYPE_L            (mtlb_dat_t_l               ), // = iommu_atd_cache_pkg::s2ptc_mtlb_dat_t_l,
    /*parameter type         */ .MICROTLB_TAG_TYPE          (microtlb_tag_t             ), // = iommu_atd_cache_pkg::s2ptc_microtlb_tag_t,
    /*parameter type         */ .MICROTLB_INV_TAG_TYPE      (microtlb_inv_tag_t         ), // = iommu_atd_cache_pkg::s2ptc_microtlb_inv_tag_t,
    /*parameter type         */ .MICROTLB_CONTENT_TYPE      (microtlb_content_t         ), // = iommu_atd_cache_pkg::s2ptc_microtlb_content_t,
    /*parameter */              .MICRO_TLB_IDX_WIDTH        (MICRO_TLB_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    /*parameter */              .CABIN_LKP_IDX_WIDTH        (CABIN_LKP_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    /*parameter */              .CABIN_UPD_IDX_WIDTH        (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    /*parameter */              .CABIN_INV_IDX_WIDTH        (CABIN_INV_IDX_WIDTH        ), // = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    /*parameter */              .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    /*parameter */              .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    /*parameter */              .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    /*parameter */              .BANK_L3_IDX_WIDTH          (BANK_L3_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    /*parameter */              .BANK_L4_IDX_WIDTH          (WORK_L4_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    /*parameter */              .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    /*parameter */              .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    /*parameter */              .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    /*parameter */              .BANK_L3_SET_IDX_WIDTH      (BANK_L3_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    /*parameter */              .BANK_L4_SET_IDX_WIDTH      (BANK_L4_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    /*parameter */              .BANK_L0_WAY_IDX_WIDTH      (BANK_L0_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    /*parameter */              .BANK_L1_WAY_IDX_WIDTH      (BANK_L1_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    /*parameter */              .BANK_L2_WAY_IDX_WIDTH      (BANK_L2_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    /*parameter */              .BANK_L3_WAY_IDX_WIDTH      (BANK_L3_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    /*parameter */              .BANK_L4_WAY_IDX_WIDTH      (BANK_L4_WAY_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    /*parameter */              .TECC_WIDTH                 (TECC_WIDTH                 ), //= 7,
    /*parameter */              .LDECC_WIDTH                (LDECC_WIDTH                ), //= 6,
    /*parameter */              .NLDECC_WIDTH               (NLDECC_WIDTH               ), //= 6,
    /*parameter */              .BANK_L4_EMPTY              (BANK_L4_IDX_WIDTH==0 ? 1'b1 : 1'b0), // = 0,
    /*parameter */              .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_ptc_top(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .lookup_req_valid_i         (tc_lookup_req_valid_i      ),
    /*output logic                                      */  .lookup_req_ready_o         (tc_lookup_req_ready_o      ),
    /*input  LOOKUP_REQ_TYPE                            */  .lookup_req_i               (tc_lookup_req_i            ),
    /*output logic                                      */  .lookup_ack_valid_o         (tc_lookup_ack_valid_o      ),
    /*output LOOKUP_ACK_TYPE                            */  .lookup_ack_o               (tc_lookup_ack_o            ),
    /*input  logic                                      */  .update_req_valid_i         (tc_update_req_valid_i      ),
    /*output logic                                      */  .update_req_ready_o         (tc_update_req_ready_o      ),
    /*input  UPDATE_REQ_TYPE                            */  .update_req_i               (tc_update_req_i            ),
    /*output logic                                      */  .update_ack_valid_o         (tc_update_ack_valid_o      ),
    /*output UPDATE_REQ_TYPE                            */  .update_ack_o               (tc_update_ack_o            ),
    /*input  logic                                      */  .invalid_req_valid_i        (tc_invalid_req_valid_i     ),
    /*output logic                                      */  .invalid_req_ready_o        (tc_invalid_req_ready_o     ),
    /*input  INVALID_REQ_TYPE                           */  .invalid_req_i              (tc_invalid_req_i           ),
    /*output logic                                      */  .invalid_ack_valid_o        (tc_invalid_ack_valid_o     ),
    /*output INVALID_REQ_TYPE                           */  .invalid_ack_o              (tc_invalid_ack_o           ),
    /*output logic                                      */  .ram_initial_done_o         (tc_ram_initial_done_o      ),
    /*input  logic                                      */  .csr_fctl_gxl_i             (1'b0                       ),
    /*input  logic                                      */  .multi_hit_check_i          (multi_hit_check_i          ),
    /*output logic                                      */  .multi_hit_fault_o          (multi_hit_fault_o          ),
    /*output logic [1:0]                                */  .ecc_err_o                  (ecc_err_o                  ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );

//}}}

endmodule
