/* Copyright bosc
 * author: zhaohong
 * Function: receive active setipnum, and map the interrupt file ,last,delivery the irqs*/
module imsic_csr_top #(
parameter NR_INTP_FILES     = 7,    // m,s,5vs,
parameter NR_HARTS          = 64,   //harts number.
parameter XLEN              = 64,   // harts number.
parameter NR_SRC            = 256,
// DO NOT INSTANCE BY PARAMETER
parameter HART_ID_WIDTH     = $clog2(NR_HARTS), // default 4 current hart Index value.
parameter NR_SRC_WIDTH      = $clog2(NR_SRC)  //max is 12.
)
(
//  crg
input                                       csr_clk        ,    
input                                       csr_rstn       ,  
//                                                         ,
input       [NR_SRC_WIDTH -1:0]             i_setipnum     , 
input       [NR_HARTS*NR_INTP_FILES-1:0]    i_setipnum_vld ,   // m,s,5vs,4harts.0-3:hart0-hart3 m file. 4-9:hart0 s+vs file.
input       [HART_ID_WIDTH-1:0]             hart_id        , // start from 0.
input                                       i_csr_addr_vld ,   
input       [11    :0]                      i_csr_addr     ,   
input       [1:0]                           i_csr_priv_lvl ,   
input                                       i_csr_v        ,    
input       [5:0]                           i_csr_vgein    ,    // the value must be in the range 0~NR_INTP_FILES -2.
input       [2:0]                           i_csr_claim    ,     
input       [XLEN-1:0]                      i_csr_wdata    ,    
input                                       i_csr_wdata_vld,    
output wire                                 o_csr_rdata_vld,    
output wire [XLEN-1:0]                      o_csr_rdata    ,    
output wire                                 o_csr_illegal  ,  
output wire [2:0]                           o_irq          ,  
output wire [31:0]                          o_mtopei       ,     
output wire [31:0]                          o_stopei       ,     
output wire [31:0]                          o_vstopei            
);


// DO NOT EDIT BY PARAMETER
localparam INTP_FILE_WIDTH       = $clog2(NR_INTP_FILES); //max is $clog2(65) =7bit.
localparam NR_REG                = (NR_SRC%32 == 0) ? (NR_SRC/32) : (NR_SRC/32 +1); // total number of active eips/eies registers. 
localparam NR_TOTAL_INTFS        = NR_HARTS*NR_INTP_FILES;  // NR_INTP_FILES*NR_HARTS*NR_GROUPs.      

wire        [11    :0]                              csr_addr            ;
wire                                                csr_rd              ;
wire        [INTP_FILE_WIDTH-1:0]                   intp_file_sel       ;
wire                                                priv_is_illegal     ;
wire        [(NR_INTP_FILES*NR_REG)-1:0]            eip_final[XLEN-1:0] ;
wire        [(NR_INTP_FILES*NR_REG)-1:0]            eip_sw[XLEN-1:0]    ;
wire                                                eip_sw_wr           ;
wire        [NR_INTP_FILES-1:0]                     xtopei[31:0]        ;      
wire        [2:0]                                   xtopei_out[31:0]    ;
assign o_mtopei        = xtopei_out[0]      ;
assign o_stopei        = xtopei_out[1]      ;
assign o_vstopei       = xtopei_out[2]      ;

imsic_csr_reg #(
.NR_INTP_FILES    (NR_INTP_FILES    ),  
.XLEN             (XLEN             ),  
.NR_REG           (NR_REG           ),  
.INTP_FILE_WIDTH  (INTP_FILE_WIDTH  )  
)
u_imsic_csr_reg
(
.clk                           (csr_clk                                        ),
.rstn                          (csr_rstn                                       ), 
.csr_addr                      (csr_addr[11    :0]                             ),
.csr_rd                        (csr_rd                                         ),
.intp_file_sel                 (intp_file_sel[INTP_FILE_WIDTH-1:0]             ),
.priv_is_illegal               (priv_is_illegal                                ),
.i_csr_vgein                   (i_csr_vgein[5:0]                               ),
.eip_final                     (eip_final                                      ),
.eip_sw                        (eip_sw                                         ),
.eip_sw_wr                     (eip_sw_wr                                      ),
.xtopei                        (xtopei                                         ),
.i_csr_v                       (i_csr_v                                        ),
.i_csr_wdata                   (i_csr_wdata[XLEN-1:0]                          ),
.i_csr_wdata_vld               (i_csr_wdata_vld                                ),
.o_csr_rdata_vld               (o_csr_rdata_vld                                ),
.o_csr_rdata                   (o_csr_rdata[XLEN-1:0]                          ),
.o_csr_illegal                 (o_csr_illegal                                  ),
.o_irq                         (o_irq[2:0]                                     ),
.o_xtopei                      (xtopei_out                                     )
);
imsic_csr_gate #(
.NR_INTP_FILES      (NR_INTP_FILES  ),    
.XLEN               (XLEN           ),    
.HART_ID_WIDTH      (HART_ID_WIDTH  ),    
.NR_SRC             (NR_SRC         ),    
.NR_TOTAL_INTFS     (NR_TOTAL_INTFS ),    
.NR_SRC_WIDTH       (NR_SRC_WIDTH   ),    
.NR_REG             (NR_REG         ),    
.INTP_FILE_WIDTH    (INTP_FILE_WIDTH)
)
u_imsic_csr_gate
(
.clk                          (csr_clk                                                  ),
.rstn                         (csr_rstn                                                 ),
.hart_id                      (hart_id[HART_ID_WIDTH-1:0]                               ),
.i_setipnum                   (i_setipnum[NR_SRC_WIDTH -1:0]                            ),
.i_setipnum_vld               (i_setipnum_vld[NR_TOTAL_INTFS-1:0]                       ),
.i_csr_priv_lvl               (i_csr_priv_lvl[1:0]                                      ),
.i_csr_v                      (i_csr_v                                                  ),
.i_csr_addr_vld               (i_csr_addr_vld                                           ),
.i_csr_addr                   (i_csr_addr[11    :0]                                     ),
.i_csr_vgein                  (i_csr_vgein[5:0]                                         ),
.i_csr_claim                  (i_csr_claim[2:0]                                         ),
.xtopei                       (xtopei                                                   ),
.csr_addr                     (csr_addr[11    :0]                                       ),
.csr_rd                       (csr_rd                                                   ),
.intp_file_sel                (intp_file_sel[INTP_FILE_WIDTH-1:0]                       ),
.priv_is_illegal              (priv_is_illegal                                          ),
.eip_sw                       (eip_sw                                                   ),
.eip_sw_wr                    (eip_sw_wr                                                ),
.eip_final                    (eip_final                                                )
);












endmodule
