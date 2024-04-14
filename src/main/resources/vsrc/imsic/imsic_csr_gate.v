/* Copyright bosc
 * author: zhaohong
 * Function: receive active setipnum, and map the interrupt file ,last,delivery the irqs*/
module imsic_csr_gate #(
parameter NR_INTP_FILES         = 7,      // m,s,5vs, maxi is 2+63=65
parameter XLEN                  = 64,     // m,s,5vs,for RV32,32,for RV64,64
parameter NR_HARTS              = 4,      // harts number in each group.
parameter HART_ID_WIDTH         = 2,      // harts ID width.
parameter NR_SRC                = 32,
//not instanced by top.
parameter NR_SRC_WIDTH          = $clog2(NR_SRC), //max is 12.
parameter NR_REG                = (NR_SRC%32 == 0) ? (NR_SRC/32) : (NR_SRC/32 +1), // total number of active eips/eies registers. 
parameter INTP_FILE_WIDTH       = $clog2(NR_INTP_FILES) , //max is $clog2(65) =7bit.
//not instanced by top
parameter NR_TOTAL_INTFS        = NR_HARTS*NR_INTP_FILES  // NR_INTP_FILES*NR_HARTS*NR_GROUPs.      
)
(
//  top 
input                                       clk	               ,
input                                       rstn	           , 
input       [HART_ID_WIDTH-1:0]             hart_id            ,// current hart id,start from 0.
input       [NR_SRC_WIDTH-1:0]              i_setipnum         ,
input       [NR_TOTAL_INTFS-1:0]            i_setipnum_vld	   ,// m,s,5vs,4harts.0-3:hart0-hart3 m file. 4-9:hart0 s+vs file.
input       [1:0]                           i_csr_priv_lvl	   ,
input                                       i_csr_v	           ,
input                                       i_csr_addr_vld	   ,
input       [11    :0]                      i_csr_addr	       ,
input       [5:0]                           i_csr_vgein	       ,
input       [2:0]                           i_csr_claim	       , 
// imsic_csr_reg
input       [NR_INTP_FILES-1:0]             xtopei[31:0]       ,      
output reg  [11    :0]                      csr_addr  	       ,
output reg                                  csr_rd             ,
output reg  [INTP_FILE_WIDTH-1:0]           intp_file_sel      ,
output reg                                  priv_is_illegal    ,
input       [(NR_INTP_FILES*NR_REG)-1:0]    eip_sw[XLEN-1:0]   ,
input                                       eip_sw_wr          ,
output reg  [(NR_INTP_FILES*NR_REG)-1:0]    eip_final[XLEN-1:0] 
//top
);
// parameter used inside
localparam M_FILE                           = {INTP_FILE_WIDTH{1'b0}};
localparam S_FILE                           = {{(INTP_FILE_WIDTH-1){1'b0}},1'b1};
localparam NR_VS_FILES                      = NR_INTP_FILES -2;  // number of VS FILES.
localparam XLEN_WIDTH                       = $clog2(XLEN);      // width of XLEN.5 for 32,or 6 for 64.
localparam NR_REG_WIDTH                     = $clog2(NR_REG);
localparam RSLT_ADD_1WIDTH                  = XLEN-XLEN_WIDTH; 
localparam RSLT_ADD_2WIDTH                  = INTP_FILE_WIDTH+NR_REG_WIDTH;
localparam RSLT_ADD_WIDTH                   = (RSLT_ADD_1WIDTH > RSLT_ADD_2WIDTH) ? RSLT_ADD_1WIDTH : RSLT_ADD_2WIDTH;//get the max width.
localparam TOTAL_INTFS_WIDTH                = $clog2(NR_TOTAL_INTFS);
localparam SETIPNUM_H_WIDTH                 = (NR_SRC_WIDTH > XLEN_WIDTH) ? (NR_SRC_WIDTH-XLEN_WIDTH) : 0;

reg        [NR_INTP_FILES-1:0]              setipnum_vld_curr   ;  // msi write about the current hart, after extract from the total setipnum of top.
reg        [NR_INTP_FILES-1:0]              setipnum_vld_sync   ;  // synchronize with the current hart cpu clk.
reg        [NR_INTP_FILES-1:0]              setipnum_vld        ;  // one cycle after setipnum_vld_sync 
reg        [NR_SRC_WIDTH-1:0]               setipnum            ;
reg        [NR_INTP_FILES-1:0]              setipnum_vld_1f     ;  
reg        [(NR_INTP_FILES*NR_REG)-1:0]     eip[XLEN-1:0]       ;
wire                                        setipnum_vld_sync_ris;  // synchronize with the current hart cpu clk.
wire       [SETIPNUM_H_WIDTH-1:0]           setipnum_h          ;  //              
wire       [XLEN-XLEN_WIDTH-1:0]            curr_xtopei_h       ;
wire       [XLEN_WIDTH-1:0]                 curr_xtopei_l       ;
wire       [RSLT_ADD_WIDTH-1:0]             curr_xtopei_h_add   ;
wire       [TOTAL_INTFS_WIDTH-1:0]          sfiles_lsb          ;
wire       [TOTAL_INTFS_WIDTH-1:0]          sfiles_msb          ;
reg                                         csr_claim	        ; 

//start:code about mapping to current hart from the total i_setipnum_vld.
  
assign sfiles_lsb[TOTAL_INTFS_WIDTH-1:0] = NR_HARTS + (NR_INTP_FILES-1)*hart_id;
//assign sfiles_msb[TOTAL_INTFS_WIDTH-1:0] = sfiles_lsb + NR_VS_FILES          ;
integer s;
always @(*)
begin
    setipnum_vld_curr[0]                 = i_setipnum_vld[hart_id];
    for(s = 1; s < NR_INTP_FILES; s= s+1)begin
        setipnum_vld_curr[s] = i_setipnum_vld[sfiles_lsb+s-1];
    end
end
//end:code about mapping to current hart from the total i_setipnum_vld.
  
//start:code about synchronize of setipnum_vld
genvar j;
generate
    for (j = 0; j < NR_INTP_FILES; j = j+1)begin: VLD_SYNC_PROC
        cmip_dff_sync u_cmip_dff_sync
        (.clk       (clk                  ),
         .rstn      (rstn                 ),
         .din       (setipnum_vld_curr[j] ),
         .dout      (setipnum_vld_sync[j] )
        );
    end
endgenerate

always @(posedge clk or negedge rstn)
begin
    if (~rstn)begin
        setipnum_vld       <= {NR_INTP_FILES{1'b0}};
        setipnum_vld_1f    <= {NR_INTP_FILES{1'b0}};
    end
    else begin
        setipnum_vld       <= setipnum_vld_sync;
        setipnum_vld_1f    <= setipnum_vld;
    end
end
always @(posedge clk or negedge rstn)
begin
    if (~rstn)
        setipnum[NR_SRC_WIDTH-1:0] <= {NR_SRC_WIDTH{1'b0}};
    else if ((|setipnum_vld_sync) && (~(setipnum_vld)))
        setipnum <= i_setipnum ;
    else;
end
/** Interrupt files registers */
// ======================= map the interrupt file from privilege mode==========================


always @(*) begin
    if(i_csr_v == 1'b0)begin
        if (i_csr_priv_lvl[1:0] == 2'b11)begin// m mode
            intp_file_sel   = M_FILE;
            priv_is_illegal = 1'b0;
            csr_claim       = i_csr_claim[0];
        end
        else if (i_csr_priv_lvl[1:0] == 2'b01)begin // s mode
            intp_file_sel   = S_FILE;
            priv_is_illegal = 1'b0;
            csr_claim       = i_csr_claim[1];
        end
        else begin
            intp_file_sel   = {{INTP_FILE_WIDTH}{1'b0}};
            priv_is_illegal = 1'b1;  //report illegal in u mode 
            csr_claim       = 1'b0;
        end
    end
    else if ((i_csr_priv_lvl[1:0] == 2'b01) && ((|i_csr_vgein[5:0] != 6'd0) && (i_csr_vgein[5:0]<= NR_VS_FILES)))begin //vs mode, and vgein is in the implemented range.
        
        intp_file_sel   = S_FILE + i_csr_vgein[5:0];
        priv_is_illegal = 1'b0; 
        csr_claim       = i_csr_claim[2];
    end
    else begin
        intp_file_sel   = {{INTP_FILE_WIDTH}{1'b0}};
        priv_is_illegal = 1'b1;//report illegal in u mode 
        csr_claim       = 1'b0;
    end
end
//start:code about csr read enable gen
always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        csr_addr[11    :0] <= 12'h0;
        csr_rd             <= 1'b0;
    end
    else if (i_csr_addr_vld) begin
        csr_addr   <= i_csr_addr;
        if(~csr_rd)
            csr_rd <= 1'b1;
        else
            csr_rd <= 1'b0;
    end
    else begin
        csr_addr <= csr_addr;
        csr_rd   <= 1'b0;
    end
end
//start:code about eip gen
assign setipnum_h                    = (setipnum >>XLEN_WIDTH); // indicate the arrange in eips array.
assign curr_xtopei_h                 = xtopei[intp_file_sel][31:XLEN_WIDTH];   // xtopei/32,or xtopei/64,max is 7bits. 11:5
assign curr_xtopei_l[XLEN_WIDTH-1:0] = xtopei[intp_file_sel][XLEN_WIDTH-1:0];   // xtopei%32,or xtopei%64
assign curr_xtopei_h_add             = curr_xtopei_h + intp_file_sel*NR_REG;
integer i;
always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        for (i = 0; i < NR_INTP_FILES; i++) begin
            eip_final[i] <= {NR_REG{1'b0}};
        end
    end
    /** If a priv lvl is claiming the intp, unpend the intp */
    else if (csr_claim)
        eip_final[curr_xtopei_h_add][curr_xtopei_l] = 1'b0;
    else if (eip_sw_wr)
        eip_final       <= eip_sw;
    else begin
    /** For each priv lvl evaluate if some device triggered an interrupt, and make this interrupt pending */
        for (i = 0; i < NR_INTP_FILES; i++) begin
            if (setipnum_vld[i] && (~setipnum_vld_1f[i]) && (setipnum< NR_SRC)) begin // rising edge of setipnum_vld.
                eip_final[setipnum_h+(i*NR_REG)][setipnum[XLEN_WIDTH-1:0]] <= 1'b1;   //setipnum[4:0] is the bit location in one eip reg.
            end 
        end
    end
end
//end:code about eip gen

endmodule
