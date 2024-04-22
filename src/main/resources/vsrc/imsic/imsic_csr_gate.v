/* Copyright bosc
 * author: zhaohong
 * Function: receive active setipnum, and map the interrupt file ,last,delivery the irqs*/
module imsic_csr_gate #(
parameter NR_INTP_FILES         = 7,      // m,s,5vs, maxi is 2+63=65
parameter XLEN                  = 64,     // m,s,5vs,for RV32,32,for RV64,64
parameter NR_HARTS              = 4,      // harts number in each group.
parameter NR_HARTS_WIDTH        = 2,      // harts ID width.
parameter NR_SRC                = 32,
parameter EID_VLD_DLY           = 0,
parameter MSI_INFO_WIDTH        = 17 ,    // NR_INTP_FILES*NR_HARTS*NR_GROUPs.      
//not instanced by top.
parameter NR_SRC_WIDTH          = $clog2(NR_SRC), //max is 12.
parameter NR_REG                = (NR_SRC%32 == 0) ? (NR_SRC/32) : (NR_SRC/32 +1), // total number of active eips/eies registers. 
parameter NR_REG_WIDTH          =1,
parameter INTP_FILE_WIDTH       = $clog2(NR_INTP_FILES)   //max is $clog2(65) =7bit.
//not instanced by top
)
(
//  top 
input                                       clk	                                    ,
input                                       rstn	                                , 
input       [NR_HARTS_WIDTH-1:0]            hart_id                                 ,// current hart id,start from 0.
input       [MSI_INFO_WIDTH-1:0]            i_msi_info                              ,
input                                       i_msi_info_vld	                        ,// m,s,5vs,4harts.0-3:hart0-hart3 m file. 4-9:hart0 s+vs file.
input       [1:0]                           i_csr_priv_lvl	                        ,
input                                       i_csr_v	                                ,
input                                       i_csr_addr_vld	                        ,
input       [11    :0]                      i_csr_addr	                            ,
input       [5:0]                           i_csr_vgein	                            ,
input       [2:0]                           i_csr_claim	                            , 
// imsic_csr_reg
input       [31:0]                          xtopei[0:NR_INTP_FILES-1] ,
output reg  [11    :0]                      csr_addr  	                            ,
output reg                                  csr_rd                                  ,
output reg  [INTP_FILE_WIDTH-1:0]           intp_file_sel                           ,
output reg                                  priv_is_illegal                         ,
input       [XLEN-1:0]                      eip_sw [0:((NR_INTP_FILES*NR_REG)-1)]   ,
input       [((NR_INTP_FILES*NR_REG)-1) :0 ]  eip_sw_wr                             ,
output reg  [XLEN-1:0]                      eip_final [0:((NR_INTP_FILES*NR_REG)-1)]
//top
);
// parameter used inside
localparam M_FILE                           = {INTP_FILE_WIDTH{1'b0}};
localparam S_FILE                           = {{(INTP_FILE_WIDTH-1){1'b0}},1'b1};
localparam NR_VS_FILES                      = NR_INTP_FILES -2;  // number of VS FILES.
localparam XLEN_WIDTH                       = $clog2(XLEN);      // width of XLEN.5 for 32,or 6 for 64.
localparam SETIPNUM_H_WIDTH                 = (NR_SRC_WIDTH > XLEN_WIDTH) ? (NR_SRC_WIDTH-XLEN_WIDTH) : 1;
localparam SETIPNUM_L_WIDTH                 = (NR_SRC_WIDTH > XLEN_WIDTH) ? XLEN_WIDTH : NR_SRC_WIDTH;
localparam RSLT_ADD_1WIDTH                  = SETIPNUM_H_WIDTH; 
localparam RSLT_ADD_2WIDTH                  = INTP_FILE_WIDTH+NR_REG_WIDTH;
localparam RSLT_ADD_WIDTH                   = (RSLT_ADD_1WIDTH > RSLT_ADD_2WIDTH) ? (RSLT_ADD_1WIDTH +1) : (RSLT_ADD_2WIDTH+1);//get the max width.
localparam N                                = EID_VLD_DLY +2; //2: cycles needed to sync.

reg         [MSI_INFO_WIDTH-1:0]            msi_info            ;
wire                                        msi_vld_sync        ;  // synchronize with the current hart cpu clk.
reg                                         msi_vld_sync_1dly   ;  // synchronize with the current hart cpu clk.
wire                                        msi_vld_sync_ris    ;  // synchronize with the current hart cpu clk.
reg                                         msi_vld_sync_ris_1dly; // synchronize with the current hart cpu clk.
reg        [NR_INTP_FILES-1:0]              setipnum_vld        ;  // one cycle after msi_vld_sync 
reg        [NR_SRC_WIDTH-1:0]               setipnum            ;
wire       [NR_INTP_FILES-1:0]              intp_file_curr      ;  
reg        [XLEN-1:0]                       eip[0:((NR_INTP_FILES*NR_REG)-1)];
wire                                        setipnum_vld_sync_ris;  // synchronize with the current hart cpu clk.
wire       [SETIPNUM_H_WIDTH-1:0]           setipnum_h          ;  //              
wire       [SETIPNUM_L_WIDTH-1:0]           setipnum_l          ;  //              
wire       [SETIPNUM_H_WIDTH-1:0]           curr_xtopei_h       ;
wire       [XLEN_WIDTH-1:0]                 curr_xtopei_l       ;
wire       [RSLT_ADD_WIDTH-1:0]             curr_xtopei_h_add   ;
reg                                         csr_claim	        ; 

//start:code about synchronize of setipnum_vld
cmip_dff_sync #(.N(N)) u_cmip_dff_sync
(.clk       (clk                  ),
 .rstn      (rstn                 ),
 .din       (i_msi_info_vld       ),
 .dout      (msi_vld_sync    )
);

//start:code about mapping to current hart from the total i_msi_info_vld.
  
//end:code about mapping to current hart from the total i_msi_info_vld.
  
always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        msi_vld_sync_1dly        <= 1'b0;
        msi_vld_sync_ris_1dly    <= 1'b0;
    end
    else begin
        msi_vld_sync_1dly        <= msi_vld_sync;
        msi_vld_sync_ris_1dly    <= msi_vld_sync_ris;
    end
end
assign msi_vld_sync_ris = msi_vld_sync & (~msi_vld_sync_1dly);
always @(posedge clk or negedge rstn)
begin
    if (~rstn)
        msi_info[MSI_INFO_WIDTH-1:0] <= {MSI_INFO_WIDTH{1'b0}};
    else if (msi_vld_sync_ris)
        msi_info                     <= i_msi_info ;
end
assign intp_file_curr   = msi_info[(NR_SRC_WIDTH + INTP_FILE_WIDTH-1):NR_SRC_WIDTH];
always @(*)
begin
    setipnum_vld                 = {NR_INTP_FILES{1'b0}};
    if (msi_vld_sync_ris_1dly & (msi_info[(MSI_INFO_WIDTH-1):(MSI_INFO_WIDTH-NR_HARTS_WIDTH)] == hart_id)) 
        setipnum_vld[intp_file_curr] = 1'b1;
    else
        setipnum_vld             = {NR_INTP_FILES{1'b0}};
end
assign setipnum = msi_info[NR_SRC_WIDTH-1:0];
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
assign setipnum_l                    = setipnum[SETIPNUM_L_WIDTH-1:0];
assign curr_xtopei_h                 = xtopei[intp_file_sel][NR_SRC_WIDTH-1:0]>>XLEN_WIDTH;   // bit[10:0] is msi priority,and id. xtopei/32,or xtopei/64,max is 7bits. 11:5
assign curr_xtopei_l[XLEN_WIDTH-1:0] = xtopei[intp_file_sel][XLEN_WIDTH-1:0];   // xtopei%32,or xtopei%64
assign curr_xtopei_h_add             = curr_xtopei_h + intp_file_sel*NR_REG;
integer i,k;
//for sim
/*
wire [XLEN-1:0] eip_0;
wire [XLEN-1:0] eip_1;
wire [XLEN-1:0] eip_7;
wire [XLEN-1:0] eip_6;
assign eip_7 = eip_final[7];
assign eip_6 = eip_final[6];
assign eip_1 = eip_final[1];
assign eip_0 = eip_final[0];
assign eip_4 = eip_final[4];*/
//
always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        for (i = 0; i < NR_INTP_FILES*NR_REG; i++) begin
            eip_final[i] <= {XLEN{1'b0}};
        end
    end
    /** If a priv lvl is claiming the intp, unpend the intp */
    else if (csr_claim)
        eip_final[curr_xtopei_h_add][curr_xtopei_l] = 1'b0;
    else begin
    /** For each priv lvl evaluate if some device triggered an interrupt, and make this interrupt pending */
        for (i = 0; i < NR_INTP_FILES; i++) begin
            if (setipnum_vld[i] & (|setipnum) & (setipnum< NR_SRC)) begin // rising edge of setipnum_vld.
                eip_final[setipnum_h+(i*NR_REG)][setipnum_l] <= 1'b1;   //setipnum[4:0] is the bit location in one eip reg.
            end 
            else begin
                for (k = 0; k < NR_REG; k++) begin
                    if (eip_sw_wr[i*NR_REG +k] == 1'b1)begin
                        eip_final[i*NR_REG +k] = eip_sw[i*NR_REG+k];
                    end
                end
            end 
        end
    end
end
//end:code about eip gen

endmodule
