/* Copyright bosc
 * author: zhaohong
 * Function: receive active setipnum, and map the interrupt file ,last,delivery the irqs*/
module imsic_csr_reg #(
parameter NR_INTP_FILES         = 7,      // m,s,5vs,
parameter XLEN                  = 64,     // m,s,5vs,for RV32,32,for RV64,64
parameter NR_REG                = 1, // total number of active eips/eies registers. 
parameter INTP_FILE_WIDTH       = 1  //max is $clog2(65) =7bit.
)
(
//  crg
input                                               clk	                ,
input                                               rstn                , 
//imsic_irt_ctrl
input       [11    :0]                              csr_addr  	        ,
input                                               csr_rd              ,
input       [INTP_FILE_WIDTH-1:0]                   intp_file_sel       ,
input                                               priv_is_illegal     ,
input       [(NR_INTP_FILES*NR_REG)-1:0]            eip_final[XLEN-1:0] ,
output reg  [(NR_INTP_FILES*NR_REG)-1:0]            eip_sw[XLEN-1:0]    ,
output reg                                          eip_sw_wr           ,
output reg  [NR_INTP_FILES-1:0]                     xtopei[31:0]        ,      
//top
input                                               i_csr_wdata_vld     ,    
input                                               i_csr_v	            ,
input       [5:0]                                   i_csr_vgein	        ,// the value must be in the range 0~NR_INTP_FILES -2.
input       [XLEN-1:0]                              i_csr_wdata	        ,
output reg                                          o_csr_rdata_vld	    ,
output reg  [XLEN-1:0]                              o_csr_rdata	        ,
output reg                                          o_csr_illegal	    ,
output wire [2:0]                                   o_irq	            ,
output reg  [2:0]                                   o_xtopei[31:0]	     
);

// csr addr allocate accroding to the riscv aia spec.
localparam IPRIO0                                   = 12'h30;
localparam IPRIO15                                  = 12'h3F;
localparam EIDELIVERY_OFF                           = 12'h70;
localparam EITHRESHOLD_OFF                          = 12'h72;
localparam EIP0_OFF                                 = 12'h80;
localparam EIP63_OFF                                = 12'hBF;
localparam EIE0_OFF                                 = 12'hC0;
localparam EIE63_OFF                                = 12'hFF;
//temp parameter used inside
localparam MUX_NR_REG = ((XLEN == 32) ? NR_REG : NR_REG*2);  //diff the XLEN,to decide whether csr_addr's scope is in the required range..

/** Interrupt files registers */
reg         [NR_INTP_FILES-1:0]                     eidelivery                ;// NR_INTP_FILES altogether, 1bit each file.
reg         [NR_INTP_FILES-1:0]                     eithreshold[XLEN-1:0]     ;// XLEN bit each file
reg         [(NR_INTP_FILES*NR_REG)-1:0]            eie[XLEN-1:0]             ;
reg                                                 csr_wr_illegal            ;
reg                                                 csr_rd_illegal            ;
reg         [NR_INTP_FILES-1:0]                     irq_min_st                ;// NR_INTP_FILES altogether, 1bit each file.
reg         [NR_INTP_FILES-1:0]                     irq_out                   ;// NR_INTP_FILES altogether, 1bit each file.

wire        [INTP_FILE_WIDTH +5 :0]                 curr_intf_addr            ;
wire        [INTP_FILE_WIDTH +4 :0]                 curr_intf_addr_even       ;
//some temp signals for recognize on illegal access when XLEN is 64.
assign curr_intf_addr[INTP_FILE_WIDTH +5:0]         = {intp_file_sel, csr_addr[5:0]}      ; //*64 max is 13bit.
assign curr_intf_addr_even[INTP_FILE_WIDTH +4:0]    = curr_intf_addr[INTP_FILE_WIDTH +5:1]; //*64 max is 12bit. curr_intf_addr/2.
assign o_csr_illegal    = csr_wr_illegal | csr_rd_illegal;
integer s;
always @(posedge clk or negedge rstn)
begin
    if (~rstn)begin 
        eidelivery          <=  {NR_INTP_FILES{1'b0}}; 
        eip_sw_wr           <=  1'b0; 
        csr_wr_illegal      <=  1'b0; 
        for (s = 0; s < NR_INTP_FILES; s=s+1) begin
            eithreshold[s]  <=  {(XLEN){1'b0}}; 
            eip_sw[s]       <=  {(XLEN*NR_REG){1'b0}}; 
            eie[s]          <=  {(XLEN*NR_REG){1'b0}}; 
        end
    end
    /** IMSIC channel handler for interrupt file CSRs */
    else if (i_csr_wdata_vld) begin
        csr_wr_illegal <=  1'b0;    
        eip_sw_wr      <=  1'b0;    
        if (priv_is_illegal)
            csr_wr_illegal <=  1'b1;    
        else begin
            case (csr_addr) 
                12'b00000011????:begin
                    if(i_csr_v == 1'b1)         // virtual mode: it is inaccessiable region.
                        csr_wr_illegal   <=  1'b1;
                    else begin
                        if(XLEN == 64) begin
                            if (csr_addr[0] == 1'b1)
                                csr_wr_illegal <=  1'b1;    //only even number can be accessed.
                        end
                    end
                end
                EIDELIVERY_OFF: begin
                    eidelivery[intp_file_sel] <= i_csr_wdata[0];
                end
                EITHRESHOLD_OFF:begin
                    eithreshold[intp_file_sel] <= i_csr_wdata[XLEN-1:0];
                end
                12'b000010??????:begin
                    if(csr_addr[5:0]<= MUX_NR_REG -1) begin
                        if(XLEN == 32)begin
                            eip_sw[curr_intf_addr] <= i_csr_wdata[XLEN-1:0];
                            eip_sw_wr      <= 1'b1;
                        end
                        else if (csr_addr[0] == 1'b1)
                            csr_wr_illegal <= 1'b1;                                       
                        else begin
                            eip_sw[curr_intf_addr_even] <= i_csr_wdata[XLEN-1:0];
                            eip_sw_wr      <= 1'b1;
                        end  
                    end
                end
                12'b000011??????:begin
                    if(csr_addr[5:0]<= MUX_NR_REG -1) begin
                        if(XLEN == 32)
                            eie[curr_intf_addr] <= i_csr_wdata[XLEN-1:0];
                        else if (csr_addr[0] == 1'b1)
                            csr_wr_illegal <= 1'b1;                                       
                        else 
                            eie[curr_intf_addr_even] <= i_csr_wdata[XLEN-1:0];
                    end
                end
                default: csr_wr_illegal <= 1'b1;
            endcase
        end
    end
    else begin
        csr_wr_illegal <= 1'b0;
        eidelivery          <=  eidelivery  ; 
        eithreshold         <=  eithreshold ; 
        eip_sw              <=  eip_sw      ; 
        eip_sw_wr           <=  1'b0        ; 
        eie                 <=  eie         ; 
    end
end
always @(posedge clk or negedge rstn)
begin
    if (~rstn)begin 
        o_csr_rdata    <= {XLEN{1'b0}};
        csr_rd_illegal <= 1'b0;
    end
    else if (csr_rd) begin
        if (priv_is_illegal)
            csr_rd_illegal <=  1'b1;    
        else begin
            case (csr_addr) 
                12'b00000011????:begin
                    if(i_csr_v == 1'b1)         // virtual mode: it is inaccessiable region.
                        csr_rd_illegal   <=  1'b1;
                    else begin
                        if(XLEN == 64) begin
                            if (csr_addr[0] == 1'b1)
                                csr_rd_illegal <=  1'b1;    //only even number can be accessed.
                        end
                    end
                end
                EIDELIVERY_OFF: begin
                    o_csr_rdata     <= {{31{1'b0}}, eidelivery[intp_file_sel]};
                    csr_rd_illegal  <=  1'b0;    
                end
                EITHRESHOLD_OFF:begin
                    o_csr_rdata     <=  eithreshold[intp_file_sel];
                    csr_rd_illegal  <=  1'b0;    
                end
                12'b000010??????:begin
                    if(csr_addr[5:0]<= MUX_NR_REG-1) begin
                        if(XLEN == 32)
                            o_csr_rdata <= eip_final[curr_intf_addr];
                        else if (csr_addr[0] == 1'b0)
                            o_csr_rdata <= eip_final[curr_intf_addr];
                        else 
                            csr_rd_illegal <=  1'b1;                                       
                    end
                end
                12'b000011??????:begin
                    if(csr_addr[5:0]<= MUX_NR_REG-1) begin
                        if(XLEN == 32)
                            o_csr_rdata <= eie[curr_intf_addr];
                        else if (csr_addr[0] == 1'b0)
                            o_csr_rdata <= eie[curr_intf_addr];
                        else 
                            csr_rd_illegal <=  1'b1;                                       
                    end
                end
                default: csr_rd_illegal<= 1'b1;
            endcase
        end
    end
    else begin
        o_csr_rdata    <= o_csr_rdata;
        csr_rd_illegal  <=  1'b0;                                       
    end
end
//code on gen of rdata_vld, the next cycle following the rd. both rdata_vld and rd are only 1cycle active.
always @(posedge clk or negedge rstn)
begin
    if (~rstn)
        o_csr_rdata_vld <= 1'b0;
    else if (csr_rd)
        o_csr_rdata_vld <= 1'b1;
    else
        o_csr_rdata_vld <= 1'b0;
end
//========== code on the delivery of irqs. =============================
integer i,j,k;
/** For each interrupt file look for the highest pending and enable interrupt 
    k - interrupt file number,
    i - register number,
    j - arrangement of interrupt number in i; 
    k*NR_REG - select the current interrupt file */
integer t;
always @(posedge clk or negedge rstn)
begin
    if (~rstn)begin
        irq_min_st    <= {NR_INTP_FILES{1'b0}};  //whether there is min irq before.
        irq_out       <= {NR_INTP_FILES{1'b0}}; 
        for (t = 0; t < NR_INTP_FILES; t=t+1) begin
            xtopei[t] <= 32'h0; 
        end
    end
    else begin
        for (k = 0; k < NR_INTP_FILES; k=k+1) begin
            if (irq_min_st[k] == 1'b0) begin // no smaller pending interrupt from little number.
                for (int i = 0; i < NR_REG; i=i+1) begin
                    for (int j = 0; j < XLEN; j=j+1) begin 
                        if ((eie[(k*NR_REG)+i][j] && eip_final[(k*NR_REG)+i][j]) &&
                            ((eithreshold[k] == 0) || (j < eithreshold[k]))) begin
                                xtopei[k][10:0]     <= XLEN*i+j;  // curr  interrupt number.
                                xtopei[k][26:16]    <= XLEN*i+j;  // curr  interrupt number.
                                irq_min_st[k]       <= eip_final[(k*NR_REG)+i][j];
                                irq_out[k]          <= eidelivery[k] & irq_min_st[k];// If delivery is enable for this intp file, notify the hart */
                        end
                    end
                end
            end
        end
    end
end
assign o_irq[2:0]     = {irq_out[i_csr_vgein+1],irq_out[1:0]}; //select the vgein file for vs.
always @(*)
begin
    o_xtopei[0]  = xtopei[0];
    o_xtopei[1]  = xtopei[1];
    o_xtopei[2]  = xtopei[i_csr_vgein+1];
end
//assign o_xtopei[2:0]  = {xtopei[i_csr_vgein+1],xtopei[1:0]};
// ================================================================
endmodule
