/* Copyright bosc
 * author: zhaohong
 * Function: receive active setipnum, and map the interrupt file ,last,delivery the irqs*/
module imsic_csr_reg #(
parameter NR_INTP_FILES         = 7,      // m,s,5vs,
parameter XLEN                  = 64,     // m,s,5vs,for RV32,32,for RV64,64
parameter NR_REG                = 1, // total number of active eips/eies registers. 
parameter NR_REG_WIDTH          = 1, // total number of active eips/eies registers. 
parameter INTP_FILE_WIDTH       = 1  //max is $clog2(65) =7bit.
)
(
//  crg
input                                               clk	                                    ,
input                                               rstn                                    , 
//imsic_irt_ctrl
input       [11    :0]                              csr_addr  	                            ,
input                                               csr_rd                                  ,
input       [INTP_FILE_WIDTH-1:0]                   intp_file_sel                           ,
input                                               priv_is_illegal                         ,
input       [XLEN-1:0]                              eip_final [0:((NR_INTP_FILES*NR_REG)-1)],
output reg  [XLEN-1:0]                              eip_sw [0:((NR_INTP_FILES*NR_REG)-1)]   ,
output reg  [((NR_INTP_FILES*NR_REG)-1) :0 ]        eip_sw_wr                               ,
output reg  [31:0]                                  xtopei[0:NR_INTP_FILES-1]               ,
//top
input                                               i_csr_wdata_vld                         ,    
input                                               i_csr_v	                                ,
input       [5:0]                                   i_csr_vgein	                            ,// the value must be in the range 0~NR_INTP_FILES -2.
input       [XLEN-1:0]                              i_csr_wdata	                            ,
output reg                                          o_csr_rdata_vld	                        ,
output reg  [XLEN-1:0]                              o_csr_rdata	                            ,
output reg                                          o_csr_illegal	                        ,
output wire [2:0]                                   o_irq	                                 
//output reg  [31:0]                                  o_xtopei[0:2]	     
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
localparam OFFSET_WIDTH = (XLEN == 32) ? 6 : 5; //only even addr of both eip and eie is used when xlen is 64.
localparam CURR_ADDR_WIDTH = (INTP_FILE_WIDTH + NR_REG_WIDTH > OFFSET_WIDTH) ? (INTP_FILE_WIDTH + NR_REG_WIDTH+1) : OFFSET_WIDTH + 1;

/** Interrupt files registers */
reg         [NR_INTP_FILES-1:0]                     eidelivery                          ;// NR_INTP_FILES altogether, 1bit each file.
reg         [XLEN-1:0]                              eithreshold[0:NR_INTP_FILES-1]      ;// XLEN bit each file
reg         [XLEN-1:0]                              eie[0:((NR_INTP_FILES*NR_REG)-1)]   ;
reg                                                 csr_wr_illegal                      ;
reg                                                 csr_rd_illegal                      ;
reg         [NR_INTP_FILES-1:0]                     irq_min_st                          ;// NR_INTP_FILES altogether, 1bit each file.
reg         [NR_INTP_FILES-1:0]                     irq_out                             ;// NR_INTP_FILES altogether, 1bit each file.

wire        [INTP_FILE_WIDTH + NR_REG_WIDTH -1:0]   curr_intf_base_addr                 ;
wire        [CURR_ADDR_WIDTH-1 :0]                  curr_intf_addr                      ;
wire        [OFFSET_WIDTH   -1 :0]                  mux_csr_addr                        ;
//some temp signals for recognize on illegal access when XLEN is 64.
assign curr_intf_base_addr                          = intp_file_sel*NR_REG                ;
assign mux_csr_addr                                 = (XLEN ==32) ? csr_addr[5:0] : csr_addr[5:1];
assign curr_intf_addr[CURR_ADDR_WIDTH-1:0]          = curr_intf_base_addr + mux_csr_addr      ; //*64 max is 13bit.
assign o_csr_illegal    = csr_wr_illegal | csr_rd_illegal;
// for sim
/*
wire [XLEN-1:0] eip_sw_0;
wire [XLEN-1:0] eip_sw_1;
wire [XLEN-1:0] eip_sw_7;
wire [XLEN-1:0] eip_sw_6;
wire [XLEN-1:0] eip_sw_4;
assign eip_sw_7 = eip_sw[7];
assign eip_sw_6 = eip_sw[6];
assign eip_sw_1 = eip_sw[1];
assign eip_sw_0 = eip_sw[0];
assign eip_sw_4 = eip_sw[4];*/
wire [XLEN-1:0] eip_final_0;
wire [XLEN-1:0] eip_final_1;
wire [XLEN-1:0] eip_final_2;
wire [XLEN-1:0] eip_final_3;
wire [XLEN-1:0] eip_final_4;
wire [XLEN-1:0] eip_final_5;
wire [XLEN-1:0] eip_final_6;
wire [XLEN-1:0] eip_final_7;
wire [XLEN-1:0] eip_final_8;
wire [XLEN-1:0] eip_final_9;
wire [XLEN-1:0] eip_final_10;
wire [XLEN-1:0] eip_final_11;
wire [XLEN-1:0] eip_final_12;
wire [XLEN-1:0] eip_final_13;
wire [XLEN-1:0] eip_final_14;
wire [XLEN-1:0] eip_final_15;
wire [XLEN-1:0] eip_final_16;
wire [XLEN-1:0] eip_final_17;
wire [XLEN-1:0] eip_final_18;
wire [XLEN-1:0] eip_final_19;
wire [XLEN-1:0] eip_final_20;
wire [XLEN-1:0] eip_final_21;
wire [XLEN-1:0] eip_final_22;
wire [XLEN-1:0] eip_final_23;
wire [XLEN-1:0] eip_final_24;
wire [XLEN-1:0] eip_final_25;
wire [XLEN-1:0] eip_final_26;
wire [XLEN-1:0] eip_final_27;
wire [XLEN-1:0] eie_final_0;
wire [XLEN-1:0] eie_final_1;
wire [XLEN-1:0] eie_final_2;
wire [XLEN-1:0] eie_final_3;
wire [XLEN-1:0] eie_final_4;
wire [XLEN-1:0] eie_final_5;
wire [XLEN-1:0] eie_final_6;
wire [XLEN-1:0] eie_final_7;
wire [XLEN-1:0] eie_final_8;
wire [XLEN-1:0] eie_final_9;
wire [XLEN-1:0] eie_final_10;
wire [XLEN-1:0] eie_final_11;
wire [XLEN-1:0] eie_final_12;
wire [XLEN-1:0] eie_final_13;
wire [XLEN-1:0] eie_final_14;
wire [XLEN-1:0] eie_final_15;
wire [XLEN-1:0] eie_final_16;
wire [XLEN-1:0] eie_final_17;
wire [XLEN-1:0] eie_final_18;
wire [XLEN-1:0] eie_final_19;
wire [XLEN-1:0] eie_final_20;
wire [XLEN-1:0] eie_final_21;
wire [XLEN-1:0] eie_final_22;
wire [XLEN-1:0] eie_final_23;
wire [XLEN-1:0] eie_final_24;
wire [XLEN-1:0] eie_final_25;
wire [XLEN-1:0] eie_final_26;
wire [XLEN-1:0] eie_final_27;
wire [XLEN-1:0] eithreshold_0    ;
wire [XLEN-1:0] eithreshold_1    ;
wire [XLEN-1:0] eithreshold_2    ;
wire [XLEN-1:0] eithreshold_3    ;
wire [XLEN-1:0] eithreshold_4    ;
wire [XLEN-1:0] eithreshold_5    ;
wire [XLEN-1:0] eithreshold_6    ;
wire [31:0] xtopei_0 ;
wire [31:0] xtopei_1 ;
wire [31:0] xtopei_2 ;
wire [31:0] xtopei_3 ;
wire [31:0] xtopei_4 ;
wire [31:0] xtopei_5 ;
wire [31:0] xtopei_6 ;

assign eip_final_0           = eip_final[0      ];               
assign eip_final_1           = eip_final[1      ];
assign eip_final_2           = eip_final[2      ];
assign eip_final_3           = eip_final[3      ];
assign eip_final_4           = eip_final[4      ];
assign eip_final_5           = eip_final[5      ];
assign eip_final_6           = eip_final[6      ];
assign eip_final_7           = eip_final[7      ];
assign eip_final_8           = eip_final[8      ];
assign eip_final_9           = eip_final[9      ];
assign eip_final_10          = eip_final[10     ];
assign eip_final_11          = eip_final[11     ];
assign eip_final_12          = eip_final[12     ];
assign eip_final_13          = eip_final[13     ];
assign eip_final_14          = eip_final[14     ];
assign eip_final_15          = eip_final[15     ];
assign eip_final_16          = eip_final[16     ];
assign eip_final_17          = eip_final[17     ];
assign eip_final_18          = eip_final[18     ];
assign eip_final_19          = eip_final[19     ];
assign eip_final_20          = eip_final[20     ];
assign eip_final_21          = eip_final[21     ];
assign eip_final_22          = eip_final[22     ];
assign eip_final_23          = eip_final[23     ];
assign eip_final_24          = eip_final[24     ];
assign eip_final_25          = eip_final[25     ];
assign eip_final_26          = eip_final[26     ];
assign eip_final_27          = eip_final[27     ];
assign eie_final_0           = eie[0      ];
assign eie_final_1           = eie[1      ];
assign eie_final_2           = eie[2      ];
assign eie_final_3           = eie[3      ];
assign eie_final_4           = eie[4      ];
assign eie_final_5           = eie[5      ];
assign eie_final_6           = eie[6      ];
assign eie_final_7           = eie[7      ];
assign eie_final_8           = eie[8      ];
assign eie_final_9           = eie[9      ];
assign eie_final_10          = eie[10     ];
assign eie_final_11          = eie[11     ];
assign eie_final_12          = eie[12     ];
assign eie_final_13          = eie[13     ];
assign eie_final_14          = eie[14     ];
assign eie_final_15          = eie[15     ];
assign eie_final_16          = eie[16     ];
assign eie_final_17          = eie[17     ];
assign eie_final_18          = eie[18     ];
assign eie_final_19          = eie[19     ];
assign eie_final_20          = eie[20     ];
assign eie_final_21          = eie[21     ];
assign eie_final_22          = eie[22     ];
assign eie_final_23          = eie[23     ];
assign eie_final_24          = eie[24     ];
assign eie_final_25          = eie[25     ];
assign eie_final_26          = eie[26     ];
assign eie_final_27          = eie[27     ];
assign eithreshold_0         = eithreshold[0    ];
assign eithreshold_1         = eithreshold[1    ];
assign eithreshold_2         = eithreshold[2    ];
assign eithreshold_3         = eithreshold[3    ];
assign eithreshold_4         = eithreshold[4    ];
assign eithreshold_5         = eithreshold[5    ];
assign eithreshold_6         = eithreshold[6    ];
assign xtopei_0         = xtopei[0    ];
assign xtopei_1         = xtopei[1    ];
assign xtopei_2         = xtopei[2    ];
assign xtopei_3         = xtopei[3    ];
assign xtopei_4         = xtopei[4    ];
assign xtopei_5         = xtopei[5    ];
assign xtopei_6         = xtopei[6    ];






























// 
integer s,t;
always @(posedge clk or negedge rstn)
begin
    if (~rstn)begin 
        eidelivery          <=  {NR_INTP_FILES{1'b0}}; 
        csr_wr_illegal      <=  1'b0; 
        for (s = 0; s < NR_INTP_FILES; s=s+1) begin
            eithreshold[s]  <=  {(XLEN){1'b0}}; 
            for(t=0; t< NR_REG;t=t+1) begin
                eip_sw[s*NR_REG +t]     <=  {XLEN{1'b0}}; 
                eie[s*NR_REG+t]         <=  {XLEN{1'b0}}; 
                eip_sw_wr[s*NR_REG+t]   <=  1'b0; 
            end
        end
    end
    /** IMSIC channel handler for interrupt file CSRs */
    else if (i_csr_wdata_vld) begin
        if (priv_is_illegal)
            csr_wr_illegal <=  1'b1;    
        else begin
            casez (csr_addr) 
                12'b0000_0011_????:begin
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
                12'b0000_10??_????:begin
                    if(csr_addr[5:0]< MUX_NR_REG) begin
                        if(XLEN == 32)begin
                            eip_sw[curr_intf_addr] <= (|curr_intf_addr) ? i_csr_wdata[XLEN-1:0] : {i_csr_wdata[XLEN-1:1],1'b0}; // interrupt 0 is invalid.
                            eip_sw_wr[curr_intf_addr] <= 1'b1;
                        end
                        else if (csr_addr[0] == 1'b1)
                            csr_wr_illegal <= 1'b1;                                       
                        else begin
                            eip_sw[curr_intf_addr] <= (|csr_addr[5:0]) ? i_csr_wdata[XLEN-1:0] : {i_csr_wdata[XLEN-1:1],1'b0};
                            eip_sw_wr[curr_intf_addr] <= 1'b1;
                        end  
                    end
                    else
                        csr_wr_illegal <= 1'b1;                                       
                end
                12'b0000_11??_????:begin
                    if(csr_addr[5:0]< MUX_NR_REG) begin
                        if(XLEN == 32)
                            eie[curr_intf_addr] <= i_csr_wdata[XLEN-1:0];
                        else if (csr_addr[0] == 1'b1)
                            csr_wr_illegal <= 1'b1;                                       
                        else 
                            eie[curr_intf_addr] <= i_csr_wdata[XLEN-1:0];
                    end
                    else
                        csr_wr_illegal <= 1'b1;                                       
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
        eip_sw_wr           <=  {{NR_INTP_FILES * NR_REG}{1'b0}}        ; 
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
            casez (csr_addr) 
                12'b0000_0011_????:begin
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
                12'b0000_10??_????:begin
                    if(csr_addr[5:0]< MUX_NR_REG) begin
                        if(XLEN == 32)
                            o_csr_rdata <= eip_final[curr_intf_addr];
                        else if (csr_addr[0] == 1'b0)
                            o_csr_rdata <= eip_final[curr_intf_addr];
                        else 
                            csr_rd_illegal <=  1'b1;                                       
                    end
                    else
                        csr_rd_illegal <= 1'b1;                                       
                end
                12'b0000_11??_????:begin
                    if(csr_addr[5:0]< MUX_NR_REG) begin
                        if(XLEN == 32)
                            o_csr_rdata <= eie[curr_intf_addr];
                        else if (csr_addr[0] == 1'b0)
                            o_csr_rdata <= eie[curr_intf_addr];
                        else 
                            csr_rd_illegal <=  1'b1;                                       
                    end
                    else
                        csr_rd_illegal <= 1'b1;                                       
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
i - reg number,
j - arrangement of interrupt number in i,
k*NR_REG - select the current interrupt file */
always @(*)begin
        for (k = 0; k < NR_INTP_FILES; k=k+1) begin
            xtopei[k] = 32'h0; 
            irq_out[k]= 1'b0; 
            for (int i = NR_REG-1; i >= 0; i=i-1) begin
                for (int j = XLEN-1; j >= 0; j=j-1) begin 
                    if ((eie[(k*NR_REG)+i][j] & eip_final[(k*NR_REG)+i][j]) & 
                        ((eithreshold[k] == 0) | (j < eithreshold[k]))) begin
                            xtopei[k][10:0]     = XLEN*i+j;  // curr  interrupt priority
                            xtopei[k][26:16]    = XLEN*i+j;  // curr  interrupt number.
                            irq_out[k]          = eidelivery[k];// If delivery is enable for this intp file, tell the hart 
                    end
                end
            end
        end
end   
assign o_irq[2:0]     = {irq_out[i_csr_vgein+1],irq_out[1:0]}; //select the vgein file for vs.
//always @(*)
//begin
//    o_xtopei[0]  = xtopei[0];
//    o_xtopei[1]  = xtopei[1];
//    o_xtopei[2]  = xtopei[i_csr_vgein+1];
//end
//assign o_xtopei[2:0]  = {xtopei[i_csr_vgein+1],xtopei[1:0]};
// ================================================================
endmodule
