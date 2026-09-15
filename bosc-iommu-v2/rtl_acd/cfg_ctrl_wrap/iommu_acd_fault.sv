module iommu_acd_fault import iommu_acd_pkg::*; #(
    parameter   FAULT_TOKEN_WIDTH           = 5,                    // should not bigger than 12
    parameter   FAULT_INFLY_NUM             = 2**FAULT_TOKEN_WIDTH,
    parameter   SPARE_PARAM                 = 0
)(                                                      
//{{{ IO                                                
    input  logic                                        clk                     ,
    input  logic                                        rstn                    ,
    // TLB_QUEUE                                        
    input  logic                                        fault_rpt_valid_i       ,
    output logic                                        fault_rpt_ready_o       ,
    input  FAULT_RPT_TYPE                               fault_rpt_i             ,
    // T2C IF                                           
    input  logic                                        msg_valid_i             ,
    output logic                                        msg_ready_o             ,
    input  MSG_FAULT_ACK_TYPE                           msg_wdata_i             ,
    // C2T IF                                           
    output logic                                        msg_fvalid_o            ,
    input  logic                                        msg_fready_i            ,
    output logic [63:0]                                 msg_fdata_o             ,
    output logic                                        msg_flast_o             ,
    //                                                  
    output logic                                        fault_buf_overflow_err_o,
    //                                                  
    input  logic                                        spare_in                 
//}}}                                                   
);                                                      
//=== Declare {{{                                       
    logic [FAULT_TOKEN_WIDTH:0]                         token;
                                                        
    logic [3:0]                                         ocnt;
    logic                                               fifo_push, fifo_pop;
    logic                                               fifo_full, fifo_empty;
    FAULT_RPT_TYPE                                      fifo_in,   fifo_out;
                                                        
    MSG_FAULT_RPT_TYPE                                  msg_fault_rpt;
    logic                                               fault_ack_got;
//}}}

//=== MainCode {{{

//=== FIFO {{{
    assign fifo_push= fault_rpt_valid_i & ~fifo_full;
    assign fault_rpt_ready_o = ~fifo_full;
    assign fifo_in  = fault_rpt_i;

    assign fifo_pop = (msg_fvalid_o & msg_fready_i & ocnt=='d4);

    iommu_acd_bus_handler_sync_fifo #(
    /*parameter  */ .WIDTH          ($bits(FAULT_RPT_TYPE)  ), // = 128,
    /*parameter  */ .DEPTH          (FAULT_INFLY_NUM        ), // = 32,
    /*parameter  */ .SPARE_PARA     (0                      )  // = 0
    ) U_fifo(
    /*input  logic                           */ .clk                    (clk                    ),
    /*input  logic                           */ .rstn                   (rstn                   ),
    /*input  logic                           */ .push_i                 (fifo_push              ),
    /*output logic                           */ .full_o                 (fifo_full              ),
    /*output logic                           */ .afull_o                (                       ), // almost full
    /*input  logic [WIDTH-1:0]               */ .wdata_i                (fifo_in                ),
    /*input  logic                           */ .pop_i                  (fifo_pop               ),
    /*output logic                           */ .empty_o                (fifo_empty             ),
    /*output logic                           */ .aempty_o               (                       ),
    /*output logic [WIDTH-1:0]               */ .rdata_o                (fifo_out               ),
    /*input  logic                           */ .spare_in               (1'b0                   ) 
    );
//}}}

//=== FAULT_RPT MSG OUT {{{
    //assign fault_buf_overflow_err_o = fault_rpt_valid_i & fifo_full;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            ocnt <= 'd0;
        else begin
            if(msg_fvalid_o & msg_fready_i) begin
                if(fifo_pop)
                    ocnt <= 'd0;
                else
                    ocnt <= ocnt + 'd1;
            end
        end
    end

    assign msg_fault_rpt.msg_code = MSGCODE_FAULT_PRT;
    assign msg_fault_rpt.reserved0= 60'd0;
    assign msg_fault_rpt.rpt      = fifo_out;

    assign msg_fvalid_o = ~fifo_empty & (token > 'd0);
    assign msg_fdata_o  = (ocnt=='d0) ? {msg_fault_rpt.reserved0, msg_fault_rpt.msg_code} :
                          (ocnt=='d1) ? {msg_fault_rpt.rpt.did, msg_fault_rpt.rpt.ttyp, msg_fault_rpt.rpt.priv, msg_fault_rpt.rpt.pv, msg_fault_rpt.rpt.pid, msg_fault_rpt.rpt.cause} :
                          (ocnt=='d2) ? 'd0 :
                          (ocnt=='d3) ? msg_fault_rpt.rpt.ioval :
                          (ocnt=='d4) ? msg_fault_rpt.rpt.ioval2 : 'd0;

    assign msg_flast_o  = (ocnt=='d4);
//}}}

//=== FAULT_ACK MSG IN {{{
    assign msg_ready_o = 1'b1;
    assign fault_ack_got = msg_valid_i & msg_ready_o;
//}}}

//=== TOKEN CTRL {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            token <= FAULT_INFLY_NUM;
        else begin
            case({fault_ack_got, fifo_pop})
            2'b00: token <= token;
            2'b10: token <= token+'d1;
            2'b01: token <= token-'d1;
            2'b11: token <= token;
            default:token<= token;
            endcase
        end
    end
//}}}

    assign fault_buf_overflow_err_o = (~fifo_empty) & (token=='d0);
//}}}
endmodule
