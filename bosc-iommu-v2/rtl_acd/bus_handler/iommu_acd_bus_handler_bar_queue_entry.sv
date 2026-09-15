////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_bar_queue_entry
//      1. bar_pair info store
//      2. bar order ctrl, wait for all pre-bar transaction out
////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_bar_queue_entry #( //{{{
    parameter   IDX_WIDTH               = 8             ,
    parameter   BUS_ADDR_WIDTH          = 64            ,
    parameter   BUS_ID_WIDTH            = 8             ,
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6             ,
    parameter type          TQE_INFO_TYPE           = iommu_acd_pkg::tqe_info_t,
    parameter   IDX_VECTOR_WIDTH        = 2**IDX_WIDTH  
)(//{{{ IO
    input  logic                                    clk,
    input  logic                                    rstn,

    input  logic                                    update_i        ,
    input  TQE_INFO_TYPE                            trans_info_i    ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        pair_token_i    ,
    input  logic [IDX_VECTOR_WIDTH-1:0]             qentry_valid_i  ,
    input  logic                                    wbuf_dequeue_fifo_empty_i,
    output logic                                    valid_o         ,

    output logic                                    output_valid_o  ,
    input  logic                                    output_ready_i  ,
    output TQE_INFO_TYPE                            output_cont_o   ,
    output logic                                    output_wait_o   ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        output_pair_token_o,

    input  logic                                    spare_in        
    //}}}
);
//=== Declare ==={{{
    localparam S_IDLE   = 3'b000;
    localparam S_OUT    = 3'b100;
    localparam S_DEPEND = 3'b010;
    localparam S_WBUF   = 3'b101;

    logic [2:0]                                     cs, ns;

    TQE_INFO_TYPE                                   transaction_info_stored;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]               pair_token_stored;
    logic [IDX_VECTOR_WIDTH-1:0]                    depend_status;
//}}}

//=== Main Code ==={{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            transaction_info_stored <= 'd0;
            pair_token_stored       <= 'd0;
        end
        else begin
            if(cs==S_IDLE & ns==S_DEPEND) begin
                transaction_info_stored <= trans_info_i;
                pair_token_stored       <= pair_token_i;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            depend_status <= 'd0;
        end
        else begin
            if(cs==S_IDLE & ns==S_DEPEND) begin
                depend_status <= qentry_valid_i;
            end
            else begin
                for(int unsigned ii=0; ii<IDX_VECTOR_WIDTH; ii++) begin
                    if(depend_status[ii] & (~qentry_valid_i[ii]))
                        depend_status[ii] <= 1'b0;
                end
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end

    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(update_i)
                ns = S_DEPEND;
            else
                ns = S_IDLE;
        end
        S_DEPEND: begin
            if(depend_status=='d0)
                ns = S_WBUF;
            else
                ns = S_DEPEND;
        end
        S_WBUF: begin
            if(wbuf_dequeue_fifo_empty_i)
                ns = S_OUT;
            else
                ns = S_WBUF;
        end
        S_OUT: begin
            if(output_ready_i)
                ns = S_IDLE;
            else
                ns = S_OUT;
        end
        default: ns = S_IDLE;
        endcase
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            output_valid_o   <= 1'b0;
        else begin
            if(cs==S_WBUF & ns==S_OUT)
                output_valid_o <= 1'b1;
            else if(cs==S_OUT & ns==S_IDLE)
                output_valid_o <= 1'b0;
        end
    end

    assign output_cont_o = transaction_info_stored;
    assign output_pair_token_o = pair_token_stored;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            output_wait_o   <= 1'b0;
        else begin
            if(cs==S_DEPEND & ns==S_WBUF)
                output_wait_o <= 1'b1;
            else if(cs==S_WBUF & ns==S_OUT)
                output_wait_o <= 1'b0;
        end
    end

    assign valid_o = (cs!=S_IDLE);
//}}}
endmodule//}}}



