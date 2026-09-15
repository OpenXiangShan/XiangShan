


module iommu_acd_bus_handler_regslice #(
    parameter   PAYLD_WIDTH     = 8
)(
    input  logic                            clk,
    input  logic                            rstn,
    
    input  logic                            valid_src,
    output logic                            ready_src,
    input  logic [PAYLD_WIDTH-1:0]          payload_src,
    
    output logic                            valid_dst,
    input  logic                            ready_dst,
    output logic [PAYLD_WIDTH-1:0]          payload_dst
);
//=== Declare {{{
    logic                                   store_sel, out_sel;
    logic [1:0]                             valid;
    logic [PAYLD_WIDTH-1:0]                 payload[1:0];
    logic [1:0]                             will_out;
    logic [1:0]                             will_store;
//}}}

//=== MainCode {{{
    always@(posedge clk  or negedge rstn) begin
        if(~rstn)
            store_sel <= 1'b0;
        else begin
            if(valid_src & ready_src)
                store_sel <= ~store_sel;
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            out_sel <= 1'b0;
        else begin
            //if(valid_dst & ready_dst)
            //    out_sel <= ~out_sel;
            if(   ~out_sel & (will_out[0] | ~valid[0])
                & (will_store[1] | (~will_out[1] & valid[1]))
              )       // current sel 0 to out, 0 is or will out and next input will store to 1
                out_sel <= 1'b1;
            else if(   out_sel & (will_out[1] | ~valid[1])
                     & (will_store[0] | (~will_out[0] & valid[0]))
                   )  // current sel 1 to out, 1 is or will out and next input will store to 0
                out_sel <= 1'b0;
        end
    end

    assign will_out[0]  = ~out_sel & valid_dst & ready_dst;
    assign will_out[1]  =  out_sel & valid_dst & ready_dst;
    assign will_store[0]= ~store_sel & valid_src & ready_src;
    assign will_store[1]=  store_sel & valid_src & ready_src;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            payload[0]  <= 'd0;
        else if(will_store[0])
            payload[0]  <= payload_src;
        else if(will_out[0])
            payload[0]  <= 'd0;
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            valid[0]    <= 1'b0;
        else begin
            if(will_store[0])
                valid[0]    <= 1'b1;
            else if(will_out[0])
                valid[0]    <= 1'b0;
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            payload[1]  <= 'd0;
        else if(will_store[1])
            payload[1]  <= payload_src;
        else if(will_out[1])
            payload[1]  <= 'd0;
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            valid[1]    <= 1'b0;
        else begin
            if(will_store[1])
                valid[1]    <= 1'b1;
            else if(will_out[1])
                valid[1]    <= 1'b0;
        end
    end

    assign ready_src    = ~(&valid);
    assign valid_dst    = |valid;
    assign payload_dst  = payload[out_sel];
//}}}

endmodule
