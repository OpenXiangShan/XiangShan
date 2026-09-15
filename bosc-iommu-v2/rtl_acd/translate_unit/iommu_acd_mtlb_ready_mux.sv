


module iommu_acd_mtlb_ready_mux #(
    parameter  NUM     = 2
)(
    input  logic                    clk,
    input  logic                    rstn,
    input  logic                    valid_i,
    output logic                    ready_o,
    output logic [NUM-1:0]          valid_o,
    input  logic [NUM-1:0]          ready_i
);
    logic  [NUM-1:0]                valid_mask;
    logic  [NUM-1:0]                valid_accp;

    logic  [NUM-1:0] [NUM-1:0]      valid_ready_other;
    logic  [NUM-1:0] [NUM-1:0]      valid_accp_other;

genvar i;
generate
    for(i=0; i<NUM; i++) begin
        assign valid_accp[i]        = (ready_i[i] | valid_mask[i]);
        assign valid_ready_other[i] = ready_i | (NUM'(1)<<i);
        assign valid_accp_other[i]  = valid_accp | (NUM'(1)<<i);
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                valid_mask[i] <= 1'b0;
            else if(valid_mask[i] & (&valid_accp_other[i]))                     // all other accepted
                valid_mask[i] <= 1'b0;
            else if(~(&valid_accp_other[i]) & valid_accp[i] & valid_o[i])       // any other not accepted, and current accepted
                valid_mask[i] <= 1'b1;
        end
        assign valid_o[i] = valid_i & ~valid_mask[i];
    end
endgenerate
    
    assign ready_o = &valid_accp;

endmodule
