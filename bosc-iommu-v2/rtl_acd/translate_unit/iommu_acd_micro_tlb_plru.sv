module iommu_acd_micro_tlb_plru #(
    parameter   WIDTH       = 8
)(
    input  logic                        clk_i,
    input  logic                        rst_ni,

    input  logic [WIDTH-1:0]            lu_hit,
    input  logic                        lu_hit_valid,

    output logic [WIDTH-1:0]            replace_en
);
//=== Declare {{{
    localparam REQ_NUM                  = WIDTH;
    localparam LEVEL_NUM                = $clog2(REQ_NUM);

    logic [REQ_NUM-1:0]                 lu_hit_masked;
    //logic [REQ_NUM-1:0]                 lu_hit_masked_shifted[LEVEL_NUM-1:0];

    logic [1:0]                         direct_in[REQ_NUM-2:0];
    logic [REQ_NUM-2:0]                 direct_out;
    logic [REQ_NUM-2:0]                 point_in;
    logic [1:0]                         point_out[REQ_NUM-2:0];
    logic                               clk, rstn;
//}}}

//=== MainCode {{{
    assign clk = clk_i;
    assign rstn = rst_ni;

    assign lu_hit_masked = lu_hit_valid ? lu_hit : 'd0;

genvar i, j;
generate
    for(i=0; i<LEVEL_NUM; i++) begin : plru_level_gen
        //assign lu_hit_masked_shifted[i] = lu_hit_masked >> (LEVEL_NUM-i-1);
        //assign update[i] = lu_hit_masked_shifted[i][0];

        for(j=0; j<2**i; j++) begin : plru_node_gen
            if(i==0)
                assign point_in[2**i-1+j] = 1'b1;
            else
                assign point_in[2**i-1+j] = point_out[2**(i-1)-1+(j/2)][j%2];

            if(i==LEVEL_NUM-1) begin
                assign direct_in[2**i-1+j][0] = lu_hit_masked[2*j+0];
                assign direct_in[2**i-1+j][1] = lu_hit_masked[2*j+1];
            end
            else begin
                assign direct_in[2**i-1+j][0] = direct_out[2**(i+1)-1+(2*j)];
                assign direct_in[2**i-1+j][1] = direct_out[2**(i+1)-1+(2*j+1)];
            end

            iommu_acd_micro_tlb_plru_node U_node(
                .clk                (clk                    ),
                .rstn               (rstn                   ),
                .update_valid       (lu_hit_valid           ),
                .direct_in          (direct_in[2**i-1+j]    ),
                .direct_out         (direct_out[2**i-1+j]   ),
                .point_in           (point_in[2**i-1+j]     ),
                .point_out          (point_out[2**i-1+j]    ) 
            );
        end
    end
endgenerate

    always@(*) begin
        for(int unsigned ii=LEVEL_NUM-1; ii<LEVEL_NUM; ii++) begin
            for(int unsigned jj=0; jj<2**ii; jj++) begin
                replace_en[2*jj+0] = point_out[2**ii-1+jj][0];
                replace_en[2*jj+1] = point_out[2**ii-1+jj][1];
            end
        end
    end
//}}}


endmodule


module iommu_acd_micro_tlb_plru_node (
    input  logic        clk,
    input  logic        rstn,
    input  logic        update_valid,
    input  logic [1:0]  direct_in,
    output logic        direct_out,
    input  logic        point_in,
    output logic [1:0]  point_out
);
    logic               point;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            point <= 1'b0;
        else begin
            if(direct_out & update_valid) begin
                case(direct_in)
                2'b01: point <= 1'b1;
                2'b10: point <= 1'b0;
                2'b11: point <= point;
                default:point <= point;
                endcase
            end
        end
    end

    assign direct_out = |direct_in;

    assign point_out[0]  = point_in & ~point;
    assign point_out[1]  = point_in &  point;

endmodule
