////////////////////////////////////////////////////////////////////////////
// multi_bit_1 check
////////////////////////////////////////////////////////////////////////////
module iommu_multi_bit_1_check #( //{{{
    parameter WIDTH = 8
)(
    input  logic [WIDTH-1:0]    din,
    output logic                ok
);
    logic [WIDTH-1:0] check;

genvar pp;
generate
    assign check[0] = 1'b0;
    for(pp=1; pp<WIDTH; pp++) begin : check_gen
        assign check[pp] = din[pp] & (din[(pp-1):0] != 'd0);
    end
endgenerate

    assign ok = |check;
endmodule
//}}}




