module iommu_acd_hpm_tlbwrap(
//{{{ IO
    input  logic                    clk,
    input  logic                    rstn,
    //
    input  logic                    lookup_req_valid_i,
    input  logic                    lookup_req_ready_i,
    input  logic                    lookup_ack_valid_i,
    input  logic                    lookup_ack_hit_i,
    input  logic                    ftlb_lookup_ack_valid_i,
    input  logic                    ftlb_lookup_ack_ready_i,
    //
    input  logic [3:0]              hpm_cnt_inhibit_i,
    output logic [63:0]             hpm_cnt_o[3:0],
    //
    input  logic                    spare_in
//}}}
);
//{{{ Declare
    logic [63:0]                    lkp_cnt;
    logic [63:0]                    ack_cnt;
    logic [63:0]                    hit_cnt;
    logic [63:0]                    fhit_cnt;
//}}}

//{{{ Main Code
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            lkp_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[0] & lookup_req_valid_i & lookup_req_ready_i)
                lkp_cnt <= lkp_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[0] = lkp_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            ack_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[1] & lookup_ack_valid_i)
                ack_cnt <= ack_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[1] = ack_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            hit_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[2] & lookup_ack_valid_i & lookup_ack_hit_i)
                hit_cnt <= hit_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[2] = hit_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            fhit_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[3] & ftlb_lookup_ack_valid_i & ftlb_lookup_ack_ready_i)
                fhit_cnt <= fhit_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[3] = fhit_cnt;

//}}}
endmodule



