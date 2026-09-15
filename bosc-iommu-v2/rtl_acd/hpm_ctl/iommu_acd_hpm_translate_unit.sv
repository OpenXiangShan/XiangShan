module iommu_acd_hpm_translate_unit(
//{{{ IO
    input  logic                    clk,
    input  logic                    rstn,
    //
    input  logic                    translate_req_valid_i,
    input  logic                    translate_req_ready_i,
    input  logic                    translate_ack_valid_i,
    input  logic                    translate_ack_success_i,
    input  logic                    ptw_req_valid_i,
    input  logic                    ptw_req_ready_i,
    input  logic                    ptw_ack_valid_i,
    input  logic                    ptw_ack_success_i,
    //
    input  logic [5:0]              hpm_cnt_inhibit_i,
    output logic [63:0]             hpm_cnt_o[5:0],
    //
    input  logic                    spare_in
//}}}
);
//{{{ Declare
    logic [63:0]                    treq_cnt, tack_cnt, tsucc_cnt, preq_cnt, pack_cnt, psucc_cnt;
//}}}

//{{{ Main Code 
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            treq_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[0] & translate_req_valid_i & translate_req_ready_i)
                treq_cnt <= treq_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[0] = treq_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            tack_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[1] & translate_ack_valid_i)
                tack_cnt <= tack_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[1] = tack_cnt;
    
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            tsucc_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[2] & translate_ack_valid_i & translate_ack_success_i)
                tsucc_cnt <= tsucc_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[2] = tsucc_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            preq_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[3] & ptw_req_valid_i & ptw_req_ready_i)
                preq_cnt <= preq_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[3] = preq_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            pack_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[4] & ptw_ack_valid_i)
                pack_cnt <= pack_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[4] = pack_cnt;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            psucc_cnt <= 'd0;
        else begin
            if(~hpm_cnt_inhibit_i[5] & ptw_ack_valid_i & ptw_ack_success_i)
                psucc_cnt <= psucc_cnt + 'd1;
        end
    end
    assign hpm_cnt_o[5] = psucc_cnt;


//}}}
endmodule
