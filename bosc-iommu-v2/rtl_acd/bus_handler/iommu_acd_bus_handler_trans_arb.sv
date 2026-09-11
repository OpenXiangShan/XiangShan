module iommu_acd_bus_handler_trans_arb #(
    parameter   ARB_TYPE        = 0,
    parameter   REQ_NUM         = 2,
    parameter type          DATA_TYPE       = logic,
    parameter   AXIVLDRDY       = 1
)(                                          
    input  logic                            clk,
    input  logic                            rstn,
                                            
    input  logic [REQ_NUM-1:0]              req_i,
    input  logic [REQ_NUM-1:0]              req_prior_i,
    input  DATA_TYPE [REQ_NUM-1:0]          data_i,
    output logic [REQ_NUM-1:0]              gnt_o,
                                            
    output logic                            req_o,
    output DATA_TYPE                        data_o,
    input  logic                            gnt_i
);
//=== Declare === {{{
localparam LVL_DEPTH = $clog2(REQ_NUM);

DATA_TYPE [REQ_NUM-2:0]                     data_nodes;
logic     [REQ_NUM-2:0]                     req_nodes;
logic     [REQ_NUM-2:0]                     gnt_nodes;
logic     [REQ_NUM-2:0]                     req_prior_nodes;
                                            
logic     [REQ_NUM-1:0]                     gnt_q;
logic     [REQ_NUM-1:0]                     rr_prior;
//}}}

//=== OUT ==={{{
assign gnt_nodes[0] = gnt_i;
assign req_o        = req_nodes[0];
assign data_o       = data_nodes[0];
//}}}

//=== LOCK ==={{{
logic  lock_d, lock_q, accept;
logic [REQ_NUM-1:0] req_q, req_d;
logic [REQ_NUM-1:0] req_prior_q, req_prior_d;

assign lock_d       = req_o & ~gnt_i;
assign accept       = req_o & gnt_i;
assign req_d        = (lock_q) ? req_q : req_i;
assign req_prior_d  = (lock_q) ? req_prior_q : req_prior_i;

always@(posedge clk or negedge rstn) begin
    if (~rstn) begin
        lock_q <= '0;
    end 
    else begin
        lock_q <= lock_d;
    end
end
always@(posedge clk or negedge rstn) begin
    if (~rstn) begin
        req_q       <= '0;
        req_prior_q <= 'd0;
    end
    else begin
        req_q       <= req_d;
        req_prior_q <= req_prior_d;
    end
end
//}}}

//=== RR {{{
generate
    if(ARB_TYPE==1) begin : rr_type_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                gnt_q <= 'd0;
            else begin
                if(accept)
                    gnt_q <= gnt_o;
                else
                    gnt_q <= gnt_q;
            end
        end
    end
    else begin : non_rr_type_gen
        assign gnt_q = 'd0;
    end
endgenerate
//assign rr_prior = ~gnt_q & req_q;
assign rr_prior = ~gnt_q & req_d;
//}}}

//=== ARB TREE === {{{
//                      node[0]                                 level 0
//                  Idx0=0,Idx1=1
//    
//    
//          node[1]                 node[2]                     level 1
//      l=0,Idx0=1,Idx1=3       l=1,Idx0=2,Idx1=5
//    
//    
//    node[3]   node[4]         node[5]     node[6]             level 2
// l=0,Idx0=3  l=1,Idx0=4      l=2,Idx0=5  l=3,Idx0=6
//
//    
generate
    if(REQ_NUM==1) begin : req_num0_gen
        assign req_nodes[0]  = req_i;
        assign data_nodes[0] = data_i;
        assign gnt_o         = gnt_nodes[0];
        assign req_prior_nodes[0]   = req_prior_i;
    end
    else begin : req_not_num0_gen
        for(genvar level=0; level<LVL_DEPTH; level++) begin : level_gen
            for(genvar l=0; l<2**level; l++) begin : level_inter_gen
                logic sel;
                localparam Idx0 = 2**level-1+l;                                    // current node number
                localparam Idx1 = (2**(level+1)-1) + (l*2);                        // lower level connected left node number
                if(ARB_TYPE == 0) begin : arb_type0_gen
                    if(level == (LVL_DEPTH-'d1)) begin : leaf_level
                        assign req_nodes[Idx0]      = req_d[l*2] | req_d[l*2+1];
                        assign req_prior_nodes[Idx0]= req_prior_d[l*2] | req_prior_d[l*2+1];
                        assign sel                  = req_prior_d[l*2+1] ?  req_d[l*2+1] :      // if right prior==1, sele right if right req==1
                                                                          (~req_d[l*2]);        // always select left if it has req
                        assign data_nodes[Idx0]     = sel ? data_i[l*2+1] : data_i[l*2];
                        assign gnt_o[l*2]           = gnt_nodes[Idx0] & (AXIVLDRDY | req_d[l*2])   & ~sel;
                        assign gnt_o[l*2+1]         = gnt_nodes[Idx0] & (AXIVLDRDY | req_d[l*2+1]) & sel;
                    end
                    else begin :  non_leaf_level
                        assign req_nodes[Idx0]      = req_nodes[Idx1] | req_nodes[Idx1+1];
                        assign req_prior_nodes[Idx0]= req_prior_nodes[Idx1] | req_prior_nodes[Idx1+1];
                        assign sel                  = req_prior_nodes[Idx1+1] ? req_nodes[Idx1+1] : (~req_nodes[Idx1]);
                        assign data_nodes[Idx0]     = sel ? data_nodes[Idx1+1] : data_nodes[Idx1];
                        assign gnt_nodes[Idx1]      = gnt_nodes[Idx0] & ~sel;
                        assign gnt_nodes[Idx1+1]    = gnt_nodes[Idx0] & sel;
                    end
                end
                else begin
                    if(level == (LVL_DEPTH-'d1)) begin : leaf_level
                        assign req_nodes[Idx0]      = req_d[l*2] | req_d[l*2+1];
                        assign req_prior_nodes[Idx0]= rr_prior[l*2] | rr_prior[l*2+1];
                        assign sel                  = rr_prior[l*2+1] ?  req_d[l*2+1] :      // if right prior==1, sele right if right req==1
                                                                          (~req_d[l*2]);        // always select left if it has req
                        assign data_nodes[Idx0]     = sel ? data_i[l*2+1] : data_i[l*2];
                        assign gnt_o[l*2]           = gnt_nodes[Idx0] & (AXIVLDRDY | req_d[l*2])   & ~sel;
                        assign gnt_o[l*2+1]         = gnt_nodes[Idx0] & (AXIVLDRDY | req_d[l*2+1]) & sel;
                    end
                    else begin :  non_leaf_level
                        assign req_nodes[Idx0]      = req_nodes[Idx1] | req_nodes[Idx1+1];
                        assign req_prior_nodes[Idx0]= req_prior_nodes[Idx1] | req_prior_nodes[Idx1+1];
                        assign sel                  = req_prior_nodes[Idx1+1] ? req_nodes[Idx1+1] : (~req_nodes[Idx1]);
                        assign data_nodes[Idx0]     = sel ? data_nodes[Idx1+1] : data_nodes[Idx1];
                        assign gnt_nodes[Idx1]      = gnt_nodes[Idx0] & ~sel;
                        assign gnt_nodes[Idx1+1]    = gnt_nodes[Idx0] & sel;
                    end
                end
            end
        end
    end
endgenerate
//}}}

    
endmodule
