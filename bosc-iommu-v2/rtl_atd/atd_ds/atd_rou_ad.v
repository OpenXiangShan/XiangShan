//*********************************************//
//Project: 	rv_iommu_bosc
//FileName: 	atd_rou_ad.v
//Version: 	1.0
//Description:address router
//Author: 	
///*********************************************///


module atd_rou_ad
#(
   parameter pPORT_NUM = 32
  ,parameter pDATA_W   = 512 
  ,parameter pARID_W   = 5
  ,parameter pRMID_W   = 5
  ,parameter pMODE_SEL = 0 //0:no pool version,1:pool verison
  ,parameter pCHN_SEL  = 1 //0:addr 1:data 
  ,parameter TOP_BUF_SIZE = 32 // number of top buffer 
  ,parameter TOP_BUF_VALID= 'hfff // top buffer mask
  ,parameter BOT_BUF_SIZE = 32 // number of bottom buffer 
  ,parameter BOT_BUF_VALID= 'hfff // bottom buffer mask
  
)(

//input one port req
input [pDATA_W-1:0]             port_araddr,
input [pARID_W-1:0]             port_arid,
input [pRMID_W-1:0]             port_rmid,
input                           port_arvalid,
output                          port_arready,

//output 32 port req
input                            clk,pin_rstn,
input  [pPORT_NUM-1:0]           buffer_grant,
output [pPORT_NUM-1:0]           buffer_req,
output [pPORT_NUM*pDATA_W-1:0]   buffer_araddr,
output [pPORT_NUM*pARID_W-1:0]   buffer_arid

);


wire rstn;
assign rstn = pin_rstn;
//atd_sync2_rstn u_sync2_rstn(
//.clk    (clk),
//.rst_n  (pin_rstn),
//.so     (rstn)
//);


// top buffer 
//========================================
reg[TOP_BUF_SIZE-1:0] top_e_full;
reg[pDATA_W*TOP_BUF_SIZE-1:0] top_e_araddr;
reg [pARID_W*TOP_BUF_SIZE-1:0] top_e_arid; 
reg [pRMID_W*TOP_BUF_SIZE-1:0] top_e_rmid; 
wire[TOP_BUF_SIZE-1:0] top_e_bus;
//========================================
// top buffer 
//========================================
localparam deRMID_W = pPORT_NUM;
reg [BOT_BUF_SIZE-1:0]      bot_e_full;
reg [pDATA_W*BOT_BUF_SIZE-1:0] bot_e_araddr;
reg [pARID_W*BOT_BUF_SIZE-1:0] bot_e_arid; 
reg [deRMID_W*BOT_BUF_SIZE-1:0] bot_e_rmid; 
//========================================

wire[BOT_BUF_SIZE-1:0] e_ren; // bottom buffer clear signals

//output 32 port req
//========================================
wire [BOT_BUF_SIZE*pPORT_NUM-1:0] e_req;
wire[BOT_BUF_SIZE*pPORT_NUM*pDATA_W-1:0] e_araddr_req;
wire[BOT_BUF_SIZE*pPORT_NUM*pARID_W-1:0] e_arid_req;
//========================================
// bottom buffer item clearing logic
//========================================
genvar bus_id;
generate
for(bus_id = 0; bus_id < BOT_BUF_SIZE; bus_id = bus_id + 1) begin : E_REN_BUS
    assign e_ren[bus_id] = (|(buffer_grant&e_req[pPORT_NUM*bus_id+:pPORT_NUM])) && BOT_BUF_VALID[bus_id];
end
endgenerate
//========================================


wire[BOT_BUF_SIZE-1:0] bot_e_take = bot_e_full & BOT_BUF_VALID;
wire load_allowed = (|(top_e_full & TOP_BUF_VALID)) && (!(&(bot_e_full | (~BOT_BUF_VALID)))); //top buffers are not empty and bottom buffers are not all full

reg[pDATA_W-1:0] top_e_port_araddr;
reg[pARID_W-1:0] top_e_port_arid;
reg[pRMID_W-1:0] top_e_port_rmid;

wire load;
reg[TOP_BUF_SIZE-1:0] top_take;

assign port_arready = ~(&(top_e_full | (~TOP_BUF_VALID)));

//top buffers load and clear operations
//========================================
generate
genvar top_e_id;

for(top_e_id = 0; top_e_id < 1; top_e_id = top_e_id + 1) begin : top_op_0
    //data
    always@(posedge clk ) begin
        if(port_arvalid && port_arready && (!top_e_full[top_e_id]) && TOP_BUF_VALID[top_e_id]) begin
            top_e_araddr[pDATA_W*top_e_id+:pDATA_W] <= port_araddr;
            top_e_arid[pARID_W*top_e_id+:pARID_W] <= port_arid;
        end
    end     

    //control 
    always@(posedge clk or negedge rstn) begin
        if(!rstn) begin
            top_e_rmid[pRMID_W*top_e_id+:pRMID_W] <= 0;
            top_e_full[top_e_id] <= 0;
        end else begin
            if(port_arvalid && port_arready && (!top_e_full[top_e_id]) && TOP_BUF_VALID[top_e_id]) begin
                top_e_rmid[pRMID_W*top_e_id+:pRMID_W] <= port_rmid; 
                top_e_full[top_e_id] <= 1'b1;                       
            end else if(top_take[top_e_id] && load && load_allowed && TOP_BUF_VALID[top_e_id]) begin
                top_e_full[top_e_id] <= 1'b0;       
            end
        end
    end     
end

for(top_e_id = 1; top_e_id < TOP_BUF_SIZE; top_e_id = top_e_id + 1) begin : top_op
    //data
    always@(posedge clk ) begin
        if(port_arvalid && port_arready && (!top_e_full[top_e_id]) && (&top_e_full[top_e_id-1:0]) && TOP_BUF_VALID[top_e_id]) begin
            top_e_araddr[pDATA_W*top_e_id+:pDATA_W] <= port_araddr;
            top_e_arid[pARID_W*top_e_id+:pARID_W] <= port_arid;
        end
    end     

    //control
    always@(posedge clk or negedge rstn) begin
        if(!rstn) begin
            top_e_rmid[pRMID_W*top_e_id+:pRMID_W] <= 0;
            top_e_full[top_e_id] <= 0;
        end else begin
            if(port_arvalid && port_arready && (!top_e_full[top_e_id]) && (&top_e_full[top_e_id-1:0]) && TOP_BUF_VALID[top_e_id]) begin
                top_e_rmid[pRMID_W*top_e_id+:pRMID_W] <= port_rmid; 
                top_e_full[top_e_id] <= 1'b1;                       
            end else if(top_take[top_e_id] && load && load_allowed && TOP_BUF_VALID[top_e_id]) begin
                top_e_full[top_e_id] <= 1'b0;       
            end
        end
    end     
end

endgenerate

//========================================
//bottom buffers load and clear operations
//========================================
genvar bot_e_id;

generate
for(bot_e_id = 0; bot_e_id < 1; bot_e_id = bot_e_id + 1) begin : bot_op_0
    //data
    always@(posedge clk ) begin
        if(!bot_e_take[bot_e_id] && load && BOT_BUF_VALID[bot_e_id]) begin
            bot_e_araddr[pDATA_W*bot_e_id+:pDATA_W] <= top_e_port_araddr;
            bot_e_arid[pARID_W*bot_e_id+:pARID_W] <= top_e_port_arid;
        end
    end     

    //control
    always@(posedge clk or negedge rstn) begin
        if(!rstn) begin
            bot_e_full[bot_e_id] <= 0;
            //bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
        end else begin 
            if(!bot_e_take[bot_e_id] && load && BOT_BUF_VALID[bot_e_id]) begin
                bot_e_full[bot_e_id] <= 1'b1;
                //bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 1 << top_e_port_rmid;           
            end else if(e_ren[bot_e_id] && BOT_BUF_VALID[bot_e_id]) begin
                bot_e_full[bot_e_id] <= 1'b0;
            end
        end
    end     
end

for(bot_e_id = 1; bot_e_id < BOT_BUF_SIZE; bot_e_id = bot_e_id + 1) begin : bot_op
    //data
    always@(posedge clk ) begin
        if(!bot_e_take[bot_e_id] && (&bot_e_take[bot_e_id-1:0]) && load && BOT_BUF_VALID[bot_e_id]) begin
            bot_e_araddr[pDATA_W*bot_e_id+:pDATA_W] <= top_e_port_araddr;
            bot_e_arid[pARID_W*bot_e_id+:pARID_W] <= top_e_port_arid;
        end
    end     

    //control
    always@(posedge clk or negedge rstn) begin
        if(!rstn) begin
            bot_e_full[bot_e_id] <= 0;
            //bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
        end else begin 
            if(!bot_e_take[bot_e_id] && (&bot_e_take[bot_e_id-1:0]) && load && BOT_BUF_VALID[bot_e_id]) begin
                bot_e_full[bot_e_id] <= 1'b1;
                //bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 1 << top_e_port_rmid;           
            end else if(e_ren[bot_e_id] && BOT_BUF_VALID[bot_e_id]) begin
                bot_e_full[bot_e_id] <= 1'b0;
            end
        end
    end     
end

endgenerate

generate
if (pPORT_NUM == 2 && pRMID_W == 1) begin
	for(bot_e_id = 0; bot_e_id < 1; bot_e_id = bot_e_id + 1) begin : bot_op_rmid_0
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	1'b0:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 2'b01;		
			        	1'b1:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 2'b10;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 2'b01;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif			        
			        	end
		            endcase
	            end
	        end
	    end     
	end
	
	for(bot_e_id = 1; bot_e_id < BOT_BUF_SIZE; bot_e_id = bot_e_id + 1) begin : bot_op_rmid
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && (&bot_e_take[bot_e_id-1:0]) && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	1'b0:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 2'b01;		
			        	1'b1:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 2'b10;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 2'b01;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end
	
end else if (pPORT_NUM == 3 && pRMID_W == 2) begin
	for(bot_e_id = 0; bot_e_id < 1; bot_e_id = bot_e_id + 1) begin : bot_op_rmid_0
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	2'b00:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b001;		
			        	2'b01:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b010;		
			        	2'b10:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b100;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b001;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end
	
	for(bot_e_id = 1; bot_e_id < BOT_BUF_SIZE; bot_e_id = bot_e_id + 1) begin : bot_op_rmid
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && (&bot_e_take[bot_e_id-1:0]) && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	2'b00:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b001;		
			        	2'b01:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b010;		
			        	2'b10:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b100;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 3'b001;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end
	
end else if (pPORT_NUM == 4 && pRMID_W == 2) begin
	for(bot_e_id = 0; bot_e_id < 1; bot_e_id = bot_e_id + 1) begin : bot_op_rmid_0
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	2'b00:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;		
			        	2'b01:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0010;		
			        	2'b10:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0100;		
			        	2'b11:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b1000;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;
			        		
                            `ifdef DEGUG_INFO_ON
                            //synopsys translate_off
                            	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                            	#100;
                            	$stop;
                            //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end
	
	for(bot_e_id = 1; bot_e_id < BOT_BUF_SIZE; bot_e_id = bot_e_id + 1) begin : bot_op_rmid
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && (&bot_e_take[bot_e_id-1:0]) && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	2'b00:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;		
			        	2'b01:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0010;		
			        	2'b10:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0100;		
			        	2'b11:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b1000;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end
	
end else if(pPORT_NUM == 4 && pRMID_W == 3) begin
	for(bot_e_id = 0; bot_e_id < 1; bot_e_id = bot_e_id + 1) begin : bot_op_rmid_0
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	3'b000:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;		
			        	3'b001:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0010;		
			        	3'b010:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0100;		
			        	3'b011:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b1000;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end
	
	for(bot_e_id = 1; bot_e_id < BOT_BUF_SIZE; bot_e_id = bot_e_id + 1) begin : bot_op_rmid
	    //control
	    always@(posedge clk or negedge rstn) begin
	        if(!rstn) begin
	            bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 0;
	        end else begin 
	            if(!bot_e_take[bot_e_id] && (&bot_e_take[bot_e_id-1:0]) && load && BOT_BUF_VALID[bot_e_id]) begin
			        case(top_e_port_rmid)
			        	3'b000:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;		
			        	3'b001:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0010;		
			        	3'b010:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0100;		
			        	3'b011:	bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b1000;		
			        	default:begin 
			        		bot_e_rmid[deRMID_W*bot_e_id+:deRMID_W] <= 4'b0001;
			        		
                            `ifdef DEGUG_INFO_ON
                                //synopsys translate_off
                                	$display("%m: at time %t ERROR: unexpected rmid to deRMID.top_e_port_rmid:%x",$time,top_e_port_rmid);
                                	#100;
                                	$stop;
                                //synopsys translate_on
                            `endif
			        	end
			        endcase
	            end
	        end
	    end     
	end		
end
endgenerate

//========================================
// ports from top buffers to bottom buffers
//========================================
always@(*) begin
        begin
                top_e_port_araddr = 0;
                top_e_port_arid = 0;
                top_e_port_rmid = 0;
        end 
        begin
                if(top_take[0] && load_allowed && load && TOP_BUF_VALID[0]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*0+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*0+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*0+:pRMID_W];                       
                end else if(top_take[1] && (!(|top_take[1-1:0])) && load_allowed && load && TOP_BUF_VALID[1]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*1+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*1+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*1+:pRMID_W];                       
                end else if(top_take[2] && (!(|top_take[2-1:0])) && load_allowed && load && TOP_BUF_VALID[2]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*2+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*2+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*2+:pRMID_W];               
                end else if(top_take[3] && (!(|top_take[3-1:0])) && load_allowed && load && TOP_BUF_VALID[3]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*3+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*3+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*3+:pRMID_W];               
                end else if(top_take[4] && (!(|top_take[4-1:0])) && load_allowed && load && TOP_BUF_VALID[4]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*4+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*4+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*4+:pRMID_W];               
                end else if(top_take[5] && (!(|top_take[5-1:0])) && load_allowed && load && TOP_BUF_VALID[5]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*5+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*5+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*5+:pRMID_W];                       
                end else if(top_take[6] && (!(|top_take[6-1:0])) && load_allowed && load && TOP_BUF_VALID[6]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*6+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*6+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*6+:pRMID_W];               
                end else if(top_take[7] && (!(|top_take[7-1:0])) && load_allowed && load && TOP_BUF_VALID[7]) begin
                        top_e_port_araddr = top_e_araddr[pDATA_W*7+:pDATA_W];
                        top_e_port_arid = top_e_arid[pARID_W*7+:pARID_W];               
                        top_e_port_rmid = top_e_rmid[pRMID_W*7+:pRMID_W];               
                //end else if(top_take[8] && (!(|top_take[8-1:0])) && load_allowed && load && TOP_BUF_VALID[8]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*8+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*8+:pARID_W];               
                //        top_e_port_rmid = top_e_rmid[pRMID_W*8+:pRMID_W];               
                //end else if(top_take[9] && (!(|top_take[9-1:0])) && load_allowed && load && TOP_BUF_VALID[9]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*9+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*9+:pARID_W];               
                //        top_e_port_rmid = top_e_rmid[pRMID_W*9+:pRMID_W];               
                //end else if(top_take[10] && (!(|top_take[10-1:0])) && load_allowed && load && TOP_BUF_VALID[10]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*10+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*10+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*10+:pRMID_W];                      
                //end else if(top_take[11] && (!(|top_take[11-1:0])) && load_allowed && load && TOP_BUF_VALID[11]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*11+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*11+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*11+:pRMID_W];  
                //end else if(top_take[12] && (!(|top_take[12-1:0])) && load_allowed && load && TOP_BUF_VALID[12]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*12+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*12+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*12+:pRMID_W];
                //end else if(top_take[13] && (!(|top_take[13-1:0])) && load_allowed && load && TOP_BUF_VALID[13]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*13+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*13+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*13+:pRMID_W];
                //end else if(top_take[14] && (!(|top_take[14-1:0])) && load_allowed && load && TOP_BUF_VALID[14]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*14+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*14+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*14+:pRMID_W];
                //end else if(top_take[15] && (!(|top_take[15-1:0])) && load_allowed && load && TOP_BUF_VALID[15]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*15+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*15+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*15+:pRMID_W];
                //end else if(top_take[16] && (!(|top_take[16-1:0])) && load_allowed && load && TOP_BUF_VALID[16]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*16+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*16+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*16+:pRMID_W];
                //end else if(top_take[17] && (!(|top_take[17-1:0])) && load_allowed && load && TOP_BUF_VALID[17]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*17+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*17+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*17+:pRMID_W];
                //end else if(top_take[18] && (!(|top_take[18-1:0])) && load_allowed && load && TOP_BUF_VALID[18]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*18+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*18+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*18+:pRMID_W];
                //end else if(top_take[19] && (!(|top_take[19-1:0])) && load_allowed && load && TOP_BUF_VALID[19]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*19+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*19+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*19+:pRMID_W];
                //end else if(top_take[20] && (!(|top_take[20-1:0])) && load_allowed && load && TOP_BUF_VALID[20]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*20+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*20+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*20+:pRMID_W];
                //end else if(top_take[21] && (!(|top_take[21-1:0])) && load_allowed && load && TOP_BUF_VALID[21]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*21+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*21+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*21+:pRMID_W];
                //end else if(top_take[22] && (!(|top_take[22-1:0])) && load_allowed && load && TOP_BUF_VALID[22]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*22+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*22+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*22+:pRMID_W];
                //end else if(top_take[23] && (!(|top_take[23-1:0])) && load_allowed && load && TOP_BUF_VALID[23]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*23+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*23+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*23+:pRMID_W];
                //end else if(top_take[24] && (!(|top_take[24-1:0])) && load_allowed && load && TOP_BUF_VALID[24]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*24+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*24+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*24+:pRMID_W];
                //end else if(top_take[25] && (!(|top_take[25-1:0])) && load_allowed && load && TOP_BUF_VALID[25]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*25+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*25+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*25+:pRMID_W];
                //end else if(top_take[26] && (!(|top_take[26-1:0])) && load_allowed && load && TOP_BUF_VALID[26]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*26+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*26+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*26+:pRMID_W];
                //end else if(top_take[27] && (!(|top_take[27-1:0])) && load_allowed && load && TOP_BUF_VALID[27]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*27+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*27+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*27+:pRMID_W];
                //end else if(top_take[28] && (!(|top_take[28-1:0])) && load_allowed && load && TOP_BUF_VALID[28]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*28+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*28+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*28+:pRMID_W];
                //end else if(top_take[29] && (!(|top_take[29-1:0])) && load_allowed && load && TOP_BUF_VALID[29]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*29+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*29+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*29+:pRMID_W];
                //end else if(top_take[30] && (!(|top_take[30-1:0])) && load_allowed && load && TOP_BUF_VALID[30]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*30+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*30+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*30+:pRMID_W];
                //end else if(top_take[31] && (!(|top_take[31-1:0])) && load_allowed && load && TOP_BUF_VALID[31]) begin
                //        top_e_port_araddr = top_e_araddr[pDATA_W*31+:pDATA_W];
                //        top_e_port_arid = top_e_arid[pARID_W*31+:pARID_W];              
                //        top_e_port_rmid = top_e_rmid[pRMID_W*31+:pRMID_W];
                end else begin
                        top_e_port_araddr = 0;
                        top_e_port_arid = 0;
                        top_e_port_rmid = 0;            
                end
        end
end     
//========================================


wire[TOP_BUF_SIZE*pPORT_NUM-1:0] top_e_req;
// top buffers filtrating logic
//========================================
generate 
for(bus_id = 0; bus_id < TOP_BUF_SIZE; bus_id = bus_id + 1) begin : TOP_E_REQ_BUS

        assign top_e_bus[bus_id] = (~(|(top_e_req[pPORT_NUM*bus_id+:pPORT_NUM] & (buffer_req & (~buffer_grant))))) & top_e_full[bus_id] & TOP_BUF_VALID[bus_id];
end
endgenerate


assign load = load_allowed && (|top_e_bus); // loading is allowed and filtrating result is good

always@(*) begin
        begin
                top_take = 0;
        end
        begin
                if(top_e_bus[0] && load_allowed && TOP_BUF_VALID[0]) begin
                        top_take[0] = 1'b1;
                end else if(top_e_bus[1] && (!(|top_e_bus[1-1:0])) && load_allowed && TOP_BUF_VALID[1]) begin
                        top_take[1] = 1'b1;
                end else if(top_e_bus[2] && (!(|top_e_bus[2-1:0])) && load_allowed && TOP_BUF_VALID[2]) begin
                        top_take[2] = 1'b1;
                end else if(top_e_bus[3] && (!(|top_e_bus[3-1:0])) && load_allowed && TOP_BUF_VALID[3]) begin
                        top_take[3] = 1'b1;
                end else if(top_e_bus[4] && (!(|top_e_bus[4-1:0])) && load_allowed && TOP_BUF_VALID[4]) begin
                        top_take[4] = 1'b1;
                end else if(top_e_bus[5] && (!(|top_e_bus[5-1:0])) && load_allowed && TOP_BUF_VALID[5]) begin
                        top_take[5] = 1'b1;
                end else if(top_e_bus[6] && (!(|top_e_bus[6-1:0])) && load_allowed && TOP_BUF_VALID[6]) begin
                        top_take[6] = 1'b1;
                end else if(top_e_bus[7] && (!(|top_e_bus[7-1:0])) && load_allowed && TOP_BUF_VALID[7]) begin
                        top_take[7] = 1'b1;
                //end else if(top_e_bus[8] && (!(|top_e_bus[8-1:0])) && load_allowed && TOP_BUF_VALID[8]) begin
                //        top_take[8] = 1'b1;
                //end else if(top_e_bus[9] && (!(|top_e_bus[9-1:0])) && load_allowed && TOP_BUF_VALID[9]) begin
                //        top_take[9] = 1'b1;
                //end else if(top_e_bus[10] && (!(|top_e_bus[10-1:0])) && load_allowed && TOP_BUF_VALID[10]) begin
                //        top_take[10] = 1'b1;
                //end else if(top_e_bus[11] && (!(|top_e_bus[11-1:0])) && load_allowed && TOP_BUF_VALID[11]) begin
                //        top_take[11] = 1'b1;
                //end else if(top_e_bus[12] && (!(|top_e_bus[12-1:0])) && load_allowed && TOP_BUF_VALID[12]) begin
                //        top_take[12] = 1'b1;
                //end else if(top_e_bus[13] && (!(|top_e_bus[13-1:0])) && load_allowed && TOP_BUF_VALID[13]) begin
                //        top_take[13] = 1'b1;
		        //end else if(top_e_bus[14] && (!(|top_e_bus[14-1:0])) && load_allowed && TOP_BUF_VALID[14]) begin
                //        top_take[14] = 1'b1;
	            //end else if(top_e_bus[15] && (!(|top_e_bus[15-1:0])) && load_allowed && TOP_BUF_VALID[15]) begin
                //        top_take[15] = 1'b1;
	            //end else if(top_e_bus[16] && (!(|top_e_bus[16-1:0])) && load_allowed && TOP_BUF_VALID[16]) begin
                //        top_take[16] = 1'b1;
	            //end else if(top_e_bus[17] && (!(|top_e_bus[17-1:0])) && load_allowed && TOP_BUF_VALID[17]) begin
                //        top_take[17] = 1'b1;
	            //end else if(top_e_bus[18] && (!(|top_e_bus[18-1:0])) && load_allowed && TOP_BUF_VALID[18]) begin
                //        top_take[18] = 1'b1;
	            //end else if(top_e_bus[19] && (!(|top_e_bus[19-1:0])) && load_allowed && TOP_BUF_VALID[19]) begin
                //        top_take[19] = 1'b1;
	            //end else if(top_e_bus[20] && (!(|top_e_bus[20-1:0])) && load_allowed && TOP_BUF_VALID[20]) begin
                //        top_take[20] = 1'b1;
	            //end else if(top_e_bus[21] && (!(|top_e_bus[21-1:0])) && load_allowed && TOP_BUF_VALID[21]) begin
                //        top_take[21] = 1'b1;
	            //end else if(top_e_bus[22] && (!(|top_e_bus[22-1:0])) && load_allowed && TOP_BUF_VALID[22]) begin
                //        top_take[22] = 1'b1;
	            //end else if(top_e_bus[23] && (!(|top_e_bus[23-1:0])) && load_allowed && TOP_BUF_VALID[23]) begin
                //        top_take[23] = 1'b1;
	            //end else if(top_e_bus[24] && (!(|top_e_bus[24-1:0])) && load_allowed && TOP_BUF_VALID[24]) begin
                //        top_take[24] = 1'b1;
	            //end else if(top_e_bus[25] && (!(|top_e_bus[25-1:0])) && load_allowed && TOP_BUF_VALID[25]) begin
                //        top_take[25] = 1'b1;
	            //end else if(top_e_bus[26] && (!(|top_e_bus[26-1:0])) && load_allowed && TOP_BUF_VALID[26]) begin
                //        top_take[26] = 1'b1;
	            //end else if(top_e_bus[27] && (!(|top_e_bus[27-1:0])) && load_allowed && TOP_BUF_VALID[27]) begin
                //        top_take[27] = 1'b1;
	            //end else if(top_e_bus[28] && (!(|top_e_bus[28-1:0])) && load_allowed && TOP_BUF_VALID[28]) begin
                //        top_take[28] = 1'b1;
	            //end else if(top_e_bus[29] && (!(|top_e_bus[29-1:0])) && load_allowed && TOP_BUF_VALID[29]) begin
                //        top_take[29] = 1'b1;
	            //end else if(top_e_bus[30] && (!(|top_e_bus[30-1:0])) && load_allowed && TOP_BUF_VALID[30]) begin
                //        top_take[30] = 1'b1;
	            //end else if(top_e_bus[31] && (!(|top_e_bus[31-1:0])) && load_allowed && TOP_BUF_VALID[31]) begin
                //        top_take[31] = 1'b1;
                end else begin
                        top_take = 0;
                end
        end
end
//===============================================
//top and bottom buffers routing converting logic
//===============================================
genvar buffer_id;
genvar buffer_id_in;

generate
        for(buffer_id=0; buffer_id<TOP_BUF_SIZE;buffer_id=buffer_id+1) begin:TOP_E_REQ
                for(buffer_id_in=0; buffer_id_in<pPORT_NUM;buffer_id_in=buffer_id_in+1) begin:TOP_E_REQ_IN
                        assign top_e_req[buffer_id*pPORT_NUM+buffer_id_in] = (top_e_full[buffer_id] & TOP_BUF_VALID[buffer_id]) ? (top_e_rmid[pRMID_W*buffer_id+:pRMID_W]==buffer_id_in) : 0;
                end
        end
endgenerate

generate

for(buffer_id=0; buffer_id<BOT_BUF_SIZE;buffer_id=buffer_id+1) begin:BOT_E_REQ
        assign e_req[buffer_id*pPORT_NUM+:pPORT_NUM] = ({pPORT_NUM{bot_e_full[buffer_id] & BOT_BUF_VALID[buffer_id]}}) & (bot_e_rmid[deRMID_W*buffer_id+:deRMID_W]);
end

endgenerate



generate

for(buffer_id=0; buffer_id<BOT_BUF_SIZE;buffer_id=buffer_id+1) begin:BOT_E_ADDR
        for(buffer_id_in=0; buffer_id_in<pPORT_NUM;buffer_id_in=buffer_id_in+1) begin:BOT_E_ADDR_IN
                assign e_araddr_req[(buffer_id*pPORT_NUM+buffer_id_in)*pDATA_W+:pDATA_W] = {pDATA_W{e_req[buffer_id*pPORT_NUM+buffer_id_in]}}&bot_e_araddr[pDATA_W*buffer_id+:pDATA_W];
        end
end

for(buffer_id=0; buffer_id<BOT_BUF_SIZE;buffer_id=buffer_id+1) begin:BOT_E_ARID
        for(buffer_id_in=0; buffer_id_in<pPORT_NUM;buffer_id_in=buffer_id_in+1) begin:BOT_E_ARID_IN
                assign e_arid_req[(buffer_id*pPORT_NUM+buffer_id_in)*pARID_W+:pARID_W] = {pARID_W{e_req[buffer_id*pPORT_NUM+buffer_id_in]}}&bot_e_arid[pARID_W*buffer_id+:pARID_W];
        end
end

endgenerate
//========================================

assign buffer_req = e_req[pPORT_NUM*0+:pPORT_NUM] | 
					e_req[pPORT_NUM*1+:pPORT_NUM] | 
					e_req[pPORT_NUM*2+:pPORT_NUM] | 
					e_req[pPORT_NUM*3+:pPORT_NUM] | 
					e_req[pPORT_NUM*4+:pPORT_NUM] | 
					e_req[pPORT_NUM*5+:pPORT_NUM] | 
					e_req[pPORT_NUM*6+:pPORT_NUM] | 
					e_req[pPORT_NUM*7+:pPORT_NUM];
                                        //| e_req[pPORT_NUM*8+:pPORT_NUM] | e_req[pPORT_NUM*9+:pPORT_NUM] | e_req[pPORT_NUM*10+:pPORT_NUM] | e_req[pPORT_NUM*11+:pPORT_NUM]
                                        //| e_req[pPORT_NUM*12+:pPORT_NUM] | e_req[pPORT_NUM*13+:pPORT_NUM] | e_req[pPORT_NUM*14+:pPORT_NUM] | e_req[pPORT_NUM*15+:pPORT_NUM]
                                        //| e_req[pPORT_NUM*16+:pPORT_NUM] | e_req[pPORT_NUM*17+:pPORT_NUM] | e_req[pPORT_NUM*18+:pPORT_NUM] | e_req[pPORT_NUM*19+:pPORT_NUM]
                                        //| e_req[pPORT_NUM*20+:pPORT_NUM] | e_req[pPORT_NUM*21+:pPORT_NUM] | e_req[pPORT_NUM*22+:pPORT_NUM] | e_req[pPORT_NUM*23+:pPORT_NUM]
                                        //| e_req[pPORT_NUM*24+:pPORT_NUM] | e_req[pPORT_NUM*25+:pPORT_NUM] | e_req[pPORT_NUM*26+:pPORT_NUM] | e_req[pPORT_NUM*27+:pPORT_NUM]
                                        //| e_req[pPORT_NUM*28+:pPORT_NUM] | e_req[pPORT_NUM*29+:pPORT_NUM] | e_req[pPORT_NUM*30+:pPORT_NUM] | e_req[pPORT_NUM*31+:pPORT_NUM];

assign buffer_araddr = e_araddr_req[pPORT_NUM*pDATA_W*0+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*1+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*2+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*3+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*4+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*5+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*6+:pPORT_NUM*pDATA_W] | 
						e_araddr_req[pPORT_NUM*pDATA_W*7+:pPORT_NUM*pDATA_W];// | 
						//e_araddr_req[pPORT_NUM*pDATA_W*8+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*9+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*10+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*11+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*12+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*13+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*14+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*15+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*16+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*17+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*18+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*19+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*20+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*21+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*22+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*23+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*24+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*25+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*26+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*27+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*28+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*29+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*30+:pPORT_NUM*pDATA_W] | 
						//e_araddr_req[pPORT_NUM*pDATA_W*31+:pPORT_NUM*pDATA_W];
assign buffer_arid = e_arid_req[pPORT_NUM*pARID_W*0+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*1+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*2+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*3+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*4+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*5+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*6+:pPORT_NUM*pARID_W] | 
						e_arid_req[pPORT_NUM*pARID_W*7+:pPORT_NUM*pARID_W];// | 
						//e_arid_req[pPORT_NUM*pARID_W*8+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*9+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*10+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*11+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*12+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*13+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*14+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*15+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*16+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*17+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*18+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*19+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*20+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*21+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*22+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*23+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*24+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*25+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*26+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*27+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*28+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*29+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*30+:pPORT_NUM*pARID_W] | 
						//e_arid_req[pPORT_NUM*pARID_W*31+:pPORT_NUM*pARID_W];
                                                                                                                           
`ifdef DEGUG_INFO_ON                                         
//synopsys translate_off

generate
genvar test_id;

for(test_id = 0; test_id < BOT_BUF_SIZE; test_id = test_id + 1) begin
        always@(posedge clk) begin
                if(e_ren[test_id] && (load && !bot_e_full[test_id]))begin
                        $display("%m: at time %t ERROR: e_ren[%d] entry not valid.",$time,test_id);
                        //#100 $finish;
                end
        end
end

endgenerate

//synopsys translate_on
`endif

endmodule


