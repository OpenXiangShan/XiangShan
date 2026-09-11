//*********************************************//
//Project: 	rv_iommu_bosc
//FileName: 	atd_arb_d.v
//Version: 	1.0
//Description:data arbiter
//Author: 	
///*********************************************///

module atd_arb_d
#( 
   parameter pPORT_N=32
  ,parameter pARID_W=5
  ,parameter pDATA_W=512
  ,parameter FIFO_TMO=1
)(

input                          clk,
input                          pin_rstn,

//arbiter input interface
input [pPORT_N*pDATA_W-1:0]    buffer_rdata,
input [pPORT_N-1:0]            buffer_rvalid,
output [pPORT_N-1:0]           buffer_rready,
input [pPORT_N*pARID_W-1:0]    buffer_rid,

//arbiter output interface
input                          port_rready,
output [pDATA_W-1:0]           port_rdata,
output                         port_rvalid,
output [pARID_W-1:0]           port_rid

);

wire rstn;
assign rstn = pin_rstn;
//atd_sync2_rstn u_sync2_rstn(
//.clk    (clk),
//.rst_n  (pin_rstn),
//.so     (rstn)
//);

//wire and reg define
//----------------------------------------------------
reg [pDATA_W-1:0]            arb_rdata;
reg [pARID_W-1:0]            arb_arid  ; 
reg                          arb_ren   ; 
reg [pPORT_N-1:0]            bus_grant,bus_grant_d ;

//arbiter input and output logic
//----------------------------------------------------
wire                         buffer_wen;
wire [pPORT_N*pARID_W-1:0]   bus_arid         = buffer_rid;
wire [pPORT_N*pDATA_W-1:0]   bus_rdata        = buffer_rdata;
wire [pPORT_N-1:0]           req_in           = buffer_rvalid;
//assign                       buffer_rready     = bus_grant_d;
assign                       buffer_rready     = bus_grant&{pPORT_N{buffer_wen}};
wire                         grant_from_input = port_rready;                        
//mask last grant result
wire [pPORT_N-1:0]           req_mask        = ~bus_grant_d;

reg [pPORT_N-1:0]            rr_req;
reg [pPORT_N-1:0]            loop_mask;
wire [pPORT_N-1:0]           loop_mask_pre;
//wire [pPORT_N-1:0]           high_prio_new_req= req_in & req_mask & loop_mask_pre;
//wire [pPORT_N-1:0]           low_prio_new_req = req_in & req_mask & ~loop_mask_pre;
//wire [pPORT_N-1:0]           high_prio_new_req= req_in & req_mask & loop_mask;
//wire [pPORT_N-1:0]           low_prio_new_req = req_in & req_mask & ~loop_mask;
wire [pPORT_N-1:0]           high_prio_new_req= req_in & loop_mask;
wire [pPORT_N-1:0]           low_prio_new_req = req_in & ~loop_mask;
wire                         high_new_req_valid = (|high_prio_new_req);
wire                         low_new_req_valid  = (|low_prio_new_req);

//all not exectued req ,exclude new req
wire                         rr_valid        = (|rr_req);

//3 type of case  request for arbiter:
//case1:rr_req ,no new request,they are buffered request before 
//case2:high_new_req_valid,high bit requests will be first granted when sampling new request;
//case3:low_new_req_valid,if high bit no request,than low bit request will be granted when sample new request;
//2 and 3 is different from case1,they are both new request

//for example
//new request is 8'b1111_0011,loop mask is 8'h1110_0000;
//case2 will be first executed : request bit5 will be first granted; 
//case1 will be second executed: request bit6~bit7

wire [pPORT_N-1:0]           bus_req         = ({pPORT_N{rr_valid}}& rr_req)|
                                               ({pPORT_N{~rr_valid & high_new_req_valid}}& high_prio_new_req)|
                                               ({pPORT_N{~rr_valid & ~high_new_req_valid & low_new_req_valid}}& low_prio_new_req);


wire                         req_valid       = (|bus_req);


//update RR arbitration priority logic 
//----------------------------------------------
//update condition:  
//new req(req_in & req_mask) number is more than one
//excepet for low_prio_req_load_en_c1  
//----------------------------------------------
//notes: rr_valid low is the new request sample window 
//once rr_valid jump to high from low ,indicate new request have multi requests;
//once rr_valid jump to low from high ,indcicate this loop arbitration will
//end,and new request sample window will comming.
reg                          rr_valid_d;
always @(posedge clk or negedge rstn) begin
    if (!rstn)
        rr_valid_d   <= 'b0;
    else
        rr_valid_d   <= rr_valid;
end

//rr_valid falling edge check
//indicate this loop arbitration done,that is,
//all new request at sample window have been served
wire all_req_done = ~rr_valid & rr_valid_d;

//for example:
//orignal   loop_mask=8'b1111_1111;
//loop_done loop_mask=8'b1000_0000;
wire loop_done = ~loop_mask[pPORT_N-2] & loop_mask[pPORT_N-1];

assign loop_mask_pre = (all_req_done)?(loop_done?{pPORT_N{1'b1}}:loop_mask<<1):loop_mask;

//update arbitration priority once one loop arbitration done
always @(posedge clk or negedge rstn) begin
    if (!rstn)
        loop_mask <= {pPORT_N{1'b1}};
    else
        loop_mask <= loop_mask_pre;
end

//----------------------------------------------
//the folllowing two case is only for both loop_mask high bit and low bit have valid reqeust
//when sampling new requests,but rr_valid will not changed at case 1,due to timing sequece

//case1 is corner case,it is total two req,one belong to loop_mask high bit range,one belong to
//low bit range :
//for example
//new req   :4'b1001
//loop mask :4'b1100  
//valid high req = 4'b1000;
//valid low req  = 4'b0001
wire both_new_req  = ~rr_valid & high_new_req_valid & low_new_req_valid & buffer_wen;

wire high_req_done = ~(|(bus_req&(~bus_grant))) & buffer_wen;

//this signal indicate high bit request has done,and low bit request wil be load to rr_req
wire low_prio_req_load_en_c1 =  both_new_req & high_req_done;

//case2
//case2 have more request belong to loop_mask high bit range than two, more request belong to
//low bit range than one
//for example
//new req   :4'b1101
//loop mask :4'b1100  
//valid high req = 4'b1100;
//valid low req  = 4'b0001
//this case is very common at our system;

reg low_prio_req_case2;
//reg [pPORT_N-1:0] low_prio_new_req_r;

//this signal indicate high bit request has done,and low bit request wil be load to rr_req
wire low_prio_req_load_en_c2 = low_prio_req_case2 & high_req_done;

always @(posedge clk or negedge rstn) begin
    if (!rstn) begin
        low_prio_req_case2 <= 'b0;
//        low_prio_new_req_r <= 'b0;
    end else if (low_prio_req_load_en_c1 | low_prio_req_load_en_c2) begin
        low_prio_req_case2 <= 1'b0;
//        low_prio_new_req_r <= 'b0;
    end else if ( both_new_req ) begin
        low_prio_req_case2 <= 1'b1;
//        low_prio_new_req_r <= low_prio_new_req;
    end
end


//push arb result into fifo
//-----------------------------------------------------------
localparam pBUF_DW = pARID_W+pDATA_W;
wire                          buffer_empty,buffer_full;
wire [pBUF_DW-1:0]            buffer_dout;
wire [pBUF_DW-1:0]            buffer_din     = {arb_arid,arb_rdata}; 
assign                        buffer_wen     = ~buffer_full & req_valid;
wire                          buffer_ren     = ~buffer_empty & grant_from_input;

//pop from fifo
assign                        port_rvalid    = ~buffer_empty;
assign                       {port_rid,port_rdata} = buffer_dout;


//arbiter main logic
//-----------------------------------------------------------
generate		
    if ( pPORT_N==3 ) begin : ARB_3TO1     
        always @(*) begin
            begin
                arb_rdata  = 'b0;
                arb_arid   = 'b0;
				bus_grant  = 3'b000;
            end
            begin

				if (bus_req[0])
					arb_rdata = bus_rdata[1*pDATA_W-1:0*pDATA_W];
				else if (bus_req[1]) 
					arb_rdata = bus_rdata[2*pDATA_W-1:1*pDATA_W];
				else if (bus_req[2]) 
					arb_rdata = bus_rdata[3*pDATA_W-1:2*pDATA_W];
				else
					arb_rdata = 'b0;
				
				if (bus_req[0])
					arb_arid = bus_arid[1*pARID_W-1:0*pARID_W];
				else if (bus_req[1]) 
					arb_arid = bus_arid[2*pARID_W-1:1*pARID_W];
				else if (bus_req[2]) 
					arb_arid = bus_arid[3*pARID_W-1:2*pARID_W];
				else
					arb_arid = 'b0;

				if (bus_req[0])
					bus_grant =  3'b001;
				else if (bus_req[1])
					bus_grant =  3'b010;
				else if (bus_req[2])
					bus_grant =  3'b100;
				else
					bus_grant = 3'b000;
				
            end                                 
        end    
	end		
	else if ( pPORT_N==4 ) begin : ARB_4TO1     
        always @(*) begin
            begin
                arb_rdata  = 'b0;
                arb_arid   = 'b0;
				bus_grant  = 4'b0000;
            end
            begin

				if (bus_req[0])
					arb_rdata = bus_rdata[1*pDATA_W-1:0*pDATA_W];
				else if (bus_req[1]) 
					arb_rdata = bus_rdata[2*pDATA_W-1:1*pDATA_W];
				else if (bus_req[2]) 
					arb_rdata = bus_rdata[3*pDATA_W-1:2*pDATA_W];
				else if (bus_req[3]) 
					arb_rdata = bus_rdata[4*pDATA_W-1:3*pDATA_W];
				else
					arb_rdata = 'b0;
				
				if (bus_req[0])
					arb_arid = bus_arid[1*pARID_W-1:0*pARID_W];
				else if (bus_req[1]) 
					arb_arid = bus_arid[2*pARID_W-1:1*pARID_W];
				else if (bus_req[2]) 
					arb_arid = bus_arid[3*pARID_W-1:2*pARID_W];
				else if (bus_req[3]) 
					arb_arid = bus_arid[4*pARID_W-1:3*pARID_W];
				else
					arb_arid = 'b0;

				if (bus_req[0])
					bus_grant =  4'b0001;
				else if (bus_req[1])
					bus_grant =  4'b0010;
				else if (bus_req[2])
					bus_grant =  4'b0100;
				else if (bus_req[2])
					bus_grant =  4'b1000;
				else
					bus_grant = 4'b0000;
				
            end                                 
        end    
	end		
		
   
endgenerate

//--------------------------------      
always @(posedge clk or negedge rstn) begin
    if (!rstn)
        bus_grant_d   <= 'b0;
    else if (buffer_wen) 
        bus_grant_d   <= bus_grant;
    else
        bus_grant_d   <= 'b0;
end

//wil be executed request next cycle
always @(posedge clk or negedge rstn) begin
    if (!rstn)
        rr_req   <= 'b0;
    else if (buffer_wen)
        rr_req   <= (bus_req&(~bus_grant)) |
                        ({pPORT_N{low_prio_req_load_en_c1}}&low_prio_new_req) |
                        ({pPORT_N{low_prio_req_load_en_c2}}&low_prio_new_req);
                        //({pPORT_N{low_prio_req_load_en_c2}}&low_prio_new_req_r);
end

//--------------------------------
// arb to fifo
//--------------------------------
// parameter USE_RS:
// 0 - USE FIFO
// 1 - USE Regsiter Slice
 
// When USE_RS=1
// parameter TMO:
// 0 - pass through mode
// 1 - forward timing mode
// 2 - full timing mode
// 3 - backward timing mode
 atd_sync_fifo_wrap
       #(
         .USE_RS    (   1   )
        ,.FIFO_TMO  (  FIFO_TMO   )
        ,.FIFO_WID  (pBUF_DW)
        ,.FIFO_DEPTH_WID(1)       //depth 2
       )
 u_fifo_wrap     (
          .clk      (clk),
          .rstn     (rstn),
          .wen      (buffer_wen ),
          .ren      (buffer_ren ),
          .din      (buffer_din ),
          .dout     (buffer_dout),
          .full     (buffer_full),
          .empty    (buffer_empty)
  ); 
`ifdef DEGUG_INFO_ON
//---------------------------------------------------------  
//Verilog Assertion
//---------------------------------------------------------  
//synopsys translate_off
localparam pER_BW = 5;
reg [pER_BW-1:0 ] assert_err;
wire low_prio_req = low_prio_req_load_en_c1|low_prio_req_load_en_c2;
//wire [pPORT_N-1:0] tmp1=(low_prio_new_req|low_prio_new_req_r);
wire [pPORT_N-1:0] tmp1=(low_prio_new_req);
wire tmp2= |({pPORT_N{low_prio_req}}&tmp1);
wire tmp3=|(bus_req&(~bus_grant));
always @ (posedge clk or negedge rstn) begin
   if(!rstn) begin
      assert_err <= 'b0;
   end else begin
      if( buffer_wen & tmp3 & tmp2 )  begin
          assert_err[0] <= 'b1;
          $display("%m: at time %t ERROR: high and low could not been request at the same time.",$time);
      end


   end
end

always @ (posedge clk or negedge rstn) begin
   if(|assert_err) begin
     $display("%m: at time %t ERROR: assertion error ....",$time);
     #100;
     $finish;
   end
end

//synopsys translate_on
`endif
endmodule
