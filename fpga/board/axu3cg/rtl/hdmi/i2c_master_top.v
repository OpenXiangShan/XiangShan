module i2c_master_top
(
	input rst,
	input clk,
	
	// I2C signals
	// i2c clock line
	input  scl_pad_i,       // SCL-line input
	output scl_pad_o,       // SCL-line output (always 1'b0)
	output scl_padoen_o,    // SCL-line output enable (active low)

	// i2c data line
	input  sda_pad_i,       // SDA-line input
	output sda_pad_o,       // SDA-line output (always 1'b0)
	output sda_padoen_o,    // SDA-line output enable (active low)
	
	input i2c_read_req,
	output i2c_read_req_ack,
	input i2c_write_req,
	output i2c_write_req_ack,
	input[7:0] i2c_slave_dev_addr,
	input[7:0] i2c_slave_reg_addr,
	input[7:0] i2c_write_data,
	output reg[7:0] i2c_read_data,
	output reg error
);
localparam S_IDLE             =  0;
localparam S_WR_DEV_ADDR      =  1;
localparam S_WR_REG_ADDR      =  2;
localparam S_WR_DATA          =  3;
localparam S_ACK              =  4;
localparam S_WR_ERR_NACK      =  5;
localparam S_RD_DEV_ADDR0     =  6;
localparam S_RD_REG_ADDR      =  7;
localparam S_RD_DEV_ADDR1     =  8;
localparam S_RD_DATA          =  9;
localparam S_RD_STOP          = 10;
localparam S_WR_STOP          = 11;
localparam S_WAIT             = 12;
reg start;
reg stop;
reg read;
reg write;
reg ack_in;
reg[7:0] txr;
wire[7:0] rxr;
wire i2c_busy;
wire i2c_al;
wire done;
wire irxack;
reg[3:0] state,	next_state;
assign i2c_read_req_ack = (state == S_ACK);
assign i2c_write_req_ack = (state == S_ACK);
always@(posedge clk or posedge rst)
begin
	if(rst)
		state <= S_IDLE;
	else
		state <= next_state;	
end
always@(*)
begin
	case(state)
		S_IDLE:
			if(i2c_write_req)
				next_state <= S_WR_DEV_ADDR;
			else if(i2c_read_req)
				next_state <= S_RD_DEV_ADDR0;
			else
				next_state <= S_IDLE;
		S_WR_DEV_ADDR:
			if(done && irxack)
				next_state <= S_WR_ERR_NACK;
			else if(done)
				next_state <= S_WR_REG_ADDR;
			else
				next_state <= S_WR_DEV_ADDR;
		S_WR_REG_ADDR:
			if(done)
				next_state <= S_WR_DATA;
			else
				next_state <= S_WR_REG_ADDR;
		S_WR_DATA:
			if(done)
				next_state <= S_WR_STOP;
			else
				next_state <= S_WR_DATA;
		S_WR_ERR_NACK:
			next_state <= S_WR_STOP;
		S_ACK:
			next_state <= S_WAIT;
		S_WAIT:
			next_state <= S_IDLE;
		S_RD_DEV_ADDR0:
			if(done && irxack)
				next_state <= S_WR_ERR_NACK;
			else if(done)
				next_state <= S_RD_REG_ADDR;
			else
				next_state <= S_RD_DEV_ADDR0;
		S_RD_REG_ADDR:
			if(done)
				next_state <= S_RD_DEV_ADDR1;
			else
				next_state <= S_RD_REG_ADDR;
		S_RD_DEV_ADDR1:
			if(done)
				next_state <= S_RD_DATA;
			else
				next_state <= S_RD_DEV_ADDR1;	
		S_RD_DATA:
			if(done)
				next_state <= S_RD_STOP;
			else
				next_state <= S_RD_DATA;
		S_RD_STOP:
			if(done)
				next_state <= S_ACK;
			else
				next_state <= S_RD_STOP;
		S_WR_STOP:
			if(done)
				next_state <= S_ACK;
			else
				next_state <= S_WR_STOP;				
		default:
			next_state <= S_IDLE;
	endcase
end

always@(posedge clk or posedge rst)
begin
	if(rst)
		error <= 1'b0;
	else if(state == S_IDLE)
		error <= 1'b0;
	else if(state == S_WR_ERR_NACK)
		error <= 1'b1;
end

always@(posedge clk or posedge rst)
begin
	if(rst)
		start <= 1'b0;
	else if(done)
		start <= 1'b0;
	else if(state == S_WR_DEV_ADDR || state == S_RD_DEV_ADDR0 || state == S_RD_DEV_ADDR1)
		start <= 1'b1;
end
always@(posedge clk or posedge rst)
begin
	if(rst)
		stop <= 1'b0;
	else if(done)
		stop <= 1'b0;
	else if(state == S_WR_STOP || state == S_RD_STOP)
		stop <= 1'b1;
end
always@(posedge clk or posedge rst)
begin
	if(rst)
		ack_in <= 1'b0;
	else 
		ack_in <= 1'b1;
end
always@(posedge clk or posedge rst)
begin
	if(rst)
		write <= 1'b0;
	else if(done)
		write <= 1'b0;
	else if(state == S_WR_DEV_ADDR || state == S_WR_REG_ADDR || state == S_WR_DATA || state == S_RD_DEV_ADDR0 || state == S_RD_DEV_ADDR1 || state == S_RD_REG_ADDR)
		write <= 1'b1;
end
always@(posedge clk or posedge rst)
begin
	if(rst)
		read <= 1'b0;
	else if(done)
		read <= 1'b0;
	else if(state == S_RD_DATA)
		read <= 1'b1;
end

always@(posedge clk or posedge rst)
begin
	if(rst)
		i2c_read_data <= 8'h00;
	else if(state == S_RD_DATA && done)
		i2c_read_data <= rxr;
end

always@(posedge clk or posedge rst)
begin
	if(rst)
		txr <= 8'd0;
	else 
		case(state)
			S_WR_DEV_ADDR,S_RD_DEV_ADDR0:
				txr <= {i2c_slave_dev_addr[7:1],1'b0};
			S_RD_DEV_ADDR1:
				txr <= {i2c_slave_dev_addr[7:1],1'b1};
			S_WR_REG_ADDR,S_RD_REG_ADDR:
				txr <= i2c_slave_reg_addr;
			S_WR_DATA:
				txr <= i2c_write_data;
			default:
				txr <= 8'hff;
		endcase
end
i2c_master_byte_ctrl byte_controller (
	.clk      ( clk          ),
	.rst      ( rst          ),
	.nReset   ( 1'b1         ),
	.ena      ( 1'b1         ),
	.clk_cnt  ( 16'd1000     ),
	.start    ( start        ),
	.stop     ( stop         ),
	.read     ( read         ),
	.write    ( write        ),
	.ack_in   ( ack_in       ),
	.din      ( txr          ),
	.cmd_ack  ( done         ),
	.ack_out  ( irxack       ),
	.dout     ( rxr          ),
	.i2c_busy ( i2c_busy     ),
	.i2c_al   ( i2c_al       ),
	.scl_i    ( scl_pad_i    ),
	.scl_o    ( scl_pad_o    ),
	.scl_oen  ( scl_padoen_o ),
	.sda_i    ( sda_pad_i    ),
	.sda_o    ( sda_pad_o    ),
	.sda_oen  ( sda_padoen_o )
);
endmodule 