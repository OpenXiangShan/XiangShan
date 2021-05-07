import "DPI-C" function void simv_init();
import "DPI-C" function void simv_step();

module tb_top();

reg         clock;
reg         reset;
wire [63:0] io_logCtrl_log_begin;
wire [63:0] io_logCtrl_log_end;
wire [63:0] io_logCtrl_log_level;
wire        io_perfInfo_clean;
wire        io_perfInfo_dump;
wire        io_uart_out_valid;
wire [ 7:0] io_uart_out_ch;
wire        io_uart_in_valid;
wire [ 7:0] io_uart_in_ch;

initial begin
  clock = 0;
  reset = 1;
  #100 reset = 0;
  if ($test$plusargs("dump-wave")) begin
    $vcdplusfile("simv.vpd");
    $vcdpluson;
  end
end
always #1 clock <= ~clock;

SimTop sim(
  .clock(clock),
  .reset(reset),
  .io_logCtrl_log_begin(io_logCtrl_log_begin),
  .io_logCtrl_log_end(io_logCtrl_log_end),
  .io_logCtrl_log_level(io_logCtrl_log_level),
  .io_perfInfo_clean(io_perfInfo_clean),
  .io_perfInfo_dump(io_perfInfo_dump),
  .io_uart_out_valid(io_uart_out_valid),
  .io_uart_out_ch(io_uart_out_ch),
  .io_uart_in_valid(io_uart_in_valid),
  .io_uart_in_ch(io_uart_in_ch)
);

assign io_logCtrl_log_begin = 0;
assign io_logCtrl_log_end = 0;
assign io_logCtrl_log_level = 0;
assign io_perfInfo_clean = 0;
assign io_perfInfo_dump = 0;
assign io_uart_in_ch = 8'hff;

always @(posedge clock) begin
  if (!reset && io_uart_out_valid) begin
    $write("%c", io_uart_out_ch);
  end
end

reg has_init;
always @(posedge clock) begin
  if (reset) begin
    has_init <= 1'b0;
  end
  else if (!has_init) begin
    simv_init();
    has_init <= 1'b1;
  end

  if (!reset && has_init) begin
    simv_step();
  end
end

endmodule

