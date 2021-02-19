import "DPI-C" function void uart_putchar
(
  input byte c
);

module UARTHelper(
  input         clock,
  input         reset,
  input         putchar_valid,
  input  [7:0]  putchar_ch,
  input         getchar_valid,
  output [7:0]  getchar_ch
);

always@(posedge clock) begin
  if(putchar_valid)
    uart_putchar(putchar_ch);
end

assign getchar_ch = 'h0;

endmodule
