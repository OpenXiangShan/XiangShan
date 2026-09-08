// The runner inserts ports parsed from the existing generated module.
// Queue pointers and entries are never forced. All stimulus is on module inputs.
module icache_waylookup_capacity_tb;
  reg clock = 0;
  always #5 clock = ~clock;
  reg reset = 1;
  reg io_flush = 0;
  reg io_flushFromBpu_s3_valid = 0;
  reg io_flushFromBpu_s3_bits_flag = 0;
  reg [5:0] io_flushFromBpu_s3_bits_value = 0;
  reg io_toMainPipe_ready = 0;
  reg io_fromMainPipe_realTwoFetchValid = 0;
  reg io_write_0_valid = 0;
  reg io_write_1_valid = 0;
  reg [5:0] io_write_0_bits_ftqIdx_value = 0;
  reg [5:0] io_write_1_bits_ftqIdx_value = 1;
  wire io_write_0_ready;
  wire io_toMainPipe_valid;
  /* DUT_CONNECTIONS */

  task automatic tick;
    @(posedge clock); #1;
    @(negedge clock); #1;
  endtask
  task automatic check_count(input integer expected);
    if (dut.numValidEntries !== expected)
      $fatal(1, "occupancy expected=%0d observed=%0d", expected, dut.numValidEntries);
  endtask
  task automatic push(input bit dual_write);
    io_write_0_valid = 1;
    io_write_1_valid = dual_write;
    #1;
    if (!io_write_0_ready) $fatal(1, "push unexpectedly blocked");
    tick();
    io_write_0_valid = 0;
    io_write_1_valid = 0;
    io_write_0_bits_ftqIdx_value += dual_write ? 2 : 1;
    io_write_1_bits_ftqIdx_value = io_write_0_bits_ftqIdx_value + 1;
  endtask
  task automatic pop(input bit dual_read);
    io_toMainPipe_ready = 1;
    io_fromMainPipe_realTwoFetchValid = dual_read;
    #1;
    if (!io_toMainPipe_valid) $fatal(1, "pop unexpectedly blocked");
    tick();
    io_toMainPipe_ready = 0;
    io_fromMainPipe_realTwoFetchValid = 0;
  endtask
  initial begin
    repeat (3) tick();
    reset = 0;
    tick();
    check_count(0);
    repeat (15) push(1);
    check_count(30);
    push(0);
    check_count(31);
    io_write_0_valid = 1;
    io_write_1_valid = 1;
    #1;
    if (io_write_0_ready) $fatal(1, "31 entries must block dual write");
    tick(); check_count(31);
    io_write_1_valid = 0;
    #1;
    if (io_write_0_ready) $fatal(1, "V3 reserves two slots even for single write");
    tick(); check_count(31);
    io_write_0_valid = 0;
    $display("PASS one_slot_blocks_single_and_dual");
    pop(0);
    check_count(30);
    push(1);
    check_count(32);
    io_write_0_valid = 1;
    #1;
    if (io_write_0_ready) $fatal(1, "full queue accepted write");
    tick(); check_count(32);
    io_write_0_valid = 0;
    $display("PASS full_blocks_write_and_dual_write_wrap");
    pop(1); check_count(30);
    io_toMainPipe_ready = 1;
    push(1); // simultaneous single read and dual write at occupancy 30
    io_toMainPipe_ready = 0;
    check_count(31);
    $display("PASS read_write_boundary");
    repeat (31) pop(0);
    check_count(0);
    $display("PASS single_read_wrap");
    // Current write pointer is 3; 29 single pushes cross 31 -> 0.
    repeat (29) push(0);
    check_count(29);
    if (dut.writePtr_value !== 0) $fatal(1, "single-write wrap was not reached");
    $display("PASS single_write_wrap");
    io_flush = 1; tick(); io_flush = 0;
    check_count(0);
    push(1); check_count(2);
    // Actual module BPU input targets the second entry of the dual write.
    io_flushFromBpu_s3_bits_value = io_write_0_bits_ftqIdx_value - 1;
    io_flushFromBpu_s3_valid = 1;
    tick(); io_flushFromBpu_s3_valid = 0;
    check_count(1);
    $display("PASS bpu_rewinds_dual_write_tail");
    $finish;
  end
  initial begin
    #20000;
    $fatal(1, "WayLookup module test timed out");
  end
endmodule
