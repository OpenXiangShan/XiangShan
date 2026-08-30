#!/usr/bin/env bash

set -euo pipefail

rtl_file=${1:?usage: wrap-xsnoctop-auto-perf.sh XSTop.sv}
tmp_file=$(mktemp "${rtl_file}.auto-perf.XXXXXX")
trap 'rm -f "${tmp_file}"' EXIT

awk '
  /^module XSTopInternal\($/ {
    print "wrap-xsnoctop-auto-perf: RTL is already wrapped" > "/dev/stderr"
    exit 1
  }

  function save_wrapper_port(line) {
    if (line !~ /difftest_perfCtrl_(clean|dump)/) {
      wrapper_ports[++wrapper_port_count] = line
    }
  }

  /^module XSTop\($/ && !renamed {
    print "module XSTopInternal("
    renamed = 1
    in_port_list = 1
    next
  }

  in_port_list {
    print
    if ($0 == ");") {
      in_port_list = 0
    } else {
      save_wrapper_port($0)
    }
    next
  }

  {
    # Perf taps use absolute XSTop paths. Account for the wrapper hierarchy.
    gsub(/XSTop\./, "XSTop.u_xstop_internal.")
    print
  }

  END {
    if (!renamed) {
      print "wrap-xsnoctop-auto-perf: module XSTop not found" > "/dev/stderr"
      exit 1
    }
    if (wrapper_port_count == 0) {
      print "wrap-xsnoctop-auto-perf: XSTop port list is empty" > "/dev/stderr"
      exit 1
    }

    print ""
    print "// Simulation-only shell for two consecutive 20M-instruction windows."
    print "module XSTop("
    for (i = 1; i <= wrapper_port_count; i++) {
      port = wrapper_ports[i]
      if (i == wrapper_port_count) {
        sub(/,[[:space:]]*$/, "", port)
      }
      print port
    }
    print ");"
    print "  wire difftest_perfCtrl_clean;"
    print "  wire difftest_perfCtrl_dump;"
    print ""
    print "  XSTopInternal u_xstop_internal (.*);"
    print ""
    print "`ifndef SYNTHESIS"
    print "  localparam [1:0] AUTO_PERF_WARMUP = 2\047d0;"
    print "  localparam [1:0] AUTO_PERF_RUN    = 2\047d1;"
    print "  localparam [1:0] AUTO_PERF_FINISH = 2\047d2;"
    print "  localparam [1:0] AUTO_PERF_DONE   = 2\047d3;"
    print "  localparam [63:0] AUTO_PERF_INSTR = 64\047d20000000;"
    print ""
    print "  reg [1:0] auto_perf_phase;"
    print "  wire [63:0] auto_perf_retired_instr ="
    print "    u_xstop_internal.logEndpoint.commitInstrCounter;"
    print "  wire auto_perf_warmup_done ="
    print "    auto_perf_phase == AUTO_PERF_WARMUP && auto_perf_retired_instr >= AUTO_PERF_INSTR;"
    print "  wire auto_perf_run_done ="
    print "    auto_perf_phase == AUTO_PERF_RUN && auto_perf_retired_instr >= AUTO_PERF_INSTR;"
    print ""
    print "  assign difftest_perfCtrl_clean = auto_perf_warmup_done;"
    print "  assign difftest_perfCtrl_dump = auto_perf_warmup_done || auto_perf_run_done;"
    print ""
    print "  always @(posedge clock or posedge reset) begin"
    print "    if (reset) begin"
    print "      auto_perf_phase <= AUTO_PERF_WARMUP;"
    print "    end else begin"
    print "      case (auto_perf_phase)"
    print "        AUTO_PERF_WARMUP: begin"
    print "          if (auto_perf_warmup_done) begin"
    print "            auto_perf_phase <= AUTO_PERF_RUN;"
    print "          end"
    print "        end"
    print "        AUTO_PERF_RUN: begin"
    print "          if (auto_perf_run_done) begin"
    print "            auto_perf_phase <= AUTO_PERF_FINISH;"
    print "          end"
    print "        end"
    print "        AUTO_PERF_FINISH: begin"
    print "          auto_perf_phase <= AUTO_PERF_DONE;"
    print "          $finish;"
    print "        end"
    print "        default: begin"
    print "          auto_perf_phase <= AUTO_PERF_DONE;"
    print "        end"
    print "      endcase"
    print "    end"
    print "  end"
    print "`else"
    print "  assign difftest_perfCtrl_clean = 1\047b0;"
    print "  assign difftest_perfCtrl_dump = 1\047b0;"
    print "`endif"
    print "endmodule"
  }
' "${rtl_file}" > "${tmp_file}"

mv "${tmp_file}" "${rtl_file}"
trap - EXIT
