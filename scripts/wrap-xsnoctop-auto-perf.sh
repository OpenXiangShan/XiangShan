#!/usr/bin/env bash

set -euo pipefail

rtl_file=${1:?usage: wrap-xsnoctop-auto-perf.sh [PREFIX]XSTop.sv}
rtl_basename=$(basename -- "${rtl_file}")
top_module=${rtl_basename%.*}
rtl_dir=$(dirname -- "${rtl_file}")

if [[ "${top_module}" != *XSTop ]]; then
  echo "wrap-xsnoctop-auto-perf: design top must end in XSTop: ${top_module}" >&2
  exit 1
fi

tmp_file=$(mktemp "${rtl_file}.auto-perf.XXXXXX")
sim_tmp_file=
cleanup() {
  rm -f "${tmp_file}"
  if [[ -n "${sim_tmp_file}" ]]; then
    rm -f "${sim_tmp_file}"
  fi
}
trap cleanup EXIT

awk -v top_module="${top_module}" '
  /^[[:space:]]*localparam[[:space:]].*AUTO_PERF_INSTR/ {
    print "wrap-xsnoctop-auto-perf: RTL is already wrapped" > "/dev/stderr"
    failed = 1
    exit 1
  }

  function emit_ports(  i) {
    if (port_count == 0) {
      print "wrap-xsnoctop-auto-perf: " top_module " port list is empty" > "/dev/stderr"
      failed = 1
      exit 1
    }
    sub(/,[[:space:]]*$/, "", ports[port_count])
    for (i = 1; i <= port_count; i++) {
      print ports[i]
    }
  }

  $0 == "module " top_module "(" && !found_top {
    print
    found_top = 1
    in_port_list = 1
    next
  }

  in_port_list {
    if ($0 == ");") {
      emit_ports()
      print
      in_port_list = 0
      print ""
      print "  // Simulation-only controller for two consecutive 20M-instruction windows."
      print "  wire difftest_perfCtrl_clean;"
      print "  wire difftest_perfCtrl_dump;"
      print "`ifndef SYNTHESIS"
      print "  localparam [1:0] AUTO_PERF_WARMUP = 2\047d0;"
      print "  localparam [1:0] AUTO_PERF_RUN    = 2\047d1;"
      print "  localparam [1:0] AUTO_PERF_FINISH = 2\047d2;"
      print "  localparam [1:0] AUTO_PERF_DONE   = 2\047d3;"
      print "  localparam [63:0] AUTO_PERF_INSTR = 64\047d20000000;"
      print ""
      print "  reg [1:0] auto_perf_phase;"
      print "  wire [63:0] auto_perf_retired_instr = logEndpoint.commitInstrCounter;"
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
    } else if ($0 ~ /difftest_perfCtrl_(clean|dump)/) {
      removed_perf_ports++
    } else {
      ports[++port_count] = $0
    }
    next
  }

  /LogPerfEndpoint[[:space:]]+logEndpoint[[:space:]]*\(/ {
    found_endpoint = 1
  }

  { print }

  END {
    if (failed) {
      exit 1
    }
    if (!found_top) {
      print "wrap-xsnoctop-auto-perf: module " top_module " not found" > "/dev/stderr"
      exit 1
    }
    if (removed_perf_ports != 2) {
      print "wrap-xsnoctop-auto-perf: expected two perf control ports, removed " removed_perf_ports > "/dev/stderr"
      exit 1
    }
    if (!found_endpoint) {
      print "wrap-xsnoctop-auto-perf: LogPerfEndpoint logEndpoint not found in " top_module > "/dev/stderr"
      exit 1
    }
  }
' "${rtl_file}" > "${tmp_file}"

# XSNoCDiffTop elaboration also emits a SimTop for verification. Remove only
# the passthrough ports created by the design top perf-control IOs so the
# generated source set remains self-consistent. SimTop itself is not wrapped.
top_prefix=${top_module%XSTop}
sim_file="${rtl_dir}/${top_prefix}SimTop.${rtl_basename##*.}"
if [[ -f "${sim_file}" ]]; then
  sim_tmp_file=$(mktemp "${sim_file}.auto-perf.XXXXXX")
  awk -v sim_module="${top_prefix}SimTop" -v top_module="${top_module}" '
    function emit_list(items, count,  i) {
      if (count > 0) sub(/,[[:space:]]*$/, "", items[count])
      for (i = 1; i <= count; i++) print items[i]
    }

    $0 == "module " sim_module "(" && !found_sim_top {
      print
      found_sim_top = 1
      in_sim_ports = 1
      next
    }

    in_sim_ports {
      if ($0 == ");") {
        emit_list(sim_ports, sim_port_count)
        print
        in_sim_ports = 0
      } else if ($0 ~ /difftest_perfCtrl_(clean|dump)_0[,[:space:]]*$/) {
        removed_sim_ports++
      } else {
        sim_ports[++sim_port_count] = $0
      }
      next
    }

    $0 ~ "^[[:space:]]*" top_module "[[:space:]]+soc[[:space:]]*\\(" {
      print
      found_soc = 1
      in_soc_ports = 1
      next
    }

    in_soc_ports {
      if ($0 ~ /^[[:space:]]*\);[[:space:]]*$/) {
        emit_list(soc_ports, soc_port_count)
        print
        in_soc_ports = 0
      } else if ($0 ~ /^[[:space:]]*\.difftest_perfCtrl_(clean|dump)[[:space:]]*\(/) {
        removed_soc_connections++
      } else {
        soc_ports[++soc_port_count] = $0
      }
      next
    }

    { print }

    END {
      if (!found_sim_top || !found_soc) {
        print "wrap-xsnoctop-auto-perf: malformed companion " sim_module > "/dev/stderr"
        exit 1
      }
      if (removed_sim_ports != 2 || removed_soc_connections != 2) {
        print "wrap-xsnoctop-auto-perf: expected two SimTop passthrough ports and connections" > "/dev/stderr"
        exit 1
      }
    }
  ' "${sim_file}" > "${sim_tmp_file}"
fi

mv "${tmp_file}" "${rtl_file}"
if [[ -n "${sim_tmp_file}" ]]; then
  mv "${sim_tmp_file}" "${sim_file}"
fi
trap - EXIT
