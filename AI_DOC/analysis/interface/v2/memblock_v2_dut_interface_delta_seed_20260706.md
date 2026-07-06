# V2 DUT interface delta seed

## Context

This document records the first V2 DUT interface deltas observed after V2 RTL
generation and the first remote VCS compile smoke.

Command:

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

Result:

The compile reached VCS and parsed the generated V2 RTL from:

```text
$MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f
```

It then stopped at DUT port mismatches between the existing UVM `dut_inst.sv`
and generated V2 `MemBlock.sv`.

## First observed mismatches

The first VCS errors were:

```text
io_ooo_to_mem_backendToTopBypass_cpuWfi
io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable
io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable
io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable
io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable
io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable
io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable
io_ooo_to_mem_enqLsq_needAlloc_6
io_ooo_to_mem_enqLsq_needAlloc_7
io_ooo_to_mem_enqLsq_req_6_valid
```

Generated V2 RTL facts already observed:

```text
build_memblock/rtl/MemBlockTop.sv
  outer_cpu_halt
  l2_tlb_req_*
  l2_pmp_resp_*
  io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable
```

## Follow-up use

This is only a seed list for the later V2 DUT adaptation plan. The next plan
must regenerate the full top port diff from generated V2 Verilog and then
update these areas together:

```text
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
mem_ut/ver/ut/memblock/agent
mem_ut/ver/ut/memblock/subagent
mem_ut/ver/ut/memblock/env
mem_ut/ver/ut/memblock/seq
mem_ut/ver/ut/memblock/cfg
```

Do not fix the mismatch by renaming generated V2 RTL ports back to V3 names.
