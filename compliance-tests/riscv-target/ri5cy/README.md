# Running the compliance tests with RI5CY
Build RI5CY's core testbench by navigating to `riscv/tb/core` and calling `make
vsim-all` or if you prefer verilator `make verilate`.

Set `TARGET_SIM` by providing the `vsim` executable and the work directory of
the compiled model of RI5CY e.g.
`export TARGET_SIM=vsim -work RI5CY_REPO/tb/core/work`
or point `TARGET_SIM` to the compiled verilator testbench e.g.
`export TARGET_SIM=RI5CY_REPO/tb/core/testbench_verilator`

Now set the following variables:
```
export RISCV_PREFIX=riscv32-unknown-elf-
export RISCV_TARGET=ri5cy
export RISCV_DEVICE=rv32imc
```

You are now ready to run the tests. The following are supported:
* `make RISCV_ISA=rv32i`
* `make RISCV_ISA=rv32im`
* `make RISCV_ISA=rv32imc`
