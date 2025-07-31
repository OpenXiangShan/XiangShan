## Introduction to XSPdb

XSPdb is a specialized Python pdb-based debugging tool for RISC-V IP cores, customized for Xiangshan's difftest interface. It provides GDB-like interactive debugging capabilities, integrating: Terminal command-line interface, RTL-level waveform toggling, Automated script replay, System snapshot save/restore, Register initialization configuration, Instruction set disassembly, etc. Advanced debugging features: conditional breakpoints, real-time watchpoints, register/memory visualization, Hardware signal-level debugging interface synchronized with software execution states. It is a Hardware/software co-verification solution for RISC-V IPs.


<div align="center">
<img src="/.github/screenshot.png" alt="screenshot" width="800" />
<br>
Screenshot of the XSPdb
</div>

### Quick Start

After cloning, run test suite:
```bash
make dpb
make pdb-run
```

This command executes:
- build Xiangshan Python binaries
- Launches interactive debug via pdb-run.py

Example output and interaction:
```bash
LD_PRELOAD=XSPython/xspcomm/libxspcomm.so.0.0.1 PYTHONPATH=. python3 example/test.py
Using simulated 32768B flash
[Info] reset complete
> XSPdb/example/test.py(13)test_sim_top()
-> while True:
(XiangShan) # Enter interactive mode (use Tab for command list)
(XiangShan)xui                               # Enter TUI mode
(XiangShan)xload ready-to-run/microbench.bin # Load binary (Tab-compatible)
(XiangShan)xistep                            # Step to next instruction commit
(XiangShan)xstep 10000                       # Execute 10000 cycles
```

