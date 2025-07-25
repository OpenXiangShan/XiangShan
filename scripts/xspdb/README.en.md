## Introduction to XSPdb

[中文介绍](/README.cn.md)

XSPdb is a specialized Python pdb-based debugging tool for RISC-V IP cores, customized for Xiangshan's difftest interface. It provides GDB-like interactive debugging capabilities, integrating: Terminal command-line interface, RTL-level waveform toggling, Automated script replay, System snapshot save/restore, Register initialization configuration, Instruction set disassembly, etc. Advanced debugging features: conditional breakpoints, real-time watchpoints, register/memory visualization, Hardware signal-level debugging interface synchronized with software execution states. It is a Hardware/software co-verification solution for RISC-V IPs.


<div align="center">
<img src="/.github/screenshot.png" alt="screenshot" width="800" />
<br>
Screenshot of the XSPdb
</div>

### Installation & Dependencies

Clone repository and install dependencies:
```bash
git clone https://github.com/OpenXiangShan/XSPdb.git # Clone repository
pip install -r XSPdb/requirements.txt                # Install dependencies
pip install .                                        # Optional installation
```

If source integration is not required, use alternative pip installation:
```bash
pip3 install XSPdb@git+https://github.com/OpenXiangShan/XSPdb@master
```

### Quick Start

After cloning, run test suite:
```bash
cd XSPdb
make test
```

This command executes:
- Downloads Xiangshan Python binaries
- Fetches test binaries
- Launches interactive debug via example/test.py

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

**Note**: XSPdb prioritizes `spike-dasm` for disassembly, falling back to `capstone`(limited instruction support).

### Manual Testing

Prerequisite: Build Xiangshan Python simulator ([build guide]()).

Alternative: Download prebuilt binaries:
```bash
cd XSPdb
wget https://github.com/OpenXiangShan/XSPdb/releases/download/v0.1.0-test/XSPython.tar.gz
wget https://github.com/OpenXiangShan/XSPdb/releases/download/v0.1.0-test/ready-to-run.tar.gz
tar xf XSPython.tar.gz
tar xf ready-to-run.tar.gz
```

Launch test:
```bash
LD_PRELOAD=XSPython/xspcomm/libxspcomm.so.0.0.1 PYTHONPATH=. python3 example/test.py
```

**Node**: The reason for using `LD_PRELOAD` to load `xspcomm` in advance is to prevent version conflicts between the `system xspcomm` library and `the local xspcomm `packaged in XSPython.

### Command-Line Mode (Non-Interactive):

In addition to the interactive mode, XSPdb also provides a batch mode, allowing you to use it as a full-system simulator. The entry point is `emu.py`, for example:

```bash
./emu.py -i ready-to-run/microbench.bin
```

During the execution of `emu.py`, you can press Ctrl+C to interrupt and enter the XSPdb interactive mode. The command-line arguments are as follows:

```bash
usage: emu.py [-h] [-v] [-C MAX_CYCLES] [-i IMAGE] [-b WAVE_BEGIN] [-e WAVE_END] [-t INTERACT_AT] [-l] [--log-file LOG_FILE]
              [-bi BATCH_INTERVAL] [-s SCRIPT] [-r REPLAY] [--debug-level {debug,info,warn,erro}]
              [--log-level {debug,info,warn,erro}] [-pc PC_COMMITS] [--sim-args SIM_ARGS] [-F FLASH] [--no-interact]
              [--wave-path WAVE_PATH] [--ram-size RAM_SIZE] [--diff DIFF] [--cmds CMDS] [--cmds-post CMDS_POST]
              [--mem-base-address MEM_BASE_ADDRESS] [--flash-base-address FLASH_BASE_ADDRESS]
              [--diff-first-inst_address DIFF_FIRST_INST_ADDRESS] [--trace-pc-symbol-block-change] [--max-run-time MAX_RUN_TIME]

XSPdb Emulation Tool

options:
  -h, --help            show this help message and exit
  -v, --version         show program's version number and exit
  -C MAX_CYCLES, --max-cycles MAX_CYCLES
                        maximum simulation cycles to execute
  -i IMAGE, --image IMAGE
                        image file to load and run
  -b WAVE_BEGIN, --wave-begin WAVE_BEGIN
                        start waveform dump at the specified cycle
  -e WAVE_END, --wave-end WAVE_END
                        stop waveform dump at the specified cycle
  -t INTERACT_AT, --interact-at INTERACT_AT
                        enter interactive mode at the specified cycle
  -l, --log             enable logging output
  --log-file LOG_FILE   log file name (default: ./XSPdb.log)
  -bi BATCH_INTERVAL, --batch-interval BATCH_INTERVAL
                        interval time (seconds) between batch commands
  -s SCRIPT, --script SCRIPT
                        script file to execute
  -r REPLAY, --replay REPLAY
                        replay log file
  --debug-level {debug,info,warn,erro}
                        set debug level
  --log-level {debug,info,warn,erro}
                        set log level
  -pc PC_COMMITS, --pc-commits PC_COMMITS
                        run until the specified number of commits; -1 means no limit
  --sim-args SIM_ARGS   additional simulator arguments (comma-separated)
  -F FLASH, --flash FLASH
                        flash binary file for simulation
  --no-interact         disable interactive mode (do not handle the ctrl-c signal)
  --wave-path WAVE_PATH
                        output path for waveform file
  --ram-size RAM_SIZE   simulation RAM size (e.g., 8GB or 128MB)
  --diff DIFF           path to REF shared object for difftest testing
  --cmds CMDS           XSPdb commands to execute before run (\n for newline)
  --cmds-post CMDS_POST
                        XSPdb commands to execute after script/replay (\n for newline)
  --mem-base-address MEM_BASE_ADDRESS
                        base address of memory
  --flash-base-address FLASH_BASE_ADDRESS
                        base address of flash
  --diff-first-inst_address DIFF_FIRST_INST_ADDRESS
                        first instruction address for difftest
  --trace-pc-symbol-block-change
                        enable tracing of PC symbol block changes
  --max-run-time MAX_RUN_TIME
                        maximum run time (eg 10s, 1m, 1h)
```

### Common Commands：

- `xload` Load a binary file into memory
- `xflash` Load a binary file into Flash
- `xreset_flash` Reset Flash
- `xexport_bin` Export Flash + memory data to a file
- `xexport_flash` Export Flash data to a file
- `xexport_ram` Export memory data to a file
- `xload_script` Load an XSPdb script
- `xmem_write` Write memory data
- `xbytes_to_bin` Convert bytes data to a binary file
- `xnop_insert` Insert NOP instructions in a specified address range
- `xclear_dasm_cache` Clear disassembly cache
- `xprint` Print the value and width of an internal signal
- `xset` Set the value of an internal signal
- `xstep` Step through the circuit
- `xistep` Step through instructions
- `xwatch_commit_pc` Watch commit PC
- `xunwatch_commit_pc` Unwatch commit PC
- `xwatch` Add a watch variable
- `xunwatch` Remove a watch variable
- `xpc` Print the current Commit PCs
- `xexpdiffstate` Set a variable to difftest_stat
- `xexportself` Set a variable to XSPdb self
- `xreset` Reset DUT
- `xlist_xclock_cb` List all xclock callbacks
- `xui` Enter the Text UI interface
- `xdasm` Disassemble memory data
- `xdasmflash` Disassemble Flash data
- `xdasmbytes` Disassemble binary data
- `xdasmnumber` Disassemble a number
- `xbytes2number` Convert bytes to an integer
- `xnumber2bytes` Convert an integer to bytes
- `xparse_instr_file` Parse uint64 strings
- `xload_instr_file` Load uint64 strings into memory
- `xparse_reg_file` Parse a register file
- `xload_reg_file` Load a register file
- `xset_iregs` Set Flash internal registers (Integer)
- `xset_mpc` Set the jump address (by mpc) after Flash initialization, default is 0x80000000
- `xget_mpc` Get the jump address after Flash initialization, default is 0x80000000
- `xset_fregs` Set Flash floating-point registers (general)
- `xset_ireg` Set a single Flash internal register (Integer)
- `xset_freg` Set a Flash floating-point register
- `xlist_flash_iregs` List Flash internal registers
- `xlist_flash_fregs` List Flash floating-point registers
- `xlist_freg_map` List floating-point register mappings

You can use `xcmds` to list all commands, their descriptions, and the modules they belong to (use `xapis` to list all APIs, their descriptions, and their modules).