# Frontend Assembly Cases

These `.S` files provide frontend instruction streams for PC/CFI tests.
The bin-trace flow compiles each selected source into a binary, obtains a NEMU
golden trace, and checks DUT instruction delivery and control-flow recovery.

## Suite selection

`scripts/run_baremode_asm_suite.sh` recursively discovers every `.S` file under
`tests/asm_cases/` by default, including local generated files. Inspect the
selection with `--list`, or pass explicit assembly paths to select cases.
Run commands from the repository root with a compatible DUT and the mcpgateway
Python environment.

```bash
src/test/python/Frontend/scripts/run_baremode_asm_suite.sh --list
src/test/python/Frontend/scripts/run_baremode_asm_suite.sh   src/test/python/Frontend/tests/asm_cases/jiabowen/fe_baremode_direct_jmp.S
```

Each selected case must compile to a non-empty binary, generate a non-empty
trace, and complete the DUT checks. Coverage targets come from the canonical
registry and the runner's mapping. Inspect the actual mapping for each case
and keep unmapped observations diagnostic until reviewed.

## Sources and generation

Author directories contain maintained directed programs. Preserve instruction
width, branch layout, cross-block/page positions and training prefixes when
editing a case, and verify those properties in the linked binary.

`generate_cases.py` is a deterministic offline generator. Generated `.S` headers
record seeds, parameters and branch target maps, and `generated/` contains its
output. Record the actual source/bin/trace hashes used for replay.
