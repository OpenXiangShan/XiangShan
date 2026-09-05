# XSPerf-Playground

This is a playground for analyzing performance data collected from simulations of the XiangShan processor. It utilizes ELF files and statistical data to offer insights into the performance characteristics of the code executing on the processor, and displays the number of performance events sampled within basic blocks or functions.

## Get Started

Using VSCode to open `playground.ipynb` and modify the paths to ELF file and stat file to your own files. Then you can run the notebook cells to see the analysis results.

## Example output:

```console
> stat_file.print_top_bbs_by_counter("if_fetch_bubble")

Top basic blocks by counter 'if_fetch_bubble':
8.98%,     6402 - Function: core_state_transition - 0x80000fc8
5.82%,     4149 - Function: __am_uartlite_putchar - 0x80001bae
4.41%,     3139 - Function: _vsnprintf - 0x8000266e
4.36%,     3105 - Function: core_list_mergesort - 0x80000372
4.07%,     2901 - Function: core_state_transition - 0x80000f30
3.94%,     2811 - Function: matrix_mul_matrix - 0x80000992
3.75%,     2675 - Function: matrix_test - 0x80000b4c
3.36%,     2393 - Function: core_state_transition - 0x80000f2c
3.30%,     2354 - Function: core_list_mergesort - 0x800003c8
3.11%,     2215 - Function: core_list_mergesort - 0x800003d6
2.36%,     1679 - Function: core_state_transition - 0x80001062
2.27%,     1617 - Function: core_state_transition - 0x80000e86
2.12%,     1514 - Function: matrix_mul_matrix_bitextract - 0x800009f0
1.99%,     1420 - Function: core_list_mergesort - 0x80000396
1.76%,     1251 - Function: matrix_mul_matrix - 0x800009b4
1.74%,     1239 - Function: core_list_mergesort - 0x80000378
1.70%,     1212 - Function: core_state_transition - 0x80000f66
1.62%,     1157 - Function: matrix_test - 0x80000a78
1.45%,     1030 - Function: calc_func - 0x80000180
1.43%,     1019 - Function: core_list_mergesort - 0x800003a8
1.35%,      963 - Function: matrix_test - 0x80000b3e
1.11%,      789 - Function: core_bench_list - 0x80000486
1.07%,      764 - Function: core_list_mergesort - 0x800003b8
```

## TODO

- Source Code Mapping from basic blocks to source lines and columns
- Group Basic Blocks by program structure
- Weighted analysis based on SimPoint weights
- Per function topdown
