// 中文注释：V2 TLXbar_6 外部 out1 路由合同，均为半开字节区间。
// 修改或重生成 RTL 后须运行 check_memblock_v2_routed_ppn_profile.py 核对。
// 49-bit limit 保留 2^48，不能缩为物理地址字段的 48-bit。
localparam bit [48:0] MEMBLOCK_ROUTE_FIRST_0 = 49'h0_0000_0000_0000;
localparam bit [48:0] MEMBLOCK_ROUTE_LIMIT_0 = 49'h0_0000_3110_8000;
localparam bit [48:0] MEMBLOCK_ROUTE_FIRST_1 = 49'h0_0000_3112_0000;
localparam bit [48:0] MEMBLOCK_ROUTE_LIMIT_1 = 49'h0_0000_3112_8000;
localparam bit [48:0] MEMBLOCK_ROUTE_FIRST_2 = 49'h0_0000_3114_0000;
localparam bit [48:0] MEMBLOCK_ROUTE_LIMIT_2 = 49'h0_0000_3802_2000;
localparam bit [48:0] MEMBLOCK_ROUTE_FIRST_3 = 49'h0_0000_3802_2080;
localparam bit [48:0] MEMBLOCK_ROUTE_LIMIT_3 = 49'h1_0000_0000_0000;

// 中文注释：窗口只提升抽样概率；0 仍保留全部外部合法空间。
// DRAM取SoC默认PmemRanges/PMA范围，实际c属性仍由本次PMA快照筛选。
localparam bit [48:0] MEMBLOCK_REGION_DRAM_FIRST = 49'h0_0000_8000_0000;
localparam bit [48:0] MEMBLOCK_REGION_DRAM_LIMIT = 49'h0_0800_0000_0000;
localparam bit [48:0] MEMBLOCK_REGION_APLIC_M_FIRST = 49'h0_0000_3110_0000;
localparam bit [48:0] MEMBLOCK_REGION_APLIC_M_LIMIT = 49'h0_0000_3110_8000;
localparam bit [48:0] MEMBLOCK_REGION_APLIC_S_FIRST = 49'h0_0000_3112_0000;
localparam bit [48:0] MEMBLOCK_REGION_APLIC_S_LIMIT = 49'h0_0000_3112_8000;
localparam bit [48:0] MEMBLOCK_REGION_UART_FIRST = 49'h0_0000_310b_0000;
localparam bit [48:0] MEMBLOCK_REGION_UART_LIMIT = 49'h0_0000_310b_1000;
localparam bit [48:0] MEMBLOCK_REGION_TIMER_FIRST = 49'h0_0000_3800_0000;
localparam bit [48:0] MEMBLOCK_REGION_TIMER_LIMIT = 49'h0_0000_3801_0000;
// PLICConsts.size(maxMaxHarts) = 64MiB，SoC基址0x3c000000。
localparam bit [48:0] MEMBLOCK_REGION_PLIC_FIRST = 49'h0_0000_3c00_0000;
localparam bit [48:0] MEMBLOCK_REGION_PLIC_LIMIT = 49'h0_0000_4000_0000;
