`ifndef MEMBLOCK_PMA_PMP_PROFILE_V2__SVH
`define MEMBLOCK_PMA_PMP_PROFILE_V2__SVH

// V2 PMA reset profile. The source ordering below is the Scala PMAConfigs
// ordering; load_v2_pma_profile() applies the RTL padding and reverse rule.
function void memblock_pma_pmp_model::load_v2_pma_profile();
    // 中文注释：PMAConfigs 在 RTL pma_init() 中先补齐 32 项再整体 reverse。
    // 本 helper 只登记 Scala 源顺序；set_pma_source_entry() 负责映射最终硬件 index。
    set_pma_source_entry(0, 48'h0000_0000_0000, 49'h1_0000_0000_0000,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_NAPOT,
                         1'b0, 1'b0, 1'b0);
    set_pma_source_entry(1, 48'h0800_0000_0000, '0,
                         1'b1, 1'b1, 1'b0, PMA_PMP_A_TOR,
                         1'b1, 1'b1, 1'b1);
    set_pma_source_entry(2, 48'h0000_8000_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(3, 48'h0000_3a00_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b0, 1'b0);
    set_pma_source_entry(4, 48'h0000_3900_2000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(5, 48'h0000_3900_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(6, 48'h0000_3802_2000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(7, 48'h0000_3802_1000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b1, 1'b1, 1'b1);
    set_pma_source_entry(8, 48'h0000_3802_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(9, 48'h0000_3005_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(10, 48'h0000_3001_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(11, 48'h0000_2000_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b1, 1'b1, 1'b1);
    set_pma_source_entry(12, 48'h0000_1000_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_TOR,
                         1'b0, 1'b1, 1'b1);
    set_pma_source_entry(13, 48'h0000_0000_0000, '0,
                         1'b0, 1'b0, 1'b0, PMA_PMP_A_OFF,
                         1'b0, 1'b0, 1'b0);
endfunction:load_v2_pma_profile

`endif
