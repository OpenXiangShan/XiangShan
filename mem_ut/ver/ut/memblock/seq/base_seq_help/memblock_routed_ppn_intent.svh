`ifndef MEMBLOCK_ROUTED_PPN_INTENT__SVH
`define MEMBLOCK_ROUTED_PPN_INTENT__SVH
// 中文注释：窄窗口main在issue前登记、builder只读的物理页意图。
// first/last_offset为允许访问的完整字节范围；reset_all_tables清除，普通main不创建。
typedef bit [37:0] memblock_routed_ppn_vpn_t;
typedef struct packed {
    bit valid;
    bit [43:0] ppn;
    bit [11:0] first_offset;
    bit [11:0] last_offset;
    int signed region_id;
} memblock_routed_ppn_intent_t;
`endif
