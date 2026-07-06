//=========================================================
//File name    : tlb_map_builder.sv
//Author       : OpenAI_Codex
//Module name  : tlb_map_builder
//Discribution : by-key TLB entry builder for dispatch framework
//Date         : 2026-05-18
//=========================================================
`ifndef TLB_MAP_BUILDER__SV
`define TLB_MAP_BUILDER__SV

class tlb_map_builder extends uvm_object;

    `uvm_object_utils(tlb_map_builder)

    function new(string name = "tlb_map_builder");
        super.new(name);
    endfunction:new

    function memblock_tlb_entry build_tlb_entry_for_req(input bit [37:0] vpn,
                                                        input bit [1:0] s2xlate,
                                                        input mmu_csr_runtime_state csr_state);
        memblock_tlb_entry entry;
        bit [63:0]         vaddr;

        if (csr_state == null) begin
            `uvm_fatal("TLB_BUILDER", "build_tlb_entry_for_req got null csr_state")
        end
        entry = memblock_tlb_entry::type_id::create($sformatf("tlb_entry_%0h_%0d", vpn, s2xlate));
        entry.reset();
        vaddr = {14'b0, vpn, 12'b0};
        entry.update_addr_fields(vaddr, choose_paddr(vaddr));
        randomize_pte_bits(entry);
        entry.fixup_pte_legal(memblock_tlb_entry::MEMBLOCK_TLB_ACCESS_UNKNOWN);
        entry.apply_csr_state(csr_state, s2xlate);
        entry.create_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        entry.last_hit_cycle = entry.create_cycle;
        return entry;
    endfunction:build_tlb_entry_for_req

    function bit [63:0] choose_paddr(input bit [63:0] vaddr);
        bit [63:0] base;
        bit [63:0] range;
        bit [63:0] page_count;
        bit [63:0] vpn_mix;
        bit [63:0] page_offset;

        base       = seq_csr_common::get_paddr_base();
        range      = seq_csr_common::get_paddr_range();
        page_count = range >> 12;
        if (page_count == 0) begin
            page_count = 1;
        end
        vpn_mix     = (vaddr >> 12) % page_count;
        page_offset = vaddr[11:0];
        return (base & 64'hffff_ffff_ffff_f000) + (vpn_mix << 12) + page_offset;
    endfunction:choose_paddr

    function void randomize_pte_bits(input memblock_tlb_entry entry);
        if (entry == null) begin
            `uvm_fatal("TLB_BUILDER", "randomize_pte_bits got null entry")
        end
        entry.pte_r = choose_weighted_bit(seq_csr_common::get_tlb_pte_r_1_wt(),
                                          seq_csr_common::get_tlb_pte_r_0_wt());
        entry.pte_w = choose_weighted_bit(seq_csr_common::get_tlb_pte_w_1_wt(),
                                          seq_csr_common::get_tlb_pte_w_0_wt());
        entry.pte_x = choose_weighted_bit(seq_csr_common::get_tlb_pte_x_1_wt(),
                                          seq_csr_common::get_tlb_pte_x_0_wt());
        entry.pte_u = choose_weighted_bit(seq_csr_common::get_tlb_pte_u_1_wt(),
                                          seq_csr_common::get_tlb_pte_u_0_wt());
        entry.pte_g = choose_weighted_bit(seq_csr_common::get_tlb_pte_g_1_wt(),
                                          seq_csr_common::get_tlb_pte_g_0_wt());
        entry.pte_a = choose_weighted_bit(seq_csr_common::get_tlb_pte_a_1_wt(),
                                          seq_csr_common::get_tlb_pte_a_0_wt());
        entry.pte_d = choose_weighted_bit(seq_csr_common::get_tlb_pte_d_1_wt(),
                                          seq_csr_common::get_tlb_pte_d_0_wt());
        entry.pte_n = choose_weighted_bit(seq_csr_common::get_tlb_pte_n_1_wt(),
                                          seq_csr_common::get_tlb_pte_n_0_wt());
        entry.pte_v = choose_weighted_bit(seq_csr_common::get_tlb_pte_v_1_wt(),
                                          seq_csr_common::get_tlb_pte_v_0_wt());
    endfunction:randomize_pte_bits

    function bit choose_weighted_bit(input int unsigned one_wt,
                                     input int unsigned zero_wt);
        int unsigned total;
        int unsigned pick;

        total = one_wt + zero_wt;
        if (total == 0) begin
            `uvm_fatal("TLB_BUILDER", "choose_weighted_bit got all-zero weights")
        end
        pick = $urandom_range(total - 1, 0);
        return pick < one_wt;
    endfunction:choose_weighted_bit

endclass:tlb_map_builder

`endif
