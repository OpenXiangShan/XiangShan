//=========================================================
//File name    : memblock_tlb_entry.sv
//Author       : OpenAI_Codex
//Module name  : memblock_tlb_entry
//Discribution : by-key TLB/PTE entry and per-uid record
//Date         : 2026-06-01
//=========================================================
`ifndef MEMBLOCK_TLB_ENTRY__SV
`define MEMBLOCK_TLB_ENTRY__SV

class memblock_tlb_entry extends uvm_object;

    typedef enum int unsigned {
        MEMBLOCK_TLB_LEVEL_FIXED      = 0,
        MEMBLOCK_TLB_LEVEL_RANDOM     = 1,
        MEMBLOCK_TLB_LEVEL_DERIVED    = 2
    } memblock_tlb_level_mode_e;

    typedef enum int unsigned {
        MEMBLOCK_TLB_PTE_MODE_LEGAL            = 0,
        MEMBLOCK_TLB_PTE_MODE_MIXED            = 1,
        MEMBLOCK_TLB_PTE_MODE_EXCEPTION_BIASED = 2
    } memblock_tlb_pte_mode_e;

    typedef enum int unsigned {
        MEMBLOCK_TLB_ACCESS_UNKNOWN = 0,
        MEMBLOCK_TLB_ACCESS_LOAD    = 1,
        MEMBLOCK_TLB_ACCESS_STORE   = 2
    } memblock_tlb_access_e;

    memblock_tlb_lookup_key_t lookup_key;

    bit [63:0] vaddr;
    bit [63:0] paddr;
    bit [63:0] vpn;
    bit [63:0] ppn;

    bit pte_r;
    bit pte_w;
    bit pte_x;
    bit pte_u;
    bit pte_g;
    bit pte_a;
    bit pte_d;
    bit pte_n;
    bit pte_v;
    bit [1:0] pbmt;

    bit tlbAF;
    bit tlbPF;
    bit tlbGPF;
    bit pmaAF;

    int unsigned asid;
    int unsigned vmid;
    bit [1:0]    s2xlate;
    bit [1:0]    priv_mode;
    bit [1:0]    level;

    bit [2:0]    addr_low;
    bit [2:0]    ppn_low[8];
    bit          valididx[8];
    bit [2:0]    pteidx[8];

    longint unsigned create_cycle;
    longint unsigned last_hit_cycle;

    `uvm_object_utils(memblock_tlb_entry)

    function new(string name = "memblock_tlb_entry");
        super.new(name);
        reset();
    endfunction:new

    function void reset();
        lookup_key     = '{default:'0};
        vaddr          = '0;
        paddr          = '0;
        vpn            = '0;
        ppn            = '0;
        pte_r          = 1'b0;
        pte_w          = 1'b0;
        pte_x          = 1'b0;
        pte_u          = 1'b0;
        pte_g          = 1'b0;
        pte_a          = 1'b0;
        pte_d          = 1'b0;
        pte_n          = 1'b0;
        pte_v          = 1'b0;
        pbmt           = '0;
        tlbAF          = 1'b0;
        tlbPF          = 1'b0;
        tlbGPF         = 1'b0;
        pmaAF          = 1'b0;
        asid           = 0;
        vmid           = 0;
        s2xlate        = 2'd0;
        priv_mode      = '0;
        level          = '0;
        addr_low       = '0;
        create_cycle   = 0;
        last_hit_cycle = 0;
        foreach (ppn_low[idx]) begin
            ppn_low[idx]  = '0;
            valididx[idx] = 1'b0;
            pteidx[idx]   = '0;
        end
    endfunction:reset

    function void update_addr_fields(input bit [63:0] vaddr_i, input bit [63:0] paddr_i);
        vaddr = vaddr_i;
        paddr = paddr_i;
        vpn   = vaddr_i[63:12];
        ppn   = paddr_i[63:12];
        addr_low = vaddr_i[14:12];
        derive_index_fields();
    endfunction:update_addr_fields

    function void apply_csr_state(input mmu_csr_runtime_state csr_state,
                                  input bit [1:0] s2xlate_i);
        if (csr_state == null) begin
            `uvm_fatal("TLB_ENTRY", "apply_csr_state got null csr_state")
        end
        s2xlate    = s2xlate_i;
        lookup_key = csr_state.make_lookup_key(vpn, s2xlate_i);
        asid       = lookup_key.asid;
        vmid       = lookup_key.vmid;
        priv_mode  = csr_state.current_priv_mode(1'b1);
    endfunction:apply_csr_state

    function void derive_index_fields();
        for (int unsigned idx = 0; idx < 8; idx++) begin
            valididx[idx] = 1'b0;
            pteidx[idx]   = idx[2:0];
            ppn_low[idx]  = ppn[2:0] + idx[2:0];
        end
        valididx[addr_low] = 1'b1;
        level = choose_level();
    endfunction:derive_index_fields

    function bit [1:0] choose_level();
        int unsigned mode;
        int unsigned low;
        int unsigned high;
        int unsigned pick;
        int unsigned fixed_level;

        mode = seq_csr_common::get_tlb_level_mode();
        case (mode)
            MEMBLOCK_TLB_LEVEL_FIXED: begin
                fixed_level = seq_csr_common::get_tlb_level_fixed_value();
                return fixed_level[1:0];
            end
            MEMBLOCK_TLB_LEVEL_RANDOM: begin
                low  = seq_csr_common::get_tlb_level_random_low();
                high = seq_csr_common::get_tlb_level_random_high();
                pick = $urandom_range(high, low);
                return pick[1:0];
            end
            MEMBLOCK_TLB_LEVEL_DERIVED: begin
                // Keep current behavior simple: default derive path still maps to base page level.
                return addr_low[2] ? 2'd1 : 2'd0;
            end
            default: begin
                return 2'd0;
            end
        endcase
    endfunction:choose_level

    function bit choose_weighted_bit(input int unsigned one_wt,
                                     input int unsigned zero_wt);
        int unsigned total;
        int unsigned pick;

        total = one_wt + zero_wt;
        if (total == 0) begin
            `uvm_fatal("TLB_ENTRY", "choose_weighted_bit got all-zero weights")
        end
        pick = $urandom_range(total - 1, 0);
        return pick < one_wt;
    endfunction:choose_weighted_bit

    function void apply_pte_profile(input memblock_tlb_access_e access_kind = MEMBLOCK_TLB_ACCESS_UNKNOWN);
        case (seq_csr_common::get_tlb_pte_mode())
            MEMBLOCK_TLB_PTE_MODE_LEGAL: begin
                // Legal mode keeps the random weights as-is, then relies on fixup for consistency.
            end
            MEMBLOCK_TLB_PTE_MODE_MIXED: begin
                // Mixed mode keeps some suspicious combinations alive for coverage, while still
                // allowing later fixup to enforce minimal consistency only.
                if (!pte_v) begin
                    pte_a = 1'b0;
                    pte_d = 1'b0;
                end
            end
            MEMBLOCK_TLB_PTE_MODE_EXCEPTION_BIASED: begin
                if (access_kind == MEMBLOCK_TLB_ACCESS_STORE) begin
                    pte_a = choose_weighted_bit(1, 3);
                    pte_d = choose_weighted_bit(1, 4);
                end else begin
                    pte_a = choose_weighted_bit(1, 4);
                    pte_d = 1'b0;
                end
                if (!pte_v) begin
                    pte_a = 1'b0;
                    pte_d = 1'b0;
                end
            end
            default: begin
            end
        endcase
    endfunction:apply_pte_profile

    function void derive_ad_bits(input memblock_tlb_access_e access_kind = MEMBLOCK_TLB_ACCESS_UNKNOWN);
        if (!pte_v) begin
            pte_a = 1'b0;
            pte_d = 1'b0;
            return;
        end
        if (access_kind == MEMBLOCK_TLB_ACCESS_UNKNOWN) begin
            pte_a = 1'b1;
            pte_d = 1'b1;
            return;
        end
        if (!pte_a) begin
            pte_d = 1'b0;
            return;
        end
        case (access_kind)
            MEMBLOCK_TLB_ACCESS_STORE: begin
                pte_d = pte_d && pte_w;
            end
            MEMBLOCK_TLB_ACCESS_LOAD: begin
                pte_d = 1'b0;
            end
            default: begin
                pte_d = pte_d && pte_w;
            end
        endcase
    endfunction:derive_ad_bits

    function void fixup_pte_legal(input memblock_tlb_access_e access_kind = MEMBLOCK_TLB_ACCESS_UNKNOWN);
        int unsigned pte_mode;

        pte_mode = seq_csr_common::get_tlb_pte_mode();
        apply_pte_profile(access_kind);
        if (!pte_v) begin
            if (pte_mode == MEMBLOCK_TLB_PTE_MODE_LEGAL) begin
                pte_r = 1'b0;
                pte_w = 1'b0;
                pte_x = 1'b0;
            end
            derive_ad_bits(access_kind);
            return;
        end
        if (pte_mode == MEMBLOCK_TLB_PTE_MODE_LEGAL) begin
            if (pte_w && !pte_r) begin
                pte_r = 1'b1;
            end
            if (!(pte_r || pte_w || pte_x)) begin
                pte_r = 1'b1;
            end
        end
        derive_ad_bits(access_kind);
    endfunction:fixup_pte_legal

endclass:memblock_tlb_entry

class memblock_uid_tlb_record extends uvm_object;

    memblock_uid_t uid;
    bit            record_valid;
    bit            pte_valid;
    bit [51:0]     vpn;
    bit [1:0]      s2xlate;
    bit            is_hypervisor_inst;
    int unsigned   asid;
    int unsigned   vmid;
    memblock_tlb_lookup_key_t lookup_key;
    mmu_csr_runtime_state     csr_snapshot;

    bit [63:0] paddr;
    bit [63:0] ppn;
    bit pte_r;
    bit pte_w;
    bit pte_x;
    bit pte_u;
    bit pte_g;
    bit pte_a;
    bit pte_d;
    bit pte_n;
    bit pte_v;
    bit [1:0] pbmt;
    bit tlbAF;
    bit tlbPF;
    bit tlbGPF;
    bit pmaAF;
    bit [1:0] level;

    longint unsigned issue_cycle;
    longint unsigned pte_update_cycle;

    `uvm_object_utils(memblock_uid_tlb_record)

    function new(string name = "memblock_uid_tlb_record");
        super.new(name);
        csr_snapshot = mmu_csr_runtime_state::type_id::create({name, "_csr_snapshot"});
        reset();
    endfunction:new

    function void reset();
        uid                = 0;
        record_valid       = 1'b0;
        pte_valid          = 1'b0;
        vpn                = '0;
        s2xlate            = 2'd0;
        is_hypervisor_inst = 1'b0;
        asid               = 0;
        vmid               = 0;
        lookup_key         = '{default:'0};
        if (csr_snapshot == null) begin
            csr_snapshot = mmu_csr_runtime_state::type_id::create("csr_snapshot");
        end
        csr_snapshot.reset();
        paddr              = '0;
        ppn                = '0;
        pte_r              = 1'b0;
        pte_w              = 1'b0;
        pte_x              = 1'b0;
        pte_u              = 1'b0;
        pte_g              = 1'b0;
        pte_a              = 1'b0;
        pte_d              = 1'b0;
        pte_n              = 1'b0;
        pte_v              = 1'b0;
        pbmt               = '0;
        tlbAF              = 1'b0;
        tlbPF              = 1'b0;
        tlbGPF             = 1'b0;
        pmaAF              = 1'b0;
        level              = '0;
        issue_cycle        = 0;
        pte_update_cycle   = 0;
    endfunction:reset

    function void init_context(input memblock_uid_t uid_i,
                               input bit [51:0] vpn_i,
                               input bit [1:0] s2xlate_i,
                               input bit is_hypervisor_inst_i,
                               input mmu_csr_runtime_state csr_snapshot_i);
        if (csr_snapshot_i == null) begin
            `uvm_fatal("UID_TLB_RECORD", "init_context got null csr_snapshot")
        end
        uid                = uid_i;
        record_valid       = 1'b1;
        pte_valid          = 1'b0;
        vpn                = vpn_i;
        s2xlate            = s2xlate_i;
        is_hypervisor_inst = is_hypervisor_inst_i;
        csr_snapshot.copy_from(csr_snapshot_i);
        lookup_key         = csr_snapshot.make_lookup_key(vpn_i, s2xlate_i);
        asid               = lookup_key.asid;
        vmid               = lookup_key.vmid;
        issue_cycle        = memblock_sync_pkg::get_dispatch_service_cycle();
        pte_update_cycle   = 0;
    endfunction:init_context

    function void copy_entry_fields(input memblock_tlb_entry entry);
        if (entry == null) begin
            `uvm_fatal("UID_TLB_RECORD", "copy_entry_fields got null entry")
        end
        lookup_key       = entry.lookup_key;
        asid             = entry.asid;
        vmid             = entry.vmid;
        s2xlate          = entry.s2xlate;
        vpn              = entry.lookup_key.vpn;
        paddr            = entry.paddr;
        ppn              = entry.ppn;
        pte_r            = entry.pte_r;
        pte_w            = entry.pte_w;
        pte_x            = entry.pte_x;
        pte_u            = entry.pte_u;
        pte_g            = entry.pte_g;
        pte_a            = entry.pte_a;
        pte_d            = entry.pte_d;
        pte_n            = entry.pte_n;
        pte_v            = entry.pte_v;
        pbmt             = entry.pbmt;
        tlbAF            = entry.tlbAF;
        tlbPF            = entry.tlbPF;
        tlbGPF           = entry.tlbGPF;
        pmaAF            = entry.pmaAF;
        level            = entry.level;
        pte_valid        = 1'b1;
        pte_update_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:copy_entry_fields

endclass:memblock_uid_tlb_record

`endif
