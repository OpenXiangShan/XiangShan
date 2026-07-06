//=========================================================
//File name    : mmu_csr_runtime_state.sv
//Author       : OpenAI_Codex
//Module name  : mmu_csr_runtime_state
//Discribution : runtime MMU CSR mirror for dispatch TLB builder
//Date         : 2026-05-18
//=========================================================
`ifndef MMU_CSR_RUNTIME_STATE__SV
`define MMU_CSR_RUNTIME_STATE__SV

class mmu_csr_runtime_state extends uvm_object;

    bit [3:0]      satp_mode;
    bit [15:0]     satp_asid;
    bit [43:0]     satp_ppn;
    bit [3:0]      vsatp_mode;
    bit [15:0]     vsatp_asid;
    bit [43:0]     vsatp_ppn;
    bit [3:0]      hgatp_mode;
    bit [15:0]     hgatp_vmid;
    bit [43:0]     hgatp_ppn;

    bit            priv_virt;
    bit            priv_spvp;
    bit [1:0]      priv_imode;
    bit [1:0]      priv_dmode;
    bit            priv_mxr;
    bit            priv_sum;
    bit            priv_vmxr;
    bit            priv_vsum;
    bit            m_pbmt_en;
    bit            h_pbmt_en;

    int unsigned   update_seq;

    `uvm_object_utils(mmu_csr_runtime_state)

    function new(string name = "mmu_csr_runtime_state");
        super.new(name);
        reset();
    endfunction:new

    function void reset();
        satp_mode  = '0;
        satp_asid  = '0;
        satp_ppn   = '0;
        vsatp_mode = '0;
        vsatp_asid = '0;
        vsatp_ppn  = '0;
        hgatp_mode = '0;
        hgatp_vmid = '0;
        hgatp_ppn  = '0;
        priv_virt  = 1'b0;
        priv_spvp  = 1'b0;
        priv_imode = 2'd3;
        priv_dmode = 2'd3;
        priv_mxr   = 1'b0;
        priv_sum   = 1'b0;
        priv_vmxr  = 1'b0;
        priv_vsum  = 1'b0;
        m_pbmt_en  = 1'b0;
        h_pbmt_en  = 1'b0;
        update_seq = 0;
    endfunction:reset

    function void update_from_csr_ctrl(input csr_ctrl_agent_agent_xaction csr_tr);
        bit changed;

        if (csr_tr == null) begin
            `uvm_fatal("MMU_CSR", "update_from_csr_ctrl got null transaction")
        end

        changed =
            satp_mode  != csr_tr.io_ooo_to_mem_tlbCsr_satp_mode  ||
            satp_asid  != csr_tr.io_ooo_to_mem_tlbCsr_satp_asid  ||
            satp_ppn   != csr_tr.io_ooo_to_mem_tlbCsr_satp_ppn   ||
            vsatp_mode != csr_tr.io_ooo_to_mem_tlbCsr_vsatp_mode ||
            vsatp_asid != csr_tr.io_ooo_to_mem_tlbCsr_vsatp_asid ||
            vsatp_ppn  != csr_tr.io_ooo_to_mem_tlbCsr_vsatp_ppn  ||
            hgatp_mode != csr_tr.io_ooo_to_mem_tlbCsr_hgatp_mode ||
            hgatp_vmid != csr_tr.io_ooo_to_mem_tlbCsr_hgatp_vmid ||
            hgatp_ppn  != csr_tr.io_ooo_to_mem_tlbCsr_hgatp_ppn  ||
            priv_virt  != csr_tr.io_ooo_to_mem_tlbCsr_priv_virt  ||
            priv_spvp  != csr_tr.io_ooo_to_mem_tlbCsr_priv_spvp  ||
            priv_imode != csr_tr.io_ooo_to_mem_tlbCsr_priv_imode ||
            priv_dmode != csr_tr.io_ooo_to_mem_tlbCsr_priv_dmode ||
            priv_mxr   != csr_tr.io_ooo_to_mem_tlbCsr_priv_mxr   ||
            priv_sum   != csr_tr.io_ooo_to_mem_tlbCsr_priv_sum   ||
            priv_vmxr  != csr_tr.io_ooo_to_mem_tlbCsr_priv_vmxr  ||
            priv_vsum  != csr_tr.io_ooo_to_mem_tlbCsr_priv_vsum  ||
            m_pbmt_en  != csr_tr.io_ooo_to_mem_tlbCsr_mPBMTE     ||
            h_pbmt_en  != csr_tr.io_ooo_to_mem_tlbCsr_hPBMTE     ||
            csr_tr.io_ooo_to_mem_tlbCsr_satp_changed             ||
            csr_tr.io_ooo_to_mem_tlbCsr_vsatp_changed            ||
            csr_tr.io_ooo_to_mem_tlbCsr_hgatp_changed            ||
            csr_tr.io_ooo_to_mem_tlbCsr_priv_virt_changed;

        satp_mode  = csr_tr.io_ooo_to_mem_tlbCsr_satp_mode;
        satp_asid  = csr_tr.io_ooo_to_mem_tlbCsr_satp_asid;
        satp_ppn   = csr_tr.io_ooo_to_mem_tlbCsr_satp_ppn;
        vsatp_mode = csr_tr.io_ooo_to_mem_tlbCsr_vsatp_mode;
        vsatp_asid = csr_tr.io_ooo_to_mem_tlbCsr_vsatp_asid;
        vsatp_ppn  = csr_tr.io_ooo_to_mem_tlbCsr_vsatp_ppn;
        hgatp_mode = csr_tr.io_ooo_to_mem_tlbCsr_hgatp_mode;
        hgatp_vmid = csr_tr.io_ooo_to_mem_tlbCsr_hgatp_vmid;
        hgatp_ppn  = csr_tr.io_ooo_to_mem_tlbCsr_hgatp_ppn;
        priv_virt  = csr_tr.io_ooo_to_mem_tlbCsr_priv_virt;
        priv_spvp  = csr_tr.io_ooo_to_mem_tlbCsr_priv_spvp;
        priv_imode = csr_tr.io_ooo_to_mem_tlbCsr_priv_imode;
        priv_dmode = csr_tr.io_ooo_to_mem_tlbCsr_priv_dmode;
        priv_mxr   = csr_tr.io_ooo_to_mem_tlbCsr_priv_mxr;
        priv_sum   = csr_tr.io_ooo_to_mem_tlbCsr_priv_sum;
        priv_vmxr  = csr_tr.io_ooo_to_mem_tlbCsr_priv_vmxr;
        priv_vsum  = csr_tr.io_ooo_to_mem_tlbCsr_priv_vsum;
        m_pbmt_en  = csr_tr.io_ooo_to_mem_tlbCsr_mPBMTE;
        h_pbmt_en  = csr_tr.io_ooo_to_mem_tlbCsr_hPBMTE;
        if (changed) begin
            update_seq++;
        end
    endfunction:update_from_csr_ctrl

    function void update_from_raw_csr(input memblock_sync_pkg::dispatch_raw_csr_t raw);
        bit changed;

        if (!raw.valid) begin
            return;
        end

        changed =
            satp_mode  != raw.satp_mode         ||
            satp_asid  != raw.satp_asid         ||
            satp_ppn   != raw.satp_ppn          ||
            vsatp_mode != raw.vsatp_mode        ||
            vsatp_asid != raw.vsatp_asid        ||
            vsatp_ppn  != raw.vsatp_ppn         ||
            hgatp_mode != raw.hgatp_mode        ||
            hgatp_vmid != raw.hgatp_vmid        ||
            hgatp_ppn  != raw.hgatp_ppn         ||
            priv_virt  != raw.priv_virt         ||
            priv_spvp  != raw.priv_spvp         ||
            priv_imode != raw.priv_imode        ||
            priv_dmode != raw.priv_dmode        ||
            priv_mxr   != raw.priv_mxr          ||
            priv_sum   != raw.priv_sum          ||
            priv_vmxr  != raw.priv_vmxr         ||
            priv_vsum  != raw.priv_vsum         ||
            m_pbmt_en  != raw.m_pbmt_en         ||
            h_pbmt_en  != raw.h_pbmt_en         ||
            raw.satp_changed                    ||
            raw.vsatp_changed                   ||
            raw.hgatp_changed                   ||
            raw.priv_virt_changed;

        satp_mode  = raw.satp_mode;
        satp_asid  = raw.satp_asid;
        satp_ppn   = raw.satp_ppn;
        vsatp_mode = raw.vsatp_mode;
        vsatp_asid = raw.vsatp_asid;
        vsatp_ppn  = raw.vsatp_ppn;
        hgatp_mode = raw.hgatp_mode;
        hgatp_vmid = raw.hgatp_vmid;
        hgatp_ppn  = raw.hgatp_ppn;
        priv_virt  = raw.priv_virt;
        priv_spvp  = raw.priv_spvp;
        priv_imode = raw.priv_imode;
        priv_dmode = raw.priv_dmode;
        priv_mxr   = raw.priv_mxr;
        priv_sum   = raw.priv_sum;
        priv_vmxr  = raw.priv_vmxr;
        priv_vsum  = raw.priv_vsum;
        m_pbmt_en  = raw.m_pbmt_en;
        h_pbmt_en  = raw.h_pbmt_en;
        if (changed) begin
            update_seq++;
        end
    endfunction:update_from_raw_csr

    function bit current_s2xlate_enabled();
        return priv_virt && (hgatp_mode != 4'd0);
    endfunction:current_s2xlate_enabled

    function bit [15:0] current_asid(input bit [1:0] s2xlate);
        case (s2xlate)
            2'd0: return satp_asid;
            2'd1: return vsatp_asid;
            2'd2: return 16'd0;
            2'd3: return vsatp_asid;
            default: return satp_asid;
        endcase
    endfunction:current_asid

    function bit [15:0] current_vmid(input bit [1:0] s2xlate = 2'd0);
        case (s2xlate)
            2'd2,
            2'd3: return hgatp_vmid;
            default: return 16'd0;
        endcase
    endfunction:current_vmid

    function bit [1:0] expected_s2xlate(input bit is_hypervisor_inst);
        if (!(priv_virt || is_hypervisor_inst)) begin
            return 2'd0;
        end
        if (vsatp_mode != 4'd0 && hgatp_mode != 4'd0) begin
            return 2'd3;
        end
        if (vsatp_mode == 4'd0) begin
            return 2'd2;
        end
        if (hgatp_mode == 4'd0) begin
            return 2'd1;
        end
        return 2'd0;
    endfunction:expected_s2xlate

    function bit [1:0] current_priv_mode(input bit use_dmode = 1'b1);
        if (use_dmode) begin
            return priv_dmode;
        end
        return priv_imode;
    endfunction:current_priv_mode

    function memblock_tlb_lookup_key_t make_lookup_key(input bit [63:0] vpn,
                                                       input bit [1:0] s2xlate);
        memblock_tlb_lookup_key_t key;

        key.vpn     = vpn[51:0];
        key.asid    = current_asid(s2xlate);
        key.vmid    = current_vmid(s2xlate);
        key.s2xlate = s2xlate;
        return key;
    endfunction:make_lookup_key

    function void copy_from(input mmu_csr_runtime_state rhs);
        if (rhs == null) begin
            `uvm_fatal("MMU_CSR", "copy_from got null rhs")
        end
        satp_mode  = rhs.satp_mode;
        satp_asid  = rhs.satp_asid;
        satp_ppn   = rhs.satp_ppn;
        vsatp_mode = rhs.vsatp_mode;
        vsatp_asid = rhs.vsatp_asid;
        vsatp_ppn  = rhs.vsatp_ppn;
        hgatp_mode = rhs.hgatp_mode;
        hgatp_vmid = rhs.hgatp_vmid;
        hgatp_ppn  = rhs.hgatp_ppn;
        priv_virt  = rhs.priv_virt;
        priv_spvp  = rhs.priv_spvp;
        priv_imode = rhs.priv_imode;
        priv_dmode = rhs.priv_dmode;
        priv_mxr   = rhs.priv_mxr;
        priv_sum   = rhs.priv_sum;
        priv_vmxr  = rhs.priv_vmxr;
        priv_vsum  = rhs.priv_vsum;
        m_pbmt_en  = rhs.m_pbmt_en;
        h_pbmt_en  = rhs.h_pbmt_en;
        update_seq = rhs.update_seq;
    endfunction:copy_from

endclass:mmu_csr_runtime_state

`endif
