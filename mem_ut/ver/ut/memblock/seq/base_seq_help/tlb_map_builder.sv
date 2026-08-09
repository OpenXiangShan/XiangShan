//=========================================================
//File name    : tlb_map_builder.sv
//Module name  : tlb_map_builder
//Discribution : V2 L2TLB single-miss payload builder
//=========================================================
`ifndef TLB_MAP_BUILDER__SV
`define TLB_MAP_BUILDER__SV

class tlb_map_builder extends uvm_object;
    localparam bit [3:0] MEMBLOCK_SV39_MODE = 4'd8;
    localparam bit [3:0] MEMBLOCK_SV48_MODE = 4'd9;

    `uvm_object_utils(tlb_map_builder)

    function new(string name = "tlb_map_builder"); super.new(name); endfunction:new

    // Abstract responsibility: sample one Boolean payload field from its
    // configured percentage.  It does not alter any other field or retry a
    // failed payload build.
    function bit choose_bit(input int unsigned one_wt, input string field_name);
        bit chosen;

        if (one_wt > 100) begin
            `uvm_fatal("L2TLB_PAYLOAD_CFG",
                       $sformatf("%s weight=%0d is outside [0:100]", field_name, one_wt))
        end
        if (!std::randomize(chosen) with {
                chosen dist {1'b1 := one_wt, 1'b0 := 100 - one_wt};
            }) begin
            `uvm_fatal("L2TLB_PAYLOAD_RANDOM",
                       $sformatf("failed to randomize %s", field_name))
        end
        return chosen;
    endfunction:choose_bit

    // Abstract responsibility: choose one PBMT encoding for a single active
    // stage.  The return value is independent of level, fault and PTE fields.
    function bit [1:0] choose_pbmt(input bit s1);
        bit [1:0] chosen;
        int unsigned w0, w1, w2;

        w0 = seq_csr_common::get_l2tlb_pbmt_wt(s1, 0);
        w1 = seq_csr_common::get_l2tlb_pbmt_wt(s1, 1);
        w2 = seq_csr_common::get_l2tlb_pbmt_wt(s1, 2);
        if (w0 + w1 + w2 == 0) begin
            `uvm_fatal("L2TLB_PAYLOAD_CFG",
                       $sformatf("%s PBMT weights are all zero", s1 ? "S1" : "S2"))
        end
        if (!std::randomize(chosen) with {
                chosen inside {[0:2]};
                chosen dist {0 := w0, 1 := w1, 2 := w2};
            }) begin
            `uvm_fatal("L2TLB_PAYLOAD_RANDOM", "failed to randomize PBMT")
        end
        return chosen;
    endfunction:choose_pbmt

    function void validate_active_paged_mode(input bit s1,
                                              input bit [3:0] mode,
                                              input bit [1:0] s2xlate);
        if (mode != MEMBLOCK_SV39_MODE && mode != MEMBLOCK_SV48_MODE) begin
            `uvm_fatal("L2TLB_PAYLOAD_MODE",
                       $sformatf("active %s stage needs Sv39/Sv48 mode, got mode=%0d s2xlate=%0d",
                                 s1 ? "S1" : "S2", mode, s2xlate))
        end
    endfunction:validate_active_paged_mode

    // Abstract responsibility: pick an allowed page level once from the
    // frozen stage mode.  NAPOT can subsequently force the final level to
    // zero, but never performs a second weighted choice.
    function bit [1:0] choose_level(input bit s1,
                                    input bit active,
                                    input bit [3:0] mode);
        bit [1:0] chosen;
        int unsigned w0, w1, w2, w3;

        if (!active || !seq_csr_common::get_l2tlb_level_weight_en()) begin
            return 2'd0;
        end
        validate_active_paged_mode(s1, mode, '0);
        w0 = seq_csr_common::get_l2tlb_level_wt(s1, 0);
        w1 = seq_csr_common::get_l2tlb_level_wt(s1, 1);
        w2 = seq_csr_common::get_l2tlb_level_wt(s1, 2);
        w3 = seq_csr_common::get_l2tlb_level_wt(s1, 3);
        if (mode == MEMBLOCK_SV39_MODE) begin
            w3 = 0;
        end
        if (w0 + w1 + w2 + w3 == 0) begin
            `uvm_fatal("L2TLB_PAYLOAD_LEVEL",
                       $sformatf("stage=%s mode=%0d has no weighted level candidate raw={%0d,%0d,%0d,%0d}",
                                 s1 ? "S1" : "S2", mode,
                                 seq_csr_common::get_l2tlb_level_wt(s1, 0),
                                 seq_csr_common::get_l2tlb_level_wt(s1, 1),
                                 seq_csr_common::get_l2tlb_level_wt(s1, 2),
                                 seq_csr_common::get_l2tlb_level_wt(s1, 3)))
        end
        if (!std::randomize(chosen) with {
                chosen dist {0 := w0, 1 := w1, 2 := w2, 3 := w3};
            }) begin
            `uvm_fatal("L2TLB_PAYLOAD_RANDOM", "failed to randomize level")
        end
        return chosen;
    endfunction:choose_level

    // Abstract responsibility: construct a reproducible raw PPN source for
    // one response payload.  Strict memory mode keeps it in the configured
    // served range; sparse mode deliberately has no paddr-range ownership.
    function bit [43:0] make_canonical_ppn(input bit [37:0] vpn,
                                           input bit [43:0] root_ppn,
                                           input bit [1:0] level,
                                           input bit s2);
        bit [43:0] ppn;
        bit [63:0] page_count;
        bit [63:0] page_offset;

        if (seq_csr_common::get_main_mem_ranges_en()) begin
            page_count = seq_csr_common::get_paddr_range() >> 12;
            if (page_count == 0) begin
                `uvm_fatal("L2TLB_PAYLOAD_PPN", "configured paddr range has no page")
            end
            page_offset = {26'b0, vpn} % page_count;
            ppn = (seq_csr_common::get_paddr_base() >> 12) + page_offset;
        end
        else begin
            // Sparse mode is still deterministic per miss, but it does not
            // reserve a DCache backing line or force a fixed physical window.
            ppn = root_ppn ^ {6'b0, vpn} ^
                  (s2 ? 44'h0000_005a_5a5 : 44'h0000_003c_3c3);
        end
        case (level)
            2'd1: ppn[8:0]   = '0;
            2'd2: ppn[17:0]  = '0;
            2'd3: ppn[26:0]  = '0;
            default: begin end
        endcase
        return ppn;
    endfunction:make_canonical_ppn

    // 中文注释：把内部完整 S2 PPN 转为 V2 38-bit response 字段。
    // 任一路径都没有高位丢失 sideband，超过接口位宽必须立即失败。
    function bit [37:0] encode_s2_entry_ppn(input bit [43:0] raw_ppn);
        if (|raw_ppn[43:38]) begin
            `uvm_fatal("L2TLB_PAYLOAD_S2_PPN_WIDTH",
                       $sformatf("S2 PPN 0x%0h exceeds 38-bit response field", raw_ppn))
        end
        return raw_ppn[37:0];
    endfunction:encode_s2_entry_ppn

    // Abstract responsibility: deterministically make a normal LEGAL PTE a
    // leaf.  It consumes a field group already sampled by the profile and
    // never invokes randomization or changes the other stage.
    function void fixup_pte_legal(input bit s1, ref memblock_tlb_entry entry);
        if (s1) begin
            if (entry.s1_pte_w && !entry.s1_pte_r) entry.s1_pte_r = 1'b1;
            if (!(entry.s1_pte_r || entry.s1_pte_w || entry.s1_pte_x)) entry.s1_pte_r = 1'b1;
            entry.s1_pte_a = 1'b1;
            entry.s1_pte_d = 1'b1;
            entry.s1_pte_v = 1'b1;
        end
        else begin
            if (entry.s2_pte_w && !entry.s2_pte_r) entry.s2_pte_r = 1'b1;
            if (!(entry.s2_pte_r || entry.s2_pte_w || entry.s2_pte_x)) entry.s2_pte_r = 1'b1;
            entry.s2_pte_a = 1'b1;
            entry.s2_pte_d = 1'b1;
        end
    endfunction:fixup_pte_legal

    // 中文注释：raw 字段随机后仅调用一次 profile；当前 request 不带 access kind，
    // MIXED/EXCEPTION_BIASED 保留独立随机的 UNKNOWN A/D，LEGAL 只由后续 fixup 收敛。
    function void apply_pte_profile(input bit s1, input int unsigned mode,
                                    ref memblock_tlb_entry entry);
        case (mode)
            memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL,
            memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_MIXED,
            memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_EXCEPTION_BIASED: begin
                // Raw fields are already the profile result for UNKNOWN access.
            end
            default: begin
                `uvm_fatal("L2TLB_PAYLOAD_PTE_MODE",
                           $sformatf("invalid %s PTE profile mode=%0d",
                                     s1 ? "S1" : "S2", mode))
            end
        endcase
    endfunction:apply_pte_profile

    // Abstract responsibility: sample the independent permission/PTE.N bits
    // for one active stage, then run only the normal-LEGAL leaf fixup.
    function void fill_pte_fields(input bit s1, input bit active,
                                  ref memblock_tlb_entry entry);
        int unsigned mode;

        if (!active) begin
            return;
        end
        mode = s1 ? entry.s1_pte_mode_at_build : entry.s2_pte_mode_at_build;
        if (s1) begin
            entry.s1_pte_r = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 0), "S1_R");
            entry.s1_pte_w = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 1), "S1_W");
            entry.s1_pte_x = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 2), "S1_X");
            entry.s1_pte_u = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 3), "S1_U");
            entry.s1_pte_g = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 4), "S1_G");
            entry.s1_pte_a = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 5), "S1_A");
            entry.s1_pte_d = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 6), "S1_D");
            entry.s1_pte_n = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 7), "S1_N");
            entry.s1_pte_v = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(1, 8), "S1_V");
        end
        else begin
            entry.s2_pte_r = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 0), "S2_R");
            entry.s2_pte_w = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 1), "S2_W");
            entry.s2_pte_x = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 2), "S2_X");
            entry.s2_pte_u = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 3), "S2_U");
            entry.s2_pte_g = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 4), "S2_G");
            entry.s2_pte_a = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 5), "S2_A");
            entry.s2_pte_d = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 6), "S2_D");
            entry.s2_pte_n = choose_bit(seq_csr_common::get_l2tlb_pte_1_wt(0, 7), "S2_N");
        end
        apply_pte_profile(s1, mode, entry);
    endfunction:fill_pte_fields

    // 中文注释：effective fault 已确定后，仅在无 fault 的 LEGAL stage 收敛 PTE。
    // fault passthrough、MIXED 和 EXCEPTION_BIASED 保留先前冻结的 raw profile 字段。
    function void finalize_pte_fields(input bit s1, ref memblock_tlb_entry entry);
        int unsigned mode;

        mode = s1 ? entry.s1_pte_mode_at_build : entry.s2_pte_mode_at_build;
        if (!entry.has_effective_fault() &&
            mode == memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL) begin
            fixup_pte_legal(s1, entry);
        end
    endfunction:finalize_pte_fields

    // Abstract responsibility: create raw fault history once and collapse it
    // to the single response-visible fault allowed by the current s2xlate.
    function void fill_faults(input bit [1:0] s2xlate,
                              ref memblock_tlb_entry entry);
        bit raw_s1_pf, raw_s1_af, raw_s2_gpf, raw_s2_gaf;
        int unsigned selected;
        int unsigned best_weight;
        int unsigned best_priority;

        raw_s1_pf  = choose_bit(seq_csr_common::get_l2tlb_fault_1_wt(1, 0), "S1_PF");
        raw_s1_af  = choose_bit(seq_csr_common::get_l2tlb_fault_1_wt(1, 1), "S1_AF");
        raw_s2_gpf = choose_bit(seq_csr_common::get_l2tlb_fault_1_wt(0, 0), "S2_GPF");
        raw_s2_gaf = choose_bit(seq_csr_common::get_l2tlb_fault_1_wt(0, 1), "S2_GAF");
        entry.fault_raw_s1_pf = raw_s1_pf;
        entry.fault_raw_s1_af = raw_s1_af;
        entry.fault_raw_s2_gpf = raw_s2_gpf;
        entry.fault_raw_s2_gaf = raw_s2_gaf;

        selected = 4;
        best_weight = 0;
        best_priority = 0;
        for (int candidate = 0; candidate < 4; candidate++) begin
            bit candidate_active;
            bit candidate_raw;
            int unsigned candidate_weight;
            int unsigned candidate_priority;

            candidate_active = (s2xlate == 2'd0 || s2xlate == 2'd1) ?
                               (candidate < 2) :
                               (s2xlate == 2'd2) ? (candidate >= 2) : 1'b1;
            candidate_raw = (candidate == 0) ? raw_s1_pf :
                            (candidate == 1) ? raw_s1_af :
                            (candidate == 2) ? raw_s2_gpf : raw_s2_gaf;
            candidate_weight = (candidate < 2) ?
                               seq_csr_common::get_l2tlb_fault_1_wt(1, candidate) :
                               seq_csr_common::get_l2tlb_fault_1_wt(0, candidate - 2);
            candidate_priority = (candidate == 3) ? 4 :
                                 (candidate == 1) ? 3 :
                                 (candidate == 0) ? 2 : 1;
            if (candidate_active && candidate_raw &&
                (candidate_weight > best_weight ||
                 (candidate_weight == best_weight && candidate_priority > best_priority))) begin
                selected = candidate;
                best_weight = candidate_weight;
                best_priority = candidate_priority;
            end
        end
        entry.fault_effective_s1_pf = selected == 0;
        entry.fault_effective_s1_af = selected == 1;
        entry.fault_effective_s2_gpf = selected == 2;
        entry.fault_effective_s2_gaf = selected == 3;
        if (selected < 2) begin
            entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
        end
        else if (selected < 4) begin
            entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
        end
        else begin
            entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
        end
    endfunction:fill_faults

    // 中文注释：onlyStage2 的输入 GVPN 超出 hgatp mode 时，DUT 语义固定为 S2 GPF。
    // 该函数只覆盖 effective 选择，保留四个 raw fault 随机历史供 debug/snapshot 使用。
    function void force_effective_s2_gpf(ref memblock_tlb_entry entry);
        entry.fault_effective_s1_pf = 1'b0;
        entry.fault_effective_s1_af = 1'b0;
        entry.fault_effective_s2_gpf = 1'b1;
        entry.fault_effective_s2_gaf = 1'b0;
        entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
    endfunction:force_effective_s2_gpf

    function void apply_legal_napot_ppn_encoding(input bit s1,
                                                  input bit [1:0] final_level,
                                                  input bit pte_n,
                                                  ref bit [43:0] canonical_ppn,
                                                  input memblock_tlb_entry entry);
        if (entry.has_effective_fault() || !pte_n ||
            (s1 ? entry.s1_pte_mode_at_build : entry.s2_pte_mode_at_build) !=
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL) begin
            return;
        end
        if (final_level != 0) begin
            `uvm_fatal("L2TLB_PAYLOAD_NAPOT",
                       $sformatf("LEGAL %s NAPOT did not converge to level 0", s1 ? "S1" : "S2"))
        end
        canonical_ppn[3:0] = 4'b1000;
    endfunction:apply_legal_napot_ppn_encoding

    function bit napot_raw_ppn_is_model_resolvable(input bit s1,
                                                    input bit pte_n,
                                                    input bit [43:0] canonical_ppn,
                                                    input memblock_tlb_entry entry);
        if (!pte_n) begin
            return 1'b1;
        end
        if (canonical_ppn[3:0] == 4'b1000) begin
            return 1'b1;
        end
        if ((s1 ? entry.s1_pte_mode_at_build : entry.s2_pte_mode_at_build) ==
            memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL) begin
            `uvm_fatal("L2TLB_PAYLOAD_NAPOT",
                       $sformatf("LEGAL %s NAPOT raw PPN=0x%0h is not encoded", s1 ? "S1" : "S2", canonical_ppn))
        end
        `uvm_info("L2TLB_PAYLOAD_NAPOT",
                  $sformatf("non-LEGAL %s NAPOT raw PPN=0x%0h is response-only",
                            s1 ? "S1" : "S2", canonical_ppn), UVM_LOW)
        return 1'b0;
    endfunction:napot_raw_ppn_is_model_resolvable

    // Abstract responsibility: reproduce the raw genPPN bit splice used by
    // the response payload.  It is used for normal derived debug state and
    // allStage's S2 tag anchor, never for a fault-path DCache address.
    function bit [43:0] resolve_ppn_from_raw(input bit [43:0] canonical_ppn,
                                              input bit [37:0] request_vpn,
                                              input bit [1:0] level,
                                              input bit pte_n);
        case (level)
            2'd3: return {canonical_ppn[43:27], request_vpn[26:0]};
            2'd2: return {canonical_ppn[43:18], request_vpn[17:0]};
            2'd1: return {canonical_ppn[43:9], request_vpn[8:0]};
            default: begin
                if (pte_n) begin
                    return {canonical_ppn[43:4], request_vpn[3:0]};
                end
                return canonical_ppn;
            end
        endcase
    endfunction:resolve_ppn_from_raw

    function bit s1_request_fits_mode(input bit [37:0] vpn,
                                      input bit [3:0] mode);
        if (mode == MEMBLOCK_SV39_MODE) begin
            return !(|vpn[37:27]);
        end
        if (mode == MEMBLOCK_SV48_MODE) begin
            return !(|vpn[37:36]);
        end
        return 1'b0;
    endfunction:s1_request_fits_mode

    function bit s2_request_fits_mode(input bit [51:0] gvpn,
                                      input bit [3:0] mode);
        if (mode == MEMBLOCK_SV39_MODE) begin
            return !(|gvpn[51:29]);
        end
        if (mode == MEMBLOCK_SV48_MODE) begin
            return !(|gvpn[51:38]);
        end
        return 1'b0;
    endfunction:s2_request_fits_mode

    // Abstract responsibility: derive and range-check the fixed S2 tag used
    // by an allStage response from the already encoded S1 raw payload.  It
    // does not decide model address validity or repair a non-canonical PTE.
    function bit [37:0] derive_allstage_raw_s2_tag(
        input memblock_tlb_entry s1_entry,
        input bit [37:0] request_vpn,
        input bit [3:0] s2_mode);
        bit [43:0] selected_s1_ppn;
        bit [43:0] raw_gvpn;

        if (s1_entry == null) begin
            `uvm_fatal("L2TLB_PAYLOAD_ALLSTAGE_TAG",
                       "derive_allstage_raw_s2_tag got null S1 entry")
        end
        s1_entry.validate_s1_sector_payload_consistency("ALLSTAGE_TAG");
        selected_s1_ppn = s1_entry.get_s1_selected_canonical_ppn();
        raw_gvpn = resolve_ppn_from_raw(selected_s1_ppn, request_vpn,
                                        s1_entry.s1_level, s1_entry.s1_pte_n);
        case (s2_mode)
            MEMBLOCK_SV39_MODE: begin
                if (|raw_gvpn[43:29]) begin
                    `uvm_fatal("L2TLB_PAYLOAD_ALLSTAGE_TAG",
                               $sformatf("allStage raw GVPN 0x%0h exceeds Sv39x4 29-bit range",
                                         raw_gvpn))
                end
                return {9'b0, raw_gvpn[28:0]};
            end
            MEMBLOCK_SV48_MODE: begin
                if (|raw_gvpn[43:38]) begin
                    `uvm_fatal("L2TLB_PAYLOAD_ALLSTAGE_TAG",
                               $sformatf("allStage raw GVPN 0x%0h exceeds Sv48x4 38-bit range",
                                         raw_gvpn))
                end
                return raw_gvpn[37:0];
            end
            default: begin
                `uvm_fatal("L2TLB_PAYLOAD_ALLSTAGE_TAG",
                           $sformatf("unsupported allStage S2 mode=%0d", s2_mode))
            end
        endcase
        return '0;
    endfunction:derive_allstage_raw_s2_tag

    // 中文注释：将冻结的 canonical S1 PPN 编码到 V2 sector 字段。
    // 本函数只负责 split/one-hot，不参与 fault 选择或二次随机。
    function void build_s1_sector_payload(input bit [43:0] canonical_ppn,
                                          ref memblock_tlb_entry entry);
        bit superpage_or_napot;

        entry.s1_entry_ppn_raw = canonical_ppn[43:3];
        superpage_or_napot = entry.s1_level != 0 || entry.s1_pte_n;
        foreach (entry.s1_ppn_low[idx]) begin
            entry.s1_ppn_low[idx] = '0;
            entry.s1_valididx[idx] = 1'b0;
            entry.s1_pteidx[idx] = 1'b0;
            if (superpage_or_napot || idx == entry.s1_addr_low) begin
                entry.s1_valididx[idx] = 1'b1;
                // 中文注释：responder 只构造一份冻结 response，不模拟八个 PTE refill；
                // 每个有效 sector 均保留该 payload 的 canonical PPN low，不能用 sector index 伪造。
                entry.s1_ppn_low[idx] = canonical_ppn[2:0];
            end
        end
        entry.s1_pteidx[entry.s1_addr_low] = 1'b1;
        entry.validate_s1_sector_payload_consistency("BUILD", null,
                                                      canonical_ppn, 1'b1);
    endfunction:build_s1_sector_payload

    // 中文注释：检查 CSR VMID 是否可编码到 V2 14-bit response wire。
    // hgatp_vmid[15:14] 非零时禁止截断，否则 DUT 零扩展比较必然无法命中。
    function void check_response_vmid_encodable(input bit [15:0] vmid,
                                                 input bit [1:0] s2xlate);
        if (|vmid[15:14]) begin
            `uvm_fatal("L2TLB_PAYLOAD_VMID_WIDTH",
                       $sformatf("s2xlate=%0d hgatp.vmid=0x%0h exceeds 14-bit response field",
                                 s2xlate, vmid))
        end
    endfunction:check_response_vmid_encodable

    function void reject_unsupported_valid_s1_nonleaf(input memblock_tlb_entry entry);
        if (!entry.has_effective_fault() && entry.s1_pte_v &&
            !(entry.s1_pte_r || entry.s1_pte_w || entry.s1_pte_x)) begin
            `uvm_fatal("L2TLB_UNSUPPORTED_VALID_S1_NONLEAF",
                       "normal valid S1 non-leaf requires a later page walk and is not modeled")
        end
    endfunction:reject_unsupported_valid_s1_nonleaf

    // Abstract responsibility: establish active stages and request-time CSR
    // provenance before payload fields are sampled.  Inactive stage fields
    // remain reset values and never receive a synthetic fallback payload.
    function void freeze_stage_context(input memblock_tlb_lookup_key_t key,
                                       input mmu_csr_runtime_state csr_state,
                                       ref memblock_tlb_entry entry);
        case (key.s2xlate)
            2'd0: begin
                entry.s1_stage_active = 1'b1;
                entry.s1_translation_mode_at_build = csr_state.satp_mode;
                entry.s1_root_ppn_at_build = csr_state.satp_ppn;
                entry.s1_asid = csr_state.satp_asid;
                entry.s1_vmid = '0;
            end
            2'd1: begin
                entry.s1_stage_active = 1'b1;
                entry.s1_translation_mode_at_build = csr_state.vsatp_mode;
                entry.s1_root_ppn_at_build = csr_state.vsatp_ppn;
                entry.s1_asid = csr_state.vsatp_asid;
                entry.s1_vmid = csr_state.hgatp_vmid;
            end
            2'd2: begin
                entry.s2_stage_active = 1'b1;
                entry.s2_translation_mode_at_build = csr_state.hgatp_mode;
                entry.s2_root_ppn_at_build = csr_state.hgatp_ppn;
                entry.s2_vmid = csr_state.hgatp_vmid;
            end
            2'd3: begin
                entry.s1_stage_active = 1'b1;
                entry.s2_stage_active = 1'b1;
                entry.s1_translation_mode_at_build = csr_state.vsatp_mode;
                entry.s1_root_ppn_at_build = csr_state.vsatp_ppn;
                entry.s1_asid = csr_state.vsatp_asid;
                entry.s1_vmid = csr_state.hgatp_vmid;
                entry.s2_translation_mode_at_build = csr_state.hgatp_mode;
                entry.s2_root_ppn_at_build = csr_state.hgatp_ppn;
                entry.s2_vmid = csr_state.hgatp_vmid;
            end
            default: begin
                `uvm_fatal("L2TLB_PAYLOAD_MODE",
                           $sformatf("unsupported s2xlate=%0d", key.s2xlate))
            end
        endcase
        if (key.s2xlate != 2'd0) begin
            check_response_vmid_encodable(csr_state.hgatp_vmid, key.s2xlate);
        end
        if (entry.s1_stage_active) begin
            validate_active_paged_mode(1'b1, entry.s1_translation_mode_at_build,
                                       key.s2xlate);
        end
        if (entry.s2_stage_active) begin
            validate_active_paged_mode(1'b0, entry.s2_translation_mode_at_build,
                                       key.s2xlate);
        end
    endfunction:freeze_stage_context

    // 中文注释：将 testcase PTE profile 配置写入本次 entry provenance。
    // profile 不是 runtime CSR；它在 sequence 启动时已固定，但仍需随 pending/UID 一起复制。
    function void freeze_pte_profile_modes(ref memblock_tlb_entry entry);
        int unsigned mode;

        if (entry.s1_stage_active) begin
            mode = seq_csr_common::get_l2tlb_pte_mode(1'b1);
            if (mode >
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_EXCEPTION_BIASED) begin
                `uvm_fatal("L2TLB_PAYLOAD_PTE_MODE", "invalid frozen S1 PTE profile mode")
            end
            entry.s1_pte_mode_at_build = mode[1:0];
        end
        if (entry.s2_stage_active) begin
            mode = seq_csr_common::get_l2tlb_pte_mode(1'b0);
            if (mode >
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_EXCEPTION_BIASED) begin
                `uvm_fatal("L2TLB_PAYLOAD_PTE_MODE", "invalid frozen S2 PTE profile mode")
            end
            entry.s2_pte_mode_at_build = mode[1:0];
        end
    endfunction:freeze_pte_profile_modes

    // Abstract responsibility: generate the complete immutable S1/S2 payload
    // for one lookup miss.  It returns a detached object; table insertion,
    // entry generation, pending tokens and lifecycle ownership remain outside
    // this builder.
    function memblock_tlb_entry build_payload_for_key_with_csr(
        input memblock_tlb_lookup_key_t key,
        input mmu_csr_runtime_state csr_state);
        memblock_tlb_entry entry;
        bit [43:0] s1_canonical_ppn;
        bit [43:0] s2_canonical_ppn;
        bit [37:0] s2_input_vpn;
        bit        s1_resolvable;
        bit        s2_resolvable;
        bit        s2_request_in_range;

        if (csr_state == null) begin
            `uvm_fatal("TLB_BUILDER", "build_payload_for_key_with_csr got null CSR snapshot")
        end
        entry = memblock_tlb_entry::type_id::create(
            $sformatf("tlb_entry_%0h_%0d", key.vpn, key.s2xlate));
        if (entry == null) begin
            `uvm_fatal("TLB_BUILDER", "failed to create payload entry")
        end
        entry.reset();
        entry.lookup_key = key;
        entry.s2xlate = key.s2xlate;
        entry.csr_context_seq_at_build = csr_state.update_seq;
        freeze_stage_context(key, csr_state, entry);
        freeze_pte_profile_modes(entry);

        if (entry.s1_stage_active) begin
            fill_pte_fields(1'b1, 1'b1, entry);
            entry.s1_level = choose_level(1'b1, 1'b1,
                                          entry.s1_translation_mode_at_build);
            entry.s1_addr_low = key.vpn[2:0];
            entry.s1_tag = key.vpn[37:3];
        end

        s2_request_in_range = 1'b1;
        if (entry.s2_stage_active) begin
            fill_pte_fields(1'b0, 1'b1, entry);
            entry.s2_level = choose_level(1'b0, 1'b1,
                                          entry.s2_translation_mode_at_build);
        end

        // 中文注释：raw PTE/profile 与候选 level 先冻结，再选择 effective fault。
        // 后续只有无 fault 的 LEGAL 路径可为正常翻译收敛 PTE/level。
        fill_faults(key.s2xlate, entry);
        if (entry.s2_stage_active && key.s2xlate == 2'd2) begin
            s2_request_in_range = s2_request_fits_mode(
                key.vpn, entry.s2_translation_mode_at_build);
            if (!s2_request_in_range) begin
                force_effective_s2_gpf(entry);
            end
        end

        if (entry.s1_stage_active) begin
            finalize_pte_fields(1'b1, entry);
            if (!entry.has_effective_fault() && entry.s1_pte_n) begin
                entry.s1_level = 2'd0;
            end
            s1_canonical_ppn = make_canonical_ppn(key.vpn[37:0],
                                                   entry.s1_root_ppn_at_build,
                                                   entry.s1_level, 1'b0);
            apply_legal_napot_ppn_encoding(1'b1, entry.s1_level,
                                            entry.s1_pte_n, s1_canonical_ppn,
                                            entry);
            build_s1_sector_payload(s1_canonical_ppn, entry);
            entry.s1_entry_pbmt = choose_pbmt(1'b1);
            if (!entry.has_effective_fault()) begin
                reject_unsupported_valid_s1_nonleaf(entry);
                s1_resolvable = entry.s1_pte_v &&
                                (entry.s1_pte_r || entry.s1_pte_w || entry.s1_pte_x) &&
                                s1_request_fits_mode(key.vpn[37:0],
                                                      entry.s1_translation_mode_at_build) &&
                                napot_raw_ppn_is_model_resolvable(1'b1,
                                    entry.s1_pte_n, s1_canonical_ppn, entry);
                entry.s1_resolved_ppn_valid = s1_resolvable;
                if (s1_resolvable) begin
                    entry.s1_resolved_ppn = resolve_ppn_from_raw(
                        s1_canonical_ppn, key.vpn[37:0], entry.s1_level,
                        entry.s1_pte_n);
                end
            end
        end

        if (entry.s2_stage_active) begin
            finalize_pte_fields(1'b0, entry);
            if (!entry.has_effective_fault() && entry.s2_pte_n) begin
                entry.s2_level = 2'd0;
            end
            if (key.s2xlate != 2'd2 && !entry.has_effective_fault()) begin
                // This is the protocol raw anchor.  It deliberately uses raw
                // S1 fields even when a non-LEGAL NAPOT cannot form a model
                // resolved PPN.
                s2_input_vpn = derive_allstage_raw_s2_tag(
                    entry, key.vpn[37:0], entry.s2_translation_mode_at_build);
            end
            else begin
                // A fault response does not claim a normal allStage GVPN.
                // s2.tag is nevertheless an encodable payload field.
                s2_input_vpn = key.vpn[37:0];
            end
            entry.s2_tag = s2_input_vpn;
            s2_canonical_ppn = make_canonical_ppn(s2_input_vpn,
                                                   entry.s2_root_ppn_at_build,
                                                   entry.s2_level, 1'b1);
            apply_legal_napot_ppn_encoding(1'b0, entry.s2_level,
                                            entry.s2_pte_n, s2_canonical_ppn,
                                            entry);
            entry.s2_entry_ppn_raw = encode_s2_entry_ppn(s2_canonical_ppn);
            entry.s2_entry_pbmt = choose_pbmt(1'b0);
            if (!entry.has_effective_fault()) begin
                s2_resolvable = (key.s2xlate != 2'd2 || s2_request_in_range) &&
                                napot_raw_ppn_is_model_resolvable(1'b0,
                                    entry.s2_pte_n, s2_canonical_ppn, entry);
                entry.s2_resolved_ppn_valid = s2_resolvable;
                if (s2_resolvable) begin
                    entry.s2_resolved_ppn = resolve_ppn_from_raw(
                        s2_canonical_ppn, s2_input_vpn, entry.s2_level,
                        entry.s2_pte_n);
                end
            end
        end

        entry.create_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        entry.last_hit_cycle = entry.create_cycle;
        entry.check_inactive_stage_defaults("BUILD");
        entry.validate_s1_sector_payload_consistency("BUILD");
        return entry;
    endfunction:build_payload_for_key_with_csr

    // Abstract responsibility: preserve the historical builder API while
    // routing it to the single new payload core.  It does not call the old
    // shared address/PTE builder or mutate the live table.
    function memblock_tlb_entry build_tlb_entry_for_req(input bit [37:0] vpn,
                                                        input bit [1:0] s2xlate,
                                                        input mmu_csr_runtime_state csr_state);
        memblock_tlb_lookup_key_t key;

        if (csr_state == null) begin
            `uvm_fatal("TLB_BUILDER", "build_tlb_entry_for_req got null CSR snapshot")
        end
        key = csr_state.make_lookup_key({26'b0, vpn}, s2xlate);
        return build_payload_for_key_with_csr(key, csr_state);
    endfunction:build_tlb_entry_for_req
endclass:tlb_map_builder

`endif
