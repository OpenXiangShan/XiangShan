//=========================================================
// Single-solve CSR profile randomizer and PMP/PMA write plan
//=========================================================
`ifndef MEMBLOCK_CSR_RANDOMIZER__SV
`define MEMBLOCK_CSR_RANDOMIZER__SV

class memblock_csr_randomizer extends uvm_object;
    memblock_csr_sequence_cfg_t cfg;
    bit dynamic_mode;

    bit [3:0] cur_satp_mode, cur_vsatp_mode, cur_hgatp_mode;
    bit [15:0] cur_satp_asid, cur_vsatp_asid, cur_hgatp_vmid;
    bit cur_mxr, cur_sum, cur_vmxr, cur_vsum;
    bit cur_priv_virt;
    bit [1:0] cur_priv_imode, cur_priv_dmode;
    bit cur_l1d_pf, cur_l1d_agt, cur_l1d_pht, cur_l1d_stride;
    bit cur_l2_master, cur_l2_recv, cur_l2_pbop, cur_l2_vbop;
    bit cur_ldld, cur_cache_error, cur_uncache, cur_misalign_ld, cur_misalign_st;
    memblock_csr_pmp_pma_profile_t cur_region;

    rand bit [3:0] satp_mode, vsatp_mode, hgatp_mode;
    rand bit [15:0] satp_asid, vsatp_asid, hgatp_vmid;
    rand bit mxr, sum, vmxr, vsum;
    rand bit priv_virt;
    rand bit [1:0] priv_imode, priv_dmode;
    rand bit l1d_pf, l1d_agt, l1d_pht, l1d_stride;
    rand bit l2_master, l2_recv, l2_pbop, l2_vbop;
    rand bit ldld, cache_error, uncache, misalign_ld, misalign_st;
    rand bit pmp_r, pmp_w, pmp_x, pma_c, pma_atomic;

    `uvm_object_utils(memblock_csr_randomizer)

    constraint c_modes {
        if (!dynamic_mode) {
            satp_mode dist {4'd0 := cfg.init_satp_bare_wt,
                            4'd8 := cfg.init_satp_sv39_wt,
                            4'd9 := cfg.init_satp_sv48_wt};
            vsatp_mode dist {4'd0 := cfg.init_vsatp_bare_wt,
                             4'd8 := cfg.init_vsatp_sv39_wt,
                             4'd9 := cfg.init_vsatp_sv48_wt};
            hgatp_mode dist {4'd0 := cfg.init_hgatp_bare_wt,
                             4'd8 := cfg.init_hgatp_sv39x4_wt,
                             4'd9 := cfg.init_hgatp_sv48x4_wt};
        } else {
            if (cfg.change_satp_enable)
                satp_mode dist {4'd0 := cfg.change_satp_bare_wt,
                                4'd8 := cfg.change_satp_sv39_wt,
                                4'd9 := cfg.change_satp_sv48_wt};
            else satp_mode == cur_satp_mode;
            if (cfg.change_vsatp_enable)
                vsatp_mode dist {4'd0 := cfg.change_vsatp_bare_wt,
                                 4'd8 := cfg.change_vsatp_sv39_wt,
                                 4'd9 := cfg.change_vsatp_sv48_wt};
            else vsatp_mode == cur_vsatp_mode;
            if (cfg.change_hgatp_enable)
                hgatp_mode dist {4'd0 := cfg.change_hgatp_bare_wt,
                                 4'd8 := cfg.change_hgatp_sv39x4_wt,
                                 4'd9 := cfg.change_hgatp_sv48x4_wt};
            else hgatp_mode == cur_hgatp_mode;
        }
    }

    constraint c_ids {
        if (satp_mode == 4'd0) satp_asid == 0;
        else if (!dynamic_mode)
            satp_asid inside {[cfg.init_satp_asid_min:cfg.init_satp_asid_max]};
        else if (cfg.change_satp_enable)
            satp_asid inside {[cfg.change_satp_asid_min:cfg.change_satp_asid_max]};
        else satp_asid == cur_satp_asid;
        if (vsatp_mode == 4'd0) vsatp_asid == 0;
        else if (!dynamic_mode)
            vsatp_asid inside {[cfg.init_vsatp_asid_min:cfg.init_vsatp_asid_max]};
        else if (cfg.change_vsatp_enable)
            vsatp_asid inside {[cfg.change_vsatp_asid_min:cfg.change_vsatp_asid_max]};
        else vsatp_asid == cur_vsatp_asid;
        hgatp_vmid[15:14] == 0;
        if (hgatp_mode == 4'd0) hgatp_vmid == 0;
        else if (!dynamic_mode)
            hgatp_vmid inside {[cfg.init_hgatp_vmid_min:cfg.init_hgatp_vmid_max]};
        else if (cfg.change_hgatp_enable)
            hgatp_vmid inside {[cfg.change_hgatp_vmid_min:cfg.change_hgatp_vmid_max]};
        else hgatp_vmid == cur_hgatp_vmid;
    }

    constraint c_solve_order {
        solve satp_mode before satp_asid;
        solve vsatp_mode before vsatp_asid;
        solve hgatp_mode before hgatp_vmid;
    }

    constraint c_permission {
        if (!dynamic_mode) {
            mxr dist {0 := cfg.init_mxr_0_wt, 1 := cfg.init_mxr_1_wt};
            sum dist {0 := cfg.init_sum_0_wt, 1 := cfg.init_sum_1_wt};
            vmxr dist {0 := cfg.init_vmxr_0_wt, 1 := cfg.init_vmxr_1_wt};
            vsum dist {0 := cfg.init_vsum_0_wt, 1 := cfg.init_vsum_1_wt};
        } else if (cfg.change_permission_enable) {
            mxr dist {0 := cfg.change_mxr_0_wt, 1 := cfg.change_mxr_1_wt};
            sum dist {0 := cfg.change_sum_0_wt, 1 := cfg.change_sum_1_wt};
            vmxr dist {0 := cfg.change_vmxr_0_wt, 1 := cfg.change_vmxr_1_wt};
            vsum dist {0 := cfg.change_vsum_0_wt, 1 := cfg.change_vsum_1_wt};
        } else {
            mxr == cur_mxr; sum == cur_sum; vmxr == cur_vmxr; vsum == cur_vsum;
        }
    }

    constraint c_priv_context {
        if (!dynamic_mode) {
            priv_virt dist {0 := cfg.init_priv_virt_0_wt, 1 := cfg.init_priv_virt_1_wt};
            priv_imode dist {2'b00 := cfg.init_priv_imode_u_wt,
                             2'b01 := cfg.init_priv_imode_s_wt,
                             2'b11 := cfg.init_priv_imode_m_wt};
            priv_dmode dist {2'b00 := cfg.init_priv_dmode_u_wt,
                             2'b01 := cfg.init_priv_dmode_s_wt,
                             2'b11 := cfg.init_priv_dmode_m_wt};
        } else if (cfg.change_priv_context_enable) {
            priv_virt dist {0 := cfg.change_priv_virt_0_wt, 1 := cfg.change_priv_virt_1_wt};
            priv_imode dist {2'b00 := cfg.change_priv_imode_u_wt,
                             2'b01 := cfg.change_priv_imode_s_wt,
                             2'b11 := cfg.change_priv_imode_m_wt};
            priv_dmode dist {2'b00 := cfg.change_priv_dmode_u_wt,
                             2'b01 := cfg.change_priv_dmode_s_wt,
                             2'b11 := cfg.change_priv_dmode_m_wt};
        } else {
            priv_virt == cur_priv_virt;
            priv_imode == cur_priv_imode;
            priv_dmode == cur_priv_dmode;
        }
    }

    constraint c_static_enable_profile {
        if (!dynamic_mode) {
            l1d_pf dist {0 := cfg.enable_l1d_pf_0_wt, 1 := cfg.enable_l1d_pf_1_wt};
            l1d_agt dist {0 := cfg.enable_l1d_pf_agt_0_wt, 1 := cfg.enable_l1d_pf_agt_1_wt};
            l1d_pht dist {0 := cfg.enable_l1d_pf_pht_0_wt, 1 := cfg.enable_l1d_pf_pht_1_wt};
            l1d_stride dist {0 := cfg.enable_l1d_pf_stride_0_wt, 1 := cfg.enable_l1d_pf_stride_1_wt};
            l2_master dist {0 := cfg.enable_l2_pf_master_0_wt, 1 := cfg.enable_l2_pf_master_1_wt};
            l2_recv dist {0 := cfg.enable_l2_pf_recv_0_wt, 1 := cfg.enable_l2_pf_recv_1_wt};
            l2_pbop dist {0 := cfg.enable_l2_pf_pbop_0_wt, 1 := cfg.enable_l2_pf_pbop_1_wt};
            l2_vbop dist {0 := cfg.enable_l2_pf_vbop_0_wt, 1 := cfg.enable_l2_pf_vbop_1_wt};
            ldld dist {0 := cfg.enable_ldld_vio_0_wt, 1 := cfg.enable_ldld_vio_1_wt};
            cache_error dist {0 := cfg.enable_cache_error_0_wt, 1 := cfg.enable_cache_error_1_wt};
            uncache dist {0 := cfg.enable_uncache_outstanding_0_wt, 1 := cfg.enable_uncache_outstanding_1_wt};
            misalign_ld dist {0 := cfg.enable_misalign_ld_0_wt, 1 := cfg.enable_misalign_ld_1_wt};
            misalign_st dist {0 := cfg.enable_misalign_st_0_wt, 1 := cfg.enable_misalign_st_1_wt};
        } else {
            l1d_pf == cur_l1d_pf; l1d_agt == cur_l1d_agt;
            l1d_pht == cur_l1d_pht; l1d_stride == cur_l1d_stride;
            l2_master == cur_l2_master; l2_recv == cur_l2_recv;
            l2_pbop == cur_l2_pbop; l2_vbop == cur_l2_vbop;
            ldld == cur_ldld; cache_error == cur_cache_error; uncache == cur_uncache;
            misalign_ld == cur_misalign_ld; misalign_st == cur_misalign_st;
        }
        l1d_agt -> (l1d_pf && l2_recv);
        l1d_pht -> (l1d_pf && l2_recv);
        l1d_stride -> l1d_pf;
        l2_recv -> l2_master;
        l2_pbop -> l2_master;
        l2_vbop -> l2_master;
    }

    constraint c_region_attributes {
        pmp_w -> pmp_r;
        if (!cfg.pmp_pma_exception_enable) {
            pmp_r == 0; pmp_w == 0; pmp_x == 0; pma_c == 0; pma_atomic == 0;
        } else if (!dynamic_mode || cfg.change_pmp_pma_enable) {
            pmp_r dist {0 := cfg.pmp_exception_r_0_wt, 1 := cfg.pmp_exception_r_1_wt};
            pmp_w dist {0 := cfg.pmp_exception_w_0_wt, 1 := cfg.pmp_exception_w_1_wt};
            pmp_x dist {0 := cfg.pmp_exception_x_0_wt, 1 := cfg.pmp_exception_x_1_wt};
            pma_c dist {0 := cfg.pma_exception_c_0_wt, 1 := cfg.pma_exception_c_1_wt};
            pma_atomic dist {0 := cfg.pma_exception_atomic_0_wt,
                             1 := cfg.pma_exception_atomic_1_wt};
        } else {
            pmp_r == cur_region.pmp_r; pmp_w == cur_region.pmp_w;
            pmp_x == cur_region.pmp_x; pma_c == cur_region.pma_c;
            pma_atomic == cur_region.pma_atomic;
        }
    }

    constraint c_dynamic_must_change {
        if (dynamic_mode) {
            cfg.change_satp_enable || cfg.change_vsatp_enable || cfg.change_hgatp_enable ||
            cfg.change_permission_enable || cfg.change_priv_context_enable ||
            cfg.change_pmp_pma_enable;
            (cfg.change_satp_enable &&
                (satp_mode != cur_satp_mode || satp_asid != cur_satp_asid)) ||
            (cfg.change_vsatp_enable &&
                (vsatp_mode != cur_vsatp_mode || vsatp_asid != cur_vsatp_asid)) ||
            (cfg.change_hgatp_enable &&
                (hgatp_mode != cur_hgatp_mode || hgatp_vmid != cur_hgatp_vmid)) ||
            (cfg.change_permission_enable &&
                (mxr != cur_mxr || sum != cur_sum || vmxr != cur_vmxr || vsum != cur_vsum)) ||
            (cfg.change_priv_context_enable &&
                (priv_virt != cur_priv_virt || priv_imode != cur_priv_imode ||
                 priv_dmode != cur_priv_dmode)) ||
            (cfg.change_pmp_pma_enable &&
                (pmp_r != cur_region.pmp_r || pmp_w != cur_region.pmp_w ||
                 pmp_x != cur_region.pmp_x || pma_c != cur_region.pma_c ||
                 pma_atomic != cur_region.pma_atomic));
        }
    }

    function new(string name = "memblock_csr_randomizer");
        super.new(name);
        cfg = '{default:'0};
        cur_region = '{default:'0};
    endfunction:new

    function void configure(input memblock_csr_sequence_cfg_t cfg_i,
                            input bit dynamic_i,
                            input csr_ctrl_agent_agent_xaction current,
                            input memblock_csr_pmp_pma_profile_t current_region);
        cfg = cfg_i;
        dynamic_mode = dynamic_i;
        cur_region = current_region;
        if (dynamic_mode && current == null)
            `uvm_fatal(get_type_name(), "dynamic CSR randomizer requires committed state")
        if (current == null) return;
        cur_satp_mode = current.io_ooo_to_mem_tlbCsr_satp_mode;
        cur_satp_asid = current.io_ooo_to_mem_tlbCsr_satp_asid;
        cur_vsatp_mode = current.io_ooo_to_mem_tlbCsr_vsatp_mode;
        cur_vsatp_asid = current.io_ooo_to_mem_tlbCsr_vsatp_asid;
        cur_hgatp_mode = current.io_ooo_to_mem_tlbCsr_hgatp_mode;
        cur_hgatp_vmid = current.io_ooo_to_mem_tlbCsr_hgatp_vmid;
        cur_mxr = current.io_ooo_to_mem_tlbCsr_priv_mxr;
        cur_sum = current.io_ooo_to_mem_tlbCsr_priv_sum;
        cur_vmxr = current.io_ooo_to_mem_tlbCsr_priv_vmxr;
        cur_vsum = current.io_ooo_to_mem_tlbCsr_priv_vsum;
        cur_priv_virt = current.io_ooo_to_mem_tlbCsr_priv_virt;
        cur_priv_imode = current.io_ooo_to_mem_tlbCsr_priv_imode;
        cur_priv_dmode = current.io_ooo_to_mem_tlbCsr_priv_dmode;
        cur_l1d_pf = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
        cur_l1d_agt = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
        cur_l1d_pht = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
        cur_l1d_stride = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
        cur_l2_master = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
        cur_l2_recv = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
        cur_l2_pbop = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
        cur_l2_vbop = current.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
        cur_ldld = current.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
        cur_cache_error = current.io_ooo_to_mem_csrCtrl_cache_error_enable;
        cur_uncache = current.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
        cur_misalign_ld = current.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
        cur_misalign_st = current.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
    endfunction:configure

    function void apply_to_transaction(input csr_ctrl_agent_agent_xaction tr);
        if (tr == null) `uvm_fatal(get_type_name(), "cannot apply random result to null transaction")
        tr.io_ooo_to_mem_tlbCsr_satp_mode = satp_mode;
        tr.io_ooo_to_mem_tlbCsr_satp_asid = satp_asid;
        tr.io_ooo_to_mem_tlbCsr_vsatp_mode = vsatp_mode;
        tr.io_ooo_to_mem_tlbCsr_vsatp_asid = vsatp_asid;
        tr.io_ooo_to_mem_tlbCsr_hgatp_mode = hgatp_mode;
        tr.io_ooo_to_mem_tlbCsr_hgatp_vmid = hgatp_vmid;
        tr.io_ooo_to_mem_tlbCsr_priv_mxr = mxr;
        tr.io_ooo_to_mem_tlbCsr_priv_sum = sum;
        tr.io_ooo_to_mem_tlbCsr_priv_vmxr = vmxr;
        tr.io_ooo_to_mem_tlbCsr_priv_vsum = vsum;
        tr.io_ooo_to_mem_tlbCsr_priv_virt = priv_virt;
        tr.io_ooo_to_mem_tlbCsr_priv_imode = priv_imode;
        tr.io_ooo_to_mem_tlbCsr_priv_dmode = priv_dmode;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable = l1d_pf;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt = l1d_agt;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht = l1d_pht;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride = l1d_stride;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable = l2_master;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable = l2_recv;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable = l2_pbop;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable = l2_vbop;
        tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable = ldld;
        tr.io_ooo_to_mem_csrCtrl_cache_error_enable = cache_error;
        tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable = uncache;
        tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable = misalign_ld;
        tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable = misalign_st;
        memblock_csr_config_state::apply_fixed_roots(tr);
    endfunction:apply_to_transaction

    function memblock_csr_pmp_pma_profile_t make_region_profile(
        input bit [63:0] normal_base, input bit [63:0] normal_range);
        memblock_csr_pmp_pma_profile_t profile;
        profile = '{default:'0};
        profile.valid = 1'b1;
        profile.exception_enable = cfg.pmp_pma_exception_enable;
        profile.normal_base = normal_base;
        profile.normal_range = normal_range;
        profile.exception_base = cfg.pmp_pma_exception_base;
        profile.exception_range = cfg.pmp_pma_exception_range;
        profile.pmp_r = pmp_r; profile.pmp_w = pmp_w; profile.pmp_x = pmp_x;
        profile.pma_c = pma_c; profile.pma_atomic = pma_atomic;
        check_region_profile(profile);
        return profile;
    endfunction:make_region_profile

    function int unsigned get_command_line_seed();
        int unsigned seed;
        seed = 0;
        void'($value$plusargs("ntb_random_seed=%d", seed));
        return seed;
    endfunction:get_command_line_seed

    function void log_enable_field(input string field_name,
                                   input bit requested_value,
                                   input string rtl_status,
                                   input bit design_reset_value,
                                   input bit dependency_ok,
                                   input string owner,
                                   input int unsigned seed);
        `uvm_info("MEMBLOCK_CSR_ENABLE_AUDIT",
                  $sformatf("field=%s requested=%0b rtl_status=%s design_reset=%0b dependency=%s payload_version=full92_v1 owner=%s random_seed=%0d",
                            field_name, requested_value, rtl_status, design_reset_value,
                            dependency_ok ? "pass" : "fail", owner, seed), UVM_LOW)
    endfunction:log_enable_field

    // 中文注释：该审计只报告已经完成合法性检查和单次求解的最终 profile，
    // 不读取指令属性或翻译路径。initial 额外逐项打印 30 个 enable 字段；
    // dynamic 只打印冻结 enable 的汇总和本次 owner，避免重复 30 行日志。
    function void log_selected_profile(input string phase_name,
                                       input string owner,
                                       input csr_ctrl_agent_agent_xaction tr,
                                       input memblock_csr_pmp_pma_profile_t profile,
                                       input bit log_enable_fields);
        int unsigned seed;
        bit dependency_ok;
        if (tr == null)
            `uvm_fatal(get_type_name(), "cannot audit a null CSR transaction")
        seed = get_command_line_seed();
        `uvm_info("MEMBLOCK_CSR_PROFILE_AUDIT",
                  $sformatf("phase=%s constraint_version=memblock_csr_v1 payload_version=full92_v1 owner=%s random_seed=%0d constraint_solve_ok=1",
                            phase_name, owner, seed), UVM_LOW)
        `uvm_info("MEMBLOCK_CSR_PROFILE_AUDIT",
                  $sformatf("phase=%s selected_atp_csr_values=satp(%0d,0x%0h,0x%0h),vsatp(%0d,0x%0h,0x%0h),hgatp(%0d,0x%0h,0x%0h) fixed_root_ppn_set={0x80000,0x80004,0x80008}",
                            phase_name,
                            tr.io_ooo_to_mem_tlbCsr_satp_mode,
                            tr.io_ooo_to_mem_tlbCsr_satp_asid,
                            tr.io_ooo_to_mem_tlbCsr_satp_ppn,
                            tr.io_ooo_to_mem_tlbCsr_vsatp_mode,
                            tr.io_ooo_to_mem_tlbCsr_vsatp_asid,
                            tr.io_ooo_to_mem_tlbCsr_vsatp_ppn,
                            tr.io_ooo_to_mem_tlbCsr_hgatp_mode,
                            tr.io_ooo_to_mem_tlbCsr_hgatp_vmid,
                            tr.io_ooo_to_mem_tlbCsr_hgatp_ppn), UVM_LOW)
        `uvm_info("MEMBLOCK_CSR_PROFILE_AUDIT",
                  $sformatf("phase=%s selected_permission_csr_values=mxr:%0b,sum:%0b,vmxr:%0b,vsum:%0b selected_priv_context=virt:%0b,imode:%0b,dmode:%0b",
                            phase_name,
                            tr.io_ooo_to_mem_tlbCsr_priv_mxr,
                            tr.io_ooo_to_mem_tlbCsr_priv_sum,
                            tr.io_ooo_to_mem_tlbCsr_priv_vmxr,
                            tr.io_ooo_to_mem_tlbCsr_priv_vsum,
                            tr.io_ooo_to_mem_tlbCsr_priv_virt,
                            tr.io_ooo_to_mem_tlbCsr_priv_imode,
                            tr.io_ooo_to_mem_tlbCsr_priv_dmode), UVM_LOW)
        `uvm_info("MEMBLOCK_CSR_PROFILE_AUDIT",
                  $sformatf("phase=%s normal_region_snapshot=base:0x%0h,range:0x%0h,lower:0x%0h,top:0x%0h exception_region_snapshot=enable:%0b,base:0x%0h,range:0x%0h,lower:0x%0h,top:0x%0h region_entry_snapshot=normal:OFF0/TOR1,exception:OFF2/TOR3,pmp_rwx:%0b%0b%0b,pma_c_atomic:%0b%0b",
                            phase_name,
                            profile.normal_base, profile.normal_range,
                            profile.normal_base, profile.normal_base + profile.normal_range,
                            profile.exception_enable, profile.exception_base,
                            profile.exception_range, profile.exception_base,
                            profile.exception_base + profile.exception_range,
                            profile.pmp_r, profile.pmp_w, profile.pmp_x,
                            profile.pma_c, profile.pma_atomic), UVM_LOW)
        if (!dynamic_mode) begin
            `uvm_info("MEMBLOCK_CSR_WEIGHT_AUDIT",
                      $sformatf("phase=%s csr_weight_snapshot=atp_init:{satp:%0d/%0d/%0d,vsatp:%0d/%0d/%0d,hgatp:%0d/%0d/%0d};permission:{mxr:%0d/%0d,sum:%0d/%0d,vmxr:%0d/%0d,vsum:%0d/%0d};priv:{virt:%0d/%0d,imode:%0d/%0d/%0d,dmode:%0d/%0d/%0d}",
                                phase_name,
                                cfg.init_satp_bare_wt, cfg.init_satp_sv39_wt, cfg.init_satp_sv48_wt,
                                cfg.init_vsatp_bare_wt, cfg.init_vsatp_sv39_wt, cfg.init_vsatp_sv48_wt,
                                cfg.init_hgatp_bare_wt, cfg.init_hgatp_sv39x4_wt, cfg.init_hgatp_sv48x4_wt,
                                cfg.init_mxr_0_wt, cfg.init_mxr_1_wt,
                                cfg.init_sum_0_wt, cfg.init_sum_1_wt,
                                cfg.init_vmxr_0_wt, cfg.init_vmxr_1_wt,
                                cfg.init_vsum_0_wt, cfg.init_vsum_1_wt,
                                cfg.init_priv_virt_0_wt, cfg.init_priv_virt_1_wt,
                                cfg.init_priv_imode_u_wt, cfg.init_priv_imode_s_wt, cfg.init_priv_imode_m_wt,
                                cfg.init_priv_dmode_u_wt, cfg.init_priv_dmode_s_wt, cfg.init_priv_dmode_m_wt), UVM_LOW)
            `uvm_info("MEMBLOCK_CSR_WEIGHT_AUDIT",
                      $sformatf("phase=%s csr_weight_snapshot=enable:{l1d:%0d/%0d,agt:%0d/%0d,pht:%0d/%0d,stride:%0d/%0d,l2:%0d/%0d,recv:%0d/%0d,pbop:%0d/%0d,vbop:%0d/%0d,ldld:%0d/%0d,cache_error:%0d/%0d,uncache:%0d/%0d,misalign_ld:%0d/%0d,misalign_st:%0d/%0d}",
                                phase_name,
                                cfg.enable_l1d_pf_0_wt, cfg.enable_l1d_pf_1_wt,
                                cfg.enable_l1d_pf_agt_0_wt, cfg.enable_l1d_pf_agt_1_wt,
                                cfg.enable_l1d_pf_pht_0_wt, cfg.enable_l1d_pf_pht_1_wt,
                                cfg.enable_l1d_pf_stride_0_wt, cfg.enable_l1d_pf_stride_1_wt,
                                cfg.enable_l2_pf_master_0_wt, cfg.enable_l2_pf_master_1_wt,
                                cfg.enable_l2_pf_recv_0_wt, cfg.enable_l2_pf_recv_1_wt,
                                cfg.enable_l2_pf_pbop_0_wt, cfg.enable_l2_pf_pbop_1_wt,
                                cfg.enable_l2_pf_vbop_0_wt, cfg.enable_l2_pf_vbop_1_wt,
                                cfg.enable_ldld_vio_0_wt, cfg.enable_ldld_vio_1_wt,
                                cfg.enable_cache_error_0_wt, cfg.enable_cache_error_1_wt,
                                cfg.enable_uncache_outstanding_0_wt, cfg.enable_uncache_outstanding_1_wt,
                                cfg.enable_misalign_ld_0_wt, cfg.enable_misalign_ld_1_wt,
                                cfg.enable_misalign_st_0_wt, cfg.enable_misalign_st_1_wt), UVM_LOW)
        end else begin
            `uvm_info("MEMBLOCK_CSR_WEIGHT_AUDIT",
                      $sformatf("phase=%s csr_weight_snapshot=change_enable:{satp:%0b,vsatp:%0b,hgatp:%0b,permission:%0b,priv:%0b,pmp_pma:%0b};atp_change:{satp:%0d/%0d/%0d,vsatp:%0d/%0d/%0d,hgatp:%0d/%0d/%0d};permission:{mxr:%0d/%0d,sum:%0d/%0d,vmxr:%0d/%0d,vsum:%0d/%0d};priv:{virt:%0d/%0d,imode:%0d/%0d/%0d,dmode:%0d/%0d/%0d}",
                                phase_name,
                                cfg.change_satp_enable, cfg.change_vsatp_enable,
                                cfg.change_hgatp_enable, cfg.change_permission_enable,
                                cfg.change_priv_context_enable, cfg.change_pmp_pma_enable,
                                cfg.change_satp_bare_wt, cfg.change_satp_sv39_wt, cfg.change_satp_sv48_wt,
                                cfg.change_vsatp_bare_wt, cfg.change_vsatp_sv39_wt, cfg.change_vsatp_sv48_wt,
                                cfg.change_hgatp_bare_wt, cfg.change_hgatp_sv39x4_wt, cfg.change_hgatp_sv48x4_wt,
                                cfg.change_mxr_0_wt, cfg.change_mxr_1_wt,
                                cfg.change_sum_0_wt, cfg.change_sum_1_wt,
                                cfg.change_vmxr_0_wt, cfg.change_vmxr_1_wt,
                                cfg.change_vsum_0_wt, cfg.change_vsum_1_wt,
                                cfg.change_priv_virt_0_wt, cfg.change_priv_virt_1_wt,
                                cfg.change_priv_imode_u_wt, cfg.change_priv_imode_s_wt, cfg.change_priv_imode_m_wt,
                                cfg.change_priv_dmode_u_wt, cfg.change_priv_dmode_s_wt, cfg.change_priv_dmode_m_wt), UVM_LOW)
        end
        `uvm_info("MEMBLOCK_CSR_WEIGHT_AUDIT",
                  $sformatf("phase=%s csr_weight_snapshot=region:{enable:%0b,pmp_r:%0d/%0d,pmp_w:%0d/%0d,pmp_x:%0d/%0d,pma_c:%0d/%0d,pma_atomic:%0d/%0d}",
                            phase_name, cfg.pmp_pma_exception_enable,
                            cfg.pmp_exception_r_0_wt, cfg.pmp_exception_r_1_wt,
                            cfg.pmp_exception_w_0_wt, cfg.pmp_exception_w_1_wt,
                            cfg.pmp_exception_x_0_wt, cfg.pmp_exception_x_1_wt,
                            cfg.pma_exception_c_0_wt, cfg.pma_exception_c_1_wt,
                            cfg.pma_exception_atomic_0_wt, cfg.pma_exception_atomic_1_wt), UVM_LOW)

        if (!log_enable_fields) return;
        log_enable_field("pf_ctrl_l1I_pf_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable,
                         owner, seed);
        log_enable_field("pf_ctrl_l2_pf_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable,
                         "supported", 1'b1, 1'b1, owner, seed);
        log_enable_field("pf_ctrl_l1D_pf_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable,
                         "supported", 1'b1, 1'b1, owner, seed);
        dependency_ok = !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt ||
                        (tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable &&
                         tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable);
        log_enable_field("pf_ctrl_l1D_pf_enable_agt", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt,
                         "supported", 1'b1, dependency_ok, owner, seed);
        dependency_ok = !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht ||
                        (tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable &&
                         tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable);
        log_enable_field("pf_ctrl_l1D_pf_enable_pht", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht,
                         "supported", 1'b1, dependency_ok, owner, seed);
        dependency_ok = !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride ||
                        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
        log_enable_field("pf_ctrl_l1D_pf_enable_stride", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride,
                         "supported", 1'b1, dependency_ok, owner, seed);
        dependency_ok = !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable ||
                        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
        log_enable_field("pf_ctrl_l2_pf_recv_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable,
                         "supported", 1'b1, dependency_ok, owner, seed);
        dependency_ok = !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable ||
                        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
        log_enable_field("pf_ctrl_l2_pf_pbop_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable,
                         "supported", 1'b1, dependency_ok, owner, seed);
        dependency_ok = !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable ||
                        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
        log_enable_field("pf_ctrl_l2_pf_vbop_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable,
                         "supported", 1'b1, dependency_ok, owner, seed);
        log_enable_field("pf_ctrl_l2_pf_tp_enable", tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable,
                         "no_top_port", 1'b0, !tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable,
                         owner, seed);
        log_enable_field("ldld_vio_check_enable", tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable,
                         "supported", 1'b1, 1'b1, owner, seed);
        log_enable_field("cache_error_enable", tr.io_ooo_to_mem_csrCtrl_cache_error_enable,
                         "supported", 1'b1, 1'b1, owner, seed);
        log_enable_field("uncache_write_outstanding_enable", tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable,
                         "supported", 1'b0, 1'b1, owner, seed);
        log_enable_field("power_down_enable", tr.io_ooo_to_mem_csrCtrl_power_down_enable,
                         "action_scoped", 1'b0, !tr.io_ooo_to_mem_csrCtrl_power_down_enable,
                         owner, seed);
        log_enable_field("flush_l2_enable", tr.io_ooo_to_mem_csrCtrl_flush_l2_enable,
                         "action_scoped", 1'b0, !tr.io_ooo_to_mem_csrCtrl_flush_l2_enable,
                         owner, seed);
        log_enable_field("frontend_trigger_tEnableVec_0", tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0, owner, seed);
        log_enable_field("frontend_trigger_tEnableVec_1", tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1, owner, seed);
        log_enable_field("frontend_trigger_tEnableVec_2", tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2, owner, seed);
        log_enable_field("frontend_trigger_tEnableVec_3", tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3, owner, seed);
        log_enable_field("mem_trigger_tEnableVec_0", tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0,
                         "payload_scoped", 1'b0, !tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0, owner, seed);
        log_enable_field("mem_trigger_tEnableVec_1", tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1,
                         "payload_scoped", 1'b0, !tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1, owner, seed);
        log_enable_field("mem_trigger_tEnableVec_2", tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2,
                         "payload_scoped", 1'b0, !tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2, owner, seed);
        log_enable_field("mem_trigger_tEnableVec_3", tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3,
                         "payload_scoped", 1'b0, !tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3, owner, seed);
        log_enable_field("bp_ctrl_btb_enable", tr.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable, owner, seed);
        log_enable_field("bp_ctrl_ras_enable", tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable, owner, seed);
        log_enable_field("bp_ctrl_sc_enable", tr.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable, owner, seed);
        log_enable_field("bp_ctrl_tage_enable", tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable, owner, seed);
        log_enable_field("bp_ctrl_ubtb_enable", tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable,
                         "unused", 1'b0, !tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable, owner, seed);
        log_enable_field("hd_misalign_ld_enable", tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable,
                         "supported", 1'b1, 1'b1, owner, seed);
        log_enable_field("hd_misalign_st_enable", tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable,
                         "supported", 1'b1, 1'b1, owner, seed);
    endfunction:log_selected_profile

    // 中文注释：在任何 CSR level item 发出前校验 semantic region。正常区必须覆盖
    // 三个固定分页根；例外区必须位于平台 PMEM 内且与正常区不重叠。该检查不修改
    // profile，失败直接终止，避免非法范围只在 monitor/model 回放阶段暴露。
    function void check_region_profile(input memblock_csr_pmp_pma_profile_t profile);
        bit [63:0] normal_top;
        bit [63:0] exception_top;
        if (!profile.valid || profile.normal_range == 0 ||
            profile.normal_base[11:0] != 0 || profile.normal_range[11:0] != 0 ||
            profile.normal_base + profile.normal_range < profile.normal_base ||
            profile.normal_base > 64'h0000_ffff_ffff_ffff ||
            profile.normal_base + profile.normal_range > 64'h0001_0000_0000_0000 ||
            profile.normal_base > 64'h8000_0000 ||
            profile.normal_base + profile.normal_range < 64'h8000_9000) begin
            `uvm_fatal(get_type_name(), "invalid normal CSR PMP/PMA region")
        end
        normal_top = profile.normal_base + profile.normal_range;
        if (profile.exception_enable) begin
            exception_top = profile.exception_base + profile.exception_range;
            if (profile.exception_range == 0 || profile.exception_base[11:0] != 0 ||
                profile.exception_range[11:0] != 0 || exception_top < profile.exception_base ||
                profile.exception_base < 64'h8000_0000 ||
                exception_top > 64'h0000_0800_0000_0000 ||
                !(exception_top <= profile.normal_base || profile.exception_base >= normal_top)) begin
                `uvm_fatal(get_type_name(), "invalid or overlapping exception CSR PMP/PMA region")
            end
        end
    endfunction:check_region_profile
endclass:memblock_csr_randomizer

class memblock_csr_pmp_pma_write_plan extends uvm_object;
    localparam bit [11:0] PMPCFG0 = 12'h3a0;
    localparam bit [11:0] PMPADDR0 = 12'h3b0;
    localparam bit [11:0] PMACFG0 = 12'h7c0;
    localparam bit [11:0] PMAADDR0 = 12'h7c8;
    memblock_csr_write_beat_t beats[$];
    `uvm_object_utils(memblock_csr_pmp_pma_write_plan)

    function new(string name = "memblock_csr_pmp_pma_write_plan");
        super.new(name);
    endfunction:new
    function void push(input bit [11:0] addr, input bit [63:0] data);
        memblock_csr_write_beat_t beat;
        beat = '{valid:1'b1, addr:addr, data:data};
        beats.push_back(beat);
    endfunction:push
    function bit [7:0] pack_cfg(input bit c, input bit atomic,
                                input bit [1:0] a, input bit x,
                                input bit w, input bit r);
        return {1'b0, c, atomic, a, x, w, r};
    endfunction:pack_cfg

    function bit [7:0] pack_entry(input pma_pmp_entry_t entry);
        return {entry.lock, entry.c, entry.atomic, entry.a,
                entry.x, entry.w, entry.r};
    endfunction:pack_entry

    function void build(input memblock_csr_pmp_pma_profile_t profile,
                        input memblock_pma_pmp_model model);
        bit [63:0] pmp_cfg, pma_cfg;
        bit [63:0] normal_top, exception_top;
        pma_pmp_entry_t entry;
        if (!profile.valid || profile.normal_range == 0 ||
            profile.normal_base[11:0] != 0 || profile.normal_range[11:0] != 0 ||
            profile.normal_base + profile.normal_range < profile.normal_base ||
            profile.normal_base > 64'h0000_ffff_ffff_ffff ||
            profile.normal_base + profile.normal_range > 64'h0001_0000_0000_0000 ||
            profile.normal_base > 64'h8000_0000 ||
            profile.normal_base + profile.normal_range < 64'h8000_9000)
            `uvm_fatal(get_type_name(), "invalid normal PMP/PMA CSR region")
        if (model == null) `uvm_fatal(get_type_name(), "PMP/PMA write plan requires model for RMW")
        normal_top = profile.normal_base + profile.normal_range;
        exception_top = profile.exception_base + profile.exception_range;
        if (profile.exception_enable &&
            (profile.exception_range == 0 || profile.exception_base[11:0] != 0 ||
             profile.exception_range[11:0] != 0 || exception_top < profile.exception_base ||
             profile.exception_base < 64'h8000_0000 ||
             exception_top > 64'h0000_0800_0000_0000 ||
             !(exception_top <= profile.normal_base || profile.exception_base >= normal_top)))
            `uvm_fatal(get_type_name(), "invalid or overlapping exception PMP/PMA CSR region")
        beats.delete();
        pmp_cfg = '0; pma_cfg = '0;
        for (int unsigned i = 0; i < 8; i++) begin
            if (!model.read_entry(1'b0, i, entry))
                `uvm_fatal(get_type_name(), "failed to read PMP entry for cfg RMW")
            pmp_cfg[(i * 8) +: 8] = pack_entry(entry);
            if (!model.read_entry(1'b1, i, entry))
                `uvm_fatal(get_type_name(), "failed to read PMA entry for cfg RMW")
            pma_cfg[(i * 8) +: 8] = pack_entry(entry);
        end
        pmp_cfg[31:0] = '0;
        pma_cfg[31:0] = '0;
        push(PMPCFG0, pmp_cfg); push(PMACFG0, pma_cfg);
        push(PMPADDR0 + 0, profile.normal_base >> 2);
        push(PMPADDR0 + 1, normal_top >> 2);
        push(PMAADDR0 + 0, profile.normal_base >> 2);
        push(PMAADDR0 + 1, normal_top >> 2);
        if (profile.exception_enable) begin
            push(PMPADDR0 + 2, profile.exception_base >> 2);
            push(PMPADDR0 + 3, exception_top >> 2);
            push(PMAADDR0 + 2, profile.exception_base >> 2);
            push(PMAADDR0 + 3, exception_top >> 2);
        end
        // 中文注释：恢复 entry 0..3 的目标配置，同时保留 RMW 基线中的 entry 4..7。
        pmp_cfg[15:8] = pack_cfg(1'b0, 1'b0, 2'b01, 1'b1, 1'b1, 1'b1);
        pma_cfg[15:8] = pack_cfg(1'b1, 1'b1, 2'b01, 1'b1, 1'b1, 1'b1);
        if (profile.exception_enable) begin
            pmp_cfg[31:24] = pack_cfg(1'b0, 1'b0, 2'b01,
                                      profile.pmp_x, profile.pmp_w, profile.pmp_r);
            pma_cfg[31:24] = pack_cfg(profile.pma_c, profile.pma_atomic, 2'b01,
                                      profile.pmp_x, profile.pmp_w, profile.pmp_r);
        end
        push(PMPCFG0, pmp_cfg); push(PMACFG0, pma_cfg);
    endfunction:build

    function bit model_matches(input memblock_pma_pmp_model model,
                               input memblock_csr_pmp_pma_profile_t profile);
        pma_pmp_entry_t pmp0, pmp1, pmp2, pmp3;
        pma_pmp_entry_t pma0, pma1, pma2, pma3;
        if (model == null ||
            !model.read_entry(1'b0, 0, pmp0) || !model.read_entry(1'b0, 1, pmp1) ||
            !model.read_entry(1'b0, 2, pmp2) || !model.read_entry(1'b0, 3, pmp3) ||
            !model.read_entry(1'b1, 0, pma0) || !model.read_entry(1'b1, 1, pma1) ||
            !model.read_entry(1'b1, 2, pma2) || !model.read_entry(1'b1, 3, pma3)) return 1'b0;
        if (pmp0.a != PMA_PMP_A_OFF || pma0.a != PMA_PMP_A_OFF ||
            pmp1.a != PMA_PMP_A_TOR || pma1.a != PMA_PMP_A_TOR ||
            pmp0.addr_raw != profile.normal_base[47:2] ||
            pma0.addr_raw != profile.normal_base[47:2] ||
            pmp1.addr_raw != (profile.normal_base + profile.normal_range) >> 2 ||
            pma1.addr_raw != (profile.normal_base + profile.normal_range) >> 2 ||
            !pmp1.r || !pmp1.w || !pmp1.x || !pma1.r || !pma1.w || !pma1.x ||
            !pma1.c || !pma1.atomic) return 1'b0;
        if (!profile.exception_enable)
            return pmp2.a == PMA_PMP_A_OFF && pmp3.a == PMA_PMP_A_OFF &&
                   pma2.a == PMA_PMP_A_OFF && pma3.a == PMA_PMP_A_OFF;
        return pmp2.a == PMA_PMP_A_OFF && pma2.a == PMA_PMP_A_OFF &&
               pmp2.addr_raw == profile.exception_base[47:2] &&
               pma2.addr_raw == profile.exception_base[47:2] &&
               pmp3.a == PMA_PMP_A_TOR && pma3.a == PMA_PMP_A_TOR &&
               pmp3.addr_raw == (profile.exception_base + profile.exception_range) >> 2 &&
               pma3.addr_raw == (profile.exception_base + profile.exception_range) >> 2 &&
               pmp3.r == profile.pmp_r && pmp3.w == profile.pmp_w &&
               pmp3.x == profile.pmp_x && pma3.r == profile.pmp_r &&
               pma3.w == profile.pmp_w && pma3.x == profile.pmp_x &&
               pma3.c == profile.pma_c &&
               pma3.atomic == profile.pma_atomic;
    endfunction:model_matches
endclass:memblock_csr_pmp_pma_write_plan

`endif
