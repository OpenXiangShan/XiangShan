`ifndef MEMBLOCK_MAIN_DISPATCH_ROUTED_PPN_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_ROUTED_PPN_SEQUENCE__SV

// 窄窗口的偏移必须在主表发布前产生，L2TLB 接口没有页内偏移，不能事后修 VA。
class memblock_main_dispatch_routed_ppn_sequence extends
    memblock_main_dispatch_auto_build_main_table_base_sequence;
    `uvm_object_utils(memblock_main_dispatch_routed_ppn_sequence)

    function new(string name = "memblock_main_dispatch_routed_ppn_sequence");
        super.new(name);
    endfunction

    // 抽象职责：给出六个实际外部窗口，最后一个窗口显式避开内部 DCacheCtrl。
    static function void narrow_region(input int unsigned index,
                                       output bit [63:0] first,
                                       output bit [63:0] limit,
                                       output string label,
                                       output int signed region_id);
        case (index)
            0: begin first = 'h310b0000; limit = 'h310b0080; label = "UART128B"; region_id = 4; end
            1: begin first = 'h31100000; limit = 'h31108000; label = "APLIC_M"; region_id = 2; end
            2: begin first = 'h31120000; limit = 'h31128000; label = "APLIC_S"; region_id = 3; end
            3: begin first = 'h38000000; limit = 'h38010000; label = "TIMER"; region_id = 5; end
            4: begin first = 'h3c000000; limit = 'h40000000; label = "PLIC"; region_id = 6; end
            5: begin first = 'h38022080; limit = 'h38023000; label = "EXTERNAL_PARTIAL_PAGE"; region_id = 0; end
            default: begin first = 0; limit = 0; label = "INVALID"; region_id = -1; end
        endcase
    endfunction

    // 抽象职责：均衡随机目标窗口，独立 VPN 配合目标 PA 的合法低 12 位，登记全部
    // intent 后才初始化 status 并发布 ready；不调用会提前发布 ready 的父构造函数。
    virtual task build_random_main_table(input int unsigned main_trans_num_i);
        memblock_rob_key_t rob_key;
        int unsigned order[$];
        bit [63:0] va_base;
        bit [63:0] va_limit;

        if (data == null) data = common_data_transaction::get();
        data.reset_all_tables(main_trans_num_i);
        rob_key = choose_rob_start_key();
        va_base = (seq_csr_common::get_main_vaddr_base() + 4095) & ~64'hfff;
        va_limit = seq_csr_common::get_main_vaddr_base() + seq_csr_common::get_main_vaddr_range();
        if (main_trans_num_i < 6 || va_limit < va_base ||
            va_base + (64'(main_trans_num_i) << 12) > va_limit ||
            va_limit > 64'h0000_0040_0000_0000) begin
            `uvm_fatal(get_type_name(), "routed PPN narrow smoke needs >=6 unique Sv39 positive VPNs inside MAIN_VADDR")
        end
        for (int unsigned idx = 0; idx < main_trans_num_i; idx++) begin
            memblock_uid_t uid;
            main_control_transaction tr;
            memblock_routed_ppn_intent_t intent;
            bit [63:0] first, limit, pa, page_first, page_limit, offset_first, offset_limit;
            int signed region_id;
            string label;
            int unsigned target;

            if (order.size() == 0) begin
                for (int unsigned n = 0; n < 6; n++) order.push_back(n);
                order.shuffle();
            end
            target = order.pop_front();
            narrow_region(target, first, limit, label, region_id);
            pa = first + (64'($urandom_range(int'((limit-first)/8)-1, 0)) << 3);
            page_first = pa & ~64'hfff;
            page_limit = page_first + 4096;
            offset_first = ((first > page_first) ? first : page_first) - page_first;
            offset_limit = ((limit < page_limit) ? limit : page_limit) - page_first;
            uid = data.alloc_uid();
            tr = main_control_transaction::type_id::create($sformatf("routed_narrow_uid_%0d", uid));
            tr.uid = uid;
            tr.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuType = MEMBLOCK_FUTYPE_LDU;
            tr.fuOpType = MEMBLOCK_LSUOP_LD;
            tr.numLsElem = memblock_num_ls_elem_t'(1);
            tr.robIdx_flag = rob_key.flag;
            tr.robIdx_value = rob_key.value;
            tr.src_0 = va_base + (64'(idx) << 12) + pa[11:0];
            tr.imm = 0;
            tr.boundary_profile = MEMBLOCK_BOUNDARY_PROFILE_ALIGNED;
            tr.boundary_size_bytes = 8;
            tr.send_pri = 50;
            tr.send_pri_std = 50;
            tr.update_vaddr();
            validate_main_table_entry(tr, $sformatf("narrow uid=%0d", uid));
            intent = '{default:'0};
            intent.valid = 1;
            intent.ppn = 44'(pa >> 12);
            intent.first_offset = 12'(offset_first);
            intent.last_offset = 12'(offset_limit - 1);
            intent.region_id = region_id;
            if (!data.register_routed_ppn_intent(tr.vaddr[49:12], intent)) begin
                `uvm_fatal(get_type_name(), $sformatf("narrow intent registration failed uid=%0d", uid))
            end
            data.set_main_transaction(uid, tr);
            `uvm_info("ROUTED_PPN_NARROW_INPUT", $sformatf("uid=%0d window=%s va=0x%0h pa=0x%0h allowed_offsets=0x%0h..0x%0h", uid, label, tr.vaddr, pa, intent.first_offset, intent.last_offset), UVM_LOW)
            rob_key = rob_order_util::rob_advance(rob_key, 1);
        end
        init_status_for_main_table();
        data.check_main_table_complete();
    endtask
endclass
`endif
