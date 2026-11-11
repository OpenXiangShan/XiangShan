`ifndef TC_RM_CMO_UNCACHE_FINAL_SV
`define TC_RM_CMO_UNCACHE_FINAL_SV
// 中文注释：只扩展验收阶段，继承basicTest原有VSEQ/CSR/responder拓扑。
// 静态Sv39用例没有尾CHECK_STORE控制marker；本专项仅在确认所有写入均走Uncache且已drain后
// 复用现有Store-table比较函数，不放宽生产RM的cacheable overlay读取门槛。
class tc_rm_cmo_uncache_final extends basicTest;
    `uvm_component_utils(tc_rm_cmo_uncache_final)
    function new(string name="tc_rm_cmo_uncache_final",uvm_component parent=null);
        super.new(name,parent);
    endfunction
    // 抽象职责：在真实Uncache写链结束后，把已有expected entry与只读实际overlay逐byte比较。
    virtual function void check_phase(uvm_phase phase);
        memblock_rm_readonly_api ro;
        memblock_rm_observer_cache::store_final_entry_view_t actual_byte;
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_ls_store_cache_entry_t entry;
        bit [127:0] actual_data,expected_data;
        bit [15:0] actual_valid_mask,actual_corrupt_mask;
        int unsigned zero_count,entry_count,checked_count;
        bit pass;
        super.check_phase(phase);
        if(env==null || env.rm==null || env.rm.ls_model==null) begin
            `uvm_error("CMO_UNCACHE_FINAL","最终Store-table检查缺少真实env/RM模型")
            return;
        end
        model=env.rm.ls_model;
        if(!model.configured || model.current_uid!=model.main_count || model.main_count==0) begin
            `uvm_error("CMO_UNCACHE_FINAL",$sformatf("程序尚未完整比较 configured=%0d uid=%0d count=%0d",
                model.configured,model.current_uid,model.main_count))
            return;
        end
        if(!memblock_sync_pkg::sbuffer_responder_done || !memblock_sync_pkg::dcache_responder_done) begin
            `uvm_error("CMO_UNCACHE_FINAL","最终Store-table读取前真实memory responder尚未drain")
            return;
        end
        zero_count=0; pass=1;
        // 终场只扫描完整item集合一次；先检查支持范围，不能读完数据后再猜路径。
        foreach(model.items[uid]) begin
            item=model.items[uid];
            if(item==null || !item.valid) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("uid=%0d缺少合法最终item",uid))
                pass=0; continue;
            end
            if(item.kind==RM_LS_KIND_STORE ||
               (item.kind==RM_LS_KIND_ATOMIC && item.expected_memory_write) ||
               (item.kind==RM_LS_KIND_CMO && item.cmo_op==RM_CMO_OP_ZERO)) begin
                if(!item.uncache_path) begin
                    `uvm_error("CMO_UNCACHE_FINAL",$sformatf("uid=%0d writer并非Uncache，禁止绕过普通cacheable比较门槛",uid))
                    pass=0;
                end
            end
            if(item.kind==RM_LS_KIND_CMO && item.cmo_op==RM_CMO_OP_ZERO) begin
                zero_count++;
                if(item.cmo_path!=RM_CMO_PATH_UNCACHE_ZERO || !item.cmo_completion_consumed ||
                   !item.cmo_requests_validated || !item.cmo_effect_valid || !item.cmo_effect_applied) begin
                    `uvm_error("CMO_UNCACHE_FINAL",$sformatf("uid=%0d zero分段/终态/唯一效果尚未完成",uid))
                    pass=0;
                end
            end
        end
        if(zero_count==0) begin
            `uvm_error("CMO_UNCACHE_FINAL","专项没有真实Uncache zero指令")
            pass=0;
        end
        if(model.sc_no_write_obligation_count()!=0) begin
            `uvm_error("CMO_UNCACHE_FINAL","Uncache zero专项存在SC无写义务，超出本测试最终比较范围")
            pass=0;
        end
        if(!pass) return;
        ro=memblock_rm_readonly_api::get();
        entry_count=model.expected_store_entry_count(); checked_count=0;
        if(entry_count==0) begin
            `uvm_error("CMO_UNCACHE_FINAL","Uncache zero专项没有任何已接受写效果entry")
            return;
        end
        for(int unsigned index=0;index<entry_count;index++) begin
            if(!model.read_expected_store_entry(index,entry)) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("expected entry %0d不可读",index))
                pass=0; continue;
            end
            if(!model.items.exists(entry.uid) || model.items[entry.uid]==null ||
               !model.items[entry.uid].uncache_path) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("entry %0d writer uid=%0d不是已验证Uncache路径",index,entry.uid))
                pass=0; continue;
            end
            actual_data=0; actual_valid_mask=0; actual_corrupt_mask=0;
            for(int unsigned byte_index=0;byte_index<entry.size_bytes;byte_index++) begin
                actual_byte='0;
                if(ro.read_store_final_for_rm(entry.pa_by_byte[byte_index],actual_byte) &&
                   actual_byte.valid && actual_byte.pa==entry.pa_by_byte[byte_index]) begin
                    actual_data[8*byte_index+:8]=actual_byte.value;
                    actual_valid_mask[byte_index]=actual_byte.byte_valid;
                    actual_corrupt_mask[byte_index]=actual_byte.corrupt;
                end
            end
            if(!model.compare_overlay_store_entry(entry,actual_valid_mask,actual_corrupt_mask,actual_data,expected_data)) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("entry=%0d effect_uid=%0d first_pa=0x%0h actual=0x%0h expected=0x%0h reason=%s",
                    index,entry.uid,entry.pa_by_byte[0],actual_data,expected_data,model.last_error_text))
                pass=0;
            end else begin
                checked_count++;
                `uvm_info("CMO_UNCACHE_FINAL",$sformatf("STORE_COMPARE PASS entry=%0d uid=%0d size=%0d PA=0x%0h data=0x%0h",
                    index,entry.uid,entry.size_bytes,entry.pa_by_byte[0],actual_data),UVM_LOW)
            end
        end
        if(pass && checked_count==entry_count)
            `uvm_info("CMO_UNCACHE_FINAL",$sformatf("Uncache zero最终Store-table比较通过 zero_count=%0d entries=%0d",zero_count,checked_count),UVM_LOW)
    endfunction
endclass
`endif
