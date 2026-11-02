`ifndef SOFT_TEST_L2TLB_ROUTED_PPN_SEQUENCE__SV
`define SOFT_TEST_L2TLB_ROUTED_PPN_SEQUENCE__SV

// 软件专项只构造输入与调用公共 API，不驱动接口、不声称 DUT 地址覆盖。
class soft_test_l2tlb_routed_ppn_sequence extends memblock_dispatch_base_sequence;
    `uvm_object_utils(soft_test_l2tlb_routed_ppn_sequence)
    memblock_routed_ppn_policy policy;
    longint unsigned generation;

    function new(string name = "soft_test_l2tlb_routed_ppn_sequence");
        super.new(name);
    endfunction

    function void check(input bit ok, input string message);
        if (!ok) `uvm_fatal("ROUTED_PPN_SOFT", message)
    endfunction

    // 抽象职责：先独立完成带output的查询，再判断结果。不同地址复用output临时量
    // 时不得把调用串在同一个逻辑表达式内，否则求值顺序可能掩盖真实边界。
    function void expect_cacheability(input bit [63:0] paddr,
                                     input bit expected_c,
                                     input string message);
        bit query_ok;
        bit actual_c;
        query_ok = policy.query_c(paddr, actual_c);
        `uvm_info("ROUTED_PPN_PMA_QUERY", $sformatf("%s generation=%0d PA=0x%0h query_ok=%0b actual_c=%0b expected_c=%0b",
                  message, policy.pma_generation, paddr, query_ok, actual_c, expected_c), UVM_LOW)
        check(query_ok && actual_c == expected_c, message);
    endfunction

    // 软件夹具保留公共API要求的非零容量，但不发布或发射任何主表事务。
    function void reset_fixture();
        data.reset_all_tables(1);
        data.main_trans_num = 0;
    endfunction

    // 抽象职责：从公共模型取得已提交版本，每项检查独立创建策略快照。
    function void configure_current();
        longint unsigned pmp_generation;
        data.pma_pmp_model.get_current_generations(pmp_generation, generation);
        policy = memblock_routed_ppn_policy::type_id::create("routed_policy_soft");
        check(policy.configure(data.pma_pmp_model, generation, 0), "current PMA version must configure");
    endfunction

    // 抽象职责：独立检查静态路由边界，不把 APLIC 粗范围或内部控制器当外部目标。
    function void check_route_boundaries();
        check(memblock_routed_ppn_policy::is_external_span('h31100000, 'h31108000), "APLIC M full span");
        check(memblock_routed_ppn_policy::is_external_span('h31120000, 'h31128000), "APLIC S full span");
        check(!memblock_routed_ppn_policy::is_external_span('h31108000, 'h31108001), "first APLIC hole start");
        check(!memblock_routed_ppn_policy::is_external_span('h3111ffff, 'h31120000), "first APLIC hole end");
        check(!memblock_routed_ppn_policy::is_external_span('h31128000, 'h31128001), "second APLIC hole start");
        check(!memblock_routed_ppn_policy::is_external_span('h311368c0, 'h311368c8), "historical B08 hole");
        check(!memblock_routed_ppn_policy::is_external_span('h3113ffff, 'h31140000), "second APLIC hole end");
        check(!memblock_routed_ppn_policy::is_external_span('h38022000, 'h38022080), "internal DCacheCtrl excluded");
        check(memblock_routed_ppn_policy::is_external_span('h38022080, 'h38023000), "external partial page included");
        check(!memblock_routed_ppn_policy::is_external_span('h38022000, 'h38023000), "generic whole partial page rejected");
        check(memblock_routed_ppn_policy::is_external_span(64'hffff_ffff_f000, 64'h1_0000_0000_0000), "49-bit exclusive PA limit retained");
        check(!memblock_routed_ppn_policy::is_external_span(64'h1_0000_0000_0000, 64'h1_0000_0000_0001), "overwidth address rejected before truncation");
    endfunction

    // 抽象职责：对保留、重选、完整页形状以及无候选分支提供可重复断言。
    function void check_policy_selection();
        bit [43:0] selected, candidate;
        bit cacheable;
        int signed region;
        int unsigned shapes[5] = '{12,16,21,30,39};
        bit [48:0] first, limit;

        configure_current();
        candidate = 44'('h20001234000 >> 12);
        check(policy.query_c('h20001234000, cacheable) && cacheable, "wide DRAM candidate PMA c1");
        check(policy.select_ppn(candidate, candidate, 12, 0, 100, -1, selected, region) &&
              selected == candidate, "PBMT00+c1 retains original PPN outside legacy 512GiB window");
        candidate = 44'h31136;
        check(policy.select_ppn(candidate, candidate, 12, 0, 0, -1, selected, region), "B08 hole must reselect even with random weight zero");
        check(policy.check_mapping(selected, 12, 0, 0), "B08 replacement retains PMA c0 class");
        check(policy.query_c({8'b0,selected,12'b0}, cacheable) && !cacheable, "PMA MMIO must not silently become cacheable DRAM");
        check(policy.select_ppn(44'h31100, 44'h31100, 12, 0, 0, -1, selected, region) &&
              selected == 44'h31100, "valid PMA MMIO retained with weight zero");
        for (int unsigned pbmt = 1; pbmt <= 2; pbmt++) begin
            for (int signed id = 0; id <= 6; id++) begin
                check(policy.select_ppn(44'h31136,44'h31136,12,pbmt[1:0],100,id,selected,region),
                    $sformatf("NC/IO named region selection pbmt=%0d id=%0d",pbmt,id));
                check(region == id && memblock_routed_ppn_policy::get_region(id,first,limit) &&
                      {8'b0,selected,12'b0} >= first && {8'b0,selected,12'b0}+4096 <= limit &&
                      policy.check_mapping(selected,12,pbmt[1:0],0), "selected full 4K mapping must fit selected region and route");
            end
        end
        foreach (shapes[i]) begin
            check(policy.select_ppn(44'h31136,44'h31136,shapes[i],1,100,1,selected,region) &&
                  policy.check_mapping(selected,shapes[i],1,0), $sformatf("full shape shift=%0d in DRAM",shapes[i]));
        end
        check(!policy.select_ppn(44'h31136,44'h31136,21,1,100,2,selected,region), "2MiB cannot fit APLIC M 32KiB");
        check(!policy.select_ppn(44'h31136,44'h31136,12,3,100,-1,selected,region), "PBMT11 is not normal NC/IO");
        check(!policy.check_mapping(44'h38022,12,1,0), "reuse cannot target entire internal-controller page");
        candidate = 44'h1000000000;
        check(!policy.check_mapping(candidate,12,1,0), "PPN over48-bit PA cannot truncate to a legal page");
        check(policy.select_ppn(candidate,candidate,12,0,0,-1,selected,region) &&
              memblock_routed_ppn_policy::is_external_span({8'b0,selected,12'b0},{8'b0,selected,12'b0}+4096), "unrepresentable anchor selects a legal full-width address");
        check(!policy.configure(data.pma_pmp_model,64'hffff_ffff_ffff_ffff,0), "missing PMA version must fail");
        check(!policy.query_c('h80000000,cacheable), "failed configure must invalidate previous snapshot");
    endfunction

    // 抽象职责：查询span与原scalar evaluator在边界处保持同一版本/优先级/debug语义。
    function void check_pma_scalar_equivalence();
        memblock_pma_attribute_span_t spans[$];
        pma_pmp_request_context_t request;
        pma_pmp_eval_t result;
        longint unsigned pmp_generation;
        bit [63:0] pa;
        data.pma_pmp_model.get_current_generations(pmp_generation,generation);
        for (int unsigned debug = 0; debug < 2; debug++) begin
            check(data.pma_pmp_model.read_pma_attribute_spans(generation,debug[0],spans), "PMA span export");
            check(spans.size() > 0 && spans[0].first == 0 &&
                  spans[spans.size()-1].limit == 49'h1_0000_0000_0000, "span export covers complete physical domain");
            foreach (spans[i]) begin
                if (i != 0) check(spans[i-1].limit == spans[i].first,"spans must form a nonoverlapping partition");
                for (int unsigned edge_idx = 0; edge_idx < 2; edge_idx++) begin
                    pa = edge_idx ? spans[i].limit-1 : spans[i].first;
                    request = '{default:'0};
                    request.valid = 1;
                    request.translation_success = 1;
                    request.paddr = pa[47:0];
                    request.size_bytes = 1;
                    request.cmd = PMA_PMP_CMD_READ;
                    request.priv_mode = 0;
                    request.debug = debug[0];
                    request.pmp_generation = pmp_generation;
                    request.pma_generation = generation;
                    check(data.pma_pmp_model.evaluate(request,result) && result.cacheable == spans[i].c,
                          $sformatf("span/scalar PMA equivalence debug=%0d PA=0x%0h",debug,pa));
                end
            end
        end
    endfunction

    // 抽象职责：以真正公共 replay 服务验证 monitor C 写事件在 C+1/C+2 的版本切换。
    function void check_mature_csr_snapshot();
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t event_item;
        longint unsigned saved_sample, old_generation, pmp_generation;
        bit cacheable;
        reset_fixture();
        data.pma_pmp_model.apply_csr_write(12'h7c8,64'h0000_0000_2400_0000,10);
        configure_current();
        old_generation = generation;
        saved_sample = memblock_sync_pkg::lifecycle_event_published_seq;
        event_item = memblock_sync_pkg::make_empty_raw_pma_pmp_csr_write();
        event_item.valid = 1;
        event_item.sample_seq = 20;
        event_item.sample_time = 1;
        event_item.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        event_item.addr = 12'h7c0;
        event_item.data = 64'h0000_0000_0000_006f;
        memblock_sync_pkg::push_raw_pma_pmp_csr_write(event_item);
        memblock_sync_pkg::lifecycle_event_published_seq = 21;
        data.service_csr_pmp_pma_write_observations();
        configure_current();
        check(generation == old_generation && policy.query_c('h31100000,cacheable) && !cacheable,
              "C+1 query must see old c0 and unconsumed writes");
        check(memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 1,"C+1 must retain immature FIFO prefix");
        memblock_sync_pkg::lifecycle_event_published_seq = 22;
        data.service_csr_pmp_pma_write_observations();
        configure_current();
        check(generation > old_generation && policy.query_c('h31100000,cacheable) && cacheable,
              "C+2 query must see new c1");
        check(memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0,"C+2 consumes mature prefix once");
        check(policy.configure(data.pma_pmp_model,old_generation,0) &&
              policy.query_c('h31100000,cacheable) && !cacheable,"old generation remains immutable");
        check(policy.configure(data.pma_pmp_model,generation,0), "configure debug-disabled PMA view");
        expect_cacheability('h3801ffff, 1, "debug boundary predecessor retains c1");
        expect_cacheability('h38020000, 0, "debug boundary first byte is protected c0");
        expect_cacheability('h38020fff, 0, "debug boundary last byte is protected c0");
        expect_cacheability('h38021000, 1, "debug boundary successor restores c1");
        check(policy.configure(data.pma_pmp_model,generation,1), "configure debug-enabled PMA view");
        expect_cacheability('h38020000, 1, "debug context restores PMA table match");
        memblock_sync_pkg::lifecycle_event_published_seq = saved_sample;
        reset_fixture();
    endfunction

    // 抽象职责：跨 S1/S2/allStage、普通/大页/NAPOT 的完整编码后输出仍遵守策略。
    function void check_builder_stages_and_shapes();
        mmu_csr_runtime_state csr;
        tlb_map_builder builder;
        memblock_tlb_lookup_key_t key;
        memblock_tlb_entry entry;
        bit n_override, forced_4k;
        bit [43:0] final_ppn;
        bit target_c;
        int unsigned shift;
        configure_current();
        for (int unsigned stage = 0; stage < 3; stage++) begin
            for (int unsigned rank = 0; rank < 5; rank++) begin
                for (int unsigned pbmt = 0; pbmt < 3; pbmt++) begin
                    csr = mmu_csr_runtime_state::type_id::create("routed_builder_csr");
                    csr.reset();
                    csr.satp_mode = (rank == 4) ? 9 : 8;
                    csr.satp_ppn = 44'h80000;
                    csr.vsatp_mode = stage == 2 ? csr.satp_mode : 0;
                    csr.vsatp_ppn = 44'h80000;
                    csr.hgatp_mode = stage == 0 ? 0 : csr.satp_mode;
                    csr.hgatp_ppn = 44'h80000;
                    csr.priv_virt = stage != 0;
                    csr.m_pbmt_en = 1;
                    csr.h_pbmt_en = 1;
                    csr.update_seq = 1;
                    key = csr.make_lookup_key(64'h8d2f5,stage == 0 ? 2'd0 : stage == 1 ? 2'd2 : 2'd3);
                    builder = tlb_map_builder::type_id::create("routed_stage_builder");
                    builder.configure_routed_ppn(data.pma_pmp_model,generation,0,'{default:'0});
                    entry = builder.prepare_tlb_payload_context(key,csr);
                    if (entry.s1_stage_active) entry.s1_entry_pbmt = pbmt[1:0];
                    if (entry.s2_stage_active) entry.s2_entry_pbmt = pbmt[1:0];
                    entry = builder.finalize_tlb_payload_shape_and_payload(entry,key,csr,5'b11111,
                        n_override,forced_4k,1,memblock_tlb_range_coverage_rank_e'(rank));
                    check(entry != null && !entry.has_effective_fault(),"stage/shape fixture must be normal translation");
                    final_ppn = stage == 0 ? entry.s1_resolved_ppn : entry.s2_resolved_ppn;
                    shift = rank == 0 ? 12 : rank == 1 ? 16 : rank == 2 ? 21 : rank == 3 ? 30 : 39;
                    check(policy.query_c({8'b0,final_ppn,12'b0},target_c) &&
                          policy.check_mapping(final_ppn,shift,pbmt[1:0],target_c),
                          $sformatf("encoded mapping must be legal stage=%0d rank=%0d pbmt=%0d",stage,rank,pbmt));
                    if (stage == 2) begin
                        check((csr.hgatp_mode == 8 && entry.s1_resolved_ppn[43:29] == 0) ||
                              (csr.hgatp_mode == 9 && entry.s1_resolved_ppn[43:38] == 0),
                              "allStage GPA must fit S2 mode independently of final route");
                    end
                    if (rank == 0) begin
                        check(!builder.check_reused_routed_ppn(entry,44'h31136),"reuse cannot reintroduce B08 hole");
                        check(builder.check_reused_routed_ppn(entry,final_ppn),"same legal mapping remains reusable");
                    end
                end
            end
        end
    endfunction

    // 抽象职责：只有一个4KiB c0岛时，大页无解只降一次且保持c0；显式大页
    // 返回冲突。allStage只改变最终S2形状，S1 GPA/tag不得被回改。
    function void check_bounded_fallback();
        mmu_csr_runtime_state csr;
        tlb_map_builder builder;
        memblock_tlb_entry entry;
        memblock_tlb_lookup_key_t key;
        bit [43:0] candidate, s1_before;
        bit [34:0] tag_before;
        bit fell_back, cacheable;
        reset_fixture();
        data.pma_pmp_model.apply_csr_write(12'h7c0,0,2);
        data.pma_pmp_model.apply_csr_write(12'h7c8,64'h0c44_01ff,3);
        data.pma_pmp_model.apply_csr_write(12'h7c9,64'h1fff_ffff_ffff,4);
        data.pma_pmp_model.apply_csr_write(12'h7c0,64'h7f3f,5);
        configure_current();
        expect_cacheability('h31100000, 0, "fallback island first byte is c0");
        expect_cacheability('h31100fff, 0, "fallback island last byte is c0");
        expect_cacheability('h310fffff, 1, "fallback island predecessor is c1");
        expect_cacheability('h31101000, 1, "fallback island successor is c1");
        csr = mmu_csr_runtime_state::type_id::create("fallback_csr");
        csr.reset(); csr.satp_mode = 8; csr.satp_ppn = 44'h80000;
        csr.vsatp_mode = 8; csr.vsatp_ppn = 44'h80000;
        csr.hgatp_mode = 8; csr.hgatp_ppn = 44'h80000;
        csr.m_pbmt_en = 1; csr.h_pbmt_en = 1;
        for (int unsigned all_stage = 0; all_stage < 2; all_stage++) begin
            csr.priv_virt = all_stage[0];
            key = csr.make_lookup_key(64'h100,all_stage ? 2'd3 : 2'd0);
            builder = tlb_map_builder::type_id::create("fallback_builder");
            builder.configure_routed_ppn(data.pma_pmp_model,generation,0,'{default:'0});
            entry = builder.prepare_tlb_payload_context(key,csr);
            entry.s1_entry_pbmt = 0;
            if (all_stage) begin
                entry.s2_entry_pbmt = 0; entry.s2_level = 1; entry.s2_pte_n = 0;
                entry.s1_entry_ppn_raw = 44'h12345; entry.s1_tag = 35'h345;
            end else begin entry.s1_level = 1; entry.s1_pte_n = 0; end
            s1_before = entry.s1_entry_ppn_raw;
            tag_before = entry.s1_tag;
            candidate = 44'h31000;
            check(!builder.select_final_routed_ppn(entry,!all_stage,38'h100,1,candidate,fell_back) &&
                  !fell_back,"explicit impossible shape must fail without downgrade");
            candidate = 44'h31000;
            check(builder.select_final_routed_ppn(entry,!all_stage,38'h100,0,candidate,fell_back) &&
                  fell_back && candidate == 44'h31100 &&
                  policy.check_mapping(candidate,12,0,0),"ordinary fallback remains on original c0 island");
            if (all_stage) check(entry.s2_level == 0 && entry.s1_entry_ppn_raw == s1_before &&
                entry.s1_tag == tag_before,"allStage fallback must preserve S1 mapping/tag");
            else check(entry.s1_level == 0,"S1-only fallback selects 4K");
        end
        reset_fixture();
    endfunction

    // 抽象职责：窄窗口意图是输入约束；重复冲突和 reset 不得悄悄复用旧关系。
    function void check_intent_lifecycle();
        memblock_routed_ppn_intent_t intent, conflict;
        mmu_csr_runtime_state csr;
        memblock_tlb_lookup_key_t req_key, anchor_key;
        memblock_tlb_lookup_result_e result;
        memblock_tlb_entry entry;
        bit created;
        reset_fixture();
        intent = '{default:'0};
        intent.valid = 1;
        intent.ppn = 44'h310b0;
        intent.first_offset = 0;
        intent.last_offset = 127;
        intent.region_id = 4;
        check(data.register_routed_ppn_intent(38'h80001,intent),"register narrow UART intent");
        check(data.register_routed_ppn_intent(38'h80001,intent),"same intent is idempotent");
        conflict = intent;
        conflict.ppn = 44'h31100;
        check(!data.register_routed_ppn_intent(38'h80001,conflict),"conflicting same-VPN intent must fail");
        csr = mmu_csr_runtime_state::type_id::create("intent_csr");
        csr.reset(); csr.satp_mode = 8; csr.satp_ppn = 44'h80000; csr.m_pbmt_en = 1;
        check(data.get_or_create_l2tlb_entry_by_req_with_snapshot(38'h80001,0,csr,0,0,
            req_key,anchor_key,result,entry,created) && created &&
            entry.s1_resolved_ppn == intent.ppn,"first canonical build consumes physical intent");
        check(data.get_or_create_l2tlb_entry_by_req_with_snapshot(38'h80001,0,csr,0,0,
            req_key,anchor_key,result,entry,created) && !created &&
            entry.s1_resolved_ppn == intent.ppn,"exact hit preserves published intent mapping");
        reset_fixture();
        check(data.register_routed_ppn_intent(38'h80001,conflict),"reset removes old intent ownership");
        reset_fixture();
    endfunction

    virtual task body();
        ensure_dispatch_runtime_helpers();
        check(!seq_csr_common::get_main_mem_ranges_en() &&
              !seq_csr_common::get_l2tlb_ppn_reuse_en(),"routed software cfg must use sparse/no-reuse");
        reset_fixture();
        check_route_boundaries();
        check_policy_selection();
        check_pma_scalar_equivalence();
        check_mature_csr_snapshot();
        check_builder_stages_and_shapes();
        check_bounded_fallback();
        check_intent_lifecycle();
        `uvm_info("ROUTED_PPN_SOFT", "routed PPN software checks completed", UVM_LOW)
    endtask
endclass
`endif
