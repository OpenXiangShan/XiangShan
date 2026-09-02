//=========================================================
//File name    : memblock_main_dispatch_pbmt_response_fault_sequence.sv
//Module name  : memblock_main_dispatch_pbmt_response_fault_sequence
//Discribution : real DUT PBMT response-fault directed main-table sequence
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_PBMT_RESPONSE_FAULT_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_PBMT_RESPONSE_FAULT_SEQUENCE__SV

// 中文注释：该 sequence 为 PBMTE 动态切换 smoke 构造同 VPN 的 A Load、B Store
// 和 C Store。A 填充 Load DTLB；B/C 使用独立的 Store DTLB，确保 B 在 PBMTE
// 关闭后真实发出 L2TLB request。B fault 不应填充 Store DTLB，因此 C 在重新
// enable 后必须再次走 L2TLB response。该 sequence 不直接驱动 CSR 或 L2TLB
// 接口，仍由 vseq 中的唯一 CSR producer 和 responder 负责。
class memblock_main_dispatch_pbmt_response_fault_sequence extends
    memblock_main_dispatch_manual_main_table_sequence;

    // 中文注释：为 CSR -> PMP 的实际流水线留出启动窗口；A 发射前，定向 CSR
    // producer 已依次写入 pmpaddr0 与 pmpcfg0，使 U 态访存不会被 reset PMP deny。
    localparam int unsigned PBMT_A_DELAY_CYCLES = 64;
    localparam int unsigned PBMT_B_DELAY_CYCLES = 1024;
    // B 的 Store PF 在真实 STA replay/fault-retire 路径中需要略长于 2048 个
    // scheduler tick；保留额外窗口，确保 CSR producer 已观察 B terminal 并让
    // C 的 response-visible C-2 CSR 稳定回到 mPBMTE=1。
    localparam int unsigned PBMT_C_DELAY_CYCLES = 3072;

    `uvm_object_utils(memblock_main_dispatch_pbmt_response_fault_sequence)

    extern function new(string name = "memblock_main_dispatch_pbmt_response_fault_sequence");
    extern virtual task body();
    extern virtual task build_directed_mixed_main_table();
    extern virtual function void check_pbmt_response_fault_results();

endclass:memblock_main_dispatch_pbmt_response_fault_sequence

function memblock_main_dispatch_pbmt_response_fault_sequence::new(
    string name = "memblock_main_dispatch_pbmt_response_fault_sequence"
);
    super.new(name);
endfunction:new

// 抽象职责：在 disabled control topology 下建立定向表、复用真实 dispatch service，
// 并在所有 token 自然完成后检查 A/B/C 的 UID payload 与 raw live entry。它不改变
// 终态、异常或 CSR 状态，检查失败只报告该 directed 场景的实现不变量被破坏。
task memblock_main_dispatch_pbmt_response_fault_sequence::body();
    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED) begin
        `uvm_fatal(get_type_name(),
                   "PBMT response-fault main sequence requires disabled control topology")
    end

    ensure_dispatch_runtime_helpers();
    build_directed_mixed_main_table();
    check_main_table_control_policy(get_type_name());
    `uvm_info(get_type_name(),
              $sformatf("PBMT response-fault main table ready: main_trans_num=%0d",
                        data.main_trans_num),
              UVM_LOW)

    service_real_dispatch_flow();
    check_pbmt_response_fault_results();
    data.end_test_check();
    `uvm_info(get_type_name(), "PBMT response-fault main sequence completed", UVM_LOW)
endtask:body

// 抽象职责：建立相同 S1 VPN 的 A Load、B Store 和 C Store。A 的 delay 先覆盖 PMP
// bootstrap 管线，B/C 的 delay 则只控制现有 issue queue 的 ready_cycle；B/C 利用
// Store DTLB 与 A 的 Load DTLB 独立这一 DUT 事实，获得各自的 L2TLB token。三者
// 都不改变 LSQ admission 或 L2TLB token 生命周期，因此 CSR sequence 可在 A/B
// terminal 后安全切换。
task memblock_main_dispatch_pbmt_response_fault_sequence::build_directed_mixed_main_table();
    main_control_transaction a_load;
    main_control_transaction b_store;
    main_control_transaction c_store;

    clear_manual_main_table();

    a_load = make_directed_transaction("pbmt_enable_build_load",
                                       MEMBLOCK_OP_CLASS_INT_LOAD,
                                       0,
                                       64'h0000_0000_8000_1000);
    a_load.delay = PBMT_A_DELAY_CYCLES;
    set_manual_main_transaction(0, a_load);

    b_store = make_directed_transaction("pbmt_disable_reuse_store",
                                        MEMBLOCK_OP_CLASS_STORE,
                                        1,
                                        64'h0000_0000_8000_1000);
    b_store.delay = PBMT_B_DELAY_CYCLES;
    set_manual_main_transaction(1, b_store);

    c_store = make_directed_transaction("pbmt_reenable_reuse_store",
                                        MEMBLOCK_OP_CLASS_STORE,
                                        2,
                                        64'h0000_0000_8000_1000);
    c_store.delay = PBMT_C_DELAY_CYCLES;
    set_manual_main_transaction(2, c_store);

    import_manual_main_table();
endtask:build_directed_mixed_main_table

// 抽象职责：检查三个 UID 从同一 raw entry 得到的 response payload。B 的 Store PF
// 是 token 私有 overlay；C 的正常 Store payload 和 raw entry 必须证明 B 没有回写
// live entry。
function void memblock_main_dispatch_pbmt_response_fault_sequence::
    check_pbmt_response_fault_results();
    memblock_uid_tlb_record a_record;
    memblock_uid_tlb_record b_record;
    memblock_uid_tlb_record c_record;
    status_transaction b_status;
    status_transaction c_status;
    memblock_tlb_entry raw_entry;

    if (data == null || data.main_trans_num != 3) begin
        `uvm_fatal(get_type_name(), "PBMT directed result check lost the three-entry main table")
    end

    a_record = data.get_uid_tlb_record(0);
    b_record = data.get_uid_tlb_record(1);
    c_record = data.get_uid_tlb_record(2);
    b_status = data.get_status(1);
    c_status = data.get_status(2);
    raw_entry = data.get_tlb_entry(a_record.lookup_key);

    if (!a_record.pte_valid || !b_record.pte_valid || !c_record.pte_valid) begin
        `uvm_fatal(get_type_name(), "PBMT directed scenario has an incomplete UID TLB payload")
    end
    if (!a_record.csr_snapshot.m_pbmt_en || b_record.csr_snapshot.m_pbmt_en ||
        !c_record.csr_snapshot.m_pbmt_en) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT CSR order mismatch A/B/C mPBMTE=%0d/%0d/%0d",
                             a_record.csr_snapshot.m_pbmt_en,
                             b_record.csr_snapshot.m_pbmt_en,
                             c_record.csr_snapshot.m_pbmt_en))
    end
    if (a_record.payload.s1_entry_pbmt == 2'd0 ||
        b_record.payload.s1_entry_pbmt != a_record.payload.s1_entry_pbmt ||
        c_record.payload.s1_entry_pbmt != a_record.payload.s1_entry_pbmt) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT raw entry was not reused A/B/C pbmt=%0d/%0d/%0d",
                             a_record.payload.s1_entry_pbmt,
                             b_record.payload.s1_entry_pbmt,
                             c_record.payload.s1_entry_pbmt))
    end
    if (!b_record.payload.fault_effective_s1_pf || !b_status.exception_vec[15]) begin
        `uvm_fatal(get_type_name(), "PBMT-disabled B did not return/retire an S1 page fault")
    end
    if (c_record.payload.fault_effective_s1_pf || c_status.exception_vec[15]) begin
        `uvm_fatal(get_type_name(), "PBMT re-enabled C inherited B token-local S1 page fault")
    end
    if (raw_entry.s1_entry_pbmt == 2'd0 || raw_entry.fault_effective_s1_pf ||
        raw_entry.fault_raw_s1_pf) begin
        `uvm_fatal(get_type_name(), "PBMT response overlay modified the raw live entry")
    end
endfunction:check_pbmt_response_fault_results

`endif
