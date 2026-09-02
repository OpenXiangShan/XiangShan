//=========================================================
//File name    : memblock_l2tlb_pbmt_toggle_csr_sequence.sv
//Module name  : memblock_l2tlb_pbmt_toggle_csr_sequence
//Discribution : unique dynamic PBMTE CSR producer for real DUT smoke
//=========================================================
`ifndef MEMBLOCK_L2TLB_PBMT_TOGGLE_CSR_SEQUENCE__SV
`define MEMBLOCK_L2TLB_PBMT_TOGGLE_CSR_SEQUENCE__SV

typedef enum int unsigned {
    // A 建表期间保持 mPBMTE=1，允许固定 nonzero PBMT 进入 raw live entry。
    MEMBLOCK_PBMT_TOGGLE_ENABLE_A,
    // A terminal 后保持 mPBMTE=0，B 的 response-visible C-2 context 应产生 PF。
    MEMBLOCK_PBMT_TOGGLE_DISABLE_B,
    // B terminal 后重新打开 mPBMTE，C 用来验证没有继承 B 的 token-local fault。
    MEMBLOCK_PBMT_TOGGLE_ENABLE_C
} memblock_pbmt_toggle_state_e;

// 中文注释：该 sequence 是 PBMT real-DUT directed 场景在 csr_ctrl_sqr 上的唯一 producer。
// 每拍持续驱动完整 Sv39/U baseline，仅切换 mPBMTE，所有 satp/vsatp/hgatp/priv change
// pulse 保持 0，因此不会把 PBMT 语义检查变成 TLB flush 或进程切换测试。
class memblock_l2tlb_pbmt_toggle_csr_sequence extends memblock_mmu_sv39_csr_sequence;

    // 中文注释：该 directed U 态 Sv39 场景必须显式建立一个覆盖 48-bit PA 的 PMP
    // TOR+RWX entry；否则 reset 后全零 PMP 表会先把 A 判为 access fault，遮蔽 PBMT
    // response 行为。两笔 distribute CSR write 只用于该 smoke 的 DUT 前置条件。
    localparam bit [11:0] PBMT_PMPADDR0_CSR_ADDR = 12'h3b0;
    localparam bit [11:0] PBMT_PMPCFG0_CSR_ADDR  = 12'h3a0;
    // 中文注释：`pmpaddr0` 仅保存 PA[47:2] 的 46 bit；数学上的 2^46 端点
    // 会被硬件截断为零，因此用最大可表示值覆盖本场景的低端物理地址窗口。
    localparam bit [63:0] PBMT_PMPADDR0_TOR_TOP  = 64'h0000_3fff_ffff_ffff;
    localparam bit [63:0] PBMT_PMPCFG0_TOR_RWX   = 64'h0000_0000_0000_000f;

    memblock_pbmt_toggle_state_e toggle_state;
    bit                          saw_disable;
    bit                          saw_reenable;

    `uvm_object_utils(memblock_l2tlb_pbmt_toggle_csr_sequence)

    extern function new(string name = "memblock_l2tlb_pbmt_toggle_csr_sequence");
    extern virtual task body();
    extern virtual function bit uid_terminal_with_tlb(input memblock_uid_t uid);
    extern virtual function void configure_toggle_xaction(
        input bit [43:0] root_ppn,
        input int unsigned item_index,
        input memblock_pbmt_toggle_state_e state,
        output csr_ctrl_agent_agent_xaction tr
    );

endclass:memblock_l2tlb_pbmt_toggle_csr_sequence

function memblock_l2tlb_pbmt_toggle_csr_sequence::new(
    string name = "memblock_l2tlb_pbmt_toggle_csr_sequence"
);
    super.new(name);
    toggle_state = MEMBLOCK_PBMT_TOGGLE_ENABLE_A;
    saw_disable = 1'b0;
    saw_reenable = 1'b0;
endfunction:new

// 抽象职责：在主表完成后持续提交完整 CSR payload，并仅在 A/B 已经 terminal 且其
// L2TLB UID record 完成的边界转换 PBMTE state。它不读取或修改 live entry，也不承担
// response 检查；转换结果由同一拍及之后的 CSR C-2 history 提供给 responder。
task memblock_l2tlb_pbmt_toggle_csr_sequence::body();
    bit [43:0] root_ppn;
    int unsigned item_index;
    csr_ctrl_agent_agent_xaction tr;

    seq_csr_common::init();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (memblock_sync_pkg::uses_control_barrier_topology()) begin
        `uvm_fatal(get_type_name(),
                   "PBMT toggle CSR sequence requires disabled control topology")
    end

    wait_for_main_table_or_stop();
    if (data.is_global_stop_requested()) begin
        return;
    end

    root_ppn = get_sv39_root_ppn();
    item_index = 0;
    while (!data.is_global_stop_requested()) begin
        if (toggle_state == MEMBLOCK_PBMT_TOGGLE_ENABLE_A &&
            uid_terminal_with_tlb(0)) begin
            toggle_state = MEMBLOCK_PBMT_TOGGLE_DISABLE_B;
            saw_disable = 1'b1;
            `uvm_info(get_type_name(), "A terminal/TLB complete: drive mPBMTE=0 for B", UVM_LOW)
        end
        else if (toggle_state == MEMBLOCK_PBMT_TOGGLE_DISABLE_B &&
                 uid_terminal_with_tlb(1)) begin
            toggle_state = MEMBLOCK_PBMT_TOGGLE_ENABLE_C;
            saw_reenable = 1'b1;
            `uvm_info(get_type_name(), "B terminal/TLB complete: drive mPBMTE=1 for C", UVM_LOW)
        end

        configure_toggle_xaction(root_ppn, item_index, toggle_state, tr);
        start_item(tr);
        finish_item(tr);
        item_index++;
    end

    if (!saw_disable || !saw_reenable ||
        toggle_state != MEMBLOCK_PBMT_TOGGLE_ENABLE_C) begin
        `uvm_fatal(get_type_name(), "PBMT toggle CSR sequence stopped before completing A/B/C states")
    end
endtask:body

function bit memblock_l2tlb_pbmt_toggle_csr_sequence::uid_terminal_with_tlb(
    input memblock_uid_t uid
);
    if (data == null || !data.is_valid_uid(uid)) begin
        return 1'b0;
    end
    return data.get_status(uid).terminal_done && data.tlb_entry_ready_for_uid(uid);
endfunction:uid_terminal_with_tlb

// 抽象职责：复用静态 Sv39/U CSR payload，并按当前 directed state 覆盖 mPBMTE。
// 前两笔 item 额外写入 PMP bootstrap；其余 item 不产生 generic CSR write 或 change
// pulse，因此 PBMT 切换不删除 entry、不触发 SFENCE barrier。
function void memblock_l2tlb_pbmt_toggle_csr_sequence::configure_toggle_xaction(
    input bit [43:0] root_ppn,
    input int unsigned item_index,
    input memblock_pbmt_toggle_state_e state,
    output csr_ctrl_agent_agent_xaction tr
);
    configure_static_sv39_xaction(root_ppn, item_index, tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to build PBMT toggle CSR xaction")
    end
    tr.io_ooo_to_mem_tlbCsr_mPBMTE =
        state != MEMBLOCK_PBMT_TOGGLE_DISABLE_B;
    tr.io_ooo_to_mem_tlbCsr_hPBMTE = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;

    case (item_index)
        0: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr =
                PBMT_PMPADDR0_CSR_ADDR;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data =
                PBMT_PMPADDR0_TOR_TOP;
        end
        1: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr =
                PBMT_PMPCFG0_CSR_ADDR;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data =
                PBMT_PMPCFG0_TOR_RWX;
        end
        default: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = '0;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = '0;
        end
    endcase
endfunction:configure_toggle_xaction

`endif
