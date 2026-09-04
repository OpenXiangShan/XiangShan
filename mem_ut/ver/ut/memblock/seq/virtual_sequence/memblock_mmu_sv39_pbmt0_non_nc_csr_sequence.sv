//=========================================================
//File name    : memblock_mmu_sv39_pbmt0_non_nc_csr_sequence.sv
//Module name  : memblock_mmu_sv39_pbmt0_non_nc_csr_sequence
//Discribution : strict PBMT=0/non-NC Sv39 CSR producer
//=========================================================
`ifndef MEMBLOCK_MMU_SV39_PBMT0_NON_NC_CSR_SEQUENCE__SV
`define MEMBLOCK_MMU_SV39_PBMT0_NON_NC_CSR_SEQUENCE__SV

// 中文注释：该 producer 只服务严格 PBMT=0/non-NC real-DUT 场景。
// U 态 reset 后 PMP 默认全 OFF，因此先通过 distribute CSR 建立一个覆盖低端
// 物理内存的 TOR/RWX entry；satp 在 PMP 写入及其流水线稳定后才切到 Sv39，
// 防止 real-smoke 的 CSR barrier 在 PMP 生效前放行首笔访存。
class memblock_mmu_sv39_pbmt0_non_nc_csr_sequence extends
    memblock_mmu_sv39_csr_sequence;

    localparam bit [11:0] PBMT0_PMPADDR0_CSR_ADDR = 12'h3b0;
    localparam bit [11:0] PBMT0_PMPCFG0_CSR_ADDR  = 12'h3a0;
    // pmpaddr 保存 PA[47:2]；该最大可表示值覆盖本场景的 C=1 PA 窗口。
    localparam bit [63:0] PBMT0_PMPADDR0_TOR_TOP = 64'h0000_3fff_ffff_ffff;
    localparam bit [63:0] PBMT0_PMPCFG0_TOR_RWX  = 64'h0000_0000_0000_000f;
    // 中文注释：PMP CSR 写入经过 NewCSR 流水线；保留 64 个无访存 item 后再
    // 发布 Sv39，避免仅凭第一份 satp snapshot 提前启动 issue/L2TLB。
    localparam int unsigned PBMT0_PMP_SETTLE_ITEMS = 64;
    localparam bit [3:0] MEMBLOCK_SATP_MODE_BARE = 4'd0;

    `uvm_object_utils(memblock_mmu_sv39_pbmt0_non_nc_csr_sequence)

    extern function new(string name =
        "memblock_mmu_sv39_pbmt0_non_nc_csr_sequence");
    extern virtual function void configure_static_sv39_xaction(
        input bit [43:0] root_ppn,
        input int unsigned item_index,
        output csr_ctrl_agent_agent_xaction tr
    );

endclass:memblock_mmu_sv39_pbmt0_non_nc_csr_sequence

function memblock_mmu_sv39_pbmt0_non_nc_csr_sequence::new(
    string name = "memblock_mmu_sv39_pbmt0_non_nc_csr_sequence"
);
    super.new(name);
endfunction:new

// 抽象职责：构造严格 PBMT=0 场景的静态 CSR item，并在首两笔 item 注入
// PMP bootstrap。除 PMP 写入和 Bare/Sv39 启动顺序外，所有字段沿用基类的
// Scala reset baseline；该函数不产生 satp changed/flush pulse。
function void memblock_mmu_sv39_pbmt0_non_nc_csr_sequence::configure_static_sv39_xaction(
    input bit [43:0] root_ppn,
    input int unsigned item_index,
    output csr_ctrl_agent_agent_xaction tr
);
    super.configure_static_sv39_xaction(root_ppn, item_index, tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(),
                   "failed to build strict PBMT=0 CSR xaction")
    end
    if (item_index == 0 &&
        (seq_csr_common::get_mmu_sv39_m_pbmte_en() ||
         seq_csr_common::get_mmu_sv39_h_pbmte_en())) begin
        `uvm_fatal(get_type_name(),
                   "strict PBMT=0 CSR producer requires mPBMTE=hPBMTE=0")
    end

    // Bare mode is held until both PMP writes have crossed the CSR pipeline.
    if (item_index < (2 + PBMT0_PMP_SETTLE_ITEMS)) begin
        tr.io_ooo_to_mem_tlbCsr_satp_mode = MEMBLOCK_SATP_MODE_BARE;
    end

    case (item_index)
        0: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr =
                PBMT0_PMPADDR0_CSR_ADDR;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data =
                PBMT0_PMPADDR0_TOR_TOP;
        end
        1: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr =
                PBMT0_PMPCFG0_CSR_ADDR;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data =
                PBMT0_PMPCFG0_TOR_RWX;
        end
        default: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = '0;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = '0;
        end
    endcase
endfunction:configure_static_sv39_xaction

`endif
