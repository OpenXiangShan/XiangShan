// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_vector_guest_fault(int argc, char **argv, bool split = false)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t vs_root = 0x94000000ULL;
    constexpr std::uint64_t g_root = 0x95000000ULL;
    const std::uint64_t fault_address = guest_virtual + (split ? 0x188 : 0x180);
    environment.configure_backpressure(0xa4093822299f31d0ULL, true);

    // G-stage maps the VS root page but deliberately omits the next-level VS
    // page-table page. The resulting guest-page fault is therefore caused by
    // a G-stage translation performed on behalf of a VS non-leaf PTE walk.
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            guest_virtual, guest_physical, vs_root) ||
        !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
        !environment.activate_two_stage(vs_root, g_root, 7, 11)) {
        std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const auto reference = memblock::reference_two_stage_walk(
        environment.memory(), vs_root, g_root, fault_address);
    if (reference.translated || !reference.guest_page_fault ||
        !reference.is_for_vs_nonleaf_pte) {
        std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle()
                  << " phase=reference-walk reason=software walk did not produce the expected VS non-leaf guest fault\n";
        return 1;
    }
    const std::uint64_t faulting_pte_gpa =
        reference.faulting_guest_physical_address;

    memblock::VectorMemoryTransaction fault{
        .address = fault_address,
        .eew = 3,
        .vl = 2,
        .rob = 0,
        .lq = 0,
        .pdest = 52,
        .lane = 0,
        .flow_num = 2,
        .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
    };
    environment.expect_vector(fault);
    if (!environment.set_rob_head(fault.rob, fault.rob_flag) ||
        !environment.enqueue_vector(fault) ||
        !environment.issue_vector(fault, 512) ||
        !environment.run_until_vector_complete_with_replays(fault, 16384) ||
        !environment.run_cycles(8) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle() << " phase=execute reason="
                  << environment.error() << '\n';
        return 1;
    }

    if (!environment.exception_is_for_vs_nonleaf_pte() ||
        environment.exception_vaddr() != fault_address ||
        environment.exception_gpaddr() != faulting_pte_gpa) {
        std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle()
                  << " phase=exception-metadata expected_vaddr=0x" << std::hex
                  << fault_address << " actual_vaddr=0x"
                  << environment.exception_vaddr()
                  << " expected_gpaddr=0x" << faulting_pte_gpa
                  << " actual_gpaddr=0x" << environment.exception_gpaddr()
                  << " expected_vs_nonleaf=1 actual_vs_nonleaf=" << std::dec
                  << environment.exception_is_for_vs_nonleaf_pte() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_VECTOR_GUEST_FAULT_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.vector_load_writebacks()
              << " vector_replays=" << environment.vector_replay_feedbacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " vaddr=0x" << std::hex << environment.exception_vaddr()
              << " gpaddr=0x" << environment.exception_gpaddr() << std::dec
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_scalar_guest_fault(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t vs_root = 0x94000000ULL;
    constexpr std::uint64_t g_root = 0x95000000ULL;
    constexpr std::uint64_t fault_address = guest_virtual + 0x188;
    environment.configure_backpressure(0x082efa98ec4e6c89ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            guest_virtual, guest_physical, vs_root) ||
        !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
        !environment.activate_two_stage(vs_root, g_root, 7, 11)) {
        std::cerr << "MEMBLOCK_SCALAR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }
    const auto reference = memblock::reference_two_stage_walk(
        environment.memory(), vs_root, g_root, fault_address);
    if (reference.translated || !reference.guest_page_fault ||
        !reference.is_for_vs_nonleaf_pte) {
        std::cerr << "MEMBLOCK_SCALAR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle()
                  << " phase=reference-walk reason=software walk did not produce the expected VS non-leaf guest fault\n";
        return 1;
    }
    const std::uint64_t faulting_pte_gpa =
        reference.faulting_guest_physical_address;
    const memblock::LoadTransaction fault{
        .address = fault_address,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 51,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
    };
    environment.expect_load(fault);
    if (!environment.set_rob_head(fault.rob, fault.rob_flag) ||
        !environment.enqueue_load(fault) ||
        !environment.issue_load(fault, 512) ||
        !environment.run_until_complete(16384) ||
        !environment.run_cycles(8) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_SCALAR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle() << " phase=execute reason="
                  << environment.error() << '\n';
        return 1;
    }
    if (!environment.exception_is_for_vs_nonleaf_pte() ||
        environment.exception_vaddr() != fault_address ||
        environment.exception_gpaddr() != faulting_pte_gpa) {
        std::cerr << "MEMBLOCK_SCALAR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle()
                  << " phase=exception-metadata expected_vaddr=0x" << std::hex
                  << fault_address << " actual_vaddr=0x"
                  << environment.exception_vaddr()
                  << " expected_gpaddr=0x" << faulting_pte_gpa
                  << " actual_gpaddr=0x" << environment.exception_gpaddr()
                  << " expected_vs_nonleaf=1 actual_vs_nonleaf=" << std::dec
                  << environment.exception_is_for_vs_nonleaf_pte() << '\n';
        return 1;
    }
    const auto &feedback = environment.scalar_load_feedback_stats();
    std::uint64_t feedback_wakeups = 0;
    std::uint64_t feedback_cancels = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        feedback_wakeups += feedback.wakeups[lane];
        feedback_cancels += feedback.ld2_cancels[lane];
    }
    if (feedback_cancels == 0 || feedback_wakeups != feedback_cancels) {
        std::cerr << "MEMBLOCK_SCALAR_GUEST_FAULT_FAIL cycle="
                  << environment.cycle()
                  << " phase=feedback-classification wakeups="
                  << feedback_wakeups << " cancels=" << feedback_cancels
                  << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_SCALAR_GUEST_FAULT_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.writebacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " vaddr=0x" << std::hex << environment.exception_vaddr()
              << " gpaddr=0x" << environment.exception_gpaddr() << std::dec
              << " wakeups=" << feedback_wakeups
              << " cancels=" << feedback_cancels
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
