// This file is included inside its generated scenario module private namespace.
// Keep scenario-local helpers here; shared scenario interfaces belong in shared.hpp.

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
              << '\n';
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
              << '\n';
    return 0;
}

int run_vector_guest_fault_boundary(int argc, char **argv, const Options &options)
{
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t vs_root = 0x94000000ULL;
    constexpr std::uint64_t g_root = 0x95000000ULL;
    const unsigned samples = std::max(1U, options.transactions);
    std::mt19937_64 random(options.seed ^ 0x6a09e667f3bcc909ULL);
    unsigned failures = 0;
    for (unsigned sample = 0; sample < samples; ++sample) {
        memblock::Environment environment(argc, argv);
        const std::uint64_t backpressure_seed = random();
        environment.configure_backpressure(backpressure_seed, options.backpressure);
        if (!environment.reset() ||
            !environment.map_sv39_4k(guest_virtual, guest_physical, vs_root) ||
            !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !environment.activate_two_stage(vs_root, g_root, 7, 11)) {
            std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_BOUNDARY_FAIL seed="
                      << options.seed << " sample=" << sample
                      << " reason=configuration\n";
            return 1;
        }
        // Keep the fault in the VS L1 PTE page, but randomize all lower VA
        // bits so split-byte and active-element offsets are independently
        // explored for every seed/sample.
        const std::uint64_t address = guest_virtual + random() % 0x1000;
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, address);
        if (reference.translated || !reference.guest_page_fault ||
            !reference.is_for_vs_nonleaf_pte) {
            std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_BOUNDARY_FAIL seed="
                      << options.seed << " sample=" << sample
                      << " reason=reference-walk address=0x" << std::hex
                      << address << std::dec << '\n';
            return 1;
        }
        const unsigned eew = static_cast<unsigned>(random() % 4);
        const unsigned elements = 16U >> eew;
        memblock::VectorMemoryTransaction transaction{
            .store = false,
            .address = address,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(1U + random() % elements),
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(1U + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kVectorMemoryLanes),
            .flow_num = 2,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        transaction.vm = (random() & 1U) != 0;
        transaction.mask_bits = static_cast<std::uint16_t>(random());
        transaction.vma = (random() & 1U) != 0;
        transaction.vta = (random() & 1U) != 0;
        transaction.vstart = static_cast<std::uint8_t>(
            transaction.vl == 0 ? 0 : random() % transaction.vl);
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random());
        }
        if (!transaction.vm) {
            const std::uint32_t vl_mask = transaction.vl >= 16
                ? 0xffffU
                : ((std::uint32_t{1} << transaction.vl) - 1U);
            transaction.mask_bits &= static_cast<std::uint16_t>(vl_mask);
            if (memblock::active_vector_elements(transaction) == 0) {
                const unsigned first_active = transaction.vstart +
                    static_cast<unsigned>(random() %
                        (transaction.vl - transaction.vstart));
                transaction.mask_bits |= static_cast<std::uint16_t>(
                    1U << first_active);
            }
        }
        // Vary the inter-arrival point relative to PTW/DCache backpressure;
        // this does not alter the architectural oracle.
        const unsigned pre_issue_delay = static_cast<unsigned>(random() % 16);
        if (!environment.run_cycles(pre_issue_delay)) {
            ++failures;
            continue;
        }
        environment.expect_vector(transaction);
        // The vector exception VA is the address of the first active element
        // in the current micro-op.  The GPA oracle, in contrast, identifies
        // the page-table byte and must remain unshifted for VS-non-leaf faults.
        unsigned first_active_element = transaction.vstart;
        while (first_active_element < (16U >> transaction.eew) &&
               ((memblock::active_vector_elements(transaction) >>
                 first_active_element) & 1U) == 0) {
            ++first_active_element;
        }
        const std::uint64_t expected_vaddr = address +
            static_cast<std::uint64_t>(first_active_element << transaction.eew);
        bool passed =
            environment.set_rob_head(transaction.rob, transaction.rob_flag) &&
            environment.enqueue_vector(transaction) &&
            environment.issue_vector(transaction, 512) &&
            environment.run_until_vector_complete_with_replays(transaction, 16384) &&
            environment.run_until_lq_retired(2048) &&
            environment.exception_is_for_vs_nonleaf_pte() &&
            environment.exception_vaddr() == expected_vaddr &&
            environment.exception_gpaddr() ==
                reference.faulting_guest_physical_address;
        if (!passed) {
            ++failures;
            std::cerr << "MEMBLOCK_VECTOR_GUEST_FAULT_BOUNDARY_SAMPLE_FAIL seed="
                      << options.seed << " sample=" << sample
                      << " address=0x" << std::hex << address
                      << " expected_vaddr=0x" << expected_vaddr
                      << " actual_vaddr=0x" << environment.exception_vaddr()
                      << " expected_vs_nonleaf=1 actual_vs_nonleaf="
                      << std::dec << environment.exception_is_for_vs_nonleaf_pte()
                      << " expected_gpaddr=0x"
                      << reference.faulting_guest_physical_address
                      << " actual_gpaddr=0x" << environment.exception_gpaddr()
                      << " eew=" << std::dec << eew
                      << " vl=" << static_cast<unsigned>(transaction.vl)
                      << " vstart=" << static_cast<unsigned>(transaction.vstart)
                      << " vm=" << transaction.vm
                      << " mask=0x" << std::hex << transaction.mask_bits
                      << " active=0x" << memblock::active_vector_elements(transaction)
                      << std::dec << " vma=" << transaction.vma
                      << " vta=" << transaction.vta
                      << " lane=" << transaction.lane
                      << " flow_num=" << static_cast<unsigned>(transaction.flow_num)
                      << " pre_issue_delay=" << pre_issue_delay
                      << " backpressure_seed=0x" << std::hex << backpressure_seed
                      << std::dec << " reason=" << environment.error() << '\n';
        }
    }
    std::cout << "MEMBLOCK_VECTOR_GUEST_FAULT_BOUNDARY_"
              << (failures == 0 ? "PASS" : "FAIL")
              << " seed=" << options.seed << " transactions=" << samples
              << " failures=" << failures
              << '\n';
    return failures == 0 ? 0 : 1;
}
