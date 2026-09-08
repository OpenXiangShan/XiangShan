// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_smoke(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.reset() || !environment.check_idle(24)) {
        std::cerr << "MEMBLOCK_SMOKE_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_SMOKE_PASS"
              << " cycle=" << environment.cycle()
              << " driven_inputs=" << memblock::generated::kDrivenInputCount
              << " checked_outputs=" << memblock::generated::kQuiescentOutputCount
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_l2_flush_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.reset() || environment.outer_l2_flush_enabled() ||
        environment.backend_l2_flush_done()) {
        std::cerr << "MEMBLOCK_L2_FLUSH_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    constexpr std::array<std::pair<bool, bool>, 8> patterns{{
        {false, false},
        {true, false},
        {true, true},
        {false, true},
        {false, false},
        {true, true},
        {true, false},
        {false, true},
    }};
    unsigned cross_bitmap = 0;
    unsigned enable_transitions = 0;
    unsigned done_transitions = 0;
    bool previous_enable = false;
    bool previous_done = false;
    for (unsigned index = 0; index < patterns.size(); ++index) {
        const auto [enable, done] = patterns[index];
        cross_bitmap |= 1U << ((enable ? 2U : 0U) | (done ? 1U : 0U));
        enable_transitions += enable != previous_enable;
        done_transitions += done != previous_done;
        environment.drive_l2_flush(enable, done);
        if (!environment.run_cycles(1) ||
            environment.outer_l2_flush_enabled() != enable ||
            environment.backend_l2_flush_done() != done) {
            std::cerr << "MEMBLOCK_L2_FLUSH_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=pattern index="
                      << index << " expected_enable=" << enable
                      << " observed_enable="
                      << environment.outer_l2_flush_enabled()
                      << " expected_done=" << done << " observed_done="
                      << environment.backend_l2_flush_done()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        previous_enable = enable;
        previous_done = done;
    }
    environment.drive_l2_flush(false, false);
    if (!environment.run_cycles(2) || environment.outer_l2_flush_enabled() ||
        environment.backend_l2_flush_done() || cross_bitmap != 0xf ||
        enable_transitions < 4 || done_transitions < 4) {
        std::cerr << "MEMBLOCK_L2_FLUSH_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=coverage"
                  << " cross_bitmap=0x" << std::hex << cross_bitmap << std::dec
                  << " enable_transitions=" << enable_transitions
                  << " done_transitions=" << done_transitions
                  << " checks=" << environment.l2_flush_checks()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_L2_FLUSH_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " combinations=4"
              << " enable_transitions=" << enable_transitions
              << " done_transitions=" << done_transitions
              << " delay_checks=" << environment.l2_flush_checks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_top_control_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.reset() || environment.backend_hart_id() != 0 ||
        environment.inner_reset_vector() != 0 ||
        environment.outer_power_down_enabled() ||
        environment.outer_cpu_halted() ||
        environment.outer_cpu_critical_error()) {
        std::cerr << "MEMBLOCK_TOP_CONTROL_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    constexpr std::array<std::uint8_t, 8> hart_ids{{
        0x00, 0x01, 0x3f, 0x15, 0x2a, 0x20, 0x0f, 0x30,
    }};
    constexpr std::array<std::uint64_t, 8> reset_vectors{{
        0x000000000000ULL,
        0x000000000001ULL,
        0xffffffffffffULL,
        0x800000000000ULL,
        0x000080000000ULL,
        0x555555555555ULL,
        0xaaaaaaaaaaaaULL,
        0x123456789abcULL,
    }};
    unsigned boolean_cross_bitmap = 0;
    unsigned power_transitions = 0;
    unsigned halt_transitions = 0;
    unsigned error_transitions = 0;
    bool previous_power = false;
    bool previous_halt = false;
    bool previous_error = false;
    for (unsigned index = 0; index < hart_ids.size(); ++index) {
        const bool power_down = (index & 1U) != 0;
        const bool cpu_halted = (index & 2U) != 0;
        const bool cpu_critical_error = (index & 4U) != 0;
        boolean_cross_bitmap |= 1U << index;
        power_transitions += power_down != previous_power;
        halt_transitions += cpu_halted != previous_halt;
        error_transitions += cpu_critical_error != previous_error;
        environment.drive_top_controls(
            hart_ids[index], reset_vectors[index], power_down,
            cpu_halted, cpu_critical_error);
        if (!environment.run_cycles(1) ||
            environment.backend_hart_id() != hart_ids[index] ||
            environment.inner_reset_vector() != reset_vectors[index] ||
            environment.outer_power_down_enabled() != power_down ||
            environment.outer_cpu_halted() != cpu_halted ||
            environment.outer_cpu_critical_error() != cpu_critical_error) {
            std::cerr << "MEMBLOCK_TOP_CONTROL_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=pattern index="
                      << index << " hart="
                      << static_cast<unsigned>(environment.backend_hart_id())
                      << " reset_vector=0x" << std::hex
                      << environment.inner_reset_vector() << std::dec
                      << " power_down="
                      << environment.outer_power_down_enabled()
                      << " halted=" << environment.outer_cpu_halted()
                      << " critical_error="
                      << environment.outer_cpu_critical_error()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        previous_power = power_down;
        previous_halt = cpu_halted;
        previous_error = cpu_critical_error;
    }

    std::uint32_t prefetch_control_bitmap = 0;
    unsigned msi_clint_valid_bitmap = 0;
    std::uint64_t perf_value_bitmap = 0;
    bool saw_zero_delay = false;
    bool saw_max_delay = false;
    for (unsigned index = 0; index < 256; ++index) {
        memblock::TopBridgeStimulus stimulus;
        stimulus.msi_info_valid = (index & 1U) != 0;
        stimulus.clint_time_valid = (index & 2U) != 0;
        stimulus.beu_valid = (index & 4U) != 0;
        stimulus.msi_ack = (index & 8U) != 0;
        stimulus.frontend_reset = (index & 16U) != 0;
        stimulus.beu_address =
            (0x123456789abcULL ^ (std::uint64_t{index} << 36)) &
            0xffffffffffffULL;
        stimulus.msi_info = static_cast<std::uint16_t>(
            (0x5a5U ^ (index * 0x91U)) & 0xfffU);
        stimulus.clint_time =
            0x0123456789abcdefULL ^
            (std::uint64_t{index} * 0x1111111111111111ULL);
        stimulus.interrupt_msip = (index & 1U) != 0;
        stimulus.interrupt_mtip = (index & 2U) != 0;
        stimulus.interrupt_meip = (index & 4U) != 0;
        stimulus.interrupt_seip = (index & 8U) != 0;
        stimulus.interrupt_debug = (index & 16U) != 0;
        stimulus.interrupt_nmi_31 = (index & 32U) != 0;
        stimulus.interrupt_nmi_43 = (index & 64U) != 0;
        stimulus.interrupt_beu_local = (index & 128U) != 0;
        for (unsigned lane = memblock::generated::kHcPerfEventFirstInputLane;
             lane <= memblock::generated::kHcPerfEventHighestInputLane;
             ++lane) {
            const auto value = static_cast<std::uint8_t>(
                (lane + index * 17U) & 0x3fU);
            stimulus.hc_perf_events[lane] = value;
            perf_value_bitmap |= std::uint64_t{1} << value;
        }
        stimulus.l2_prefetch = {
            .master_enable = (index & 1U) != 0,
            .receive_enable = (index & 2U) != 0,
            .pbop_enable = (index & 4U) != 0,
            .vbop_enable = (index & 8U) != 0,
            .tp_enable = (index & 16U) != 0,
            .delay_latency = static_cast<std::uint16_t>(
                index == 0 ? 0 : index == 31 ? 0x3ff : index * 31U),
        };
        prefetch_control_bitmap |= std::uint32_t{1} << (index & 31U);
        msi_clint_valid_bitmap |=
            1U << ((stimulus.msi_info_valid ? 1U : 0U) |
                   (stimulus.clint_time_valid ? 2U : 0U));
        saw_zero_delay |= stimulus.l2_prefetch.delay_latency == 0;
        saw_max_delay |= stimulus.l2_prefetch.delay_latency == 0x3ff;
        environment.drive_top_bridge_stimulus(stimulus);
        if (!environment.run_cycles(1)) {
            std::cerr << "MEMBLOCK_TOP_CONTROL_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=bridge index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    environment.drive_top_controls(0, 0, false, false, false);
    environment.drive_top_bridge_stimulus({});
    if (!environment.run_cycles(3) || environment.backend_hart_id() != 0 ||
        environment.inner_reset_vector() != 0 ||
        environment.outer_power_down_enabled() ||
        environment.outer_cpu_halted() ||
        environment.outer_cpu_critical_error() ||
        boolean_cross_bitmap != 0xff || power_transitions < 7 ||
        halt_transitions < 3 || error_transitions < 1 ||
        prefetch_control_bitmap != 0xffffffffU ||
        msi_clint_valid_bitmap != 0xfU ||
        perf_value_bitmap != ~std::uint64_t{0} ||
        !saw_zero_delay || !saw_max_delay) {
        std::cerr << "MEMBLOCK_TOP_CONTROL_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=coverage"
                  << " boolean_cross_bitmap=0x" << std::hex
                  << boolean_cross_bitmap
                  << " prefetch_control_bitmap=0x"
                  << prefetch_control_bitmap
                  << " msi_clint_valid_bitmap=0x"
                  << msi_clint_valid_bitmap
                  << " perf_value_bitmap=0x" << perf_value_bitmap << std::dec
                  << " transitions=" << power_transitions << ','
                  << halt_transitions << ',' << error_transitions
                  << " control_checks=" << environment.top_control_checks()
                  << " bridge_checks=" << environment.top_bridge_checks()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_TOP_CONTROL_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " boolean_combinations=8"
              << " hart_patterns=" << hart_ids.size()
              << " reset_vector_patterns=" << reset_vectors.size()
              << " transitions=" << power_transitions << ','
              << halt_transitions << ',' << error_transitions
              << " bridge_patterns=256"
              << " interrupt_combinations=256"
              << " l2_prefetch_combinations=32"
              << " perf_event_shared_lanes="
              << memblock::generated::kHcPerfEventLastSharedLane -
                     memblock::generated::kHcPerfEventFirstSharedLane + 1
              << " perf_event_values=64"
              << " control_checks=" << environment.top_control_checks()
              << " bridge_checks=" << environment.top_bridge_checks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_trace_bridge_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_TRACE_BRIDGE_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    constexpr std::uint64_t address_mask =
        (std::uint64_t{1} << 50) - 1U;
    std::uint16_t group_zero_itype_bitmap = 0;
    unsigned group_valid_bitmap = 0;
    unsigned encoder_bitmap = 0;
    unsigned privilege_bitmap = 0;
    unsigned ftq_offset_bitmap = 0;
    unsigned trap_updates = 0;
    bool saw_zero_iretire = false;
    bool saw_max_iretire = false;
    for (unsigned index = 0; index < 32; ++index) {
        memblock::TraceBridgeStimulus stimulus;
        stimulus.encoder_enable = (index & 1U) != 0;
        stimulus.encoder_stall = (index & 2U) != 0;
        stimulus.privilege = static_cast<std::uint8_t>(index & 0x7U);
        stimulus.mstatus =
            0x8000000000000000ULL ^
            (std::uint64_t{index} * 0x0101010101010101ULL);
        stimulus.trap_cause = 0x1000ULL + index;
        stimulus.trap_tval =
            (0x2000000000000ULL + index * 0x101ULL) & address_mask;
        stimulus.groups[0].valid = (index & 16U) != 0;
        stimulus.groups[1].valid = (index & 1U) != 0;
        stimulus.groups[2].valid = (index & 2U) != 0;
        unsigned valid_pattern = 0;
        for (unsigned group = 0; group < memblock::kTraceGroups; ++group) {
            auto &trace_group = stimulus.groups[group];
            const auto ftq_offset = static_cast<std::uint8_t>(
                (index + group * 5U) & 0xfU);
            trace_group.iaddr =
                index == 31 && group == 0
                    ? address_mask - 4U
                    : (0x123456780000ULL + index * 0x101ULL +
                       group * 0x10000ULL) & address_mask;
            trace_group.ftq_offset = ftq_offset;
            trace_group.itype = static_cast<std::uint8_t>(
                (index + group * 5U) & 0xfU);
            trace_group.iretire = static_cast<std::uint8_t>(
                index == 31 && group == 2
                    ? 0x7fU
                    : (index * 7U + group * 11U) & 0x7fU);
            trace_group.ilastsize = ((index + group) & 1U) != 0;
            valid_pattern |= trace_group.valid ? 1U << group : 0U;
            ftq_offset_bitmap |= 1U << ftq_offset;
            saw_zero_iretire |= trace_group.iretire == 0;
            saw_max_iretire |= trace_group.iretire == 0x7f;
        }
        if (stimulus.groups[0].valid) {
            group_zero_itype_bitmap |=
                std::uint16_t{1} << stimulus.groups[0].itype;
            trap_updates += stimulus.groups[0].itype == 1 ||
                            stimulus.groups[0].itype == 2;
        }
        group_valid_bitmap |= 1U << valid_pattern;
        encoder_bitmap |=
            1U << ((stimulus.encoder_enable ? 1U : 0U) |
                   (stimulus.encoder_stall ? 2U : 0U));
        privilege_bitmap |= 1U << stimulus.privilege;
        environment.drive_trace_bridge_stimulus(stimulus);
        if (!environment.run_cycles(1)) {
            std::cerr << "MEMBLOCK_TRACE_BRIDGE_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=pattern index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    environment.drive_trace_bridge_stimulus({});
    if (!environment.run_cycles(2) ||
        group_zero_itype_bitmap != 0xffffU ||
        group_valid_bitmap != 0xffU || encoder_bitmap != 0xfU ||
        privilege_bitmap != 0xffU || ftq_offset_bitmap != 0xffffU ||
        trap_updates != 2 || !saw_zero_iretire || !saw_max_iretire ||
        environment.trace_bridge_checks() < 34) {
        std::cerr << "MEMBLOCK_TRACE_BRIDGE_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=coverage"
                  << " itype_bitmap=0x" << std::hex
                  << group_zero_itype_bitmap
                  << " valid_bitmap=0x" << group_valid_bitmap
                  << " encoder_bitmap=0x" << encoder_bitmap
                  << " privilege_bitmap=0x" << privilege_bitmap
                  << " ftq_offset_bitmap=0x" << ftq_offset_bitmap << std::dec
                  << " trap_updates=" << trap_updates
                  << " checks=" << environment.trace_bridge_checks()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_TRACE_BRIDGE_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " patterns=32 groups=3 valid_combinations=8"
              << " itypes=16 ftq_offsets=16 privileges=8"
              << " encoder_combinations=4 trap_updates=" << trap_updates
              << " checks=" << environment.trace_bridge_checks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_dft_bridge_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.reset() || !environment.check_dft_bridge_space() ||
        !environment.reset() || !environment.check_idle(2) ||
        environment.dft_bridge_patterns() != 1024) {
        std::cerr << "MEMBLOCK_DFT_BRIDGE_CONTRACTS_FAIL cycle="
                  << environment.cycle()
                  << " patterns=" << environment.dft_bridge_patterns()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_DFT_BRIDGE_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " inputs=10 patterns=" << environment.dft_bridge_patterns()
              << " frontend_outputs=10 backend_outputs=4"
              << " digest=0x" << std::hex << environment.dft_bridge_digest()
              << std::dec
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_pin_space(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.check_pin_space() || !environment.reset() ||
        !environment.check_idle(24)) {
        std::cerr << "MEMBLOCK_PIN_SPACE_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_PIN_SPACE_PASS"
              << " cycle=" << environment.cycle()
              << " inputs=" << memblock::generated::kSweptInputCount + 2
              << " swept_inputs=" << memblock::generated::kSweptInputCount
              << " input_bits=" << memblock::generated::kSweptInputBitCount + 2
              << " outputs=" << memblock::generated::kSampledOutputCount
              << " output_bits=" << memblock::generated::kSampledOutputBitCount
              << " patterns=" << memblock::generated::kPinSpacePatternCount
              << " digest=0x" << std::hex << environment.pin_space_digest() << std::dec
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_frontend_bridge(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    if (!environment.reset() ||
        !environment.exercise_frontend_bridges(
            options.transactions, options.seed)) {
        std::cerr << "MEMBLOCK_FRONTEND_BRIDGE_FAIL"
                  << " seed=" << options.seed
                  << " transactions=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    const auto &stats = environment.frontend_bridge_stats();
    std::cout << "MEMBLOCK_FRONTEND_BRIDGE_PASS"
              << " seed=" << options.seed
              << " transactions=" << options.transactions
              << " cycle=" << environment.cycle()
              << " requests=" << stats.requests
              << " responses=" << stats.responses
              << " request_stalls=" << stats.request_stalls
              << " response_stalls=" << stats.response_stalls
              << " source_credit_stalls=" << stats.source_credit_stalls
              << " field_checks=" << stats.field_checks
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_frontend_reset_recovery(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.check_frontend_bridge_reset_recovery() ||
        environment.frontend_reset_canceled_requests() != 3 ||
        environment.frontend_reset_canceled_responses() != 1 ||
        environment.frontend_reset_survivor_requests() != 3 ||
        environment.frontend_reset_stall_checks() != 8) {
        std::cerr << "MEMBLOCK_FRONTEND_RESET_RECOVERY_FAIL cycle="
                  << environment.cycle()
                  << " canceled_requests="
                  << environment.frontend_reset_canceled_requests()
                  << " canceled_responses="
                  << environment.frontend_reset_canceled_responses()
                  << " survivor_requests="
                  << environment.frontend_reset_survivor_requests()
                  << " stall_checks="
                  << environment.frontend_reset_stall_checks()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_FRONTEND_RESET_RECOVERY_PASS"
              << " cycle=" << environment.cycle()
              << " paths=3 canceled_requests="
              << environment.frontend_reset_canceled_requests()
              << " canceled_responses="
              << environment.frontend_reset_canceled_responses()
              << " survivor_requests="
              << environment.frontend_reset_survivor_requests()
              << " stall_checks="
              << environment.frontend_reset_stall_checks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
